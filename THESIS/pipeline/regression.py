"""Memtile PPA regression fit.

Follows the Kahng-hybrid recipe (Kahng et al., DATE 2015 / SLIP 2013 /
TCAD): the SRAM macro is treated as a piecewise-constant lookup and
subtracted from the target; a sparse linear model (Lasso) is then fit
on the surrounding logic. Coefficients are interpretable as
"cost per unit of parameter X" — exactly the format the thesis'
``tab:memtile_model_coeff`` expects.

Design choices (see PIPELINE.md §7 and the regression research memory):
  - **SRAM split.** ``synth_storage_area_um2`` (from
    ``extract_power_area.py``) is subtracted from
    ``synth_total_area_um2`` → ``logic_area``. Lasso fits ``logic_area``.
  - **Features.** ``dim, fw, vc, inp, outp, log2(msw), log2(storage_cap),
    log2(data_width)``. Powers-of-two are log-encoded so the coefficient
    reads as "cost per doubling."
  - **Regularization.** LassoCV with 5-fold on the feature standardizer,
    then refit; alpha selected via CV within the training set only.
  - **Cross-validation.** Leave-one-experiment-out (LOEO) — each sweep
    experiment is held out in turn. Random k-fold silently leaks because
    within-experiment features co-vary.
  - **Delay.** Fit only on unsaturated points
    (``crit_path_delay_ps < 0.99 * clock_period_ps``); report the clock
    ceiling separately. This is the Tobit-lite compromise — a full
    censored regression is overkill at n=110.
  - **Power.** Same recipe as area but on ``synth_power_w``. Falls back
    to MissingDataError when the power flow hasn't landed yet.
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path

import numpy as np
import pandas as pd
from sklearn.linear_model import LassoCV
from sklearn.metrics import mean_absolute_percentage_error
from sklearn.preprocessing import StandardScaler

from .errors import MissingDataError

# Feature schema. All numeric; power-of-two params are log2-encoded on
# construction. Missing columns are treated as 0 (many experiments only
# vary a subset of params).
FEATURES = [
    "dim",
    "fw",
    "vc",
    "inp",
    "outp",
    "me",
    "log2_msw",
    "log2_storage_cap",
    "log2_data_width",
]


def _prep_features(df: pd.DataFrame) -> pd.DataFrame:
    """Return a DataFrame with exactly ``FEATURES`` columns, log-encoded."""
    out = pd.DataFrame(index=df.index)
    for col in ("dim", "fw", "vc", "inp", "outp", "me"):
        out[col] = df[col].fillna(0).astype(float) if col in df.columns else 0.0
    for col in ("msw", "storage_cap", "data_width"):
        vals = df[col].fillna(1).astype(float) if col in df.columns else pd.Series(1.0, index=df.index)
        vals = vals.clip(lower=1)
        out[f"log2_{col}"] = np.log2(vals)
    return out[FEATURES]


@dataclass
class FitResult:
    target: str
    coef: pd.Series          # per-feature coefficient (µm² or W per unit)
    intercept: float
    n_train: int
    n_features_used: int     # non-zero coefficients
    mape_in_sample: float
    loeo: pd.DataFrame       # per-held-out-experiment residual stats


def fit_area_logic(df: pd.DataFrame) -> FitResult:
    """Fit Lasso on ``logic_area = total_area - storage_area``.

    Requires ``synth_total_area_um2`` and ``synth_storage_area_um2``.
    """
    need = ["synth_total_area_um2", "synth_storage_area_um2", "experiment"]
    missing = [c for c in need if c not in df.columns]
    if missing:
        raise MissingDataError(f"missing columns for area fit: {missing}")

    work = df.dropna(subset=["synth_total_area_um2", "synth_storage_area_um2"]).copy()
    if work.empty:
        raise MissingDataError("no rows with both total + storage area")

    work["logic_area"] = work["synth_total_area_um2"] - work["synth_storage_area_um2"]
    X = _prep_features(work)
    y = work["logic_area"].astype(float)
    return _fit_lasso_with_loeo("logic_area_um2", X, y, work["experiment"])


def fit_power(df: pd.DataFrame) -> FitResult:
    """Fit Lasso on ``synth_power_w`` (all logic + SRAM lumped together
    — the ptpx report is total). Raises MissingDataError if power_w is
    all-null."""
    if "synth_power_w" not in df.columns:
        raise MissingDataError("synth_power_w column missing entirely")
    work = df.dropna(subset=["synth_power_w"]).copy()
    if work.empty:
        raise MissingDataError("synth_power_w present but all-null — power flow hasn't been run")
    X = _prep_features(work)
    y = work["synth_power_w"].astype(float)
    return _fit_lasso_with_loeo("synth_power_w", X, y, work["experiment"])


def fit_delay_unsaturated(df: pd.DataFrame) -> FitResult:
    """Fit critical-path delay on unsaturated points only.

    Points where ``crit_path_delay_ps >= 0.99 * clock_period_ps`` are
    dropped (they're at the timing ceiling and dominated by clock-target
    choice, not the parameter of interest). The ceiling itself should be
    reported as a scoped constant, not a coefficient.
    """
    need = ["crit_path_delay_ps", "clock_period_ps", "experiment"]
    missing = [c for c in need if c not in df.columns]
    if missing:
        raise MissingDataError(f"missing columns for delay fit: {missing}")

    work = df.dropna(subset=["crit_path_delay_ps", "clock_period_ps"]).copy()
    if work.empty:
        raise MissingDataError("no rows with both crit_delay and clock_period")

    ceiling = work["clock_period_ps"] * 0.99
    unsat = work[work["crit_path_delay_ps"] < ceiling]
    if len(unsat) < 10:
        raise MissingDataError(
            f"only {len(unsat)} unsaturated points (design pegged at timing "
            "ceiling); fit not meaningful without a frequency sweep"
        )
    X = _prep_features(unsat)
    y = unsat["crit_path_delay_ps"].astype(float)
    return _fit_lasso_with_loeo("crit_path_delay_ps", X, y, unsat["experiment"])


def _fit_lasso_with_loeo(
    target: str,
    X: pd.DataFrame,
    y: pd.Series,
    groups: pd.Series,
) -> FitResult:
    """Standardize + LassoCV, then LOEO CV for out-of-sweep MAPE."""
    scaler = StandardScaler()
    X_scaled = scaler.fit_transform(X.values)

    lasso = LassoCV(cv=min(5, len(np.unique(groups))), max_iter=20000)
    lasso.fit(X_scaled, y.values)

    # De-standardize coefficients so they read in native units per feature unit.
    raw_coef = lasso.coef_ / scaler.scale_
    intercept = float(lasso.intercept_ - np.dot(raw_coef, scaler.mean_))
    coef = pd.Series(raw_coef, index=X.columns).round(4)

    y_pred_in = lasso.predict(X_scaled)
    mape_in = float(mean_absolute_percentage_error(y.values, y_pred_in))

    # Leave-one-experiment-out CV.
    rows: list[dict] = []
    for held in sorted(groups.unique()):
        train_mask = (groups != held).values
        test_mask = ~train_mask
        if test_mask.sum() == 0 or train_mask.sum() == 0:
            continue
        s = StandardScaler().fit(X.values[train_mask])
        Xtr = s.transform(X.values[train_mask])
        Xte = s.transform(X.values[test_mask])
        m = LassoCV(cv=min(3, train_mask.sum()), max_iter=20000).fit(Xtr, y.values[train_mask])
        y_pred = m.predict(Xte)
        rows.append({
            "held_out": held,
            "n_test": int(test_mask.sum()),
            "mape": float(mean_absolute_percentage_error(y.values[test_mask], y_pred)),
            "median_y_true": float(np.median(y.values[test_mask])),
            "median_y_pred": float(np.median(y_pred)),
        })

    return FitResult(
        target=target,
        coef=coef,
        intercept=intercept,
        n_train=len(y),
        n_features_used=int((coef != 0).sum()),
        mape_in_sample=mape_in,
        loeo=pd.DataFrame(rows),
    )


# ---- LaTeX table emitters --------------------------------------------------


def render_coef_table(fits: list[FitResult]) -> str:
    r"""Emit a \begin{tabular} snippet with one column per fit target."""
    if not fits:
        raise MissingDataError("no fits to render")

    all_feats = FEATURES
    header_cols = ["Feature"] + [f.target for f in fits]
    header = " & ".join(header_cols) + r" \\"
    lines = [
        r"\begin{tabular}{l" + "r" * len(fits) + "}",
        r"\toprule",
        header,
        r"\midrule",
    ]
    for feat in all_feats:
        row = [feat.replace("_", r"\_")]
        for f in fits:
            v = f.coef.get(feat, 0.0)
            row.append(f"{v:.3g}" if v != 0 else "--")
        lines.append(" & ".join(row) + r" \\")
    lines.append(r"\midrule")
    row = ["intercept"] + [f"{f.intercept:.3g}" for f in fits]
    lines.append(" & ".join(row) + r" \\")
    row = ["n (train)"] + [str(f.n_train) for f in fits]
    lines.append(" & ".join(row) + r" \\")
    row = ["in-sample MAPE"] + [f"{f.mape_in_sample:.1%}" for f in fits]
    lines.append(" & ".join(row) + r" \\")
    lines.extend([r"\bottomrule", r"\end{tabular}"])
    return "\n".join(lines) + "\n"


def render_verif_table(fits: list[FitResult]) -> str:
    r"""Emit a \begin{tabular} of LOEO residuals (one block per fit)."""
    if not fits:
        raise MissingDataError("no fits to render")

    lines = [
        r"\begin{tabular}{llrrrr}",
        r"\toprule",
        r"Target & Held-out experiment & $n_{\mathrm{test}}$ & MAPE & median true & median pred \\",
        r"\midrule",
    ]
    for f in fits:
        if f.loeo.empty:
            lines.append(f"{f.target.replace('_', chr(92) + '_')} & (no groups) & -- & -- & -- & -- \\\\")
            continue
        first = True
        for _, r in f.loeo.iterrows():
            tgt = f.target.replace("_", r"\_") if first else ""
            first = False
            lines.append(
                f"{tgt} & {r['held_out'].replace('_', chr(92)+'_')} & "
                f"{int(r['n_test'])} & {r['mape']:.1%} & "
                f"{r['median_y_true']:.3g} & {r['median_y_pred']:.3g} \\\\"
            )
        lines.append(r"\midrule")
    if lines[-1] == r"\midrule":
        lines.pop()
    lines.extend([r"\bottomrule", r"\end{tabular}"])
    return "\n".join(lines) + "\n"


def emit_coef_table(df: pd.DataFrame, outpath: Path) -> None:
    """Fit whichever targets have data and write the coefficient table.

    Area is required; power/delay are best-effort. Raises
    MissingDataError only if *nothing* fits.
    """
    fits: list[FitResult] = []
    errors: list[str] = []
    for name, fn in (("area", fit_area_logic), ("power", fit_power), ("delay", fit_delay_unsaturated)):
        try:
            fits.append(fn(df))
        except MissingDataError as e:
            errors.append(f"{name}: {e}")
    if not fits:
        raise MissingDataError("; ".join(errors))
    outpath.parent.mkdir(parents=True, exist_ok=True)
    body = render_coef_table(fits)
    if errors:
        body = "% partial fit — skipped: " + "; ".join(errors) + "\n" + body
    outpath.write_text(body)


def emit_verif_table(df: pd.DataFrame, outpath: Path) -> None:
    fits: list[FitResult] = []
    errors: list[str] = []
    for name, fn in (("area", fit_area_logic), ("power", fit_power), ("delay", fit_delay_unsaturated)):
        try:
            fits.append(fn(df))
        except MissingDataError as e:
            errors.append(f"{name}: {e}")
    if not fits:
        raise MissingDataError("; ".join(errors))
    outpath.parent.mkdir(parents=True, exist_ok=True)
    body = render_verif_table(fits)
    if errors:
        body = "% partial fit — skipped: " + "; ".join(errors) + "\n" + body
    outpath.write_text(body)
