"""Well-formatted placeholder figures with a big red BOGUS watermark.

Used by the orchestrator when a real generator raises ``MissingDataError``.
The placeholder still shows a plausibly-shaped chart so the LaTeX layout
doesn't collapse; the watermark makes it visually impossible to mistake
for a real result.
"""

from __future__ import annotations

from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np


def make_bogus_figure(
    outpath: Path,
    title: str,
    reason: str = "data not yet available",
    xlabel: str = "sweep parameter",
    ylabel: str = "metric",
) -> None:
    """Emit ``outpath`` with a dummy line chart + diagonal red BOGUS overlay.

    Saves in whatever format the suffix implies (PDF is what LaTeX wants;
    PNG also works for review).
    """
    outpath.parent.mkdir(parents=True, exist_ok=True)

    x = np.array([1, 2, 4, 8, 16, 32])
    rng = np.random.default_rng(hash(str(outpath)) & 0xFFFFFFFF)
    y1 = 100 + 40 * np.log2(x) + rng.normal(0, 8, size=x.size)
    y2 = 80 + 25 * np.log2(x) + rng.normal(0, 6, size=x.size)

    fig, ax = plt.subplots(figsize=(6.0, 4.0))
    ax.plot(x, y1, marker="o", label="series A")
    ax.plot(x, y2, marker="s", label="series B")
    ax.set_xscale("log", base=2)
    ax.set_xlabel(xlabel)
    ax.set_ylabel(ylabel)
    ax.set_title(title)
    ax.grid(True, alpha=0.3)
    ax.legend(fontsize=8, loc="best")

    ax.text(
        0.5, 0.5, "BOGUS",
        transform=ax.transAxes,
        color="red", alpha=0.35,
        fontsize=90, fontweight="bold",
        ha="center", va="center", rotation=30,
        zorder=10,
    )
    fig.text(
        0.5, 0.02, f"placeholder — {reason}",
        color="red", alpha=0.9, fontsize=8, ha="center", style="italic",
    )

    fig.tight_layout(rect=(0, 0.04, 1, 1))
    fig.savefig(outpath, dpi=150)
    plt.close(fig)


def make_bogus_table(outpath: Path, title: str, reason: str = "data not yet available") -> None:
    """Emit a LaTeX .tex table body that clearly says BOGUS.

    Kept text-only so ``\\input{...}`` in the thesis picks it up without a
    graphics-driver dependency.
    """
    outpath.parent.mkdir(parents=True, exist_ok=True)
    body = (
        "% AUTO-GENERATED BOGUS TABLE PLACEHOLDER\n"
        f"% title:  {title}\n"
        f"% reason: {reason}\n"
        "\\begin{center}\n"
        "\\fbox{\\parbox{0.7\\linewidth}{\\centering\\color{red}\\bfseries\n"
        f"BOGUS PLACEHOLDER \\\\\n\\normalfont\\normalsize {title} \\\\\n"
        f"\\itshape\\small ({reason})\n"
        "}}\n"
        "\\end{center}\n"
    )
    outpath.write_text(body)
