"""Snapshot source data into per-figure directories.

Each figure that consumes build-tree data gets ``THESIS/data/<fig_id>/``
containing a ``builds/`` symlink pointing at the relevant subtree of
``THESIS_BUILDS``. This keeps the figure's provenance obvious without
copying multi-GB build outputs. Symlinks are safe here because we only
read from them.
"""

from __future__ import annotations

from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]
DATA_ROOT = REPO_ROOT / "THESIS" / "data"


def symlink_builds(fig_id: str, source: Path, link_name: str = "builds") -> Path:
    """Create ``THESIS/data/<fig_id>/<link_name>`` -> ``source``.

    Idempotent: existing symlink is replaced if it points elsewhere.
    Returns the symlink path.
    """
    fig_dir = DATA_ROOT / fig_id
    fig_dir.mkdir(parents=True, exist_ok=True)
    link = fig_dir / link_name
    source = Path(source).resolve()

    if link.is_symlink() or link.exists():
        if link.is_symlink() and Path(link).resolve() == source:
            return link
        link.unlink()
    link.symlink_to(source)
    return link
