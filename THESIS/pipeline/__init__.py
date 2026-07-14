"""Thesis-artifact generation pipeline.

Registry-driven — see ``registry.py`` for the source-of-truth mapping of
every figure / table referenced by ``main_thesis.tex`` to its generator
callable, source data, and output path (relative to ``THESIS/output/``).
"""

from .errors import MissingDataError

__all__ = ["MissingDataError"]
