"""Errors surfaced by generators. Raising ``MissingDataError`` in a
generator triggers the BOGUS fallback in the orchestrator instead of
crashing the whole run."""


class MissingDataError(Exception):
    """Raised by a generator when required source data isn't available.

    The message should describe *what* is missing (e.g. a build dir, a
    CSV column, a report file) so the ``missing.log`` entry is
    actionable.
    """
