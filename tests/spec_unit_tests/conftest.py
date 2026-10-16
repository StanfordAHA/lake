"""Let this directory's helpers (rtl_harness, spec_rtl) be imported as top-level
modules when pytest runs from the repo root (CI: `pytest tests/`), not only from here."""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
