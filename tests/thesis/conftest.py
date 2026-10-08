"""Thesis-pipeline tests.

Run with:

    python3 -m pytest --confcutdir=tests/thesis tests/thesis/

``--confcutdir`` prevents pytest from loading the repo-root ``conftest.py``,
which imports ``magma``/``kratos`` unconditionally at collect time. The
thesis pipeline has no such dependencies and shouldn't drag them in.
"""
