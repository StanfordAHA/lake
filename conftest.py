import pytest
from magma import clear_cachedFunctions
import magma.backend.coreir_ as coreir_
from kratos import clear_context

collect_ignore = [
    "tests/test_aggregator_magma.py"
]

@pytest.fixture(autouse=True)
def kratos_test():
    clear_cachedFunctions()
    coreir_.CoreIRContextSingleton().reset_instance()
    clear_context()

def pytest_addoption(parser):
    parser.addoption('--longrun', action='store_true', dest="longrun",
                     default=False, help="enable longrun decorated tests")

def pytest_configure(config):
    if not config.option.longrun:
        setattr(config.option, 'markexpr', 'not longrun')