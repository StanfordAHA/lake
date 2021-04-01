import pytest
from magma import clear_cachedFunctions
import magma
from kratos import clear_context

collect_ignore = [
    "tests/test_transpose_buffer_kratos.py"
]

@pytest.fixture(autouse=True)
def kratos_test():
    clear_cachedFunctions()
    magma.frontend.coreir_.ResetCoreIR()
    clear_context()

def pytest_addoption(parser):
    parser.addoption('--longrun', action='store_true', dest="longrun",
                     default=False, help="enable longrun decorated tests")

def pytest_configure(config):
    if not config.option.longrun:
        setattr(config.option, 'markexpr', 'not longrun')