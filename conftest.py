from magma import clear_cachedFunctions
import magma.backend.coreir_ as coreir_
clear_cachedFunctions()
coreir_.CoreIRContextSingleton().reset_instance()
from kratos import clear_context
clear_context()