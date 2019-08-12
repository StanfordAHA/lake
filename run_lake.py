import kratos
from lake.modules.passthru import *
from lake.top.lake_top import *
from lake.modules.sram_stub import *

#lake_top = LakeTop(16)
#lake_top_src = kratos.verilog(lake_top, optimize_passthrough=False)
#print(lake_top_src["LakeTop"])

sramstub = SRAMStub(16, 512)
sramstub_src = kratos.verilog(sramstub, optimize_passthrough=False)
print(sramstub_src["SRAMStub"])
