import kratos
from lake.modules.passthru import *
from lake.top.lake_top import *

lake_top = LakeTop(16)
lake_top_src = kratos.verilog(lake_top, optimize_passthrough=False)
print(lake_top_src["LakeTop"])
