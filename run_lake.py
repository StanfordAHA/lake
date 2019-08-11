import kratos
from lake.modules.passthru import *
from lake.top.lake_top import *

lake_top = LakeTop(2)
lake_top_src = kratos.verilog(lake_top)
print(lake_top_src["LakeTop"])
