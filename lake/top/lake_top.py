import kratos
from lake import modules


mod = PassThroughMod()
mod_src = kratos.verilog(mod)
print(mod_src["PassThrough"])
