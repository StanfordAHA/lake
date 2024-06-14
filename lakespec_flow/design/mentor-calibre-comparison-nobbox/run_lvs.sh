envsubst < calibre_lvs.rule.template > calibre_lvs.rule
v2lvs -i -lsp inputs/adk/stdcells.spi -s inputs/adk/stdcells.spi -lsp inputs/*.sp -s inputs/*.sp -lsp source.added -s source.added -v inputs/design.nofillerflatbuslvs.v -o design.lvs.v.spice

v2lvs -i -lsp inputs/design_extracted.spice -s inputs/design_extracted.spice -lsp source.added -s source.added -o design.extracted.spice

calibre -lvs -hier ./calibre_lvs.rule -hcell hcells -automatch
