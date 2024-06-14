envsubst < calibre_lvs.rule.template > calibre_lvs.rule
v2lvs -i -lsp inputs/adk/stdcells.spi -s inputs/adk/stdcells.spi -lsp source.added -s source.added -v inputs/design.lvs.v -o design.lvs.v.spice
calibre -lvs -hier ./calibre_lvs.rule -hcell hcells -automatch