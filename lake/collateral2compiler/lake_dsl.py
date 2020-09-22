# example of DSL

# mem_collateral is part of Lake, not exposed to user
# could consumer ports and parameters and then set port_info appropriately (see second example)

agg_write_port = MemPort(1, 0)
agg_read_port = MemPort(0, 0)

agg_params = {"capacity": 4,
              "word_width": 16,
              "num_read_ports": 1,
              "read_port_width": 4,
              "num_write_ports": 1,
              "write_port_width": 1,
              "chaining": 0,
              "write_info": agg_write_port.port_info,
              "read_info": agg_read_port.port_info}

agg = mem_inst(agg_params)


agg_write_port = MemPort(1, 0)
agg_read_port = MemPort(0, 0)

agg_params = {"capacity": 4,
              "word_width": 16,
              "num_read_ports": 1,
              "read_port_width": 4,
              "num_write_ports": 1,
              "write_port_width": 1,
              "chaining": 0)

agg = mem_inst(params=agg_params,
               write_ports=[agg_write_port],
               read_ports=[agg_read_port])
