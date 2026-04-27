from lake.spec.spec import Spec
from lake.spec.port import Port
from lake.utils.spec_enum import Runtime, Direction, MemoryPortType, LFComparisonOperator
from lake.spec.address_generator import AddressGenerator
from lake.spec.iteration_domain import IterationDomain
from lake.spec.schedule_generator import ScheduleGenerator
from lake.spec.storage import SingleBankStorage
from lake.spec.memory_port import MemoryPort
from lake.utils.util import get_data_sizes, TestPrepper, calculate_read_out_vec
from lake.top.tech_maps import GF_Tech_Map
import argparse
import json
import math
import os


def build_four_port_wide_fetch(storage_capacity=4096, data_width=16, dims: int = 6, vec_width=4, physical=False,
                                  reg_file=False, vec_capacity=2, opt_rv=False, add_filter_path=False,
                                  in_ports=2, out_ports=2, dual_port=False,
                                  max_extent=None, max_sequence_width=None) -> Spec:

    opt_rv = False

    # TODO: Override this in garnet and not here...
    id_width = 11
    if max_extent is not None:
        id_width = max(1, math.ceil(math.log2(max(max_extent, 2))))
    stride_width = 16
    if max_sequence_width is not None:
        stride_width = max(1, math.ceil(math.log2(max(max_sequence_width, 2))))
    remote_storage = False

    ls = Spec(name="lakespec", opt_rv=opt_rv, remote_storage=remote_storage,
              config_passthru=False, comply_17=True)

    # Determine if vec_capacity is needed (only when vectorized)
    vc = vec_capacity if vec_width > 1 else None
    int_dw = data_width * vec_width

    # Create input ports
    input_port_list = []
    for i in range(in_ports):
        p = Port(ext_data_width=data_width, int_data_width=int_dw,
                 vec_capacity=vc, runtime=Runtime.STATIC, direction=Direction.IN,
                 opt_rv=opt_rv, opt_timing=False)
        input_port_list.append(p)

    ls.register(*input_port_list)

    if add_filter_path:
        in_port_filter = Port(ext_data_width=data_width, runtime=Runtime.DYNAMIC,
                              direction=Direction.IN, opt_rv=opt_rv, filter=True)
        ls.register(in_port_filter)

    # Create output ports
    output_port_list = []
    for i in range(out_ports):
        p = Port(ext_data_width=data_width, int_data_width=int_dw,
                 vec_capacity=vc, runtime=Runtime.STATIC, direction=Direction.OUT)
        output_port_list.append(p)

    ls.register(*output_port_list)

    if add_filter_path:

        out_port_filter = Port(ext_data_width=data_width, runtime=Runtime.DYNAMIC,
                              direction=Direction.OUT, opt_rv=opt_rv, filter=False)
        ls.register(out_port_filter)

    # Create iteration domains, address generators, schedule generators for each port
    in_controllers = []
    for i in range(in_ports):
        id_ = IterationDomain(dimensionality=dims, extent_width=id_width)
        ag_ = AddressGenerator(dimensionality=dims)
        sg_ = ScheduleGenerator(dimensionality=dims, stride_width=stride_width)
        ls.register(id_, ag_, sg_)
        in_controllers.append((id_, ag_, sg_))

    out_controllers = []
    for i in range(out_ports):
        id_ = IterationDomain(dimensionality=dims, extent_width=id_width)
        ag_ = AddressGenerator(dimensionality=dims)
        sg_ = ScheduleGenerator(dimensionality=dims, stride_width=stride_width)
        ls.register(id_, ag_, sg_)
        out_controllers.append((id_, ag_, sg_))

    if add_filter_path:
        in_id_filter = IterationDomain(dimensionality=dims, extent_width=id_width)
        in_ag_filter = AddressGenerator(dimensionality=dims)
        in_sg_filter = ScheduleGenerator(dimensionality=dims, stride_width=stride_width)

        out_id_filter = IterationDomain(dimensionality=dims, extent_width=id_width)
        out_ag_filter = AddressGenerator(dimensionality=dims)
        out_sg_filter = ScheduleGenerator(dimensionality=dims, stride_width=stride_width)

        ls.register(in_id_filter, in_ag_filter, in_sg_filter)
        ls.register(out_id_filter, out_ag_filter, out_sg_filter)

    data_bytes = (data_width * vec_width) // 8
    tech_map = None
    if physical:
        tech_map = GF_Tech_Map(depth=storage_capacity // data_bytes, width=data_width * vec_width, dual_port=dual_port)

    # 1024 Bytes
    stg = SingleBankStorage(capacity=storage_capacity, tech_map=tech_map, remote=remote_storage)

    if dual_port:
        write_mem_port = MemoryPort(data_width=data_width * vec_width, mptype=MemoryPortType.W, delay=1)
        read_mem_port = MemoryPort(data_width=data_width * vec_width, mptype=MemoryPortType.R, delay=1)
        ls.register(stg, write_mem_port, read_mem_port)
    else:
        shared_rw_mem_port = MemoryPort(data_width=data_width * vec_width, mptype=MemoryPortType.RW, delay=1)
        ls.register(stg, shared_rw_mem_port)

    if add_filter_path:
        # Just try buffering 8 data for now ... want to turn into a fifo if possible.
        filter_cap = data_bytes * 8
        stg_filter = SingleBankStorage(capacity=filter_cap, remote=False)
        write_port_filter = MemoryPort(data_width=data_width, mptype=MemoryPortType.W, delay=1)
        read_port_filter = MemoryPort(data_width=data_width, mptype=MemoryPortType.R, delay=1)
        ls.register(stg_filter, write_port_filter, read_port_filter)

    # All cores are registered at this point
    # Now connect them

    # Connect input ports to their controllers
    for i in range(in_ports):
        id_, ag_, sg_ = in_controllers[i]
        ls.connect(input_port_list[i], id_)
        ls.connect(input_port_list[i], ag_)
        ls.connect(input_port_list[i], sg_)

    # Connect output ports to their controllers
    for i in range(out_ports):
        id_, ag_, sg_ = out_controllers[i]
        ls.connect(output_port_list[i], id_)
        ls.connect(output_port_list[i], ag_)
        ls.connect(output_port_list[i], sg_)

    # Connect ports to memory ports
    if dual_port:
        for p in input_port_list:
            ls.connect(p, write_mem_port)
        for p in output_port_list:
            ls.connect(p, read_mem_port)
    else:
        for p in input_port_list:
            ls.connect(p, shared_rw_mem_port)
        for p in output_port_list:
            ls.connect(p, shared_rw_mem_port)

    # Memory Ports to storage
    if dual_port:
        ls.connect(write_mem_port, stg)
        ls.connect(read_mem_port, stg)
    else:
        ls.connect(shared_rw_mem_port, stg)

    if add_filter_path:
        # In to filter
        ls.connect(in_port_filter, in_id_filter)
        ls.connect(in_port_filter, in_ag_filter)
        ls.connect(in_port_filter, in_sg_filter)

        # Out to filter
        ls.connect(out_port_filter, out_id_filter)
        ls.connect(out_port_filter, out_ag_filter)
        ls.connect(out_port_filter, out_sg_filter)

        # In and Out to filter memory ports
        ls.connect(in_port_filter, write_port_filter)
        ls.connect(out_port_filter, read_port_filter)

        # Memory Ports to storage
        ls.connect(write_port_filter, stg_filter)
        ls.connect(read_port_filter, stg_filter)

    return ls


def get_conv_2_1_app():

        linear_test = {}

        length_scale = 32

        pw_vec_w = 0
        pr_vec_w = 1

        pr_raw_idx_vec_w = 0
        pw_raw_idx_vec_w = 1
        raw_comp_vec_w = LFComparisonOperator.LT.value
        raw_scalar_vec_w = 0
        raw_constraint_vec_w = (pr_vec_w, pr_raw_idx_vec_w,
                                pw_vec_w, pw_raw_idx_vec_w, raw_comp_vec_w, raw_scalar_vec_w)

        pr_war_idx_vec_w = 0
        pw_war_idx_vec_w = 1
        war_comp_vec_w = LFComparisonOperator.GT.value
        war_scalar_vec_w = 2
        war_constraint_vec_w = (pw_vec_w, pw_war_idx_vec_w, pr_vec_w,
                                pr_war_idx_vec_w, war_comp_vec_w, war_scalar_vec_w)

        in_size = 64 * 65
        out_size = 64 * 65

        linear_test[0] = {
            'type': Direction.IN,
            'name': 'port_w0',
            'config': {
                'dimensionality': 1,
                # 'extents': [16 * length_scale],
                'extents': [in_size],
                'address': {
                    'strides': [1],
                    'offset': 0
                },
                'schedule': {
                    'strides': [4],
                    'offset': 4
                }
            },
            'vec_in_config': {
                'dimensionality': 2,
                'extents': [4, 16 * length_scale],
                'address': {
                    'strides': [1, 4],
                    # Start this here to handle the bogus data creation
                    'offset': -64
                },
                'schedule': {
                    'strides': [1, 4],
                    'offset': 0
                }
            },
            'vec_out_config': {
                'dimensionality': 1,
                'extents': [16 * length_scale],
                'address': {
                    'strides': [1],
                    'offset': 0
                },
                'schedule': {
                    'strides': [4],
                    'offset': 4
                }
            },
            'vec_constraints': [raw_constraint_vec_w, war_constraint_vec_w]
        }

        pw_vec_r = 0
        pr_vec_r = 1

        pr_raw_idx_vec_r = 1
        pw_raw_idx_vec_r = 0
        raw_comp_vec_r = LFComparisonOperator.LT.value
        raw_scalar_vec_r = 0
        raw_constraint_vec_r = (pr_vec_r, pr_raw_idx_vec_r,
                                pw_vec_r, pw_raw_idx_vec_r, raw_comp_vec_r, raw_scalar_vec_r)

        pr_war_idx_vec_r = 1
        pw_war_idx_vec_r = 0
        war_comp_vec_r = LFComparisonOperator.GT.value
        war_scalar_vec_r = 2
        war_constraint_vec_r = (pw_vec_r, pw_war_idx_vec_r, pr_vec_r,
                                pr_war_idx_vec_r, war_comp_vec_r, war_scalar_vec_r)

        linear_test[2] = {
            'type': Direction.OUT,
            'name': 'port_r0',
            'config': {
                'dimensionality': 1,
                # 'extents': [16 * length_scale],
                'extents': [out_size],
                'address': {
                    'strides': [1],
                    'offset': 0
                },
                'schedule': {
                    'strides': [4],
                    'offset': 17
                }
            },
            'vec_in_config': {
                'dimensionality': 1,
                'extents': [16 * length_scale],
                'address': {
                    'strides': [1],
                    'offset': 0
                },
                'schedule': {
                    'strides': [4],
                    'offset': 18
                }
            },
            'vec_out_config': {
                'dimensionality': 2,
                'extents': [4, 16 * length_scale],
                'address': {
                    'strides': [1, 4],
                    'offset': 0
                },
                'schedule': {
                    'strides': [1, 4],
                    'offset': 19
                }
            },
            'vec_constraints': [raw_constraint_vec_r, war_constraint_vec_r]
        }

        pw = 0
        pr = 2

        pr_raw_idx = 0
        pw_raw_idx = 0
        raw_comp = LFComparisonOperator.LT.value
        # raw_scalar = 4
        # Allows the reads to start early
        raw_scalar = -64
        raw_constraint = (pr, pr_raw_idx, pw, pw_raw_idx, raw_comp, raw_scalar)

        pw_war_idx = 0
        pr_war_idx = 0
        war_comp = LFComparisonOperator.GT.value
        war_scalar = 100
        war_constraint = (pw, pw_war_idx, pr, pr_war_idx, war_comp, war_scalar)

        # Just have read follow write
        linear_test['constraints'] = [raw_constraint, war_constraint]

        return linear_test


def get_two_read_test():

    linear_test = {}

    pw_vec_w = 0
    pr_vec_w = 1

    pr_raw_idx_vec_w = 0
    pw_raw_idx_vec_w = 1
    raw_comp_vec_w = LFComparisonOperator.LT.value
    raw_scalar_vec_w = 0
    raw_constraint_vec_w = (pr_vec_w, pr_raw_idx_vec_w,
                            pw_vec_w, pw_raw_idx_vec_w, raw_comp_vec_w, raw_scalar_vec_w)

    pr_war_idx_vec_w = 0
    pw_war_idx_vec_w = 1
    war_comp_vec_w = LFComparisonOperator.LT.value
    war_scalar_vec_w = 2
    war_constraint_vec_w = (pw_vec_w, pw_war_idx_vec_w, pr_vec_w,
                            pr_war_idx_vec_w, war_comp_vec_w, war_scalar_vec_w)

    linear_test[0] = {
        'type': Direction.IN,
        'name': 'write_port_0',
        'config': {
            'dimensionality': 1,
            'extents': [16],
            'address': {
                'strides': [1],
                'offset': 0
            },
            'schedule': {
                'strides': [4],
                'offset': 4
            }
        },
        'vec_in_config': {
            'dimensionality': 2,
            'extents': [4, 16],
            'address': {
                'strides': [1, 4],
                'offset': 0
            },
            'schedule': {
                'strides': [1, 4],
                'offset': 0
            }
        },
        'vec_out_config': {
            'dimensionality': 1,
            'extents': [16],
            'address': {
                'strides': [1],
                'offset': 0
            },
            'schedule': {
                'strides': [4],
                'offset': 4
            }
        },
        'vec_constraints': [raw_constraint_vec_w, war_constraint_vec_w]
    }

    pw_vec_r = 0
    pr_vec_r = 1

    pr_raw_idx_vec_r = 1
    pw_raw_idx_vec_r = 0
    raw_comp_vec_r = LFComparisonOperator.LT.value
    raw_scalar_vec_r = 0
    raw_constraint_vec_r = (pr_vec_r, pr_raw_idx_vec_r,
                            pw_vec_r, pw_raw_idx_vec_r, raw_comp_vec_r, raw_scalar_vec_r)

    pr_war_idx_vec_r = 1
    pw_war_idx_vec_r = 0
    war_comp_vec_r = LFComparisonOperator.LT.value
    war_scalar_vec_r = 2
    war_constraint_vec_r = (pw_vec_r, pw_war_idx_vec_r, pr_vec_r,
                            pr_war_idx_vec_r, war_comp_vec_r, war_scalar_vec_r)

    linear_test[2] = {
        'type': Direction.OUT,
        'name': 'read_port_0',
        'config': {
            'dimensionality': 1,
            'extents': [16],
            'address': {
                'strides': [1],
                'offset': 0
            },
            'schedule': {
                'strides': [4],
                'offset': 17
            }
        },
        'vec_in_config': {
            'dimensionality': 1,
            'extents': [16],
            'address': {
                'strides': [1],
                'offset': 0
            },
            'schedule': {
                'strides': [4],
                'offset': 18
            }
        },
        'vec_out_config': {
            'dimensionality': 2,
            'extents': [4, 16],
            'address': {
                'strides': [1, 4],
                'offset': 0
            },
            'schedule': {
                'strides': [1, 4],
                'offset': 19
            }
        },
        'vec_constraints': [raw_constraint_vec_r, war_constraint_vec_r]
    }

    linear_test[3] = {
        'type': Direction.OUT,
        'name': 'read_port_1',
        'config': {
            'dimensionality': 1,
            'extents': [16],
            'address': {
                'strides': [1],
                'offset': 0
            },
            'schedule': {
                'strides': [4],
                'offset': 18
            }
        },
        'vec_in_config': {
            'dimensionality': 1,
            'extents': [16],
            'address': {
                'strides': [1],
                'offset': 0
            },
            'schedule': {
                'strides': [4],
                'offset': 19
            }
        },
        'vec_out_config': {
            'dimensionality': 2,
            'extents': [4, 16],
            'address': {
                'strides': [1, 4],
                'offset': 0
            },
            'schedule': {
                'strides': [1, 4],
                'offset': 20
            }
        },
        'vec_constraints': [raw_constraint_vec_r, war_constraint_vec_r]
    }

    pw = 0
    pr = 2

    pr_raw_idx = 0
    pw_raw_idx = 0
    raw_comp = LFComparisonOperator.LT.value
    raw_scalar = 0
    raw_constraint1 = (pr, pr_raw_idx, pw, pw_raw_idx, raw_comp, raw_scalar)

    pw_war_idx = 0
    pr_war_idx = 0
    war_comp = LFComparisonOperator.LT.value
    war_scalar = 16
    war_constraint1 = (pw, pw_war_idx, pr, pr_war_idx, war_comp, war_scalar)

    pw = 0
    pr = 3

    pr_raw_idx = 0
    pw_raw_idx = 0
    raw_comp = LFComparisonOperator.LT.value
    raw_scalar = 0
    raw_constraint2 = (pr, pr_raw_idx, pw, pw_raw_idx, raw_comp, raw_scalar)

    pw_war_idx = 0
    pr_war_idx = 0
    war_comp = LFComparisonOperator.LT.value
    war_scalar = 16
    war_constraint2 = (pw, pw_war_idx, pr, pr_war_idx, war_comp, war_scalar)

    # Just have read follow write
    linear_test['constraints'] = [raw_constraint1, war_constraint1,
                                  raw_constraint2, war_constraint2]

    return linear_test


def get_linear_test(max_extent=None):

    linear_test = {}

    length_scale = 32

    # When max_extent is set the IterationDomain extent register is narrowed
    # to ceil(log2(max_extent)) bits. The outer extent here is 16 * length_scale,
    # so cap length_scale to keep the workload representable.
    if max_extent is not None:
        length_scale = max(1, min(length_scale, max_extent // 16))

    pw_vec_w = 0
    pr_vec_w = 1

    pr_raw_idx_vec_w = 0
    pw_raw_idx_vec_w = 1
    raw_comp_vec_w = LFComparisonOperator.LT.value
    raw_scalar_vec_w = 0
    raw_constraint_vec_w = (pr_vec_w, pr_raw_idx_vec_w,
                            pw_vec_w, pw_raw_idx_vec_w, raw_comp_vec_w, raw_scalar_vec_w)

    pr_war_idx_vec_w = 0
    pw_war_idx_vec_w = 1
    war_comp_vec_w = LFComparisonOperator.GT.value
    war_scalar_vec_w = 2
    war_constraint_vec_w = (pw_vec_w, pw_war_idx_vec_w, pr_vec_w,
                            pr_war_idx_vec_w, war_comp_vec_w, war_scalar_vec_w)

    linear_test[0] = {
        'type': Direction.IN,
        'name': 'port_w0',
        'config': {
            'dimensionality': 1,
            'extents': [16 * length_scale],
            'address': {
                'strides': [1],
                'offset': 0
            },
            'schedule': {
                'strides': [4],
                'offset': 4
            }
        },
        'vec_in_config': {
            'dimensionality': 2,
            'extents': [4, 16 * length_scale],
            'address': {
                'strides': [1, 4],
                'offset': 0
            },
            'schedule': {
                'strides': [1, 4],
                'offset': 0
            }
        },
        'vec_out_config': {
            'dimensionality': 1,
            'extents': [16 * length_scale],
            'address': {
                'strides': [1],
                'offset': 0
            },
            'schedule': {
                'strides': [4],
                'offset': 4
            }
        },
        'vec_constraints': [raw_constraint_vec_w, war_constraint_vec_w]
    }

    pw_vec_r = 0
    pr_vec_r = 1

    pr_raw_idx_vec_r = 1
    pw_raw_idx_vec_r = 0
    raw_comp_vec_r = LFComparisonOperator.LT.value
    raw_scalar_vec_r = 0
    raw_constraint_vec_r = (pr_vec_r, pr_raw_idx_vec_r,
                            pw_vec_r, pw_raw_idx_vec_r, raw_comp_vec_r, raw_scalar_vec_r)

    pr_war_idx_vec_r = 1
    pw_war_idx_vec_r = 0
    war_comp_vec_r = LFComparisonOperator.GT.value
    war_scalar_vec_r = 2
    war_constraint_vec_r = (pw_vec_r, pw_war_idx_vec_r, pr_vec_r,
                            pr_war_idx_vec_r, war_comp_vec_r, war_scalar_vec_r)

    linear_test[2] = {
        'type': Direction.OUT,
        'name': 'port_r0',
        'config': {
            'dimensionality': 1,
            'extents': [16 * length_scale],
            'address': {
                'strides': [1],
                'offset': 0
            },
            'schedule': {
                'strides': [4],
                'offset': 17
            }
        },
        'vec_in_config': {
            'dimensionality': 1,
            'extents': [16 * length_scale],
            'address': {
                'strides': [1],
                'offset': 0
            },
            'schedule': {
                'strides': [4],
                'offset': 18
            }
        },
        'vec_out_config': {
            'dimensionality': 2,
            'extents': [4, 16 * length_scale],
            'address': {
                'strides': [1, 4],
                'offset': 0
            },
            'schedule': {
                'strides': [1, 4],
                'offset': 19
            }
        },
        'vec_constraints': [raw_constraint_vec_r, war_constraint_vec_r]
    }

    pw = 0
    pr = 2

    pr_raw_idx = 0
    pw_raw_idx = 0
    raw_comp = LFComparisonOperator.LT.value
    raw_scalar = 4
    raw_constraint = (pr, pr_raw_idx, pw, pw_raw_idx, raw_comp, raw_scalar)

    pw_war_idx = 0
    pr_war_idx = 0
    war_comp = LFComparisonOperator.GT.value
    war_scalar = 8
    war_constraint = (pw, pw_war_idx, pr, pr_war_idx, war_comp, war_scalar)

    # Just have read follow write
    linear_test['constraints'] = [raw_constraint, war_constraint]

    return linear_test


def get_linear_test_rv():

    linear_test = {}

    length_scale = 8

    linear_test[0] = {
        'type': Direction.IN,
        'name': 'port_w0',
        'config': {
            'dimensionality': 1,
            'extents': [64 * length_scale],
            'address': {
                'strides': [1],
                'offset': 0
            },
            'schedule': {
                'strides': [4],
                'offset': 4
            }
        },
        'vec_in_config': {
            'dimensionality': 1,
            'extents': [64 * length_scale],
            'address': {
                'strides': [1],
                'offset': 0
            },
            'schedule': {
                'strides': [1],
                'offset': 0
            }
        },
        'vec_out_config': {
            'dimensionality': 1,
            'extents': [64 * length_scale],
            'address': {
                'strides': [1],
                'offset': 0
            },
            'schedule': {
                'strides': [4],
                'offset': 4
            }
        },
        'vec_constraints': []
    }

    pw_vec_r = 0
    pr_vec_r = 1

    pr_raw_idx_vec_r = 1
    pw_raw_idx_vec_r = 0
    raw_comp_vec_r = LFComparisonOperator.LT.value
    raw_scalar_vec_r = 0
    raw_constraint_vec_r = (pr_vec_r, pr_raw_idx_vec_r,
                            pw_vec_r, pw_raw_idx_vec_r, raw_comp_vec_r, raw_scalar_vec_r)

    pr_war_idx_vec_r = 1
    pw_war_idx_vec_r = 0
    war_comp_vec_r = LFComparisonOperator.GT.value
    war_scalar_vec_r = 2
    war_constraint_vec_r = (pw_vec_r, pw_war_idx_vec_r, pr_vec_r,
                            pr_war_idx_vec_r, war_comp_vec_r, war_scalar_vec_r)

    linear_test[2] = {
        'type': Direction.OUT,
        'name': 'port_r0',
        'config': {
            'dimensionality': 1,
            'extents': [64 * length_scale],
            'address': {
                'strides': [1],
                'offset': 0
            },
            'schedule': {
                'strides': [4],
                'offset': 17
            }
        },
        'vec_in_config': {
            'dimensionality': 1,
            'extents': [64 * length_scale],
            'address': {
                'strides': [1],
                'offset': 0
            },
            'schedule': {
                'strides': [4],
                'offset': 18
            }
        },
        'vec_out_config': {
            'dimensionality': 1,
            'extents': [64 * length_scale],
            'address': {
                'strides': [1],
                'offset': 0
            },
            'schedule': {
                'strides': [1],
                'offset': 19
            }
        },
        'vec_constraints': [raw_constraint_vec_r, war_constraint_vec_r]
    }

    pw = 0
    pr = 2

    pr_raw_idx = 0
    pw_raw_idx = 0
    raw_comp = LFComparisonOperator.LT.value
    raw_scalar = 9
    raw_constraint = (pr, pr_raw_idx, pw, pw_raw_idx, raw_comp, raw_scalar)

    pr_war_idx = 0
    pw_war_idx = 0
    war_comp = LFComparisonOperator.GT.value
    war_scalar = 16
    war_constraint = (pw, pw_war_idx, pr, pr_war_idx, war_comp, war_scalar)

    # Just have read follow write
    linear_test['constraints'] = [raw_constraint, war_constraint]

    return linear_test


def get_linear_test_generic(in_ports_count, vec_width, max_extent=None):
    """Generate a simple linear write-then-read test that works with any port configuration.
    Uses only the first input port (index 0) and first output port (index in_ports_count)."""
    linear_test = {}
    length_scale = 32

    # See get_linear_test() for rationale: cap workload to fit narrowed extent register.
    if max_extent is not None:
        length_scale = max(1, min(length_scale, max_extent // 16))

    pw_idx = 0
    pr_idx = in_ports_count

    if vec_width > 1:
        # Vectorized write port
        pw_vec = 0
        pr_vec = 1
        raw_constraint_vec_w = (pr_vec, 0, pw_vec, 1, LFComparisonOperator.LT.value, 0)
        war_constraint_vec_w = (pw_vec, 1, pr_vec, 0, LFComparisonOperator.GT.value, 2)

        linear_test[pw_idx] = {
            'type': Direction.IN,
            'name': 'port_w0',
            'config': {
                'dimensionality': 1,
                'extents': [16 * length_scale],
                'address': {
                    'strides': [1],
                    'offset': 0
                },
                'schedule': {
                    'strides': [vec_width],
                    'offset': vec_width
                }
            },
            'vec_in_config': {
                'dimensionality': 2,
                'extents': [vec_width, 16 * length_scale],
                'address': {
                    'strides': [1, vec_width],
                    'offset': 0
                },
                'schedule': {
                    'strides': [1, vec_width],
                    'offset': 0
                }
            },
            'vec_out_config': {
                'dimensionality': 1,
                'extents': [16 * length_scale],
                'address': {
                    'strides': [1],
                    'offset': 0
                },
                'schedule': {
                    'strides': [vec_width],
                    'offset': vec_width
                }
            },
            'vec_constraints': [raw_constraint_vec_w, war_constraint_vec_w]
        }

        # Vectorized read port
        pw_vec_r = 0
        pr_vec_r = 1
        raw_constraint_vec_r = (pr_vec_r, 1, pw_vec_r, 0, LFComparisonOperator.LT.value, 0)
        war_constraint_vec_r = (pw_vec_r, 0, pr_vec_r, 1, LFComparisonOperator.GT.value, 2)

        linear_test[pr_idx] = {
            'type': Direction.OUT,
            'name': 'port_r0',
            'config': {
                'dimensionality': 1,
                'extents': [16 * length_scale],
                'address': {
                    'strides': [1],
                    'offset': 0
                },
                'schedule': {
                    'strides': [vec_width],
                    'offset': 17
                }
            },
            'vec_in_config': {
                'dimensionality': 1,
                'extents': [16 * length_scale],
                'address': {
                    'strides': [1],
                    'offset': 0
                },
                'schedule': {
                    'strides': [vec_width],
                    'offset': 18
                }
            },
            'vec_out_config': {
                'dimensionality': 2,
                'extents': [vec_width, 16 * length_scale],
                'address': {
                    'strides': [1, vec_width],
                    'offset': 0
                },
                'schedule': {
                    'strides': [1, vec_width],
                    'offset': 19
                }
            },
            'vec_constraints': [raw_constraint_vec_r, war_constraint_vec_r]
        }
    else:
        # Non-vectorized (fw=1) — single-port SRAM cannot service simultaneous
        # read+write. Stride writes/reads on different mod-4 phases so they
        # never collide (write phase 0, read phase 1), mirroring get_linear_test().
        linear_test[pw_idx] = {
            'type': Direction.IN,
            'name': 'port_w0',
            'config': {
                'dimensionality': 1,
                'extents': [16 * length_scale],
                'address': {
                    'strides': [1],
                    'offset': 0
                },
                'schedule': {
                    'strides': [4],
                    'offset': 4
                }
            },
            'vec_in_config': {
                'dimensionality': 1,
                'extents': [16 * length_scale],
                'address': {
                    'strides': [1],
                    'offset': 0
                },
                'schedule': {
                    'strides': [4],
                    'offset': 0
                }
            },
            'vec_out_config': {
                'dimensionality': 1,
                'extents': [16 * length_scale],
                'address': {
                    'strides': [1],
                    'offset': 0
                },
                'schedule': {
                    'strides': [4],
                    'offset': 4
                }
            },
            'vec_constraints': []
        }

        linear_test[pr_idx] = {
            'type': Direction.OUT,
            'name': 'port_r0',
            'config': {
                'dimensionality': 1,
                'extents': [16 * length_scale],
                'address': {
                    'strides': [1],
                    'offset': 0
                },
                'schedule': {
                    'strides': [4],
                    'offset': 17
                }
            },
            'vec_in_config': {
                'dimensionality': 1,
                'extents': [16 * length_scale],
                'address': {
                    'strides': [1],
                    'offset': 0
                },
                'schedule': {
                    'strides': [4],
                    'offset': 18
                }
            },
            'vec_out_config': {
                'dimensionality': 1,
                'extents': [16 * length_scale],
                'address': {
                    'strides': [1],
                    'offset': 0
                },
                'schedule': {
                    'strides': [4],
                    'offset': 18
                }
            },
            'vec_constraints': []
        }

    # Cross-port constraints (RAW and WAR)
    raw_scalar = vec_width if vec_width > 1 else 4
    raw_constraint = (pr_idx, 0, pw_idx, 0, LFComparisonOperator.LT.value, raw_scalar)
    war_constraint = (pw_idx, 0, pr_idx, 0, LFComparisonOperator.GT.value, 8)
    linear_test['constraints'] = [raw_constraint, war_constraint]

    return linear_test


# Expected keys in the compiler collateral JSON
COLLATERAL_REQUIRED_KEYS = {
    'controller_name', 'capacity', 'word_width', 'in_port_width', 'out_port_width',
    'bank_num', 'single_port', 'fetch_width', 'max_chaining', 'iteration_level',
    'iter_level_map', 'load_latency', 'store_latency', 'counter_ub',
    'multi_sram_accessor', 'dual_port_sram', 'wire_chain_en',
    'interconnect_in_num', 'interconnect_out_num', 'read_port', 'write_port',
}


def validate_generated_outputs(collateral_path, verilog_path,
                               expected_in_ports, expected_out_ports,
                               expected_fetch_width, expected_dimensionality,
                               expected_storage_capacity, expected_dual_port,
                               expected_data_width):
    """Validate that the collateral JSON and verilog file exist and have reasonable contents."""

    # --- Check files exist and are non-empty ---
    assert os.path.isfile(collateral_path), f"Collateral file not found: {collateral_path}"
    assert os.path.getsize(collateral_path) > 0, f"Collateral file is empty: {collateral_path}"
    assert os.path.isfile(verilog_path), f"Verilog file not found: {verilog_path}"
    assert os.path.getsize(verilog_path) > 0, f"Verilog file is empty: {verilog_path}"

    # --- Load and validate collateral JSON ---
    with open(collateral_path, 'r') as f:
        collateral = json.load(f)

    # Check all required keys are present
    missing_keys = COLLATERAL_REQUIRED_KEYS - set(collateral.keys())
    assert len(missing_keys) == 0, f"Collateral missing keys: {missing_keys}"

    # --- Validate key values are reasonable ---

    # controller_name should be a non-empty list of strings
    assert isinstance(collateral['controller_name'], list) and len(collateral['controller_name']) > 0, \
        "controller_name must be a non-empty list"

    # capacity should be a dict with positive integer values
    assert isinstance(collateral['capacity'], dict), "capacity must be a dict"
    for cname, cap in collateral['capacity'].items():
        assert isinstance(cap, int) and cap > 0, f"capacity[{cname}] must be a positive int, got {cap}"

    # Port counts should match
    assert collateral['interconnect_in_num'] == expected_in_ports, \
        f"interconnect_in_num: expected {expected_in_ports}, got {collateral['interconnect_in_num']}"
    assert collateral['interconnect_out_num'] == expected_out_ports, \
        f"interconnect_out_num: expected {expected_out_ports}, got {collateral['interconnect_out_num']}"

    # fetch_width should match vec_width
    assert collateral['fetch_width'] == expected_fetch_width, \
        f"fetch_width: expected {expected_fetch_width}, got {collateral['fetch_width']}"

    # iteration_level should be >= 1 and match expected dimensionality
    assert collateral['iteration_level'] == expected_dimensionality, \
        f"iteration_level: expected {expected_dimensionality}, got {collateral['iteration_level']}"

    # dual_port_sram should match
    assert collateral['dual_port_sram'] == expected_dual_port, \
        f"dual_port_sram: expected {expected_dual_port}, got {collateral['dual_port_sram']}"

    # Latencies should be non-negative integers
    assert isinstance(collateral['load_latency'], int) and collateral['load_latency'] >= 0, \
        f"load_latency must be a non-negative int, got {collateral['load_latency']}"
    assert isinstance(collateral['store_latency'], int) and collateral['store_latency'] >= 0, \
        f"store_latency must be a non-negative int, got {collateral['store_latency']}"

    # counter_ub should be positive
    assert isinstance(collateral['counter_ub'], int) and collateral['counter_ub'] > 0, \
        f"counter_ub must be a positive int, got {collateral['counter_ub']}"

    # bank_num should be a dict with positive values
    assert isinstance(collateral['bank_num'], dict), "bank_num must be a dict"
    for cname, bn in collateral['bank_num'].items():
        assert isinstance(bn, int) and bn > 0, f"bank_num[{cname}] must be positive, got {bn}"

    # word_width, in_port_width, out_port_width should be dicts with positive values
    for key in ('word_width', 'in_port_width', 'out_port_width'):
        assert isinstance(collateral[key], dict), f"{key} must be a dict"
        for cname, val in collateral[key].items():
            assert isinstance(val, int) and val > 0, f"{key}[{cname}] must be positive, got {val}"

    # collateral['capacity'] is in entries (see lake/spec/spec.py: cap_bytes // (word_width * data_width_bytes)).
    # Convert back to bytes per controller and check at least one matches the expected storage capacity.
    data_width_bytes = expected_data_width // 8
    capacities_bytes = {
        cname: collateral['capacity'][cname] * collateral['word_width'].get(cname, 1) * data_width_bytes
        for cname in collateral['capacity']
    }
    assert expected_storage_capacity in capacities_bytes.values(), \
        f"Expected storage_capacity {expected_storage_capacity} bytes not found in per-controller byte capacities {capacities_bytes} (raw entries: {dict(collateral['capacity'])}, word_width: {dict(collateral['word_width'])}, data_width_bytes: {data_width_bytes})"

    # Boolean fields
    assert isinstance(collateral['single_port'], dict), "single_port must be a dict"
    assert isinstance(collateral['multi_sram_accessor'], bool), "multi_sram_accessor must be a bool"
    assert isinstance(collateral['dual_port_sram'], bool), "dual_port_sram must be a bool"
    assert isinstance(collateral['wire_chain_en'], bool), "wire_chain_en must be a bool"

    print(f"Collateral and verilog validation passed: {collateral_path}")


def test_linear_read_write_qp_wf_rv(output_dir=None, storage_capacity=1024, data_width=16, physical=False, vec_width=4,
                                    tp: TestPrepper = None, test='linear', reg_file=False, dimensionality=6, opt_rv=False,
                                    in_ports=2, out_ports=2, dual_port=False, vec_capacity=2,
                                    max_extent=None, max_sequence_width=None):

    assert tp is not None

    # Put it at the lake directory by default
    if output_dir is None:
        output_dir = os.path.dirname(os.path.abspath(__file__))
        output_dir = output_dir + "/../../"

    output_dir_verilog = os.path.join(output_dir, 'inputs')

    print(f"putting verilog at {output_dir_verilog}")
    # Build the spec
    simple_four_port_spec = build_four_port_wide_fetch(storage_capacity=storage_capacity, data_width=data_width,
                                                       physical=physical, vec_width=vec_width, reg_file=reg_file,
                                                       dims=dimensionality, opt_rv=opt_rv,
                                                       in_ports=in_ports, out_ports=out_ports,
                                                       dual_port=dual_port, vec_capacity=vec_capacity,
                                                       max_extent=max_extent, max_sequence_width=max_sequence_width)
    simple_four_port_spec.visualize_graph()
    simple_four_port_spec.generate_hardware()

    # Save compiler collateral to the output directory
    collateral_path = os.path.join(output_dir_verilog, 'lake_collateral.json')
    simple_four_port_spec.save_compiler_information(collateral_path)

    # output this to simple_single_port_specthe inputs thing
    simple_four_port_spec.get_verilog(output_dir=output_dir_verilog)

    # Validate that the collateral JSON and verilog file were created and are well-formed
    verilog_path = os.path.join(output_dir_verilog, 'lakespec.sv')
    validate_generated_outputs(collateral_path, verilog_path,
                               expected_in_ports=in_ports, expected_out_ports=out_ports,
                               expected_fetch_width=vec_width, expected_dimensionality=dimensionality,
                               expected_storage_capacity=storage_capacity, expected_dual_port=dual_port,
                               expected_data_width=data_width)

    # Define the test
    num_total_ports = in_ports + out_ports
    lt = None
    use_standard_test = (in_ports == 2 and out_ports == 2 and vec_width >= 2)
    if use_standard_test:
        if test == 'linear':
            if opt_rv:
                lt = get_linear_test_rv()
            else:
                lt = get_linear_test(max_extent=max_extent)
        elif test == 'two_read':
            lt = get_two_read_test()
        elif test == 'conv_2_1':
            lt = get_conv_2_1_app()
        else:
            raise NotImplementedError(f"Cannot run test: {test}")
    else:
        # Use generic linear test for non-standard port configurations
        lt = get_linear_test_generic(in_ports, vec_width, max_extent=max_extent)

    if test == 'conv_2_1':
        max_time = 6500
    else:
        max_time = 0
        read_outs = calculate_read_out_vec(lt, vec=vec_width)
        # Now we have the output sequences
        # Need to write them out
        for pnum, sequences in read_outs.items():
            port_name = lt[pnum]['name']
            times = sequences['time']
            datas = sequences['data']
            if times[-1] > max_time:
                max_time = times[-1]
            # Need to add a cycle delay if using SRAM
            # if reg_file is False:
            #     times = [time + 1 for time in times]
            gold_output_path_data = os.path.join(output_dir, "gold", f"{port_name}_data.txt")
            gold_output_path_time = os.path.join(output_dir, "gold", f"{port_name}_time.txt")
            with open(gold_output_path_data, 'w') as file:
                for data_ in datas:
                    file.write(f"{data_}\n")
            with open(gold_output_path_time, 'w') as file:
                for time_ in times:
                    file.write(f"{time_}\n")

    # Now generate the bitstream to a file (will be loaded in test harness later)
    bs = simple_four_port_spec.gen_bitstream(lt, over=True)

    # Convert the number to a hexadecimal string
    hex_string = hex(bs)[2:]  # Remove the '0x' prefix

    bs_output_path = os.path.join(output_dir, "inputs", "bitstream.bs")

    print(f"bitstream path {bs_output_path}")

    # Write the hexadecimal string to the input folders
    with open(bs_output_path, 'w') as file:
        file.write(hex_string)

    # Write out the preprocessor args to inputs
    cfgsz_output_path = os.path.join(output_dir, "inputs", "comp_args.txt")
    config_size = simple_four_port_spec.get_total_config_size()
    config_define_str = f"+define+CONFIG_MEMORY_SIZE={config_size}\n"
    # Write out num ports for preprocessor arg
    num_ports = simple_four_port_spec.get_num_ports()
    numports_define_str = f"+define+NUMBER_PORTS={num_ports}\n"

    with open(cfgsz_output_path, 'w') as file:
        file.write(config_define_str)
        file.write(numports_define_str)

    data_sizes = get_data_sizes(lt, num_ports=num_total_ports)
    tp.add_pargs(data_sizes)
    # tp.add_pargs(('max_time', max_time + int((max_time / 10))))
    tp.add_pargs(('max_time', max_time + 15))
    tp.add_pargs(('static', 1))


if __name__ == "__main__":

    parser = argparse.ArgumentParser(description='Simple Dual Port')
    parser.add_argument("--storage_capacity", type=int, default=1024)
    parser.add_argument("--data_width", type=int, default=16)
    parser.add_argument("--fetch_width", type=int, default=4)
    parser.add_argument("--clock_count_width", type=int, default=64)
    parser.add_argument("--in_ports", type=int, default=2)
    parser.add_argument("--out_ports", type=int, default=2)
    parser.add_argument("--dimensionality", type=int, default=6)
    parser.add_argument("--test", type=str, default="linear")
    parser.add_argument("--reg_file", action="store_true")
    parser.add_argument("--tech", type=str, default="GF")
    parser.add_argument("--physical", action="store_true")
    parser.add_argument("--outdir", type=str, default=None)
    parser.add_argument("--opt_rv", action="store_true")
    parser.add_argument("--dual_port", action="store_true")
    parser.add_argument("--vec_capacity", type=int, default=2)
    parser.add_argument("--max_extent", type=int, default=None)
    parser.add_argument("--max_sequence_width", type=int, default=None)
    args = parser.parse_args()

    print("Preparing hardware test")

    in_ports = args.in_ports
    out_ports = args.out_ports
    fw = args.fetch_width

    tp = TestPrepper(base_dir=args.outdir)
    hw_test_dir = tp.prepare_hw_test()
    print(f"Put hw test at {hw_test_dir}")

    test_linear_read_write_qp_wf_rv(output_dir=hw_test_dir, storage_capacity=args.storage_capacity, data_width=args.data_width,
                                    physical=args.physical, vec_width=fw, tp=tp, test=args.test,
                                    reg_file=args.reg_file, dimensionality=args.dimensionality,
                                    opt_rv=args.opt_rv, in_ports=in_ports, out_ports=out_ports,
                                    dual_port=args.dual_port, vec_capacity=args.vec_capacity,
                                    max_extent=args.max_extent, max_sequence_width=args.max_sequence_width)
