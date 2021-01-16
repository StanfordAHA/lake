from kratos import *
import json


def make_params(name,
                capacity,
                read_port_width=0,
                write_port_width=0,
                read_write_port_width=0,
                num_chain=1,
                use_macro=False,
                macro_name="SRAM_default_name",
                rw_same_cycle=False):

    assert num_chain >= 1, "Can chain 1 or more Lake objects"

    params_dict = {"name": name,
                   "capacity": capacity,
                   "use_macro": use_macro,
                   "rw_same_cycle": rw_same_cycle,
                   "macro_name": macro_name,
                   "chaining": not (num_chain == 1),
                   "num_chain": num_chain}

    if read_port_width != 0:
        params_dict["read_port_width"] = read_port_width
    if write_port_width != 0:
        params_dict["write_port_width"] = write_port_width
    if read_write_port_width != 0:
        params_dict["read_write_port_width"] = read_write_port_width

    return params_dict


def get_params(gen, collateral, name_id):
    orig_gen = Generator("original")
    gen_dict = vars(gen).copy()

    gen_dict = dict((key, value) for key, value in gen.__dict__.items()
                    if not callable(value) and not key.startswith('__'))

    for key in vars(orig_gen):
        if key in gen_dict:
            del gen_dict[key]

    if name_id + "_name" not in gen_dict:
        idx = len(collateral)
        collateral[name_id + f"_{idx}"] = gen_dict
    else:
        collateral[gen_dict[name_id + "_name"]] = gen_dict


def get_json(mem_collateral,
             edge_collateral,
             input_edge_collateral,
             output_edge_collateral,
             filename="collateral2compiler.json"):
    all_collateral = {}
    all_collateral["memories"] = mem_collateral
    all_collateral["edges"] = edge_collateral
    all_collateral["input_edges"] = input_edge_collateral
    all_collateral["output_edges"] = output_edge_collateral

    with open (filename, 'w') as outfile:
        json.dump(all_collateral, outfile, indent=4)


def get_edge_name(edge):
    # get unique edge_name identifier for hardware modules
    from_sigs, to_sigs = "", ""
    for e in edge["from_signal"]:
        from_sigs += e + "_"
    for e in edge["to_signal"]:
        to_sigs += e + "_"

    return from_sigs + to_sigs + "edge"
