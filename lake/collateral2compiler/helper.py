from kratos import *
import json


def get_memory_params(memory, mem_collateral):
    orig_gen = Generator("original")
    mem_dict = vars(memory).copy()

    mem_dict = dict((key, value) for key, value in memory.__dict__.items()
                    if not callable(value) and not key.startswith('__'))

    for key in vars(orig_gen):
        if key in mem_dict:
            del mem_dict[key]

    if "mem_name" not in mem_dict:
        mem_idx = len(mem_collateral)
        mem_collateral[f"mem_{mem_idx}"] = mem_dict
    else:
        mem_collateral[mem_dict["mem_name"]] = mem_dict


def get_json(mem_collateral, filename="collateral2compiler.json"):  # will also include edge collateral to form Lake object
    all_collateral = {}
    all_collateral["memories"] = mem_collateral

    with open (filename, 'w') as outfile:
        json.dump(all_collateral, outfile, indent=4)
