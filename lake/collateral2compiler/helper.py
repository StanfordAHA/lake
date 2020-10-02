from kratos import *
import json


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


def get_json(mem_collateral, edge_collateral, filename="collateral2compiler.json"):  # will also include edge collateral to form Lake object
    all_collateral = {}
    all_collateral["memories"] = mem_collateral
    all_collateral["edges"] = edge_collateral

    with open (filename, 'w') as outfile:
        json.dump(all_collateral, outfile, indent=4)
