import glob
import csv
import argparse


def parse_area(filename):
    return_dict = {}
    area_dict = {}
    with open(filename) as f:
        contents = f.readlines()[18:]
        for i, c in enumerate(contents):
            c = c.rstrip()
            split_list = c.split()
            instance_name = split_list[0]
            total_area = split_list[-1]
            area_dict[instance_name] = total_area

    # print(area_dict)
    return_dict["SRAM_macro"] = float(area_dict["memory_0"])
    return_dict["UB_controller"] = float(area_dict["strg_ub_vec_inst"])    # strg_ub_vec
    return_dict["config_register"] = float(area_dict["MemCore_inst0"]) - float(area_dict["MemCore_inner_W_inst0"])
    return_dict["other_controller_mode"] = float(area_dict["mem_ctrl_stencil_valid_flat"]) + float(area_dict["mem_ctrl_strg_ram_64_512_delay1_flat"])
    return_dict["sparse_feature"] = float(area_dict["MemCore_inner"]) - return_dict["SRAM_macro"] - return_dict["UB_controller"] - return_dict["other_controller_mode"]
    return_dict["CBSB"] = float(area_dict["Tile_MemCore"]) - float(area_dict["MemCore_inst0"])
    return_dict["total_area"] = float(area_dict["Tile_MemCore"])

    return return_dict


def exp_result(foldername):
    total_table = {}
    total_names = []
    files = glob.glob(f"./{foldername}/*.rpt")
    files.sort()
    for filename in files:
        name = filename.split("/")[-1].split("_")[:-1]
        name = "_".join(name)
        total_table[name] = parse_area(filename)
        total_names.append(name)

    # [print(key, ':', value) for key, value in total_table.items()]

    with open(f'./{foldername}/{foldername}.csv', 'w', ) as myfile:
        wr = csv.writer(myfile, quoting=csv.QUOTE_ALL)
        head = ["optimization", "SRAM_macro", "config_register", "UB_controller", "other_controller_mode", "sparse_feature", "CBSB", "total_area"]
        wr.writerow(head)
        print(total_names)
        for name in total_names:
            row = [name]
            for i in range(1, 4):
                row.append(total_table[name][head[i]])
            wr.writerow(row)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('--name', type=str, required=True)
    args = parser.parse_args()
    # exp_result("id_dim_out")
    # exp_result("config_bitwidth_out")
    # exp_result("sharing_out")
    exp_result(args.name)
