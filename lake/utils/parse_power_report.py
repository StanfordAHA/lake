import glob
import csv
import argparse


def parse_power(filename, fence_width=32):
    return_dict = {}
    power_dict = {}
    with open(filename) as f:
        contents = f.readlines()[16:]
        tile_list = []
        for i, c in enumerate(contents):
            split_list = c.split()
            if len(split_list) >= 6:
                instance_name = " ".join(split_list[:2])
                if instance_name.startswith("Tile_X"):                    # tile
                    # print(instance_name)
                    x_coord = int(instance_name.split("_")[1][1:], base=16)
                    if x_coord < fence_width:
                        if "PE" in instance_name or "MemCore" in instance_name:
                            tile_list.append(i)
                            power_dict[instance_name] = float(split_list[-2])
                        else:                                             # io tile
                            power_dict[instance_name] = {"Other": float(split_list[-2])}
                        
                elif instance_name == "global_buffer (global_buffer)":    # glb
                    return_dict["GLB"] = split_list[-2]
                elif instance_name.startswith("glb_tile"):
                    power_dict[instance_name] = {"GLB": float(split_list[-2])}

    # at this point power_dict is the total power number
    for j in range(len(tile_list)):
        top_name = " ".join(contents[tile_list[j]].split()[:2])
        top_level = len(contents[tile_list[j]])- len(contents[tile_list[j]].lstrip(' '))
        if j < len(tile_list) - 1:
            tile_content = contents[tile_list[j] + 1:tile_list[j + 1]]
        else:
            tile_content = contents[tile_list[j] + 1:]
        
        tile_dict = {}
        tile_dict["total"] = power_dict[top_name]
        for i, c in enumerate(tile_content):
            c_level = len(c)- len(c.lstrip(' '))
            if c_level == top_level:
                tile_content = tile_content[:i]
                break
            split_list = c.split()
            if len(split_list) >= 6:
                instance_name = " ".join(split_list[:2])
                total_power = float(split_list[-2])
                for keyword in ["MemCore", "SB", "CB", "PE", "PondCore"]:
                    if instance_name.startswith(keyword):
                        if tile_dict.get(keyword) is not None:
                            tile_dict[keyword] += total_power
                        else:
                            tile_dict[keyword] = total_power
                        break
        # print(len(tile_content))
        if "MemCore" in top_name:
            tile_dict["Other"] = power_dict[top_name] - tile_dict["MemCore"] - tile_dict["SB"] - tile_dict["CB"]
            assert tile_dict.get("PE") is None
            assert tile_dict.get("PondCore") is None
            assert tile_dict.get("Other") > 0
        elif "PE" in top_name:
            # pondcore was count in PE tile
            # tile_dict["PE"] += tile_dict["PondCore"]
            # del tile_dict["PondCore"]
            tile_dict["Other"] = power_dict[top_name] - tile_dict["PE"] - tile_dict["SB"] - tile_dict["CB"] - tile_dict["PondCore"]
            assert tile_dict.get("MemCore") is None
            assert tile_dict.get("Other") > 0
        else:
            tile_dict["Other"] = power_dict[top_name]

        power_dict[top_name] = tile_dict

    # now power_dict is the detail info for each tile
    # [print(key,':',value) for key, value in power_dict.items()]

    for keyword in ["MemCore", "SB", "CB", "PE", "Other", "PondCore", "total"]:
        return_dict[keyword] = 0
    for k, tile_content in power_dict.items():
        for keyword in ["MemCore", "SB", "CB", "PE", "Other", "PondCore"]:
            if tile_content.get(keyword) is not None: 
                return_dict[keyword] += tile_content[keyword]
                return_dict["total"] += tile_content[keyword]
    print(return_dict)
    return power_dict, return_dict

def exp_result(foldername, fence_width=32):
    rpt_file = glob.glob(f"./{foldername}/*.rpt")[0]
    summary, power_rpt = parse_power(rpt_file, fence_width)
    
    # [print(key,':',value) for key, value in power_rpt.items()]

    with open(f'./{foldername}/{foldername}.csv', 'w', ) as myfile:
        wr = csv.writer(myfile, quoting=csv.QUOTE_ALL)
        head = ["instance", "total_power"]
        wr.writerow(head)
        for k, v in power_rpt.items():
            row = [k, float(v)*32/fence_width]
            wr.writerow(row)

    # [print(key,':',value) for key, value in summary.items()]
    with open(f'./{foldername}/{foldername}_summary.csv', 'w', ) as myfile:
        wr = csv.writer(myfile, quoting=csv.QUOTE_ALL)
        head = ["instance", "SB", "CB", "MemCore", "PE", "Other", "GLB", "PondCore"]
        wr.writerow(head)
        for k, v in sorted(summary.items()):
            row = [k]
            for i in range(1, len(head)):
                row.append(v.get(head[i], "None"))
            wr.writerow(row)
  
def find_fence(app_name):
    fence_list = {"gaussian":4, "unsharp":8, "camera":32, "harris": 8}
    for k in fence_list.keys():
        if k in app_name:
            return fence_list[k]
    print("app name not match")

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('--app_name', type=str, required=True)
    # app_name = "gaussian"
    args = parser.parse_args()
    fence_width = find_fence(args.app_name)
    print("app:", args.app_name)
    print("fence_width:", fence_width)
    exp_result(args.app_name, fence_width=fence_width)


