import json
import os
import argparse

parser = argparse.ArgumentParser()
parser.add_argument("--json_file_name", type=str, required=True)
args = parser.parse_args()
json_file = args.json_file_name

# json_file = "./resnet_design_top.json"
module_name_list = ["input_cgra_stencil_ub", "kernel_cgra_stencil_ub", "output_cgra_stencil_ub", "resnet_output_stationary"]

folder_name = json_file.split(".json")[0]
if not os.path.isdir(f"{folder_name}"):
    os.mkdir(f"{folder_name}")

with open(json_file) as jf:
    design_top = json.load(jf)
    design_top = design_top["namespaces"]["global"]["modules"]
    # module_list = list(design_top.keys())
    
    for module_name in module_name_list:

        module_top = design_top[module_name]["instances"]
        instance_list = list(module_top.keys()) 
        instance_list = [instance for instance in instance_list if "metadata" in module_top[instance].keys() ]
        print(len(instance_list))
        # print(instance_list) 
        for instance in instance_list:
            config = {}
        
            # ============
            # important to set mode to "UB" !!!
            # ============
            config["mode"] = "UB"

            if "config" in module_top[instance]["metadata"].keys():
                config["config"] = module_top[instance]["metadata"]["config"]
                if "in2agg_0" not in config["config"]:
                    continue
                # print(config)
            else:
                config["config"] = module_top[instance]["metadata"]
                if "in2agg_0" not in config["config"]:
                    continue
        
            genargs = module_top[instance]["genargs"]
            for key in genargs.keys():
                config[key] = genargs[key][1]

            if not os.path.isdir(f"{folder_name}/{module_name}"):
                os.mkdir(f"{folder_name}/{module_name}")
            out_file = open(f"./{folder_name}/{module_name}/{instance}.json", "w")
            json.dump(config, out_file, indent=4)
            out_file.close()

       


