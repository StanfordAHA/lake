import pytest
import kratos as kts
import fault
import tempfile
from lake.top.lake_top import LakeTop
from lake.models.laketop2_model import LakeTopModel
from test_config.random_config import gen_random_config
import time
import json
import os

data_width = 16
mem_width = 64
mem_depth = 512
interconnect_input_ports = 2
interconnect_output_ports = 2

@pytest.mark.parametrize("area_opt", [True, False])
@pytest.mark.parametrize("num_tests", [2])
def test_lake_strg_ub_vec(area_opt, num_tests):
    lake_dut = LakeTop(data_width=data_width,
                  mem_width=mem_width,
                  mem_depth=mem_depth,
                  banks=1,
                  input_iterator_support=6,
                  output_iterator_support=6,
                  interconnect_input_ports=interconnect_input_ports,
                  interconnect_output_ports=interconnect_output_ports,
                  agg_height=2,                     ############### necessary!
                  read_delay=1,
                  rw_same_cycle=False,
                  config_width=16,
                  config_data_width=32,
                  config_addr_width=8,
                  add_clk_enable=True,
                  add_flush=True,
                  stencil_valid=True,
                  area_opt=area_opt,                #######################
                  name="LakeTop")

    magma_dut = kts.util.to_magma(lake_dut.dut,
                                  flatten_array=True,
                                  check_multiple_driver=False,
                                  optimize_if=False,
                                  check_flip_flop_always_ff=False)

    tester = fault.Tester(magma_dut, magma_dut.clk)
    
    data_in_0 = [i for i in range(1,3000)]
    data_in_1 = [i for i in range(3000,6000)]
    data_in_list = [data_in_0, data_in_1]
    
    test_step = 0
    while test_step < num_tests:
        # ============= 
        # generate random config for testing
        # =============
        filePath = f"/aha/lake/tests/test_config/random/{test_step}_{area_opt}.json"
        if os.path.exists(filePath):
            os.remove(filePath)
        gen_random_config(name=test_step, area_opt=area_opt)
        f = open(filePath)
        config_data = json.load(f)
        f.close()

        if "in2agg_1" in config_data["config"].keys():
            num_inputs = 2
            num_outputs = 2
        else:
            num_inputs = 1
            num_outputs= 1

        # =============
        # fix inconsistent num_input with config
        # =============
        if config_data["num_inputs"] != num_inputs:
            print("Fix inconsist num_inputs")
            print("num_inputs should be", num_inputs)
            config_data["num_inputs"] = num_inputs
            config_data["num_outputs"] = num_outputs
       

        # ============= 
        # use functional model to generate golden data
        # ============= 
        laketop_model = LakeTopModel(data_width=data_width,
                                     mem_width=mem_width,
                                     mem_depth=mem_depth,
                                     interconnect_input_ports=interconnect_input_ports,
                                     interconnect_output_ports=interconnect_output_ports,
                                     area_opt=area_opt)         #######
        
        # print(config_data)
        configs = lake_dut.dut.get_bitstream(config_json=config_data)
        # original config_data might be changed by flattening after get_bit_stream
        
        # ==============
        # need to make sure get_bitstream can generate right one
        # ==============
        laketop_model.set_config(config_data)
        
        start_time = time.time()
        print("\nstart generate golden pattern.....")
        golden_data_out_list, valid_out_list, data_in_sched_list = laketop_model.interact(data_in_list)
        if golden_data_out_list == None:
            continue

        print("generate golden data successfully!", len(golden_data_out_list[0]))
        end_time = time.time()
        print("generate golden time: ", end_time - start_time)

        # ============
        # check output only zeros
        # cannot test whether we get the correct data
        # if all zeros, generate config again
        # ============
        pattern = set(golden_data_out_list[0])
        if len(golden_data_out_list) == 2:
            pattern.update(golden_data_out_list[1])
        if len(pattern) == 1:
            print("===================== read only zeros ================================")
            continue
        

        # ============================ Finish config generation ==========================

        print(f"============= test {test_step}, {area_opt} =============")
        print(config_data)
        # configs = lake_dut.dut.get_bitstream(config_json=config_data)
        # original config_data might be changed by flattening after get_bit_stream
    
        tester.zero_inputs()
        tester.circuit.clk = 0
        tester.circuit.clk_en = 1
        tester.circuit.tile_en = 1
        tester.circuit.rst_n = 1

        for key, value in configs:
            setattr(tester.circuit, key, value)
        tester.eval()
        tester.step(2)   ##

        tester.circuit.flush = 1
        tester.eval()
        tester.step(2)
        tester.circuit.flush = 0

        # valid_out should has same length
        run_cycle = len(valid_out_list[0])
        input_cycle = max(len(data_in_sched) for data_in_sched in data_in_sched_list)
        print("\nexpect to run ", run_cycle, "cycles.")
        # print("use ", input_cycle, "cycles to input data.")


        # =============================== Start testing ==================================
        start_time = time.time()
        for cycle_count in range(run_cycle):
            for i in range(num_inputs):
            
                # ===============================
                # write
                if cycle_count <= input_cycle:
                    if cycle_count < len(data_in_list[i]):
                        setattr(tester.circuit, f"LakeTop_input_width_17_num_{i+2}", data_in_list[i][cycle_count])
                    elif cycle_count < len(data_in_sched_list[i]): 
                        setattr(tester.circuit, f"LakeTop_input_width_17_num_{i+2}", 0)
            
                # ===============================
                # read
                # check valid_out correct until the last cycle
                # check data_out until last valid_out, don't care afterward
                getattr(tester.circuit, f"LakeTop_output_width_1_num_{i}").expect(valid_out_list[i][cycle_count])
                if cycle_count < len(golden_data_out_list[i]):
                    getattr(tester.circuit, f"LakeTop_output_width_17_num_{i}").expect(golden_data_out_list[i][cycle_count])

            tester.eval()
            tester.step(2)

        print("\nfinish running ", run_cycle, "cycles.")
        end_time = time.time()
        print("run cycle time: ", end_time - start_time)


        start_time = time.time()
        with tempfile.TemporaryDirectory() as tempdir:
            tempdir = "temp_random"
            tester.compile_and_run(target="verilator",
                                directory=tempdir,
                                magma_output="verilog",
                                #flags=["-Wno-fatal"])
                                flags=["-Wno-fatal", "--trace"])
        end_time = time.time()
        print("compile time: ", end_time - start_time)
       

        # =============
        # clear tester for the next testing config
        # =============
        tester.clear()
        test_step += 1

if __name__ == "__main__":
    
    test_lake_strg_ub_vec(area_opt=False, num_tests=1)
    
    # zero_output = test_lake_strg_ub_vec(config_datas[-1], area_opt=True)
    # zero_output = test_lake_strg_ub_vec(config_datas[-1], area_opt=False)
    
