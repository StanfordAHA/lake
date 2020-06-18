from accessor import Accessor


def test_Accessor():

    hw_spec = Accessor()
    hw_spec.setConfig(
        st_size = 2,
        depth = 36,
        st_name_list = ["write", "read"],
        var_dim = [2, 2],
        var_range_list = [[5, 5], [3, 3]],
        expr_dim_list = [1, 1],
        expr_config = [[[([5, 5], [1, 6, 0])]], [[([3, 3], [1, 6, 14])]]]
    )

    sim_cycle = 36

    for cyc in range(sim_cycle):
        print("\n**********cycle: ", cyc, "**********\n")
        hw_spec.exeComb()
        hw_spec.print_sim_info()
        hw_spec.exeSeq()

if __name__ == "__main__":
    test_Accessor()
