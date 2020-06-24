from accessor import Accessor

def test_Accessor():

    '''
    write->|Agg|->agg2sram->|SRAM|->sram2tb->|TB|->read
    '''

    #1 set of counter
    #Access(size = 4)
    #4 sets of counters
    #accesor[4]

    hw_spec = Accessor()

    #build hardware
    hw_spec.setConstraint(
        st_size = 4,
        var_dim = 6,
        expr_dim = 1,
        expr_piece_dim = 3
    )
    #already could generate hw from this step

    #hw_spec.setConfigTemplate

    #set configuration for a specific application
    '''
    write->|SRAM|->read
    '''
    hw_spec.setConfig(
        #debug axuilary
        st_name_list = ["write", "read"],
        #real hardware config
        st_size = 2,
        depth = 36,
        var_dim = [2, 2],
        var_range_list = [[5, 5], [3, 3]],
        expr_dim = [1, 1],
        #FIXME: bound need lower bound
        expr_config = [[[([5, 5], [1, 6, 0])]], [[([3, 3], [1, 6, 14])]]]
    )

    hw_spec.checkConstraint()

    sim_cycle = 36

    for cyc in range(sim_cycle):
        print("\n**********cycle: ", cyc, "**********\n")
        hw_spec.exeComb()
        hw_spec.print_sim_info()
        hw_spec.exeSeq()

if __name__ == "__main__":
    test_Accessor()
