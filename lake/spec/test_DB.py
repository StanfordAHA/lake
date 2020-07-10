from ir import *


def test_DB():
    '''
    Conv 33 linebuffer  on 6x6 image
    schedule: {input[i_wr, j_wr]->[i_wr]: 0<=i_wr<=5, j_wr=0} and
                {input[i_wr, j_wr] -> [i_wr + j_wr * 12 - 6] : 0<=i_wr<= 5, 1<=j_wr<=5}
    schedule: {output[i_rd, j_rd]->[i_rd + 12*j_rd + 6]: 0<=i_wr<=11, 0<=j_wr<=5}
    '''

    i = var("i_wr", 0, 5)
    j = var("j_wr", 0, 5)

    # accessor for input
    acc_in = expr([i, j], [([bound("i_wo", 0, 5), bound("j_wo", 0, 0)], [1, 0, 0]),
                           ([bound("i_ss", 0, 5), bound("j_ss", 1, 5)], [1, 12, -6])])
    acc_ctr_in = map([i, j], [acc_in])

    i_addr = var("i_in", 0, 5)
    j_addr = var("j_in", 0, 5)
    # address generator for input
    addr_in = expr([i_addr, j_addr], [([bound("bd_i", 0, 5), bound("bd_j", 0, 5)], [1, 6, 0])])
    addr_gen_in = map([i_addr, j_addr], [addr_in])

    ir = var("i_rd", 0, 11)
    jr = var("j_rd", 0, 5)

    acc_out = expr([ir, jr], [([bound("ir_ss", 0, 11), bound("jr_ss", 0, 5)], [1, 12, 6])])
    acc_ctr_out = map([ir, jr], [acc_out])

    ir_addr = var("i_out", 0, 5)
    kr_addr = var("k_out", 0, 1)
    jr_addr = var("j_out", 0, 5)
    addr_out = expr([ir_addr, kr_addr, jr_addr],
                    [([bound("bd_i", 0, 5), bound("bd_k", 0, 1), bound("bd_j", 0, 5)], [1, 0, 6, 0])])
    addr_gen_out = map([ir_addr, kr_addr, jr_addr], [addr_out])

    # test stream
    cycle = 78
    for itr in range(cycle):
        sched_rd = acc_ctr_out.eval()
        sched_wr = acc_ctr_in.eval()
        is_rd = (sched_rd == itr)
        is_wr = (sched_wr == itr)
        wr_idx = acc_ctr_in.getDomain()
        rd_idx = acc_ctr_out.getDomain()
        # print ("cycle:", itr, " write idx: ", wr_idx, "sched_wr:", sched_wr)
        if is_wr:
            print("cycle:", itr, " write ", wr_idx)
            print("write addr: ", addr_gen_in.eval())
            acc_ctr_in.update()
            addr_gen_in.update()
        if is_rd:
            print("cycle:", itr, " read ", rd_idx)
            print("read addr: ", addr_gen_out.eval())
            acc_ctr_out.update()
            addr_gen_out.update()
        print("\n***************************************\n")


if __name__ == "__main__":
    test_DB()
