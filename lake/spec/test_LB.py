from ir import *


def test_LB():
    '''
    Conv 33 linebuffer  on 6x6 image
    schedule: {input[i_wr, j_wr]->[i_wr + 6*j_wr + 0]: 0<=i_wr<=5, 0<=j_wr<=5}
    schedule: {output[i_rd, j_rd]->[i_rd + 6*j_rd + 14]: 0<=i_wr<=3, 0<=j_wr<=3}
    '''

    # input accessor
    i = var("i_wr", 0, 5)
    j = var("j_wr", 0, 5)
    acc_in = expr([i, j], [([bound("i_ss", 0, 5), bound("j_ss", 0, 5)], [1, 6, 0])])
    acc_ctr_in = map([i, j], [acc_in])

    # input address generator
    i = var("i_in", 0, 5)
    j = var("j_in", 0, 5)
    addr_in = expr([i, j], [([bound("i_ss", 0, 5), bound("j_ss", 0, 5)], [1, 6, 0])])
    addr_gen_in = map([i, j], [addr_in])

    # output accessor
    ir = var("i_rd", 0, 3)
    jr = var("j_rd", 0, 3)
    acc_out = expr([ir, jr], [([bound("ir_ss", 0, 3), bound("jr_ss", 0, 3)], [1, 6, 14])])
    acc_ctr_out = map([ir, jr], [acc_out])

    # output address generator : the right bottom of the 3x3 window
    ir = var("i_out", 0, 3)
    jr = var("j_out", 0, 3)
    addr_out = expr([ir, jr], [([bound("ir_ss", 0, 3), bound("jr_ss", 0, 3)], [1, 6, 14])])
    addr_gen_out = map([ir, jr], [addr_out])
    # test stream
    cycle = 36
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
            print("write address: ", addr_gen_in.eval())
            acc_ctr_in.update()
            addr_gen_in.update()
        if is_rd:
            print("cycle:", itr, " read ", rd_idx)
            print("read address: ", addr_gen_out.eval())
            acc_ctr_out.update()
            addr_gen_out.update()
        print("\n***************************************\n")


if __name__ == "__main__":
    test_LB()
