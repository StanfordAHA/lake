from ir import map, var, bound, expr


'''
I am trying to use this class to demonstrate what a spec is.
It should contain all the information for generate hardware,
mapping constraint and functional model.
'''


class Accessor:
    def __init__(self):
        self.map_dict = {}
        self.config_cons_dict = {}

    '''
    This method should define all the generator parameter
    We need to add a parser/decorator to get all the information
    for hardware generation, I think generator parameter also
    subsume all the constraint that compiler need to be awared.
    '''
    def setConstraint(self, **kwargs):
        for k, v in kwargs.items():
            self.config_cons_dict[k] = bound(k + "_bound", 0, v)

    def checkConstraint(self):
        for k, bd in self.config_cons_dict.items():
            if k == "st_size":
                assert bd.inBound(len(self.map_dict)), "too much expr, controller number exceeded!"
            elif k == "var_dim":
                for _, ctrl in self.map_dict.items():
                    assert bd.inBound(ctrl.in_dim), "iterator dimension exceeded!"
            elif k == "expr_dim":
                for _, ctrl in self.map_dict.items():
                    assert bd.inBound(ctrl.out_dim), "output dimension exceeded!"
            elif k == "expr_piece_dim":
                for _, ctrl in self.map_dict.items():
                    for e in ctrl.expr_list:
                        assert bd.inBound(len(e.bd_list)), "piecewise expression pieces exceeded!"

        print("All constraints satisfied!")

    '''
    This method should define all the configuration register
    We need to add a parser/decorator to get all the information
    for hardware generation
    '''
    def setConfig(self, **kwargs):
        '''
        example of config:
            st_name_list: this is for prettyprint
            st_size: 2
            depth: 36
            var_dim: [2, 2]
            expr_dim: [1, 1]
            var_range_list: [[5, 5], [3,3]]
            expr_config:[different accessor
                          [multi dimension expr
                            [multi-piece
                              ([5, 5], [1, 6, 0])]
                          ],
                          [
                            [([3, 3], [1, 6, 14])]
                          ]
                        ]
        '''
        for k, v in kwargs.items():
            if k == "st_size":
                st_size = v
            elif k == "depth":
                depth = v
            elif k == "st_name_list":
                st_name = v
            elif k == "var_dim":
                var_dim = v
            elif k == "var_range_list":
                var_range_list = v
            elif k == "expr_dim":
                expr_dim = v
            elif k == "expr_config":
                expr_config = v

        var_l = [var("cycle", 0, depth)]
        expr_l = [expr(var_l, [([bound("bd", 0, depth)], [1, 0])])]
        self.cycle_cnt = map(var_l, expr_l)

        for i in range(st_size):
            var_list = []
            for in_dim in range(var_dim[i]):
                tmp = var("i" + "_" + str(i) + "_" + str(in_dim), 0, var_range_list[i][in_dim])
                var_list.append(tmp)
            expr_list = []
            for out_dim in range(expr_dim[i]):
                pwa = expr_config[i][out_dim]
                pw_list = []
                for bd_info, w in pwa:
                    bd_list = [bound("bd" + "_" + str(i) + "_" + str(cnt), 0, bd) for cnt, bd in enumerate(bd_info)]
                    pw_list.append((bd_list, w))
                tmp_expr = expr(var_list, pw_list)
                expr_list.append(tmp_expr)

            self.map_dict[st_name[i]] = map(var_list, expr_list)

    '''
    The method below should define the functional model
    I feel that I am writing verilog/simulator,
    I do not know if this is the best way. But it may
    easily generate hw to some extent.
    '''
    def exeSeq(self):
        self.cycle_cnt.update()
        for k, is_update in self.is_update_dict.items():
            if is_update:
                self.map_dict[k].update()

    def exeComb(self):
        itr = self.cycle_cnt.eval()
        cnt_dict = {k: m.eval() for k, m in self.map_dict.items()}
        self.is_update_dict = {k: cnt == itr for k, cnt in cnt_dict.items()}

    '''
    Simulator print out info, add an data interface this will drive memory port
    '''
    def print_sim_info(self):
        for k, is_update in self.is_update_dict.items():
            if is_update:
                print(k, ": ", self.map_dict[k].getDomain())
