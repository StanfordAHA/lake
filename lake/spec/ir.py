import numpy as np
from basic import Counter, PWA
from kratos import *

class bound:
    def __init__(self, n, _lb, _ub):
        self.name = n
        self.lb = _lb
        self.ub = _ub

    def getLB(self):
        return self.lb

    def getUB(self):
        return self.ub

    def inBound(self, val):
        return val >= self.lb and val <= self.ub

class var:
    def __init__(self, n,  _lb, _ub):
        #self.name = n
        #self.lb = _lb
        #self.ub = _ub
        self.name = n
        self.bd = bound(n + "_bd", _lb, _ub)
        self.val = self.bd.getLB()

    def update(self)->bool:
        if self.val < self.bd.getUB():
            self.val += 1
            return False
        else:
            self.val = self.bd.getLB()
            return True

    def getVal(self):
        return self.val

    def generate_verilog(self)->Generator:
        return Counter()

class expr:
    def __init__(self, _var_list:list, bd_w_list: list):
        '''
        piecewise linear expression, weight is a dictionary
        the key save the domain of that piece, and value is
        the weight which gives the function expression
        '''
        self.var_list = _var_list
        self.weight_list = []
        self.bd_list = []
        for bd, w in bd_w_list:
            self.weight_list.append(w)
            self.bd_list.append(bd)

        self.piece = len(bd_w_list)
        self.input_dim = len(self.var_list)

    def eval(self)->int:
        var_list = [var.getVal() for var in self.var_list]
        for nd_bd, weights in zip(self.bd_list, self.weight_list):
            list_inBD = [bd.inBound(val) for bd, val in zip(nd_bd, var_list)]
            if all(list_inBD):
                val = 0
                val += weights[-1]
                for idx, var in enumerate(self.var_list):
                    val += weights[idx] * var.getVal()
                return val
        assert False, "variable exceeded all pieces of bounds."

    def generate_verilog(self)->Generator:
        return PWA(piece_num=self.piece, input_dim=self.input_dim)

class map:
    def __init__(self, _var_list:list, _expr_list:list):
        '''
        map is a list of variable map to a list of expression,
        basically a [in_dim] vector of varibles map to a [out_dim]
        vector of variable
        '''
        self.var_list = _var_list
        self.expr_list = _expr_list

        #constraints
        self.in_dim = len(self.var_list)
        self.out_dim = len(self.expr_list)
        self.flatten_eval_vec = [1 for _ in range(self.out_dim)]

    def set_flatten_eval_vec(self, vec: list):
        self.flatten_eval_vec = vec

    def eval(self):
        ret = []
        for expr in self.expr_list:
            ret.append(expr.eval())
        return np.dot(ret, self.flatten_eval_vec)

    def getDomain(self):
        ret = {}
        for var in self.var_list:
            ret[var.name] = var.getVal()
        return ret

    def update(self):
        for var in self.var_list:
            inc_next = var.update()
            if inc_next == False:
                break

    def generate_verilog(self)->Generator:
        ret = Generator("map")
        clk = ret.clock("clk")
        rst = ret.reset("rst")
        out = ret.var("cout", 32, size = self.in_dim, packed = True, explicit_array = True)
        cnt_list = [var.generate_verilog() for var in self.var_list]
        for idx, cnt in enumerate(cnt_list):
            bd = ret.input(f"bd_{idx}", 32)
            ret.add_child(f"cnt_{idx}", cnt, clk = "clk", rst_n = "rst",
                          bound = bd, current_val = out[idx])

        pwa_list = [expr.generate_verilog() for expr in self.expr_list]
        for idx, pwa in enumerate(pwa_list):
            output = ret.output(f"expr_dim_{idx}", 32)
            lb_ = ret.input(f"lb_in_{idx}", 32, size = [pwa.piece_num, pwa.input_dim],
                            packed = True, explicit_array = True)
            ub_ = ret.input(f"ub_in_{idx}", 32, size = [pwa.piece_num, pwa.input_dim],
                            packed = True, explicit_array = True)
            w = ret.input(f"w_in_{idx}", 32, size = [pwa.piece_num, pwa.input_dim+1],
                            packed = True, explicit_array = True)
            ret.add_child(f"expr_{idx}", pwa,
                          iterator = "cout",
                          value = output,
                          lb = lb_,
                          ub = ub_,
                          weight = w)

        return ret

