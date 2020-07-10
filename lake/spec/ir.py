import numpy as np


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
    def __init__(self, n, _lb, _ub):
        # self.name = n
        # self.lb = _lb
        # self.ub = _ub
        self.name = n
        self.bd = bound(n + "_bd", _lb, _ub)
        self.val = self.bd.getLB()

    def update(self) -> bool:
        if self.val < self.bd.getUB():
            self.val += 1
            return False
        else:
            self.val = self.bd.getLB()
            return True

    def getVal(self):
        return self.val


class expr:
    def __init__(self, _var_list: list, bd_w_list: list):
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

    def eval(self) -> int:
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


class map:
    def __init__(self, _var_list: list, _expr_list: list):
        '''
        map is a list of variable map to a list of expression,
        basically a [in_dim] vector of varibles map to a [out_dim]
        vector of variable
        '''
        self.var_list = _var_list
        self.expr_list = _expr_list
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
            if inc_next is False:
                break
