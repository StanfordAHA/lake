from enum import IntEnum


class EdgeType(IntEnum):
    DIRECT = 0
    WEIGHTED = 1


class NodeType(IntEnum):
    ITERATOR = 0
    CONDITION = 1
    EXTERNAL = 2


class Controller():
    def __init__(self):
        self.iterators = []

    def add_node(it, path):
        self.iterators.append(it)


class ControllerEdge():
    def __init__(self,
                 inputs,
                 outputs,
                 edge_type):

        self.inputs = []
        self.outputs = []
        self.edge_type = edge_type


class DirectEdge(ControllerEdge):
    def __init__(self, inputs, outputs):

        super().__init__(self, inputs, outputs, DIRECT)


class WeightedEdge(ControllerEdge):
    def __init__(self, inputs, outputs):
        super().__init__(self, inputs, outputs, WEIGHTED)


class ControllerNode():
    def __init__(self, name, node_type):

        self.name = name
        self.node_type = node_type


class IteratorNode(ControllerNode):
    def __init__(self, name, it_range, it_stride, num_conditions):
        super().__init__(self, name, ITERATOR)

        self.it_range = it_range
        self.it_stride = it_stride
        self.num_conditions = num_conditions


class ExternalNode(ControllerNode):
    def __init__(self, name):
        super().__init__(self, name, EXTERNAL)


# normal affine expression (current)
addressor = Controller()
for i in range(2):
    it = IteratorNode(f"it_{i}", 65535, 65535, 1)
    addressor.add_node(it, [WEIGHTED])

# default could be order in which they are added
addressor.define_loop_levels(["it_0", "it_1"])

# condition for one iterator
cond_add = Controller()
cond_add.add_node(IteratorNode("i", 65535, 65535, 2),
                  [DIRECT, CONDITION, WEIGHTED])
cond_add.add_node(IteratorNode("j", 65535, 65535, 1), [WEIGHTED])

addressor.define_loop_levels(["i", "j"])

# condition for two iterators
cond_add2 = Controller()
cond_add2.add_node(IteratorNode("i", 65535, 65535, 2),
                   [DIRECT, CONDITION, WEIGHTED])
cond_add2.add_node(IteratorNOde("j", 65535, 65535, 2),
                   [DIRECT, CONDITION, WEIGHTED])

addressor.define_loop_levels(["i", "j"])

ext_ctrl = Controller()
ext_ctrl.add_node(ExternalNode("addr"), [DIRECT])
