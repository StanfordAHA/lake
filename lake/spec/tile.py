import json

class tile:
    def __init__(self, _name:str):
        self.accessors = {}
        self.memories = {}
        self.ctrl_connection = []
        self.name = _name
        '''
        connection accessor->memory
        data path
        control path : input2agg_accessor->agg.write_en
                       agg2sram_accessor->agg.read_en && sram.write_en
                       ...
        '''

    def addInPort(self, name:str, word_width:int):
        self.memories[name] = resource(name, "in_port", word_width)

    def addOutPort(self, name:str, word_width:int):
        self.memories[name] = resource(name, "out_port", word_width)

    def generate_verilog(self):
        verilog_str = "module " + self.name + "();\nendmodule"
        f = open(self.name + ".v", "w")
        f.write(verilog_str)
        f.close()

    def connect(self, input_, output):
        self.ctrl_connection.append((input_, output))

    def generate_compiler_target(self):
        json_string = json.dumps({"name": self.name})
        f = open(self.name + ".json", "w")
        f.write(json_string)
        f.close()

class resource:
    def __init__(self, inst_name, type_name, width):
        self.inst_name = inst_name
        self.type_name = type_name
        self.width = width


    def read(self):
        pass

    def write(self):
        pass
