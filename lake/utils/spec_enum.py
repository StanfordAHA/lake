from enum import Enum


###
# Port Enums
###
class Direction(Enum):
    IN = 0
    OUT = 1


class Runtime(Enum):
    STATIC = 0
    DYNAMIC = 1


###
# MemoryPort Enums
###
class MemoryPortType(Enum):
    R = 0
    W = 1
    RW = 2
    READ = 0
    WRITE = 1
    READWRITE = 2


###
# LF Comparison Enum
###
class LFComparisonOperator(Enum):
    LT = 0
    GT = 1
    EQ = 2
    LTE = 3
