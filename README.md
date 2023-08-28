# lake

lake is a framework for generating synthesizable memory modules from a high-level behavioral specification and widely-available memory macros. lake also comprises a library of generalized hardware modules aimed at memory controller designs.

## Install
`git clone github.com/StanfordAHA/lake`

`cd lake && pip install -e .`

## Run a test
To run a test, you can simply generate the verilog and push through your favorite verilog simulator. Alternatively, lake uses the pytest framework for unit tests of constituent modules. These tests leverage [fault](https://github.com/leonardt/fault) and [verilator](https://www.veripool.org/wiki/verilator) for open source simulation. Tests should run and pass on Linux and MacOS.

## Documentation
Check out the [wiki](https://github.com/StanfordAHA/lake/wiki) of this github repo.
