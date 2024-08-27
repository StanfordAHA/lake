#!/bin/bash

# Set up synthesis
module purge
module load base dc_shell lc
make 6
# Set up pnr->signoff - have to perform weird unload antics to not break tclsh (TODO)
module purge
module load base innovus dc_shell
module unload dc_shell
make 17