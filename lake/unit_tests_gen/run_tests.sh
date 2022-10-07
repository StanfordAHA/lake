TEST_DIR=/aha/lake/lake/unit_tests_gen
OUT_DIR=/aha/lake/lake/unit_tests_gen/dump_dir
SRC_DIR=/aha/lake/lake/unit_tests

PRIMITIVES=(
	intersect
)


for a in 0 1 2
do
	python $TEST_DIR/test_crd_hold.py $a
	xmverilog $SRC_DIR/crd_hold.py $SRC_DIR/crd_hold_tb.sv
done

printf "_________________________________________________________\n"
for a in 0 1 2 3
do
	python $TEST_DIR/test_crd_drop.py $a
	xmverilog $SRC_DIR/crd_drop.sv $SRC_DIR/crd_drop_tb.sv
done

printf "_________________________________________________________\n"

for a in 0 1 2 3
do
	python $TEST_DIR/test_reduce.py $a
	xmverilog $SRC_DIR/reg.sv $SRC_DIR/reg_tb.sv
done

printf "_________________________________________________________\n"
for inp in 1 2
do
	python $TEST_DIR/test_union.py $inp
	xmverilog $SRC_DIR/intersect.sv $SRC_DIR/union_tb.sv
done

printf "_________________________________________________________\n"
for i in ${!PRIMITIVES[@]}; do	
	bench=${PRIMITIVES[$b]}
	for input in 4 16 32 64
		do
			python  $TEST_DIR/test_$bench.py "1D" $input
			xmverilog $SRC_DIR/$bench.sv $SRC_DIR/${bench}_tb.sv
		done
done
