ARGS=`cat inputs/design.args`

# Grab all design/testbench files
for f in inputs/*.v; do
    [ -e "$f" ] || continue
    ARGS="$ARGS $f"
done

for f in inputs/*.sv; do
    [ -e "$f" ] || continue
    ARGS="$ARGS $f"
done

# Set-up testbench
ARGS="$ARGS -s $testbench_name"

(
    set -x;
    iverilog -o testbench.vvp $ARGS && \
    vvp testbench.vvp
)

mv run.vcd outputs/run.vcd