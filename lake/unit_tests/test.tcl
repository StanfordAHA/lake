database -open -vcd vcddb -into waveforms_reduce.vcd -default -timescale ps
probe -create -all -R -vcd -depth all -R
probe -create -all -vcd -depth all
run 1000000ns
assertion -summary -final
quit
