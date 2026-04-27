# gen_srams.sh — Generates GF12 SRAM macros (single OR dual port).
#
# Replaces the ADK's gen_srams.sh which hardcoded the IN12LP_S1DB_ family prefix
# and defaulted mc_name to "s1db", so dual-port macros (SDPB) could never be built.
#
# Inputs (from get_macro_name.py):
#   $1 = full macro name extracted from RTL
#        (e.g., IN12LP_SDPB_W02048B016M04S4_H or IN12LP_S1DB_W01024B064M04S2_H)
#   $2 = SRAM family token (S1DB | S1PB | SDPB | R2PB)
#
# Required env vars (from configure.yml parameters):
#   corner, bc_corner

corner_string_to_arg () {
  corner_arg=$(sed 's/_/ /g' <<<$1)
  corner_arg=$(sed 's/\([0-9]\)P/\1./g' <<<$corner_arg)
  corner_arg=$(sed 's/C//g' <<<$corner_arg)
  corner_arg=$(sed 's/V//g' <<<$corner_arg)
  echo "$corner_arg"
}

sram_name_in="$1"
sram_family_in="${2:-S1DB}"

# Pick the matching memory compiler binary directory based on family.
# Available compilers in the GF12 ADK: s1db, s1pb, sdpb, r2pb.
mc_short=$(echo "$sram_family_in" | tr '[:upper:]' '[:lower:]')
mc_name=v-comp_in_gf12lp_${mc_short}
export IMDK_ROOT=inputs/adk/mc/${mc_name}

# Use the full macro name passed in (already has correct family prefix, dimensions,
# mux, subarrays, and H/L suffix). Fall back to constructing from parameters if not.
if [ -n "$sram_name_in" ]; then
  export sram_name="$sram_name_in"
else
  export sram_name="IN12LP_${sram_family_in}_W$( printf '%05d' $num_words )B$( printf '%03d' $word_size )M$( printf '%02d' $mux_size )S${num_subarrays}_"
  if [ "$leakage_opt" == "True" ]; then
    sram_name+="L"
  else
    sram_name+="H"
  fi
  if [ "$partial_write" == "True" ]; then
    sram_name+="B"
  fi
fi

corner_arg=$(corner_string_to_arg $corner)
bc_corner_arg=$(corner_string_to_arg $bc_corner)

echo "Building SRAM macro: $sram_name"
echo "  family   : $sram_family_in"
echo "  compiler : $mc_name"
echo "  corners  : $corner_arg / $bc_corner_arg"

cmd='./inputs/adk/mc/${mc_name}/bin/IN12LP_MEM_genviews -macro $sram_name -corner '\'"${corner_arg}"\'' -corner '\'"$bc_corner_arg"\'''
eval $cmd |& tee mc.log

ln -s ../genviews-output/model/timing/ccs/${sram_name}_${corner}.lib outputs/sram_tt.lib
ln -s ../genviews-output/model/timing/ccs/${sram_name}_${bc_corner}.lib outputs/sram_ff.lib
ln -s ../genviews-output/gds/${sram_name}.gds outputs/sram.gds
ln -s ../genviews-output/lef/${sram_name}.lef outputs/sram.lef
ln -s ../genviews-output/model/verilog/${sram_name}_pwr.v outputs/sram-pwr.v
ln -s ../genviews-output/model/verilog/${sram_name}.v outputs/sram.v
ln -s ../genviews-output/cdl/${sram_name}.cdl outputs/sram.cdl
ln -s ../genviews-output/cdl/${sram_name}.cdl outputs/sram.spi

# Build sram_tt.db
cd lib2db/
make
cd ..

# Emit findable error message and die HERE if SRAMs are missing
sram_exists=True
head outputs/sram_tt.lib > /dev/null || sram_exists=False
if [ $sram_exists == "False" ]; then
    echo "**ERROR Could not build SRAMs...memory compiler error maybe?"
    exit 13
fi

echo '--- continue...'
