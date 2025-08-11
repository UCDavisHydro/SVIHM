#!/bin/bash -eu
sed -i 's/basecase/MAR/1I' general_inputs.txt                             #replace basecase flag with ILR flag for SWBM
sed -i 's/basecase/MAR/1I' SVIHM_SWBM_General_Inputs.jtf                  #replace basecase flag with ILR flag for SWBM

cp Drains_initial_m3day.txt Drains_m3day.txt

./SWBM
./OWHM.nix SVIHM.nam

Rscript Update_SVIHM_Drain_Inflows.R         
# Rscript Update_SVIHM_Starting_Heads.R   Starting heads from basecase run are used, which are copied in with the MODFLOW files in the sbatch script        

./SWBM
./OWHM.nix SVIHM.nam