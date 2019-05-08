#!/bin/bash 
cp Drains_initial_m3day.txt Drains_m3day.txt
sed -i '1s/Basecase/ILR/' general_inputs.txt     # replace scenario type 
./SWBM
./OWHM "SVIHM.nam"
Rscript Update_SVIHM_Drain_Inflows.R
Rscript Update_SVIHM_Starting_Heads.R
./SWBM
./OWHM "SVIHM.nam"