#!/bin/bash -eu

# Run basecase version of SVIHM
echo 'Starting basecase model run'
echo 'Initializing drain inflows to zero'
cp Drains_initial_m3day.txt Drains_m3day.txt
echo 'Drains initialized to zero'
./SWBM              
./OWHM SVIHM_bc.nam
echo 'Updating drain inflows'
Rscript Update_SVIHM_Drain_Inflows.R SVIHM.lst
./SWBM              
./OWHM SVIHM_bc.nam
echo 'Basecase model run completed'

# Run MAR_ILR version of SVIHM
echo 'Starting model run with MAR_ILR scenario'
echo 'Initializing drain inflows to zero'
cp Drains_initial_m3day.txt Drains_m3day.txt
echo 'Drains initialized to zero'
echo 'Replacing scenario flag in general_inputs.txt with MAR_ILR'
sed -i 's/basecase/MAR_ILR/1I' general_inputs.txt  #replace basecase flag with MAR_ILR flag
echo 'Scenario flag repaced'
./SWBM              
./OWHM SVIHM_scen.nam
echo 'Updating drain inflows'
Rscript Update_SVIHM_Drain_Inflows.R SVIHM_scen.lst
./SWBM              
./OWHM SVIHM_scen.nam
echo 'MAR_ILR scenario model run completed'