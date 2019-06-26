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

# Run MAR version of SVIHM
echo 'Starting model run with MAR scenario'
echo 'Initializing drain inflows to zero'
cp Drains_initial_m3day.txt Drains_m3day.txt
echo 'Drains initialized to zero'
echo 'Replacing scenario flag in general_inputs.txt with MAR'
sed -i 's/basecase/MAR/1I' general_inputs.txt  #replace basecase flag with MAR flag
echo 'Scenario flag repaced'
./SWBM              
./OWHM SVIHM_scen.nam
echo 'Updating drain inflows'
Rscript Update_SVIHM_Drain_Inflows.R SVIHM_scen.lst
./SWBM              
./OWHM SVIHM_scen.nam
echo 'MAR scenario model run completed'