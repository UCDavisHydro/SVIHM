#!/bin/bash -eu

cp Drains_initial_m3day.txt Drains_m3day.txt

./SWBM
./OWHM.nix SVIHM.nam

Rscript Update_SVIHM_Drain_Inflows.R             

./SWBM
./OWHM.nix SVIHM.nam