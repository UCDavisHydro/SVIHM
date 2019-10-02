#!/bin/bash

gfortran -c define_poly.f90
gfortran -c irrigation.f90
gfortran -c outputmodule_depletion_accretion.f90
gfortran -c SWBM_depletion_accretion_Linux.f90
gfortran *.o -o ../bin/SWBM_depletion_accretion

rm *.o 
rm *.mod 
chmod +x ../bin/SWBM_depletion_accretion

[ -x ../bin/SWBM_depletion_accretion ] && echo "SWBM_depletion_accretion successfully compiled" || echo "SWBM compilation failed"


