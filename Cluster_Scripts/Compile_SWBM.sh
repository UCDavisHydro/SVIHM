#!/bin/bash

gfortran -c define_poly.f90
gfortran -c irrigation.f90
gfortran -c outputmodule.f90
gfortran -c SWBM_Linux.f90
gfortran *.o -o ../bin/SWBM

rm *.o
rm *.mod0
rm *.mod

echo SWBM successfully compiled


