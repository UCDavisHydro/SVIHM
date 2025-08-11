@ECHO OFF

cd ..\SWBM_depletion_accretion\src

gfortran -c define_poly.f90
gfortran -c irrigation.f90
gfortran -c outputmodule_depletion_accretion.f90
gfortran -c SWBM_depletion_accretion.f90
gfortran *.o -o ..\bin\SWBM_depletion_accretion.exe

del /S *.o
del /S *.mod0
del /S *.mod

if exist ..\bin\SWBM_depletion_accretion.exe Echo SWBM_depletion_accretion successfully compiled
pause


