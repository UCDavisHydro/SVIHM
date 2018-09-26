@ECHO OFF

cd ..\SWBM\src

gfortran -c define_poly.f90
gfortran -c irrigation.f90
gfortran -c outputmodule.f90
gfortran -c SWBM_MAR_42cfs.f90
gfortran *.o -o ..\bin\SWBM_MAR_42cfs.exe

del /S *.o
del /S *.mod0
del /S *.mod

Echo SWBM_MAR_42cfs successfully compiled
pause


