@ECHO OFF

cd ..\SWBM\src

gfortran -c define_poly.f90
gfortran -c irrigation.f90
gfortran -c outputmodule.f90
gfortran -c SWBM_MAR.f90
gfortran *.o -o ..\bin\SWBM_MAR.exe

del /S *.o
del /S *.mod0
del /S *.mod

if exist ..\bin\SWBM_MAR.exe Echo SWBM_MAR successfully compiled
pause


