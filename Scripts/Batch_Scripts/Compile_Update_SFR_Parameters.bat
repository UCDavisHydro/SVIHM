@echo off
cd ..\Update_SFR_Parameters\src
gfortran Update_SFR_Parameters_Master.f90  -o ..\bin\SFR_params_m.exe
gfortran Update_SFR_Parameters_Runner.f90  -o ..\bin\SFR_params_r.exe
echo Update_SFR_Parameters Successfully Compiled
pause
