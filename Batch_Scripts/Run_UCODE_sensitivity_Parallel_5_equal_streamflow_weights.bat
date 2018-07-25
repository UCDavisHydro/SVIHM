@echo off

RMDIR Sensitivity /S /Q     
MKDIR Sensitivity           

for /L %%i in (1,1,6) do (  REM copy only necessary files for sensitivity analysis run            
  MKDIR Sensitivity\Runner%%i
  copy Run_SVIHMv3.1_ucode_sensitivity_5.bat Sensitivity\Runner%%i\Run_SVIHMv3.1_ucode_sensitivity_5.bat   
  xcopy UCODE\UCODE_Input_Files\SVIHMv3.1_sensitivity.param5 Sensitivity\Runner%%i /E /I /Y 
  xcopy UCODE\UCODE_Input_Files\Starting_Params_5.txt Sensitivity\Runner%%i /E /I /Y
  xcopy MODFLOW Sensitivity\Runner%%i /E /I /Y
  xcopy SWBM Sensitivity\Runner%%i /E /I /Y 
  xcopy UCODE\UCODE_Input_Files\SVIHMv3.1.flowobs Sensitivity\Runner%%i /E /I /Y
  xcopy UCODE\UCODE_Input_Files\SVIHMv3.1.headobs Sensitivity\Runner%%i /E /I /Y
  xcopy UCODE\UCODE_Input_Files\SVIHMv3.1_equal_streamflow_weights.obsgrp Sensitivity\Runner%%i /E /I /Y
  xcopy UCODE\UCODE_Input_Files\SVIHMv3.1.pargrp Sensitivity\Runner%%i /E /I /Y
  xcopy UCODE\UCODE_Instruction_Files Sensitivity\Runner%%i /E /I /Y
  xcopy UCODE\UCODE_Template_Files Sensitivity\Runner%%i /E /I /Y
  copy runner.exe Sensitivity\Runner%%i\runner.exe 
  copy SFR_Params.exe Sensitivity\Runner%%i\SFR_Params.exe
  copy Drains_m3day_Precal_5.txt Sensitivity\Runner%%i\Drains_m3day.txt
  )
 
xcopy Sensitivity\Runner1 Sensitivity /E /I /Y
copy UCODE\SVIHMv3.1_sensitivity_5_EW.in Sensitivity\SVIHMv3.1_sensitivity_5_EW.in
cd Sensitivity_5_EW       

for /L %%i in (1,1,6) do (
cd Runner%%i
start runner.exe
cd ..
)
@echo on

"C:\wrdapp\ucode_2014_1.004_and_more\ucode_2014_1.004\bin\UCODE_2014.exe"  SVIHMv3.1_sensitivity_5.in SVIHMv3.1_Sens_5_EW \wait
for /L %%i in (0,1,61) do (  REM Loop through n+1 number of parameters to collect mass balance percentage into one file
find /I "PERCENT DISCREPANCY" SVIHMv3.1_%%i.rec >> Sensitivity_Mass_Balance.rec
)
pause