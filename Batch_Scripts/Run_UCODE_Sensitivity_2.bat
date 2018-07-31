@echo off

cd ..
IF EXIST Sensitivity_2 (
set INPUT=
set /P INPUT="All data will be overwritten. Continue? (y/n):"
echo %INPUT%
)
If "%INPUT%"=="n" EXIT
If "%INPUT%"=="N" EXIT

RMDIR Sensitivity_2 /S /Q  
MKDIR Sensitivity_2
  
REM copy only necessary files for Sensitivity_2 analysis run  
for /L %%i in (1,1,6) do (  
  MKDIR Sensitivity_2\Runner%%i  
  xcopy MODFLOW Sensitivity_2\Runner%%i /E /I /Y
  xcopy SWBM\input Sensitivity_2\Runner%%i /E /I /Y
  xcopy UCODE\UCODE_Instruction_Files Sensitivity_2\Runner%%i /E /I /Y  
  xcopy UCODE\UCODE_Template_Files Sensitivity_2\Runner%%i /E /I /Y
  xcopy Update_SFR_Parameters\bin Sensitivity_2\Runner%%i /E /I /Y  
  copy UCODE\UCODE_Input_Files\SVIHM_Sensitivity_2.txt Sensitivity_2\Runner%%i\SVIHM_Sensitivity.txt
  copy Batch_Scripts\Run_SVIHM_UCODE_Sensitivity_Runner.bat Sensitivity_2\Runner%%i\Run_SVIHM_UCODE_Sensitivity.bat
  copy UCODE\UCODE_Input_Files\SVIHM_sensitivity.param2 Sensitivity_2\Runner%%i\SVIHM_sensitivity.param2  
  copy UCODE\UCODE_Input_Files\SVIHM.flowobs Sensitivity_2\Runner%%i\SVIHM.flowobs  
  copy UCODE\UCODE_Input_Files\SVIHM.headobs Sensitivity_2\Runner%%i\SVIHM.headobs  
  copy UCODE\UCODE_Input_Files\SVIHM.obsgrp Sensitivity_2\Runner%%i\SVIHM.obsgrp 
  copy UCODE\UCODE_Input_Files\SVIHM.pargrp Sensitivity_2\Runner%%i\SVIHM.pargrp 
  copy UCODE\runner.exe Sensitivity_2\Runner%%i\runner.exe  
  copy SWBM\bin\SWBM.exe Sensitivity_2\Runner%%i\SWBM.exe
  )

xcopy Sensitivity_2\Runner1 Sensitivity_2 /E /I /Y 
copy UCODE\SVIHM_Sensitivity_2.in Sensitivity_2\SVIHM_Sensitivity_2.in 
copy Batch_Scripts\Run_SVIHM_UCODE_Sensitivity_Master.bat Sensitivity_2\Run_SVIHM_UCODE_Sensitivity.bat
copy R_Files\Model\Update_SVIHMv3_Drain_Inflows.R Sensitivity_2\Update_SVIHMv3_Drain_Inflows.R 
copy R_Files\Model\Update_Starting_Heads.R Sensitivity_2\Update_Starting_Heads.R

cd Sensitivity_2  
for /L %%i in (1,1,6) do ( 
cd Runner%%i
start runner.exe  
cd .. 
)  
REM Sensitivity
"C:\wrdapp\UCODE_2014_1.004_and_more\UCODE_2014_1.004\bin\UCODE_2014.exe"  SVIHM_Sensitivity_2.in SVIHM_Sensitivity_2_out \wait 
pause
