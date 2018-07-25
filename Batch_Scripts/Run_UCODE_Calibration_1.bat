@echo off

cd ..
IF EXIST Calibration_1 (
set INPUT=
set /P INPUT="All data will be overwritten. Continue? (y/n):"
echo %INPUT%
)
If "%INPUT%"=="n" EXIT
If "%INPUT%"=="N" EXIT

RMDIR Calibration_1 /S /Q  
MKDIR Calibration_1
  
REM copy only necessary files for Calibration_1 analysis run  
for /L %%i in (1,1,6) do (  
  MKDIR Calibration_1\Runner%%i  
  xcopy MODFLOW Calibration_1\Runner%%i /E /I /Y
  xcopy SWBM\input Calibration_1\Runner%%i /E /I /Y
  xcopy UCODE\UCODE_Instruction_Files Calibration_1\Runner%%i /E /I /Y  
  xcopy UCODE\UCODE_Template_Files Calibration_1\Runner%%i /E /I /Y  
  copy Batch_Scripts\Run_SVIHM_UCODE_Calibration_Runner.bat Calibration_1\Runner%%i\Run_SVIHM_UCODE_Calibration.bat
  copy UCODE\UCODE_Input_Files\SVIHM_precal.param1 Calibration_1\Runner%%i\SVIHM_precal.param1  
  copy UCODE\UCODE_Input_Files\SVIHM.flowobs Calibration_1\Runner%%i\SVIHM.flowobs  
  copy UCODE\UCODE_Input_Files\SVIHM.headobs Calibration_1\Runner%%i\SVIHM.headobs  
  copy UCODE\UCODE_Input_Files\SVIHM.obsgrp Calibration_1\Runner%%i\SVIHM.obsgrp 
  copy UCODE\UCODE_Input_Files\SVIHM.pargrp Calibration_1\Runner%%i\SVIHM.pargrp 
  copy UCODE\runner.exe Calibration_1\Runner%%i\runner.exe  
  copy SWBM\bin\SWBM.exe Calibration_1\Runner%%i\SWBM.exe
  )

xcopy Calibration_1\Runner1 Calibration_1 /E /I /Y 
copy UCODE\SVIHM_Calibration_1.in Calibration_1\SVIHM_Calibration_1.in 
copy Batch_Scripts\Run_SVIHM_UCODE_Calibration_Master.bat Calibration_1\Run_SVIHM_UCODE_Calibration.bat
copy R_Files\Model\Update_SVIHMv3_Drain_Inflows.R Calibration_1\Update_SVIHMv3_Drain_Inflows.R 
copy R_Files\Model\Update_Starting_Heads.R Calibration_1\Update_Starting_Heads.R

cd Calibration_1  
for /L %%i in (1,1,6) do ( 
cd Runner%%i
start runner.exe  
cd .. 
)  
REM Calibration
"C:\wrdapp\UCODE_2014_1.004_and_more\UCODE_2014_1.004\bin\UCODE_2014.exe"  SVIHM_Calibration_1.in SVIHM_Calibration_1_out \wait 
REM Residual Analysis
"C:\wrdapp\UCODE_2014_1.004_and_more\UCODE_2014_1.004\bin\residual_analysis.exe" SVIHM_Calibration_1_out 
pause
