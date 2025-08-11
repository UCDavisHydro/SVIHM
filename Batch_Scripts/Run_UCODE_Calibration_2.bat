@echo off

cd ..
IF EXIST Calibration_2 (
set INPUT=
set /P INPUT="All data will be overwritten. Continue? (y/n):"
)
If "%INPUT%"=="n" EXIT
If "%INPUT%"=="N" EXIT

RMDIR Calibration_2 /S /Q  
MKDIR Calibration_2
  
REM copy only necessary files for Calibration_2 analysis run  
for /L %%i in (1,1,6) do (  
  MKDIR Calibration_2\Runner%%i  
  xcopy MODFLOW Calibration_2\Runner%%i /E /I /Y
  xcopy SWBM\input Calibration_2\Runner%%i /E /I /Y
  xcopy UCODE\UCODE_Instruction_Files Calibration_2\Runner%%i /E /I /Y  
  xcopy UCODE\UCODE_Template_Files Calibration_2\Runner%%i /E /I /Y  
  copy Batch_Scripts\Run_SVIHM_UCODE_Calibration_Runner.bat Calibration_2\Runner%%i\Run_SVIHM_UCODE_Calibration.bat
  copy UCODE\UCODE_Input_Files\SVIHM_precal.param2 Calibration_2\Runner%%i\SVIHM_precal.param2  
  copy UCODE\UCODE_Input_Files\SVIHM.flowobs Calibration_2\Runner%%i\SVIHM.flowobs  
  copy UCODE\UCODE_Input_Files\SVIHM.headobs Calibration_2\Runner%%i\SVIHM.headobs  
  copy UCODE\UCODE_Input_Files\SVIHM.obsgrp Calibration_2\Runner%%i\SVIHM.obsgrp 
  copy UCODE\UCODE_Input_Files\SVIHM.pargrp Calibration_2\Runner%%i\SVIHM.pargrp 
  copy UCODE\runner.exe Calibration_2\Runner%%i\runner.exe  
  copy SWBM\bin\SWBM.exe Calibration_2\Runner%%i\SWBM.exe
  )

xcopy Calibration_2\Runner1 Calibration_2 /E /I /Y 
copy UCODE\SVIHM_Calibration_2.in Calibration_2\SVIHM_Calibration_2.in 
copy Batch_Scripts\Run_SVIHM_UCODE_Calibration_Master.bat Calibration_2\Run_SVIHM_UCODE_Calibration.bat
copy R_Files\Model\Update_SVIHM_Drain_Inflows.R Calibration_2\Update_SVIHM_Drain_Inflows.R 
copy R_Files\Model\Update_SVIHM_Starting_Heads.R Calibration_2\Update_SVIHM_Starting_Heads.R

cd Calibration_2  
for /L %%i in (1,1,6) do ( 
cd Runner%%i
start runner.exe  
cd .. 
)  
REM Calibration
"C:\wrdapp\UCODE_2014_1.004_and_more\UCODE_2014_1.004\bin\UCODE_2014.exe"  SVIHM_Calibration_2.in SVIHM_Calibration_2_out \wait 
REM Residual Analysis
"C:\wrdapp\UCODE_2014_1.004_and_more\UCODE_2014_1.004\bin\residual_analysis.exe" SVIHM_Calibration_2_out 
pause
