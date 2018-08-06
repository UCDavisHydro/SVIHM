@echo off

cd ..
IF EXIST Calibration_5 (
set INPUT=
set /P INPUT="All data will be overwritten. Continue? (y/n):"
)
If "%INPUT%"=="n" EXIT
If "%INPUT%"=="N" EXIT

RMDIR Calibration_5 /S /Q  
MKDIR Calibration_5
  
REM copy only necessary files for Calibration_5 analysis run  
for /L %%i in (1,1,6) do (  
  MKDIR Calibration_5\Runner%%i  
  xcopy MODFLOW Calibration_5\Runner%%i /E /I /Y
  xcopy SWBM\input Calibration_5\Runner%%i /E /I /Y
  xcopy UCODE\UCODE_Instruction_Files Calibration_5\Runner%%i /E /I /Y  
  xcopy UCODE\UCODE_Template_Files Calibration_5\Runner%%i /E /I /Y  
  copy Batch_Scripts\Run_SVIHM_UCODE_Calibration_Runner.bat Calibration_5\Runner%%i\Run_SVIHM_UCODE_Calibration.bat
  copy UCODE\UCODE_Input_Files\SVIHM_precal.param5 Calibration_5\Runner%%i\SVIHM_precal.param5  
  copy UCODE\UCODE_Input_Files\SVIHM.flowobs Calibration_5\Runner%%i\SVIHM.flowobs  
  copy UCODE\UCODE_Input_Files\SVIHM.headobs Calibration_5\Runner%%i\SVIHM.headobs  
  copy UCODE\UCODE_Input_Files\SVIHM.obsgrp Calibration_5\Runner%%i\SVIHM.obsgrp 
  copy UCODE\UCODE_Input_Files\SVIHM.pargrp Calibration_5\Runner%%i\SVIHM.pargrp 
  copy UCODE\runner.exe Calibration_5\Runner%%i\runner.exe  
  copy SWBM\bin\SWBM.exe Calibration_5\Runner%%i\SWBM.exe
  )

xcopy Calibration_5\Runner1 Calibration_5 /E /I /Y 
copy UCODE\SVIHM_Calibration_5.in Calibration_5\SVIHM_Calibration_5.in 
copy Batch_Scripts\Run_SVIHM_UCODE_Calibration_Master.bat Calibration_5\Run_SVIHM_UCODE_Calibration.bat
copy R_Files\Model\Update_SVIHMv3_Drain_Inflows.R Calibration_5\Update_SVIHMv3_Drain_Inflows.R 
copy R_Files\Model\Update_Starting_Heads.R Calibration_5\Update_Starting_Heads.R

cd Calibration_5  
for /L %%i in (1,1,6) do ( 
cd Runner%%i
start runner.exe  
cd .. 
)  
REM Calibration
"C:\wrdapp\UCODE_2014_1.004_and_more\UCODE_2014_1.004\bin\UCODE_2014.exe"  SVIHM_Calibration_5.in SVIHM_Calibration_5_out \wait 
REM Residual Analysis
"C:\wrdapp\UCODE_2014_1.004_and_more\UCODE_2014_1.004\bin\residual_analysis.exe" SVIHM_Calibration_5_out 
pause
