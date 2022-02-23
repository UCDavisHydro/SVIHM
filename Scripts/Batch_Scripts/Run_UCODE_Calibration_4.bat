@echo off

cd ..
IF EXIST Calibration_4 (
set INPUT=
set /P INPUT="All data will be overwritten. Continue? (y/n):"
)
If "%INPUT%"=="n" EXIT
If "%INPUT%"=="N" EXIT

RMDIR Calibration_4 /S /Q  
MKDIR Calibration_4
  
REM copy only necessary files for Calibration_4 analysis run  
for /L %%i in (1,1,6) do (  
  MKDIR Calibration_4\Runner%%i  
  xcopy MODFLOW Calibration_4\Runner%%i /E /I /Y
  xcopy SWBM\input Calibration_4\Runner%%i /E /I /Y
  xcopy UCODE\UCODE_Instruction_Files Calibration_4\Runner%%i /E /I /Y  
  xcopy UCODE\UCODE_Template_Files Calibration_4\Runner%%i /E /I /Y  
  copy Batch_Scripts\Run_SVIHM_UCODE_Calibration_Runner.bat Calibration_4\Runner%%i\Run_SVIHM_UCODE_Calibration.bat
  copy UCODE\UCODE_Input_Files\SVIHM_precal.param4 Calibration_4\Runner%%i\SVIHM_precal.param4  
  copy UCODE\UCODE_Input_Files\SVIHM.flowobs Calibration_4\Runner%%i\SVIHM.flowobs  
  copy UCODE\UCODE_Input_Files\SVIHM.headobs Calibration_4\Runner%%i\SVIHM.headobs  
  copy UCODE\UCODE_Input_Files\SVIHM.obsgrp Calibration_4\Runner%%i\SVIHM.obsgrp 
  copy UCODE\UCODE_Input_Files\SVIHM.pargrp Calibration_4\Runner%%i\SVIHM.pargrp 
  copy UCODE\runner.exe Calibration_4\Runner%%i\runner.exe  
  copy SWBM\bin\SWBM.exe Calibration_4\Runner%%i\SWBM.exe
  )

xcopy Calibration_4\Runner1 Calibration_4 /E /I /Y 
copy UCODE\SVIHM_Calibration_4.in Calibration_4\SVIHM_Calibration_4.in 
copy Batch_Scripts\Run_SVIHM_UCODE_Calibration_Master.bat Calibration_4\Run_SVIHM_UCODE_Calibration.bat
copy R_Files\Model\Update_SVIHM_Drain_Inflows.R Calibration_4\Update_SVIHM_Drain_Inflows.R 
copy R_Files\Model\Update_SVIHM_Starting_Heads.R Calibration_4\Update_SVIHM_Starting_Heads.R

cd Calibration_4  
for /L %%i in (1,1,6) do ( 
cd Runner%%i
start runner.exe  
cd .. 
)  
REM Calibration
"C:\wrdapp\UCODE_2014_1.004_and_more\UCODE_2014_1.004\bin\UCODE_2014.exe"  SVIHM_Calibration_4.in SVIHM_Calibration_4_out \wait 
REM Residual Analysis
"C:\wrdapp\UCODE_2014_1.004_and_more\UCODE_2014_1.004\bin\residual_analysis.exe" SVIHM_Calibration_4_out 
pause
