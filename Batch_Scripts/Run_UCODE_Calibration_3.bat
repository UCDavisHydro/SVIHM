@echo off

cd ..
IF EXIST Calibration_3 (
set INPUT=
set /P INPUT="All data will be overwritten. Continue? (y/n):"
echo %INPUT%
)
If "%INPUT%"=="n" EXIT
If "%INPUT%"=="N" EXIT

RMDIR Calibration_3 /S /Q  
MKDIR Calibration_3
  
REM copy only necessary files for Calibration_3 analysis run  
for /L %%i in (1,1,6) do (  
  MKDIR Calibration_3\Runner%%i  
  xcopy MODFLOW Calibration_3\Runner%%i /E /I /Y
  xcopy SWBM\input Calibration_3\Runner%%i /E /I /Y
  xcopy UCODE\UCODE_Instruction_Files Calibration_3\Runner%%i /E /I /Y  
  xcopy UCODE\UCODE_Template_Files Calibration_3\Runner%%i /E /I /Y  
  copy Batch_Scripts\Run_SVIHM_UCODE_Calibration_Runner.bat Calibration_3\Runner%%i\Run_SVIHM_UCODE_Calibration.bat
  copy UCODE\UCODE_Input_Files\SVIHM_precal.param3 Calibration_3\Runner%%i\SVIHM_precal.param3  
  copy UCODE\UCODE_Input_Files\SVIHM.flowobs Calibration_3\Runner%%i\SVIHM.flowobs  
  copy UCODE\UCODE_Input_Files\SVIHM.headobs Calibration_3\Runner%%i\SVIHM.headobs  
  copy UCODE\UCODE_Input_Files\SVIHM.obsgrp Calibration_3\Runner%%i\SVIHM.obsgrp 
  copy UCODE\UCODE_Input_Files\SVIHM.pargrp Calibration_3\Runner%%i\SVIHM.pargrp 
  copy UCODE\runner.exe Calibration_3\Runner%%i\runner.exe  
  copy SWBM\bin\SWBM.exe Calibration_3\Runner%%i\SWBM.exe
  )

xcopy Calibration_3\Runner1 Calibration_3 /E /I /Y 
copy UCODE\SVIHM_Calibration_3.in Calibration_3\SVIHM_Calibration_3.in 
copy Batch_Scripts\Run_SVIHM_UCODE_Calibration_Master.bat Calibration_3\Run_SVIHM_UCODE_Calibration.bat
copy R_Files\Model\Update_SVIHMv3_Drain_Inflows.R Calibration_3\Update_SVIHMv3_Drain_Inflows.R 
copy R_Files\Model\Update_Starting_Heads.R Calibration_3\Update_Starting_Heads.R

cd Calibration_3  
for /L %%i in (1,1,6) do ( 
cd Runner%%i
start runner.exe  
cd .. 
)  
REM Calibration
"C:\wrdapp\UCODE_2014_1.004_and_more\UCODE_2014_1.004\bin\UCODE_2014.exe"  SVIHM_Calibration_3.in SVIHM_Calibration_3_out \wait 
REM Residual Analysis
"C:\wrdapp\UCODE_2014_1.004_and_more\UCODE_2014_1.004\bin\residual_analysis.exe" SVIHM_Calibration_3_out 
pause
