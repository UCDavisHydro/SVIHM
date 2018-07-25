@echo off

cd ..
IF EXIST Sensitivity_5 (
set INPUT=
set /P INPUT="All data will be overwritten. Continue? (y/n):"
echo %INPUT%
)
If "%INPUT%"=="n" EXIT
If "%INPUT%"=="N" EXIT

RMDIR Sensitivity_5 /S /Q  
MKDIR Sensitivity_5
  
REM copy only necessary files for Sensitivity_5 analysis run  
for /L %%i in (1,1,6) do (  
  MKDIR Sensitivity_5\Runner%%i  
  xcopy MODFLOW Sensitivity_5\Runner%%i /E /I /Y
  xcopy SWBM\input Sensitivity_5\Runner%%i /E /I /Y
  xcopy UCODE\UCODE_Instruction_Files Sensitivity_5\Runner%%i /E /I /Y  
  xcopy UCODE\UCODE_Template_Files Sensitivity_5\Runner%%i /E /I /Y
  xcopy Update_SFR_Parameters\bin Sensitivity_5\Runner%%i /E /I /Y  
  copy UCODE\UCODE_Input_Files\SVIHM_Sensitivity_5.txt Sensitivity_5\Runner%%i\SVIHM_Sensitivity.txt
  copy Batch_Scripts\Run_SVIHM_UCODE_Sensitivity_Runner.bat Sensitivity_5\Runner%%i\Run_SVIHM_UCODE_Sensitivity.bat
  copy UCODE\UCODE_Input_Files\SVIHM_sensitivity.param5 Sensitivity_5\Runner%%i\SVIHM_sensitivity.param5  
  copy UCODE\UCODE_Input_Files\SVIHM_Sensitivity_5.txt Sensitivity_5\Runner%%i\SVIHM_Sensitivity_5.txt
  copy UCODE\UCODE_Input_Files\SVIHM.flowobs Sensitivity_5\Runner%%i\SVIHM.flowobs  
  copy UCODE\UCODE_Input_Files\SVIHM.headobs Sensitivity_5\Runner%%i\SVIHM.headobs  
  copy UCODE\UCODE_Input_Files\SVIHM.obsgrp Sensitivity_5\Runner%%i\SVIHM.obsgrp 
  copy UCODE\UCODE_Input_Files\SVIHM.pargrp Sensitivity_5\Runner%%i\SVIHM.pargrp 
  copy UCODE\runner.exe Sensitivity_5\Runner%%i\runner.exe  
  copy SWBM\bin\SWBM.exe Sensitivity_5\Runner%%i\SWBM.exe
  )

xcopy Sensitivity_5\Runner1 Sensitivity_5 /E /I /Y 
copy UCODE\SVIHM_Sensitivity_5.in Sensitivity_5\SVIHM_Sensitivity_5.in 
copy Batch_Scripts\Run_SVIHM_UCODE_Sensitivity_Master.bat Sensitivity_5\Run_SVIHM_UCODE_Sensitivity.bat
copy R_Files\Model\Update_SVIHMv3_Drain_Inflows.R Sensitivity_5\Update_SVIHMv3_Drain_Inflows.R 
copy R_Files\Model\Update_Starting_Heads.R Sensitivity_5\Update_Starting_Heads.R

cd Sensitivity_5  
for /L %%i in (1,1,6) do ( 
cd Runner%%i
start runner.exe  
cd .. 
)  
REM Sensitivity
"C:\wrdapp\UCODE_2014_1.004_and_more\UCODE_2014_1.004\bin\UCODE_2014.exe"  SVIHM_Sensitivity_5.in SVIHM_Sensitivity_5_out \wait 
REM Residual Analysis
"C:\wrdapp\UCODE_2014_1.004_and_more\UCODE_2014_1.004\bin\residual_analysis.exe" SVIHM_Sensitivity_5_out 
pause
