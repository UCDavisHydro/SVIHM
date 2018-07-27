@echo off

cd ..
IF EXIST Sensitivity_1 (
set INPUT=
set /P INPUT="All data will be overwritten. Continue? (y/n):"
echo %INPUT%
)
If "%INPUT%"=="n" EXIT
If "%INPUT%"=="N" EXIT

RMDIR Sensitivity_1 /S /Q  
MKDIR Sensitivity_1
  
REM copy only necessary files for Sensitivity_1 analysis run  
for /L %%i in (1,1,6) do (  
  MKDIR Sensitivity_1\Runner%%i  
  xcopy MODFLOW Sensitivity_1\Runner%%i /E /I /Y
  xcopy SWBM\input Sensitivity_1\Runner%%i /E /I /Y
  xcopy UCODE\UCODE_Instruction_Files Sensitivity_1\Runner%%i /E /I /Y  
  xcopy UCODE\UCODE_Template_Files Sensitivity_1\Runner%%i /E /I /Y
  xcopy Update_SFR_Parameters\bin Sensitivity_1\Runner%%i /E /I /Y  
  copy UCODE\UCODE_Input_Files\SVIHM_Sensitivity_1.txt Sensitivity_1\Runner%%i\SVIHM_Sensitivity.txt
  copy Batch_Scripts\Run_SVIHM_UCODE_Sensitivity_Runner.bat Sensitivity_1\Runner%%i\Run_SVIHM_UCODE_Sensitivity.bat
  copy UCODE\UCODE_Input_Files\SVIHM_sensitivity.param1 Sensitivity_1\Runner%%i\SVIHM_sensitivity.param1  
  copy UCODE\UCODE_Input_Files\SVIHM.flowobs Sensitivity_1\Runner%%i\SVIHM.flowobs  
  copy UCODE\UCODE_Input_Files\SVIHM.headobs Sensitivity_1\Runner%%i\SVIHM.headobs  
  copy UCODE\UCODE_Input_Files\SVIHM.obsgrp Sensitivity_1\Runner%%i\SVIHM.obsgrp 
  copy UCODE\UCODE_Input_Files\SVIHM.pargrp Sensitivity_1\Runner%%i\SVIHM.pargrp 
  copy UCODE\runner.exe Sensitivity_1\Runner%%i\runner.exe  
  copy SWBM\bin\SWBM.exe Sensitivity_1\Runner%%i\SWBM.exe
  )

xcopy Sensitivity_1\Runner1 Sensitivity_1 /E /I /Y 
copy UCODE\SVIHM_Sensitivity_1.in Sensitivity_1\SVIHM_Sensitivity_1.in 
copy Batch_Scripts\Run_SVIHM_UCODE_Sensitivity_Master.bat Sensitivity_1\Run_SVIHM_UCODE_Sensitivity.bat
copy R_Files\Model\Update_SVIHMv3_Drain_Inflows.R Sensitivity_1\Update_SVIHMv3_Drain_Inflows.R 
copy R_Files\Model\Update_Starting_Heads.R Sensitivity_1\Update_Starting_Heads.R

cd Sensitivity_1  
for /L %%i in (1,1,6) do ( 
cd Runner%%i
start runner.exe  
cd .. 
)  
REM Sensitivity
"C:\wrdapp\UCODE_2014_1.004_and_more\UCODE_2014_1.004\bin\UCODE_2014.exe"  SVIHM_Sensitivity_1.in SVIHM_Sensitivity_1_out \wait 
REM Residual Analysis
"C:\wrdapp\UCODE_2014_1.004_and_more\UCODE_2014_1.004\bin\residual_analysis.exe" SVIHM_Sensitivity_1_out 
pause
