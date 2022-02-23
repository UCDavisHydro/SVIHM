@echo off

cd ..
IF EXIST Sensitivity_4 (
set INPUT=
set /P INPUT="All data will be overwritten. Continue? (y/n):"
echo %INPUT%
)
If "%INPUT%"=="n" EXIT
If "%INPUT%"=="N" EXIT

RMDIR Sensitivity_4 /S /Q  
MKDIR Sensitivity_4
  
REM copy only necessary files for Sensitivity_4 analysis run  
for /L %%i in (1,1,6) do (  
  MKDIR Sensitivity_4\Runner%%i  
  xcopy MODFLOW Sensitivity_4\Runner%%i /E /I /Y
  xcopy SWBM\input Sensitivity_4\Runner%%i /E /I /Y
  xcopy UCODE\UCODE_Instruction_Files Sensitivity_4\Runner%%i /E /I /Y  
  xcopy UCODE\UCODE_Template_Files Sensitivity_4\Runner%%i /E /I /Y
  xcopy Update_SFR_Parameters\bin Sensitivity_4\Runner%%i /E /I /Y  
  copy UCODE\UCODE_Input_Files\SVIHM_Sensitivity_4.txt Sensitivity_4\Runner%%i\SVIHM_Sensitivity.txt
  copy Batch_Scripts\Run_SVIHM_UCODE_Sensitivity_Runner.bat Sensitivity_4\Runner%%i\Run_SVIHM_UCODE_Sensitivity.bat
  copy UCODE\UCODE_Input_Files\SVIHM_sensitivity.param4 Sensitivity_4\Runner%%i\SVIHM_sensitivity.param4  
  copy UCODE\UCODE_Input_Files\SVIHM.flowobs Sensitivity_4\Runner%%i\SVIHM.flowobs  
  copy UCODE\UCODE_Input_Files\SVIHM.headobs Sensitivity_4\Runner%%i\SVIHM.headobs  
  copy UCODE\UCODE_Input_Files\SVIHM.obsgrp Sensitivity_4\Runner%%i\SVIHM.obsgrp 
  copy UCODE\UCODE_Input_Files\SVIHM.pargrp Sensitivity_4\Runner%%i\SVIHM.pargrp 
  copy UCODE\runner.exe Sensitivity_4\Runner%%i\runner.exe  
  copy SWBM\bin\SWBM.exe Sensitivity_4\Runner%%i\SWBM.exe
  )

xcopy Sensitivity_4\Runner1 Sensitivity_4 /E /I /Y 
copy UCODE\SVIHM_Sensitivity_4.in Sensitivity_4\SVIHM_Sensitivity_4.in 
copy Batch_Scripts\Run_SVIHM_UCODE_Sensitivity_Master.bat Sensitivity_4\Run_SVIHM_UCODE_Sensitivity.bat
copy R_Files\Model\Update_SVIHM_Drain_Inflows.R Sensitivity_4\Update_SVIHM_Drain_Inflows.R 
copy R_Files\Model\Update_SVIHM_Starting_Heads.R Sensitivity_4\Update_SVIHM_Starting_Heads.R

cd Sensitivity_4  
for /L %%i in (1,1,6) do ( 
cd Runner%%i
start runner.exe  
cd .. 
)  
REM Sensitivity
"C:\wrdapp\UCODE_2014_1.004_and_more\UCODE_2014_1.004\bin\UCODE_2014.exe"  SVIHM_Sensitivity_4.in SVIHM_Sensitivity_4_out \wait 
pause
