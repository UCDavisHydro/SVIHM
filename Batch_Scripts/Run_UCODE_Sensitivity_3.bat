@echo off

cd ..
IF EXIST Sensitivity_3 (
set INPUT=
set /P INPUT="All data will be overwritten. Continue? (y/n):"
echo %INPUT%
)
If "%INPUT%"=="n" EXIT
If "%INPUT%"=="N" EXIT

RMDIR Sensitivity_3 /S /Q  
MKDIR Sensitivity_3
  
REM copy only necessary files for Sensitivity_3 analysis run  
for /L %%i in (1,1,6) do (  
  MKDIR Sensitivity_3\Runner%%i  
  xcopy MODFLOW Sensitivity_3\Runner%%i /E /I /Y
  xcopy SWBM\input Sensitivity_3\Runner%%i /E /I /Y
  xcopy UCODE\UCODE_Instruction_Files Sensitivity_3\Runner%%i /E /I /Y  
  xcopy UCODE\UCODE_Template_Files Sensitivity_3\Runner%%i /E /I /Y
  xcopy Update_SFR_Parameters\bin Sensitivity_3\Runner%%i /E /I /Y  
  copy UCODE\UCODE_Input_Files\SVIHM_Sensitivity_3.txt Sensitivity_3\Runner%%i\SVIHM_Sensitivity.txt
  copy Batch_Scripts\Run_SVIHM_UCODE_Sensitivity_Runner.bat Sensitivity_3\Runner%%i\Run_SVIHM_UCODE_Sensitivity.bat
  copy UCODE\UCODE_Input_Files\SVIHM_sensitivity.param3 Sensitivity_3\Runner%%i\SVIHM_sensitivity.param3  
  copy UCODE\UCODE_Input_Files\SVIHM.flowobs Sensitivity_3\Runner%%i\SVIHM.flowobs  
  copy UCODE\UCODE_Input_Files\SVIHM.headobs Sensitivity_3\Runner%%i\SVIHM.headobs  
  copy UCODE\UCODE_Input_Files\SVIHM.obsgrp Sensitivity_3\Runner%%i\SVIHM.obsgrp 
  copy UCODE\UCODE_Input_Files\SVIHM.pargrp Sensitivity_3\Runner%%i\SVIHM.pargrp 
  copy UCODE\runner.exe Sensitivity_3\Runner%%i\runner.exe  
  copy SWBM\bin\SWBM.exe Sensitivity_3\Runner%%i\SWBM.exe
  )

xcopy Sensitivity_3\Runner1 Sensitivity_3 /E /I /Y 
copy UCODE\SVIHM_Sensitivity_3.in Sensitivity_3\SVIHM_Sensitivity_3.in 
copy Batch_Scripts\Run_SVIHM_UCODE_Sensitivity_Master.bat Sensitivity_3\Run_SVIHM_UCODE_Sensitivity.bat
copy R_Files\Model\Update_SVIHM_Drain_Inflows.R Sensitivity_3\Update_SVIHM_Drain_Inflows.R 
copy R_Files\Model\Update_SVIHM_Starting_Heads.R Sensitivity_3\Update_SVIHM_Starting_Heads.R

cd Sensitivity_3  
for /L %%i in (1,1,6) do ( 
cd Runner%%i
start runner.exe  
cd .. 
)  
REM Sensitivity
"C:\wrdapp\UCODE_2014_1.004_and_more\UCODE_2014_1.004\bin\UCODE_2014.exe"  SVIHM_Sensitivity_3.in SVIHM_Sensitivity_3_out \wait 
pause
