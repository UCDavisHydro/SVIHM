@echo off

cd  Calibration_4_adv      

for /L %%i in (1,1,6) do (  REM copy only necessary files for updating sensitivity analysis run            
  MKDIR Runner%%i
  xcopy ..\MODFLOW Runner%%i /E /I /Y
  xcopy ..\SWBM Runner%%i /E /I /Y 
  copy ..\UCODE\UCODE_Input_Files\SVIHMv3.1.flowobs Runner%%i\SVIHMv3.1.flowobs
  copy ..\UCODE\UCODE_Input_Files\SVIHMv3.1.headobs Runner%%i\SVIHMv3.1.headobs
  copy ..\UCODE\UCODE_Input_Files\SVIHMv3.1.obsgrp Runner%%i\SVIHMv3.1.obsgrp
  copy ..\UCODE\UCODE_Input_Files\SVIHMv3.1.pargrp Runner%%i\SVIHMv3.1.pargrp
  xcopy ..\UCODE\UCODE_Instruction_Files Runner%%i /E /I /Y
  xcopy ..\UCODE\UCODE_Template_Files Runner%%i /E /I /Y
  copy ..\Run_SVIHMv3.1_Runner.bat Runner%%i\Run_SVIHMv3.1.bat   
  xcopy ..\UCODE\UCODE_Input_Files\SVIHMv3.1_postcal.param4 Runner%%i /E /I /Y   
  copy runner.exe Runner%%i\runner.exe
  )
    
copy ..\Run_SVIHMv3.1_Master.bat Run_SVIHMv3.1.bat
copy ..\UCODE\SVIHMv3.1_sensitivity_4_adv.in SVIHMv3.1_sensitivity_4_adv.in  
copy ..\UCODE\UCODE_Input_Files\SVIHMv3.1_postcal.param4 SVIHMv3.1_postcal.param4    

for /L %%i in (1,1,6) do (
cd Runner%%i
start runner.exe
cd ..
)

"C:\wrdapp\ucode_2014_1.004_and_more\ucode_2014_1.004\bin\UCODE_2014.exe"  SVIHMv3.1_sensitivity_4_adv.in SVIHMv3.1_sensitivity_4_adv_out \wait
pause