@echo off

cd  ..\Calibration_4      

copy ..\UCODE\SVIHM_postcal_sensitivity_4.in SVIHM_postcal_sensitivity_4.in

REM Re-run Sensitivity Analysis
"C:\wrdapp\ucode_2014_1.004_and_more\ucode_2014_1.004\bin\UCODE_2014.exe"  SVIHM_postcal_sensitivity_4.in SVIHM_Calibration_4_out \wait


for /L %%i in (1,1,6) do (  REM copy only necessary files for updating sensitivity analysis run            
  copy ..\Run_SVIHM_forward.bat Runner%%i\Run_SVIHM_forward.bat   
  copy ..\UCODE\UCODE_Input_Files\SVIHMv3.1_postcal.param4 Runner%%i 
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