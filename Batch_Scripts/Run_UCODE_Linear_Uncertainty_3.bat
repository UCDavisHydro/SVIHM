@echo off
cd ..
copy UCODE\SVIHM_linearity_3.in Calibration_3\SVIHM_linearity_3.in
copy Batch_Scripts\Run_SVIHM_forward.bat Calibration_3\Run_SVIHM_forward.bat
cd Calibration_3

@echo on

"C:\wrdapp\UCODE_2014_1.004_and_more\UCODE_2014_1.004\bin\UCODE_2014.exe"  SVIHM_linearity_3.in SVIHM_Calibration_3_out \wait

"C:\wrdapp\UCODE_2014_1.004_and_more\UCODE_2014_1.004\bin\model_linearity.exe" SVIHM_Calibration_3_out

pause