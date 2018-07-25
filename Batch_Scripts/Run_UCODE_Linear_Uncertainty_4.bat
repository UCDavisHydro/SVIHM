@echo off
cd ..
copy UCODE\SVIHM_linearity_4.in Calibration_4\SVIHM_linearity_4.in
copy Batch_Scripts\Run_SVIHM_forward.bat Calibration_4\Run_SVIHM_forward.bat
cd Calibration_4

@echo on

"C:\wrdapp\UCODE_2014_1.004_and_more\UCODE_2014_1.004\bin\UCODE_2014.exe"  SVIHM_linearity_4.in SVIHM_Calibration_4_out \wait

"C:\wrdapp\UCODE_2014_1.004_and_more\UCODE_2014_1.004\bin\model_linearity.exe" SVIHM_Calibration_4_out

pause