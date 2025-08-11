@echo off
IF EXIST Calibration_5\SVIHM_Calibration_5_out.#modlin (
set INPUT=
set /P INPUT="Linear uncertainty data will be overwritten. Continue? (y/n):"
If "%INPUT%"=="n" EXIT
If "%INPUT%"=="N" EXIT
)
cd ..
copy UCODE\SVIHM_postcal_sensitivity_5.in Calibration_5\SVIHM_postcal_sensitivity_5.in
copy UCODE\SVIHM_linearity_5.in Calibration_5\SVIHM_linearity_5.in
copy Batch_Scripts\Run_SVIHM_forward.bat Calibration_5\Run_SVIHM_forward.bat
copy UCODE\UCODE_Input_Files\SVIHM_Calibration_5_out.corfac Calibration_5\SVIHM_Calibration_5_out.corfac
cd Calibration_5


REM Run Post-Calibration sensitivity analysis to replace values for parameters that went inactive during calibration
"C:\wrdapp\UCODE_2014_1.004_and_more\UCODE_2014_1.004\bin\UCODE_2014.exe"  SVIHM_postcal_sensitivity_5.in SVIHM_Calibration_5_out 

REM Run RESIDUAL_ANALYSIS_ADV
"C:\wrdapp\UCODE_2014_1.004_and_more\UCODE_2014_1.004\bin\residual_analysis_adv.exe"  SVIHM_Calibration_5_out 

REM Run UCODE in model lineary mode
"C:\wrdapp\UCODE_2014_1.004_and_more\UCODE_2014_1.004\bin\UCODE_2014.exe"  SVIHM_linearity_5.in SVIHM_Calibration_5_out

REM Run MODEL_LINEARITY program
"C:\wrdapp\UCODE_2014_1.004_and_more\UCODE_2014_1.004\bin\model_linearity.exe" SVIHM_Calibration_5_out

REM Run LINEAR_UNCERTAINTY program
"C:\wrdapp\UCODE_2014_1.004_and_more\UCODE_2014_1.004\bin\linear_uncertainty.exe" SVIHM_Calibration_5_out

REM Run CORFAC_PLUS program
"C:\wrdapp\UCODE_2014_1.004_and_more\UCODE_2014_1.004\bin\corfac_plus.exe" SVIHM_Calibration_5_out
pause

