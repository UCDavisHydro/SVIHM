@echo off

for /L %%i in (1,1,6) do (  REM copy only necessary files for Calibration analysis run            
  MKDIR Calibration_1\Runner%%i
  copy Calibration_1\* Calibration_1\Runner%%i\*
  copy Run_SVIHM_PEST_Sens_Runner.bat Calibration_1\Runner%%i\Run_SVIHM_PEST_Sens.bat
  copy Update_SVIHMv3_Drain_Inflows.R Calibration_1\Runner%%i\Update_SVIHM_Drain_Inflows.R
  copy Update_Starting_Heads.R Calibration_1\Runner%%i\Update_Starting_Heads.R 
)

copy Run_SVIHM_PEST_Sens_Master.bat Calibration_1\Run_SVIHM_PEST_Sens.bat
copy Update_SVIHM_Drain_Inflows.R Calibration_1\Update_SVIHM_Drain_Inflows.R
copy Update_SVIHM_Starting_Heads.R Calibration_1\Update_SVIHM_Starting_Heads.R 

cd Calibration_1
start "Master" "beopest64" "SVIHM_sensitivity_1 /H :4004" 
       
for /L %%i in (1,1,6) do (
cd Runner%%i
start "Runner%%i" "beopest64" "SVIHM_sensitivity_1 /H METRO-VH138A:4004"
timeout 1
cd ..
)