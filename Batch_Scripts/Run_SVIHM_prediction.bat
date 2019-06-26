@ECHO OFF

REM Run basecase version of SVIHM
cp drains_initial_m3day.txt drains_m3day.txt
SWBM.exe
MF_OWHM SVIHM_bc.nam
Rscript Update_SVIHM_Drain_Inflows.R
SWBM.exe
MF_OWHM SVIHM_bc.nam

REM Run MAR version of SVIHM
cp drains_initial_m3day.txt drains_m3day.txt
powershell -Command "(gc general_inputs.txt) -replace 'Basecase', 'MAR' | Out-File general_inputs.txt -Encoding ASCII"
SWBM.exe
MF_OWHM SVIHM_scen.nam
Rscript Update_SVIHM_Drain_Inflows.R
SWBM.exe
MF_OWHM SVIHM_scen.nam