@ECHO OFF
copy Drains_initial_m3day.txt Drains_m3day.txt

REM Run SWBM
SWBM.exe

REM Run MODFLOW
MF_OWHM.exe "SVIHM.nam"

REM Update drain flows going into big slough
Rscript Update_SVIHM_Drain_Inflows.R
REM Update Starting Heads              
Rscript Update_SVIHM_Starting_Heads        

REM re-run SWBM to update SFR inflows
SWBM.exe

REM Run MODFLOW
MF_OWHM.exe "SVIHM.nam"