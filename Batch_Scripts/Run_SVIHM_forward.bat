@ECHO OFF
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