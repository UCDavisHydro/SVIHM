@ECHO OFF
copy Drains_initial_m3day.txt Drains_m3day.txt

REM Run SWBM
SWBM.exe
"C:\Program Files\R\R-4.0.2\bin\Rscript.exe" Add_Reservoir_Pipe_Releases_to_SFR_Inputs.R

REM Run MODFLOW
MF_OWHM.exe "SVIHM.nam"

REM Update drain flows going into big slough
REM Old command: Rscript Update_SVIHM_Drain_Inflows.R
"C:\Program Files\R\R-4.0.2\bin\Rscript.exe" Update_SVIHM_Drain_Inflows.R
REM Update Starting Heads              
REM Old command: Rscript Update_SVIHM_Starting_Heads.R       
"C:\Program Files\R\R-4.0.2\bin\Rscript.exe" Update_SVIHM_Starting_Heads.R

REM re-run SWBM to update SFR inflows
SWBM.exe
"C:\Program Files\R\R-4.0.2\bin\Rscript.exe" Add_Reservoir_Pipe_Releases_to_SFR_Inputs.R

REM Run MODFLOW
MF_OWHM.exe "SVIHM.nam"