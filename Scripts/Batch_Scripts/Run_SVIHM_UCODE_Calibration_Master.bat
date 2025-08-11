@ECHO OFF
REM Run SWBM
SWBM.exe

REM Run MODFLOW
MF_OWHM.exe "SVIHM.nam"

REM Update drain flows going into big slough
Rscript Update_SVIHM_Drain_Inflows.R
REM Update Starting Heads              
Rscript Update_SVIHM_Starting_Heads.R      
for /L %%i in (1,1,6) do (  REM copy updated files to runner folders            
  copy Drains_m3day.txt .\Runner%%i\Drains_m3day.txt
  copy Starting_Heads_L1.txt .\Runner%%i\Starting_Heads_L1.txt
  copy Starting_Heads_L2.txt .\Runner%%i\Starting_Heads_L2.txt
)

REM re-run SWBM to update SFR inflows
SWBM.exe

REM Run MODFLOW
MF_OWHM.exe "SVIHM.nam"

find /I "****FAILED" SVIHM.lst >> SVIHM_Conv_Fails.rec           
find /I "PERCENT DISCREPANCY" SVIHM.lst >> SVIHM_Mass_Balance.rec