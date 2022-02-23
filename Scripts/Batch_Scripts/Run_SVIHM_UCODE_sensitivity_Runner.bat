@ECHO OFF

REM Run SWBM
SWBM.exe

REM Delay execution so counter file is not read simultaneously
set /a num=%random% %%20+1
timeout %num%

REM Update SFR parameters
SFR_params_r.exe

REM Run MODFLOW
MF_OWHM.exe "SVIHM.nam"

REM Copy listing file for each sensitivity run for troubleshooting
find /I "****FAILED" SVIHM.lst >> ..\SVIHM_Conv_Fails.rec
find /I "PERCENT DISCREPANCY" SVIHM.lst >> ..\SVIHM_Mass_Balance.rec
     