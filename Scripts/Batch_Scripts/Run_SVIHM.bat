@echo OFF
@setlocal
title Scott Valley Integrated Hydrologic Model
:: Set bindir to location of bin directory containing EXEs
:: relative to *inside* the SWBM/MODFLOW directories
set bindir=..\..\bin
::copy Drains_initial_m3day.txt Drains_m3day.txt

REM Run SWBM
cd SWBM
call %bindir%\SWBM.exe

REM Copy over new SWBM-generated MODFLOW files
cd ..\
xcopy SWBM\SVIHM.* MODFLOW /Y /I

REM Run MODFLOW
cd MODFLOW
call %bindir%\MF_OWHM.exe SVIHM.nam

REM Update Starting Heads              
REM Old command: Rscript Update_SVIHM_Starting_Heads.R
:: Command line arguments: heads_file_name SP_to_use
call %bindir%\Update_Starting_Heads.exe SVIHM.hds 2

cd ..\
REM Update drain flows going into big slough
REM Old command: Rscript Update_SVIHM_Drain_Inflows.R
:: Reads in file Update_Drain_Inflows.in
call %bindir%\Update_Drain_Inflows.exe

REM re-run SWBM to update SFR inflows
cd SWBM
call %bindir%\SWBM.exe

REM Copy over new SWBM-generated MODFLOW files
cd ..\
xcopy SWBM\SVIHM.* MODFLOW /Y /I

REM Run MODFLOW
cd MODFLOW
call %bindir%\MF_OWHM.exe SVIHM.nam