@ECHO OFF

cp Drains_initial_m3day.txt Drains_m3day.txt  REM Set drain inflow to zero 

SWBM.exe                                      REM run SWBM
MF_OWHM.exe SVIHM.nam                         REM run MODFLOW


