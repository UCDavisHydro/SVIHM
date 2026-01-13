@setlocal
@echo off
set scen=SVIHM_PRMS

:: Intended to be run in the main SVIHM folder

:: Create Run folders (if does not exist)
if not exist "Run" mkdir Run
if not exist "Run\SWBM" mkdir Run\SWBM
if not exist "Run\MODFLOW" mkdir Run\MODFLOW

:: Copy scenario independant MODFLOW model files
xcopy SVIHM_Input_Files\time_independent_input_files\ET_Cells_Extinction_Depth.txt Run\SWBM /Y /I
xcopy SVIHM_Input_Files\time_independent_input_files\MF_Polygon_Overlaps.txt Run\SWBM /Y /I
xcopy SVIHM_Input_Files\time_independent_input_files\SFR_routing.txt Run\SWBM /Y /I
xcopy SVIHM_Input_Files\time_independent_input_files\SFR_inflow_segments.txt Run\SWBM /Y /I
xcopy SVIHM_Input_Files\time_independent_input_files\ag_well_summary.txt Run\SWBM /Y /I
xcopy SVIHM_Input_Files\time_independent_input_files\ag_well_list_by_polygon.txt Run\SWBM /Y /I
xcopy SVIHM_Input_Files\time_independent_input_files\precip_factors.txt Run\SWBM /Y /I
xcopy SVIHM_Input_Files\time_independent_input_files\irr_ditch.txt Run\SWBM /Y /I
xcopy SVIHM_Input_Files\time_independent_input_files\modflow_cell_to_catchment.txt Run\SWBM /Y /I
xcopy SVIHM_Input_Files\time_independent_input_files\SVIHM.* Run\MODFLOW /Y /I
copy /Y SVIHM_Input_Files\time_independent_input_files\SVIHM_tabfiles.nam Run\MODFLOW\SVIHM.nam
xcopy SVIHM_Input_Files\time_independent_input_files\Starting_Heads_L*.txt Run\MODFLOW /Y /I
xcopy SVIHM_Input_Files\time_independent_input_files\SVIHM_*_template.txt Run\SWBM /Y /I
xcopy SVIHM_Input_Files\time_independent_input_files\SFR_network_jtf.txt Run\SWBM /Y /I

:: Copy in generic run batch file
xcopy Scripts\Batch_Scripts\Run_SVIHM.bat Run /Y /I

:: Copy Updated Files to run folders
xcopy Scenarios\PRMS_prms_2025-08-31\svihm.swbm Run\SWBM /Y /I
xcopy Scenarios\PRMS_prms_2025-08-31\*.txt Run\SWBM /Y /I
xcopy Scenarios\PRMS_prms_2025-08-31\SVIHM.* Run\MODFLOW /Y /I && del Run\MODFLOW\svihm.swbm

:: Report
echo.
echo Run folder populated with %scen%. Model can be run using Run\Run_SVIHM.bat
pause
