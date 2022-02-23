@setlocal
@echo off
set scen=basecase

:: Intended to be run in the main SVIHM folder

:: Create Run folders (if does not exist)
if not exist "Run" mkdir Run
if not exist "Run\SWBM" mkdir Run\SWBM
if not exist "Run\MODFLOW" mkdir Run\MODFLOW

:: Copy scenario independant MODFLOW model files
xcopy SVIHM_Input_Files\time_independent_input_files\SVIHM.* Run\MODFLOW /Y /I
xcopy SVIHM_Input_Files\time_independent_input_files\Starting_Heads_L*.txt Run\MODFLOW /Y /I

:: Copy files from scenario folder to run folder
xcopy Scenarios\%scen%\*.txt Run\SWBM /Y /I
xcopy Scenarios\%scen%\*.zone Run\SWBM /Y /I
xcopy Scenarios\%scen%\SVIHM.* Run\MODFLOW /Y /I 
xcopy SVIHM_Input_Files\time_independent_input_files\SVIHM_*_template.txt Run\SWBM /Y /I

:: Copy in generic run batch file
xcopy Scripts\Batch_Scripts\Run_SVIHM.bat Run /Y /I

:: Copy in Update_Drain_Inflows input file
xcopy SVIHM_Input_Files\Update_Drain_Inflows.in Run\MODFLOW /Y /I

:: Files of Unknown Necessity
xcopy Calib_Sens_Files\UCODE\UCODE_Template_Files\SVIHM_SFR.jtf Run\SWBM /Y /I

:: Report
echo.
echo Run folder populated with %scen%. Model can be run using Run\Run_SVIHM.bat
pause