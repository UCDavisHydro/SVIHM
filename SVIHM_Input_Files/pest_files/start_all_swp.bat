@echo off
setlocal enabledelayedexpansion

:: Rename window
title PESTpp Starter

:: Ask if this is the host machine
set /p is_host="Is this the host machine? (Y/N): "

:: Set the number of copies to create
set /p n="Enter the number of workers to create: "

:: Set the source folder
set "source_folder=svihm_template"

:: Check if the source folder exists
if not exist "%source_folder%" (
    echo Source folder "%source_folder%" does not exist.
    exit /b
)

if /I "%is_host%"=="Y" (
	set "dest_folder=SVIHM_Controller"
    echo Copying to !dest_folder!...
    xcopy /E /I "%source_folder%" "!dest_folder!"
)

:: Loop to create n copies
for /l %%i in (1,1,%n%) do (
    set "num=0%%i"
    set "num=!num:~-2!"
    set "dest_folder=wrkr_!num!"

    echo Copying to !dest_folder!...
    xcopy /E /I "%source_folder%" "!dest_folder!"
)

:: Only start the host if this machine is the host
if /I "%is_host%"=="Y" (
    echo "Workers Created. Starting SWP Host..."

    :: Start Controller (Host)
    cd SVIHM_Controller
    start cmd.exe /k pest_host.bat
    cd ../

    echo "Host created. Starting workers..."
) else (
    echo "Skipping host startup. Starting workers..."
)

:: start python env
call conda activate SV_AEM_T2P

:: Start Worker Loop
for /D %%F in ("*wrkr*") do (
    start cmd.exe /k pest_worker_swp.bat %%F
)