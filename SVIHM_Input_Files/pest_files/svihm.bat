:: load python 
call conda activate SV_AEM_T2P

:: Run par2par
call Bin\par2par.exe t2p_par2par.in

:: Run preproc, T2P
cd preproc
python ..\Bin\RES2PAR_preproc.py
call ..\Bin\Texture2Par.exe svihm.t2p

:: Adjust streamflow, Run SWBM
cd ..\SWBM
del subwatershed_nonirrigation_inflows.txt
del subwatershed_irrigation_inflows.txt
python ..\Bin\STREAM_ADJUSTER.py
call ..\Bin\SWBM.exe svihm.swbm

:: Copy over new SWBM-generated MODFLOW files
cd ..\
xcopy SWBM\SVIHM.* MODFLOW /Y /I
xcopy SWBM\SVIHM_tabfile_seg*.tab MODFLOW /Y /I 

:: Run SFR2Par
copy MODFLOW\SVIHM.sfr MODFLOW\SVIHM_intermediate.sfr /Y
del MODFLOW\SVIHM.sfr
call Bin\SFR2PAR.exe sfr2par.in

:: Run MODFLOW
cd MODFLOW
call ..\Bin\MODFLOW-NWT.exe SVIHM.nam

:: Run python post processors
python ..\Bin\LOGSTRSIM.py Streamflow_FJ_SVIHM.dat MidptFlow --obs-csv FJ_log.csv
python ..\Bin\LOGSTRSIM.py Streamflow_AS_SVIHM.dat MidptFlow --obs-csv AS_log.csv
python ..\Bin\LOGSTRSIM.py Streamflow_BY_SVIHM.dat MidptFlow --obs-csv BY_log.csv
python ..\Bin\LOGSTRSIM.py Streamflow_SCK_SVIHM.dat MidptFlow --obs-csv SCK_log.csv
python ..\Bin\GAGE2VOL.py Streamflow_FJ_SVIHM.dat 1990-09-30 5
python ..\Bin\HOBPOSTPROC.py HobData_SVIHM.dat head_obs_master.csv
conda deactivate

cd ..\