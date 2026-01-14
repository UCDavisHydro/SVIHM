#--------------------------------------------------------------------------#

         .d8888b.  888     888 8888888 888    888 888b     d888 
        d88P  Y88b 888     888   888   888    888 8888b   d8888 
        Y88b.      888     888   888   888    888 88888b.d88888 
         "Y888b.   Y88b   d88P   888   8888888888 888Y88888P888 
            "Y88b.  Y88b d88P    888   888    888 888 Y888P 888 
              "888   Y88o88P     888   888    888 888  Y8P  888 
        Y88b  d88P    Y888P      888   888    888 888   "   888 
         "Y8888P"      Y8P     8888888 888    888 888       888 
                                                                
#--------------------------------------------------------------------------#
              Scott Valley Integrated Hydrologic Model (SVIHM)
		                  Ensemble Run Readme
                              1/13/2026
#--------------------------------------------------------------------------#

Running the full ensemble of SVIHM, or a scenario using the full ensemble,
is a bit trickier than running the "base" model  located in the main folder.
For that reason, we have included a high-level guide on running the full
ensemble in this file.

The instructions assume you are on Windows. Additional work and software will
be required to run the ensemble on mac/linux/etc. The instructions also
assume familiarity with PEST, PEST++, and GW modeling in general.

The intended workflow takes advantage of the same setup used to calibrate
the model using PEST++ ("PESTPP") and uses the PESTPP-SWP utility to allow 
for parallelized runs. It is important to note that SWP does not "save" each
folder it runs the model in, thus any output not related to the streamflow/
heads targets setup in the PEST PST file must be saved using the PESTPP
"panther_transfer_on_finish" command. For more information, check out the
PESTPP manual:

https://github.com/usgs/pestpp/blob/master/documentation/pestpp_users_manual.md

When running scenarios, keep in mind some parameters are controlled by PEST,
and others are not. Refer to the template files in order to determine if a 
parameter important to your scenario belongs to the latter or the former.
If it is not controlled by SWP, modify the model input file (*which may be
a template file SWP adjusts!*).
If it is controlled by SWP, set the parameter in the sweep parameter csv file.
(each line is an ensemble member). Note that setting a parameter to a constant
value across the ensemble implies that the parameter no longer has any
uncertainty within your scenario (which may be intended).

At the bottom of this document is a tree of the setup used to run a previous
SWP run. Use this to guide setting up your own run. This guide will proceed
with an explanation of where to find various files/utilities needed and then
finally a brief guide on starting the runs using panther. Some troubleshooting
is almost certainly needed given all the moving parts of the SVIHM ensemble
setup.

1. File/Utility locations:
- PEST input/template files: .\SVIHM_Input_Files\pest_files\
- Windows batch files:       .\SVIHM_Input_Files\pest_files\
- Python utilities           .\Utilities\python\
- SFR2PAR executable:        .\Utilities\SFR2PAR.exe
- Texture2Par executable:    .\Utilities\Texture2Par.exe
- Texture2Par files:         .\SVIHM_Input_Files\t2p_files\
- PEST++ executables:        https://github.com/usgs/pestpp/releases
- Example SWP Input File:    .\SVIHM_Input_Files\ensemble\

SVIHM itself can be created using the Prepare_Basecase_Run.bat file.

You will need to follow the instructions in python_readme.txt (located with
the python utilities) to setup the python environment needed to run the
python utilities.

2. Running the Ensemble:
It is suggested that you use the "start_all_swp.bat" batch file.
That script is setup to copy a folder named "svihm_template" with all the
necessary files (listed below) to multiple folders and run the simulations
in parallel. You will need to edit the "pest_worker_swp.bat" batch script
it calls and adjust the last line:

call ..\bin\pestpp-swp.exe svihm_swp.pst /h <ENTER_IP_ADDRESS>:5050

(Ensure the chosen port (default 5050) is open on the host machine and not 
blocked by Windows Firewall.)

where <ENTER_IP_ADDRESS> should be your host machine IP address. You can use
a single machine as both the host ("manager") and run multiple workers
("agents") on it. See PESTPP manual for more details.

3. File Hierarchy Overview (folders start with +)

[batch files]
+bin
 ¦   pestpp-swp.exe
+svihm_template
 ¦   [pest files]
 +---SVIHM
     +---Bin
	 ¦   [executables, python utilities]
     +---MODFLOW
	 ¦   [MODFLOW Files]
     +---preproc
	 ¦   [Texture2Par Files]
     +---SWBM
	 ¦   [SWBM Files]

4. Full Example File Hierarchy:

start_all_swp.bat
pest_worker_swp.bat
+bin
 ¦   pestpp-swp.exe
+svihm_template
 ¦   calib_iter3_sweep.csv
 ¦   catchment_mult.txt.tpl
 ¦   forward_run.bat
 ¦   head_obs_reader.ins
 ¦   landcover_table.txt.tpl
 ¦   pest_host.bat
 ¦   pp_kv_var_L1.dat.tpl
 ¦   pp_kv_var_L2.dat.tpl
 ¦   run_pest.bat
 ¦   scale_pp_Fine_L1.dat.tpl
 ¦   scale_pp_Fine_L2.dat.tpl
 ¦   scale_pp_Mixed_Coarse_L1.dat.tpl
 ¦   scale_pp_Mixed_Coarse_L2.dat.tpl
 ¦   scale_pp_Mixed_Fine_L1.dat.tpl
 ¦   scale_pp_Mixed_Fine_L2.dat.tpl
 ¦   scale_pp_Sand_L1.dat.tpl
 ¦   scale_pp_Sand_L2.dat.tpl
 ¦   scale_pp_Very_Coarse_L1.dat.tpl
 ¦   scale_pp_Very_Coarse_L2.dat.tpl
 ¦   sfr2par.in.tpl
 ¦   Streamflow_AS_SVIHM_MidptFlow_LOG.ins
 ¦   Streamflow_BY_SVIHM_MidptFlow_LOG.ins
 ¦   Streamflow_FJ_SVIHM_MidptFlow_LOG.ins
 ¦   Streamflow_FJ_SVIHM_VOL.ins
 ¦   streamflow_multipliers.txt.tpl
 ¦   Streamflow_SCK_SVIHM_MidptFlow_LOG.ins
 ¦   svihmt2p.tpl
 ¦   svihm_swp.pst
 ¦   t2p_par2par.in.tpl
 ¦   
 +---SVIHM
     ¦   svihm.bat
     ¦   
     +---Bin
     ¦       GAGE2VOL.py
     ¦       HOBPOSTPROC.py
     ¦       LOGSTRSIM.py
     ¦       MODFLOW-NWT.exe
     ¦       par2par.exe
     ¦       RES2PAR_preproc.py
     ¦       SFR2PAR.exe
     ¦       STREAM_ADJUSTER.py
     ¦       SWBM.exe
     ¦       Texture2Par.exe
     ¦       
     +---MODFLOW
     ¦       AS_log.csv
     ¦       BY_log.csv
     ¦       FJ_log.csv
     ¦       head_obs_master.csv
     ¦       SCK_log.csv
     ¦       Starting_Heads_L1.txt
     ¦       Starting_Heads_L2.txt
     ¦       SVIHM.bas
     ¦       SVIHM.dis
     ¦       SVIHM.drno
     ¦       SVIHM.gag
     ¦       SVIHM.hob
     ¦       SVIHM.nam
     ¦       SVIHM.nwt
     ¦       SVIHM.oc
     ¦       SVIHM.upw
     ¦       SVIHM_t2p.nam
     ¦       
     +---preproc
     ¦       aemlogs.csv
     ¦       kv_mult_pp_L1.fac
     ¦       kv_mult_pp_L2.fac
     ¦       lithologs.csv
     ¦       lognorm_dist_clustered.par
     ¦       scale_pp_L1.fac
     ¦       scale_pp_L2.fac
     ¦       svihm.t2p
     ¦       SVIHM_TEMPLATE.upw
     ¦       
     +---SWBM
     ¦       ag_well_list_by_polygon.txt
     ¦       ag_well_summary.txt
     ¦       catchment_mult.txt
     ¦       curtailment_fractions.txt
     ¦       ET_Cells_Extinction_Depth.txt
     ¦       et_segments.txt
     ¦       ET_Zone_Cells.txt
     ¦       field_et_corrections.txt
     ¦       irr_ditch.txt
     ¦       kc_values.txt
     ¦       landcover_table.txt
     ¦       MAR_depth.txt
     ¦       MF_Polygon_Overlaps.txt
     ¦       modflow_cell_to_catchment.txt
     ¦       monthly_MFR_by_catchment.txt
     ¦       polygons_table.txt
     ¦       polygon_landcover_ids.txt
     ¦       precip.txt
     ¦       precip_factors.txt
     ¦       ref_et.txt
     ¦       SFR_inflow_segments.txt
     ¦       SFR_network.txt
     ¦       SFR_network_jtf.txt
     ¦       SFR_routing.txt
     ¦       stress_period_days.txt
     ¦       subwatershed_irrigation_inflows.txt
     ¦       subwatershed_irrigation_inflows_raw.txt
     ¦       subwatershed_nonirrigation_inflows.txt
     ¦       subwatershed_nonirrigation_inflows_raw.txt
     ¦       SVIHM.sfr
     ¦       svihm.swbm
     ¦       SVIHM_ETS_template.txt
     ¦       SVIHM_WEL_template.txt
	 
	 
Before running, verify that:
- All relative paths in forward_run.bat are correct
- Python scripts are executed from an activated environment
- Required executables are present in SVIHM\Bin

SWP may continue running even if individual ensemble members fail. Always inspect logs and
transferred output files to verify successful completion.