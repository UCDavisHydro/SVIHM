#Generate input files for SVIHM

library(lubridate)
library(stringr)
library(dplyr)
library(DBI)
#spatial tools
library(raster)
library(rpostgis)
library(rgdal)
#library(postGIStools) # pull from gis server
library(rgeos) # for buffer function
library(dataRetrieval) # for FJ flow download
library(lubridate)


# Settings ----------------------------------------------------------------

# Recharge and flow scenarios
recharge_scenario = "Basecase" # Can be Basecase/MAR/ILR/MAR_ILR/MAR_ILR_max
if(recharge_scenario == "MAR_ILR_max"){max_infil_rate_for_unknowns = 0.035}
flow_scenario = "Basecase" # Can be Basecase/Flow_Lims/FlowLimsMAR. Flow limits on stream diversion specified in "available ratio" table.

# Irrigation demand: Different from the kc_CROP_mult values in crop_coeff_mult.txt, which is used for calibrating.
irr_demand_mult = 1 # Can be 1 (Basecase) or < 1 or > 1 (i.e., reduced or increased irrigation; assumes land use change)
# set nat veg kc under Land Use Scenario.

# Curtailing all irrigation scenarios:
curtailment_scenario = "NoCurtail" # Default is "NoCurtail". "YesCurtail"
curtail_start_mo = 8 # Month as month of year (7 = July)
curtail_start_day = 15
curtail_yrs_flag = "AllYears" # default is "AllYears". Currently not set up for "DryYearsOnly" for 91, 92, 94, 01, 09, 13, 14 and 18
# Convert to month of wy (Oct=1, Nov=2, ..., Jul = 10, Aug=11, Sep=0)
if(curtail_start_mo<9){curtail_start_mo = curtail_start_mo + 3
}else{curtail_start_mo = curtail_start_mo - 9}

# Month and day of the final day of alfalfa irrigation season.
# Default is Aug 31, 8/31
alf_irr_stop_mo = 8 # Month as month of year (7 = July)
alf_irr_stop_day = 31
early_cutoff_flag = "AllYears" # default is "AllYears" ; alt is "DryYearsOnly" for 91, 92, 94, 01, 09, 13, 14 and 18
# Convert to month of wy (Oct=1, Nov=2, ..., Jul = 10, Aug=11, Sep=0)
if(alf_irr_stop_mo<9){alf_irr_stop_mo = alf_irr_stop_mo + 3
}else{alf_irr_stop_mo = alf_irr_stop_mo - 9}

# Reservoir scenario
reservoir_scenario = "Basecase"#"French"#"Shackleford" #"Basecase" #"Shackleford","French", "Etna", "South_Fork"
reservoir_plus_pipeline = FALSE    # set pipeline status
reservoir_capacity =   "Basecase" #59.504 * 150 * 7.5 # Basecase or,  59.504 or 119.01 (30 or 60 cfs/day in AF) * 150 (days of dry season) * n years
reservoir_start = "empty" # "empty" "full" or a fraction of capacity, or a number of AF
dry_season_release_cfs = 30 # typically 30 or 60 cfs

# BDAs scenario
BDAs_scenario = "Basecase" # Can be Basecase/Tributaries/All_Streams/Scott_R_Mainstem
if(tolower(BDAs_scenario) != "basecase"){ stream_bed_elev_increase = 0.5} # set average stream bed elevation increase

# Irrigation Efficiency scenario
irr_eff_scenario = "Basecase" # Basecase, or set irr efficiency increase or decrease amount (not applied to flood irrigation)

#Land use scenario.
landuse_scenario ="basecase" # Default: basecase. For attribution study: major_natveg
if(landuse_scenario=="major_natveg"){ # Default: 0.6. Set at 1.0 for major natveg scenarios.
  # natveg_kc = 0.6
  natveg_kc = 1.0
  extinction_depth_value = 10 # 4.5 # extinction depth outside the discharge zone
} else if(tolower(landuse_scenario)=="basecase"){
  natveg_kc = 0.6
}
# landuse_scenario_detail = "native veg, gw and mixed fields, outside adj"
# landuse_scenario_detail = "native veg outside adj"
# landuse_scenario_detail = "native veg, gw and mixed fields, inside adj"
# landuse_scenario_detail = "native veg inside adj"
# landuse_scenario_detail = "native veg, gw and mixed fields, all cultivated fields"
# landuse_scenario_detail = "native veg all cultivated fields"

# Overall scenario identifier. Also makes the directory name; must match folder
scenario_name = "basecase"
# scenario_name = "mar_ilr" # "ilr" "mar"
# scenario_name = "mar_ilr_max_0.019" # Options: 0.035, 0.003, or 0.019 (the arithmetic mean) or 0.01 (the geometric mean)
# scenario_name = "mar_ilr_flowlims"#"flowlims"
# scenario_name = "irrig_0.8"#"irrig_0.9" #
# scenario_name = "curtail_start_jun01"
# scenario_name = "curtail_start_jun15"
# scenario_name = "curtail_start_jul01"
# scenario_name = "curtail_start_jul15"
# scenario_name = "curtail_start_aug01"
# scenario_name = "curtail_start_aug15"
# scenario_name = "alf_irr_stop_jul10"
# scenario_name = "alf_irr_stop_aug01"
# scenario_name = "alf_irr_stop_aug01_dry_yrs_only"
# scenario_name = "alf_irr_stop_aug15"
# scenario_name = "alf_irr_stop_aug15_dry_yrs_only"
# scenario_name = "natveg_outside_adj"
# scenario_name = "natveg_gwmixed_outside_adj"
# scenario_name = "natveg_inside_adj"
# scenario_name = "natveg_gwmixed_inside_adj"
# scenario_name = "natveg_all"
# scenario_name = "natveg_gwmixed_all"
# scenario_name = "reservoir_shackleford" # "reservoir_etna" "reservoir_sfork" "reservoir_shackleford"
# scenario_name = "reservoir_pipeline_etna"
# scenario_name = "reservoir_etna_29KAF"
# scenario_name = "reservoir_etna_134kAF_60cfs"
# scenario_name = "reservoir_pipeline_etna_29KAF"
# scenario_name = "reservoir_pipeline_etna_134kAF_60cfs"
# scenario_name = "bdas_all_streams" # "bdas_tribs" "bdas_scott_r"
# scenario_name = "irr_eff_improve_0.2"
# scenario_name = "natveg_all_et_check_1.0nvkc_10m_ext"


# Build Scenario ----------------------------------------------------------

# 1) Set drives for collecting all SWBM input files and SVIHM modflow files

# 1a) Set project directory.
#This code allows it to automatically detect the location of this R script.
isRStudio <- Sys.getenv("RSTUDIO") == "1"
if(isRStudio == TRUE){ library(rstudioapi); svihm_dir <- dirname(dirname(getActiveDocumentContext()$path))}
if(isRStudio == FALSE){ library(here); svihm_dir <- dirname(here::here("Update_SVIHM_Inputs.R"))}

