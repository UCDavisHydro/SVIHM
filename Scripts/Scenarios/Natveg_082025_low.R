# 02_BaseUpdate.R
#
#
library(RSVP)
library(sf)

# ------------------------------------------------------------------------------------------------#
# Settings ----------------------------------------------------------------------------------------

# Scenario Settings -----------------------------------------------------
scen <- list(
  'name'             = 'natveg_low',   # Scenario name, will be part of directory name
  'type'             = 'update',       # Basecase, Update, or PRMS - where to get meteorological inputs
  'natveg_kc'        = 0.6,            # Native vegetation daily ET coefficient, default = 0.6
  'natveg_rd'        = 0.9144,         # Native vegetation rooting depth (m), default = 2.4384 (8 ft)
  'natveg_rd_mult'   = 1.0,
  'natveg_extD'      = 1.8288          # Native vegetation extinction depth (m), default 0.5
)

# ------------------------------------------------------------------------------------------------#

# Scenario Processing -------------------------------------------------------------------------

# Add default items to scen related to scen$type
scen <- scenario_setup(scen)

# Create working directory
working_dir <- create_scenario_dir(scen$scen_dir)


# Read in basecase SWBM time-invariant files ------------------------------

landcover_desc <- read.table(scen$landcover_desc_file, header=T)
tributary_desc <- read_inflow_seg_file(scen$inflow_seg_file)

# Polygon File defines the SWBM Fields
# The special reader adds columns to translate numeric codes into
polygon_fields <- read_SWBM_polygon_file(scen$polygon_file, landcover_desc, tributary_desc)

# Create Time-Varying SWBM Inputs -----------------------------------------

#-- Days in month
num_days_df <-  data.frame("stress_period" = 1:scen$num_stress_periods, ndays = scen$num_days)

#-- Streamflow
subws_inflow_filename <- file.path(scen$input_dir,"daily_tributary_streamflow.txt")
subws_inflows <- process_sfr_inflows(scen, subws_inflow_filename)

# Historical Streamflow Curtailments for 2021, 2022
subws_inflows <- streamflow_curtailment(subws_inflows, percent = 1, date_start = "2021-09-10", date_end = "2021-10-25")
subws_inflows <- streamflow_curtailment(subws_inflows, percent = 1, date_start = "2022-07-01", date_end = "2022-12-27")

# Land use by field by month
# Valid scenario_ids are basecase, nv_gw_mix, and nv_all
landcover_df <- create_SWBM_landcover_df(scenario_id = 'nv_all',
                                         scen$start_date,
                                         scen$end_date,
                                         polygon_fields,
                                         landcover_desc)

# ET (both which cells have ET, and the extinction depths) Returns a list of matrices (by MODFLOW cell)
cell_et <- read_SWBM_ET_inputs(file_cells = file.path(data_dir["time_indep_dir","loc"], "ET_Zone_Cells.txt"),
                                      file_ext_depth = file.path(data_dir["time_indep_dir","loc"], "ET_Cells_Extinction_Depth.txt"))

# Matrix mapping SWBM fields to MODFLOW cells
cell_recharge  <- as.matrix(read.table(header = F,  file = file.path(data_dir["time_indep_dir","loc"], "recharge_zones.txt")))

# Update Native Vegetation Rooting Depth
nat_id <- landcover_desc[landcover_desc['Landcover_Name']=='Native_Vegetation', 'id']
landcover_desc[nat_id, 'RootDepth'] <- scen$natveg_rd
landcover_desc[nat_id, 'RD_Mult'] <- scen$natveg_rd_mult

#-- Crop coefficients (specified daily, change seaonally for some crops)
daily_kc_df <- create_daily_crop_coeff_df(scen$start_date, scen$end_date, natveg_kc=scen$natveg_kc)

# MAR applications by field by month
mar_depth_df <- create_MAR_depth_df(scen$start_date, scen$end_date, mar_scenario='none')

# Mountain Front Recharge (water passed through SWBM to MODFLOW)
mfr_df <- create_SWBM_MFR_df(num_days_df)

# Irrigation curtailment fractions (as fraction of calculated demand) by field by month
# Also includes Local Cooperative Solutions (LCSs) that reduce water use (implemented as curtailment)
curtail_df <- create_SWBM_curtailment_df(scen$start_date, scen$end_date, scenario_id='none')

# ET Correction file
# Includes LCSs that essentially reduce evaporated water losses
et_corr <- create_SWBM_ET_correction_df(scen$start_date, scen$end_date, scenario_id='none')

# Scenario-specific commands (please read documentation of commands)
# Uncomment if desired
# polygon_fields <- SWBM_no_pumping(polygon_fields)
cell_et <- apply_native_veg_ET_override(cell_et, cell_recharge, landcover_df, landcover_desc, scen$natveg_extD)
# curtail_df <- SWBM_monthly_curtailment(curtail_df, date_start, date_end)

# Optional: Plots for QA/QC
# plot_landcover(landcover_df, landcover_desc, stress_period="1990-10-01")
# plot_curtailment(curtail_df, stress_period="2024-08-01")
# plot_field_continuous(et_corr, stress_period="2024-08-01", plot_title=paste('ET Correction 2024-08-01'))
# plot_field_continuous(mar_depth_df, stress_period="2024-03-01", plot_title=paste('MAR Depth 2024-03-01'))

# ------------------------------------------------------------------------------------------------#

# Write SWBM Inputs to Working Dir -------------------------------------------------------
write_SWBM_sp_days_file(num_days_df, working_dir)
write_SWBM_daily_kc_file(daily_kc_df, working_dir)
write_SWBM_SFR_inflow_files(subws_inflows$irr,     working_dir, "subwatershed_irrigation_inflows.txt")
write_SWBM_SFR_inflow_files(subws_inflows$non_irr, working_dir, "subwatershed_nonirrigation_inflows.txt")
write_SWBM_landcover_file(landcover_df, working_dir)
write_SWBM_MAR_depth_file(mar_depth_df, working_dir)
write_SWBM_MFR_file(mfr_df, working_dir)
write_SWBM_curtailment_file(curtail_df, working_dir)
write_SWBM_polygon_file(polygon_fields, working_dir)
write_SWBM_landcover_desc_file(landcover_desc, working_dir)
write_SWBM_ET_inputs(cell_et, working_dir)
write_SWBM_ET_correction_file(et_corr, working_dir)

# SFR Template "Network" file (requires total time steps)
write_SWBM_sfr_template_file(nsteps = sum(scen$num_days), output_dir = working_dir)

# Main input file
write_SWBM_main_input_file(output_dir = working_dir,
                           num_stress_periods = scen$num_stress_periods,
                           nSubws = scen$nSubws,
                           nSFR_inflow_segs = scen$nSFR_inflow_segs)

# Copy meteorological files from the input_dir to the working_dir
file.copy(from=file.path(scen$input_dir, 'precip.txt'), to = working_dir)
file.copy(from=file.path(scen$input_dir, 'ref_et.txt'), to = working_dir)

# Write MODFLOW Inputs ----------------------------------------------------

# Discretization (DIS)
update_DIS_stress_periods(scen$num_days, scen$num_stress_periods, output_dir = working_dir)

# Drain (DRN)
update_DRNO_stress_periods(scen$num_stress_periods, output_dir = working_dir)

# Head Observations (HOB)
#write_SVIHM_head_obs_file(model_start_date, model_end_date, output_dir = update_dir)
# No longer written each time. Superseded by HOB file in basecase.

# Output Control (OC) (slow)
update_OC_stress_periods(scen$num_days,
                         scen$num_stress_periods,
                         output_dir = working_dir,
                         save_budget=F,
                         save_drawdown=F)

# ------------------------------------------------------------------------------------------------#

# Batch File --------------------------------------------------------------

# Create new batchfile for assembling the updated model
write_scen_prep_batchfile(scen_dir = working_dir, scenario_name = scen$name)

message('Done!')
