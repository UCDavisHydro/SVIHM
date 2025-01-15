
#-------------------------------------------------------------------------------------------------#

#' Write SWBM Drains Input Files (Drain_m3day.txt, Drains_initial_m3day.txt)
#'
#' @param num_stress_periods Number of model stress periods, determining the length of values array
#' to be written to the drains file
#' @param output_dir directory to write the files in
#' @param values Values to write to drain files, by default (NULL) will write zeroes
#' @param drains_filename Optional, default "Drains_m3day.txt"
#' @param drains_iniital_filename Optional, default "Drains_initial_m3day.txt"
#' @param verbose T/F write status info to console (default: TRUE)
#'
#' @return None
#' @export
#'
#' @examples write_SWBM_drain_files(num_stress_periods=336,
#' @examples output_dir = getwd(),
#' @examples values = NULL,
#' @examples drains_filename = "Drains_m3day.txt",
#' @examples init_filename = "Drains_initial_m3day.txt",
#' @examples verbose = TRUE)
#'
write_SWBM_drain_files <- function(num_stress_periods,
                                   output_dir,
                                   values=NULL,
                                   drains_filename="Drains_m3day.txt",
                                   init_filename="Drains_initial_m3day.txt",
                                   verbose=TRUE) {
  if (is.null(values)) {
    values <- rep(0, num_stress_periods)
  } else if (length(values) != num_stress_periods) {
    stop('values array too short - need one value for each stress period.')
  }
  drains_vector = c("#Initial Drain Flow", values)

  if (verbose) {message(paste('Writing file: ', drains_filename))}

  write.table(drains_vector, file = file.path(output_dir, drains_filename),
              sep = " ", quote = FALSE, col.names = FALSE, row.names = FALSE)

  if (verbose) {message(paste('Writing SWBM file: ', init_filename))}
  write.table(drains_vector, file = file.path(output_dir, init_filename),
              sep = " ", quote = FALSE, col.names = FALSE, row.names = FALSE)
}

#-------------------------------------------------------------------------------------------------#

#' Write SWBM General Inputs File (DEPRECIATED)
#'
#' Writes all basecase values by default
#'
#' @param num_stress_periods Number of model stress periods
#' @param output_dir directory to write the files to
#' @param filename general input filename, general_inputs.txt by default
#' @param recharge_scenario character
#' @param flow_scenario character
#' @param alf_irr_stop_mo integer Calendar month
#' @param alf_irr_stop_day integer
#' @param early_cutoff_flag character
#' @param curtailment_scenario character
#' @param curtail_start_mo integer
#' @param curtail_start_day integer
#' @param landuse_scenario character
#' @param verbose T/F write status info to console (default: TRUE)
#'
#' @return None
#' @export
#'
#' @examples
write_SWBM_gen_inputs_file_orig <- function(num_stress_periods,
                                       output_dir,
                                       filename="general_inputs.txt",
                                       recharge_scenario="Basecase",
                                       flow_scenario="Basecase",
                                       alf_irr_stop_mo=8,
                                       alf_irr_stop_day=31,
                                       early_cutoff_flag="AllYears",
                                       curtailment_scenario="NoCurtail",
                                       curtail_start_mo=8,
                                       curtail_start_day=15,
                                       landuse_scenario="basecase",
                                       verbose=TRUE) {

  # Convert months from calendar months to WY months
  if(alf_irr_stop_mo<9){alf_irr_stop_mo = alf_irr_stop_mo + 3
  }else{alf_irr_stop_mo = alf_irr_stop_mo - 9}

  if(curtail_start_mo<9){curtail_start_mo = curtail_start_mo + 3
  }else{curtail_start_mo = curtail_start_mo - 9}

  gen_inputs = c(
    paste("2119  167", num_stress_periods,
          "440  210  1.4 UCODE",
          "! num_fields, num_irr_wells, num_stress_periods, nrow, ncol, RD_Mult, UCODE/PEST",
          sep = "  "),
    paste(recharge_scenario, flow_scenario,
          "! Basecase/MAR/ILR/MAR_ILR, Basecase/Flow_Lims",
          sep = "  "),
    paste(alf_irr_stop_mo, alf_irr_stop_day, early_cutoff_flag,
          "! alf_irr_stop_mo  alf_irr_stop_day early_alf_cutoff_scenario",
          sep = "  "),
    paste(curtailment_scenario, curtail_start_mo, curtail_start_day,
          "! curtailment_scenario curtail_start_mo curtail_start_day",
          sep = "  "),
    paste(landuse_scenario, "! Basecase/Major_NatVeg"),
    paste('irr_ditch.txt           ! Irrigation Ditch Module Input File')

  )

  if (verbose) {message(paste('Writing SWBM file: ', filename))}
  write.table(gen_inputs, file = file.path(output_dir, filename),
              sep = " ", quote = FALSE, col.names = FALSE, row.names = FALSE)
}

#-------------------------------------------------------------------------------------------------#

#' Write SWBM General Inputs File
#'
#' Writes all basecase values by default
#'
#' @param num_stress_periods Number of model stress periods
#' @param output_dir directory to write the files to
#' @param filename general input filename, general_inputs.txt by default
#' @param recharge_scenario character
#' @param flow_scenario character
#' @param alf_irr_stop_mo integer Calendar month
#' @param alf_irr_stop_day integer
#' @param early_cutoff_flag character
#' @param curtailment_scenario character
#' @param curtail_start_mo integer
#' @param curtail_start_day integer
#' @param landuse_scenario character
#' @param verbose T/F write status info to console (default: TRUE)
#'
#' @return None
#' @export
#'
#' @examples
write_SWBM_gen_inputs_file <- function(output_dir,
                                       num_stress_periods,
                                       filename="general_inputs.txt",
                                       modelName = "SVIHM",
                                       WYstart = 1991,
                                       npoly = 2119,
                                       nlandcover = 6,
                                       nAgWells = 167,
                                       nMuniWells = 0,
                                       nSubws = 8,
                                       inflow_is_vol = FALSE,
                                       daily_sw = TRUE,
                                       nSFR_inflow_segs = 12,
                                       nrows = 440,
                                       ncols = 210,
                                       RD_Mult = 1.4,
                                       calib_software = "UCODE",
                                       using_neighbor_irr_rule = TRUE,
                                       scenario_id = "basecase",
                                       irr_ditch_file = 'irr_ditch.txt',
                                       verbose=TRUE) {


  gen_inputs = c(
    paste(modelName, WYstart, npoly, nlandcover, nAgWells, nMuniWells, nSubws,
          "! modelName, WYstart, npoly, nlandcover, nAgWells, nMuniWells, nSubws",
          sep = "  "),
    paste(inflow_is_vol, daily_sw, nSFR_inflow_segs, num_stress_periods, nrows, ncols,
          "! inflow_is_vol, daily_sw, nSFR_inflow_segs, nmonths, nrows, ncols",
          sep = "  "),
    paste(RD_Mult, calib_software, "! RD_Mult, UCODE/PEST",
          sep = "  "),
    paste(using_neighbor_irr_rule, "! using_neighbor_irr_rule (do farmers look to neighbor behavior for irrigation onset [TRUE] or only own field's soil moisture [FALSE])",
          sep = "  "),
    paste('irr_ditch.txt', '           ! Irrigation Ditch Module Input File'),
    paste(scenario_id, "!scenario_id (used only in postprocessing)"))


  if (verbose) {message(paste('Writing SWBM file: ', filename))}
  write.table(gen_inputs, file = file.path(output_dir, filename),
              sep = " ", quote = FALSE, col.names = FALSE, row.names = FALSE)
}

#-------------------------------------------------------------------------------------------------#

#' Write SWBM Main Input File (svihm.swbm)
#'
#' This function writes the main input file for the Soil Water Budget Model (SWBM).
#'
#' @param output_dir Directory where the input file will be written.
#' @param num_stress_periods Integer. Number of model stress periods (NMONTHS in the input file).
#' @param filename Character. The name of the output file. Default is "svihm.swbm".
#' @param modelName Character. The name of the MODFLOW model. Default is "SVIHM".
#' @param WYstart Integer. The water year start (WYSTART in the input file). Default is 1991.
#' @param npoly Integer. The number of polygons (NPOLY in the input file). Default is 2119.
#' @param nlandcover Integer. The number of land cover types (NLANDCOVER in the input file). Default is 6.
#' @param nAgWells Integer. The number of agricultural wells (NAGWELLS in the input file). Default is 167.
#' @param nMuniWells Integer. The number of municipal wells (NMUNIWELLS in the input file). Default is 0.
#' @param nSubws Integer. The number of sub-watersheds (NSUBWS in the input file). Default is 8.
#' @param inflow_is_vol Logical. Whether the inflow is volume-based. Default is FALSE.
#' @param daily_sw Logical. Whether to write daily soil water budget (DAILY_SW in the input file). Default is TRUE.
#' @param nSFR_inflow_segs Integer. The number of streamflow routing inflow segments (NSFR_INFLOW_SEGS in the input file). Default is 12.
#' @param nrows Integer. The number of rows in the model grid (NROWS in the input file). Default is 440.
#' @param ncols Integer. The number of columns in the model grid (NCOLS in the input file). Default is 210.
#' @param RD_Mult Numeric. The root depth multiplier (RD_MULT in the input file). Default is 1.4.
#' @param neighborRuleYearDay Integer. The neighbor rule cutoff day of the year (NEIGHBOR_RULE in the input file). Default is 250.
#' @param absoluteIrrDate Integer vector of length 2. The absolute irrigation cutoff date as month and day (ABSOLUTE_IRR_DATE in the input file). Default is c(5, 15).
#' @param writeUCODE Logical. Whether to write UCODE files (WRITE_UCODE in the input file). Default is TRUE.
#' @param writePEST Logical. Whether to write PEST files (WRITE_PEST in the input file). Default is FALSE.
#' @param precip_file Character. The name of the precipitation input file (PRECIP in the input file). Default is "precip.txt".
#' @param poly_landcover_file Character. The name of the polygon landcover IDs file (POLY_LANDCOVER in the input file). Default is "polygon_landcover_ids.txt".
#' @param et_file Character. The name of the reference evapotranspiration file (ET in the input file). Default is "ref_et.txt".
#' @param et_ext_depth_file Character. The name of the ET cells extinction depth file (ET_EXT_DEPTH in the input file). Default is "ET_Cells_Extinction_Depth.txt".
#' @param ets_template_file Character. The name of the ETS template file (ETS_TEMPLATE in the input file). Default is "SVIHM_ETS_template.txt".
#' @param kc_frac_file Character. The name of the crop coefficient fractions file (KC_FRAC in the input file). Default is "kc_values.txt".
#' @param sfr_network_file Character. The name of the streamflow routing network file (SFR_NETWORK in the input file). Default is "SFR_network.txt".
#' @param sfr_partition_file Character. The name of the streamflow routing partition file (SFR_PARTITION in the input file). Default is "SFR_subws_flow_partitioning.txt".
#' @param wel_template_file Character. The name of the well template file (WEL_TEMPLATE in the input file). Default is "SVIHM_WEL_template.txt".
#' @param recharge_zones_file Character. The name of the recharge zones file (RECHARGE_ZONES in the input file). Default is "recharge_zones.txt".
#' @param irr_ditch_file Character. The name of the irrigation ditch file (IRR_DITCH in the input file). Default is "irr_ditch.txt".
#' @param et_zone_cells_file Character. The name of the ET zone cells file (ET_ZONE_CELLS in the input file). Default is "ET_Zone_Cells.txt".
#' @param mar_depth_file Character. The name of the managed aquifer recharge depth file (MAR_DEPTH in the input file). Default is "MAR_depth.txt".
#' @param curtail_frac_file Character. The name of the curtailment fractions file (CURTAIL_FRAC in the input file). Default is "curtailment_fractions.txt".
#' @param water_mover_file Character. The name of the water mover file (WATER_MOVER in the input file). Default is NULL.
#' @param print_daily_fields List of named lists. Each element should contain `id` (Integer, Field ID) and `prefix` (Character, filename prefix) for fields to print daily in the PRINT_DAILY block.
#' @param verbose Logical. Whether to print status information to the console. Default is TRUE.
#'
#' @return None. The function writes the input file to the specified directory.
#' @export
#'
#' @examples
#' # Set Directory (Created in SVIHM_Input_Files/Updates) - Grabs latest version
#' update_dir <- latest_dir(data_dir['update_dir','loc'])
#'
#' # Set output fields
#' print_daily_fields <- list(
#'   list(id = 1, prefix = "Field_1"),
#'   list(id = 91, prefix = "Field_91"),
#'   list(id = 136, prefix = "AG_MIX_Flood")
#' # Add more fields as needed
#'
#' # Write file
#' write_SWBM_main_input_file(output_dir = update_dir, num_stress_periods = 395)
write_SWBM_main_input_file <- function(output_dir,
                                       num_stress_periods,
                                       filename="svihm.swbm",
                                       modelName = "SVIHM",
                                       WYstart = 1991,
                                       npoly = 2119,
                                       nlandcover = 6,
                                       nAgWells = 167,
                                       nSpecWells = 0,
                                       nSubws = 8,
                                       inflow_is_vol = FALSE,
                                       daily_sw = TRUE,
                                       nSFR_inflow_segs = 12,
                                       nrows = 440,
                                       ncols = 210,
                                       neighborRuleYearDay = 250,
                                       absoluteIrrDate = c(5,15),
                                       writeUCODE=TRUE,
                                       writePEST=FALSE,
                                       precip_file = 'precip.txt',
                                       poly_landcover_file = 'polygon_landcover_ids.txt',
                                       poly_agwell_file = 'ag_well_list_by_polygon.txt',
                                       et_file = 'ref_et.txt',
                                       et_ext_depth_file = 'ET_Cells_Extinction_Depth.txt',
                                       ets_template_file = 'SVIHM_ETS_template.txt',
                                       kc_frac_file = 'kc_values.txt',
                                       sfr_network_file = 'SFR_network.txt',
                                       sfr_partition_file = 'SFR_subws_flow_partitioning.txt',
                                       wel_template_file = 'SVIHM_WEL_template.txt',
                                       agwell_locs_file = 'ag_well_summary.txt',
                                       recharge_zones_file = 'recharge_zones.txt',
                                       sfr_jtf_file = 'SFR_network_jtf.txt',
                                       irr_ditch_file = 'irr_ditch.txt',
                                       et_zone_cells_file = 'ET_Zone_Cells.txt',
                                       mar_depth_file = 'MAR_depth.txt',
                                       curtail_frac_file = 'curtailment_fractions.txt',
                                       water_mover_file = NULL,
                                       specwell_locs_file = NULL,
                                       specwell_vol_file = NULL,
                                       print_daily_fields  = list(),
                                       verbose=TRUE) {

  if (verbose) {message(paste('Writing SWBM file: ', filename))}

  # Open the connection to the file
  file_conn <- file(file.path(output_dir, filename), "w")

  # Write the header
  writeLines("#===============================================================#", file_conn)
  writeLines("#                Soil Water Budget Model (SWBM)                 #", file_conn)
  writeLines("#===============================================================#", file_conn)

  # DISCRETIZATION Block
  writeLines("\nBEGIN DISCRETIZATION", file_conn)
  writeLines(sprintf("  NMONTHS          %d", num_stress_periods), file_conn)
  writeLines(sprintf("  WYSTART         %d", WYstart), file_conn)
  writeLines(sprintf("  NPOLY           %d", npoly), file_conn)
  writeLines(sprintf("  NSUBWS             %d", nSubws), file_conn)
  writeLines(sprintf("  NLANDCOVER         %d", nlandcover), file_conn)
  writeLines(sprintf("  NAGWELLS         %d", nAgWells), file_conn)
  writeLines(sprintf("  NSPECWELLS         %d", nSpecWells), file_conn)
  writeLines("  # MODFLOW INFO", file_conn)
  writeLines(sprintf("  MFNAME         %s", modelName), file_conn)
  writeLines(sprintf("  NROWS            %d", nrows), file_conn)
  writeLines(sprintf("  NCOLS            %d", ncols), file_conn)
  writeLines(sprintf("  NSFR_INFLOW_SEGS  %d", nSFR_inflow_segs), file_conn)
  writeLines("END DISCRETIZATION", file_conn)

  # OPTIONS Block
  writeLines("\nBEGIN OPTIONS", file_conn)
  writeLines("  DAILY_SW", file_conn)
  writeLines(sprintf("  NEIGHBOR_RULE      %d", neighborRuleYearDay), file_conn)
  writeLines(sprintf("  ABSOLUTE_IRR_DATE %d %d", absoluteIrrDate[1], absoluteIrrDate[2]), file_conn)
  writeLines("  WRITE_MODFLOW", file_conn)
  if (writeUCODE) {writeLines("  WRITE_UCODE", file_conn)}
  if (writePEST) {writeLines("  WRITE_PEST", file_conn)}
  writeLines("END OPTIONS", file_conn)

  # PARAMETERS Block
  #writeLines("\nBEGIN PARAMETERS", file_conn)
  #writeLines(sprintf("  RD_MULT    %.1f", RD_Mult), file_conn)
  #writeLines("END PARAMETERS", file_conn)

  # INPUT_FILES Block
  writeLines("\nBEGIN INPUT_FILES", file_conn)
  # Required files
  writeLines(sprintf("  PRECIP            %s", precip_file), file_conn)
  writeLines(sprintf("  ET                %s", et_file), file_conn)
  writeLines(sprintf("  ET_EXT_DEPTH      %s", et_ext_depth_file), file_conn)
  writeLines(sprintf("  ETS_TEMPLATE      %s", ets_template_file), file_conn)
  writeLines(sprintf("  KC_FRAC           %s", kc_frac_file), file_conn)
  writeLines(sprintf("  SFR_NETWORK       %s", sfr_network_file), file_conn)
  writeLines(sprintf("  SFR_PARTITION     %s", sfr_partition_file), file_conn)
  writeLines(sprintf("  WEL_TEMPLATE      %s", wel_template_file), file_conn)
  writeLines(sprintf("  RECHARGE_ZONES    %s", recharge_zones_file), file_conn)
  writeLines(sprintf("  POLY_LANDCOVER    %s", poly_landcover_file), file_conn)
  writeLines(sprintf("  POLY_AGWELL       %s", poly_agwell_file), file_conn)
  writeLines(sprintf("  AGWELL_LOCS       %s", agwell_locs_file), file_conn)
  # Optional Files
  if (!is.null(et_zone_cells_file)) { writeLines(sprintf("  ET_ZONE_CELLS     %s", et_zone_cells_file), file_conn)}
  if (!is.null(sfr_jtf_file))       { writeLines(sprintf("  SFR_NETWORK_JTF   %s", sfr_jtf_file), file_conn)}
  if (!is.null(irr_ditch_file))     { writeLines(sprintf("  IRR_DITCH         %s", irr_ditch_file), file_conn)}
  if (!is.null(mar_depth_file))     { writeLines(sprintf("  MAR_DEPTH         %s", mar_depth_file), file_conn)}
  if (!is.null(curtail_frac_file))  { writeLines(sprintf("  CURTAIL_FRAC      %s", curtail_frac_file), file_conn)}
  if (!is.null(water_mover_file))   { writeLines(sprintf("  WATER_MOVER       %s", water_mover_file), file_conn)}
  if (nSpecWells>0)                 { writeLines(sprintf("  SPECWELL_LOCS     %s", specwell_locs_file), file_conn)}
  if (nSpecWells>0)                 { writeLines(sprintf("  SPECWELL_VOL      %s", specwell_vol_file), file_conn)}
  writeLines("END INPUT_FILES", file_conn)

  # [Optional] PRINT_DAILY Block
  if (length(print_daily_fields) > 0) {
    writeLines("\nBEGIN PRINT_DAILY", file_conn)
    writeLines("# Field ID, filename_prefix", file_conn)
    for (field in print_daily_fields) {
      writeLines(sprintf("  %4d  %s", field$id, field$prefix), file_conn)
    }
    writeLines("END PRINT_DAILY", file_conn)
  }

  # Close the connection to the file
  close(file_conn)
}

#-------------------------------------------------------------------------------------------------#

#' Write SWBM Instream Flow Available Ratio File
#'
#' @param avail_monthly Dataframe of ratio of instream flow available over the model period, as
#' generated by \code{\link{build_cdfw_instream_flow_cal}}
#' @param output_dir directory to write the files to
#' @param filename input filename, instream_flow_available_ratio.txt by default
#' @param verbose T/F write status info to console (default: TRUE)
#'
#' @return
#' @export
#'
#' @examples
write_SWBM_instream_available_file <- function(avail_monthly, output_dir,
                                               filename="instream_flow_available_ratio.txt",
                                               verbose=TRUE) {

  if (verbose) {message(paste('Writing SWBM file: ', filename))}
  write.table(avail_monthly, file = file.path(output_dir, filename),
              sep = " ", quote = FALSE, col.names = F, row.names = FALSE)
}

#-------------------------------------------------------------------------------------------------#

#' Write SWBM Crop Coefficient (Kc) File
#'
#' @param kc_df Dataframe of days and coefficients (see
#'   \code{\link{gen_daily_binary_crop_coefficients}} and
#'   \code{\link{gen_daily_curve_crop_coefficients}})
#' @param output_dir directory to write the files to
#' @param filename input filename, instream_flow_available_ratio.txt by default
#' @param verbose T/F write status info to console (default: TRUE)
#'
#' @return
#' @export
#'
#' @examples
write_SWBM_crop_coefficient_file <- function(kc_df, output_dir, filename, verbose=TRUE) {
  if (verbose) {message(paste('Writing SWBM Crop Coefficient file: ', filename))}

  kc_df$day <- format(kc_df$Date, '%d/%m/%Y')
  kc_df$kc = round(kc_df$kc, 4)

  write.table(kc_df[,c('kc','day')],
              file = file.path(output_dir, filename),
              sep = " ", quote = FALSE, col.names = FALSE, row.names = FALSE)
}

#-------------------------------------------------------------------------------------------------#

#' Write SWBM File Partitioning Subwatershed Surface Flows to Stream Inflows
#'
#' @param sfr_component Dataframe of months and either: 1) subwatershed inflow partition
#' fractions (see \code{\link{gen_monthly_sfr_flow_partition}}), or 2) inflows available
#' for irrigation, or 3) inflows NOT available for irrigation (e.g. reserved for environmental
#' flows) (for items 2 and 3 see \code{\link{process_monthly_sfr_inflows}}).
#' @param output_dir directory to write the files to
#' @param filename Writes to this filename
#' @param verbose T/F write status info to console (default: TRUE)
#'
#' @return
#' @export
#'
#' @examples
#'
write_SWBM_SFR_inflow_files <- function(sfr_component, output_dir, filename, verbose=TRUE) {
  if (verbose) {message(paste('Writing SWBM SFR Handling file: ', filename))}

  # see if daily
  daily = FALSE
  if (mean(diff.Date(sfr_component[,1]))) { daily = TRUE }

  # if(filename=="SFR_subws_flow_partitioning.txt"){
  if (daily) {
    # Really no reformat necessary
    #sfr_component[1] <- as.character(format(x = sfr_component[1], format= '%d-%b-%Y'))
  } else {
    sfr_component[1] <- as.character(format(x = sfr_component[1], format= '%b-%Y'))
  }

  write.table(sfr_component,
              file = file.path(output_dir, filename),
              sep = " ", quote = FALSE, col.names = TRUE, row.names = FALSE)

  # }


}

#-------------------------------------------------------------------------------------------------#

#' Write SFR Diversions File
#'
#' Generates text file specifying the number, stream segment, priority handling (in MODFLOW), and
#' unchanging flowrate of each diversion point in the stream network.
#' Default is currently (Nov 2022) configured to specify diversion points for the SVID ditch and
#' Farmers ditch, with 0 flow in them (as ditch diversions are currently  handled in a
#' separate module).
#'
#' @param num_divs Number of diversions in the stream network
#' @param iseg_for_divs The stream segment associated with each diversion
#' @param iprior_for_divs Specifies priority handling for diversion in MODFLOW.
#' @param flow_for_divs Uniform flowrate for the diversion.
#'
#' @return
#' @author Claire Kouba, Leland Scantlebury
#' @export
#'
#' @examples

write_SWBM_SFR_diversions_file <- function(filename = "SFR_diversions.txt",
                                           output_dir,
                                           num_divs = 2,
                                           iseg_for_divs = c(3,10), # Stream segment of diversion
                                           iprior_for_divs = c(1,1), # Modflow param. for calc.
                                           flow_for_divs = c(19600, 39100),    # Flowrate (units m3/day)
                                           verbose = T) {

  sfr_divs = c(
    paste(num_divs, "    ! Number of diversions", sep = "  "),
    paste(paste(iseg_for_divs, collapse = "  "), "    ! ISEG for diversions",
          sep = "  "),
    paste(paste(iprior_for_divs, collapse = "  "), "    ! IPRIOR for diversions",
          sep = "  "),
    paste(paste(flow_for_divs, collapse = "  "), "    ! FLOW for diversions",
          sep = "  "))

  if (verbose) {message(paste('Writing SWBM file: ', filename))}
  write.table(sfr_divs, file = file.path(output_dir, filename),
              sep = " ", quote = FALSE, col.names = FALSE, row.names = FALSE)

}


#' Build Field-value Data Frame
#'
#' Empty dataframe of field-level values, for every month between the start and end dates. This
#' format is common to many temporal SWBM inputs
#'
#' @param nfields number of fields in the Soil Water Balance Model simulation
#' @param model_start_date Start date of simulation
#' @param model_end_date End date of simulation
#' @param default_values Default value(s), given by field (optional, defaults to NA)
#'
#' @return Dataframe of stress periods (rows) for each field (columns)
#' @export
#'
#' @examples
#' # Dates
#' start_date <- get_model_start(1991)
#' end_date <- as.Date(floor_date(Sys.Date(), 'month')-1)
#' # Fields
#' nfields <- 100
#' # For example: land use
#' default_lu <- rep(swbm_lutype['Alfalfa','Code'], nfields)
#'
#' lu_df <- swbm_build_field_value_df(nfields, start_date, end_date, default_lu)
#'
swbm_build_field_value_df <- function(nfields=2119, model_start_date, model_end_date, default_values=NA) {

  # Define Stress Periods & IDs
  vect_sp <- seq.Date(model_start_date, model_end_date, by='month')
  id_list <- paste0("ID_", 1:nfields)

  # Error check
  if (!is.na(default_values) & length(default_values) > 1 & (!length(default_values) == length(id_list))) {
    stop('Default value list must be single value or same length as # of fields in poly_table')
  }

  # Create dataframe of irrigation types for each field (columns) for each stress period (rows)
  df <- data.frame(matrix(default_values,
                          nrow=length(vect_sp),
                          ncol=length(id_list),
                          byrow = T))
  colnames(df) <- id_list # assign column names
  df$Stress_Period <- vect_sp

  #-- Stress period column first
  df <- df[c('Stress_Period', names(df)[names(df) != 'Stress_Period'])]

  return(df)
}

# ------------------------------------------------------------------------------------------------#

#' Update Land Use Table With New Data
#'
#' @param lu_df Dataframe of field land uses for each stress period, possibly created by
#'              [swbm_build_field_value_df()]
#' @param update_table Table of land use updates, where each column has a name of the form
#'                     'landuse_XX' where XX is year 20XX, each row is a field, in order, and the
#'                     cells contain valid land use codes (see [swbm_lutype])
#' @param verbose T/F write # of yearly changed to console (default: TRUE)
#'
#' @return Updated lu_df
#' @export
#'
#' @examples
#' # Dates
#' start_date <- get_model_start(1991)
#' end_date <- as.Date(floor_date(Sys.Date(), 'month')-1)
#' # Fields
#' nfields <- 100
#' # For example: land use
#' default_lu <- rep(swbm_lutype['Alfalfa','Code'], nfields)
#'
#' lu_df <- swbm_build_field_value_df(nfields, start_date, end_date, default_lu)
#'
#' # Make update table
#' updates <- data.frame('ID'=1:nfields, 'landuse_11'=swbm_lutype['Pasture','Code'])
#'
#' # Do updates
#' lu_df <- swbm_landuse_update(lu_df, updates)
#'
swbm_landuse_update <- function(lu_df, update_table, verbose=TRUE) {

  # update_table is assumed to have columns of 'landuse_XX' where XX is year 20XX
  for (col in names(update_table)[2:length(names(update_table))]) {
    col_yr <- 2000 + as.numeric(strsplit(col, '_')[[1]][2])
    sps_after_change <- nrow(lu_df[year(lu_df$Stress_Period) >= col_yr,])

    # Get changes
    change_df <- update_table[update_table[,col]>0,c('ID',col)]

    # Create change matrix
    diff <- matrix(change_df[,col],
                   nrow=sps_after_change,
                   ncol=length(change_df[,col]),
                   byrow = T)

    # Report
    if (verbose) {
      message(paste('Column:', col, ' Year:', col_yr, '- ', length(change_df[,col]), 'Fields Updated'))
    }

    # Update
    lu_df[year(lu_df$Stress_Period) >= col_yr, names(lu_df) %in% paste0('ID_',change_df$ID)] <- diff
  }
  return(lu_df)
}

# ------------------------------------------------------------------------------------------------#

#' Update Irrigation Type Table with New Data
#'
#' @param irrtype_df Dataframe of field irrigation types for each stress period, possibly created by
#'                   [swbm_build_field_value_df()]
#' @param update_table Dataframe with columns 'ID' of field IDs and 'Year' of the year (integer XXXX)
#'                     when the field switched to center pivot. Zeros can be used to denote that it
#'                     has never switched.
#' @param verbose T/F write # of yearly changed to console (default: TRUE)
#'
#' @return Updated irrtype_df
#' @export
#'
#' @examples
#' # Dates
#' start_date <- get_model_start(1991)
#' end_date <- as.Date(floor_date(Sys.Date(), 'month')-1)
#' # Fields
#' nfields <- 100
#' # For example: land use
#' default_irr <- rep(swbm_irrtype['Wheel Line','Code'], nfields)
#'
#' lu_df <- swbm_build_field_value_df(nfields, start_date, end_date, default_irr)
#'
#' # Make update table - magically all fields updated to center pivot in 2005!
#' updates <- data.frame('ID'=1:nfields, 'Year'=2005)
#'
#' # Do updates
#' lu_df <- swbm_irrtype_cp_update(lu_df, updates)
#'
swbm_irrtype_cp_update <- function(irrtype_df, update_table, verbose=TRUE) {

  # Switch is to center pivot
  new_code <- swbm_irrtype['Center Pivot', 'Code']

  #-- Loop over years where changes occur
  for (yr in unique(update_table[update_table$Year>0,]$Year)) {  # when year==0 no data/change
    id_cols <- update_table$ID[update_table$Year == yr]

    if (verbose) {
      message(paste('Year:', yr, '- ', length(id_cols), 'fields switched to center pivot (code =',new_code,')'))
    }

    irrtype_df[year(irrtype_df$Stress_Period) >= yr, paste0('ID_', id_cols)] <- new_code
  }

  return(irrtype_df)
}


# ------------------------------------------------------------------------------------------------#

#' Write file specifying pumping volumes in agricultural wells
#'
#' @param scenario_name Name of  management scenario. Default is historical basecase or "basecase".
#'
#' @return none; writes input file
#' @export
#'
#' @examples
#' # Dates
#' start_date <- get_model_start(1991)
#' end_date <- as.Date(floor_date(Sys.Date(), 'month')-1)
#' # Fields
#' nfields <- 100
#' # For example: land use
#' default_irr <- rep(swbm_irrtype['Wheel Line','Code'], nfields)
#'
#' lu_df <- swbm_build_field_value_df(nfields, start_date, end_date, default_irr)
#'
#' # Make update table - magically all fields updated to center pivot in 2005!
#' updates <- data.frame('ID'=1:nfields, 'Year'=2005)
#'
#' # Do updates
#' lu_df <- swbm_irrtype_cp_update(lu_df, updates)
#'
write_ag_pumping_file <- function(start_date, n_stress, output_dir,
                                  ag_pumping_data = NA,
                                  filename = "ag_well_specified_volume.txt") {

  stress_period_vector = format(seq.Date(from=start_date, length.out=n_stress, by="month"),
                                format = "%b%Y")

  # get vector of well IDs
  ag_wells = read.table(file.path(data_dir['time_indep_dir','loc'],"ag_well_summary.txt"),
                        header = T)
  well_ids = ag_wells$well_id


  if(is.na(ag_pumping_data)){
    #If no ag pumping data provided, set all stress periods to FALSE (no specified pumping volumes)
    # and pump volumes to 0
    specify_pumping = rep(FALSE, length(stress_period_vector))
    pumping_volumes = data.frame(matrix(data = 0, nrow = n_stress, ncol = length(well_ids)))

    ag_pumping_tab = cbind(stress_period_vector, specify_pumping, pumping_volumes)
    colnames(ag_pumping_tab) = c("well_id", "specify_pumping", well_ids)
  } else {
    # placeholder - if we receive specified ag pumping data (or want to specifically dictate it for
    # a scenario), file design can go here.
  }

  write.table(ag_pumping_tab, file = file.path(output_dir, filename), sep = "  ", quote = F,
              col.names = TRUE, row.names = FALSE)
}


# ------------------------------------------------------------------------------------------------#

#' Write file specifying pumping volumes in municipal wells
#'
#' @param scenario_name Name of  management scenario. Default is historical basecase or "basecase".
#'
#' @return none; writes input file
#' @export
#'
#' @examples
#' # Dates
#' start_date <- get_model_start(1991)
#' end_date <- as.Date(floor_date(Sys.Date(), 'month')-1)
#' # Fields
#' nfields <- 100
#' # For example: land use
#' default_irr <- rep(swbm_irrtype['Wheel Line','Code'], nfields)
#'
#' lu_df <- swbm_build_field_value_df(nfields, start_date, end_date, default_irr)
#'
#' # Make update table - magically all fields updated to center pivot in 2005!
#' updates <- data.frame('ID'=1:nfields, 'Year'=2005)
#'
#' # Do updates
#' lu_df <- swbm_irrtype_cp_update(lu_df, updates)
#'
write_muni_pumping_file <- function(start_date, n_stress, output_dir,
                                  muni_pumping_data = NA,
                                  filename = "muni_well_specified_volume.txt") {

  stress_period_vector = format(seq.Date(from=start_date, length.out=n_stress, by="month"),
                                format = "%b%Y")

  # get vector of well IDs
  muni_wells = read.table(file.path(data_dir['time_indep_dir','loc'],"muni_well_summary.txt"),
                        header = T)
  well_ids = muni_wells$well_id


  if(is.na(muni_pumping_data)){
    #If no muni pumping data provided, set all stress periods to FALSE (no specified pumping volumes)
    # and pump volumes to 0
    specify_pumping = rep(FALSE, length(stress_period_vector))
    pumping_volumes = data.frame(matrix(data = 0, nrow = n_stress, ncol = length(well_ids)))

    muni_pumping_tab = cbind(stress_period_vector, specify_pumping, pumping_volumes)
    colnames(muni_pumping_tab) = c("well_id", "specify_pumping", well_ids)
  } else {
    # placeholder - if we receive specified ag pumping data (or want to specifically dictate it for
    # a scenario), file design can go here.
  }

  write.table(muni_pumping_tab, file = file.path(output_dir, filename), sep = "  ", quote = F,
              col.names = TRUE, row.names = FALSE)
}



# ------------------------------------------------------------------------------------------------#

#' Write file specifying land use types for each field, for each stress period
#'
#' @param scenario_name Name of  management scenario. Default is historical basecase or "basecase".
#'
#' @return landcover_tab
#' @export
#'
#' @examples
#'
#'
#'
write_SWBM_landcover_file <- function(scenario_id = "basecase",
                                      output_dir, start_date, end_date) {
  recognized_basecase_landuse_scenarios = c("basecase", "curtail_00_pct_all_years",
                                            "curtail_30_pct_2022","curtail_50_pct_2022",
                                            "curtail_10_pct_2022")
  recognized_scenarios = c(recognized_basecase_landuse_scenarios,
                           "natveg_all") # placeholder for more landuse scenarios
  output_filename = "polygon_landcover_ids.txt"

  # pull reference land cover
  poly_tab = read.table(file.path(data_dir["time_indep_dir","loc"],"polygons_table.txt"),
                      header = T)
  num_unique_fields = length(unique(poly_tab$SWBM_id))
  # Check for polygon ID number continuity
  if(nrow(poly_tab) != num_unique_fields){
    print("Caution: polygons (fields) info table contains replicated or missing ID numbers")
  }
  # generate a stress-period-by-field table (wide format)
  field_df = swbm_build_field_value_df(nfields = num_unique_fields,
                                       model_start_date = start_date,
                                       model_end_date = end_date)

  # Build default (time-invarying) land use table
  for(i in 1:num_unique_fields){
    field_id = poly_tab$SWBM_id[i]
    # set entire model period to the baseline land use from reference poly_tab file
    field_df[,paste0("ID_",field_id)] = poly_tab$SWBM_LU[poly_tab$SWBM_id==field_id]
  }

  # Write land cover file for scenarios with basecase land use
  # i.e., no major crop changes or native vegetation coverage changes
  if(tolower(scenario_id) %in% recognized_basecase_landuse_scenarios | # If it's a basecase-landuse scenario OR
     !(tolower(scenario_id) %in% recognized_scenarios)){ # If it's not any of the recognized scenarios
    #Set fields equal to unchanging default
    landcover_output = field_df
    # Then, Implement alfalfa-grain rotation
    n_stress = length(seq.Date(from = start_date, to = end_date, by = "month"))
    # Initialize a grain_months dataframe for allocating alfalfa-grain attributes in the for loop
    grain_months = data.frame(month = field_df$Stress_Period)
    grain_months$water_year = year(grain_months$month)
    next_wy_indices = month(grain_months$month)>9
    grain_months$water_year[next_wy_indices] = 1 +
      grain_months$water_year[next_wy_indices]
    grain_months$grain_year = grain_months$water_year - year(start_date)
    grain_months$alf_1_grain_2 = 1

    # n_rotating = sum(poly_tab$SWBM_LU==1) # how many fields are using alfalfa-grain rotation? Assume all alfalfa
    # n_grain = n_rotating / 8 # 1 out of every 8 fields is grain at any given time. 8-yr schedule
    field_rotator = 1 # initialize the eenie-meenie-miney-mo counter
    for(i in 1:num_unique_fields){
      if(poly_tab$SWBM_LU[poly_tab$SWBM_id==i]==1){ # If it's designated as alfalfa-type landuse
        first_grain_year = field_rotator %% 8 + 1 # designate first year of grain rotation
        # print(first_grain_year)
        grain_years = seq(from = first_grain_year, by = 8, to = n_stress)
        grain_sp_indices = which(grain_months$grain_year %in% grain_years)
        # print(grain_sp_indices)
        grain_months$alf_1_grain_2[grain_sp_indices] = 2 # Assign grain to those stress periods

        # Assign the alfalfa-grain vector to the output file
        landcover_output[,paste0("ID_",i)] = grain_months$alf_1_grain_2
        field_rotator = field_rotator + 1 # increment the counter

        # reset alfalfa vector
        grain_months$alf_1_grain_2 = 1

      }
    }
  }

  # if(tolower(scenario_id=="natveg")){} # Placeholder :
  # Replace default with new landcover file, and update recognized_scenarios
  # possible steps: read in fields and potentially adjudicated zone
  # potentially read in schedule of land use updates

  if(!(tolower(scenario_id) %in% recognized_scenarios)){
    print("Warning: specified landuse scenario not recognized in current codebase. Using basecase land cover file")
    landcover_output = field_df
  }

  write.table(landcover_output, file = file.path(output_dir, output_filename),
              sep = "  ", quote = FALSE, col.names = TRUE, row.names = FALSE)

}


# ------------------------------------------------------------------------------------------------#

#' Write file specifying Managed Aquifer Recharge (applied irrigation) volumes for each field, for each stress period
#'
#' @param scenario_name Name of  management scenario. Default is historical basecase or "basecase".
#'
#' @return none, saves MAR_volumes.txt
#' @import sf
#' @export
#'
#' @examples
#'
#'
#'
write_SWBM_MAR_depth_file <- function(scenario_id = "basecase",
                                      output_dir, start_date, end_date) {
  recognized_scenarios = c("basecase", "basecase_noMAR", "maxMAR2024")
  output_filename = "MAR_depth.txt"

  # pull reference land cover
  poly_tab = read.table(file.path(data_dir["time_indep_dir","loc"],"polygons_table.txt"),
                        header = T)
  num_unique_fields = length(unique(poly_tab$SWBM_id))
  # Check for polygon ID number continuity
  if(nrow(poly_tab) != num_unique_fields){
    print("Caution: polygons (fields) info table contains replicated or missing ID numbers")
  }

  # generate a stress-period-by-field table (wide format)
  field_df = swbm_build_field_value_df(nfields = num_unique_fields,
                                       model_start_date = start_date,
                                       model_end_date = end_date)

  # Build default MAR volumes table (no MAR applications)
  field_column_selector = grepl(pattern = "ID", x = colnames(field_df))
  field_df[,field_column_selector] = 0

  if(tolower(scenario_id) == "basecase_noMAR"){
    mar_depth_output = field_df
  }

  if(tolower(scenario_id) %in% tolower(c("basecase", "maxMAR2024"))){
    # Initialize output file
    mar_depth_output = field_df
    # 1) TO DO: add in observed MAR for winter 2023

    # MAR 2024 ----------------------------------------------------------------

    # 1) Read in data

    # Read in fields spatial file for spatial relation
    svihm_fields = st_read(dsn = file.path(data_dir["ref_data_dir","loc"]),
                           layer = "Landuse_20190219_updated2023")
    svihm_fields$MAR_distrib_mult = 0     # column for MAR distribution multiplier
    svihm_fields$area_m2 = sf::st_area(svihm_fields)
    # read in spatial file of recharge areas
    mar_fields24 = st_read(dsn = file.path(data_dir["ref_data_dir","loc"]),
                           layer = "MAR_Fields_2024")
    mar_fields24=st_transform(mar_fields24, crs=sf::st_crs(svihm_fields))
    # calculate and name field areas in m2
    mar_fields24_areas = st_area(mar_fields24)
    names(mar_fields24_areas) = substr(x = mar_fields24$Name, start = 7, stop = 7)
    # read in and process MAR diversion volumes
    if(tolower(scenario_id)=="basecase"){
      mar_app = read.csv(file=file.path(data_dir["ref_data_dir","loc"],
                                        "Scott MAR wy 2024_cfs daily record.csv"),
                         colClasses = c("character",rep("numeric",10)))
    }
    if(tolower(scenario_id)==tolower("maxMAR2024")){
      mar_app = read.csv(file=file.path(data_dir["ref_data_dir","loc"],
                                        "Scott MAR wy 2024_cfs daily record_Max Div.csv"),
                         colClasses = c("character",rep("numeric",10)))
    }

    mar_app$Date= as.Date(mar_app$Date, format = "%m/%d/%Y") # convert to dates
    # convert NAs to 0s and rename columns to field letter ID
    cfs_data = 2:ncol(mar_app) #identify columns with cfs data
    mar_app[,cfs_data][is.na(mar_app[,cfs_data])]= 0
    colnames(mar_app)[colnames(mar_app)!="Date"] =
      substr(colnames(mar_app)[colnames(mar_app)!="Date"], start=1, stop=1)
    # convert from daily avg cfs to cubic meters per day
    mar_app_m3day = mar_app
    cfs_to_m3day = 1 * 60*60*24 / 35.3147
    mar_app_m3day[,cfs_data] = round(mar_app[,cfs_data] * cfs_to_m3day)
    # aggregate to monthly for MAR input file
    mar_app_m3month = aggregate(x = mar_app_m3day[,cfs_data],
                                by = list(Date = floor_date(mar_app_m3day$Date, unit="month")),
                                FUN = sum)
    # mar_depth_monthly = mar_app_m3month
    # mar_depth_monthly[,cfs_data] = mapply(`/`,mar_app_m3month[,cfs_data], mar_fields24_areas)

    # make table of stress periods for assigning water volumes to MAR output tab
    months = seq.Date(from = as.Date("1990-10-01"),
                      to = floor_date(Sys.Date(), unit="month"),
                      by="month")
    sp_tab = data.frame(Month = months, stress_period = 1:length(months))
    # Assign stress periods to table of MAR diversions
    mar_app_m3month$stress_period =
      sp_tab$stress_period[match(mar_app_m3month$Date,sp_tab$Month)]

    # 2) Relate MAR fields to SVIHM fields

    # Spatial relations to distribute water diverted for recharge
    # monthly applied volume by field (rchA_i)
    # for each MAR field i, need a column for:
    # recharge area for each field (rchA_i)

    # calculate area and total recharge area
    mar_fields24$area_m2 = sf::st_area(mar_fields24)
    rchA_all = sum(mar_fields24$area_m2)# total area of recharge fields
    # total recharge volume applied to each field i (rchMth_i)
    # 1) calculate:
    # rchMth_i = time series for each field
    # Then, for each intercepted SVIHM field j, need a column for:
    # SVIHM field area (sfA_j)
    # intercept area (intA_j)
    # Total monthly volume applied to each intercepted field (rchMth_j)
    # 2) calculate:
    # rchMth_j = rchMth_i * (intA_j / rchA_i)
    # 3) Convert to MAR depth for full intercepted field
    # rchMth_j_depth = rchMth_j / sfA_j
    # 4)  Populate output table with rchMth_j_depth for each field by month

    for(i in 1:nrow(mar_fields24)){
      mar_field = mar_fields24[i,]
      svihm_intercepts = svihm_fields[mar_field,]
      rchA_i = mar_field$area_m2

      # wigglies = c(714, 1015) # keep out some long snaking ones
      # svihm_intercepts=svihm_intercepts[svihm_intercepts$Poly_nmbr %in% wigglies,]
      # plot(svihm_intercepts$geometry, col = "lightblue")
      # plot(mar_field$geometry, add=T, col=rgb(1,0,0,.5))
      for (j in 1:length(svihm_intercepts)){
        sv_field = svihm_intercepts[j,]
        sfA_j = sv_field$area_m2
        # plot(mar_field)
        for(k in 1:nrow(mar_app_m3month)){
          nchar_name = nchar(mar_field$Name)
          field_picker = substr(mar_field$Name, stop = nchar_name, start = nchar_name) == colnames(mar_app_m3month)
          rchMth_i_m3 = mar_app_m3month[k, field_picker]

          if(rchMth_i_m3>0){ # don't run spatial calcs if value of mar is 0
            field_intersec = sf::st_intersection(x=mar_field, y = sv_field)
            intA_j = sf::st_area(field_intersec)

            rchMth_j_m3 = rchMth_i_m3 * (intA_j / rchA_i)
            rchMth_j_depth = rchMth_j_m3 / sfA_j
            # assign output
            row_picker = mar_depth_output$Stress_Period==mar_app_m3month$Date[k]
            out_fields = gsub(x = colnames(mar_depth_output[,-1]),
                              pattern = "ID_", replacement = "")
            col_picker = which(sv_field$Poly_nmbr == out_fields) + 1
            mar_depth_output[row_picker,col_picker] = round(rchMth_j_depth,3)

          }
        }
      }

    }

  }
  # if(tolower(scenario_id=="natveg")){} # Placeholder :
  # Replace default with new MAR vol file, and update recognized_scenarios
  # possible steps: read in fields and potentially adjudicated zone
  # potentially read in schedule of land use updates

  if(!(tolower(scenario_id) %in% tolower(recognized_scenarios))){
    print("Warning: specified MAR scenario not recognized in current codebase. Using basecase MAR file (no irrigation applied for MAR)")
    mar_depth_output = field_df
  }
  write.table(mar_depth_output, file = file.path(output_dir, output_filename),
              sep = "  ", quote = FALSE, col.names = TRUE, row.names = FALSE)

}


# ------------------------------------------------------------------------------------------------#

#' Write file specifying curtailment for each field, for each stress period
#'
#' @param scenario_name Name of  management scenario. Default is historical basecase or "basecase".
#' @import sf
#' @return none, saves MAR_volumes.txt
#' @export
#'
#' @examples
#'
#'
#'
write_SWBM_curtailment_file <- function(output_dir, start_date, end_date) {
  m2_to_acres = 1/4046.856

  # generate a stress-period-by-field table (wide format)  for saving to output
  curtail_output <- swbm_build_field_value_df(model_start_date = start_date,
                                       model_end_date = end_date, default_values = 0.0)

  # Read in 2021/2022 curtailments (see SVIHM/Scripts/Stored_Analyses/2021_2022_LCS_Curtailments.R)
  curt21_22 <- read.csv(file.path(data_dir["ref_data_dir","loc"], 'Curtail_21_22.csv'))
  curt21_22$Stress_Period <- as.Date(curt21_22$Stress_Period)

  # Read in 2024 curtailment
  curt24 <- read.csv(file.path(data_dir["ref_data_dir","loc"], 'Curtail_24.csv'))
  curt24$Stress_Period <- as.Date(curt24$Stress_Period)

  # Remove rows in curtail_output that match the Stress_Periods, then combine
  curtail_output <- curtail_output[!curtail_output$Stress_Period %in% c(curt21_22$Stress_Period, curt24$Stress_Period), ]
  curtail_output <- rbind(curtail_output, curt21_22, curt24)

  # Ensure the result is sorted by Stress_Period (optional but often needed)
  curtail_output <- curtail_output[order(curtail_output$Stress_Period), ]

  output_filename = paste0("curtailment_fractions.txt")
  write.table(curtail_output, file = file.path(output_dir, output_filename),
              sep = "  ", quote = FALSE, col.names = TRUE, row.names = FALSE)
}


# ------------------------------------------------------------------------------------------------#

#' Write file specifying crop coefficients for each crop type, for each daily timestep
#'
#' @param model_start_date Start date of simulation
#' @param model_end_date End date of simulation
#' @param output_dir directory to write the files in
#'
#' @return none; writes file
#' @export
#'
#' @examples
#'
#'
#'
write_daily_crop_coeff_values_file = function(model_start_date, model_end_date, output_dir){

  #TO DO: Read max kc values from the landcover_table. Use to update scenarios

  # Generate new kc records based on model period
  kc_alfalfa <- gen_daily_binary_crop_coefficients(model_start_date, model_end_date)
  kc_pasture <- gen_daily_binary_crop_coefficients(model_start_date, model_end_date)
  kc_grain   <- gen_daily_curve_crop_coefficients(model_start_date, model_end_date)
  kc_natveg <- gen_daily_binary_crop_coefficients(model_start_date, model_end_date,
                                                  kc_growing = 0.6,
                                                  growing_season_start_day = 1, growing_season_start_month = 1,
                                                  growing_season_end_day = 31, growing_season_end_month = 12)
  kc_water <- gen_daily_binary_crop_coefficients(model_start_date, model_end_date,
                                                 kc_growing = 0,
                                                 growing_season_start_day = 1, growing_season_start_month = 1,
                                                 growing_season_end_day = 31, growing_season_end_month = 12)
  kc_urban <- gen_daily_binary_crop_coefficients(model_start_date, model_end_date,
                                                 kc_growing = 0,
                                                 growing_season_start_day = 1, growing_season_start_month = 1,
                                                 growing_season_end_day = 31, growing_season_end_month = 12)
  # Combine daily kc records into one table
  kc_values = data.frame(Date = format(kc_alfalfa$Date, "%Y-%m-%d"),
                         Alfalfa_Kc = kc_alfalfa$kc, Grain_Kc = kc_grain$kc,
                         Pasture_Kc = kc_pasture$kc, Native_Veg_Kc = kc_natveg$kc,
                         Barren_Urban_Kc = kc_urban$kc, Water_Kc = kc_water$kc)
  # Write combined kc values file
  write.table(kc_values, file = file.path(output_dir, "kc_values.txt"),
              sep = "  ", quote = FALSE, col.names = TRUE, row.names = FALSE)
  }

# ------------------------------------------------------------------------------------------------#

#' Write SWBM SFR Network File
#'
#' Writes the file that essentially gets used as the SFR package template in the SWBM. Has support
#' for including daily tabfiles.
#'
#' @param nsteps Total number of MODFLOW time steps (i.e. days in model) to be found in tab files
#' @param output_dir directory to write the files to
#' @param filename output filename (default: sfr_network.txt)
#' @param daily T/F if using daily time steps in SFR (i.e., tabfiles) (default: FALSE)
#' @param ntabfiles if daily, the number of tabfiles to be read by MODFLOW (default: 30)
#' @param comment SFR file comment printed at top of file (default: 'SFR Package written by RSVP')
#'
#' @return none; writes file
#' @export
#'
#' @examples
write_sfr_network_file <- function(nsteps, output_dir, filename='SFR_network.txt', daily=FALSE,
                                   ntabfiles=30,
                                   comment='SFR Package written by RSVP') {
  f <- file.path(output_dir, filename)
  write(paste('#', comment, ifelse(daily, '- tabfile version -','-'), format(now(), '%Y-%m-%d')), f, append=F)
  if (daily) {
    write(paste('TABFILES', ntabfiles, nsteps), f, append=T)
  }

  # From here it's just copy paste
  sfr <- readLines(file.path(data_dir['ref_data_dir','loc'], 'SFR_network_template.txt'))
  write(sfr,f, append=ifelse(daily,T,F))
}
