
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
                                       nMuniWells = 0,
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
                                       et_file = 'ref_et.txt',
                                       et_ext_depth_file = 'ET_Cells_Extinction_Depth.txt',
                                       ets_template_file = 'SVIHM_ETS_template.txt',
                                       kc_frac_file = 'kc_values.txt',
                                       sfr_network_file = 'SFR_network.txt',
                                       sfr_partition_file = 'SFR_subws_flow_partitioning.txt',
                                       wel_template_file = 'SVIHM_WEL_template.txt',
                                       recharge_zones_file = 'recharge_zones.txt',
                                       sfr_jtf_file = 'SFR_network_jtf.txt',
                                       irr_ditch_file = 'irr_ditch.txt',
                                       et_zone_cells_file = 'ET_Zone_Cells.txt',
                                       mar_depth_file = 'MAR_depth.txt',
                                       curtail_frac_file = 'curtailment_fractions.txt',
                                       water_mover_file = NULL,
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
  writeLines(sprintf("  NMUNIWELLS         %d", nMuniWells), file_conn)
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
  # Optional Files
  if (!is.null(et_zone_cells_file)) { writeLines(sprintf("  ET_ZONE_CELLS     %s", et_zone_cells_file), file_conn)}
  if (!is.null(sfr_jtf_file))       { writeLines(sprintf("  SFR_NETWORK_JTF   %s", sfr_jtf_file), file_conn)}
  if (!is.null(irr_ditch_file))     { writeLines(sprintf("  IRR_DITCH         %s", irr_ditch_file), file_conn)}
  if (!is.null(mar_depth_file))     { writeLines(sprintf("  MAR_DEPTH         %s", mar_depth_file), file_conn)}
  if (!is.null(curtail_frac_file))  { writeLines(sprintf("  CURTAIL_FRAC      %s", curtail_frac_file), file_conn)}
  if (!is.null(water_mover_file))   { writeLines(sprintf("  WATER_MOVER       %s", water_mover_file), file_conn)}
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
#' @param default_values Default values, given by field (optional, defaults to NA)
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
  if (!is.na(default_values) & (!length(default_values) == length(id_list))) {
    stop('Default value list must be same length as # of fields in poly_table')
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

  if(tolower(scenario_id) == "basecase"){
    # Initialize output file
    mar_depth_output = field_df
    # 1) TO DO: add in observed MAR for winter 2023

    # MAR 2024 ----------------------------------------------------------------

    # 1) Read in data

    # Read in fields spatial file for spatial relation
    svihm_fields = st_read(dsn = file.path(data_dir["ref_data_dir","loc"]),
                           layer = "Landuse_20190219_updated2023")
    svihm_fields$MAR_distrib_mult = 0     # column for MAR distribution multiplier
    # read in spatial file of recharge areas
    mar_fields24 = st_read(dsn = file.path(data_dir["ref_data_dir","loc"]),
                           layer = "MAR_Fields_2024")
    mar_fields24=st_transform(mar_fields24, crs=sf::st_crs(svihm_fields))
    # read in and process MAR diversion volumes
    mar_app = read.csv(file=file.path(data_dir["ref_data_dir","loc"],
                                      "Scott MAR wy 2024_cfs daily record.csv"),
                       colClasses = c("character",rep("numeric",10)))
                                       # "MAR 2024 season_ditch flows m3day.csv") )
    mar_app$Date= as.Date(mar_app$Date, format = "%m/%d/%Y") # convert to dates
    # convert NAs to 0s and rename columns to field letter ID
    cfs_data = 2:ncol(mar_app) #identify columns with cfs data
    mar_app[,cfs_data][is.na(mar_app[,cfs_data])]= 0
    colnames(mar_app)[colnames(mar_app)!="Date"] = substr(colnames(mar_app)[colnames(mar_app)!="Date"], start=1, stop=1)
    # convert from daily avg cfs to cubic meters per day
    mar_app_m3day = mar_app
    cfs_to_m3day = 1 * 60*60*24 / 35.3147
    mar_app_m3day[,cfs_data] = round(mar_app[,cfs_data] * cfs_to_m3day)
    # aggregate to monthly for MAR input file
    mar_app_m3month = aggregate(x = mar_app_m3day[,cfs_data], by = list(month(mar_app_m3day$Date)),
                                FUN = sum)

    # CURRENTLY HERE 2024.10.21
    # make table of stress periods for assigning water volumes to MAR output tab
    months = seq.Date(from = as.Date("1990-10-01"),
                      to = floor_date(Sys.Date(), unit="month"),
                      by="month")
    sp_tab = data.frame(Month = months, stress_period = 1:length(months))
    # Assign stress periods to table of MAR diversions
    div_monthly$stress_period = sp_tab$stress_period[match(div_monthly$Month,sp_tab$Month)]

    # 2) Relate MAR fields to SVIHM fields

    # Spatial relations to distribute water diverted for recharge
    # monthly diversion volume (divMth)
    # total area of recharge fields (rchA_all)
    # for each MAR field i, need a column for:
    # recharge area for each field (rchA_i)

    # calculate area and total recharge area
    mar_fields24$area_m3 = sf::st_area(mar_fields24)
    rchA_all = sum(mar_fields24$area_m3)# total area of recharge fields
    # total recharge volume applied to each field i (rchMth_i)
    # 1) calculate:
    # rchMth_i = divMnth * (rchA_i / rchA_all)
    # NOTE that rchMth_i will change and eventually be a time series for each field
    # Then, for each intercepted SVIHM field j, need a column for:
    # SVIHM field area (sfA_j)
    # intercept area (intA_j)
    # Total monthly volume applied to each intercepted field (rchMth_j)
    # 2) calculate:
    # rchMth_j = rchMth_i * (intA_j / rchA_i)
    # 3) Convert to MAR depth for full intercepted field
    # rchMth_j_depth = rchMth_j / sfA_j
    # 4)  Populate output table with rchMth_j_depth for each field by month

    for(i in 1:length(mar_fields24)){
      mar_field = mar_fields24[i,]
      rchA_i = mar_field$area_m3
      svihm_intercepts = svihm_fields[mar_field,]
      # wigglies = c(714, 1015) # keep out some long snaking ones
      # svihm_intercepts=svihm_intercepts[svihm_intercepts$Poly_nmbr %in% wigglies,]
      # plot(svihm_intercepts$geometry, col = "lightblue")
      # plot(mar_field$geometry, add=T, col=rgb(1,0,0,.5))
      for (j in 1:length(svihm_intercepts)){
        sv_field = svihm_intercepts[j,]
        sfA_j = sf::st_area(sv_field)
        field_intersec = sf::st_intersection(x=mar_field, y = sv_field)
        intA_j = sf::st_area(field_intersec)
        # plot(mar_field)
        for(k in 1:nrow(div_monthly)){
          divMnth = div_monthly$m3_diverted[k]
          rchMth_i = divMnth * rchA_i / rchA_all # later this calc may be read from a table instead
          rchMth_j = rchMth_i * (intA_j / rchA_i)
          rchMth_j_depth = rchMth_j / sfA_j
          # assign output
          row_picker = mar_depth_output$Stress_Period==div_monthly$Month[k]
          out_fields = gsub(x = colnames(mar_depth_output[,-1]),
                            pattern = "ID_", replacement = "")
          col_picker = which(sv_field$Poly_nmbr == out_fields) + 1
          mar_depth_output[row_picker,col_picker] = rchMth_j_depth
        }
      }

    }

  }
  # if(tolower(scenario_id=="natveg")){} # Placeholder :
  # Replace default with new MAR vol file, and update recognized_scenarios
  # possible steps: read in fields and potentially adjudicated zone
  # potentially read in schedule of land use updates

  if(!(tolower(scenario_id) %in% recognized_scenarios)){
    print("Warning: specified MAR scenario not recognized in current codebase. Using basecase MAR file (no irrigation applied for MAR)")
    mar_depth_output = field_df
  }
  write.table(mar_depth_output, file = file.path(output_dir, output_filename),
              sep = "  ", quote = FALSE, col.names = TRUE, row.names = FALSE)

}


# ------------------------------------------------------------------------------------------------#

#' Write file specifying Managed Aquifer Recharge (applied irrigation) volumes for each field, for each stress period
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
write_SWBM_curtailment_file <- function(scenario_id = "basecase",
                                        output_dir, start_date, end_date,
                                        apn_overlap_threshold = 0.5,
                                        exclude_terrible_and_major_overflow_polygons = F,
                                        curtail_100_pct_non_lcs_fields = T # for 2022 and 2023 curtail scenarios
                                        ) {
  m2_to_acres = 1/4046.856

  recognized_scenarios = c("basecase",
                           "curtail_00_pct_all_years",
                           "curtail_50_pct_2022",
                           "curtail_30_pct_2022", "curtail_10_pct_2022",
                           "basecase_2023.06.05_curtail_00_pct_2023",
                           "basecase_2023.06.05_curtail_30_pct_2023")
  curtail_scenarios = c("curtail_00_pct_all_years", "curtail_50_pct_2022",
                        "curtail_30_pct_2022", "curtail_10_pct_2022")
  curtail_2023_scenarios = c("basecase_2023.06.05_curtail_00_pct_2023",
                             "basecase_2023.06.05_curtail_10_pct_2023",
                             "basecase_2023.06.05_curtail_20_pct_2023",
                             "basecase_2023.06.05_curtail_30_pct_2023",
                             "basecase_2023.06.05_curtail_40_pct_2023",
                             "basecase_2023.06.05_curtail_50_pct_2023",
                             "basecase_2023.06.05_curtail_60_pct_2023") # 2023 projected curtailment scenarios, June 4th 2023
  output_filename = "curtailment_fractions.txt"
  print(paste("Writing SWBM file:", output_filename))

  # pull reference land cover
  poly_tab = read.table(file.path(data_dir["time_indep_dir","loc"],"polygons_table.txt"),
                        header = T)
  num_unique_fields = length(unique(poly_tab$SWBM_id))
  # Check for polygon ID number continuity
  if(nrow(poly_tab) != num_unique_fields){
    print("Caution: polygons (fields) info table contains replicated or missing ID numbers")
  }
  # generate a stress-period-by-field table (wide format)  for saving to output
  field_df = swbm_build_field_value_df(nfields = num_unique_fields,
                                       model_start_date = start_date,
                                       model_end_date = end_date)

  # Build 0-curtailment fraction table
  field_column_selector = grepl(pattern = "ID", x = colnames(field_df))
  field_df[,field_column_selector] = 0

  ## Basecase curtailment scenario -----------------------------------------

  ## _Process 2022 LCS spatial data -------------------------------------------------------

  # Will need LCS spatial data if the scenario is basecase, in the curtailment scenarios, or just not in any recognized scenarios (in which case default to basecase)
  if(tolower(scenario_id) == "basecase" |
     tolower(scenario_id) %in% curtail_scenarios |
     tolower(scenario_id) %in% curtail_2023_scenarios |
     !(tolower(scenario_id) %in% recognized_scenarios) ){
    ref_dir = data_dir["ref_data_dir","loc"]

    ### Process spatial data associated with curtailment applications from 2022
    # Read LCS land cover files (fields from Landuse and APNs from Plans shapefile)
    lcs_fields_all = sf::read_sf(dsn = file.path(ref_dir,
                                             "Landuse_LCS2022 app matching_2023.05.16.shp"))

    # plot(lcs_fields$geometry, col = lcs_fields$LCS_APP)

    # Process LCS fields with data quality notes
    # overflow50_fields = lcs_fields_all[grepl(pattern = "overflow50",
    #                                        lcs_fields_all$Comments),]
    # overflow_major_fields = lcs_fields_all[grepl(pattern = "majoroverflow",
    #                                          lcs_fields_all$Comments),]
    # overflow_terrible_fields = lcs_fields_all[grepl(pattern = "terribleoverflow",
    #                                              lcs_fields_all$Comments),]
    # sliver_fields = lcs_fields_all[grepl(pattern = "slivercoverage",
    #                                                 lcs_fields_all$Comments),]
    # multipol_fields = lcs_fields_all[grepl(pattern = "multipol",
    #                                      lcs_fields_all$Comments),]
    # other_overflow_fields = c(overflow50_fields$Poly_nmbr,
    #                           overflow_major_fields$Poly_nmbr,
    #                           overflow_terrible_fields$Poly_nmbr,
    #                           sliver_fields$Poly_nmbr)
    # overflow_fields = lcs_fields_all[grepl(pattern = "overflow", lcs_fields_all$Comments) &
    #                                    !(lcs_fields_all$Poly_nmbr %in% other_overflow_fields),]
    # # calculate acreage for categories
    # (sum(as.numeric(st_area(overflow50_fields))) * m2_to_acres)
    # (sum(as.numeric(st_area(overflow_major_fields))) * m2_to_acres)
    # (sum(as.numeric(st_area(overflow_terrible_fields))) * m2_to_acres)
    # (sum(as.numeric(st_area(overflow_fields))) * m2_to_acres)
    # (sum(as.numeric(st_area(sliver_fields))) * m2_to_acres)
    # (sum(as.numeric(st_area(multipol_fields))) * m2_to_acres)


    # plot(overflow50_fields$geometry)
    # plot(overflow_major_fields$geometry)
    # plot(overflow_terrible_fields$geometry, col = c("red", "dodgerblue", "green4"), main = "Top 3 unreasonable polygons")
    # plot(lcs_fields_all$geometry, add=T, fill = NA)
    # plot(sliver_fields$geometry)

    # png(filename = file.path("top 3 unreasonable polygons.png"))
    # plot(overflow_terrible_fields$geometry, col = c("red", "dodgerblue", "green4"), main = "Top 3 unreasonable polygons")
    # plot(lcs_fields_all$geometry, add=T, fill = NA)
    # dev.off()

    # plot(lcs_fields_all$geometry)
    # plot(overflow_terrible_fields$geometry, col = "firebrick", add=t)
    # plot(overflow_major_fields$geometry, col = "orangered", add=t)
    # plot(overflow50_fields$geometry, col = "goldenrod", add=t)

    # Process APNs
    # For apps with no visual maps, gotta use the APN data
    lcs_apns_all = sf::read_sf(dsn = file.path(data_dir["ref_data_dir","loc"],
                                           "LCS 2022 Plans_App matching_2023.05.shp"))
    lcs_apns_all = sf::st_transform(x = lcs_apns_all, crs = sf::st_crs(lcs_fields_all))
    lcs_apns = lcs_apns_all[lcs_apns_all$NoMap!=0,]
    nomap_apps = lcs_apns$NoMap
    # remove "2045" and add "20" and "45"
    nomap_apps = nomap_apps[nomap_apps!=2045]
    nomap_apps = c(nomap_apps, 20, 45)

    # Read in the LCS application info tab
    lcs_info = read.table(file.path(data_dir["ref_data_dir","loc"], "2022 LCS Field Number Matching_interim.txt"),
                          header = T, sep = "\t", fill =T, fileEncoding = "latin1")
    for(i_app in nomap_apps){ #
      print(paste("Finding field overlap fractions for APNs in LCS app. no.",i_app))
      lcs_acres = lcs_info$Acres[lcs_info$Application_Number==i_app]

      if(i_app == 20 | i_app == 45){i_app=2045}

      apn_footprint = lcs_apns[lcs_apns$NoMap==i_app,]
      apn_acres = as.numeric(sf::st_area(apn_footprint))*m2_to_acres
      # Find fields overlapping with apn footprint
      lu_poly_overlap = lcs_fields_all[apn_footprint,]
      lu_poly_overlap$fraction_overlap_apn = NA

      # calculate % of area of each field that overlaps with APN footprint
      for(j in 1:nrow(lu_poly_overlap)){
        field_j = lu_poly_overlap[j,]
        field_apn_intersect = sf::st_intersection(x = field_j, y = apn_footprint)
        field_area_overlap_fraction = as.numeric(sf::st_area(field_apn_intersect) / sf::st_area(field_j))
        lu_poly_overlap$fraction_overlap_apn[j] = field_area_overlap_fraction
      }

      # Assign fields with more than the designated threshold (i.e. 50%) of overlap
      # with the APN footprint to the relevant LCS app number
      assign_these_fields = lu_poly_overlap$Poly_nmbr[lu_poly_overlap$fraction_overlap_apn > apn_overlap_threshold]
      lcs_fields_all$LCS_APP[lcs_fields_all$Poly_nmbr %in% assign_these_fields] = i_app
    }

    # Now that we've assigned all the fields with their LCS app,
    # restrict lcs_fields to just ones with an app number
    lcs_fields = lcs_fields_all[!is.na(lcs_fields_all$LCS_APP),]
    # exclude terrible overflow and major overflow
    if(exclude_terrible_and_major_overflow_polygons == T){
      lcs_fields_excl_major = lcs_fields[!(grepl(pattern = "majoroverflow",
                                                 x = lcs_fields$Comments) |
                                             grepl(pattern = "terribleoverflow",
                                                   x = lcs_fields$Comments)),]
      # sum(st_area(lcs_fields_excl_major)) * m2_to_acres
      # sort(unique(lcs_fields_excl_major$LCS_APP))
      # not included (no map): 12, 13, 18, 20, 23, 25, 28, 31, 32, 45
      # not included (major or terrible overflow): 15, 30 (small acreage)
      lcs_fields = lcs_fields_excl_major
      # sum(st_area(lcs_fields)) * m2_to_acres  # 18.7k acres relative to 17k reported. Not so bad
    }
    # sum(st_area(lcs_fields)) * m2_to_acres  # If include major and terrible overflow, 20.9k acres relative to 17k reported. Not so bad

  }


  ## _Process 2022 LCS curtailment data -------------------------------------------------------

  # Build the curtailment file for 2022 and then 2021.
  # build historical curtailments if scenario is Basecase or unrecognized

  if(tolower(scenario_id) == "basecase" |
     tolower(scenario_id) %in% curtail_2023_scenarios |
     !(tolower(scenario_id) %in% recognized_scenarios) ){
    # read and process 2021 and 2022 curtailment input data

    # Compare spatial area of identified corresponding fields (gis acres) to reported acreage
    lcs_info$acres_reported_to_gis_ratio = NA
    for(i_app in lcs_info$Application_Number){
      acres_reported = lcs_info$Acres[i_app]
      if(i_app == 20 | i_app == 45){i_app_temp = 2045} else { i_app_temp = i_app}
      app_fields = lcs_fields[lcs_fields$LCS_APP==i_app_temp,]
      acres_gis = sum(as.numeric(sf::st_area(app_fields))*m2_to_acres)
      # print(paste("App",i_app, ":", round(acres_reported), "rep. acres;",round(acres_gis), "GIS acres"))
      lcs_info$acres_reported_to_gis_ratio[i_app] = acres_reported/acres_gis

    }

    # Table: month, parcel id, fraction curtailed

    # Simplified curtailment rules
    # For missing % conserved volumes, apply the average of all LCS apps for that month
    conserv_cols_index = grepl(pattern = "Spreadsheet_Check", x = colnames(lcs_info)) &
      grepl(pattern = "conserved", x = colnames(lcs_info)) &
      ! (grepl(pattern = "Total", x = colnames(lcs_info)))
    lcs_info[,conserv_cols_index] = as.numeric(as.matrix(lcs_info[,conserv_cols_index]))
    # calculate monthly conservation averages
    month_avg_cons =apply(X = lcs_info[,conserv_cols_index], MARGIN = 2,
                          FUN = mean, na.rm=T)

    # add new columns to the end of lcs_info table for building model input (to avoid altering original columns with NAs)
    conserv_preweight = paste0(month.abb[4:10], "_pct_cons_preweight")
    # Assign values to model-input columns
    lcs_info[,conserv_preweight] = lcs_info[,conserv_cols_index]
    # fill NAs in each month with monthly average
    for(i in 1:sum(conserv_cols_index)){
      month_cons = lcs_info[,conserv_preweight[i]]
      month_cons[is.na(month_cons)] = month_avg_cons[i]
      # reset curtail_frac model input column for that month
      lcs_info[,conserv_preweight[i]] = month_cons
    }


    # OK, now each application has:
    ## A curtailment conservation % for each month.
    ## A reported acreage-reported-to-GIS-acreage ratio.
    ## For each month, we can multiply the conservation % by the reported-to-GIS ratio
    # to get a value to apply to each field within the Landuse fields footprint (minus unirrigated fields)

    # This should be the same spatial data as lcs_fields_all but why not bring in the fresh dataset
    svihm_fields = sf::read_sf(dsn = file.path(data_dir["ref_data_dir","loc"],"Landuse_20190219.shp"))

    # Initialize tab to store area weighted info for each field
    summary_tab_22 = data.frame(swbm_id = sort(svihm_fields$Polynmbr))
    # Attach LCS curtailment app number to each field ID
    summary_tab_22$curtail_app_id = NA
    summary_tab_22$curtail_app_id = lcs_fields_all$LCS_APP[match( summary_tab_22$swbm_id, lcs_fields_all$Poly_nmbr)]
    # Apr-Oct conserv percentages from gap-filled LCS data
    monthly_conserv = data.frame(matrix(data = NA, nrow = nrow(summary_tab_22),
                                        ncol = length(conserv_preweight)))
    colnames(monthly_conserv) = conserv_preweight
    summary_tab_22 = cbind(summary_tab_22, monthly_conserv)
    # fill monthly conservation %s in summary_tab_22
    for(month_abb in month.abb[4:10]){
      month_cons_colname = paste0(month_abb,"_pct_cons_preweight")
      summary_tab_22[,month_cons_colname] = lcs_info[
        match(summary_tab_22$curtail_app_id, lcs_info$Application_Number),
        month_cons_colname]
    }

    # Attach Landuse fields (gis) to reported acreage ratio
    summary_tab_22$acres_reported_to_gis_ratio = NA
    summary_tab_22$acres_reported_to_gis_ratio = lcs_info$acres_reported_to_gis_ratio[
      match(summary_tab_22$curtail_app_id, lcs_info$Application_Number)]

    # Initialize columns for Apr-Oct conserv %ages after final weighting
    # final weighting =
    ## reported curtail app % conserved for a given month, times
    ## the Landuse fields-area-to-reported-acreage-area ratio

    conserv_for_model_lcs = paste0(month.abb[4:10], "_pct_cons_model")
    summary_tab_22[,conserv_for_model_lcs] = NA
    summary_tab_22[,conserv_for_model_lcs] = summary_tab_22[,conserv_preweight] * summary_tab_22$acres_reported_to_gis_ratio

    ## _2022 non-LCS fields -------------------------------------------------------

    # OK. there are 3 time periods we are dealing with here.
    # LCS months, Apr-Oct 2022.
    # Non-LCS fields curtailed on July 13 - so July 2022 is 0.5 curtailment for non-LCS fields.
    # Non-LCS fields remained curtailed through October 2022. So Aug-Oct 2022 is 1.0 curtailment for non-LCS fields.

    conserv_for_model_nonlcs_0.5 = paste0(month.abb[7], "_pct_cons_model")
    conserv_for_model_nonlcs_1.0 = paste0(month.abb[8:10], "_pct_cons_model")

    non_lcs_fields = lcs_fields_all[is.na(lcs_fields_all$LCS_APP),]
    # include fields listed as App #0, I think that's a quality control thing
    fields_0 = lcs_fields_all[lcs_fields_all$LCS_APP==0 & !is.na(lcs_fields_all$LCS_APP),]
    non_lcs_fields = rbind(non_lcs_fields, fields_0)
    non_lcs_field_ids = non_lcs_fields$Poly_nmbr
    # curtail 100% of all water use on non-lcs fields
    summary_tab_22[summary_tab_22$swbm_id %in% non_lcs_field_ids, conserv_for_model_nonlcs_0.5] = (17/31) #0.5
    summary_tab_22[summary_tab_22$swbm_id %in% non_lcs_field_ids, conserv_for_model_nonlcs_1.0] = 1.0

    # _Produce: month-by-field curtailment fraction tables for historical basecase ---------------------------------------------
    # Initialize model input file (curtail_output of this process, input for model run)
    curtail_output = field_df
    swbm_ids = sort(svihm_fields$Polynmbr)
    # match months to stress periods
    # Basecase: non-LCS fields get 0.5 curtailment in July, 1.0 Aug, Sept, Oct of 2022
    conserv_2022_months_lcs = as.Date(paste0("2022-",4:10,"-01"))
    conserv_2022_month_dates_0.5 = as.Date(paste0("2022-",7,"-01"))
    conserv_2022_month_dates_1.0 = as.Date(paste0("2022-",8:10,"-01"))

    # Make the final input file. Make it run in fortran.
    date_matcher_tab_lcs = data.frame(stress_per_date = conserv_2022_months_lcs,
                                  curtail_colname = conserv_for_model_lcs)
    date_matcher_tab_0.5 = data.frame(stress_per_date = conserv_2022_month_dates_0.5,
                                  curtail_colname = conserv_for_model_nonlcs_0.5)
    date_matcher_tab_1.0 = data.frame(stress_per_date = conserv_2022_month_dates_1.0,
                                      curtail_colname = conserv_for_model_nonlcs_1.0)
    # Assign curtailment fractions for LCS fields, Apr-Oct 2022
    for(i in 1:nrow(date_matcher_tab_lcs)){
      sp = date_matcher_tab_lcs$stress_per_date[i]
      cname = date_matcher_tab_lcs$curtail_colname[i]
      # assign overlap- and acre-ratio- weighted curtailment fractions to each stress period
      curtail_output[curtail_output$Stress_Period==sp, 2:ncol(curtail_output)] =
        summary_tab_22[match(swbm_ids, summary_tab_22$swbm_id),cname]
    }
    # Assign curtailments for non-LCS fields, 0.5 Jul 2022
    for(i in 1:nrow(date_matcher_tab_0.5)){
      sp = date_matcher_tab_0.5$stress_per_date[i]
      cname = date_matcher_tab_0.5$curtail_colname[i]
      # assign overlap- and acre-ratio- weighted curtailment fractions to each stress period
      curtail_output[curtail_output$Stress_Period==sp, 2:ncol(curtail_output)] =
        summary_tab_22[match(swbm_ids, summary_tab_22$swbm_id),cname]
    }
    # Assign curtailments for non-LCS fields, 1.0 Aug-Oct 2022
    for(i in 1:nrow(date_matcher_tab_1.0)){
      sp = date_matcher_tab_1.0$stress_per_date[i]
      cname = date_matcher_tab_1.0$curtail_colname[i]
      # assign overlap- and acre-ratio- weighted curtailment fractions to each stress period
      curtail_output[curtail_output$Stress_Period==sp, 2:ncol(curtail_output)] =
        summary_tab_22[match(swbm_ids, summary_tab_22$swbm_id),cname]
    }

    ## _2021 data -------------------------------------------------------
    # Make new summary tab including only the fields that did curtailments in Aug-Oct 2021
    lcs_apps_21 = c(12, 13, 31) # Fawaz, Finley, Menne: 12, 13, 31
    conserv_for_model_21 = paste0(month.abb[8:10],"_pct_cons_model")
    summary_tab_21 = summary_tab_22[, c("swbm_id", "curtail_app_id", conserv_for_model_21)]
    summary_tab_21[,conserv_for_model_21] = 0
    summary_tab_21[summary_tab_21$curtail_app_id %in% lcs_apps_21, conserv_for_model_21] = 1

    # match months to stress periods
    conserv_2021_month_dates = as.Date(paste0("2021-",8:10,"-01"))
    date_matcher_tab_21 = data.frame(stress_per_date = conserv_2021_month_dates,
                                     curtail_colname = paste0(month.abb[8:10],"_pct_cons_model"))

    # Assign LCS
    for(i in 1:nrow(date_matcher_tab_21)){
      sp = date_matcher_tab_21$stress_per_date[i]
      cname = date_matcher_tab_21$curtail_colname[i]
      # assign overlap- and acre-ratio- weighted curtailment fractions to each stress period
      curtail_output[curtail_output$Stress_Period==sp, 2:ncol(curtail_output)] =
        summary_tab_21[match(swbm_ids, summary_tab_21$swbm_id),cname]
    }

    # 2021 Part 2
    # GW curtailed for whole valley from Sept 10 - Oct 25
    conserv_for_model_0921 = paste0(month.abb[9], "_pct_cons_model")
    conserv_for_model_1021 = paste0(month.abb[10], "_pct_cons_model")
    curt_2021_sept = as.Date(paste0("2021-",10,"-01"))
    curt_2021_oct = as.Date(paste0("2021-", 9,"-01"))
    # Apply to all fields NOT n the lcs_apps_21
    summary_tab_21[!(summary_tab_21$curtail_app_id %in% lcs_apps_21), conserv_for_model_0921] = 2/3 # 20 of 30 days
    summary_tab_21[!(summary_tab_21$curtail_app_id %in% lcs_apps_21), conserv_for_model_1021] = 25/31

    # Date matchers
    date_matcher_tab_sept21 = data.frame(stress_per_date = curt_2021_sept,
                                         curtail_colname = conserv_for_model_0921)
    date_matcher_tab_oct21 = data.frame(stress_per_date = curt_2021_oct,
                                        curtail_colname = conserv_for_model_1021)

    # Assign curtailments for Sept 2021
    for(i in 1:nrow(date_matcher_tab_sept21)){
      sp = date_matcher_tab_sept21$stress_per_date[i]
      cname = date_matcher_tab_sept21$curtail_colname[i]
      # assign overlap- and acre-ratio- weighted curtailment fractions to each stress period
      curtail_output[curtail_output$Stress_Period==sp, 2:ncol(curtail_output)] =
        summary_tab_21[match(swbm_ids, summary_tab_21$swbm_id),cname]
    }

    # Assign curtailments for Oct 2021
    for(i in 1:nrow(date_matcher_tab_oct21)){
      sp = date_matcher_tab_oct21$stress_per_date[i]
      cname = date_matcher_tab_oct21$curtail_colname[i]
      # assign overlap- and acre-ratio- weighted curtailment fractions to each stress period
      curtail_output[curtail_output$Stress_Period==sp, 2:ncol(curtail_output)] =
        summary_tab_21[match(swbm_ids, summary_tab_21$swbm_id),cname]
    }

    # Final table cleanup
    # Replace any NAs with 0s
    curtail_output[is.na(curtail_output)] = 0
    field_cols = grep(pattern = "ID", x=colnames(curtail_output))

    # PLACEHOLDER UNTIL BETTER DATA QUALITY
    # Identify fields with curtailment fractions > 1. Reset to 1.
    # sum(curtail_output[,field_cols] > 1) # 195 of them
    # sum(curtail_output[,field_cols] < 1 & curtail_output[,field_cols] > 0) #
    curtail_output[,field_cols][curtail_output[,field_cols] > 1] = 1
  }

  ## Non-basecase scenarios -------------------------------------------------
  if(tolower(scenario_id)=="curtail_00_pct_all_years"){
    curtail_output = field_df
  }
  if(tolower(scenario_id)=="curtail_50_pct_2022"){
    curtail_output = field_df

    curtail_months = curtail_output$Stress_Period >= as.Date("2022-04-01") &
      curtail_output$Stress_Period <= as.Date("2022-10-01")
    lcs_fields = lcs_fields_all[!is.na(lcs_fields_all$LCS_APP),]
    # plot(lcs_fields$geometry)
    lcs_fields_colnames = paste0("ID_",lcs_fields$Poly_nmbr)
    curtail_output[curtail_months,lcs_fields_colnames] = 0.5

    # Assign 100% curtailment to non-LCS fields
    non_lcs_fields = lcs_fields_all[is.na(lcs_fields_all$LCS_APP),]
    # include fields listed as App #0, I think that's a quality control thing
    fields_0 = lcs_fields_all[lcs_fields_all$LCS_APP==0 & !is.na(lcs_fields_all$LCS_APP),]
    non_lcs_fields = rbind(non_lcs_fields, fields_0)
    non_lcs_field_ids = non_lcs_fields$Poly_nmbr
    # curtail 100% of all water use on non-lcs fields
    non_lcs_fields_colnames = paste0("ID_",non_lcs_fields$Poly_nmbr)
    curtail_output[curtail_months,non_lcs_fields_colnames] = 1
  }
  if(tolower(scenario_id)=="curtail_30_pct_2022"){
    curtail_output = field_df

    curtail_months = curtail_output$Stress_Period >= as.Date("2022-04-01") &
      curtail_output$Stress_Period <= as.Date("2022-10-01")
    lcs_fields = lcs_fields_all[!is.na(lcs_fields_all$LCS_APP),]
    # plot(lcs_fields$geometry)
    lcs_fields_colnames = paste0("ID_",lcs_fields$Poly_nmbr)
    curtail_output[curtail_months,lcs_fields_colnames] = 0.3

    # Assign 100% curtailment to non-LCS fields
    non_lcs_fields = lcs_fields_all[is.na(lcs_fields_all$LCS_APP),]
    # include fields listed as App #0, I think that's a quality control thing
    fields_0 = lcs_fields_all[lcs_fields_all$LCS_APP==0 & !is.na(lcs_fields_all$LCS_APP),]
    non_lcs_fields = rbind(non_lcs_fields, fields_0)
    non_lcs_field_ids = non_lcs_fields$Poly_nmbr
    # curtail 100% of all water use on non-lcs fields
    non_lcs_fields_colnames = paste0("ID_",non_lcs_fields$Poly_nmbr)
    curtail_output[curtail_months,non_lcs_fields_colnames] = 1
  }
  if(tolower(scenario_id)=="curtail_10_pct_2022"){
    curtail_output = field_df
    curtail_months = curtail_output$Stress_Period >= as.Date("2022-04-01") &
      curtail_output$Stress_Period <= as.Date("2022-10-01")
    lcs_fields = lcs_fields_all[!is.na(lcs_fields_all$LCS_APP),]
    # plot(lcs_fields$geometry)
    lcs_fields_colnames = paste0("ID_",lcs_fields$Poly_nmbr)
    curtail_output[curtail_months,lcs_fields_colnames] = 0.1

    # Assign 100% curtailment to non-LCS fields
    non_lcs_fields = lcs_fields_all[is.na(lcs_fields_all$LCS_APP),]
    # include fields listed as App #0, I think that's a quality control thing
    fields_0 = lcs_fields_all[lcs_fields_all$LCS_APP==0 & !is.na(lcs_fields_all$LCS_APP),]
    non_lcs_fields = rbind(non_lcs_fields, fields_0)
    non_lcs_field_ids = non_lcs_fields$Poly_nmbr
    # curtail 100% of all water use on non-lcs fields
    non_lcs_fields_colnames = paste0("ID_",non_lcs_fields$Poly_nmbr)
    curtail_output[curtail_months,non_lcs_fields_colnames] = 1
  }
  # For scenarios in the June 2023, forcast the rest of 2023 group
  apply_curtail_frac_to_forecasted_2023 = function(curtail_output, curtail_frac_lcs_2023){
    #implement LCS curtailments April-Oct
    curtail_months_lcs = curtail_output$Stress_Period >= as.Date("2023-04-01") &
      curtail_output$Stress_Period <= as.Date("2023-10-01")
    lcs_fields = lcs_fields_all[!is.na(lcs_fields_all$LCS_APP),]
    # plot(lcs_fields$geometry)
    lcs_fields_colnames = paste0("ID_",lcs_fields$Poly_nmbr)
    curtail_output[curtail_months_lcs,lcs_fields_colnames] = curtail_frac_lcs_2023

    # Assign 100% curtailment to non-LCS fields for Sep and Oct 2023
    curtail_months_nonlcs = curtail_output$Stress_Period >= as.Date("2023-09-01") &
      curtail_output$Stress_Period <= as.Date("2023-10-01")
    non_lcs_fields = lcs_fields_all[is.na(lcs_fields_all$LCS_APP),]
    # include fields listed as App #0, I think that's a quality control thing
    fields_0 = lcs_fields_all[lcs_fields_all$LCS_APP==0 & !is.na(lcs_fields_all$LCS_APP),]
    non_lcs_fields = rbind(non_lcs_fields, fields_0)
    non_lcs_field_ids = non_lcs_fields$Poly_nmbr
    # curtail 100% of all water use on non-lcs fields
    non_lcs_fields_colnames = paste0("ID_",non_lcs_fields$Poly_nmbr)
    curtail_output[curtail_months_nonlcs,non_lcs_fields_colnames] = 1

    # # 2023 forecast, 2023.06.04: non-LCS fields get 1.0 curtailment in Sept and Oct
    # conserv_2023_month_dates_1.0 = as.Date(paste0("2023-",9:10,"-01"))

    return(curtail_output)
  }
  if(tolower(scenario_id)=="basecase_2023.06.05_curtail_00_pct_2023"){
    # Do nothing. Default basecase, if you put in the full 2023 record, will have 0s for all the 2023 months
  }
  if(tolower(scenario_id)=="basecase_2023.06.05_curtail_10_pct_2023"){
    # For 2023, assuming flow conditions similar to 2019, we would implement the full LCS
    # curtailments for April-October, but the 100% curtailments on non-LCS fields
    # only for Aug-Sept, based on dropping below 30 and 33 cfs in those months
    # https://www.waterboards.ca.gov/waterrights/water_issues/programs/drought/scott_shasta_rivers/docs/scott_adjudicated_gw_curtailment_order.pdf
    # https://waterdata.usgs.gov/monitoring-location/11519500/#parameterCode=00060&startDT=2019-01-01&endDT=2020-01-01
    curtail_output = apply_curtail_frac_to_forecasted_2023(curtail_output=curtail_output,
                                                           curtail_frac_lcs_2023 = 0.1)
  }
  if(tolower(scenario_id)=="basecase_2023.06.05_curtail_20_pct_2023"){
    # For 2023, assuming flow conditions similar to 2019, we would implement the full LCS
    # curtailments for April-October, but the 100% curtailments on non-LCS fields
    # only for Aug-Sept, based on dropping below 30 and 33 cfs in those months
    # https://www.waterboards.ca.gov/waterrights/water_issues/programs/drought/scott_shasta_rivers/docs/scott_adjudicated_gw_curtailment_order.pdf
    # https://waterdata.usgs.gov/monitoring-location/11519500/#parameterCode=00060&startDT=2019-01-01&endDT=2020-01-01
    curtail_output = apply_curtail_frac_to_forecasted_2023(curtail_output=curtail_output,
                                                           curtail_frac_lcs_2023 = 0.2)
  }
  if(tolower(scenario_id)=="basecase_2023.06.05_curtail_30_pct_2023"){
    # For 2023, assuming flow conditions similar to 2019, we would implement the full LCS
    # curtailments for April-October, but the 100% curtailments on non-LCS fields
    # only for Aug-Sept, based on dropping below 30 and 33 cfs in those months
    # https://www.waterboards.ca.gov/waterrights/water_issues/programs/drought/scott_shasta_rivers/docs/scott_adjudicated_gw_curtailment_order.pdf
    # https://waterdata.usgs.gov/monitoring-location/11519500/#parameterCode=00060&startDT=2019-01-01&endDT=2020-01-01
    curtail_output = apply_curtail_frac_to_forecasted_2023(curtail_output=curtail_output,
                                          curtail_frac_lcs_2023 = 0.3)
    }
  if(tolower(scenario_id)=="basecase_2023.06.05_curtail_40_pct_2023"){
    # For 2023, assuming flow conditions similar to 2019, we would implement the full LCS
    # curtailments for April-October, but the 100% curtailments on non-LCS fields
    # only for Aug-Sept, based on dropping below 30 and 33 cfs in those months
    # https://www.waterboards.ca.gov/waterrights/water_issues/programs/drought/scott_shasta_rivers/docs/scott_adjudicated_gw_curtailment_order.pdf
    # https://waterdata.usgs.gov/monitoring-location/11519500/#parameterCode=00060&startDT=2019-01-01&endDT=2020-01-01
    curtail_output = apply_curtail_frac_to_forecasted_2023(curtail_output=curtail_output,
                                                           curtail_frac_lcs_2023 = 0.4)
  }
  if(tolower(scenario_id)=="basecase_2023.06.05_curtail_50_pct_2023"){
    # For 2023, assuming flow conditions similar to 2019, we would implement the full LCS
    # curtailments for April-October, but the 100% curtailments on non-LCS fields
    # only for Aug-Sept, based on dropping below 30 and 33 cfs in those months
    # https://www.waterboards.ca.gov/waterrights/water_issues/programs/drought/scott_shasta_rivers/docs/scott_adjudicated_gw_curtailment_order.pdf
    # https://waterdata.usgs.gov/monitoring-location/11519500/#parameterCode=00060&startDT=2019-01-01&endDT=2020-01-01
    curtail_output = apply_curtail_frac_to_forecasted_2023(curtail_output=curtail_output,
                                                           curtail_frac_lcs_2023 = 0.5)
  }


  ## Write curtailment output file -----------------------------------------
  if(!(tolower(scenario_id) %in% recognized_scenarios)){
    print("Warning: specified curtailment scenario not recognized in current codebase. Using basecase curtailment file")
  }

  if(tolower(scenario_id) %in% curtail_2023_scenarios){
    output_filename = paste0("curtailment_fractions_", scenario_id,".txt")
  }
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
