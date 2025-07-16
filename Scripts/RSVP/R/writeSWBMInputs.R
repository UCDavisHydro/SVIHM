#' Read SWBM Polygon File and Add Text Translations
#'
#' Reads the SWBM polygon table, then, if present, adds human-readable text columns
#' for any of the code fields \code{SWBM_LU}, \code{SWBM_IRR}, \code{WATERSOURC},
#' and \code{subws_ID} by calling \code{translate_SWBM_codes()}.
#'
#' @param filename Character. Path to the polygon file (passed to \code{read.table}).
#' @param landcover_desc Data.frame with columns \code{id} and \code{Landcover_Name};
#'   used to translate \code{SWBM_LU} to \code{LU_txt}.
#' @param tributary_desc Data.frame with columns \code{subws_ID} and \code{subws_name};
#'   used to translate \code{subws_ID} to \code{subws_txt}.
#' @param header Logical. Does the file have a header row? Default: \code{TRUE}.
#' @param sep Character. Field separator. Default: any whitespace (the \code{""} default for \code{read.table}).
#' @param stringsAsFactors Logical. Should character columns be converted to factors? Default: \code{FALSE}.
#' @param ... Additional arguments passed to \code{read.table()}.
#'
#' @return A \code{data.frame} containing all the original columns plus, where applicable:
#'   \itemize{
#'     \item \code{LU_txt}     - text for \code{SWBM_LU}
#'     \item \code{IRR_txt}    - text for \code{SWBM_IRR}
#'     \item \code{WS_txt}     - text for \code{WATERSOURC}
#'     \item \code{subws_txt}  - text for \code{subws_ID}
#'   }
#'
#' @export
#' @examples
#' \dontrun{
#' landcov <- read.table("landcover_table.txt", header=TRUE)
#' trib    <- read.table("tributary_desc.txt", header=TRUE)
#' poly    <- read_SWBM_polygon_file(
#'   filename        = "polygons_table.txt",
#'   landcover_desc  = landcov,
#'   tributary_desc  = trib
#' )
#' head(poly)
#' }
read_SWBM_polygon_file <- function(filename,
                                   landcover_desc,
                                   tributary_desc,
                                   header           = TRUE,
                                   sep              = "",
                                   stringsAsFactors = FALSE,
                                   ...) {
  # 1) Read the raw table
  df <- read.table(
    filename,
    header           = header,
    sep              = sep,
    stringsAsFactors = stringsAsFactors,
    ...
  )
  # 2) Translate any code columns into *_txt columns
  df <- translate_SWBM_codes(df, landcover_desc, tributary_desc)
  return(df)
}

#-------------------------------------------------------------------------------------------------#
#' Write SWBM Polygon File
#'
#' Writes a SWBM polygon table to disk, **excluding** any of the helper
#' "_txt" columns added by \code{translate_SWBM_codes()}.  Only the original
#' code and numeric fields (in the standard SWBM order) are written.
#'
#' @param df A \code{data.frame} as returned by \code{read_SWBM_polygon_file()},
#'   possibly containing additional \code{*_txt} columns.
#' @param output_dir Character. Directory in which to write the file.
#' @param filename Character. File name, default \code{"polygons_table.txt"}.
#' @param sep Character. Field separator. Default: tab (\code{"\t"}).
#' @param quote Logical. Quote character fields? Default: \code{FALSE}.
#' @param row.names Logical or character. Whether to write row names. Default: \code{FALSE}.
#' @param col.names Logical. Whether to write column names. Default: \code{TRUE}.
#' @param verbose Logical. If \code{TRUE}, prints a message before writing. Default: \code{TRUE}.
#'
#' @return Invisibly returns \code{NULL}.  Side effect: writes the file.
#'
#' @export
#' @examples
#' \dontrun{
#' poly   <- read_SWBM_polygon_file("polygons_table.txt", landcov, trib)
#' # ... manipulate poly as needed ...
#' write_SWBM_polygon_file(
#'   df         = poly,
#'   output_dir = "outputs",
#'   filename   = "polygons_table.txt"
#' )
#' }
write_SWBM_polygon_file <- function(df,
                                    output_dir,
                                    filename='polygons_table.txt',
                                    sep       = "\t",
                                    quote     = FALSE,
                                    row.names = FALSE,
                                    col.names = TRUE,
                                    verbose   = TRUE) {
  # Define the original SWBM polygon columns, in order:
  orig_cols <- c("SWBM_id","subws_ID","SWBM_LU","SWBM_IRR","MF_Area_m2","WATERSOURC","WHC",
                 "Initial_fill_fraction","max_infil_rate","runoff_ISEG","WL_2_CP_Yr","ILR_Flag")
  # Check for missing columns
  missing <- setdiff(orig_cols, names(df))
  if (length(missing) > 0) {
    stop(
      "Cannot write polygon file: missing column(s): ",
      paste(missing, collapse = ", ")
    )
  }
  # Subset to just the original columns
  df_out <- df[orig_cols]
  # Write it
  out_path <- file.path(output_dir, filename)
  if (verbose) message("Writing SWBM polygon file: ", out_path)
  write.table(
    df_out,
    file      = out_path,
    sep       = sep,
    quote     = quote,
    row.names = row.names,
    col.names = col.names
  )
  invisible(NULL)
}

#-------------------------------------------------------------------------------------------------#
#' Read Inflow Segment File
#'
#' Reads a tab-delimited inflow segment file into a data frame.  The expected columns are:
#' * `subws_ID`(integer) subwatershed ID (may repeat)
#' * `ISEG`(integer) inflow-segment number
#' * `subws_name`(character) subwatershed name
#' * `stream_name`(character) stream name (may contain spaces)
#'
#' @param file Character. Path to the inflow-segment file.
#' @param header Logical. Does the first line contain column names? Default: `TRUE`.
#' @param sep Character. Field separator. Default: `"\t"`.
#' @param stringsAsFactors Logical. Should character columns be converted to factors? Default: `FALSE`.
#' @param ... Additional arguments passed to `read.table()`.
#'
#' @return A `data.frame` with columns
#'   \describe{
#'     \item{subws_ID}{integer}
#'     \item{ISEG}{integer}
#'     \item{subws_name}{character}
#'     \item{stream_name}{character}
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' df <- read_inflow_seg_file("inflow_segments.txt")
#' str(df)
#' }
read_inflow_seg_file <- function(file,
                                 header = TRUE,
                                 sep = "\t",
                                 stringsAsFactors = FALSE,
                                 ...) {
  # Read raw table, preventing quote-based splitting and comments
  df <- read.table(file,
                   header           = header,
                   sep              = sep,
                   quote            = "",
                   comment.char     = "",
                   stringsAsFactors = stringsAsFactors,
                   ...)
  # Coerce key columns to integer
  df$subws_ID <- as.integer(df$subws_ID)
  df$ISEG     <- as.integer(df$ISEG)
  # Ensure name columns are character
  df$subws_name  <- as.character(df$subws_name)
  df$stream_name <- as.character(df$stream_name)
  return(df)
}

#-------------------------------------------------------------------------------------------------#

#' Write a SWBM-Formatted Table to File
#'
#' Writes the contents of a data frame to a space-delimited text file suitable for
#' ingestion by the Soil Water Budget Model (SWBM).
#'
#' @param df A `data.frame` whose contents will be written to disk.
#' @param output_dir Character. Path to the directory where the file should be written.
#' @param filename Character. Name of the output file (e.g. `"subwatershed_irrigation_inflows.txt"`).
#' @param verbose Logical. If `TRUE`, prints a message to the console indicating which file is being written.
#'
#' @details
#' This is a thin wrapper around `write.table()` that uses:
#' - space (`" "`) as the field separator
#' - no quoting of values
#' - inclusion of column names but no row names
#'
#' @return Invisibly returns `NULL`. The primary effect is the side-effect of writing `df` to disk.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Suppose `my_df` is your SWBM input table:
#' my_df <- data.frame(
#'   Date = c("2021-01-01", "2021-01-02"),
#'   Flow = c(10.5, 12.3)
#' )
#' # Write it out:
#' write_SWBM_file(df= my_df, output_dir = "inputs", filename = "example_inflows.txt")
#'}
write_SWBM_file <- function(df, output_dir, filename, verbose) {
  if (verbose) {
    message("Writing SWBM file: ", filename)
  }
  write.table(
    df,
    file      = file.path(output_dir, filename),
    sep       = " ",
    quote     = FALSE,
    col.names = TRUE,
    row.names = FALSE
  )
}
#-------------------------------------------------------------------------------------------------#

#' Write SWBM Stress Period Days File
#'
#' Writes the number of days in each stress period (month) for the Soil Water Budget Model (SWBM).
#' The output file is used to inform the model how many days are in each stress period.
#'
#' @param num_days_df A data frame with one row per stress period and one or more columns (e.g., stress period number, number of days).
#'   Column names will be included in the output file.
#' @param output_dir Directory where the file will be written.
#' @param filename Character. Output file name. Default is `"stress_period_days.txt"`.
#' @param verbose Logical. If `TRUE`, prints a message indicating the file is being written. Default is `TRUE`.
#'
#' @return None. Writes the stress period days file to disk.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' num_days <- data.frame(Period = 1:12, Days = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))
#' write_SWBM_sp_days_file(num_days)
#' }
write_SWBM_sp_days_file <- function(num_days_df, output_dir, filename='stress_period_days.txt', verbose=TRUE) {
  write_SWBM_file(num_days_df, output_dir, filename, verbose)
}

#-------------------------------------------------------------------------------------------------#
#' Write SWBM Main Input File (svihm.swbm)
#'
#' Writes the main input file for the Soil Water Budget Model (SWBM), using defaults
#' consistent with the example configuration. All potential input files can be specified,
#' including optional managed-recharge and streamflow files.
#'
#' @param output_dir Directory where the input file will be written.
#' @param num_stress_periods Integer. Number of model stress periods (`NMONTHS`).
#' @param filename Character. Name of the output file. Default: `"svihm.swbm"`.
#' @param modelName Character. MODFLOW model name (`MFNAME`). Default: `"SVIHM"`.
#' @param WYstart Integer. Water-year start (`WYSTART`). Default: `1991`.
#' @param npoly Integer. Number of polygons (`NPOLY`). Default: `2119`.
#' @param nlandcover Integer. Number of land-cover types (`NLANDCOVER`). Default: `6`.
#' @param nAgWells Integer. Number of agricultural wells (`NAGWELLS`). Default: `167`.
#' @param nSpecWells Integer. Number of special wells (`NSPECWELLS`). Default: `0`.
#' @param nSubws Integer. Number of sub-watersheds (`NSUBWS`). Default: `8`.
#' @param nMfrWells Integer. Number of managed-recharge wells (`NMFRWELLS`). Default: `1163`.
#' @param inflow_is_vol Logical. Whether inflow is volume-based. Default: `FALSE`.
#' @param daily_sw Logical. Write daily soil-water budget output (`DAILY_SW`). Default: `TRUE`.
#' @param nSFR_inflow_segs Integer. Number of SFR inflow segments (`NSFR_INFLOW_SEGS`). Default: `12`.
#' @param nrows Integer. Number of grid rows (`NROWS`). Default: `440`.
#' @param ncols Integer. Number of grid columns (`NCOLS`). Default: `210`.
#' @param neighborRuleYearDay Integer. Neighbor rule cutoff day (`NEIGHBOR_RULE`). Default: `250`.
#' @param absoluteIrrDate Integer(2). Absolute irrigation cutoff date (`ABSOLUTE_IRR_DATE`) as `c(month, day)`. Default: `c(5, 15)`.
#' @param writeUCODE Logical. Write UCODE block (`WRITE_UCODE`). Default: `TRUE`.
#' @param writePEST Logical. Write PEST block (`WRITE_PEST`). Default: `FALSE`.
#'
#' @param precip_file Character. PRECIP file. Default: `"precip.txt"`.
#' @param et_file Character. ET file. Default: `"ref_et.txt"`.
#' @param et_ext_depth_file Character. ET_EXT_DEPTH file. Default: `"ET_Cells_Extinction_Depth.txt"`.
#' @param ets_template_file Character. ETS_TEMPLATE file. Default: `"SVIHM_ETS_template.txt"`.
#' @param kc_frac_file Character. KC_FRAC file. Default: `"kc_values.txt"`.
#'
#' @param sfr_template_file Character. SFR_TEMPLATE file. Default: `"SFR_network.txt"`.
#' @param sfr_routing_file Character. SFR_ROUTING file. Default: `"SFR_routing.txt"`.
#' @param sfr_segments_file Character. SFR_SEGMENTS file. Default: `"SFR_inflow_segments.txt"`.
#' @param sfr_irr_flows_file Character. SFR_IRR_FLOWS file. Default: `"subwatershed_irrigation_inflows.txt"`.
#' @param sfr_nonirr_flows_file Character. SFR_NONIRR_FLOWS file. Default: `"subwatershed_nonirrigation_inflows.txt"`.
#' @param wel_template_file Character. WEL_TEMPLATE file. Default: `"SVIHM_WEL_template.txt"`.
#'
#' @param recharge_zones_file Character. RECHARGE_ZONES file. Default: `"recharge_zones.txt"`.
#' @param poly_landcover_file Character. POLY_LANDCOVER file. Default: `"polygon_landcover_ids.txt"`.
#' @param poly_agwell_file Character. POLY_AGWELL file. Default: `"ag_well_list_by_polygon.txt"`.
#' @param agwell_locs_file Character. AGWELL_LOCS file. Default: `"ag_well_summary.txt"`.
#'
#' @param specwell_locs_file Character or NULL. SPECWELL_LOCS file. Default: `NULL`.
#' @param specwell_vol_file Character or NULL. SPECWELL_VOL file. Default: `NULL`.
#'
#' @param mfrwell_locs_file Character or NULL. MFRWELL_LOCS file. Default: `"modflow_cell_to_catchment.txt"`.
#' @param mfrwell_rates_file Character or NULL. MFRWELL_RATES file. Default: `"monthly_MFR_by_catchment.txt"`.
#' @param mfrwell_mult_file Character or NULL. MFRWELL_MULT file. Default: `NULL`.
#'
#' @param et_zone_cells_file Character or NULL. ET_ZONE_CELLS file. Default: `"ET_Zone_Cells.txt"`.
#' @param sfr_jtf_file Character or NULL. SFR_NETWORK_JTF file. Default: `"SFR_network_jtf.txt"`.
#' @param irr_ditch_file Character or NULL. IRR_DITCH file. Default: `"irr_ditch.txt"`.
#' @param mar_depth_file Character or NULL. MAR_DEPTH file. Default: `"MAR_depth.txt"`.
#' @param curtail_frac_file Character or NULL. CURTAIL_FRAC file. Default: `"curtailment_fractions.txt"`.
#' @param et_correction_file Character or NULL. ET_CORRECTION file. Default: `"field_et_corrections.txt"`.
#' @param water_mover_file Character or NULL. WATER_MOVER file. Default: `NULL`.
#'
#' @param print_daily_fields List of named lists, each with `id` (integer) and `prefix` (character), for the PRINT_DAILY block. Default: `list()`.
#' @param verbose Logical. Print progress messages? Default: `TRUE`.
#'
#' @return None; writes the SWBM input file to `file.path(output_dir, filename)`.
#' @export
#'
#' @examples
#' \dontrun{
#' write_SWBM_main_input_file(
#'   output_dir           = "inputs",
#'   num_stress_periods   = 396
#' )
#' }
write_SWBM_main_input_file <- function(
    output_dir,
    num_stress_periods,
    filename               = "svihm.swbm",
    modelName              = "SVIHM",
    WYstart                = 1991,
    npoly                  = 2119,
    nlandcover             = 6,
    nAgWells               = 167,
    nSpecWells             = 0,
    nSubws                 = 8,
    nMfrWells              = 1163,
    inflow_is_vol          = FALSE,
    daily_sw               = TRUE,
    nSFR_inflow_segs       = 12,
    nrows                  = 440,
    ncols                  = 210,
    neighborRuleYearDay    = 250,
    absoluteIrrDate        = c(5, 15),
    writeUCODE             = TRUE,
    writePEST              = FALSE,
    precip_file            = "precip.txt",
    et_file                = "ref_et.txt",
    et_ext_depth_file      = "ET_Cells_Extinction_Depth.txt",
    ets_template_file      = "SVIHM_ETS_template.txt",
    kc_frac_file           = "kc_values.txt",
    sfr_template_file      = "SFR_network.txt",
    sfr_routing_file       = "SFR_routing.txt",
    sfr_segments_file      = "SFR_inflow_segments.txt",
    sfr_irr_flows_file     = "subwatershed_irrigation_inflows.txt",
    sfr_nonirr_flows_file  = "subwatershed_nonirrigation_inflows.txt",
    wel_template_file      = "SVIHM_WEL_template.txt",
    recharge_zones_file    = "recharge_zones.txt",
    poly_landcover_file    = "polygon_landcover_ids.txt",
    poly_agwell_file       = "ag_well_list_by_polygon.txt",
    agwell_locs_file       = "ag_well_summary.txt",
    specwell_locs_file     = NULL,
    specwell_vol_file      = NULL,
    mfrwell_locs_file      = "modflow_cell_to_catchment.txt",
    mfrwell_rates_file     = "monthly_MFR_by_catchment.txt",
    mfrwell_mult_file      = NULL,
    et_zone_cells_file     = "ET_Zone_Cells.txt",
    sfr_jtf_file           = "SFR_network_jtf.txt",
    irr_ditch_file         = "irr_ditch.txt",
    mar_depth_file         = "MAR_depth.txt",
    curtail_frac_file      = "curtailment_fractions.txt",
    et_correction_file     = "field_et_corrections.txt",
    water_mover_file       = NULL,
    print_daily_fields     = list(),
    verbose                = TRUE
) {
  if (verbose) {
    message("Writing SWBM file: ", filename)
  }
  con <- file(file.path(output_dir, filename), "w")
  on.exit(close(con), add = TRUE)

  ## Header
  writeLines("#===============================================================#", con)
  writeLines("#                Soil Water Budget Model (SWBM)                 #", con)
  writeLines("#===============================================================#", con)

  ## DISCRETIZATION
  writeLines("\nBEGIN DISCRETIZATION", con)
  writeLines(sprintf("  NMONTHS           %d", num_stress_periods), con)
  writeLines(sprintf("  WYSTART           %d", WYstart), con)
  writeLines(sprintf("  NPOLY             %d", npoly), con)
  writeLines(sprintf("  NSUBWS            %d", nSubws), con)
  writeLines(sprintf("  NLANDCOVER        %d", nlandcover), con)
  writeLines(sprintf("  NAGWELLS          %d", nAgWells), con)
  writeLines(sprintf("  NSPECWELLS        %d", nSpecWells), con)
  writeLines(sprintf("  NMFRWELLS         %d", nMfrWells), con)
  writeLines("  # MODFLOW INFO", con)
  writeLines(sprintf("  MFNAME            %s", modelName), con)
  writeLines(sprintf("  NROWS             %d", nrows), con)
  writeLines(sprintf("  NCOLS             %d", ncols), con)
  writeLines(sprintf("  NSFR_INFLOW_SEGS  %d", nSFR_inflow_segs), con)
  writeLines("END DISCRETIZATION", con)

  ## OPTIONS
  writeLines("\nBEGIN OPTIONS", con)
  if (daily_sw) {
    writeLines("  DAILY_SW", con)
  }
  writeLines(sprintf("  NEIGHBOR_RULE      %d", neighborRuleYearDay), con)
  writeLines(sprintf("  ABSOLUTE_IRR_DATE  %d %d", absoluteIrrDate[1], absoluteIrrDate[2]), con)
  writeLines("  WRITE_MODFLOW", con)
  if (writeUCODE)  writeLines("  WRITE_UCODE", con)
  if (writePEST)   writeLines("  WRITE_PEST", con)
  writeLines("END OPTIONS", con)

  ## INPUT_FILES
  writeLines("\nBEGIN INPUT_FILES", con)
  writeLines(sprintf("  PRECIP             %s", precip_file), con)
  writeLines(sprintf("  ET                 %s", et_file), con)
  writeLines(sprintf("  ET_EXT_DEPTH       %s", et_ext_depth_file), con)
  writeLines(sprintf("  ETS_TEMPLATE       %s", ets_template_file), con)
  writeLines(sprintf("  KC_FRAC            %s", kc_frac_file), con)
  writeLines(sprintf("  SFR_TEMPLATE       %s", sfr_template_file), con)
  writeLines(sprintf("  SFR_ROUTING        %s", sfr_routing_file), con)
  writeLines(sprintf("  SFR_SEGMENTS       %s", sfr_segments_file), con)
  writeLines(sprintf("  SFR_IRR_FLOWS      %s", sfr_irr_flows_file), con)
  writeLines(sprintf("  SFR_NONIRR_FLOWS   %s", sfr_nonirr_flows_file), con)
  writeLines(sprintf("  WEL_TEMPLATE       %s", wel_template_file), con)
  writeLines(sprintf("  RECHARGE_ZONES     %s", recharge_zones_file), con)
  writeLines(sprintf("  POLY_LANDCOVER     %s", poly_landcover_file), con)
  writeLines(sprintf("  POLY_AGWELL        %s", poly_agwell_file), con)
  writeLines(sprintf("  AGWELL_LOCS        %s", agwell_locs_file), con)

  if (!is.null(specwell_locs_file)) {
    writeLines(sprintf("  SPECWELL_LOCS      %s", specwell_locs_file), con)
  }
  if (!is.null(specwell_vol_file)) {
    writeLines(sprintf("  SPECWELL_VOL       %s", specwell_vol_file), con)
  }
  if (!is.null(mfrwell_locs_file)) {
    writeLines(sprintf("  MFRWELL_LOCS       %s", mfrwell_locs_file), con)
  }
  if (!is.null(mfrwell_rates_file)) {
    writeLines(sprintf("  MFRWELL_RATES      %s", mfrwell_rates_file), con)
  }
  if (!is.null(mfrwell_mult_file)) {
    writeLines(sprintf("  MFRWELL_MULT       %s", mfrwell_mult_file), con)
  }
  if (!is.null(et_zone_cells_file)) {
    writeLines(sprintf("  ET_ZONE_CELLS      %s", et_zone_cells_file), con)
  }
  if (!is.null(sfr_jtf_file)) {
    writeLines(sprintf("  SFR_NETWORK_JTF    %s", sfr_jtf_file), con)
  }
  if (!is.null(irr_ditch_file)) {
    writeLines(sprintf("  IRR_DITCH          %s", irr_ditch_file), con)
  }
  if (!is.null(mar_depth_file)) {
    writeLines(sprintf("  MAR_DEPTH          %s", mar_depth_file), con)
  }
  if (!is.null(curtail_frac_file)) {
    writeLines(sprintf("  CURTAIL_FRAC       %s", curtail_frac_file), con)
  }
  if (!is.null(et_correction_file)) {
    writeLines(sprintf("  ET_CORRECTION      %s", et_correction_file), con)
  }
  if (!is.null(water_mover_file)) {
    writeLines(sprintf("  WATER_MOVER        %s", water_mover_file), con)
  }
  writeLines("END INPUT_FILES", con)

  ## PRINT_DAILY (optional)
  if (length(print_daily_fields) > 0) {
    writeLines("\nBEGIN PRINT_DAILY", con)
    writeLines("# Field ID, filename_prefix", con)
    for (fld in print_daily_fields) {
      writeLines(sprintf("  %4d  %s", fld$id, fld$prefix), con)
    }
    writeLines("END PRINT_DAILY", con)
  }
}

#-------------------------------------------------------------------------------------------------#

#' Write SWBM File Partitioning Subwatershed Surface Flows to Stream Inflows
#'
#' @param sfr_component Dataframe of months and either: 1) subwatershed inflow partition
#' fractions (see \code{\link{gen_monthly_sfr_flow_partition}}), or 2) inflows available
#' for irrigation, or 3) inflows NOT available for irrigation (e.g. reserved for environmental
#' flows) (for items 2 and 3 see \code{\link{process_sfr_inflows}}).
#' @param output_dir directory to write the files to
#' @param filename Writes to this filename
#' @param verbose T/F write status info to console (default: TRUE)
#'
#' @return None, writes file to disk
#' @export
#'
#' @examples
#' \dontrun{
#' subws_inflows <- process_sfr_inflows(scen, "daily_tributary_streamflow.txt")
#' write_SWBM_SFR_inflow_files(subws_inflows$non_irr, '.', "subwatershed_irrigation_inflows.txt")
#' }
write_SWBM_SFR_inflow_files <- function(sfr_component, output_dir, filename, verbose=TRUE) {
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

  write_SWBM_file(sfr_component, output_dir, filename, verbose)
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
#' @return None, writes file to disk
#' @author Claire Kouba, Leland Scantlebury
#' @export
#'
#' @examples
#' write_SWBM_SFR_diversions_file(output_dir='.')

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

#-------------------------------------------------------------------------------------------------#
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
    sps_after_change <- nrow(lu_df[lubridate::year(lu_df$Stress_Period) >= col_yr,])

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
    lu_df[lubridate::year(lu_df$Stress_Period) >= col_yr, names(lu_df) %in% paste0('ID_',change_df$ID)] <- diff
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
  new_code <- update_table['Center Pivot', 'Code']

  #-- Loop over years where changes occur
  for (yr in unique(update_table[update_table$Year>0,]$Year)) {  # when year==0 no data/change
    id_cols <- update_table$ID[update_table$Year == yr]

    if (verbose) {
      message(paste('Year:', yr, '- ', length(id_cols), 'fields switched to center pivot (code =',new_code,')'))
    }

    irrtype_df[lubridate::year(irrtype_df$Stress_Period) >= yr, paste0('ID_', id_cols)] <- new_code
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

#' Create SWBM Landcover Data Frame
#'
#' Builds a stress-period-by-field landcover assignment table for the Soil Water Budget Model (SWBM),
#' according to a chosen land-use scenario.
#'
#' @param scenario_id Character. Scenario identifier; one of `"basecase"`, `"nv_gw_mix"`, or `"nv_all"` (case-insensitive).
#'   - `"basecase"`: start from each polygon's default SWBM_LU code and apply an
#'     alfalfa to grain rotation on alfalfa-coded fields every \code{alfalfa_grain_rotate_years}.
#'   - `"nv_all"`: replace *all* irrigated landcover codes with the native vegetation code.
#'   - `"nv_gw_mix"`: replace codes with native vegetation *only* on fields whose
#'     water source is groundwater (2), mixed (3), or unknown (999).
#' @param start_date Date. Model start date (first stress period).
#' @param end_date   Date. Model end date (last stress period).
#' @param alfalfa_grain_rotate_years Integer. Rotation interval in years for alfalfa fields. Default: `8`.
#' @param polygon_table_file Character. Path to the polygon table file. Must contain at least
#'   columns \code{SWBM_id}, \code{SWBM_LU}, and \code{WATERSOURC}.
#'   Default: \code{file.path(data_dir["time_indep_dir","loc"], "polygons_table.txt")}.
#' @param landcover_desc_file Character. Path to the landcover description file. Must contain
#'   columns \code{id} and \code{Landcover_Name}.
#'   Default: \code{file.path(data_dir["time_indep_dir","loc"], "landcover_table.txt")}.
#'
#' @details
#' 1. Validates \code{scenario_id} against \code{c("basecase","nv_gw_mix","nv_all")}.
#' 2. Reads the polygon table (\code{polygon_table_file}) and landcover descriptions
#'    (\code{landcover_desc_file}). Warns if \code{SWBM_id} values are duplicated or missing.
#' 3. Calls \code{swbm_build_field_value_df(nfields, start_date, end_date)} to create a
#'    wide table with one column per field (named \code{ID_<SWBM_id>}) and a
#'    \code{Stress_Period} column.
#' 4. Scenario logic:
#'    - **basecase**:
#'      * Start with each field's baseline \code{SWBM_LU} from \code{polygon_table_file}.
#'      * For fields with \code{SWBM_LU == 1} (alfalfa), apply an 8-year (by default)
#'        alfalfa to grain rotation.  Field 1 rotates in years 1,9,...; field 2 in years 2,10,... etc.
#'    - **nv_all**:
#'      * Identify all \code{Landcover_Name}s matching `"Irrigated"` and replace their codes
#'        with the native vegetation code.
#'    - **nv_gw_mix**:
#'      * For fields whose water source is groundwater (2), mixed (3), or unknown (999),
#'        set their landcover code to the native vegetation code for the entire period
#'        (Unknown water source fields are automatically assumed to be GW in SWBM).
#'
#' @return A \code{data.frame} with columns
#'   \itemize{
#'     \item \code{Stress_Period} (Date or month-start Date)
#'     \item \code{ID_<SWBM_id>} (integer codes) for each field
#'   }
#'   representing the landcover code at each stress period.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' df1 <- create_SWBM_landcover_df(
#'   scenario_id         = "basecase",
#'   output_dir          = "outputs",
#'   start_date          = as.Date("1991-01-01"),
#'   end_date            = as.Date("1991-12-31"),
#'   alfalfa_grain_rotate_years = 8
#' )
#'
#' df2 <- create_SWBM_landcover_df(
#'   "nv_all", "outputs",
#'   as.Date("2000-01-01"), as.Date("2000-12-31")
#' )
#'
#' df3 <- create_SWBM_landcover_df(
#'   "nv_gw_mix", "/tmp",
#'   as.Date("2005-01-01"), as.Date("2005-12-31")
#' )
#' }
create_SWBM_landcover_df <- function(scenario_id = "basecase",
                                     start_date,
                                     end_date,
                                     poly_df,
                                     landcover_df,
                                     alfalfa_grain_rotate_years = 8
                                     )
{

  recognized_scenarios=c('basecase','nv_gw_mix', 'nv_all')
  if(!(tolower(scenario_id) %in% tolower(recognized_scenarios))){
    stop("Warning: specified landuse scenario not recognized.")
  }

  # Read in landcover, polygon files for reference of
  num_unique_fields = length(unique(poly_df$SWBM_id))

  # Check for polygon ID number continuity
  if(nrow(poly_df) != num_unique_fields){
    message("Caution: polygons (fields) info table contains replicated or missing ID numbers")
  }

  # generate a stress-period-by-field table (wide format)
  field_df = swbm_build_field_value_df(nfields = num_unique_fields,
                                       model_start_date = start_date,
                                       model_end_date = end_date)

  # Build default (time-invarying) land use table
  for(i in 1:num_unique_fields){
    field_id = poly_df$SWBM_id[i]
    # set entire model period to the baseline land use from reference poly_df file
    field_df[,paste0("ID_",field_id)] = poly_df$SWBM_LU[poly_df$SWBM_id==field_id]
  }

  # Write land cover file for scenarios with basecase land use
  # i.e., no major crop changes or native vegetation coverage changes
  if(scenario_id == "basecase"){
    #Set fields equal to unchanging default
    landcover_output = field_df
    # Then, Implement alfalfa-grain rotation
    n_stress = length(seq.Date(from = start_date, to = end_date, by = "month"))
    # Initialize a grain_months dataframe for allocating alfalfa-grain attributes in the for loop
    grain_months = data.frame(month = field_df$Stress_Period)
    grain_months$water_year = lubridate::year(grain_months$month)
    next_wy_indices = lubridate::month(grain_months$month)>9
    grain_months$water_year[next_wy_indices] = 1 + grain_months$water_year[next_wy_indices]
    grain_months$grain_year = grain_months$water_year - lubridate::year(start_date)
    grain_months$alf_1_grain_2 = 1

    # n_rotating = sum(poly_df$SWBM_LU==1) # how many fields are using alfalfa-grain rotation? Assume all alfalfa
    # n_grain = n_rotating / 8 # 1 out of every 8 fields is grain at any given time. 8-yr schedule
    field_rotator = 1 # initialize the eenie-meenie-miney-mo counter
    rot_yrs <- alfalfa_grain_rotate_years
    for(i in 1:num_unique_fields){
      if(poly_df$SWBM_LU[poly_df$SWBM_id==i]==1){ # If it's designated as alfalfa-type landuse
        first_grain_year = field_rotator %% rot_yrs + 1 # designate first year of grain rotation
        # print(first_grain_year)
        grain_years = seq(from = first_grain_year, by = rot_yrs, to = n_stress)
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
  } else if(scenario_id == "nv_all"){

    codes_to_replace = landcover_df$id[grep(pattern = "Irrigated", landcover_df$Landcover_Name)]
    natveg_code = landcover_df$id[grep(pattern = "Native", landcover_df$Landcover_Name)]

    for(i in 1:(ncol(field_df)-1)){
      field_vals = field_df[,i+1]
      field_vals[field_vals %in% codes_to_replace] = natveg_code
      field_df[,i+1] = field_vals
    }

    landcover_output = field_df
  } else if(scenario_id == "nv_gw_mix"){
    field_nv_gw_mix = field_df
    # table indicating which numeric code corresponds to which crop

    natveg_code = landcover_df$id[grep(pattern = "Native", landcover_df$Landcover_Name)]

    # Build new time-invarying fields table
    for(i in 1:num_unique_fields){
      field_id = poly_df$SWBM_id[i]
      if(poly_df$WATERSOURC[i] %in% c(2,3,999)){ # GW or MIX or unkown source (unknown assumed to be GW in SWBM)
        # set entire model period to native vegetation land use
        field_nv_gw_mix[,paste0("ID_",field_id)] = natveg_code
      }
    }
    landcover_output = field_nv_gw_mix
  }
  return(landcover_output)
}

# ------------------------------------------------------------------------------------------------#

#' Write SWBM Polygon Landcover ID File
#'
#' Wrapper around \code{\link{write_SWBM_file}} to save the per-stress-period,
#' per-field landcover ID table for SWBM.
#'
#' @param landcover_df A \code{data.frame} of monthly polygon landcover codes.
#'   Each column should be named \code{"ID_<fieldID>"} and contain the landcover
#'   code for that field in each stress period (row).
#' @param output_dir Character. Directory in which to write the file.
#' @param filename Character. Name of the output file. Default:
#'   \code{"polygon_landcover_ids.txt"}.
#' @param verbose Logical. Whether to print a message before writing. Default:
#'   \code{TRUE}.
#'
#' @return Invisibly returns \code{NULL}. The primary effect is writing
#'   \code{landcover_df} to \code{file.path(output_dir, filename)} in SWBM format.
#'
#' @seealso \code{\link{write_SWBM_file}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Suppose `lc_df` is your polygon_landcover_ids data.frame:
#' write_SWBM_landcover_file(lc_df, output_dir = "model_inputs")
#' }
write_SWBM_landcover_file <- function(landcover_df, output_dir, filename="polygon_landcover_ids.txt", verbose=TRUE) {
  write_SWBM_file(landcover_df,output_dir,filename,verbose)
}

# ------------------------------------------------------------------------------------------------#
#' Write SWBM Polygon Landcover Description Table File
#'
#' Outputs the polygon landcover description table with fixed-width, column-aligned formatting,
#' ready for ingestion by the Soil Water Budget Model (SWBM). Each column is padded
#' to the maximum width needed (based on the header and all data rows), ensuring
#' human-readable alignment in the text file.
#'
#' @param landcover_desc A \code{data.frame} whose columns describe polygon landcover properties,
#'   for example:
#'   \itemize{
#'     \item \code{id} - Numeric or character landcover identifier.
#'     \item \code{Landcover_Name} - Landcover type name.
#'     \item \code{IrrFlag}, \code{ET_Flag} - Logical flags for irrigation and ET.
#'     \item \code{IrrSWC}, \code{RootDepth}, \code{RD_Mult}, \code{Kc_Mult} - Numeric parameters.
#'     \item \code{IrrMonStart}, \code{IrrDayStart}, \code{IrrMonEnd}, \code{IrrDayEnd} - Integer dates.
#'     \item \code{IrrEff_Flood}, \code{IrrEff_WL}, \code{IrrEff_CP} - Irrigation efficiencies.
#'   }
#'   All columns will be formatted to a fixed width based on their longest entry.
#' @param output_dir Character. Directory in which to write the output file.
#' @param filename Character. Name of the output file. Defaults to \code{"landcover_table.txt"}.
#' @param verbose Logical. If \code{TRUE}, prints a message indicating the full path of the file written.
#'
#' @details
#' Unlike standard space-delimited writes, this function:
#' \enumerate{
#'   \item Computes, for each column, the maximum number of characters needed (header vs. data).
#'   \item Formats every cell in that column to that width (padding with spaces).
#'   \item Joins columns with a single space, preserving neat vertical alignment.
#' }
#'
#' @return Invisibly returns \code{NULL}. The aligned landcover table is written
#'   to \code{file.path(output_dir, filename)}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' write_SWBM_landcover_desc_file(
#'   landcover_desc = lc_desc,
#'   output_dir     = "SWBM_inputs",
#'   filename       = "landcover_table_aligned.txt",
#'   verbose        = TRUE
#' )
#' }
write_SWBM_landcover_desc_file <- function(landcover_desc,
                                              output_dir,
                                              filename = "landcover_table.txt",
                                              verbose  = TRUE) {
  col_names <- names(landcover_desc)
  as_chr    <- function(x) if (is.factor(x)) as.character(x) else as.character(x)
  widths <- vapply(col_names, function(nm) {
    data_vals <- as_chr(landcover_desc[[nm]])
    max(nchar(c(nm, data_vals)), na.rm = TRUE)
  }, integer(1))

  # Header
  header_cells <- mapply(format, col_names, widths, MoreArgs = list(justify = "left"), SIMPLIFY = FALSE)
  header_line  <- paste0(unlist(header_cells), collapse = " ")

  # Data rows
  data_lines <- apply(landcover_desc, 1, function(row) {
    formatted <- mapply(function(cell, w) {
      format(as_chr(cell), width = w, justify = "right")
    }, row, widths, SIMPLIFY = FALSE)
    paste0(unlist(formatted), collapse = " ")
  })

  out_path <- file.path(output_dir, filename)
  if (verbose) message("Writing landcover table to: ", out_path)
  writeLines(c(header_line, data_lines), con = out_path)

  invisible(NULL)
}

# ------------------------------------------------------------------------------------------------#

#' Read SWBM ET-From-GW Matrices
#'
#' Reads the two SWBM input matrices used for ET-from-groundwater:
#' - a binary mask of active ET-from-GW cells,
#' - the per-cell extinction depths.
#'
#' @param file_cells Character. Path to the ET-zone-cells file
#'   (e.g. "ET_Zone_Cells.txt").
#' @param file_ext_depth Character. Path to the ET-cells-extinction-depth file
#'   (e.g. "ET_Cells_Extinction_Depth.txt").
#' @param header Logical. Does the file have a header row? Default: \code{FALSE}.
#' @param comment.char Character. Comment character to skip lines. Default: \code{"!"}.
#' @param ... Additional arguments passed to \code{read.table()}.
#'
#' @return A \code{list} with components:
#' \describe{
#'   \item{\code{ET_cells}}{Numeric matrix; 0/1 mask of ET-from-GW activity.}
#'   \item{\code{ET_ext_depth}}{Numeric matrix; per-cell extinction depths.}
#' }
#' @export
#' @examples
#' \dontrun{
#' inputs <- read_SWBM_ET_inputs(
#'   file_cells     = "ET_Zone_Cells.txt",
#'   file_ext_depth = "ET_Cells_Extinction_Depth.txt"
#' )
#' str(inputs$ET_cells)
#' str(inputs$ET_ext_depth)
#' }
read_SWBM_ET_inputs <- function(file_cells,
                                file_ext_depth,
                                header      = FALSE,
                                comment.char = "!",
                                ...) {
  # Read the binary ET-zone mask
  ET_cells <- as.matrix(read.table(file= file_cells, header= FALSE,comment.char = comment.char, ...))
  # Read the extinction depth matrix
  ET_ext_depth <- as.matrix(read.table(file= file_ext_depth,header= FALSE,comment.char = comment.char, ...))
  return(list(ET_cells= ET_cells, ET_ext_depth = ET_ext_depth))

}

# ------------------------------------------------------------------------------------------------#

#' Write SWBM ET-From-GW Matrices
#'
#' Writes the ET-from-groundwater input matrices to disk:
#' - a binary ET-zone mask,
#' - the extinction depth matrix.
#'
#' @param et_list A \code{list} with components \code{ET_cells} and \code{ET_ext_depth},
#'   as returned by \code{read_SWBM_ET_inputs()}.
#' @param output_dir Character. Directory in which to write the files.
#' @param file_cells Character. Filename for the ET-zone-cells file.
#'   Default: \code{"ET_Zone_Cells.txt"}.
#' @param file_ext_depth Character. Filename for the ET-cells-extinction-depth file.
#'   Default: \code{"ET_Cells_Extinction_Depth.txt"}.
#' @param sep Character. Field separator. Default: \code{"\t"}.
#' @param quote Logical. Whether to quote character fields. Default: \code{FALSE}.
#' @param row.names Logical or character. Passed to \code{write.table()}. Default: \code{FALSE}.
#' @param col.names Logical. Passed to \code{write.table()}. Default: \code{FALSE}.
#' @param verbose Logical. If \code{TRUE}, prints a message before writing. Default: \code{TRUE}.
#'
#' @return Invisibly returns \code{NULL}. Side effect: writes the two files.
#' @export
#' @examples
#' \dontrun{
#' inputs <- read_SWBM_ET_inputs("ET_Zone_Cells.txt", "ET_Cells_Extinction_Depth.txt")
#' write_SWBM_ET_inputs(
#'   et_list    = inputs,
#'   output_dir = "model_outputs"
#' )
#' }
write_SWBM_ET_inputs <- function(et_list,
                                 output_dir,
                                 file_cells       = "ET_Zone_Cells.txt",
                                 file_ext_depth   = "ET_Cells_Extinction_Depth.txt",
                                 sep              = "\t",
                                 quote            = FALSE,
                                 row.names        = FALSE,
                                 col.names        = FALSE,
                                 verbose          = TRUE) {
  if (verbose) {
    message("Writing ET-zone mask to: ",
            file.path(output_dir, file_cells))
    message("Writing ET extinction depths to: ",
            file.path(output_dir, file_ext_depth))
  }
  # Write ET zone mask
  write.table(
    et_list$ET_cells,
    file      = file.path(output_dir, file_cells),
    sep       = sep,
    quote     = quote,
    row.names = row.names,
    col.names = col.names
  )
  # Write extinction depth matrix
  write.table(
    et_list$ET_ext_depth,
    file      = file.path(output_dir, file_ext_depth),
    sep       = sep,
    quote     = quote,
    row.names = row.names,
    col.names = col.names
  )
  invisible(NULL)
}

# ------------------------------------------------------------------------------------------------#
#' Apply Native-Vegetation ET-from-Groundwater Override
#'
#' For any field that is native vegetation at least a given fraction of the
#' simulation, activates ET-from-groundwater and sets a scenario-specific
#' extinction depth in that field's cells.  Cells in the "discharge zone"
#' (where ET_from_GW was already active) keep their original depth.
#'
#' @param et_list A list with two numeric matrices:
#'   \describe{
#'     \item{\code{ET_cells}}{0/1 mask of cells where ET from GW is active}
#'     \item{\code{ET_ext_depth}}{per-cell extinction depths (m)}
#'   }
#'   As returned by \code{\link{read_SWBM_ET_inputs}}.
#' @param rech_cell_map Integer matrix mapping each model cell to a field ID
#'   (as in \code{recharge_zones.txt}).
#' @param landcover_df Data.frame of monthly field IDs, with columns
#'   \code{ID_<fieldID>}.  Each entry is the landcover code for that field
#'   in that month.
#' @param landcover_desc Data.frame with columns \code{id} and \code{Landcover_Name},
#'   used to find the numeric code for "Native" vegetation.
#' @param natveg_ExtD Numeric. The new extinction depth (m) to assign
#'   in cells of predominantly native vegetation.
#' @param fraction_cutoff Numeric in (0,1].  Fields whose native-vegetation code
#'   occurs in at least this fraction of months are treated as "native-veg fields."
#'   Default: 0.5.
#'
#' @return A list with the same structure as \code{et_list}, but with
#'   \code{ET_cells} and \code{ET_ext_depth} modified:
#'   \itemize{
#'     \item Cells in a native-vegetation field have \code{ET_cells=1}.
#'     \item Cells where \code{ET_cells} was 0 and belong to such a field
#'       get \code{ET_ext_depth = natveg_ExtD}.
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' et_inputs <- read_SWBM_ET_inputs("ET_Zone_Cells.txt",
#'                                  "ET_Cells_Extinction_Depth.txt")
#' rech_cell_map <- as.matrix(
#'   read.table("recharge_zones.txt", header = FALSE)
#' )
#' landcov_tbl <- read.table("polygon_landcover_ids.txt", header = TRUE)
#' landcov_desc <- read.table("landcover_table.txt", header = TRUE)
#'
#' updated <- apply_native_veg_ET_override(
#'   et_list               = et_inputs,
#'   rech_cell_map         = rech_cell_map,
#'   landcover_df        = landcov_tbl,
#'   landcover_desc        = landcov_desc,
#'   natveg_ExtD           = 3.05,
#'   fraction_cutoff        = 0.5
#' )
#' write_SWBM_ET_inputs(updated, output_dir = "model_outputs")
#' }
apply_native_veg_ET_override <- function(et_list,
                                         rech_cell_map,
                                         landcover_df,
                                         landcover_desc,
                                         natveg_ExtD,
                                         fraction_cutoff = 0.5) {
  # find native-vegetation code
  natveg_code <- landcover_desc$id[grep("Native", landcover_desc$Landcover_Name)]
  # identify field columns (ID_<fieldID>)
  field_cols <- grep("^ID_", names(landcover_df), value = TRUE)
  # compute fraction of months native-veg per field
  frac_natveg <- vapply(landcover_df[field_cols], function(col) mean(col == natveg_code),numeric(1)
  )
  # fields meeting or exceeding cutoff
  natveg_cols <- field_cols[frac_natveg >= fraction_cutoff]
  # extract numeric field IDs
  natveg_field_ids <- as.integer(sub("^ID_", "", natveg_cols))

  # copy matrices
  ET_cells     <- et_list$ET_cells
  ET_ext_depth <- et_list$ET_ext_depth

  # mask of cells belonging to native-veg fields
  mask <- rech_cell_map %in% natveg_field_ids

  # update extinction depth where ET was originally off
  ET_ext_depth[mask & ET_cells == 0] <- natveg_ExtD
  # activate ET from GW in all native-veg cells
  ET_cells[mask] <- 1

  # return updated list
  list(
    ET_cells     = ET_cells,
    ET_ext_depth = ET_ext_depth
  )
}

# ------------------------------------------------------------------------------------------------#

#' Create Managed Aquifer Recharge Depth Table
#'
#' Builds a stress-period-by-field table of managed-aquifer recharge (MAR) depths(in m)
#' by starting from a default zero-MAR SWBM field table and then inserting
#' 2024 MAR depths from a pre-computed CSV for the chosen scenario.
#'
#' @param start_date Date. Model start date for the first stress period.
#' @param end_date   Date. Model end date for the last stress period.
#' @param mar_scenario Character. Which 2024 MAR scenario to use:
#'   \describe{
#'     \item{\code{"basecase"}}{Historical MAR volumes for WY2024, from \code{MAR_24basecase.csv}.}
#'     \item{\code{"max24"}}{Maximum-diversion MAR volumes for WY2024, from \code{MAR_24max.csv}.}
#'     \item{\code{"none"}}{Returns a MAR table of 0.0 for every stress period.}
#'   }
#'
#' @return A \code{data.frame} whose first column is \code{Stress_Period} (Date)
#'   and whose remaining columns are named \code{ID_<fieldID>} with numeric MAR
#'   depth values (m) for each field in each stress period.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a MAR depth table for the historical 2024 scenario:
#' mar_df <- create_MAR_depth_df(
#'   start_date   = as.Date("2023-10-01"),
#'   end_date     = as.Date("2024-09-30"),
#'   mar_scenario = "basecase"
#' )
#' head(mar_df)
#' }
create_MAR_depth_df <- function(start_date, end_date, mar_scenario) {
  # Create blank field df
  mar_df <- swbm_build_field_value_df(model_start_date = start_date,
                                        model_end_date = end_date,
                                        default_values = 0)

  if (mar_scenario=='basecase') {
    # Read in 2024 MAR (see .\Scripts\Stored_Analyses\2024_MAR.R)
    mar24 <- read.table(file.path(data_dir["ref_data_dir","loc"], 'MAR_24basecase.csv'), header=T)
    mar24$Stress_Period <- as.Date(mar24$Stress_Period)

    # Remove rows in mar_df that match the Stress_Periods, then combine
    mar_df <- mar_df[!mar_df$Stress_Period %in% c(mar24$Stress_Period),]
    mar_df <- rbind(mar_df, mar24)
  } else if (mar_scenario=='max24') {
    # Read in 2024 MAR (see .\Scripts\Stored_Analyses\2024_MAR.R)
    mar24 <- read.table(file.path(data_dir["ref_data_dir","loc"], 'MAR_24max.csv'), header=T)
    mar24$Stress_Period <- as.Date(mar24$Stress_Period)

    # Remove rows in mar_df that match the Stress_Periods, then combine
    mar_df <- mar_df[!mar_df$Stress_Period %in% c(mar24$Stress_Period), ]
    mar_df <- rbind(mar_df, mar24)
  } else if (mar_scenario=='none') {
    # Do nada
  } else {
    stop(paste('Unrecognized scenario:',mar_scenario,'. Must be one of "basecase", "max24", "none"'))
  }
  # Ensure the result is sorted by Stress_Period
  mar_df <- mar_df[order(mar_df$Stress_Period), ]

  return(mar_df)

}

# ------------------------------------------------------------------------------------------------#

#' Write SWBM MAR Depth Table to SWBM Input File
#'
#' Writes a managed-aquifer recharge (MAR) depth (m) table to disk in SWBM input format.
#'
#' @param mar_df      A \code{data.frame} as returned by \code{create_MAR_depth_df()},
#'                    with \code{Stress_Period} and \code{ID_<fieldID>} columns.
#' @param output_dir  Character. Directory in which to write the file.
#' @param filename    Character. Name of the output file. Default: \code{"MAR_depth.txt"}.
#' @param verbose     Logical. If \code{TRUE}, prints a message before writing. Default: \code{TRUE}.
#'
#' @return Invisibly returns \code{NULL}.  Side effect: writes \code{mar_df} to
#'   \code{file.path(output_dir, filename)} using space-delimited format.
#'
#' @seealso \code{\link{create_MAR_depth_df}}
#' @export
#'
#' @examples
#' \dontrun{
#' df <- create_MAR_depth_df(
#'   start_date   = as.Date("2023-10-01"),
#'   end_date     = as.Date("2024-09-30"),
#'   mar_scenario = "max24"
#' )
#' write_SWBM_MAR_depth_file(df, output_dir = "SWBM_inputs")
#' }
write_SWBM_MAR_depth_file <- function(mar_df, output_dir, filename = "MAR_depth.txt", verbose  = TRUE) {
  write_SWBM_file(df = mar_df, output_dir = output_dir, filename = filename, verbose = FALSE)
}

# ------------------------------------------------------------------------------------------------#
#' Create SWBM Curtailment Fractions File for SWBM
#'
#' Generates DataFrame water curtailment fractions for each SWBM agricultural
#' field for each stress period in the simulation. Fields without curtailments are assigned a default value of 0.0.
#'
#' @param start_date Start date of the simulation (as a Date object).
#' @param end_date End date of the simulation (as a Date object).
#' @param scenario_id The curtailment scenario (anything but `basecase` returns a dataframe of zeros)
#'
#' @return A \code{data.frame} whose first column is \code{Stress_Period} (Date)
#'   and whose remaining columns are named \code{ID_<fieldID>} with Curtailment fractions
#'   for each field in each stress period.
#'
#' @details
#' The curtailment fraction file is used in the SWBM model to specify reductions in applied irrigation water
#' due to local cooperative solutions (LCS) and regulatory curtailments. The fraction represents the proportion
#' of normal applied water that is reduced, with values typically ranging from 0.0 (no curtailment) to 1.0
#' (full curtailment).
#'
#' When scenario_id is basecase, the function reads in the stored curtailment data for different
#' years (2021, 2022, 2023, and 2024), replaces overlapping dates, and ensures the final dataset is sorted
#' before saving to file.
#'
#' Any other scenario_id results in a \code{data.frame} of 0.0 for every field/month.
#'
#' @export
#'
#' @examples
#' create_SWBM_curtailment_df(start_date = as.Date("2020-10-01"),
#'                            end_date = as.Date("2024-09-30"),
#'                            scenario_id = "basecase")
create_SWBM_curtailment_df <- function(start_date, end_date, scenario_id) {

  # generate a stress-period-by-field table (wide format)  for saving to output
  curtail_output <- swbm_build_field_value_df(model_start_date = start_date,
                                              model_end_date = end_date,
                                              default_values = 0)

  no_curtail_scenarios = c("natveg_all_lowET", "natveg_all_highET", "curtail_00_pct_all_years")
  basecase_curtail_scenarios = c("basecase", "basecase_noMAR", "maxMAR2024")

  if(scenario_id == "basecase"){
    # Read in 2021/2022 LCS/curtailments (see SVIHM/Scripts/Stored_Analyses/2021_2022_LCS_Curtailments.R)
    curt21_22 <- read.csv(file.path(data_dir["ref_data_dir","loc"], 'Curtail_21_22.csv'))
    curt21_22$Stress_Period <- as.Date(curt21_22$Stress_Period)

    # Read in 2023 LCS/curtailment
    curt23 <- read.csv(file.path(data_dir["ref_data_dir","loc"], 'Curtail_23.csv'))
    curt23$Stress_Period <- as.Date(curt23$Stress_Period)

    # Read in 2024 LCS/curtailment
    curt24 <- read.csv(file.path(data_dir["ref_data_dir","loc"], 'Curtail_24.csv'))
    curt24$Stress_Period <- as.Date(curt24$Stress_Period)

    # Remove rows in curtail_output that match the Stress_Periods, then combine
    curtail_output <- curtail_output[!curtail_output$Stress_Period %in% c(curt21_22$Stress_Period, curt23$Stress_Period, curt24$Stress_Period), ]
    curtail_output <- rbind(curtail_output, curt21_22, curt23, curt24)
  }

  # Ensure the result is sorted by Stress_Period
  curtail_output <- curtail_output[order(curtail_output$Stress_Period), ]

  return(curtail_output)
}

# ------------------------------------------------------------------------------------------------#

#' Write SWBM Curtailment Fractions File
#'
#' Writes a curtailment fractions table to disk in SWBM input format.  The
#' table should come from \code{\link{create_SWBM_curtailment_df}()} and
#' contain \code{Stress_Period} and \code{ID_<fieldID>} columns.
#'
#' @param curtail_df A \code{data.frame} as returned by
#'   \code{create_SWBM_curtailment_df()}, with \code{Stress_Period} (Date)
#'   plus \code{ID_<fieldID>} numeric columns.
#' @param output_dir Character. Directory in which to write the file.
#' @param filename   Character. Name of the output file. Default:
#'   \code{"curtailment_fractions.txt"}.
#' @param verbose    Logical. If \code{TRUE}, prints a message before writing.
#'   Default: \code{TRUE}.
#'
#' @return Invisibly returns \code{NULL}.  Side effect: writes
#'   \code{curtail_df} to \code{file.path(output_dir, filename)} as a
#'   space-delimited text file with header.
#'
#' @seealso \code{\link{create_SWBM_curtailment_df}}, \code{\link{write_SWBM_file}}
#' @export
#'
#' @examples
#' \dontrun{
#' df <- create_SWBM_curtailment_df(
#'   start_date   = as.Date("2020-10-01"),
#'   end_date     = as.Date("2024-09-30"),
#'   scenario_id  = "basecase"
#' )
#' write_SWBM_curtailment_file(df, output_dir = "SWBM_inputs")
#' }
write_SWBM_curtailment_file <- function(curtail_df, output_dir, filename = "curtailment_fractions.txt", verbose = TRUE) {
  write_SWBM_file(df = curtail_df, output_dir = output_dir, filename = filename, verbose)
}

# ------------------------------------------------------------------------------------------------#
#' Create SWBM ET Correction Data Frame
#'
#' Generates a stress-period-by-field table of evapotranspiration (ET) correction
#' factors for SWBM.  By default, all factors are 1.0 (no change).  If
#' \code{scenario_id == "basecase"}, precomputed WY2024 correction factors are
#' loaded from \code{ETcor_24.csv}.
#'
#' @param start_date Date; model start date (first stress period).
#' @param end_date   Date; model end date (last stress period).
#' @param scenario_id Character; ET-correction scenario.  Use \code{"basecase"} to
#'   apply stored WY2024 corrections, otherwise returns all 1.0.
#'
#' @return A \code{data.frame} with first column \code{Stress_Period} (Date) and
#'   subsequent columns \code{ID_<fieldID>} giving the daily ET correction factor
#'   for each field and stress period.
#'
#' @details
#' The ET correction table is used by SWBM to adjust daily evapotranspiration
#' based on best management practices (BMPs) in WY2024.  When
#' \code{scenario_id == "basecase"}, factors are read from
#' \code{ETcor_24.csv}; otherwise, all fields remain at the default factor of 1.0.
#'
#' @export
#' @examples
#' \dontrun{
#' df <- create_SWBM_ET_correction_df(
#'   start_date  = as.Date("2023-10-01"),
#'   end_date    = as.Date("2024-09-30"),
#'   scenario_id = "basecase"
#' )
#' }
create_SWBM_ET_correction_df <- function(start_date, end_date, scenario_id) {
  m2_to_acres = 1/4046.856

  # generate a stress-period-by-field table (wide format)  for saving to output
  et_cor_output <- swbm_build_field_value_df(model_start_date = start_date,
                                             model_end_date = end_date,
                                             default_values = 1.0)

  if (scenario_id=='basecase') {
    # Read in 2024 LCS
    et_cor24 <- read.csv(file.path(data_dir["ref_data_dir","loc"], 'ETcor_24.csv'))
    et_cor24$Stress_Period <- as.Date(et_cor24$Stress_Period)

    # Remove rows in et_cor_output that match the Stress_Periods, then combine
    et_cor_output <- et_cor_output[!et_cor_output$Stress_Period %in% c(et_cor24$Stress_Period), ]
    et_cor_output <- rbind(et_cor_output, et_cor24)
  }

  # Ensure the result is sorted by Stress_Period
  et_cor_output <- et_cor_output[order(et_cor_output$Stress_Period), ]

  return(et_cor_output)
}

# ------------------------------------------------------------------------------------------------#

#' Write SWBM ET Correction File
#'
#' Writes an ET correction factors table to a space-delimited SWBM input file.
#'
#' @param et_cor_df  A \code{data.frame} as returned by
#'   \code{\link{create_SWBM_ET_correction_df}()}, with \code{Stress_Period}
#'   and \code{ID_<fieldID>} columns.
#' @param output_dir Character. Directory where the file will be written.
#' @param filename   Character. Name of the output file. Default:
#'   \code{"ETcorrection_factors.txt"}.
#' @param verbose    Logical. If \code{TRUE}, prints a message before writing.
#'   Default: \code{TRUE}.
#'
#' @return Invisibly returns \code{NULL}.  Side effect: writes
#'   \code{et_cor_df} to \code{file.path(output_dir, filename)}.
#'
#' @seealso \code{\link{create_SWBM_ET_correction_df}}, \code{\link{write_SWBM_file}}
#' @export
#' @examples
#' \dontrun{
#' df <- create_SWBM_ET_correction_df(
#'   start_date  = as.Date("2023-10-01"),
#'   end_date    = as.Date("2024-09-30"),
#'   scenario_id = "basecase"
#' )
#' write_SWBM_ET_correction_file(df, output_dir = "SWBM_inputs")
#' }
write_SWBM_ET_correction_file <- function(et_cor_output, output_dir, filename = "field_et_corrections.txt", verbose = TRUE) {
  write_SWBM_file(df = et_cor_output, output_dir = output_dir, filename = filename, verbose)
}

# ------------------------------------------------------------------------------------------------#

#' Recode Polygon Water Sources to Remove Pumping
#'
#' Converts groundwater and mixed water-source codes to non-pumping equivalents,
#' and optionally recodes unknown sources to dry-farmed.  After recoding, updates
#' any associated text labels via \code{\link{translate_SWBM_codes}}.
#'
#' @param polygon_df A \code{data.frame} containing at least the column
#'   \code{WATERSOURC} (numeric codes).
#' @param include_unknown Logical; if \code{TRUE} (default), recode
#'   unknown sources (code 999) to dry-farmed (code 5).  If \code{FALSE},
#'   leave code 999 unchanged.
#'
#' @details
#' SWBM water-source codes:
#' \tabular{rl}{
#'   1   \tab Surface water\cr
#'   2   \tab Groundwater\cr
#'   3   \tab Mixed surface/groundwater\cr
#'   4   \tab Sub-irrigated\cr
#'   5   \tab Dry-farmed (no pumping)\cr
#'   999 \tab Unknown (assumed groundwater)\cr
#' }
#'
#' This function applies the following recoding to \code{WATERSOURC}:
#' \itemize{
#'   \item Groundwater (2) to Dry-farmed (5)
#'   \item Mixed (3)      to Surface water (1)
#'   \item Unknown (999)  to Dry-farmed (5), if \code{include_unknown = TRUE}
#' }
#'
#' After recoding the numeric codes, the function calls
#' \code{\link{translate_SWBM_codes}} to regenerate any associated
#' \code{*_txt} label columns.
#'
#' @return A modified copy of \code{polygon_df} with updated
#'   \code{WATERSOURC} codes (and their text labels).
#'
#' @export
#' @examples
#' \dontrun{
#' polygon_fields <- read_SWBM_polygon_file(scen$polygon_file, landcover_desc, tributary_desc)
#' polygon_fields <- SWBM_no_pumping(polygon_fields)
#' }
SWBM_no_pumping <- function(polygon_df, include_unknown = TRUE) {
  # convert water sources: GW to DRY, MIX to SW
  # WATERSOURC data: 1 = surface water; 2 = groundwater; 3 = mix
  # 4 = subirrigated; 5 = dry-farmed; 999 = unknown (assumed GW)
  polygon_df$WATERSOURC[polygon_df$WATERSOURC == 2] <- 5
  polygon_df$WATERSOURC[polygon_df$WATERSOURC == 3] <- 1
  if (include_unknown) {
    polygon_df$WATERSOURC[polygon_df$WATERSOURC == 999] <- 5
  }
  # Update any *_txt label columns
  polygon_df <- translate_SWBM_codes(polygon_df)
  return(polygon_df)
}


# ------------------------------------------------------------------------------------------------#
#' Add Monthly Curtailments to an Existing SWBM Curtailment Data Frame
#'
#' Updates a \code{curtailment_df} by applying user-specified curtailments over a date range.
#' You can choose to either replace the existing curtailment fractions, or add to them.
#'
#' Three mutually-exclusive modes are supported:
#' \enumerate{
#'   \item \strong{Global}: apply \code{percent} to *every* field.
#'   \item \strong{Field list}: apply (vectorised) \code{percent} to the specified
#'         \code{field_list}.
#'   \item \strong{Spatial}: intersect a polygon layer (\code{field_shp}) with the
#'         SWBM field shapefile; per-field curtailment is
#'         \code{overlap_fraction * percent_field}.
#' }
#' *Imporantly, the function does not handle temporal fractions.* The start/end dates solely are used
#' to select the appropriate stress periods. If your curtailment, for example, should only be applied
#' 1/2 the stress period (month) simply adjust your `percent` for that month to 0.5. This may mean
#' you will need more than one call to `SWBM_monthly_curtailment` to implement a curtailment
#' schedule that spans multiple months, but not in their entirety.
#'
#' @param curtailment_df A wide data.frame produced by
#'   \code{create_SWBM_curtailment_df()}. First column must be
#'   \code{Stress_Period}; remaining columns are \code{ID_<fieldID>}.
#' @param date_start,date_end \code{Date}. Inclusive range of stress periods
#'   (months) to which the curtailment is applied.
#' @param percent Numeric (0-1).  Scalar or vector of curtailment fractions.
#'   Interpreted as "fraction of *normal* irrigation that is curtailed."
#' @param field_list Optional character or numeric vector of field IDs
#'   (without the "ID_" prefix).  If supplied, curtailment is limited to these fields.
#' @param field_shp Optional \code{sf} object or path to polygon shapefile/GeoPackage defining
#'   areas to be curtailed.  If present, \code{field_list} is ignored.
#'   \itemize{
#'     \item If \code{field_shp} has a column named \code{percent}, those values
#'           are used per polygon.  Otherwise the scalar \code{percent} is used.
#'     \item The function reprojects \code{field_shp} to the CRS of \code{swbm_fields}.
#'   }
#' @param swbm_fields \code{sf} layer or path to SWBM fields (default
#'   \code{Landuse_20190219.shp} from \code{data_dir}).  Used only in spatial mode.
#' @param min_overlap_pct Numeric.  Polygons covering less than this fraction of a field
#'   are ignored (spatial mode only).  Default \code{0.05}.
#' @param additive Logical; if \code{TRUE}, *adds* \code{percent} to existing values
#'   (capped at 1.0); if \code{FALSE} (default), *replaces* existing values.
#' @param verbose Logical; print progress messages? Default \code{TRUE}.
#'
#' @return The updated \code{curtailment_df}.
#' @export
#'
#' @examples
#' # Get polygons (requires landcover_desc, tributary_desc), empty curtail_df
#' landcover_desc <- read.table(scen$landcover_desc_file, header=T)
#' tributary_desc <- read_inflow_seg_file(scen$inflow_seg_file)
#' polygon_fields <- read_SWBM_polygon_file(scen$polygon_file, landcover_desc, tributary_desc)
#' curtail_df <- create_SWBM_curtailment_df(scen$start_date, scen$end_date, scenario_id='none')
#'
#' # All fields
#' curtail_df <- SWBM_monthly_curtailment(curtail_df, '2025-03-01', '2025-03-31', percent = .6)
#' plot_curtailment(curtail_df, stress_period="2025-03-01")
#'
#' # By field, 10 largest
#' biggest_fields <- polygon_fields[order(polygon_fields$MF_Area_m2, decreasing = T),]
#' biggest_fields <- biggest_fields[biggest_fields$SWBM_LU %in% c(1,2,3),'SWBM_id'][1:10]
#' curtail_df <- SWBM_monthly_curtailment(curtail_df, '2025-04-01', '2025-04-30', percent = 1, biggest_fields)
#' plot_curtailment(curtail_df, stress_period="2025-04-01")
#'
#' # Using a shapefile
#' curt_shp_file <- file.path(data_dir["ref_data_dir","loc"], '2024_LCS_Fields_Clean.shp')
#' curtail_df <- SWBM_monthly_curtailment(curtail_df, '2025-05-01', '2025-05-31', percent = 1, field_shp = curt_shp_file)
#' plot_curtailment(curtail_df, stress_period="2025-05-01")
#'
SWBM_monthly_curtailment <- function(curtailment_df,
                                     date_start,
                                     date_end,
                                     percent,
                                     field_list      = NULL,
                                     field_shp       = NULL,
                                     swbm_fields     = file.path(data_dir["ref_data_dir","loc"],
                                                                 "Landuse_20190219.shp"),
                                     min_overlap_pct = 0.05,
                                     additive        = FALSE,
                                     verbose         = TRUE) {

  stopifnot("Stress_Period" %in% names(curtailment_df))
  date_start <- as.Date(date_start)
  date_end   <- as.Date(date_end)
  if (date_start > date_end)
    stop("date_start must be <= date_end")

  rows <- which(curtailment_df$Stress_Period >= date_start &
                  curtailment_df$Stress_Period <= date_end)
  if (!length(rows)) stop("No stress periods in the requested date range")

  field_cols <- names(curtailment_df)[-1]
  field_ids  <- sub("^ID_", "", field_cols)

  ## SPATIAL MODE
  if (!is.null(field_shp)) {
    if (verbose) message("Applying spatial curtailment.")
    if (is.character(field_shp))      field_shp   <- sf::read_sf(field_shp)
    if (is.character(swbm_fields))    swbm_fields <- sf::read_sf(swbm_fields)
    field_shp <- sf::st_transform(field_shp, sf::st_crs(swbm_fields))
    overlaps <- sf::st_intersection(field_shp, swbm_fields)
    overlaps$overlap_fraction <- as.numeric(
      sf::st_area(overlaps) /
        sf::st_area(swbm_fields[match(overlaps$Polynmbr,
                                      swbm_fields$Polynmbr), ])
    )
    overlaps <- overlaps[overlaps$overlap_fraction >= min_overlap_pct, ]
    overlaps$curt_pct <- if ("percent" %in% names(overlaps)) overlaps$percent else percent
    per_field <- overlaps |>
      dplyr::group_by(Polynmbr) |>
      dplyr::summarise(curtailed = sum(overlap_fraction * curt_pct, na.rm = TRUE),
                       .groups = "drop")
    idx         <- match(per_field$Polynmbr, field_ids)
    target_cols <- field_cols[idx[!is.na(idx)]]
    target_pcts <- per_field$curtailed[!is.na(idx)]

    ## FIELD-LIST MODE
  } else if (!is.null(field_list)) {
    if (verbose) message("Applying field-specific curtailment.")
    if (length(percent)==1L) percent <- rep(percent, length(field_list))
    if (length(percent)!=length(field_list))
      stop("Length of percent must be 1 or length(field_list)")
    target_cols <- paste0("ID_", field_list)
    missing     <- setdiff(target_cols, field_cols)
    if (length(missing))
      stop("Field(s) not found: ", paste(sub("^ID_", "", missing), collapse=","))
    target_pcts <- percent

    ## GLOBAL MODE
  } else {
    if (verbose) message("Applying global curtailment.")
    if (length(percent)!=1L)
      stop("In global mode, percent must be length 1")
    target_cols <- field_cols
    target_pcts <- rep(percent, length(target_cols))
  }

  ## APPLY: replace or add?
  for (k in seq_along(target_cols)) {
    col <- target_cols[k]; pct <- target_pcts[k]
    if (additive) {
      curtailment_df[rows, col] <- pmin(curtailment_df[rows, col] + pct, 1.0)
    } else {
      curtailment_df[rows, col] <- pct
    }
  }

  if (verbose) {
    mode <- if (additive) "added to" else "set for"
    message("Curtailment ", mode, " ",
            length(target_cols), " fields over ",
            length(rows), " stress period(s).")
  }

  curtailment_df
}

# ------------------------------------------------------------------------------------------------#

#' Write SWBM SFR Template File
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
#' \dontrun{
#' write_SWBM_sfr_template_file(nsteps = 412, output_dir = file.path(data_dir['scenario_dir','loc'],'apocalypse_scen'))
#' }
write_SWBM_sfr_template_file <- function(nsteps, output_dir, filename='SFR_network.txt', daily=TRUE,
                                   ntabfiles=30,
                                   comment='SFR Package written by RSVP') {
  f <- file.path(output_dir, filename)
  write(paste('#', comment, ifelse(daily, '- tabfile version -','-'), format(lubridate::now(), '%Y-%m-%d')), f, append=F)
  if (daily) {
    write(paste('TABFILES', ntabfiles, nsteps), f, append=T)
  }

  # From here it's just copy paste
  sfr <- readLines(file.path(data_dir['ref_data_dir','loc'], 'SFR_network_template.txt'))
  write(sfr,f, append=ifelse(daily,T,F))
}

# ------------------------------------------------------------------------------------------------#

#' Create a Monthly Mountain Front Recharge (MFR) Data Frame
#'
#' Generates a data.frame of monthly mountain front recharge (MFR) estimates at the catchment scale.
#' By default, it computes each catchment's monthly total as \code{ndays * daily_rate},
#' converts stress-period numbers to dates via \code{mftime2date()}, and then zeros out
#' any specified "no-MFR" months (e.g. summer months with no recharge).
#' Alternatively, you can read pre-computed PRMS output instead.
#'
#' @param num_days_df A \code{data.frame} with at least two columns:
#'   \itemize{
#'     \item \code{ndays}: number of days in each stress period (month).
#'     \item \code{stress_period}: integer stress period index for each row.
#'   }
#' @param use_PRMS Logical; if \code{TRUE}, read a pre-written PRMS file rather than computing from \code{num_days_df}.
#' @param input_dir Character. Directory containing the PRMS file. Required if \code{use_PRMS = TRUE}.
#' @param input_file Character. Filename of the PRMS output table (default \code{"monthly_MFR_by_catchment.txt"}).
#'   Must include a \code{Date} column.
#' @param catchment_ids Character vector of catchment identifiers (must match those in
#'   \code{"modflow_cell_to_catchment.txt"}). Defaults to
#'   \code{c("5","6","7","8","9","10","11")}.
#' @param catchment_daily_rates Numeric vector of the corresponding daily recharge rates
#'   (in cubic meters per day) for each catchment in \code{catchment_ids}.
#'   Must be the same length as \code{catchment_ids}.
#' @param no_mfr_months Integer vector of month numbers (1-12) for which to force recharge to zero.
#'   Default \code{6:9} (June through September).
#'
#' @return A \code{data.frame} with columns:
#'   \itemize{
#'     \item \code{Stress_Period}: Date of the first day of each stress period (month).
#'     \item one column per catchment in \code{catchment_ids}, giving that month's
#'       total recharge (computed or PRMS), with zeros in \code{no_mfr_months}.
#'   }
#'
#' @details
#' If \code{use_PRMS = TRUE}, the function stops with an error unless \code{input_dir} is supplied,
#' reads \code{input_file}, and returns its contents (with \code{Date} as \code{Stress_Period}).
#' Otherwise it:
#' \enumerate{
#'   \item Multiplies \code{num_days_df\$ndays} by each \code{catchment_daily_rates}.
#'   \item Converts \code{num_days_df\$stress_period} to \code{Date} via
#'     \code{mftime2date(origin = '1990-09-30')}.
#'   \item Sets all catchment columns to zero for any month in \code{no_mfr_months}.
#' }
#'
#' @author Leland Scantlebury
#' @export
#'
#' @examples
#' # Compute MFR and zero out summer (Jun-Sep):
#' num_days_df <- data.frame(
#'   stress_period = 1:12,
#'   ndays         = c(31,28,31,30,31,30,31,31,30,31,30,31)
#' )
#' create_SWBM_MFR_df(
#'   num_days_df,
#'   no_mfr_months = 6:9
#' )
#'
#' # Read PRMS output instead:
#' create_SWBM_MFR_df(
#'   num_days_df,
#'   use_PRMS    = TRUE,
#'   input_dir   = "/path/to/prms",
#'   input_file  = "monthly_MFR_by_catchment.txt"
#' )
create_SWBM_MFR_df <- function(num_days_df,
                               use_PRMS=FALSE,
                               input_dir=NULL,
                               input_file="monthly_MFR_by_catchment.txt",
                               catchment_ids = c('5','6','7','8','9','10','11'),
                               catchment_daily_rates = c(234.0, 6300.0, 12040.0, 22350.0, 2000.0, 3860.0, 2430.0),
                               no_mfr_months = 6:9) {
  if (use_PRMS) {
    if (is.null(input_dir)) {stop('Must pass input_dir of PRMS files if use_PRMS is TRUE')}
    mfr_df <- read.table(file.path(input_dir,input_file),header = T)
    names(mfr_df)[1] <- 'Stress_Period'
    mfr_df$Stress_Period <- as.Date(mfr_df$Stress_Period)
    # Remove leading Xs that R adds...
    # These columns must be numeric
    names(mfr_df)[2:length(names(mfr_df))] <- as.numeric(gsub('X', '', names(mfr_df)[2:length(names(mfr_df))]))
    # Overwrite catchment_ids
    catchment_ids <- names(mfr_df)[2:length(names(mfr_df))]
  } else {
    # Use basecase calibrated values
    mfr_df <- num_days_df
    for (i in seq_along(catchment_ids)) {
      mfr_df[[catchment_ids[i]]] <- mfr_df$ndays * catchment_daily_rates[i]
    }
    mfr_df['Stress_Period'] <- mftime2date(sp = mfr_df$stress_period,ts = 1, origin_date = '1990-09-30')
    # Take out summer months
    mfr_df[lubridate::month(mfr_df$Stress_Period) %in% no_mfr_months,catchment_ids] <- 0.0
  }

  return(mfr_df[,c('Stress_Period',catchment_ids)])
}

# ------------------------------------------------------------------------------------------------#

#' Write SWBM Monthly Mountain Front Recharge (MFR) File
#'
#' Writes a monthly MFR table (at the catchment scale) to a space-delimited text file
#' for use with the Soil Water Budget Model (SWBM). The table should have one column
#' named \code{Stress_Period} (Date) and one column per catchment.
#'
#' @param mfr_df A \code{data.frame} produced by \code{create_SWBM_MFR_df()}, containing:
#'   \itemize{
#'     \item \code{Stress_Period}: Date of each stress period.
#'     \item one column per catchment ID.
#'   }
#' @param output_dir Character. Directory where the file should be written.
#' @param filename Character. Name of the output file. Default is \code{"monthly_MFR_by_catchment.txt"}.
#' @param verbose Logical; if \code{TRUE}, prints a message indicating which file is being written.
#'
#' @details
#' This is a thin wrapper around \code{write_SWBM_file()}, using:
#' - space as the field separator,
#' - no quoting of values,
#' - inclusion of column names but no row names.
#'
#' @return Invisibly returns \code{NULL}. The primary effect is writing \code{mfr_df} to disk.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Assume mfr_df was created earlier:
#' mfr_df <- create_SWBM_MFR_df(num_days_df)
#' write_SWBM_MFR_file(
#'   mfr_df     = mfr_df,
#'   output_dir = "inputs",
#'   filename   = "monthly_MFR_by_catchment.txt"
#' )
#' }
write_SWBM_MFR_file <- function(mfr_df,
                                output_dir,
                                filename = "monthly_MFR_by_catchment.txt",
                                verbose = TRUE) {
  write_SWBM_file(
    df         = mfr_df,
    output_dir = output_dir,
    filename   = filename,
    verbose    = verbose
  )
  invisible(NULL)
}

