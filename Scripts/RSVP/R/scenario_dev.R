# Various tools to aid in scenario development
# where we assume scenarios are stored in a list `scen`

#-------------------------------------------------------------------------------------------------#

#' Set Up Scenario Object
#'
#' Initializes input directories, model start/end dates, and the output scenario directory
#' for a groundwater-surface water model scenario. This function supports several predefined
#' scenario types and performs final time discretization calculations based on the selected
#' model period.
#'
#' @param scen A named list representing the scenario to be configured. Must include:
#'   \itemize{
#'     \item `name` - character; scenario name (used for output directory naming).
#'     \item `type` - character; scenario type: one of `"BASECASE"`, `"UPDATE"`, or `"PRMS"` (case-insensitive).
#'   }
#'
#' @param start_year Numeric year indicating the start of the model period. Defaults to `1991`.
#'
#' @details
#' The behavior varies depending on `scen$type`:
#' \describe{
#'   \item{`"BASECASE"`}{
#'     Uses reference input data from the "basecase" folder in the Scenario directory.
#'     Sets the model end date to 2024-09-30.
#'   }
#'   \item{`"UPDATE"`}{
#'     Uses the latest update directory from `data_dir['update_dir', 'loc']` (via `latest_dir()`).
#'     Derives the model end date from the folder name (converted to a date), minus one day.
#'   }
#'   \item{`"PRMS"`}{
#'     Uses input data from the `"PRMS"` subfolder within `data_dir['input_files_dir', 'loc']`.
#'     Sets a fixed model end date of 2023-09-30.
#'   }
#' }
#'
#' The following components are added to the `scen` list:
#' \itemize{
#'   \item `input_dir`: path to scenario input files.
#'   \item `start_date`: start date, derived from `start_year` via `get_model_start()`.
#'   \item `end_date`: end date, as described above.
#'   \item `num_stress_periods`: total number of model stress periods (typically monthly).
#'   \item `num_days`: vector of days in each stress period.
#'   \item `scen_dir`: output directory for writing scenario-specific files.
#' }
#'
#' @return A modified version of the input `scen` list with additional scenario metadata.
#'
#' @export
#' @examples
#' scen <- list('name'='basecase', 'type'='basecase')
#' scen <- scenario_setup(scen)

# ------------------------------------------------------------------------------------------------#

scenario_setup <- function(scen, start_year=1991) {
  # General Base
  scen$nSubws = 8
  scen$nSFR_inflow_segs = 12
  scen$polygon_file        = file.path(data_dir["time_indep_dir","loc"], "polygons_table.txt")
  scen$landcover_desc_file = file.path(data_dir["time_indep_dir","loc"], "landcover_table.txt")
  scen$inflow_seg_file     = file.path(data_dir["time_indep_dir","loc"], "SFR_inflow_segments.txt")
  if (toupper(scen$type) =='BASECASE') {
    # Set Input Directory (Created in SVIHM_Input_Files/Updates) - Grabs latest version
    scen$input_dir <- file.path(data_dir['scenario_dir','loc'],'basecase')

    # Setup Dates
    scen$start_date <- get_model_start(start_year)
    scen$end_date <- as.Date('2024-09-30')

  } else if (toupper(scen$type) =='UPDATE') {
    # Set Input Directory (Created in SVIHM_Input_Files/Updates) - Grabs latest version
    scen$input_dir <- latest_dir(data_dir['update_dir','loc'])

    # Setup Dates
    scen$start_date <- get_model_start(start_year)
    scen$end_date <- as.Date(basename(scen$input_dir))-1

  } else if (toupper(scen$type) =='PRMS') {
    # Set Input Directory (Created in SVIHM_Input_Files/Updates) - Grabs latest version
    scen$input_dir <- file.path(data_dir['input_files_dir','loc'], 'PRMS')
    scen$inflow_seg_file <- file.path(scen$input_dir, "SFR_inflow_segments.txt")
    scen$nSubws = 11
    scen$nSFR_inflow_segs = 25

    # Setup Dates
    scen$start_date <- get_model_start(start_year)
    scen$end_date <- as.Date('2023-09-30')
  }
  # Final temporal discretization calcs
  scen$num_stress_periods <- calc_num_stress_periods(scen$start_date, scen$end_date)
  scen$num_days <- days_in_month_diff(scen$start_date, scen$end_date)
  # Scenario folder name
  scen$full_name <- paste(scen$name,scen$type,scen$end_date,sep="_")
  scen$scen_dir <- file.path(data_dir['scenario_dir','loc'], scen$full_name)

  return(scen)
}
