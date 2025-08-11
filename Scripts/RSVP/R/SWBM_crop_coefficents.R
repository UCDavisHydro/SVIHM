#-------------------------------------------------------------------------------------------------#

#' Create Daily Crop Coefficient Data Frame
#'
#' Generates a daily table of crop coefficients (Kc) for multiple land-use types
#' over the model period.  Legacy-file handling has been disabled entirely.
#'
#' @param start_date Date; first day of simulation.
#' @param end_date   Date; last day of simulation.
#'
#' @param alfalfa_kc_dormant      Numeric; alfalfa dormant Kc. Default `0`.
#' @param alfalfa_kc_growing      Numeric; alfalfa growing-season Kc. Default `0.9`.
#' @param alfalfa_irr_demand_mult Numeric; alfalfa irrigation multiplier. Default `1`.
#'
#' @param pasture_kc_dormant      Numeric; pasture dormant Kc. Default `0`.
#' @param pasture_kc_growing      Numeric; pasture growing-season Kc. Default `0.9`.
#' @param pasture_irr_demand_mult Numeric; pasture irrigation multiplier. Default `1`.
#'
#' @param grain_kc_dormant         Numeric; grain dormant Kc. Default `0`.
#' @param grain_kc_by_crop_stage   Numeric array; the 5-point Kc curve. Default `c(0,0.27,1.15,1.15,0)`.
#' @param grain_days_in_crop_stage Integer array; days per stage. Default `c(38,26,32,36)`.
#' @param grain_irr_demand_mult    Numeric; grain irrigation multiplier. Default `1`.
#'
#' @param natveg_kc                Numeric; native-veg growing-season Kc. Default `0.6`.
#' @param natveg_irr_demand_mult   Numeric; native-veg irrigation multiplier. Default `1`.
#' @details Native vegetation and barren/urban/water are assumed year-round Jan 1-Dec 31.
#'
#' @return A data.frame with columns:
#'   \itemize{
#'     \item `Date` (character `"YYYY-MM-DD"`)
#'     \item `Alfalfa_Kc`, `Grain_Kc`, `Pasture_Kc`,
#'           `Native_Veg_Kc`, `Barren_Urban_Kc`, `Water_Kc`
#'   }
#' @export
#' @examples
#' df <- create_daily_crop_coeff_df(start_date = as.Date("2000-01-01"), end_date = as.Date("2000-12-31"))
create_daily_crop_coeff_df <- function(
    start_date,
    end_date,
    alfalfa_kc_dormant      = 0,
    alfalfa_kc_growing      = 0.9,
    alfalfa_irr_demand_mult = 1,
    pasture_kc_dormant      = 0,
    pasture_kc_growing      = 0.9,
    pasture_irr_demand_mult = 1,
    grain_kc_dormant         = 0,
    grain_kc_by_crop_stage   = c(0, 0.27, 1.15, 1.15, 0),
    grain_days_in_crop_stage = c(38, 26, 32, 36),
    grain_irr_demand_mult    = 1,
    natveg_kc                = 0.6,
    natveg_irr_demand_mult   = 1
) {
  # Alfalfa (Mar 1-Nov 14 defaults)
  kc_alfalfa <- gen_daily_binary_crop_coefficients(
    start_date, end_date,
    kc_dormant       = alfalfa_kc_dormant,
    kc_growing       = alfalfa_kc_growing,
    irr_demand_mult  = alfalfa_irr_demand_mult
  )

  # Pasture (same Mar 1-Nov 14 defaults)
  kc_pasture <- gen_daily_binary_crop_coefficients(
    start_date, end_date,
    kc_dormant       = pasture_kc_dormant,
    kc_growing       = pasture_kc_growing,
    irr_demand_mult  = pasture_irr_demand_mult
  )

  # Grain (curve, with legacy_file forced NULL)
  kc_grain <- gen_daily_curve_crop_coefficients(
    start_date, end_date,
    kc_dormant         = grain_kc_dormant,
    kc_by_crop_stage   = grain_kc_by_crop_stage,
    days_in_crop_stage = grain_days_in_crop_stage,
    irr_demand_mult    = grain_irr_demand_mult
  )

  # Native vegetation (year-round)
  kc_natveg <- gen_daily_binary_crop_coefficients(
    start_date, end_date,
    kc_dormant       = 0,
    kc_growing       = natveg_kc,
    growing_season_start_month = 1,
    growing_season_start_day   = 1,
    growing_season_end_month   = 12,
    growing_season_end_day     = 31,
    irr_demand_mult  = natveg_irr_demand_mult
  )

  # Open water (always 0)
  kc_water <- gen_daily_binary_crop_coefficients(
    start_date, end_date,
    kc_dormant       = 0,
    kc_growing       = 0,
    growing_season_start_month = 1,
    growing_season_start_day   = 1,
    growing_season_end_month   = 12,
    growing_season_end_day     = 31,
  )

  # Barren/Urban (always 0)
  kc_urban <- gen_daily_binary_crop_coefficients(
    start_date, end_date,
    kc_dormant       = 0,
    kc_growing       = 0,
    growing_season_start_month = 1,
    growing_season_start_day   = 1,
    growing_season_end_month   = 12,
    growing_season_end_day     = 31,
  )

  daily_kc_df <- data.frame(
    Date            = format(kc_alfalfa$Date, "%Y-%m-%d"),
    Alfalfa_Kc      = kc_alfalfa$kc,
    Grain_Kc        = kc_grain$kc,
    Pasture_Kc      = kc_pasture$kc,
    Native_Veg_Kc   = kc_natveg$kc,
    Barren_Urban_Kc = kc_urban$kc,
    Water_Kc        = kc_water$kc,
    stringsAsFactors = FALSE
  )

  return(daily_kc_df)
}

#-------------------------------------------------------------------------------------------------#

#' Generate Daily Binary Crop Coefficients (Kc)
#'
#' Currently used for alfalfa and pasture. Binary coefficients for growing/dormant.
#'
#' @param start_date Start date of model
#' @param end_date End date of model
#' @param kc_dormant coefficient during dormant period, default 0.0
#' @param kc_growing coefficient during growing period, default 0.9
#' @param growing_season_start_month growing season start month, default 3 (March)
#' @param growing_season_start_day growing season deafult day, default 1 (March 1st)
#' @param growing_season_end_month growing season start month, default 11 (November)
#' @param growing_season_end_day growing season deafult day, default 14 (November 14th)
#' @param irr_demand_mult Irrigation demand multiplier, default 1
#'
#' @return DataFrame
#' @author Claire Kouba, Leland Scantlebury
#' @export
#'
#' @examples
#' gen_daily_binary_crop_coefficients(start_date = as.Date("2000-01-01"), end_date = as.Date("2000-12-31"))
gen_daily_binary_crop_coefficients <- function(start_date,
                                     end_date,
                                     kc_dormant = 0,
                                     kc_growing = 0.9,
                                     growing_season_start_month = 3,
                                     growing_season_start_day = 1,
                                     growing_season_end_month = 11,
                                     growing_season_end_day = 14,
                                     irr_demand_mult = 1) {
  model_days = seq(from = start_date, to = end_date, by = "days")

  kc_days = rep(kc_growing, length(model_days))
  #days before or after the growing season are assigned a dormant kc value
  #TO DO: there must be a more elegant way to do this
  kc_days[(lubridate::month(model_days) < growing_season_start_month) |
                (lubridate::month(model_days) == growing_season_start_month & lubridate::day(model_days) < growing_season_start_day)|
                (lubridate::month(model_days) > growing_season_end_month) |
                (lubridate::month(model_days) == growing_season_end_month & lubridate::day(model_days) > growing_season_end_day)
  ] = kc_dormant

  #Pad single-digit month, day values with 0s (e.g. "2" becomes "02"), then concatenate date strings
  kc_df = data.frame(Date=model_days, kc=kc_days)

  # Multiply the k_c by the demand multiplier (if irr_demand_mult!= 1, assumes land use change from alfalfa)
  kc_df$kc = kc_df$kc * irr_demand_mult

  return(kc_df)
}

#-------------------------------------------------------------------------------------------------#

#' Generate Daily Curve Crop Coefficients (Kc)
#'
#' Currently used for grain (see: default values). Uses a curve to represent crop stages.
#'
#' @param start_date Start date of model
#' @param end_date End date of model
#' @param kc_dormant coefficient during dormant period, default 0.0
#' @param kc_by_crop_stage array of coefficients for various growing stages (default: grain curve)
#' @param days_in_crop_stage array of days for each stage (2 less than kc_by_crop_stage in length)
#' @param growing_season_start_month growing season start month, default 3 (March)
#' @param growing_season_start_day growing season deafult day, default 1 (March 14th)
#' @param ref_data_dir Reference data directory (default: SVIHM/SVIHM_Input_Files/reference_data/)
#' used to fine legacy_file
#' @param legacy_file legacy crop coefficient file to overwrite, default kc_1991_2011.txt. NULL
#' to ignore
#' @param irr_demand_mult Irrigation demand multiplier, default 1
#'
#' @return DataFrame
#' @author Claire Kouba, Leland Scantlebury
#' @export
#'
#' @examples
#' gen_daily_curve_crop_coefficients(start_date = as.Date("2000-01-01"), end_date = as.Date("2000-12-31"))
gen_daily_curve_crop_coefficients <- function(start_date,
                                        end_date,
                                        kc_dormant = 0,
                                        kc_by_crop_stage = c(0, 0.27, 1.15, 1.15, 0),
                                        days_in_crop_stage = c(38, 26, 32, 36),
                                        growing_season_start_month = 3,
                                        growing_season_start_day = 14,
                                        ref_data_dir = data_dir['ref_data_dir','loc'],
                                        legacy_file = "kc_grain_1991_2011.txt",
                                        irr_demand_mult = 1) {

  #initialize growing season kc profile based on growth stage
  kc_growing_season = rep(0, (sum(days_in_crop_stage) - 3)) # minus 3 fenceposts
  crop_stage_index <- 1
  start_year <- lubridate::year(start_date)
  end_year <- lubridate::year(end_date)

  for(i in 1:length(days_in_crop_stage)){
    stage_length = days_in_crop_stage[i]
    start_kc = kc_by_crop_stage[i]
    end_kc = kc_by_crop_stage[i+1]
    kc_growing_season[crop_stage_index: (crop_stage_index + stage_length -1)] =
      seq(start_kc, end_kc, length.out = stage_length)
    crop_stage_index = crop_stage_index + stage_length - 1
  }

  model_days = seq(from = start_date, to = end_date, by = "days")

  #initialize full model period kc vector
  kc_days = rep(kc_dormant, length(model_days))

  #find the index of the growing season start for each year and assign the growing season curve

  # But if the growing season hasn't started yet this year, make these indices end last year
  growing_season_start_date_final_year = as.Date(paste(end_year, growing_season_start_month, growing_season_start_day, sep = "-"))
  if(end_date < growing_season_start_date_final_year){
    end_year = end_year - 1
  }
  for(yr in (start_year+1):end_year){
    start_day_index = which(lubridate::year(model_days) == yr &
                            lubridate::month(model_days) == growing_season_start_month &
                            lubridate::day(model_days) == growing_season_start_day)
    kc_days[start_day_index:(start_day_index + sum(days_in_crop_stage) - 4)] = kc_growing_season
  }

  # Cutoff extra days generated by possible incomplete year
  kc_days <- kc_days[1:length(model_days)]

  # Build data frame with date formatted for file-writing
  kc_df = data.frame(Date=model_days, kc=kc_days)

  if (!is.null(legacy_file)) {
    # TODO is hardcoded for grain
    # Read from file to exactly match legacy kc values for 1991-2011 period
    kc_legacy = read.table(file.path(ref_data_dir,legacy_file))
    kc_legacy = data.frame(Date = as.Date(kc_legacy$V2, format = "%d/%m/%Y"), kc_grain = kc_legacy$V1)
    #leap_days = as.Date(paste("29","02", seq(1904, end_year, by=4),sep="/"), format = "%d/%m/%Y")
    #legacy_days_selector = model_days <= as.Date("2011-09-30") & !(model_days %in% leap_days)
    #kc_days[legacy_days_selector] = kc_legacy$kc_grain
    kc_df[kc_df$Date %in% kc_legacy$Date, 'kc'] <- kc_legacy$kc_grain  #TODO match instead of %in%??
  }

  # Multiply the k_c by the demand multiplier (if irr_demand_mult!= 1, assumes land use change from grain)
  kc_df$kc = kc_df$kc * irr_demand_mult

  return(kc_df)
}

#-------------------------------------------------------------------------------------------------#
# Writing Functions

#' Write SWBM Daily Crop Coefficients File
#'
#' Writes a daily table of crop coefficients (Kc) to a space-delimited text file
#' suitable for ingestion by the Soil Water Budget Model (SWBM).
#'
#' @param daily_kc_df A data.frame as produced by \code{create_daily_crop_coeff_df()},
#'   containing a \code{Date} column (character "YYYY-MM-DD") and one column per land-use
#'   type (e.g. \code{Alfalfa_Kc}, \code{Grain_Kc}, etc.).
#' @param output_dir Character. Path to the directory where the file should be written.
#' @param filename Character. Name of the output file (default \code{"kc_values.txt"}).
#' @param verbose Logical. If \code{TRUE}, prints a message indicating which file is being written.
#'
#' @details
#' This is a thin wrapper around \code{write_SWBM_file()}, which uses:
#' - space (\code{" "}) as the field separator
#' - no quoting of values
#' - inclusion of column names but no row names
#'
#' @return Invisibly returns \code{NULL}.  The primary effect is the side-effect of writing
#'   \code{daily_kc_df} to disk.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Assume you have already generated daily_kc_df:
#' daily_kc_df <- create_daily_crop_coeff_df(
#'   start_date = as.Date("2021-01-01"),
#'   end_date   = as.Date("2021-12-31")
#' )
#' # Write it out:
#' write_SWBM_daily_kc_file(
#'   daily_kc_df = daily_kc_df,
#'   output_dir  = "SWBM_inputs",
#'   filename    = "year1_kc.txt",
#'   verbose     = TRUE
#' )
#' }
write_SWBM_daily_kc_file <- function(daily_kc_df, output_dir, filename='kc_values.txt', verbose=TRUE) {
  write_SWBM_file(daily_kc_df, output_dir, filename, verbose)
}


