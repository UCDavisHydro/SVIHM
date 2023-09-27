#-------------------------------------------------------------------------------------------------#

#' Generate Daily Binary Crop Coefficients (Kc)
#'
#' Currently used for alfalfa and pasture. Binary coefficients for growing/dormant.
#'
#' @param model_start_date Start date of model
#' @param model_end_date End date of model
#' @param kc_dormant coefficient during dormant period, default 0.0
#' @param kc_growing coefficient during growing period, default 0.9
#' @param growing_season_start_month growing season start month, default 3 (March)
#' @param growing_season_start_day growing season deafult day, default 1 (March 1st)
#' @param growing_season_end_month growing season start month, default 11 (November)
#' @param growing_season_end_day growing season deafult day, default 14 (November 14th)
#' @param irr_demand_mult Irrigation demand multiplier, default 1
#'
#' @return
#' @author Claire Kouba, Leland Scantlebury
#' @export
#'
#' @examples
gen_daily_binary_crop_coefficients <- function(model_start_date,
                                     model_end_date,
                                     kc_dormant = 0,
                                     kc_growing = 0.9,
                                     growing_season_start_month = 3,
                                     growing_season_start_day = 1,
                                     growing_season_end_month = 11,
                                     growing_season_end_day = 14,
                                     irr_demand_mult = 1) {
  model_days = seq(from = model_start_date, to = model_end_date, by = "days")

  kc_days = rep(kc_growing, length(model_days))
  #days before or after the growing season are assigned a dormant kc value
  #TO DO: there must be a more elegant way to do this
  kc_days[(lubridate::month(model_days) < growing_season_start_month) |
                (lubridate::month(model_days) == growing_season_start_month & lubridate::day(model_days) < growing_season_start_day)|
                (lubridate::month(model_days) > growing_season_end_month) |
                (lubridate::month(model_days) == growing_season_end_month & lubridate::day(model_days) > growing_season_end_day)
  ] = kc_dormant

  # Change 8 specific dates to match legacy input
  #legacy_0_kc_dates = as.Date(c("1996-11-14","1997-11-14","2004-11-14", "2005-11-14"))
  #legacy_0.9_kc_dates = as.Date(c("1997-02-28","1998-02-28","2005-02-28", "2006-02-28"))
  #kc_days[model_days %in% legacy_0_kc_dates] = 0
  #kc_days[model_days %in% legacy_0.9_kc_dates] = 0.9

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
#' @param model_start_date Start date of model
#' @param model_end_date End date of model
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
#' @return
#' @author Claire Kouba, Leland Scantlebury
#' @export
#'
#' @examples
gen_daily_curve_crop_coefficients <- function(model_start_date,
                                        model_end_date,
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

  for(i in 1:length(days_in_crop_stage)){
    stage_length = days_in_crop_stage[i]
    start_kc = kc_by_crop_stage[i]
    end_kc = kc_by_crop_stage[i+1]
    kc_growing_season[crop_stage_index: (crop_stage_index + stage_length -1)] =
      seq(start_kc, end_kc, length.out = stage_length)
    crop_stage_index = crop_stage_index + stage_length - 1
  }

  model_days = seq(from = model_start_date, to = model_end_date, by = "days")

  #initialize full model period kc vector
  kc_days = rep(kc_dormant, length(model_days))

  #find the index of the growing season start for each year and assign the growing season curve

  # But if the growing season hasn't started yet this year, make these indices end last year
  growing_season_start_date_final_year = as.Date(paste(end_year, growing_season_start_month, growing_season_start_day, sep = "-"))
  if(model_end_date < growing_season_start_date_final_year){
    end_year = end_year - 1
  }
  for(yr in (start_year+1):end_year){
    start_day_index = which(year(model_days) == yr &
                            month(model_days) == growing_season_start_month &
                            day(model_days) == growing_season_start_day)
    kc_days[start_day_index:
                    (start_day_index + sum(days_in_crop_stage) - 4)] = kc_growing_season
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

