
#-------------------------------------------------------------------------------------------------#

#' Download CIMIS Station Data
#'
#' Quick wrapper for cimir package data request function. Obtains the full set of "default"
#' measurements (see \code{\link[cimir]{data_items}}. Can request from multiple stations, but the
#' default is just station 225 (Scott Valley). CIMIS only allows requests of 1750 records
#' at a time, data is requested in a loop to build the full record.
#'
#' CIMIS key must be set by \code{\link[cimir]{set_key}} before using this function.
#'
#' More options are available from the full \code{\link[cimir]{cimis_data}} function.
#'
#' @param start_date data request start date
#' @param end_date data request end date
#' @param stations CIMIS stations (default is 225, Scott Valley)
#' @param verbose T/F write status info to console (default: TRUE)
#' @param ... additional arguments passed to \code{\link[cimir]{cimis_data}}
#'
#' @return Tibble of requested data
#' @author Leland Scantlebury
#' @seealso \code{\link[cimir]{cimis_data}}, \code{\link[cimir]{data_items}},
#' \code{\link{build_daily_et_df}}
#'
#' @export
#' @examples
#' \dontrun{
#' et225 <- download_cimis_data(start_date = as.Date('2015-10-01'),
#'                              end_date = as.Date('2016-9-30'),
#'                              stations = 225,
#'                              items = 'day-eto')
#'}
download_cimis_data <- function(start_date, end_date, stations=225, verbose=TRUE, ...) {

  # Concatenate if multiple stations passed
  if (length(stations) > 1) {
    stations <- paste(stations, collapse=',')
  }

  # Limit to today's date
  if (end_date > Sys.Date()) {
    message('end_date is greater than current date - reducing query to current date')
    end_date <- Sys.Date()
  }

  # Create sequence of dates seperated by <= 1750 days
  req_dates <- c(seq.Date(start_date, end_date, 1749), end_date)
  req_list <- list()

  # Loop obtaining data and adding to list.
  if (verbose) {message('Querying CIMIS (may take a while)')}
  for (i in 1:(length(req_dates)-1)) {
    if (verbose) {
      message(paste(' - Obtaining Query', i, 'of', length(req_dates)-1, '|',
                    req_dates[i],'-', req_dates[i+1]))
    }
    req_list[[i]] <- cimir::cimis_data(targets = stations,
                                       start.date = req_dates[i],
                                       end.date = req_dates[i+1],
                                       measure.unit = 'M',
                                       ...)
  }

  # Combine
  cimis <- do.call(rbind, req_list)

  return(cimis)
}


# Functions from former SVIHM_input_analyses.R ----------------------------

#-------------------------------------------------------------------------------------------------#

#' Build Daily ET DataFrame
#'
#' Combines 3 datasets to create a final evapotranspiration dataset for SVIHM:
#' - Original SVIHM ET data (ref_et_1991_2011.txt)
#' - CIMIS Station 225 (Scott Valley) measured ET (from 2015-04-19 to current)
#' - Spatial SVIHM ET data for gap between datasets
#'
#' @param start_date Start of data period
#' @param end_date End of data period
#' @param ref_data_dir Reference data directory
#'                     (optional, default: SVIHM/SVIHM_Input_Files/reference_data)
#' @param verbose T/F write status info to console (default: TRUE)
#'
#' @return dataframe of ET data from start_date to end_date
#' @author Claire Kouba, Leland Scantlebury
#' @seealso \code{\link{download_cimis_data}}, \code{\link{write_swbm_et_input_file}}
#' @export
#'
#' @examples
#' \dontrun{
#' model_start_date <- get_model_start(1991)
#' model_end_date <- as.Date(floor_date(Sys.Date(), 'month')-1)
#' et <- build_daily_et_df(model_start_date, model_end_date)
#' }
build_daily_et_df <- function(start_date,
                              end_date,
                              ref_data_dir=data_dir['ref_data_dir','loc'],
                              verbose=TRUE) {

  #-- Obtain Data Sources
  et225 <- download_cimis_data(start_date = as.Date('2015-04-19'),
                               end_date = model_end_date,
                               verbose=verbose, items = 'day-eto')
  et225$month1 = lubridate::floor_date(et225$Date, unit = "month")
  et225_monthly = aggregate(et225$Value, by = list(et225$month1), FUN = sum, na.rm=T)
  colnames(et225_monthly) = c("Date", "ETo_mm")

  et_dl_aug2019 = read.csv(file.path(ref_data_dir,"spatial_eto_report_aug2019.csv"))
  et_sp = data.frame(Date = et_dl_aug2019$Date, ETo_mm = et_dl_aug2019$ETo..mm)
  et_sp$Date = as.Date(et_sp$Date, format = "%m/%d/%Y")
  et_sp$month1 = lubridate::floor_date(et_sp$Date, unit = "month")
  et_sp_monthly = aggregate(et_sp$ETo_mm, by = list(et_sp$month1), FUN = sum, na.rm=T)
  colnames(et_sp_monthly) = c("Date", "ETo_mm")

  # Original eto record
  et_orig_input = read.table(file.path(ref_data_dir,"ref_et_1991_2011.txt"))
  et_orig = data.frame(Date = as.Date(et_orig_input$V3, format = "%d/%m/%Y"), ETo_mm = et_orig_input$V1*1000)

  #-- Processing (leap days, avg daily by month)
  model_days = seq(from = start_date, to = end_date, by = "days")
  model_months = seq(from = start_date, to = end_date, by = "month")
  num_days <- days_in_month_diff(model_start_date, model_end_date)

  # Add leap days to et record. This is easy, because they are constant values for each month.
  leap_days = as.Date(paste0(c(1992, 1996, 2000, 2004, 2008), "-02-29"))
  leap_day_values = rep(NA, 5)
  for(i in 1:length(leap_days)){ leap_day_values[i] = et_orig$ETo_mm[et_orig$Date == leap_days[i]-1]} #assign the Feb 28 value to Feb 29
  # Jeez, it's the same for each february except 2004. What weird formula made this?
  et_orig = rbind(et_orig, data.frame(Date = leap_days, ETo_mm = leap_day_values)) #append leap days
  et_orig = et_orig[order(et_orig$Date),]

  # add number of days per month to monthly data frames
  num_days_df = data.frame(month_day1 = model_months, num_days = num_days)
  et_sp_monthly$num_days = as.numeric(num_days_df$num_days[match(et_sp_monthly$Date, num_days_df$month_day1)])
  et225_monthly$num_days = as.numeric(num_days_df$num_days[match(et225_monthly$Date, num_days_df$month_day1)])

  # Calculate monthly averages
  et_sp_monthly$daily_avg_by_mo = et_sp_monthly$ETo_mm / et_sp_monthly$num_days
  et225_monthly$daily_avg_by_mo = et225_monthly$ETo_mm / et225_monthly$num_days

  #Initialize stitched original-monthly record
  et_stitched_2 = data.frame(Date = model_days)
  et_stitched_2$ETo_mm = NA

  # -- Suture dates:
  ## First Rationale:
  # last plausible ET record for the original input is June 2011. After that it flatlines for some reason.
  # first plausible CIMIS225 record is Apr 21, 2015. first full month is May 2015.
  # So, the stitched record will be:
  # original record 1991-June 30 2011, spatial CIMIS June 30 2011-Apr 20 2015, and 225 CIMIS Apr 21 2015-Sep 30 2018.
  # So, the stitched record will be:
  # original record 1991-June 30 2011, spatial CIMIS June 30 2011-Apr 20 2015, and 225 CIMIS Apr 21 2015-Sep 30 2018.
  # end_orig = as.Date("2011-06-30"); end_sp = as.Date("2015-04-20")

  ## Second Rationale:
  # I need to make the updated input files match the original calibrated input files
  # as closely as possible. So the original ET record will extend all the way to
  # 2011-09-30.
  end_orig = as.Date("2011-09-30"); end_sp = as.Date("2015-04-20")

  #Assign original record
  et_stitched_2$ETo_mm[1:which(et_stitched_2$Date == end_orig)] = et_orig$ETo_mm[which(et_orig$Date==model_start_date):which(et_orig$Date == end_orig)]

  #Assigng spatial CIMIS record
  #declare indices for the spatial section of the CIMIS record
  sp_indices_stitched = which(et_stitched_2$Date == (end_orig+1)):which(et_stitched_2$Date ==end_sp)
  # Assign each day in the Spatial Cimis chunk of the record the monthly average ET value from the et_sp_monthly table.
  # Generate indices by matching the floor_date of each day in stitched_2 with the date in et_sp_monthly.
  et_stitched_2$ETo_mm[sp_indices_stitched] =
    et_sp_monthly$daily_avg_by_mo[match(lubridate::floor_date(et_stitched_2$Date[sp_indices_stitched], unit="month"), et_sp_monthly$Date )]

  # Assign CIMIS225 record
  # declare indices
  indices225_stitched = which(et_stitched_2$Date == (end_sp+1)):length(et_stitched_2$Date)
  # indices225_daily = which(et225$Date== (end_sp +1)):which(et_sp$Date == model_end_date)
  et_stitched_2$ETo_mm[indices225_stitched] =
    et225_monthly$daily_avg_by_mo[match(lubridate::floor_date(et_stitched_2$Date[indices225_stitched], unit="month"), et225_monthly$Date )]

  if (verbose) {message('ET data processing complete.')}

  return(et_stitched_2)
}

#-------------------------------------------------------------------------------------------------#

#' Write SWBM Evapotranspiration Input File
#'
#' Writes the Soil Water Balance Model (SWBM) evapotranspiration (ET) input file (ref_et_monthly.txt).
#'
#' @param et_record DataFrame of ET data, as generated by \code{\link{build_daily_et_df}}
#'                  Expects, at a minimum, columns "ETo_mm" and "Date"
#' @param output_dir Directory to write file
#' @param filename Filename (optional, default: ref_et_monthly.txt)
#' @param verbose T/F write status info to console (default: TRUE)
#'
#' @author Claire Kouba, Leland Scantlebury
#' @seealso \code{\link{build_daily_et_df}}
#' @export
#'
#' @examples
#' \dontrun{
#' model_start_date <- get_model_start(1991)
#' model_end_date <- as.Date(floor_date(Sys.Date(), 'month')-1)
#' update_dir <- create_update_dir(end_date = model_end_date)
#'
#' et <- build_daily_et_df(model_start_date, model_end_date)
#' write_swbm_et_input_file(et_record = et,
#'                          output_dir = update_dir,
#'                          filename = 'ref_et.txt')
#' }
write_swbm_et_input_file <- function(et_record,
                                     output_dir,
                                     filename="ref_et_monthly.txt",
                                     verbose=TRUE) {

  # Convert
  et_out <- data.frame(ETo_m  = et_record$ETo_mm / 1000,
                       ETo_in = et_record$ETo_mm / 25.4, #convert to inches
                       Date   = et_record$Date)
  et_out$Date <- format(et_out$Date, '%d/%m/%Y')

  # Write
  if (verbose) {message(paste('Writing file: ', filename))}
  write.table(et_out, file = file.path(output_dir, filename),
              sep = " ", quote = FALSE, col.names = FALSE, row.names = FALSE)
}
