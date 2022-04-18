#-------------------------------------------------------------------------------------------------#

#' Calculate SWBM Instream Flow Available Ratio Calendar
#'
#' @param model_start_date Model start date
#' @param model_end_date Model end date
#' @param cdfw_file Existing CDFW file to calculate from, cdfw_2017_instream_flows.csv by default
#'
#' @return dataframe
#' @author Claire Kouba
#' @export
#'
#' @examples
build_cdfw_instream_flow_cal <- function(model_start_date,
                                         model_end_date,
                                         fort_jones_flows,
                                         cdfw_file=file.path(data_dir['ref_data_dir','loc'],
                                                             "cdfw_2017_instream_flows.csv")) {
  cdfw_tab = read.csv(cdfw_file, stringsAsFactors = F)

  #build calendar of cdfw rec flow start and end dates for the years covering the model period
  model_days = seq(from = model_start_date, to = model_end_date, by = "days")
  model_yrs = lubridate::year(seq(from = model_start_date, to = model_end_date+365, by = "year"))
  cdfw_start_dates = as.Date(paste(sort(rep(model_yrs, dim(cdfw_tab)[1])),
                                   cdfw_tab$Start.date.month, cdfw_tab$start.date.day, sep ="-"))
  cdfw_end_dates = as.Date(paste(sort(rep(model_yrs, dim(cdfw_tab)[1])),
                                 cdfw_tab$End.date.month, cdfw_tab$End.date.day, sep ="-"))
  leap_years = seq(from=1900, to = 2100, by = 4)
  leapday_selector = lubridate::day(cdfw_end_dates) == 28 & lubridate::year(cdfw_end_dates) %in% leap_years
  cdfw_end_dates[leapday_selector] = 1 + cdfw_end_dates[leapday_selector] # adjust to include all of Feb
  cdfw_rec_flows = rep(cdfw_tab$Recommended.flow.cfs, length(model_yrs))

  instream_rec = data.frame(dates = model_days, cdfw_rec_flow_cfs = NA)

  for(i in 1:length(cdfw_start_dates)){
    selector = instream_rec$dates >= cdfw_start_dates[i] &
      instream_rec$dates <= cdfw_end_dates[i]
    instream_rec$cdfw_rec_flow_cfs[selector] = cdfw_rec_flows[i]
  }

  # Add FJ flow to this dataframe and find difference (available flow)
  instream_rec$fj_flow_cfs = fort_jones_flows$Flow[match(instream_rec$dates, fort_jones_flows$Date)]

  # Convert to cubic meters per day
  cfs_to_m3d = 1/35.3147 * 86400 # 1 m3/x ft3 * x seconds/day
  instream_rec$cdfw_rec_flow_m3d = instream_rec$cdfw_rec_flow_cfs * cfs_to_m3d
  instream_rec$fj_flow_m3d = instream_rec$fj_flow_cfs * cfs_to_m3d
  # Calculate m3d available for expanded MAR+ILR scenario
  instream_rec$avail_m3d = instream_rec$fj_flow_m3d - instream_rec$cdfw_rec_flow_m3d
  instream_rec$avail_m3d[instream_rec$avail_m3d<0] = 0

  # Add the stress period and day of month
  instream_rec$month = lubridate::floor_date(instream_rec$dates, unit = "month")

  #calculate aggregate daily average flow, by month, for whole record
  rec_monthly = aggregate(instream_rec$cdfw_rec_flow_m3d, by = list(instream_rec$month), FUN = sum)
  fj_monthly = aggregate(instream_rec$fj_flow_m3d, by = list(instream_rec$month), FUN = sum)
  avail_monthly = merge(rec_monthly, fj_monthly, by.x = "Group.1", by.y = "Group.1")
  colnames(avail_monthly) = c("month","cdfw_rec_flow_m3","fj_flow_m3")
  avail_monthly$avail_flow_m3 = avail_monthly$fj_flow_m3 - avail_monthly$cdfw_rec_flow_m3
  avail_monthly$avail_flow_m3[avail_monthly$avail_flow_m3 <0] = 0
  avail_monthly$avail_ratio = round(avail_monthly$avail_flow_m3 / avail_monthly$fj_flow_m3,3)

  #format for table
  avail_monthly[,colnames(avail_monthly) != "avail_ratio"] = NULL

  return(avail_monthly)
}

#-------------------------------------------------------------------------------------------------#
