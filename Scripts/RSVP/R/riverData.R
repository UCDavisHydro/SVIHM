library(dataRetrieval)

# Some local global variables

fj_usgs_id <- "11519500"

# Generic Functions -------------------------------------------------------

#' Download USGS Daily Mean Flow Data for Specified Gauge. Wrapper for
#' \code{\link[dataRetrieval]{read_waterdata_daily}} (USGS Water Data API, replaces NWIS).
#'
#' Output keeps the old readNWISdv/renameNWISColumns layout (agency_cd, site_no, Date, Flow, Flow_cd)
#' since downstream functions (e.g. assimilate_fj_update) rely on column order.
#'
#' @param site_no USGS Site Number
#' @param start_date Start of data period
#' @param end_date End of data period
#' @param parameterCd Parameter codes of data to download (daily flow=00060 by default)
#' @param statCd Statistic code (daily mean=00003 by default)
#'
#' @seealso \code{\link[dataRetrieval]{read_waterdata_daily}}
#'
#' @return dataframe
#' @export
download_USGS_flow <- function(site_no, start_date, end_date, parameterCd="00060", statCd="00003") {
  flow <- dataRetrieval::read_waterdata_daily(monitoring_location_id = paste0('USGS-', site_no),
                                              parameter_code = parameterCd,
                                              statistic_id = statCd,
                                              time = c(format(as.Date(start_date)), format(as.Date(end_date))),
                                              skipGeometry = TRUE)
  flow <- as.data.frame(flow)
  flow <- flow[order(flow$time),]

  # Approval code in old NWIS style (A = Approved, P = Provisional), with any qualifier appended
  flow_cd <- ifelse(flow$approval_status == 'Approved', 'A', 'P')
  has_qual <- !is.na(flow$qualifier) & flow$qualifier != ''
  flow_cd[has_qual] <- paste(flow_cd[has_qual], flow$qualifier[has_qual])

  out <- data.frame(agency_cd = 'USGS',
                    site_no   = site_no,
                    Date      = as.Date(flow$time),
                    Flow      = flow$value,
                    Flow_cd   = flow_cd)
  return(out)
}

# SVIHM specific functions ------------------------------------------------

#' Download Fort Jones Flow from USGS
#'
#' @param start_date Start of data period
#' @param end_date End of data period
#' @param output_dir Directory to write file
#' @param save_csv boolean if csv of data should be saved to ref_data_dir, FALSE by default
#' @param verbose T/F write status info to console
#'
#' @return dataframe
#' @export
download_fort_jones_flow <- function(start_date, end_date, output_dir, save_csv=FALSE, verbose=TRUE) {
  # Download
  fjdf <- download_USGS_flow(fj_usgs_id, start_date, end_date)
  # Save if desired
  if (save_csv) {
    fj_flow_file <- file.path(output_dir, get_fj_flow_file_name(start_date, end_date))
    if (verbose) {message(paste('Writing file: ', fj_flow_file))}
    write.csv(fjdf, fj_flow_file, row.names = FALSE)
  }
  return(fjdf)
}
