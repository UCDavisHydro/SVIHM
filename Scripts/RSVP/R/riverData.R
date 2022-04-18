library(dataRetrieval)

# Some local global variables

fj_usgs_id <- "11519500"

# Generic Functions -------------------------------------------------------

#' Download USGS Flow Data for Specified Gauge. Thin wrapper for \code{\link[dataRetrieval]{readNWISdv}}.
#'
#' @param site_no USGS Site Number
#' @param start_date Start of data period
#' @param end_date End of data period
#' @param parameterCd Parameter codes of data to download (daily flow=00060 by default)
#'
#' @seealso \code{\link[dataRetrieval]{readNWISdv}}
#'
#' @return dataframe
#' @export
#'
#' @examples
download_USGS_flow <- function(site_no, start_date, end_date, parameterCd="00060") {
  flow = readNWISdv(siteNumbers = site_no, parameterCd=parameterCd,
                       startDate=start_date, endDate = end_date)
  return(renameNWISColumns(flow))
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
#'
#' @examples
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
