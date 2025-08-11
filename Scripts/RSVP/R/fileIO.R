# My intent was to have a centralized place where file naming could be standardized. I didn't really follow through, but this stub remains
# - LS

#' Get Fort Jones Flow File Name
#'
#' Codifies a pattern for naming
#'
#' @param start_date Start date of data
#' @param end_date End date of data
#'
#' @return String filename
#' @author Leland Scantlebury
#' @export
#'
#' @examples
#' fname <- get_fj_flow_file_name(start_date = as.Date('2015-10-01'),
#'                                end_date = as.Date('2016-9-30'))
#'
get_fj_flow_file_name <- function(start_date, end_date) {
  fj_flow_file = paste0("FJ (USGS 11519500) Daily Flow, ",start_date,"_",end_date,".csv")
}
