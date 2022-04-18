
#' Get Fort Jones Flow File Name
#'
#' Codifies a pattern for naming
#'
#' @param start_date
#' @param end_date
#'
#' @return
#' @export
#'
#' @examples
get_fj_flow_file_name <- function(start_date, end_date) {
  fj_flow_file = paste0("FJ (USGS 11519500) Daily Flow, ",start_date,"_",end_date,".csv")
}
