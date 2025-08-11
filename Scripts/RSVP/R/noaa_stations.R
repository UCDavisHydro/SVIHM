#' Basic data on NOAA gauges used in SVIHM
#'
#' Location and name data for the noaa weather gauges used for the Scott Valley Integrated Hydrologic Model (SVIHM)
#'
#' @format A data frame with 6 observations of 7 variables
#' @usage data(noaa_stations)
#' \describe{
#'   \item{id}{NOAA station id}
#'   \item{name}{Common name of station}
#'   \item{name_short}{short 2-3 letter version of name}
#'   \item{noaa_name}{NOAA official name of station}
#'   \item{latitude}{latitude of station}
#'   \item{longitude}{longitude of station}
#'   \item{elevation}{elevation of station}
#' }
#' @source rnoaa package
"noaa_stations"
