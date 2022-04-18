
#-------------------------------------------------------------------------------------------------#

#' Precipitation Bar Chart
#'
#' @param dates array of dates of precip events
#' @param precip array of precipitation amount at dates
#' @param unit character, the units the precipitation is measured in
#' @param title title of plot (optional)
#' @param col color of bars (optional)
#' @param border color of bar borders (optional)
#'
#' @return
#' @export
#'
#' @examples
plot.precip <- function(dates, precip, unit='mm', title=NULL, col=NULL, border=par("fg")){
  barplot(height = precip,
          names.arg = dates,
          xlab="Date",
          ylab=paste0("Precipitation (", unit, ")"))
  grid()
  barplot(height = precip,
          names.arg = dates,
          col = col,
          border = border,
          add=T)
  if (!is.null(title)) {
    title(main=title)
  }
}
#-------------------------------------------------------------------------------------------------#

#' Precipitation Comparison Bar Chart
#'
#' Creates plot showing identical (color 1) and differing (color 2, color 3) precipitation data
#'
#' @param dates array of dates of precip events
#' @param precip1 array of first precipitation dataset at dates
#' @param precip2 array of second precipitation dataset at dates
#' @param unit character, the units the precipitation is measured in
#' @param title title of plot (optional)
#' @param colors colors for data (identical, precip1, precip2) (optional)
#' @param legend_names names added to legend to describe precip1 and precip2
#'
#' @return
#' @export
#'
#' @examples
plot.precip.compare <- function(dates, precip1, precip2, unit='mm', title=NULL, colors=NULL,
                               legend_names){
  if (is.null(colors)) {
    colors <- c('grey69', 'blue', 'red')
  }
  plot.precip(dates, precip1, unit, title, border=colors[2])
  barplot(height = precip2,
          names.arg = dates,
          border = colors[3], add=T)
  barplot(height = pmin(precip1, precip2),
          names.arg = dates,
          border = colors[1], add=T)
  legend('topright',cex = 0.65, ncol = 3,
         legend=c('Identical', legend_names),
         col=colors,
         pch=rep(NA,3),
         lty=rep(1, 3))
}
#-------------------------------------------------------------------------------------------------#

#' Title
#'
#' @param dates
#' @param ...
#' @param title
#' @param unit
#' @param col
#'
#' @return
#' @export
#'
#' @examples
plot.precip.cumulative <- function(dates, ..., title=NULL, unit='mm', col=NULL){

  # Convert multiple datasets to list
  precip <- list(...)

  # Calculate cumulative sums for each dataset - sets NAs to zero to prevent errors
  for (i in 1:length(precip)) {
    prcp <- precip[[i]]
    prcp[is.na(prcp)] <- 0
    precip[[i]] <- cumsum(prcp)
  }

  # Setup base plot
  plot(x = dates,
       y = precip[[i]],
       xlab='Date',
       ylab=paste0("Precipitation (", unit, ")"),
       type='n')
  grid()

  # Loop over data adding to plot
  for (i in 1:length(precip)) {
    lines(dates, precip[[i]], col=col[i])
  }
  box()
}
