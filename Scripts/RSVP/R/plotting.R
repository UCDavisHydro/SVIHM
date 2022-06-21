

# Precipitation -----------------------------------------------------------

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

#' Cumulative Precipitation Comparison Plot
#'
#' @param dates array of dates of precip events
#' @param ... one or more arrays of precipitation at dates
#' @param title title of plot (optional)
#' @param unit character, the units the precipitation is measured in
#' @param col array of colors for data (optional)
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


# Time Series -------------------------------------------------------------

#-------------------------------------------------------------------------------------------------#
plot.ts_setup <- function(dates, ..., xlabel, ylabel, log='', interval='year', bgcolor='grey90',
                          gridcolor='white', gridcolor2='grey93', las=2, xlim_override=NULL, ylim_min_diff=10) {

  pdat <- list(...)

  #-- Unlist dates if needed
  if (typeof(dates) == 'list') { dates <- do.call("c", dates) }

  #-- Axis limits
  if (is.null(xlim_override)) {
    xlim <- seq(floor_date(min(dates), 'month'), ceiling_date(max(dates),'month'), interval)
  } else {
    xlim <- xlim_override
  }

  #-- Handle log10 y
  if (log=='y') {
    ylim <- c(10^floor(log10(min(do.call(c, pdat), na.rm = T))), 10^ceiling(log10(max(do.call(c, pdat), na.rm = T))))

  } else  {
    ylim <- c(floor(min(do.call(c, pdat), na.rm = T)), ceiling(max(do.call(c, pdat), na.rm = T)))
  }

  #-- Enforce minimum (does this need to be different for the log plots?)
  if ((ylim[2] - ylim[1]) < ylim_min_diff) {
    ylim <- c(floor(mean(ylim)-ylim_min_diff/2), ceiling(mean(ylim)+ylim_min_diff/2))
  }

  #-- "Fake" plot
  plot(1, 1,
       ann=FALSE,
       xaxt = 'n',
       yaxt = 'n',
       xlab = NA,
       ylab = NA,
       xlim = range(min(xlim),max(xlim)),
       ylim = ylim,
       log = log,
       pch  = NA)

  #-- Plot Background
  if (log=='y') {
    rect(par("usr")[1], 10^par("usr")[3], par("usr")[2], 10^par("usr")[4], col=bgcolor)
  } else {
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col=bgcolor)
  }

  #-- x-axis
  axis(1,at=xlim, labels=NA, tck=1, col=gridcolor, las=2)

  if(tolower(interval) == 'year'){
    axis(1,at=xlim, labels=format(xlim, "%Y"), tck=1, col=gridcolor, las=las, cex.axis=1)
  } else if (tolower(interval) == 'month'){
    axis(1,at=xlim, labels=format(xlim, "%b-%Y"), tck=1, col=gridcolor, las=las, cex.axis=1)
  }

  #-- y-axis
  if (log == 'y') {
    major_grid <- 10^(log10(ylim[1]):log10(ylim[2])) %o% 1
    minor_grid <- 10^(log10(ylim[1]):log10(ylim[2])) %o% 1:9
    axis(2,minor_grid,tck=1,col=gridcolor2,labels=FALSE,las=1)
    axis(2,major_grid,tck=1,col=gridcolor,labels=TRUE,las=1)
  } else {
    # In automatic spacing we trust
    axis(2,tck=1,col=gridcolor,labels=TRUE,las=1)
  }


  #-- Clean up
  title(xlab=xlabel, ylab=ylabel)
  box()
}

#-------------------------------------------------------------------------------------------------#

plot.gw.hydrograph_wMap <- function(dates, obs, sim, xloc, yloc, map_xs, map_ys, ylabel, log='',
                                 title=NULL, colors=NULL, map_x_offset=0, map_y_offset=0,
                                 mapbmar=30, ...) {

  #-- Coerce dates to list if not list (Hacky)
  if (typeof(dates) != 'list') {
    dates <- list(dates, dates)
  }

  #-- Set up plot layout
  layout(matrix(c(1,2,3,2), 2, 2, byrow = TRUE),
         heights = c(1,8),
         widths = c(4,1))

  #-- Legend/Title Plot
  par(mar=c(1,1,4.5,0))  #bottom, left, top, right
  plot(c(0,1), c(0,1), type="n", axes=F, xlab="", ylab="")
  # Title
  title(main=title, cex.main=1.0)
  # Legend
  legend("center",
         legend=c('Observed', 'Simulated'),
         col=c('dodgerblue2','black'),
         lty=c(NA,1),
         lwd=c(NA,1),
         pch=c(16,NA),
         cex=1.0,
         ncol=1,
         bty='n',
         bg='white')

  #-- Map
  par(mar=c(mapbmar,0,0,1))  #bottom, left, top, right
  plot.svihm_minimap(xloc, yloc, map_xs, map_ys, map_x_offset, map_y_offset)
#  box()

  #-- Time Series
  par(mar=c(6,6,0.5,0.5))  #bottom, left, top, right
  plot.ts_setup(dates, obs, sim, xlabel='Date', ylabel=ylabel, log=log)
  points(dates[[1]], obs, pch=16, col='dodgerblue2')
  lines(dates[[2]], sim, col='black')
}

#-------------------------------------------------------------------------------------------------#

plot.stream.hydrograph_wMap <- function(dates, obs, sim, xloc, yloc, map_xs, map_ys, ylabel, log='',
                                 title=NULL, colors=NULL, map_x_offset=0, map_y_offset=0,
                                 mapbmar=30, zero_rep=NA, ...) {

  #-- Coerce dates to list if not list (Hacky)
  if (typeof(dates) != 'list') {
    dates <- list(dates, dates)
  }

  #-- Handle zeros (only for log plot)
  if (log=='y') {
    obs[obs == 0] <- zero_rep
    sim[sim == 0] <- zero_rep
  }

  #-- Set up plot layout
  layout(matrix(c(1,2,3,2), 2, 2, byrow = TRUE),
         heights = c(1,8),
         widths = c(4,1))

  #-- Legend/Title Plot
  par(mar=c(1,1,3,0))  #bottom, left, top, right
  plot(c(0,1), c(0,1), type="n", axes=F, xlab="", ylab="")
  # Title
  title(main=title, cex.main=1.0)
  # Legend
  legend("center",
         legend=c('Observed', 'Simulated'),
         col=c('dodgerblue2','black'),
         lty=c(1,1),
         lwd=c(1,1),
         pch=c(NA,NA),
         cex=1.0,
         ncol=1,
         bty='n',
         bg='white')

  #-- Map
  par(mar=c(mapbmar,0,0,1))  #bottom, left, top, right
  plot.svihm_minimap(xloc, yloc, map_xs, map_ys, map_x_offset, map_y_offset)
  #  box()

  #-- Time Series
  par(mar=c(6,6,0.5,0.5))  #bottom, left, top, right
  plot.ts_setup(dates, obs, sim, xlabel='Date', ylabel=ylabel, log=log, ...)
  lines(dates[[1]], obs, col='dodgerblue2')
  lines(dates[[2]], sim, col='black')
}

#-------------------------------------------------------------------------------------------------#

plot.pre_post_compare <- function(dates, obs, sim, split_date, ylabel, log='',
                                  pre_title=NULL, post_title=NULL, colors=NULL, zero_rep=NA, ...) {

  #-- Handle zeros (only for log plot)
  if (log=='y') {
    obs[obs == 0] <- zero_rep
    sim[sim == 0] <- zero_rep
  }

  pre_dates <- dates[dates < split_date]
  pst_dates <- dates[dates >= split_date]

  pre_obs <- obs[dates < split_date]
  pst_obs <- obs[dates >= split_date]

  pre_sim <- sim[dates < split_date]
  pst_sim <- sim[dates >= split_date]

  #-- Set up plot layout
  layout(matrix(c(1,2), 2, 1, byrow = TRUE),
         heights = c(1,1))

  #-- Pre Plot TS
  par(mar=c(4,3,2,1))  #bottom, left, top, right
  plot.ts_setup(pre_dates, pre_obs, pre_sim, xlabel='Date', ylabel=ylabel, log=log, ...)
  lines(pre_dates, pre_obs, col='dodgerblue2')
  lines(pre_dates, pre_sim, col='black')
  title(main=pre_title)

  #-- Post Plot TS
  par(mar=c(4,3,2,1))  #bottom, left, top, right
  plot.ts_setup(pst_dates, pst_obs, pst_sim, xlabel='Date', ylabel=ylabel, log=log, ...)
  lines(pst_dates, pst_obs, col='dodgerblue2')
  lines(pst_dates, pst_sim, col='black')
  title(main=post_title)
}

#-------------------------------------------------------------------------------------------------#

#' SVIHM Minimap Plot
#'
#' Plots coordinate data, with a specific highlighted coordinate, on a map of the SVIHM model with
#' the SFR river shown. Intended to be a minimap in the corner of a time series (etc) plot
#'
#' @param xloc numeric UTM 10N x-coordinate of highlighted location
#' @param yloc numeric UTM 10N x-coordinate of highlighted location
#' @param map_xs numeric UTM 10N x-coordinates of locations
#' @param map_ys numeric UTM 10N x-coordinates of locations
#' @param map_x_offset
#' @param map_y_offset
#'
#' @return
#' @author Leland Scantlebury
#' @export
#'
#' @examples
plot.svihm_minimap <- function(xloc, yloc, map_xs, map_ys, map_x_offset, map_y_offset) {
  map_xmin <- sf::st_bbox(sv_shp_grid_outline)$xmin - map_x_offset
  map_xmax <- sf::st_bbox(sv_shp_grid_outline)$xmax + map_x_offset
  map_ymin <- sf::st_bbox(sv_shp_grid_outline)$ymin - map_y_offset
  map_ymax <- sf::st_bbox(sv_shp_grid_outline)$ymax + map_y_offset
  plot(c(), c(), type="n", xaxt = "n", yaxt = "n", xlab="", ylab="",
       xlim = c(map_xmin, map_xmax),
       ylim = c(map_ymin, map_ymax),
       asp=1, bty='n')
  plot(sv_shp_sfr$geometry, add = TRUE, col = "lightblue", lwd=1, border=NA)
  plot(sv_shp_grid_outline$geometry, add = TRUE, col = "grey50", lwd=0.75)
  points(map_xs, map_ys, col='grey60', pch=16, cex=0.75)
  points(xloc, yloc, col='red', pch=16, cex=1.25)
}

#-------------------------------------------------------------------------------------------------#


# Scatterplots ------------------------------------------------------------

plot.scatterbox <- function(obs, sim, groups, xlab='Observation', ylab='Simulation', pch=1,
                            col=NULL, log=F, zero_rep=1e-01) {
  layout(matrix(c(2,5,1,3,4,4), 3, 2, byrow = TRUE), heights = c(1,5,0.5), widths = c(7,1.25))

  if (is.null(col)) {
    col <- c('#4e79a7','#f28e2b','#e15759','#76b7b2','#59a14f',
             '#edc948','#b07aa1','#ff9da7','#9c755f','#bab0ac')
  }
  if (log==T) {log = 'xy'}
  if (log==F) {log = ''}

  #-- Handle zeros (only for log plot)
  if (log=='xy') {
    obs[obs == 0] <- zero_rep
    sim[sim == 0] <- zero_rep
  }

  #-- Plot 1, main scatter
  par(mar=c(4.0, 4.3, 0.0, 0.0)) # bottom, left, top, right
  plot(obs, sim,
       ylab=ylab,
       xlab=xlab,
       type='n',
       asp=1,
       log=log,
       frame.plot=F,
       axes=F)

  #-- BG, Grid, Axes
  plim <- par('usr')
  if (log == 'xy') {
    plim <- 10^plim
    rect(10^par("usr")[1],10^par("usr")[3],10^par("usr")[2],10^par("usr")[4],col = "grey90",border=F)
    major_grid <- 10^(floor(log10(min(plim))):ceiling(log10(max(plim)))) %o% 1
    minor_grid <- 10^(floor(log10(min(plim))):ceiling(log10(max(plim)))) %o% 1:9
    axis(1,minor_grid,tck=1,col='grey95',labels=FALSE,las=1)
    axis(1,major_grid,tck=1,col='white',labels=TRUE,las=1)
    axis(2,minor_grid,tck=1,col='grey95',labels=FALSE,las=1)
    axis(2,major_grid,tck=1,col='white',labels=TRUE,las=1)
  } else {
    rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "grey90",border=F)
    grid(NULL, NULL ,col = 'grey93',lty = 1, lwd=1)
    axis(1)
    axis(2)
  }

  #-- Plot 1:1 line
  abline(a = 0, b = 1, lty=2)

  pdat <- data.frame('obs' = obs, 'sim' = sim, group=groups)
  i <- 1
  grp_order <- c()
  lapply(split(pdat, groups), function(grp) {
    #-- Plot Spike Data
    points(grp$obs, grp$sim, pch=pch, col=col[i])
    i <<- i + 1
    grp_order <<- c(grp_order, grp$group[1])
  })

  #-- Outline
  box()

  # Plot 2 - Top boxplot (delta 18 O)
  par(mar=c(0.0, 4.3, 0.5, 0.0)) # bottom, left, top, right

  boxplot(obs ~ groups, pdat, col=col, horizontal = T, axes=F, xlab='', ylab='',
          ylim=plim[1:2], xaxs="i",yaxs="i", log=substr(log,1,1))

  # Plot 3 - Side boxplot (delta 2H)
  par(mar=c(4.0, 0.0, 0.0, 0.5)) # bottom, left, top, right

  boxplot(sim ~ groups, pdat, col=col, axes=F, xlab='', ylab='',
          ylim=plim[3:4], xaxs="i",yaxs="i", log=substr(log,2,2))

  # Plot 4 - Legend
  par(mar=c(0.0, 0.5, 0.0, 0.5)) # bottom, left, top, right
  plot(c(0),c(1), type='n', axes=F, xlab='', ylab='')
  legend(x='center',
         bg=F,
         legend=c('1:1 line', grp_order),
         col=c('black', col),
         ncol=6,
         pch=c(NA, rep(pch, length(unique(groups)))),
         lty=c(2, rep(NA, length(unique(groups)))),
         bty='n',
         cex=1.0)

  # Plot 5 - Fakeout
  par(mar=c(0.0, 0.0, 0.5, 0.5)) # bottom, left, top, right
  plot(c(0),c(1), type='n', axes=F, xlab='', ylab='')
}

