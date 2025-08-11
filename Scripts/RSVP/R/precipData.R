# Functions from former SVIHM_input_analyses.R

#-------------------------------------------------------------------------------------------------#

#' Get table of daily precipitation
#'
#' @param start_date Start of data period
#' @param end_date End of data period
#' @param ref_data_dir Reference data directory
#'                     (optional, default: SVIHM/SVIHM_Input_Files/reference_data)
#' @param na_fill string of NA filling behavior, 'average' for averages of the preceding and next-day-with-data values,
#' pass a numeric value to fill NA with that value, or NA for no filling (default: 'average')
#' @param use_corrected_fj T/F use local file to correct Fort Jones (FJ) precip data (NOAA API has missing values present in file)
#' @param verbose T/F write status info to console (default: TRUE)
#'
#' @return
#' @author Claire Kouba, Leland Scantlebury
#' @export
#'
#' @examples
get_daily_precip_table <- function(start_date,
                                   end_date,
                                   ref_data_dir=data_dir['ref_data_dir','loc'],
                                   na_fill='average',
                                   use_corrected_fj=TRUE,
                                   verbose=TRUE) {

  # Use pre-created SVIHM stations dataset
  data("noaa_stations", package = 'RSVP')

  # Obtain precip data from NOAA API
  if (verbose) {message('Obtaining NOAA Precipitation Data')}
  noaa_prcp <- download_meteo_data(noaa_stations$id, start_date, end_date, datatypeid = "PRCP")

  # Inject fort jones overwrite data (data missing from NOAA API, but present in NOAA PDFs)
  if (use_corrected_fj) {
    fj_corrected <- read.csv(file.path(data_dir['ref_data_dir','loc'], 'fj_precip_missingdata.csv'))
    fj_corrected$Date <- as.Date(fj_corrected$Date, '%m/%d/%Y')
    fj_corrected$precip_pdf_mm <- fj_corrected$precip_pdf_in * 25.4
    fj_temp <- merge(noaa_prcp[noaa_prcp$id==noaa_stations[noaa_stations$name_short=='fj','id'],],
                     fj_corrected, by.x='date', by.y='Date', all.x=T)
    fj_temp[!is.na(fj_temp$precip_pdf_mm),'prcp'] <- fj_temp[!is.na(fj_temp$precip_pdf_mm), 'precip_pdf_mm']
    # A little safety check
    if (nrow(noaa_prcp[noaa_prcp$id==noaa_stations[noaa_stations$name_short=='fj','id'],]) == nrow(fj_temp)) {
      message(paste('Correcting',length(fj_temp[!is.na(fj_temp$precip_pdf_mm),'prcp']),'values missing from FJ precip gauge data'))
      noaa_prcp[noaa_prcp$id==noaa_stations[noaa_stations$name_short=='fj','id'],'prcp'] <- fj_temp$prcp
    } else {
      stop('Error adding corrected FJ precip data.')
    }
  }

  # Create station-to-station distance matrix
  #TODO This isn't being used, right?
  station_dist <- calc_station_dist_matrix(noaa_stations) #station_info[[2]]

  # Generate daily precip table to make model coefficients
  # Each station is a column (plus a couple extra columns); each row is a date, WY 1944-2019
  if (verbose) {message('Filling data gaps using regression')}
  daily_precip_regression = make_daily_precip(weather_table = noaa_prcp,
                                              station_table = noaa_stations,
                                              daily_precip_start_date = as.Date("1943-10-01"),
                                              daily_precip_end_date = as.Date("2018-09-30"),
                                              ref_data_dir = ref_data_dir)

  # TODO: Couldn't the above just be run *once* and then subset when passed to make_model_coeffs_table
  #       (see second call of make_daily_precip, below)

  # Get coefficients for performing gap-filling regression
  model_coeff = make_model_coeffs_table(months = 1:12,
                                        daily_precip = daily_precip_regression,
                                        ys = c("fj", "cal", "gv"),
                                        xs = c("fj", "cal", "gv", "et", "yr", "y2"))

  # Generate daily precip table to make the gap-filled records
  daily_precip_p_record = make_daily_precip(weather_table = noaa_prcp,
                                            station_table = noaa_stations,
                                            daily_precip_start_date = start_date,
                                            daily_precip_end_date = end_date,
                                            ref_data_dir = ref_data_dir)

  p_record = fill_fj_cal_gv_gaps_regression_table(model_coeff = model_coeff,
                                                  station_table = noaa_stations,
                                                  daily_precip = daily_precip_p_record,
                                                  start_date = start_date,
                                                  end_date = end_date)

  # average interp-fj and interp-cal records and compare to original
  # p_record$interp_cal_fj_gv_mean =  apply(X = dplyr::select(p_record, fj_interp, cal_interp, gv_interp),
  #                                              MARGIN = 1, FUN = mean, na.rm=T)

  # p_record$interp_cal_fj_gv_mean =  apply(X = dplyr::select(p_record, PRCP_mm_fj, PRCP_mm_cal, PRCP_mm_gv),
  #                                         MARGIN = 1, FUN = mean, na.rm=T)
  # p_record$interp_cal_fj_mean =  apply(X = dplyr::select(p_record, PRCP_mm_fj, PRCP_mm_cal),
  #                                         MARGIN = 1, FUN = mean, na.rm=T)
  # p_record$interp_cal_gv_mean =  apply(X = dplyr::select(p_record, PRCP_mm_gv, PRCP_mm_cal),
  #                                      MARGIN = 1, FUN = mean, na.rm=T)
  if (verbose) {message('Calculating mean precip value for SVIHM')}
  p_record$interp_cal_fj_gv_mean =  apply(X = dplyr::select(p_record, fj_interp, cal_interp, gv_interp),
                                          MARGIN = 1, FUN = mean, na.rm=T)
  p_record$interp_cal_fj_mean =  apply(X = dplyr::select(p_record,fj_interp, cal_interp),
                                       MARGIN = 1, FUN = mean, na.rm=T)
  p_record$interp_cal_gv_mean =  apply(X = dplyr::select(p_record, gv_interp, cal_interp,),
                                       MARGIN = 1, FUN = mean, na.rm=T)

  # Prepare to combine original precip and new regressed gap-filled fj-cal average
  p_record$stitched = p_record$PRCP_mm_orig
  p_record$stitched[is.na(p_record$PRCP_mm_orig)] = p_record$interp_cal_fj_mean[is.na(p_record$PRCP_mm_orig)]

  # Fill any remaining NaN values (days with no rainfall data at any of the 6 stations)
  # with averages of the preceding and next-day-with-data values
  if (na_fill=='average') {
    nan_indices = which(is.nan(p_record$stitched))
    for(i in 1:length(nan_indices)){
      nan_index = nan_indices[i]
      days_with_data_indices = which(!is.nan(p_record$stitched))
      next_index_with_data = min(days_with_data_indices[days_with_data_indices > nan_index])
      if (nan_index == nrow(p_record)) {
        # If the last day is an NA... a little trick to average between previous and zero
        next_index_with_data = nrow(p_record)
        p_record$stitched[next_index_with_data] <- 0
      }
      p_record$stitched[nan_index] = mean(c(p_record$stitched[nan_index - 1],
                                          p_record$stitched[next_index_with_data]))
    }
  } else if (!is.na(na_fill)) {
    na_fill = as.numeric(na_fill)
    if (nrow(p_record[is.na(p_record$stitched),]) > 0) {
      message(paste('-',nrow(p_record[is.na(p_record$stitched),]), 'missing values filled with', na_fill))
      p_record[is.na(p_record$stitched), 'stitched'] <- na_fill
    }
  }
  #Note: this include filling the leap days missing in the original record :)


  # orig_record_end_date = as.Date("2011-09-30"); orig_record_start_date = as.Date("1990-10-01")
  # orig_record = p_record$Date <= orig_record_end_date & p_record$Date >= orig_record_start_date
  # updated_record = p_record$Date > orig_record_end_date
  #
  # #Fill in 5 leap days in the original record using the gap-filled cal-FJ record
  # leap_day_finder_pre2011 = orig_record & is.na(p_record$PRCP_mm_orig)
  # p_record$PRCP_mm_orig[leap_day_finder_pre2011] = p_record$interp_cal_fj_mean[leap_day_finder_pre2011]
  #
  # #Subset original record
  # p_record$stitched = NA
  # p_record$stitched[orig_record] = p_record$PRCP_mm_orig[orig_record]
  # p_record$stitched[updated_record] = p_record$interp_cal_fj_mean[updated_record]



  if (verbose) {message('Precipitation data processing complete.')}
  return(p_record)
}

#-------------------------------------------------------------------------------------------------#

#' Download (Daily) Meteorological Data for Gauges from NOAA
#' Wrapper for rnoaa function meteo_pull_monitors. Converts data from tenths (mm, degrees) to full
#' value (i.e., from 1/10 mm to mm)
#'
#' @param station_ids array of station ids to obtain data from
#' @param start_date  Start of data period
#' @param end_date    End of data period
#' @param datatypeid  Accepts a valid data type id or a vector or list of data type ids. (optional)
#'                    (see rnoaa documentation, passed to rnoaa::meteo_pull_monitors as var)
#'
#' @return tibble of precip data (long data, for all monitors)
#' @author Leland Scantlebury
#' @export
#'
#' @examples
#' noaa <- download_meteo_data(c("USC00041316", "USC00043182"),
#'                             start_date = '2012-10-01',
#'                             end_date = '2013-09-30',
#'                             datatypeid = "PRCP")
download_meteo_data <- function(station_ids, start_date, end_date, datatypeid="all") {
  df <- rnoaa::meteo_pull_monitors(monitors = station_ids,
                                   keep_flags = FALSE,
                                   date_min = as.Date(start_date),
                                   date_max = as.Date(end_date),
                                   var = datatypeid)
  # First 2 columns are id & date - only changes values in data columns (3+)
  df[,3:ncol(df)] <- df[,3:ncol(df)]/10.0

  return(df)
}

#-------------------------------------------------------------------------------------------------#

#TODO: Should this just be pre-calculated data? IS THIS EVEN USED?
#' Calculate station-to-station distance matrix
#'
#' @param stations DataFrame with station locations as columns latitude, longitude
#'                 and column name_short with short names for stations (row, col headers)
#'
#' @return Symmetric Matrix of distances between stations
#' @export
#'
#' @examples
calc_station_dist_matrix = function(stations){

  # Make station_table a SpatialPointsDataFrame and add proj
  sp::coordinates(stations) = ~longitude + latitude
  sp::proj4string(stations) = sp::CRS("+init=epsg:4326")

  # Use coordinates to calculate distance between all the stations
  station_dist = raster::pointDistance(stations, lonlat=T,  allpairs = T)

  # Make the matrix easier to query later
  station_dist = as.matrix(Matrix::forceSymmetric(station_dist, uplo = "L"))
  diag(station_dist) = NA  # Ls - why not zero??
  station_dist = as.data.frame(station_dist)
  rownames(station_dist) = stations$name_short; colnames(station_dist) = stations$name_short

  return(station_dist)
}

#-------------------------------------------------------------------------------------------------#

#' Make Daily Precip
#'
#' Creates daily precip dataframe out of (1) downloaded noaa data (weather_table) and (2) the
#' "original" model 1991-2011 precipitation data. Each datasource is in a different column.
#'
#' @param weather_table DataFrame of weather data from RSVP::download_meteo_data or equivalent
#'                      long precipitation data
#' @param station_table Dataframe of station info (i.e., data(noaa_stations))
#' @param daily_precip_start_date Start of data period
#' @param daily_precip_end_date End of data period
#' @param ref_data_dir Reference data directory
#'
#' @author Claire Kouba, Leland Scantlebury
#' @return DataFrame with all data sources and no missing days (but may have missing data)
#' @export
#'
#' @examples
make_daily_precip = function(weather_table,
                             station_table,
                             daily_precip_start_date,
                             daily_precip_end_date,
                             ref_data_dir){

  # Build all-dates list
  record_days = seq(from = daily_precip_start_date, to = daily_precip_end_date, by = "days")
  daily_precip = data.frame("Date" = record_days)

  # Read in original data (wys 1991-2011)
  daily_precip_orig = read.table(file.path(ref_data_dir,"precip_1991_2011.txt"))
  colnames(daily_precip_orig) = c("PRCP_mm_orig", "date")
  daily_precip_orig$date = as.Date(daily_precip_orig$date, format = "%d/%m/%Y")
  daily_precip_orig$PRCP_mm_orig = daily_precip_orig$PRCP_mm_orig*1000

  #Subset data into stations
  # cal = subset(weather_table, STATION=="USC00041316" & DATE >= daily_precip_start_date & DATE <= daily_precip_end_date)
  # cal = data.frame(DATE = cal$DATE, PRCP = cal$PRCP)
  # fj = subset(weather_table, STATION=="USC00043182" & DATE >= daily_precip_start_date & DATE <= daily_precip_end_date)
  # fj = data.frame(DATE = fj$DATE, PRCP = fj$PRCP)
  # et = subset(weather_table, STATION == "USC00042899" & DATE >= daily_precip_start_date & DATE <= daily_precip_end_date)
  # et =data.frame(DATE = et$DATE, PRCP = et$PRCP)
  # gv = subset(weather_table, STATION == "USC00043614" & DATE >= daily_precip_start_date & DATE <= daily_precip_end_date)
  # gv =data.frame(DATE = gv$DATE, PRCP = gv$PRCP)
  # yr = subset(weather_table, STATION == "USC00049866" & DATE >= daily_precip_start_date & DATE <= daily_precip_end_date)
  # yr =data.frame(DATE = yr$DATE, PRCP = yr$PRCP)
  # y2 = subset(weather_table, STATION == "US1CASK0005" & DATE >= daily_precip_start_date & DATE <= daily_precip_end_date)
  # y2 =data.frame(DATE = y2$DATE, PRCP = y2$PRCP)

  ### COMPARISON TABLE FOR 2 STATIONS AND ORIG DATA
  # daily_precip = merge(x = daily_precip, y = cal, by.x = "record_days", by.y = "DATE", all=TRUE)
  # daily_precip = merge(x = daily_precip, y = fj, by.x = "record_days", by.y = "DATE", all=TRUE)
  # daily_precip = merge(x = daily_precip, y = et, by.x = "record_days", by.y = "DATE", all=TRUE)
  # daily_precip = merge(x = daily_precip, y = gv, by.x = "record_days", by.y = "DATE", all=TRUE)
  # daily_precip = merge(x = daily_precip, y = yr, by.x = "record_days", by.y = "DATE", all=TRUE)
  # daily_precip = merge(x = daily_precip, y = y2, by.x = "record_days", by.y = "DATE", all=TRUE)

  # LS - replaced with a loop
  # Combine precipitation datasets into a single DF, each with their own column
  for (i in 1:nrow(station_table)) {
    stn_id <- station_table[i,'id']
    stn_abbrv <- station_table[i,'name_short']
    stn_sub <- subset(weather_table, id==stn_id & date >= daily_precip_start_date & date <= daily_precip_end_date)
    new_col_name <- paste0('PRCP_mm_', stn_abbrv)
    colnames(stn_sub) <- c('id', 'date', new_col_name)
    daily_precip = merge(x = daily_precip,
                         y = stn_sub[c('date',new_col_name)],
                         by.x = "Date",
                         by.y = "date",
                         all.x=TRUE)
  }

  # Manually add in "original" model precip data
  daily_precip = merge(x = daily_precip, y = daily_precip_orig, by.x = "Date", by.y = "date", all=TRUE)

  #colnames(daily_precip)=c("Date", paste0('PRCP_mm_',noaa_stations$name_short), )

  # Compare original data to FJ-Cal mean
  daily_precip$mean_PRCP_fjcal = apply(X = daily_precip[,2:3], MARGIN = 1, FUN = mean, na.rm=T)

  #Add aggregation columns
  daily_precip$month_day1 = lubridate::floor_date(daily_precip$Date, "month")
  daily_precip$water_year <- RSVP::get_water_year(daily_precip$Date)

  return(daily_precip)
}

#-------------------------------------------------------------------------------------------------#

#' Create Precipitation Gauge Linear Regression Coefficient Table
#'
#' Performs regressions on the various gauges (columns) of the daily_precip table to obtain linear
#' coefficents for gauge combinations between those in xs and those in ys.
#'
#' Expects the xs and ys to be datasets in daily_precip under the columns of 'PRCP_mm_(xs/ys)'
#'
#' @param months numeric months to be used in regression
#' @param daily_precip DataFrame with columns Date and PRCP_mm_XX, from make_daily_precip()
#' @param ys list of station abbreviations for predicting at
#' @param xs list of station abbreviations used to predict at ys
#'
#' @return DataFrame of coeffients
#' @author Claire Kouba, Leland Scantlebury
#' @export
#'
#' @examples
make_model_coeffs_table <- function(months = 1:12,
                                    station_table,
                                    daily_precip,
                                    ys = c("fj", "cal"),
                                    xs = c("fj", "cal", "gv", "et", "yr", "y2")
                                    ){

  model_coeff = expand.grid(months = months, Y_var = ys, X_var = xs)

  # Take out matching combinations (e.g. x = fj, y = fj)
  model_coeff = model_coeff[as.character(model_coeff$Y_var) != as.character(model_coeff$X_var),]

  # Add model parameter columns
  model_coeff$intercept=NA; model_coeff$coeff_m=NA; model_coeff$r2=NA

  for(mnth in months){
    for(y in ys){
      for(x in xs){
        if(y == x){next} #no need for autoregression
        # Find the output table row
        model_coeff_index = which(model_coeff$months==mnth & model_coeff$X_var==x & model_coeff$Y_var==y)

        # Gen col names of x & y
        col_name_x <- paste0('PRCP_mm_', x)
        col_name_y <- paste0('PRCP_mm_', y)

        # Declare X and Y in daily precip
        X = daily_precip[lubridate::month(daily_precip$Date) == mnth, col_name_x]
        Y = daily_precip[lubridate::month(daily_precip$Date) == mnth, col_name_y]

        model_name = paste(y, "on", x, "in", month.abb[mnth] )

        #check to see if there are 0 matching pairs
        if(sum(!is.na(X) & !is.na(Y))==0){next}

        model = lm(Y~ 0 + X)
        # CASE DESCRIPTION?
        if(length(coef(model)) ==2){print(paste("2 coeff of this model", mnth,X,Y))
          model_coeff[model_coeff_index, 4:5] = coef(model)}
        if(length(coef(model)) ==1){
          model_coeff[model_coeff_index, 4] = 0; model_coeff[model_coeff_index, 5] = coef(model)}
        model_coeff$r2[model_coeff_index] = summary(model)$r.sq
      }
    }
  }
  return(model_coeff)
}

#-------------------------------------------------------------------------------------------------#

#' Fill Fort Jones, Cal, and Greenview Gaps using Regression
#'
#' Use best available regression to fill in gaps in precip records for FJ and Cal
#'
#' @param model_coeff Model coefficients generated using make_model_coeffs_table
#' @param station_table Dataframe of station info (i.e., data(noaa_stations))
#' @param daily_precip DataFrame with columns Date and PRCP_mm_XX, from make_daily_precip()
#' @param start_date Start of data period
#' @param end_date End of data period
#'
#' @return
#' @author Claire Kouba
#' @export
#'
#' @examples
fill_fj_cal_gv_gaps_regression_table = function(model_coeff,
                                                station_table,
                                                daily_precip,
                                                start_date,
                                                end_date){

  #Rename daily precip table and restrict dates
  p_record = daily_precip[daily_precip$Date >= start_date & daily_precip$Date <= end_date,]

  ### Fill in gaps in FJ
  # Initialize the interpolated record as the official FJ record (with gaps)
  p_record$fj_interp = p_record$PRCP_mm_fj
  # Assign colors for the station attribution plot, indicating that we got these values from the FJ station.
  p_record$fj_interp_color[!is.na(p_record$fj_interp)] = station_table$color[station_table$name_short == "fj"]

  # For each NA daily precip value in the FJ record, predict the value in the FJ record using the regression
  # coefficients from the station with the best R^2 value for predicting FJ.
  # (If that station also has a gap on that day, use the one with the next-best R^2 value, until the gap is filled.)
  for(i in 1:length(p_record$fj_interp)){
    if(is.na(p_record$fj_interp[i])){
      #find the appropriate regression coefficients for this month of fj data
      coefs = model_coeff[model_coeff$Y_var == "fj" & model_coeff$months == lubridate::month(p_record$Date[i]), ]
      index_of_ranks = rev(order(coefs$r2)) # test the regressions in order of best R2 to worst

      #Predict rainfall
      for(j in 1:length(index_of_ranks)){
        if(is.na(p_record$fj_interp[i])){ #If this daily value didn't get filled by a previous calc in this for loop
          # Use coef matrix to assign X variable station for the regression, based on ranked R^2
          coef_index = index_of_ranks[j]
          coef_x = coefs$X_var[coef_index]
          # Assign the column number and indicator color of the relevant predictor station
          daily_precip_column_num = which(colnames(daily_precip)==paste0('PRCP_mm_', coef_x))
          daily_precip_x_var_color = station_table$color[station_table$name_short == coef_x]
          # If there's data for that X variable, use it to predict rainfall at FJ for that day. also assign color.

          if(!is.na(p_record[i,daily_precip_column_num])){
            p_record$fj_interp[i] =  coefs$intercept[coef_index] + (p_record[i, daily_precip_column_num] * coefs$coeff_m[coef_index])
            p_record$fj_interp_color[i] = daily_precip_x_var_color
          }
          #If there's no data for the best-ranked variable, it will go back to the beginning of the for loop and try again with other stations
        }
      }

    }
  }

  ### Fill in gaps in Cal
  # Initialize the interpolated record as the official Cal record (with gaps)
  p_record$cal_interp = p_record$PRCP_mm_cal
  # Assign colors for the station attribution plot, indicating that we got these values from the Cal station.
  p_record$cal_interp_color[!is.na(p_record$cal_interp)] = station_table$color[station_table$name_short == "cal"]

  for(i in 1:length(p_record$cal_interp)){
    if(is.na(p_record$cal_interp[i])){
      #find the appropriate regression coefficients for this month of Cal data
      coefs = model_coeff[model_coeff$Y_var == "cal" & model_coeff$months == lubridate::month(p_record$Date[i]), ]
      index_of_ranks = rev(order(coefs$r2)) # test the regressions in order of best R2 to worst

      #Predict rainfall
      for(j in 1:length(index_of_ranks)){
        if(is.na(p_record$cal_interp[i])){ #If this daily value didn't get filled by a previous calc in this for loop
          # Use coef matrix to assign X variable station for the regression, based on ranked R^2
          coef_index = index_of_ranks[j]
          coef_x = coefs$X_var[coef_index]
          daily_precip_column_num = which(colnames(daily_precip)==paste0('PRCP_mm_', coef_x))
          daily_precip_x_var_color = station_table$color[station_table$name_short == coef_x]
          # If there's data for that X variable, use it to predict rainfall at cal for that day
          if(!is.na(p_record[i,daily_precip_column_num])){
            p_record$cal_interp[i] =  coefs$intercept[coef_index] + (p_record[i, daily_precip_column_num] * coefs$coeff_m[coef_index])
            p_record$cal_interp_color[i] =daily_precip_x_var_color
          }
          #If there's no data for the best-ranked variable, it will go back to the beginning of the for loop and try again with other stations
        }
      }

    }
  }

  ### Fill in gaps in Greenview
  # Initialize the interpolated record as the official Greenview record (with gaps)
  p_record$gv_interp = p_record$PRCP_mm_gv
  # Assign colors for the station attribution plot, indicating that we got these values from the Greenview station.
  p_record$gv_interp_color[!is.na(p_record$gv_interp)] = station_table$color[station_table$name_short == "gv"]

  for(i in 1:length(p_record$gv_interp)){
    if(is.na(p_record$gv_interp[i])){
      #find the appropriate regression coefficients for this month of Greenview data
      coefs = model_coeff[model_coeff$Y_var == "gv" & model_coeff$months == lubridate::month(p_record$Date[i]), ]
      index_of_ranks = rev(order(coefs$r2)) # test the regressions in order of best R2 to worst

      #Predict rainfall
      for(j in 1:length(index_of_ranks)){
        if(is.na(p_record$gv_interp[i])){ #If this daily value didn't get filled by a previous calc in this for loop
          # Use coef matrix to assign X variable station for the regression, based on ranked R^2
          coef_index = index_of_ranks[j]
          coef_x = coefs$X_var[coef_index]
          daily_precip_column_num = which(colnames(daily_precip)==paste0('PRCP_mm_', coef_x))
          daily_precip_x_var_color = station_table$color[station_table$name_short == coef_x]
          # If there's data for that X variable, use it to predict rainfall at gv for that day
          if(!is.na(p_record[i,daily_precip_column_num])){
            p_record$gv_interp[i] =  coefs$intercept[coef_index] + (p_record[i, daily_precip_column_num] * coefs$coeff_m[coef_index])
            p_record$gv_interp_color[i] =daily_precip_x_var_color
          }
          #If there's no data for the best-ranked variable, it will go back to the beginning of the for loop and try again with other stations
        }
      }

    }
  }

  return(p_record)
}


#-------------------------------------------------------------------------------------------------#

#' Write SWBM Precipitation Input File
#'
#' Writes the Soil Water Balance Model (SWBM) precipitation input file (precip.txt). Allows for a
#' another time series data frame to serve as an "overwrite" for the data via a merge. This is to
#' try to accomodate different regression analyses used throughout time.
#'
#' Uses the "stitched" column of the p_record as the final output (after merge_dataset merge)
#'
#' @param p_record DataFrame of precipitation data, as generated by get_daily_precip_table()
#'                 Expects, at a minimum, columns "stitched" and "Date"
#' @param output_dir Directory to write file
#' @param merge_dataset DataFrame with columns "Date" and "PRCP" to use to overwrite values in p_record.
#'                      All non-NA values will be used to overwrite p_record
#' @param filename Filename (optional, default: precip_regressed.txt)
#' @param verbose T/F write status info to console (default: TRUE)
#'
#' @return None
#' @author Leland Scantlebury
#' @export
#'
#' @examples
write_swbm_precip_input_file <- function(p_record,
                                         output_dir,
                                         merge_dataset=NULL,
                                         filename="precip_regressed.txt",
                                         verbose=TRUE){

  daily_precip_updated = data.frame(PRCP = p_record$stitched, Date = p_record$Date)

  if (!is.null(merge_dataset)) {
    #TODO Test
    if(!"PRCP" %in% colnames(merge_dataset))
    {
      stop('Merge Dataset missing PRCP column')
    }
    p_record <- merge(x = p_record, y = merge_dataset, by.x = "Date", by.y = "Date", all=TRUE)
    p_record[!is.na(p_record$PRCP),'stitched'] <- p_record[!is.na(p_record$PRCP), 'PRCP']
  }

  daily_precip_updated$Date <- format(daily_precip_updated$Date, '%d/%m/%Y')
  daily_precip_updated$PRCP = as.character(daily_precip_updated$PRCP / 1000) #convert to meters

  if (verbose) {message(paste('Writing file: ', filename))}
  write.table(daily_precip_updated, file = file.path(output_dir, filename),
              sep = " ", quote = FALSE, col.names = FALSE, row.names = FALSE) # Current SWBM (9/25/2023) doesn't use a header on THIS file

}


#-------------------------------------------------------------------------------------------------#

#' Calculate SWBM Precipitation Factors
#'
#' Relates each landuse polygon (field) to spatially distributed rainfall data.
#' Generates a multiplication factor for each field to modify the model-domain-wide daily
#' precipitation input record.
#'
#' @param poly_filename Filename containing table of land use polygons
#' @param output_dir Directory to write file
#' @param rainfall_records Dataframe containing daily precipitation records (includes
#' public weather station data and date collected from private gauges).
#' @param rainfall_locations Dataframe containing locations for each rainfall record (includes
#' public weather station data and date collected from private gauges).
#' @param verbose T/F write status info to console (default: TRUE)
#'
#' @return A two-column dataframe containing the SWBM polygon (field) ID; and the precip
#' multiplication factor.
#' @author Leland Scantlebury and Claire Kouba
#' @export
#'
#' @examples
#'
calc_swbm_spatial_precip_factors <- function(
                                          poly_tab_filename = "polygons_table.txt",
                                          poly_shapefile_filename = NA,
                                          rainfall_records = NA,
                                          rainfall_locations = NA,
                                          verbose=TRUE){

  if(is.na(poly_shapefile_filename) | is.na(rainfall_records) | is.na(rainfall_locations)){
    print("Information for spatial rainfall variability not available.")
    print("Generating precip factors corresponding to no spatial variability (all field factors = 1).")

    poly_tab = read.table(file.path(data_dir['time_indep_dir','loc'], poly_tab_filename),
                          comment.char = "!", fill = T, header = T)
    precip_factors = data.frame(SWBM_id = poly_tab$SWBM_id,
                                  ppt_fact = 1)

  } else {
    # PLACEHOLDER: spatially relate fields to krigged precip data?
    # Make different factor for each month?
  }

  return(precip_factors)

}



#-------------------------------------------------------------------------------------------------#

#' Write SWBM Precipitation Factor File
#'
#' Writes the Soil Water Balance Model (SWBM) precipitation factor file (precip_factors.txt).
#' Consists of one factor per land use polygon (i.e., field). Is used to multiply the
#' daily precip value to implement spatial precipitation gradients.
#'
#' @param output_dir Directory to write file
#' @param filename Precip factors filename
#' @param precip_factors_df A two-column dataframe containing the SWBM polygon (field) ID; and the
#' precip multiplication factor.
#' @param verbose T/F write status info to console (default: TRUE)
#'
#' @return None
#' @author Leland Scantlebury and Claire Kouba
#' @export
#'
#' @examples
write_swbm_precip_factor_file <- function(output_dir,
                                          filename = "precip_factors.txt",
                                          precip_factors_df,
                                          verbose=TRUE){

  if (verbose) {message(paste('Writing file: ', filename))}
  write.table(precip_factors_df, file = file.path(output_dir, filename),
              sep = " ", quote = FALSE, col.names = T, row.names = FALSE)

}

#-------------------------------------------------------------------------------------------------#
