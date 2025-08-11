# Functions originally from SVIHM_Streamflow_Regression_Model.R
# Modified/Documented by Leland Scantlebury

#-------------------------------------------------------------------------------------------------#

#' Read in River Gauge Daily Means
#'
#' Reads in primary gauge (Fort Jones) and input/tributary gauge daily.
#'
#' @param sf_reg_dir streamflow regression directory (optional, default: SVIHM/Streamflow_Regression_Model)
#'
#' @details Input/Tributary Gauges included:
#' * East Fork Scott River
#' * South Fork Scott River
#' * Sugar Creek
#' * Etna Creek
#' * French Creek
#' * Patterson Creek
#' * Kidder Creek
#' * Moffett Creek
#' * Mill Creek
#' * Shackleford Creek
#'
#' No Data for Clark, Johnson, Crystal, or Oro Fino Creeks.
#' @md
#'
#' @return List of daily mean dataframes (FJ followed by order above)
#' @export
#'
#' @examples
# read_gauge_daily_data <-  function(sf_reg_dir=data_dir['sf_reg_dir', 'loc']){
#
#   #-- LS Replaced with internal RSVP data
#   # Loop over streams reading in pre-saved time series datasets
#   daily_means_all = list()
#   for (i in 1:nrow(stream_metadata)) {
#     daily_means_all[[i]] <- read.table(file.path(sf_reg_dir, stream_metadata[i,'daily_mean_file']),
#                                        header = T,
#                                        stringsAsFactors = F,
#                                        sep = '\t')[,-4]
#     daily_means_all[[i]]$Date = as.Date(daily_means_all[[i]]$Date,'%m/%d/%Y')
#   }
#
#   return(daily_means_all)
# }

#-------------------------------------------------------------------------------------------------#

#' Assimilate Fort Jones Historical & New Updated Data
#'
#' Assumes that fj_update is the more accurate dataset (will drop historical days with overlap)
#'
#' @param historical DataFrame of historical (1941 - ) data
#' @param fj_update DataFrame of more recent data
#'
#' @return DataFrame of single, combined dataset
#' @export
#'
#' @examples
# assimilate_fj_update <- function(historical, fj_update) {
#   colnames(fj_update) <-  c('Data_Source','site_no','Date','mean_cfs','Qual_Flag')
#   merged <- rbind(historical[historical$Date < min(fj_update$Date),],
#                   fj_update[,c('Date','mean_cfs','Qual_Flag')])
#   return(merged)
# }

#-------------------------------------------------------------------------------------------------#

#' Generate SVIHM Streamflow Inputs
#'
#' @param end_date End of data period
#' @param fj_update DataFrame of latest Fort Jones (FJ data), from download_fort_jones_flow() (optional)
#' @param sf_reg_dir streamflow regression directory (optional, default: SVIHM/Streamflow_Regression_Model)
#' @param max_missing_days Maximum number of missing daily flow data before month is removed from
#'                         the regression (optional, default = 3)
#'
#' @return
#' @export
#'
#' @examples
generate_streamflow_input <- function(end_date,
                                      fj_update=NULL,
                                      sf_reg_dir=data_dir['sf_reg_dir', 'loc'],
                                      max_missing_days = 3,
                                      stat_cutoff_date){

  # Generate date values
  SVIHM_months = seq(from = as.Date("1990/10/1"), to = end_date,by = "month")  #Months during model simulation
  NumDays = days_in_month_diff(as.Date("1990/10/1"), end_date)

  #Date vectors for regression models
  Pre_WY1973 = seq(as.Date("1940/10/1"), by = "month", length.out = 384)  #Months Prior to WY1973
  Post_WY1972 = seq(from = as.Date("1972/10/1"), to = end_date, by = "month")  #Months After WY1972

  ### 1. Declare stream names and read input data
  stream_names = stream_metadata$name
  daily_means_all = read_gauge_daily_data(sf_reg_dir)

  ### 1.1 Handle new FJ data
  if (!is.null(fj_update)) {
    daily_means_all[[1]] <- assimilate_fj_update(daily_means_all[[1]], fj_update)
    write.csv(daily_means_all[[1]], 'FJ_reedited.csv')
  } else  {
    write.csv(daily_means_all[[1]], 'FJ_unedited.csv')
  }

  ### 1.2 Convert to other units
  # for (i in 1:length(daily_means_all)) {
  #   df_daily_mean[[i]]$ft3 = df_daily_mean$mean_cfs*86400
  #   df_daily_mean[[i]]$m3 = df_daily_mean[[i]]$ft3 * 0.0283168
  #   df_daily_mean[[i]]$AF = df_daily_mean[[i]]$ft3 * 2.2957e-5
  # }

  ### 1.3 Get Mean, SD for normalization


  ### 2. Loop over streams for pre-processing
  monthly_cleaned_all = preproflow(daily_means_all = daily_means_all,
                                   Max_Num_Missing_Days = max_missing_days,
                                   Streams = stream_names,
                                   stat_cutoff_date = stat_cutoff_date)

  ### 3. Calculate pre-WY1973 relationships between tributaries and FJ gage
  all_tribs_regress_pre_WY1973 = tribs_regress_pre_WY1973(monthly_cleaned_all = monthly_cleaned_all,
                                                          Streams = stream_names, Pre_WY1973=Pre_WY1973)

  ### 4. Calculate post-WY1972 relationships between tributaries and FJ gage
  all_tribs_regress_post_WY1972 = tribs_data_post_WY1972(monthly_cleaned_all = monthly_cleaned_all,
                                                         Streams = stream_names,
                                                         Post_WY1972=Post_WY1972,
                                                         stat_cutoff_date = stat_cutoff_date)

  ### 5. Write SVIHM coefficient tables
  coef_tables = calc_coef_tables(all_tribs_regress_pre_WY1973 = all_tribs_regress_pre_WY1973,
                                   all_tribs_regress_post_WY1972 = all_tribs_regress_post_WY1972,
                                   Streams=stream_names)
  Regression_Coefs_Pre_WY1973 = coef_tables[[1]]
  Regression_Coefs_Post_WY1972 = coef_tables[[2]]

  ### Use regressions to predict monthly average flows in tributaries for model period
  model_period_flows = populate_SVIHM_flows(monthly_cleaned_all = monthly_cleaned_all,
                                            Regression_Coefs_Pre_WY1973 = Regression_Coefs_Pre_WY1973,
                                            Regression_Coefs_Post_WY1972 = Regression_Coefs_Post_WY1972,
                                            Streams=stream_names, SVIHM_months = SVIHM_months, NumDays = NumDays)


  ### 5. Make assumptions to fill in no-flow data for Johnson or Crystal Creeks
  Patterson_Model_Inputs=model_period_flows[[7-1]]
  Johnson_Model_Inputs = data.frame(Month = SVIHM_months,
                                    avg_flow_m3day = Patterson_Model_Inputs$avg_flow_m3day * 0.2)  #Johnson inflows estimated at 20% of Patterson inflows
  Crystal_Model_Inputs = data.frame(Month = SVIHM_months,
                                    avg_flow_m3day = Patterson_Model_Inputs$avg_flow_m3day * 0.15) #Crystal inflows estimated at 15% of Patterson inflows
  johnson_crystal = data.frame(Johnson_Avg_Flow_m3day = Johnson_Model_Inputs$avg_flow_m3day,
                               Crystal_Avg_Flow_m3day = Crystal_Model_Inputs$avg_flow_m3day)
  #No inflow for Oro Fino Creek and Clark Creek is not currently included in the model


  ### 6. Generate final SVIHM streamflow inputs data frame and write file

  #Initialize dataframe
  stream_out = data.frame(Month = SVIHM_months)

   #Add a column of average monthly flows, for the model period, for each tributary
  for(i in 1:length(model_period_flows)){
    trib_model_period = model_period_flows[[i]]
    colnames_new = c(colnames(stream_out),paste0(stream_names[i+1],"_Avg_Flow_m3day"))

    stream_out = cbind(stream_out, trib_model_period$avg_flow_m3day)
    colnames(stream_out) = colnames_new
  }

  #Insert records for two ungaged columns
  stream_out = cbind(stream_out, johnson_crystal)

  #Reorder to maintain tributary order in model input files
  trib_order = c(1:4, 6, 5, 12, 13, 7:11)
  stream_out = stream_out[,trib_order]

  return(stream_out)
}

#-------------------------------------------------------------------------------------------------#

########     Streamflow Pre-Processing Function
#Pre processing of tributary streamflow. Cleans and log-normalizes monthly flow volumes
#Returns a list (one for each tributary) of dataframes of monthly flow data.
preproflow = function(daily_means_all, Max_Num_Missing_Days, Streams, stat_cutoff_date) {
  #Initialize empty output list
  monthly_cleaned_all = list()

  #Process each stream record
  for (i in 1:length(Streams)){
    Stream_name = Streams[i]
    #df_daily_mean = eval(parse(text = paste0(Streams[i],'_daily_mean'))) #Old global variables structure
    df_daily_mean = daily_means_all[[i]]

    # Calc mean/sd on a subset keep them constant despite updates in data
    log_stats <- calc_mean_sd_before_date(df_daily_mean, stat_cutoff_date, Max_Num_Missing_Days)
    use_log_mean <- log_stats[[1]]
    use_log_sd   <- log_stats[[2]]

    #df_daily_mean$Date = as.Date(df_daily_mean$Date,'%m/%d/%Y')  # Done during read
    df_daily_mean$Month = format(df_daily_mean$Date, format = '%m-%Y')
    df_daily_mean$ft3 = df_daily_mean$mean_cfs*86400

    #Monthly Volume
    df_Monthly = aggregate(.~Month,df_daily_mean[c('Month', 'ft3')], FUN = sum, na.rm = T)
    df_Monthly$Month = as.Date(paste0(df_Monthly$Month,'-01'),'%m-%Y-%d')
    df_Monthly = df_Monthly[order(df_Monthly$Month),]
    df_Monthly$m3 = df_Monthly$ft3 * 0.0283168
    df_Monthly$AF = df_Monthly$ft3 * 2.2957e-5
    df_Monthly$log_AF = log10(df_Monthly$AF)
    df_missing_data = subset(df_daily_mean,Qual_Flag == 151 |
                               Qual_Flag == 160 |
                               Qual_Flag == 255)                                          #Extract months with missing data
    if (nrow(df_missing_data)>=1){                                                        #Delete months with too many data points missing
      df_missing_data = as.data.frame(table(df_missing_data$Month))                       #Count number of missing days for each month
      names(df_missing_data) = c('Month','Count')                                         #rename columns
      df_missing_data$Month = as.Date(paste0(df_missing_data$Month,'-01'),'%m-%Y-%d')     #Convert to date
      df_missing_data = df_missing_data[order(df_missing_data$Month),]                    #sort dataframe by month
      df_missing_data = subset(df_missing_data,Count>Max_Num_Missing_Days)                #Keep months missing more days than maximum specified by Max_Num_Missing_Days
      df_monthly_cleaned = subset(df_Monthly, ! Month %in%  df_missing_data$Month)        #Remove months missing too many days of data
      #df_mean_log = mean(df_monthly_cleaned$log_AF)                                       #calculate mean of monthly volume in acre-ft
      #df_SD_log = sd(df_monthly_cleaned$log_AF)                                           #calculate standard deviation of monthly volume in acre-ft
      # df_monthly_cleaned$norm_log_AF = (df_monthly_cleaned$log_AF -
      #                                     df_mean_log)/df_SD_log                          #normalize log-transformed volumes in ac-ft
    } else {
      df_monthly_cleaned = df_Monthly
      #df_mean_log = mean(df_monthly_cleaned$log_AF)                                       #calculate mean of monthly volume in acre-ft
      #df_SD_log = sd(df_monthly_cleaned$log_AF)                                           #calculate standard deviation of monthly volume in acre-ft
      # df_monthly_cleaned$norm_log_AF = (df_monthly_cleaned$log_AF -
      #                                     df_mean_log)/df_SD_log                          #normalize log-transformed volumes in ac-ft
    }

    df_monthly_cleaned$norm_log_AF = (df_monthly_cleaned$log_AF -
                                        use_log_mean)/use_log_sd                          #normalize log-transformed volumes in ac-ft

    if (i==1) {
      #TODO Delete!
      print(paste('FJ Mean:', use_log_mean, '| Std. Dev.:', use_log_sd))
    }
    output_df_name = paste0(Stream_name,'_monthly_cleaned')                  #name of tributary output data frame
    output_mean_name = paste0(Stream_name,'_mean_log')                       #name of tributary variable for mean of log flows
    output_sd_name =  paste0(Stream_name,'_SD_log')                         #name of tributary variable for standard deviation of log flows
    # eval(parse(text = paste0(output_df_name,' <<- df_monthly_cleaned')))   #export dataframe
    # eval(parse(text = paste0(output_mean_name,' <<- df_mean_log')))        #export mean
    # eval(parse(text = paste0(output_sd_name,' <<- df_SD_log')))            #export standard deviation

    monthly_cleaned_all[[i]] = df_monthly_cleaned
  }
  return(monthly_cleaned_all)
}

#-------------------------------------------------------------------------------------------------#

#' Calculate Monthly Mean & Standard Deviation using data before a certain date
#'
#' Very case-specific function created to ensure the means & std devs used for normalization do
#' not change over time (barring corrections to earlier data). Repeats many of the calculations
#' in RSVP::preproflow() but on a data subset.
#'
#' @param df_daily_mean dataframe of daily average flows (columns: Date, mean_cfs, Qual_Flag)
#' @param stat_cutoff_date date after which data should not be included in the mean and sd
#'
#' @return
#' @export
#'
#' @examples
calc_mean_sd_before_date <- function(df_daily_mean, stat_cutoff_date, Max_Num_Missing_Days) {
  # Subset to previous analysis date to keep mean/sd constant
  df_daily_mean <- df_daily_mean[df_daily_mean$Date <= stat_cutoff_date,]

  df_daily_mean$Month = format(df_daily_mean$Date, format = '%m-%Y')
  df_daily_mean$ft3 = df_daily_mean$mean_cfs*86400

  #Monthly Volume
  df_Monthly = aggregate(.~Month,df_daily_mean[c('Month', 'ft3')], FUN = sum, na.rm = T)
  df_Monthly$Month = as.Date(paste0(df_Monthly$Month,'-01'),'%m-%Y-%d')
  df_Monthly = df_Monthly[order(df_Monthly$Month),]
  df_Monthly$m3 = df_Monthly$ft3 * 0.0283168
  df_Monthly$AF = df_Monthly$ft3 * 2.2957e-5
  df_Monthly$log_AF = log10(df_Monthly$AF)
  df_missing_data = subset(df_daily_mean,Qual_Flag == 151 |
                             Qual_Flag == 160 |
                             Qual_Flag == 255)                                          #Extract months with missing data
  if (nrow(df_missing_data)>=1){                                                        #Delete months with too many data points missing
    df_missing_data = as.data.frame(table(df_missing_data$Month))                       #Count number of missing days for each month
    names(df_missing_data) = c('Month','Count')                                         #rename columns
    df_missing_data$Month = as.Date(paste0(df_missing_data$Month,'-01'),'%m-%Y-%d')     #Convert to date
    df_missing_data = df_missing_data[order(df_missing_data$Month),]                    #sort dataframe by month
    df_missing_data = subset(df_missing_data,Count>Max_Num_Missing_Days)                #Keep months missing more days than maximum specified by Max_Num_Missing_Days
    df_monthly_cleaned = subset(df_Monthly, ! Month %in%  df_missing_data$Month)        #Remove months missing too many days of data
    df_mean_log = mean(df_monthly_cleaned$log_AF)                                       #calculate mean of monthly volume in acre-ft
    df_SD_log = sd(df_monthly_cleaned$log_AF)                                           #calculate standard deviation of monthly volume in acre-ft
  } else {
    df_monthly_cleaned = df_Monthly
  }
  df_mean_log = mean(df_monthly_cleaned$log_AF)                                       #calculate mean of monthly volume in acre-ft
  df_SD_log = sd(df_monthly_cleaned$log_AF)                                           #calculate standard deviation of monthly volume in acre-ft
  return(list(df_mean_log, df_SD_log))
}

#-------------------------------------------------------------------------------------------------#

############   Regression - Pre WY1973
tribs_regress_pre_WY1973 = function(monthly_cleaned_all, Streams, Pre_WY1973) {
  #initialize empty output lists
  all_tribs_regress_pre_WY1973=list()
  all_Pre_WY1973_mean_log=list()
  all_Stream_Pre_WY1973_SD_log=list()

  for (i in 1:length(Streams)){
    Stream_name = Streams[i]
    #df_monthly_volume = eval(parse(text = paste0(Streams[i],'_monthly_cleaned')))
    df_monthly_volume = monthly_cleaned_all[[i]]

    if (Stream_name == 'FJ') {  #Fort Jones Gage
      Stream_Pre_WY1973 = subset(df_monthly_volume, Month %in% Pre_WY1973, select = c('Month','log_AF'))  #Keep only Pre-WY1973 values
      Stream_Pre_WY1973_mean_log = mean(Stream_Pre_WY1973$log_AF)                                         #Calculate Pre-WY1973 mean
      Stream_Pre_WY1973_SD_log = sd(Stream_Pre_WY1973$log_AF)                                             #Calculate Pre-WY1973 standard deviation

      print(paste('pre1973',Stream_name,'| mean =',Stream_Pre_WY1973_mean_log, 'sd = ',Stream_Pre_WY1973_SD_log))

      Stream_Pre_WY1973$norm_log_AF = (Stream_Pre_WY1973$log_AF - Stream_Pre_WY1973_mean_log)/Stream_Pre_WY1973_SD_log #normalize log-transformed volumes in ac-ft
      output_df_name = paste0(Stream_name,'_Pre_WY1973')                               #name of tributary output data frame
      output_mean_name = paste0(Stream_name,'_Pre_WY1973_mean_log')                    #name of tributary variable for mean of log flows
      output_sd_name =  paste0(Stream_name,'_Pre_WY1973_SD_log')                      #name of tributary variable for standard deviation of log flows
      eval(parse(text = paste0(output_df_name,' <<- Stream_Pre_WY1973')))              #export dataframe
      eval(parse(text = paste0(output_mean_name,' <<- Stream_Pre_WY1973_mean_log')))   #export dataframe
      eval(parse(text = paste0(output_sd_name,' <<- Stream_Pre_WY1973_SD_log')))       #export dataframe
    } else {  #Tributaries
      Stream_Pre_WY1973 = subset(df_monthly_volume, Month %in% Pre_WY1973, select = c('Month','log_AF'))
      Stream_Pre_WY1973_mean_log = mean(Stream_Pre_WY1973$log_AF)                      #Calculate Pre-WY1973 mean
      Stream_Pre_WY1973_SD_log = sd(Stream_Pre_WY1973$log_AF)                          #Calculate Pre-WY1973 standard deviation

      print(paste('pre1973',Stream_name,'| mean =',Stream_Pre_WY1973_mean_log, 'sd = ',Stream_Pre_WY1973_SD_log))

      Stream_Pre_WY1973$norm_log_AF = (Stream_Pre_WY1973$log_AF - Stream_Pre_WY1973_mean_log)/Stream_Pre_WY1973_SD_log  #normalize log-transformed volumes in ac-ft
      Stream_Pre_WY1973 = cbind(Stream_Pre_WY1973,subset(FJ_Pre_WY1973, Month %in% Stream_Pre_WY1973$Month, select = 'norm_log_AF' ))
      names(Stream_Pre_WY1973) = c('Month','log_AF','norm_log_AF','FJ_norm_log_AF')
      output_df_name = paste0(Stream_name,'_tribs_regress_pre_WY1973')                               #name of tributary output data frame
      output_mean_name = paste0(Stream_name,'_Pre_WY1973_mean_log')                    #name of tributary variable for mean of log flows
      output_sd_name =  paste0(Stream_name,'_Pre_WY1973_SD_log')                      #name of tributary variable for standard deviation of log flows
      eval(parse(text = paste0(output_df_name,' <<- Stream_Pre_WY1973')))              #export dataframe
      eval(parse(text = paste0(output_mean_name,' <<- Stream_Pre_WY1973_mean_log')))   #export Pre WY1973 mean
      eval(parse(text = paste0(output_sd_name,' <<- Stream_Pre_WY1973_SD_log')))       #export Pre WY1973 standard deviation
    }
    all_tribs_regress_pre_WY1973[[i]] = Stream_Pre_WY1973
    all_Pre_WY1973_mean_log[[i]] = Stream_Pre_WY1973_mean_log
    all_Stream_Pre_WY1973_SD_log[[i]] = Stream_Pre_WY1973_SD_log
  }
  return(all_tribs_regress_pre_WY1973)

}

#-------------------------------------------------------------------------------------------------#

############   Regression - Post WY1972
tribs_data_post_WY1972 = function(monthly_cleaned_all, Streams, Post_WY1972, stat_cutoff_date) {
  #initialize empty output list
  all_tribs_regress_post_WY1972=list()

  for (i in 1:length(Streams)){
    Stream_name = Streams[i]
    #df_monthly_volume = eval(parse(text = paste0(Streams[i],'_monthly_cleaned')))
    df_monthly_volume = monthly_cleaned_all[[i]]

    if (Stream_name == 'FJ') {  #Fort Jones Gage
      Stream_Post_WY1972 = subset(df_monthly_volume, Month %in% Post_WY1972, select = c('Month','log_AF'))  #Keep only Pre-WY1973 values
      Stream_Post_WY1972_mean_log = mean(Stream_Post_WY1972$log_AF)                                         #Calculate Pre-WY1973 mean
      Stream_Post_WY1972_SD_log = sd(Stream_Post_WY1972$log_AF)                                             #Calculate Pre-WY1973 standard deviation

      print(paste('post1973',Stream_name,'| mean =',Stream_Post_WY1972_mean_log, 'sd = ',Stream_Post_WY1972_SD_log))

      Stream_Post_WY1972$norm_log_AF = (Stream_Post_WY1972$log_AF - Stream_Post_WY1972_mean_log)/Stream_Post_WY1972_SD_log #normalize log-transformed volumes in ac-ft
      output_df_name = paste0(Stream_name,'_Post_WY1972')                               #name of tributary output data frame
      output_mean_name = paste0(Stream_name,'_Post_WY1972_mean_log')                    #name of tributary variable for mean of log flows
      output_sd_name =  paste0(Stream_name,'_Post_WY1972_SD_log')                       #name of tributary variable for standard deviation of log flows
      eval(parse(text = paste0(output_df_name,' <<- Stream_Post_WY1972')))              #export dataframe
      eval(parse(text = paste0(output_mean_name,' <<- Stream_Post_WY1972_mean_log')))   #export dataframe
      eval(parse(text = paste0(output_sd_name,' <<- Stream_Post_WY1972_SD_log')))       #export dataframe
    } else {  #Tributaries
      Stream_Post_WY1972 = subset(df_monthly_volume, Month %in% Post_WY1972, select = c('Month','log_AF'))
      Stream_Post_WY1972_mean_log = mean(Stream_Post_WY1972$log_AF)                      #Calculate Pre-WY1973 mean
      Stream_Post_WY1972_SD_log = sd(Stream_Post_WY1972$log_AF)                          #Calculate Pre-WY1973 standard deviation

      print(paste('post1973',Stream_name,'| mean =',Stream_Post_WY1972_mean_log, 'sd = ',Stream_Post_WY1972_SD_log))

      Stream_Post_WY1972$norm_log_AF = (Stream_Post_WY1972$log_AF - Stream_Post_WY1972_mean_log)/Stream_Post_WY1972_SD_log  #normalize log-transformed volumes in ac-ft
      Stream_Post_WY1972 = cbind(Stream_Post_WY1972,subset(FJ_Post_WY1972, Month %in% Stream_Post_WY1972$Month, select = 'norm_log_AF' ))
      names(Stream_Post_WY1972) = c('Month','log_AF','norm_log_AF','FJ_norm_log_AF')
      output_df_name = paste0(Stream_name,'_tribs_regress_post_WY1972')                               #name of tributary output data frame
      output_mean_name = paste0(Stream_name,'_Post_WY1972_mean_log')                    #name of tributary variable for mean of log flows
      output_sd_name =  paste0(Stream_name,'_Post_WY1972_SD_log')                       #name of tributary variable for standard deviation of log flows
      eval(parse(text = paste0(output_df_name,' <<- Stream_Post_WY1972')))              #export dataframe
      eval(parse(text = paste0(output_mean_name,' <<- Stream_Post_WY1972_mean_log')))   #export Pre WY1973 mean
      eval(parse(text = paste0(output_sd_name,' <<- Stream_Post_WY1972_SD_log')))       #export Pre WY1973 standard deviation
    }
    all_tribs_regress_post_WY1972[[i]]=Stream_Post_WY1972

  }
  return(all_tribs_regress_post_WY1972)
}

#-------------------------------------------------------------------------------------------------#

calc_coef_tables = function(all_tribs_regress_pre_WY1973, all_tribs_regress_post_WY1972, Streams){
  #Pre-WY1973
  Pre_WY1973_df = all_tribs_regress_pre_WY1973[[2]]
  for(i in 3:length(Streams)){
    Pre_WY1973_df=rbind(Pre_WY1973_df, all_tribs_regress_pre_WY1973[[i]])
  }

  # tribs_regress_pre_WY1973 = rbind(East_Fork_tribs_regress_pre_WY1973, South_Fork_tribs_regress_pre_WY1973, Sugar_tribs_regress_pre_WY1973, French_tribs_regress_pre_WY1973, Etna_tribs_regress_pre_WY1973,
  #                                  Patterson_tribs_regress_pre_WY1973, Kidder_tribs_regress_pre_WY1973, Moffett_tribs_regress_pre_WY1973, Mill_tribs_regress_pre_WY1973, Shackleford_tribs_regress_pre_WY1973)
  tribs_regress_pre_WY1973_model = lm(norm_log_AF ~ FJ_norm_log_AF, data = Pre_WY1973_df)
  Summary_Pre_WY1973 = summary(tribs_regress_pre_WY1973_model)
  Regression_Coefs_Pre_WY1973 = coef(tribs_regress_pre_WY1973_model)
  R2_Pre_WY1973.exp = paste("R^2 == ",round(Summary_Pre_WY1973$adj.r.squared,2))
  if (Summary_Pre_WY1973$coefficients[1] > 0){
    Eqn_Pre_WY1973.exp = paste('y = ',round(Summary_Pre_WY1973$coefficients[2],2),'x + ',round(Summary_Pre_WY1973$coefficients[1],2),sep = '')
  } else {
    Eqn_Pre_WY1973.exp = paste('y = ',round(Summary_Pre_WY1973$coefficients[2],2),'x - ',abs(round(Summary_Pre_WY1973$coefficients[1],2)),sep = '')
  }
  Intercept_Pre_WY1973 = Summary_Pre_WY1973$coefficients[1]

  #Post-WY1972
  Post_WY1972_df = all_tribs_regress_post_WY1972[[2]]
  for(i in 3:length(Streams)){
    Post_WY1972_df=rbind(Post_WY1972_df, all_tribs_regress_post_WY1972[[i]])
  }
  # tribs_regress_post_WY1972 = rbind(East_Fork_tribs_regress_post_WY1972, South_Fork_tribs_regress_post_WY1972, Sugar_tribs_regress_post_WY1972, French_tribs_regress_post_WY1972, Etna_tribs_regress_post_WY1972,
  #                                   Patterson_tribs_regress_post_WY1972, Kidder_tribs_regress_post_WY1972, Moffett_tribs_regress_post_WY1972, Mill_tribs_regress_post_WY1972, Shackleford_tribs_regress_post_WY1972)
  tribs_regress_post_WY1972_model = lm(norm_log_AF ~ FJ_norm_log_AF, data = Post_WY1972_df)
  Summary_Post_WY1972 = summary(tribs_regress_post_WY1972_model)
  Regression_Coefs_Post_WY1972 = coef(tribs_regress_post_WY1972_model)
  R2_Post_WY1972.exp = paste("R^2 == ",round(Summary_Post_WY1972$adj.r.squared,2))
  if (Summary_Post_WY1972$coefficients[1] > 0){
    Eqn_Post_WY1972.exp = paste('y = ',round(Summary_Post_WY1972$coefficients[2],2),'x + ',round(Summary_Post_WY1972$coefficients[1],2),sep = '')
  } else {
    Eqn_Post_WY1972.exp = paste('y = ',round(Summary_Post_WY1972$coefficients[2],2),'x - ',abs(round(Summary_Post_WY1972$coefficients[1],2)),sep = '')
  }
  Intercept_Post_WY1972 = Summary_Post_WY1972$coefficients[1]

  return(list(Regression_Coefs_Pre_WY1973, Regression_Coefs_Post_WY1972))
}

#-------------------------------------------------------------------------------------------------#

###########   Estimate Missing Streamflow
populate_SVIHM_flows = function(monthly_cleaned_all, Streams, SVIHM_months, NumDays,
                                Regression_Coefs_Pre_WY1973, Regression_Coefs_Post_WY1972) {
  #initialize empty output lists
  all_Model_Input=list()
  FJ_monthly_cleaned = monthly_cleaned_all[[1]]

  for (i in 2:length(Streams)){
    # df_monthly_volume = eval(parse(text = paste0(Streams[i],'_monthly_cleaned')))
    df_monthly_volume = monthly_cleaned_all[[i]]
    Trib_name = Streams[i]

    #Fill in tributary months for which there are no records
    Observed_months = subset(df_monthly_volume, Month %in% SVIHM_months, select = c('Month','m3'))    #Subset observed data during model simulation
    Missing_months = data.frame(Month = SVIHM_months[!(SVIHM_months %in% df_monthly_volume$Month)])      #Create data frame with missing months
    if (nrow(Observed_months)>0) {                                                 #Back transform using post-WY1972 statistics if data is available
      Missing_months = cbind(Missing_months,subset(FJ_monthly_cleaned, Month %in% Missing_months$Month, select = 'norm_log_AF'))
      names(Missing_months)= c('Month', 'FJ_norm_log_AF')
      Missing_months$est_norm_log_AF = Missing_months$FJ_norm_log_AF*Regression_Coefs_Post_WY1972[2] +
        Regression_Coefs_Post_WY1972[1]
      eval(parse(text=paste0('mean = ',Trib_name,'_Post_WY1972_mean_log')))      #Obtain tributary specific back transformation mean
      eval(parse(text=paste0('SD = ',Trib_name,'_Post_WY1972_SD_log')))          #Obtain tributary specific back transformation standard deviation
      Missing_months$log_AF = (Missing_months$est_norm_log_AF*SD)+mean             #Transform back to log10(AF)
      Missing_months$AF = 10^(Missing_months$log_AF)                               #Convert to AF
      Missing_months$m3 = Missing_months$AF*1233.48                                #Convert to m^3
      Model_Input = rbind(Observed_months,subset(Missing_months,select = c('Month','m3')))  #combine observed and estimated data
      Model_Input = Model_Input[order(Model_Input$Month),]                                  #order data by month
      Model_Input$avg_flow_m3day = Model_Input$m3/NumDays                                        #convert to average monthly flow rate
      Model_Input = subset(Model_Input,select = c('Month','avg_flow_m3day'))                     #subset dataframe for export
      # output_df_name = paste0(Trib_name,'_Model_Inputs')                                    #create name for exported dataframe
      # eval(parse(text = paste0(output_df_name,' <<- Model_Input')))                         #export dataframe
    } else {                                                                       #Back transform using pre-WY1973 statistics only data available
      Missing_months = cbind(Missing_months,subset(FJ_monthly_cleaned, Month %in% Missing_months$Month, select = 'norm_log_AF'))
      names(Missing_months)= c('Month', 'FJ_norm_log_AF')
      Missing_months$est_norm_log_AF = Missing_months$FJ_norm_log_AF*Regression_Coefs_Pre_WY1973[2] +
        Regression_Coefs_Pre_WY1973[1]
      eval(parse(text=paste0('mean = ',Trib_name,'_Pre_WY1973_mean_log')))      #Obtain tributary specific back transformation mean
      eval(parse(text=paste0('SD = ',Trib_name,'_Pre_WY1973_SD_log')))          #Obtain tributary specific back transformation standard deviation
      Missing_months$log_AF = (Missing_months$est_norm_log_AF*SD)+mean             #Transform back to log10(AF)
      Missing_months$AF = 10^(Missing_months$log_AF)                               #Convert to AF
      Missing_months$m3 = Missing_months$AF*1233.48                                #Convert to m^3
      Model_Input = subset(Missing_months,select = c('Month','m3'))                #only estimated data available
      Model_Input = Model_Input[order(Model_Input$Month),]                         #order data by month
      Model_Input$avg_flow_m3day = Model_Input$m3/NumDays                               #convert to average monthly flow rate
      Model_Input = subset(Model_Input,select = c('Month','avg_flow_m3day'))            #subset dataframe for export
      # output_df_name = paste0(Trib_name,'_Model_Inputs')                           #create name for exported dataframe
      # eval(parse(text = paste0(output_df_name,' <<- Model_Input')))                #export dataframe
    }

    all_Model_Input[[i-1]] = Model_Input
  }
  return(all_Model_Input)
}

#-------------------------------------------------------------------------------------------------#

#' Write Streamflow Input File
#'
#' @param stream_out DataFrame, output from generate_streamflow_input()
#' @param output_dir Directory to write file
#' @param filename Filename (optional, default: streamflow_input.txt)
#' @param verbose T/F write status info to console (default: TRUE)
#'
#' @return None
#' @export
#'
#' @examples
write_streamflow_input_file <- function(stream_out,
                                        output_dir,
                                        filename='streamflow_input.txt',
                                        verbose=TRUE) {

  if (verbose) {message(paste('Writing file: ', filename))}

  write.table(stream_out,
              file = file.path(output_dir, filename),
              append = F,
              quote = F,
              row.names = F,
              col.names = T,
              sep = '\t')
}
