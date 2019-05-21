rm(list=ls())
library(ggplot2)

#Specify end date of model and create date vectors for SVIHM flows output
end_date = as.Date("2018/9/30")
SVIHM_months = seq(from = as.Date("1990/10/1"), to = end_date,by = "month")  #Months during model simulation
Months_plus_one = seq(as.Date("1990/10/1"), by = "month", length.out = length(SVIHM_months)+1)  #Months during model simulation
NumDays = as.numeric(diff(Months_plus_one))                                  #Numer of days in each simulation month

#Date vectors for regression models
Pre_WY1973 = seq(as.Date("1940/10/1"), by = "month", length.out = 384)  #Months Prior to WY1973
Post_WY1972 = seq(from = as.Date("1972/10/1"), to = end_date, by = "month")  #Months After WY1972

Max_Num_Missing_Days = 3                                                #Maximum number of missing daily flow data before month is removed from the regression

############################################################
########     Streamflow Pre-Processing Function     ########
############################################################
#Pre processing of tributary streamflow. Cleans and log-normalizes monthly flow volumes
preproflow = function(df_daily_mean, Stream_name) {
  df_daily_mean$Date = as.Date(df_daily_mean$Date,'%m/%d/%Y')
  df_daily_mean$Month = format(df_daily_mean$Date, format = '%m-%Y')
  df_daily_mean$ft3 = df_daily_mean$mean_cfs*86400
  
  #Monthly Volume
  df_Monthly = aggregate(.~Month,df_daily_mean[c('Month', 'ft3')],FUN = sum, na.rm = T)
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
    df_monthly_cleaned$norm_log_AF = (df_monthly_cleaned$log_AF - 
                                        df_mean_log)/df_SD_log                          #normalize log-transformed volumes in ac-ft
  } else {
    df_monthly_cleaned = df_Monthly
    df_mean_log = mean(df_monthly_cleaned$log_AF)                                       #calculate mean of monthly volume in acre-ft
    df_SD_log = sd(df_monthly_cleaned$log_AF)                                           #calculate standard deviation of monthly volume in acre-ft 
    df_monthly_cleaned$norm_log_AF = (df_monthly_cleaned$log_AF - 
                                        df_mean_log)/df_SD_log                          #normalize log-transformed volumes in ac-ft
  } 
  output_df_name = paste0(Stream_name,'_monthly_cleaned')                  #name of tributary output data frame
  output_mean_name = paste0(Stream_name,'_mean_log')                       #name of tributary variable for mean of log flows
  output_sd_name =  paste0(Stream_name,'_SD_log')                         #name of tributary variable for standard deviation of log flows
  eval(parse(text = paste0(output_df_name,' <<- df_monthly_cleaned')))   #export dataframe
  eval(parse(text = paste0(output_mean_name,' <<- df_mean_log')))        #export mean
  eval(parse(text = paste0(output_sd_name,' <<- df_SD_log')))            #export standard deviation
}
############################################################
###########        Import Streamflow Data         ##########
############################################################
setwd("C:/Users/ckouba/Git/SVIHM/SVIHM/Streamflow_Regression_Model")

FJ_daily_mean = read.table('USGS_11519500_WY_1942_2018.txt', header = TRUE, stringsAsFactors = F)[-4]   #import daily streamflow data
East_Fork_daily_mean = read.table('East_Fork_Scott_River_R_input.txt', header = T, stringsAsFactors = F, sep = '\t')[-4]
South_Fork_daily_mean = read.table('South_Fork_Scott_River_R_input.txt', header = T, stringsAsFactors = F, sep = '\t')[-4]
Sugar_daily_mean = read.table('Sugar_Creek_R_input.txt', header = T, stringsAsFactors = F, sep = '\t')[-4]
Etna_daily_mean = read.table('Etna_Creek_R_input.txt', header = T, stringsAsFactors = F, sep = '\t')[-4]
French_daily_mean = read.table('French_Creek_R_input.txt', header = T, stringsAsFactors = F, sep = '\t')[-4]
Patterson_daily_mean = read.table('Patterson_Creek_R_input.txt', header = T, stringsAsFactors = F, sep = '\t')[-4]
Kidder_daily_mean = read.table('Kidder_Creek_R_input.txt', header = T, stringsAsFactors = F, sep = '\t')[-4]
Moffett_daily_mean = read.table('Moffett_Creek_R_input.txt', header = T, stringsAsFactors = F, sep = '\t')[-4]
Mill_daily_mean = read.table('Mill_Creek_R_input.txt', header = T, stringsAsFactors = F, sep = '\t')[-4]
Shackleford_daily_mean = read.table('Shackleford_Creek_R_input.txt', header = T, stringsAsFactors = F, sep = '\t')[-4]
#No Data for Clark, Johnson, Crystal, or Oro Fino Creeks

Streams = c('FJ','East_Fork','South_Fork','Sugar','Etna','French','Patterson','Kidder','Moffett','Mill','Shackleford')
############################################################
###########        Pre-process Streamflow         ##########
############################################################
#Loop over streams for pre-processing
for (i in 1:length(Streams)){
  preproflow(df_daily_mean = eval(parse(text = paste0(Streams[i],'_daily_mean'))), Stream_name = Streams[i])
}

############################################################
##############   Regression - All Years    #################
############################################################
regress_all = function(Trib_df, Trib_name) {
  Trib_regress_all = subset(Trib_df, Month %in% FJ_monthly_cleaned$Month)[c('Month', 'norm_log_AF')]
  Trib_regress_all = cbind(Trib_regress_all,subset(FJ_monthly_cleaned, Month %in% Trib_regress_all$Month)['norm_log_AF'])
  names(Trib_regress_all) = c('Month', 'norm_log_AF','FJ_norm_log_AF')
  output_df_name = paste0(Trib_name,'_regress_all')                    #name of tributary output data frame
  eval(parse(text = paste0(output_df_name,' <<- Trib_regress_all')))   #export dataframe
  }

for (i in 2:length(Streams)){
  regress_all(Trib_df = eval(parse(text = paste0(Streams[i],'_monthly_cleaned'))), Trib_name = Streams[i])
} 
  
tribs_regress_all = rbind(East_Fork_regress_all, South_Fork_regress_all, Sugar_regress_all, French_regress_all, Etna_regress_all,
                          Patterson_regress_all, Kidder_regress_all, Moffett_regress_all, Mill_regress_all, Shackleford_regress_all)
tribs_regress_all_model = lm(norm_log_AF ~ FJ_norm_log_AF, data = tribs_regress_all)
Summary_All_Years = summary(tribs_regress_all_model)
Regression_Coefs_All_Years = coef(tribs_regress_all_model)
R2_All_Years.exp = paste("R^2 == ",round(Summary_All_Years$adj.r.squared,2))
if (Summary_All_Years$coefficients[1] > 0){
  Eqn_All_Years.exp = paste('y = ',round(Summary_All_Years$coefficients[2],2),'x + ',round(Summary_All_Years$coefficients[1],2),sep = '')
} else {
  Eqn_All_Years.exp = paste('y = ',round(Summary_All_Years$coefficients[2],2),'x - ',abs(round(Summary_All_Years$coefficients[1],2)),sep = '')
}
Intercept_All_Years = Summary_All_Years$coefficients[1]

# png('Regression_Plot_All_Years_4_square.png',width = 8, height = 5, units = 'in', res = 600)
# layout(matrix(1:4,2,2))
# plot(tribs_regress_all_model)
# graphics.off()
# png('Regression_Plot_All_Years.png',width = 8, height = 5, units = 'in', res = 600)
# ggplot(data = tribs_regress_all,aes(FJ_norm_log_AF, norm_log_AF)) + 
#   geom_point()  +  
#   geom_smooth(method='lm') + 
#   xlab(expression(Normalized~Log[10]~USGS~Fort~Jones~Streamflow)) +
#   ylab(expression(Normalized~Log[10]~Tributary~Streamflow)) + 
#   ggtitle('All Years Regression') +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_rect(colour = 'black', fill = NA),
#         panel.background = element_blank(), axis.line = element_line(colour = "black"),
#         axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"),
#         plot.title = element_text(hjust = 0.5)) +
#   scale_x_continuous(limits = c(-3,3), breaks = seq(-3,3,1), expand = c(0,0)) +
#   scale_y_continuous(limits = c(-3,3), breaks = seq(-3,3,1), expand = c(0,0)) +
#   annotate('text', x=1.5, y=-1.5, label = R2_All_Years.exp, parse = T) +
#   annotate('text', x=1.5, y=-1.8, label = Eqn_All_Years.exp, parse = F) 
# graphics.off()

# #Color by date
# png('Regression_Plot_All_Years.png',width = 8, height = 5, units = 'in', res = 600)
# ggplot(data = tribs_regress_all,aes(FJ_norm_log_AF, norm_log_AF)) + 
#   geom_point(data = subset(tribs_regress_all, Month %in% Pre_WY1973),aes(color = 'Pre-WY1973'))  +  
#   geom_point(data = subset(tribs_regress_all, Month %in% Post_WY1972),aes(color = 'Post-WY1972'))  +    
#   geom_smooth(method='lm') + 
#   xlab(expression(Normalized~Log[10]~USGS~Fort~Jones~Streamflow)) +
#   ylab(expression(Normalized~Log[10]~Tributary~Streamflow)) + 
#   ggtitle('Regression - All Years') +
#   theme(axis.title.y = element_text(size = 14),
#         axis.title.x =element_text(size = 14),
#         title = element_text(size = 16),
#         panel.grid.major = element_blank(),panel.grid.minor = element_blank(), panel.border = element_rect(colour = 'black', fill = NA),
#         panel.background = element_blank(), axis.line = element_line(colour = "black"),
#         axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"),
#         plot.title = element_text(hjust = 0.5), legend.title = element_blank(), legend.key = element_blank(),
#         legend.position = c(0.25,0.8),
#         legend.text = element_text(size = 16)) +
#   scale_x_continuous(limits = c(-3,3), breaks = seq(-3,3,1), expand = c(0,0)) +
#   scale_y_continuous(limits = c(-3,3), breaks = seq(-3,3,1), expand = c(0,0)) +
#   annotate('text', x=1.5, y=-1.5, label = R2_All_Years.exp, parse = T, size = 5) +
#   annotate('text', x=1.5, y=-2, label = Eqn_All_Years.exp, parse = F, size = 5) 
# graphics.off()

############################################################
############   Regression - Pre WY1973    #################
############################################################
tribs_regress_pre_WY1973 = function(df_monthly_volume, Stream_name) {
  if (Stream_name == 'FJ') {  #Fort Jones Gage
    Stream_Pre_WY1973 = subset(df_monthly_volume, Month %in% Pre_WY1973, select = c('Month','log_AF'))  #Keep only Pre-WY1973 values
    Stream_Pre_WY1973_mean_log = mean(Stream_Pre_WY1973$log_AF)                                         #Calculate Pre-WY1973 mean    
    Stream_Pre_WY1973_SD_log = sd(Stream_Pre_WY1973$log_AF)                                             #Calculate Pre-WY1973 standard deviation
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
}

for (i in 1:length(Streams)){
  tribs_regress_pre_WY1973(df_monthly_volume = eval(parse(text = paste0(Streams[i],'_monthly_cleaned'))), Stream_name = Streams[i])
} 

tribs_regress_pre_WY1973 = rbind(East_Fork_tribs_regress_pre_WY1973, South_Fork_tribs_regress_pre_WY1973, Sugar_tribs_regress_pre_WY1973, French_tribs_regress_pre_WY1973, Etna_tribs_regress_pre_WY1973,
                                 Patterson_tribs_regress_pre_WY1973, Kidder_tribs_regress_pre_WY1973, Moffett_tribs_regress_pre_WY1973, Mill_tribs_regress_pre_WY1973, Shackleford_tribs_regress_pre_WY1973)
tribs_regress_pre_WY1973_model = lm(norm_log_AF ~ FJ_norm_log_AF, data = tribs_regress_pre_WY1973)
Summary_Pre_WY1973 = summary(tribs_regress_pre_WY1973_model)
Regression_Coefs_Pre_WY1973 = coef(tribs_regress_pre_WY1973_model)
R2_Pre_WY1973.exp = paste("R^2 == ",round(Summary_Pre_WY1973$adj.r.squared,2))
if (Summary_Pre_WY1973$coefficients[1] > 0){
  Eqn_Pre_WY1973.exp = paste('y = ',round(Summary_Pre_WY1973$coefficients[2],2),'x + ',round(Summary_Pre_WY1973$coefficients[1],2),sep = '')
} else {
  Eqn_Pre_WY1973.exp = paste('y = ',round(Summary_Pre_WY1973$coefficients[2],2),'x - ',abs(round(Summary_Pre_WY1973$coefficients[1],2)),sep = '')
}
Intercept_Pre_WY1973 = Summary_Pre_WY1973$coefficients[1]

# png('Regression_Plot_Pre_WY1973_4_square.png',width = 8, height = 5, units = 'in', res = 600)
# layout(matrix(1:4,2,2))
# plot(tribs_regress_pre_WY1973_model)
# graphics.off()
# png('Regression_Plot_Pre_WY1973.png',width = 8, height = 5, units = 'in', res = 600)
# ggplot(data = tribs_regress_pre_WY1973,aes(FJ_norm_log_AF, norm_log_AF)) + 
#   geom_point()  +  
#   geom_smooth(method='lm') + 
#   xlab(expression(Normalized~Log[10]~USGS~Fort~Jones~Streamflow)) +
#   ylab(expression(Normalized~Log[10]~Tributary~Streamflow)) + 
#   ggtitle('Pre-WY1973 Regression') +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_rect(colour = 'black', fill = NA),
#         panel.background = element_blank(), axis.line = element_line(colour = "black"),
#         axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"),
#         plot.title = element_text(hjust = 0.5)) +
#   scale_x_continuous(limits = c(-3,3), breaks = seq(-3,3,1), expand = c(0,0)) +
#   scale_y_continuous(limits = c(-3,3), breaks = seq(-3,3,1), expand = c(0,0)) +
#   annotate('text', x=1.5, y=-1.5, label = R2_Pre_WY1973.exp, parse = T) +
#   annotate('text', x=1.5, y=-1.8, label = Eqn_Pre_WY1973.exp, parse = F) 
# graphics.off()

############################################################
############   Regression - Post WY1972    #################
############################################################
tribs_data_post_WY1972 = function(df_monthly_volume, Stream_name) {
  if (Stream_name == 'FJ') {  #Fort Jones Gage
    Stream_Post_WY1972 = subset(df_monthly_volume, Month %in% Post_WY1972, select = c('Month','log_AF'))  #Keep only Pre-WY1973 values
    Stream_Post_WY1972_mean_log = mean(Stream_Post_WY1972$log_AF)                                         #Calculate Pre-WY1973 mean    
    Stream_Post_WY1972_SD_log = sd(Stream_Post_WY1972$log_AF)                                             #Calculate Pre-WY1973 standard deviation
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
}

for (i in 1:length(Streams)){
  tribs_data_post_WY1972(df_monthly_volume = eval(parse(text = paste0(Streams[i], '_monthly_cleaned'))), Stream_name = Streams[i])
} 

tribs_regress_post_WY1972 = rbind(East_Fork_tribs_regress_post_WY1972, South_Fork_tribs_regress_post_WY1972, Sugar_tribs_regress_post_WY1972, French_tribs_regress_post_WY1972, Etna_tribs_regress_post_WY1972,
                            Patterson_tribs_regress_post_WY1972, Kidder_tribs_regress_post_WY1972, Moffett_tribs_regress_post_WY1972, Mill_tribs_regress_post_WY1972, Shackleford_tribs_regress_post_WY1972)
tribs_regress_post_WY1972_model = lm(norm_log_AF ~ FJ_norm_log_AF, data = tribs_regress_post_WY1972)
Summary_Post_WY1972 = summary(tribs_regress_post_WY1972_model)
Regression_Coefs_Post_WY1972 = coef(tribs_regress_post_WY1972_model)
R2_Post_WY1972.exp = paste("R^2 == ",round(Summary_Post_WY1972$adj.r.squared,2))
if (Summary_Post_WY1972$coefficients[1] > 0){
  Eqn_Post_WY1972.exp = paste('y = ',round(Summary_Post_WY1972$coefficients[2],2),'x + ',round(Summary_Post_WY1972$coefficients[1],2),sep = '')
} else {
  Eqn_Post_WY1972.exp = paste('y = ',round(Summary_Post_WY1972$coefficients[2],2),'x - ',abs(round(Summary_Post_WY1972$coefficients[1],2)),sep = '')
}
Intercept_Post_WY1972 = Summary_Post_WY1972$coefficients[1]

# png('Regression_Plot_Post_WY1972_4_square.png',width = 8, height = 5, units = 'in', res = 600)
# layout(matrix(1:4,2,2))
# plot(tribs_regress_post_WY1972_model)
# graphics.off()
# png('Regression_Plot_Post_WY1972.png',width = 8, height = 5, units = 'in', res = 600)
# ggplot(data = tribs_regress_post_WY1972,aes(FJ_norm_log_AF, norm_log_AF)) + 
#   geom_point()  +  
#   geom_smooth(method='lm') + 
#   xlab(expression(Normalized~Log[10]~USGS~Fort~Jones~Streamflow)) +
#   ylab(expression(Normalized~Log[10]~Tributary~Streamflow)) + 
#   ggtitle('Regression - Post WY1972 ') +
#   theme(axis.title.x = element_text(size = 14),
#         axis.title.y = element_text(size = 14),
#         title = element_text(size = 16),
#         panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_rect(colour = 'black', fill = NA),
#         panel.background = element_blank(), axis.line = element_line(colour = "black"),
#         axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"),
#         plot.title = element_text(hjust = 0.5),
#         legend.text = element_text(size = 16)) +
#   scale_x_continuous(limits = c(-3,3), breaks = seq(-3,3,1), expand = c(0,0)) +
#   scale_y_continuous(limits = c(-3,3), breaks = seq(-3,3,1), expand = c(0,0)) +
#   annotate('text', x=1.5, y=-1.5, label = R2_Post_WY1972.exp, parse = T, size = 5) +
#   annotate('text', x=1.5, y=-2, label = Eqn_Post_WY1972.exp, parse = F, size = 5) 
# graphics.off()

############################################################
###########   Estimate Missing Streamflow    ###############
############################################################
populate_SVIHM_flows = function(df_monthly_volume,Trib_name) {
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
    output_df_name = paste0(Trib_name,'_Model_Inputs')                                    #create name for exported dataframe
    eval(parse(text = paste0(output_df_name,' <<- Model_Input')))                         #export dataframe
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
    output_df_name = paste0(Trib_name,'_Model_Inputs')                           #create name for exported dataframe
    eval(parse(text = paste0(output_df_name,' <<- Model_Input')))                #export dataframe
    }
}

for (i in 2:length(Streams)){
  populate_SVIHM_flows(df_monthly_volume = eval(parse(text = paste0(Streams[i],'_monthly_cleaned'))), Trib_name = Streams[i])
} 

#No flow data for Johnson or Crystal Creeks
Johnson_Model_Inputs = data.frame(Month = SVIHM_months,
                                  avg_flow_m3day = Patterson_Model_Inputs$avg_flow_m3day * 0.2)  #Johnson inflows estimated at 20% of Patterson inflows
Crystal_Model_Inputs = data.frame(Month = SVIHM_months,
                                  avg_flow_m3day = Patterson_Model_Inputs$avg_flow_m3day * 0.15) #Crystal inflows estimated at 15% of Patterson inflows
#No inflow for Oro Fino Creek and Clark Creek is not currently included in the model


WY1991_2018_Streamflow = data.frame(Month = SVIHM_months,
                                   East_Fork_Avg_Flow_m3day = East_Fork_Model_Inputs$avg_flow_m3day,
                                   South_Fork_Avg_Flow_m3day = South_Fork_Model_Inputs$avg_flow_m3day,
                                   Sugar_Avg_Flow_m3day = Sugar_Model_Inputs$avg_flow_m3day,
                                   French_Avg_Flow_m3day = French_Model_Inputs$avg_flow_m3day,
                                   Etna_Avg_Flow_m3day = Etna_Model_Inputs$avg_flow_m3day,
                                   Johnson_Avg_Flow_m3day = Johnson_Model_Inputs$avg_flow_m3day,
                                   Crystal_Avg_Flow_m3day = Crystal_Model_Inputs$avg_flow_m3day,
                                   Patterson_Avg_Flow_m3day = Patterson_Model_Inputs$avg_flow_m3day,
                                   Kidder_Avg_Flow_m3day = Kidder_Model_Inputs$avg_flow_m3day,
                                   Moffett_Avg_Flow_m3day = Moffett_Model_Inputs$avg_flow_m3day,
                                   Mill_Avg_Flow_m3day = Mill_Model_Inputs$avg_flow_m3day,
                                   Shackleford_Avg_Flow_m3day = Shackleford_Model_Inputs$avg_flow_m3day)

write.table(WY1991_2018_Streamflow,file = 'streamflow_input.txt', append = F, quote = F, row.names = F, col.names = T, sep = '\t')

