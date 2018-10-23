# Script for post-processing changes to streamflow resulting from implementing different conjunctive use scenarios
rm(list=ls())

library(ggplot2)
library(lubridate)
library(magrittr)

#############################################################################################
############################             IMPORT DATA             ############################
#############################################################################################
FJ_Basecase_flow = data.frame(Date = seq(as.Date("1990/10/1"), as.Date("2011/9/30"), "days"),                         # Import Basecase flow data
                           Flow_m3day = read.table('Streamflow_FJ_SVIHM_Basecase.txt', skip = 2)[,3],
                           Flow_cfs = read.table('Streamflow_FJ_SVIHM_Basecase.txt', skip = 2)[,3]*0.000408734569)
FJ_MAR_flow = data.frame(Date = seq(as.Date("1990/10/1"), as.Date("2011/9/30"), "days"),                              # Import MAR flow data
                           Flow_m3day = read.table('Streamflow_FJ_SVIHM_MAR.txt', skip = 2)[,3],
                           Flow_cfs = read.table('Streamflow_FJ_SVIHM_MAR.txt', skip = 2)[,3]*0.000408734569)
FJ_ILR_flow = data.frame(Date = seq(as.Date("1990/10/1"), as.Date("2011/9/30"), "days"),                              # Import ILR flow data
                           Flow_m3day = read.table('Streamflow_FJ_SVIHM_ILR.txt', skip = 2)[,3],
                           Flow_cfs = read.table('Streamflow_FJ_SVIHM_ILR.txt', skip = 2)[,3]*0.000408734569)

#############################################################################################
##########################             DATA PROCESSING             ##########################
#############################################################################################
Flow_Diff_Daily = data.frame(Date = seq(as.Date("1990/10/1"), as.Date("2011/9/30"), "days"),                          # Calculate daily difference from basecase condition
                             MAR_difference_m3day = FJ_MAR_flow$Flow_m3day - FJ_Basecase_flow$Flow_m3day,
                             MAR_difference_cfs = FJ_MAR_flow$Flow_cfs - FJ_Basecase_flow$Flow_cfs,
                             ILR_difference_m3day = FJ_ILR_flow$Flow_m3day - FJ_Basecase_flow$Flow_m3day,
                             ILR_difference_cfs = FJ_ILR_flow$Flow_cfs - FJ_Basecase_flow$Flow_cfs)
# Average difference in flow for each Stress Period
Flow_Diff_SP_Avg = Flow_Diff_Daily                                                                               # Copy daily data frame
Flow_Diff_SP_Avg$Date = format(Flow_Diff_SP_Avg$Date, '%b-%y')
Flow_Diff_SP_Avg = aggregate(.~Date,data =Flow_Diff_SP_Avg,  FUN = mean)
Flow_Diff_SP_Avg$Date = as.Date(paste0(Flow_Diff_SP_Avg$Date,'-01'), format = '%b-%y-%d')
Flow_Diff_SP_Avg = Flow_Diff_SP_Avg[order(Flow_Diff_SP_Avg$Date),]

# Average difference in flow for each calendar month
Flow_Diff_Monthly_Avg = Flow_Diff_Daily[seq(-1,-92),]                                                                            # Copy daily data frame
Flow_Diff_Monthly_Avg$Date = format(Flow_Diff_Monthly_Avg$Date, '%b')
Flow_Diff_Monthly_Avg = aggregate(.~Date,data =Flow_Diff_Monthly_Avg,  FUN = mean)
Flow_Diff_Monthly_Avg$Date  = factor(Flow_Diff_Monthly_Avg$Date , levels = as.character(format(seq(as.Date("1990/1/1"), as.Date("1990/12/31"), "month"), '%b')))
Flow_Diff_Monthly_Avg = Flow_Diff_Monthly_Avg[order(Flow_Diff_Monthly_Avg$Date), ]
#############################################################################################
############################                PLOTS                ############################
#############################################################################################

# Daily Difference Plots
ggplot(Flow_Diff_Daily, aes(x = Date, y = MAR_difference_cfs)) + geom_line()
ggplot(Flow_Diff_Daily, aes(x = Date, y = ILR_difference_cfs)) + geom_line()

#Monthly Average Difference
ggplot(Flow_Diff_SP_Avg, aes(x = Date, y = MAR_difference_cfs)) + geom_line()
ggplot(Flow_Diff_SP_Avg, aes(x = Date, y = ILR_difference_cfs)) + geom_line()

#21 yr average difference by month
ggplot(Flow_Diff_Monthly_Avg, aes(x = seq(1,12), y = MAR_difference_cfs)) + geom_line()
ggplot(Flow_Diff_Monthly_Avg, aes(x = seq(1,12), y = ILR_difference_cfs)) + geom_line()

