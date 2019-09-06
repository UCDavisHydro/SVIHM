# Script for post-processing changes to streamflow resulting from implementing different conjunctive use scenarios
rm(list=ls())

library(ggplot2)
library(lubridate)
library(magrittr)
library(reshape2)
library(dplyr)
library(grid)
#############################################################################################
############################              USER INPUT             ############################
#############################################################################################
COMPARE_MAR = TRUE
COMPARE_ILR = TRUE
COMPARE_MAR_ILR = TRUE
graphics_type = 'png'    #output type for graphics, currently pdf or png
#############################################################################################
############################             IMPORT DATA             ############################
#############################################################################################
FJ_Basecase_flow = data.frame(Date = seq(as.Date("1990/10/1"), as.Date("2011/9/30"), "days"),                         # Import Basecase flow data
                           Flow_m3day = read.table('Streamflow_FJ_SVIHM.dat', skip = 2)[,3],
                           Flow_cfs = read.table('Streamflow_FJ_SVIHM.dat', skip = 2)[,3]*0.000408734569)
FJ_MAR_flow = data.frame(#Date = seq(as.Date("1990/10/1"), as.Date("2011/9/30"), "days"),                              # Import MAR flow data
                           Flow_m3day = read.table('Streamflow_FJ_SVIHM_MAR.dat', skip = 2)[,3],
                           Flow_cfs = read.table('Streamflow_FJ_SVIHM_MAR.dat', skip = 2)[,3]*0.000408734569)
FJ_ILR_flow = data.frame(Date = seq(as.Date("1990/10/1"), as.Date("2011/9/30"), "days"),                              # Import ILR flow data
                           Flow_m3day = read.table('Streamflow_FJ_SVIHM_ILR.dat', skip = 2)[,3],
                           Flow_cfs = read.table('Streamflow_FJ_SVIHM_ILR.dat', skip = 2)[,3]*0.000408734569)
FJ_MAR_ILR_flow = data.frame(Date = seq(as.Date("1990/10/1"), as.Date("2011/9/30"), "days"),                          # Import MAR_ILR flow data
                         Flow_m3day = read.table('Streamflow_FJ_SVIHM_MAR_ILR.dat', skip = 2)[,3],
                         Flow_cfs = read.table('Streamflow_FJ_SVIHM_MAR_ILR.dat', skip = 2)[,3]*0.000408734569)

Inflow_Basecase = readLines('SVIHM.sfr')
Inflow_Seg1_Basecase = as.numeric(sapply(lapply(strsplit(Inflow_Basecase[grep(x = Inflow_Basecase, pattern = '1  1  3  0')],' '),function(x){x[!x ==""]}),"[[",5))
MODFLOW_Seg1_Inflows = data.frame(Date = seq(as.Date("1990/10/1"), by = "month", length.out = 252),
                                  Basecase = Inflow_Seg1_Basecase)

Inflow_Seg32_Basecase = as.numeric(sapply(lapply(strsplit(Inflow_Basecase[grep(x = Inflow_Basecase, pattern = '32  1  0  10  0')],' '),function(x){x[!x ==""]}),"[[",6))
MODFLOW_Seg32_Inflows = data.frame(Date = seq(as.Date("1990/10/1"), by = "month", length.out = 252),
                                  Basecase = Inflow_Seg32_Basecase)
if (COMPARE_MAR==TRUE){
  Inflow_MAR = readLines('SVIHM_MAR.sfr')
  Inflow_Seg1_MAR = as.numeric(sapply(lapply(strsplit(Inflow_MAR[grep(x = Inflow_MAR, pattern = '1  1  3  0')],' '),function(x){x[!x ==""]}),"[[",5))
  Inflow_Seg1_MAR_diff = Inflow_Seg1_MAR - Inflow_Seg1_Basecase
  MODFLOW_Seg1_Inflows$MAR = Inflow_Seg1_MAR
  MODFLOW_Seg1_Inflows$MAR_diff = Inflow_Seg1_MAR_diff
  
  Inflow_Seg32_MAR = as.numeric(sapply(lapply(strsplit(Inflow_MAR[grep(x = Inflow_MAR, pattern = '32  1  0  10  0')],' '),function(x){x[!x ==""]}),"[[",6))
  Inflow_Seg32_MAR_diff = Inflow_Seg32_MAR - Inflow_Seg32_Basecase
  MODFLOW_Seg32_Inflows$MAR = Inflow_Seg32_MAR
  MODFLOW_Seg32_Inflows$MAR_diff = Inflow_Seg32_MAR_diff
  }
if (COMPARE_ILR==TRUE){
  Inflow_ILR = readLines('SVIHM_ILR.sfr')
  Inflow_Seg1_ILR = as.numeric(sapply(lapply(strsplit(Inflow_ILR[grep(x = Inflow_ILR, pattern = '1  1  3  0')],' '),function(x){x[!x ==""]}),"[[",5))
  Inflow_Seg1_ILR_diff = Inflow_Seg1_ILR - Inflow_Seg1_Basecase
  MODFLOW_Seg1_Inflows$ILR = Inflow_Seg1_ILR
  MODFLOW_Seg1_Inflows$ILR_diff = Inflow_Seg1_ILR_diff
  
  Inflow_Seg32_ILR = as.numeric(sapply(lapply(strsplit(Inflow_ILR[grep(x = Inflow_ILR, pattern = '32  1  0  10  0')],' '),function(x){x[!x ==""]}),"[[",6))
  Inflow_Seg32_ILR_diff = Inflow_Seg32_ILR - Inflow_Seg32_Basecase
  MODFLOW_Seg32_Inflows$ILR = Inflow_Seg32_ILR
  MODFLOW_Seg32_Inflows$ILR_diff = Inflow_Seg32_ILR_diff
  }
if (COMPARE_MAR_ILR==TRUE){
  Inflow_MAR_ILR = readLines('SVIHM_MAR_ILR.sfr')
  Inflow_Seg1_MAR_ILR = as.numeric(sapply(lapply(strsplit(Inflow_MAR_ILR[grep(x = Inflow_MAR_ILR, pattern = '1  1  3  0')],' '),function(x){x[!x ==""]}),"[[",5))
  Inflow_Seg1_MAR_ILR_diff = Inflow_Seg1_MAR_ILR - Inflow_Seg1_Basecase
  MODFLOW_Seg1_Inflows$MAR_ILR = Inflow_Seg1_MAR_ILR
  MODFLOW_Seg1_Inflows$MAR_ILR_diff = Inflow_Seg1_MAR_ILR_diff
  
  Inflow_Seg32_MAR_ILR = as.numeric(sapply(lapply(strsplit(Inflow_MAR_ILR[grep(x = Inflow_MAR_ILR, pattern = '32  1  0  10  0')],' '),function(x){x[!x ==""]}),"[[",6))
  Inflow_Seg32_MAR_ILR_diff = Inflow_Seg32_MAR_ILR - Inflow_Seg32_Basecase
  MODFLOW_Seg32_Inflows$MAR_ILR = Inflow_Seg32_MAR_ILR
  MODFLOW_Seg32_Inflows$MAR_ILR_diff = Inflow_Seg32_MAR_ILR_diff
  }

pumping_bc = read.table('monthly_groundwater_by_luse.dat', header = T)
pumping_MAR = read.table('monthly_groundwater_by_luse_MAR.dat', header = T)
pumping_ILR = read.table('monthly_groundwater_by_luse_ILR.dat', header = T)
pumping_MAR_ILR = read.table('monthly_groundwater_by_luse_MAR_ILR.dat', header = T)

pumping_bc$Stress_Period = rep(seq(1991,2011),each = 12)
pumping_MAR$Stress_Period = rep(seq(1991,2011),each = 12)
pumping_ILR$Stress_Period = rep(seq(1991,2011),each = 12)
pumping_MAR_ILR$Stress_Period = rep(seq(1991,2011),each = 12)

pumping_bc = aggregate(.~Stress_Period, pumping_bc, FUN = sum)
pumping_bc = rowSums(pumping_bc[,-1])*0.000000810714
pumping_MAR = aggregate(.~Stress_Period, pumping_MAR, FUN = sum)
pumping_MAR = rowSums(pumping_MAR[,-1])*0.000000810714
pumping_ILR = aggregate(.~Stress_Period, pumping_ILR, FUN = sum)
pumping_ILR = rowSums(pumping_ILR[,-1])*0.000000810714
pumping_MAR_ILR = aggregate(.~Stress_Period, pumping_MAR_ILR, FUN = sum)
pumping_MAR_ILR = rowSums(pumping_MAR_ILR[,-1])*0.000000810714

MAR_pumping_diff = pumping_bc - pumping_MAR
ILR_GW_Reduction = data.frame(Year=seq(1991,2011),
                              Reduction_TAF = (pumping_bc - pumping_ILR),
                              Reduction_pct = (pumping_bc - pumping_ILR)/pumping_bc*100)

MAR_ILR_pumping_diff = pumping_bc - pumping_MAR_ILR


#############################################################################################
##########################             DATA PROCESSING             ##########################
#############################################################################################
Flow_Diff_Daily = data.frame(Date = seq(as.Date("1990/10/1"), as.Date("2011/9/30"), "days"))                          # Calculate daily difference from basecase condition
if (COMPARE_MAR==TRUE){
  Flow_Diff_Daily$MAR_difference_m3day = FJ_MAR_flow$Flow_m3day - FJ_Basecase_flow$Flow_m3day
  Flow_Diff_Daily$MAR_difference_cfs = FJ_MAR_flow$Flow_cfs - FJ_Basecase_flow$Flow_cfs
}                          
if (COMPARE_ILR==TRUE){
  Flow_Diff_Daily$ILR_difference_m3day = FJ_ILR_flow$Flow_m3day - FJ_Basecase_flow$Flow_m3day
  Flow_Diff_Daily$ILR_difference_cfs = FJ_ILR_flow$Flow_cfs - FJ_Basecase_flow$Flow_cfs
}
if (COMPARE_MAR_ILR==TRUE){
  Flow_Diff_Daily$MAR_ILR_difference_m3day = FJ_MAR_ILR_flow$Flow_m3day - FJ_Basecase_flow$Flow_m3day
  Flow_Diff_Daily$MAR_ILR_difference_cfs = FJ_MAR_ILR_flow$Flow_cfs - FJ_Basecase_flow$Flow_cfs
} 

# Average difference in flow for each Stress Period
Flow_Diff_SP_Avg = Flow_Diff_Daily# Copy daily data frame
Flow_Diff_SP_Avg$Date = format(Flow_Diff_SP_Avg$Date, '%b-%y')
Flow_Diff_SP_Avg = aggregate(.~Date,data = Flow_Diff_SP_Avg,  FUN = mean)
Flow_Diff_SP_Avg$Date = as.Date(paste0(Flow_Diff_SP_Avg$Date,'-01'), format = '%b-%y-%d')
Flow_Diff_SP_Avg = Flow_Diff_SP_Avg[order(Flow_Diff_SP_Avg$Date),]

# Average difference in flow for each calendar month
Flow_Diff_Monthly_Avg = Flow_Diff_Daily[seq(-1,-92),]   # Exclude first three months when MAR is not active                                                                            
Flow_Diff_Monthly_Avg$Date = format(Flow_Diff_Monthly_Avg$Date, '%b')
Flow_Diff_Monthly_Avg_2 = Flow_Diff_Monthly_Avg                                      # Copy daily data frame
Flow_Diff_Monthly_Avg = aggregate(.~Date,data =Flow_Diff_Monthly_Avg,  FUN = mean)
Flow_Diff_Monthly_SD = aggregate(.~Date,data =Flow_Diff_Monthly_Avg_2,  FUN = sd)

names(Flow_Diff_Monthly_SD) = c('Date',paste0(names(Flow_Diff_Monthly_Avg[-1]),'_SD'))

Flow_Diff_Monthly_Avg = merge(Flow_Diff_Monthly_Avg,Flow_Diff_Monthly_SD)
Flow_Diff_Monthly_Avg$Date  = factor(Flow_Diff_Monthly_Avg$Date , levels = as.character(format(seq(as.Date("1990/1/1"), as.Date("1990/12/31"), "month"), '%b')))
Flow_Diff_Monthly_Avg = Flow_Diff_Monthly_Avg[order(Flow_Diff_Monthly_Avg$Date), ]


MAR_xintercept_cfs = 3 +(-Flow_Diff_Monthly_Avg$MAR_difference_cfs[3] /(Flow_Diff_Monthly_Avg$MAR_difference_cfs[4] - Flow_Diff_Monthly_Avg$MAR_difference_cfs[3]))
MAR_geom_ribbon_data = data.frame(x = c(1,2,3,MAR_xintercept_cfs,4,5,6,7,8,9,10,11,12),
                                  y = c(Flow_Diff_Monthly_Avg$MAR_difference_cfs[1:3], 0, Flow_Diff_Monthly_Avg$MAR_difference_cfs[4:12])      )
ILR_xintercept_1_cfs = 3 +(-Flow_Diff_Monthly_Avg$ILR_difference_cfs[3] /(Flow_Diff_Monthly_Avg$ILR_difference_cfs[4] - Flow_Diff_Monthly_Avg$ILR_difference_cfs[3]))
ILR_xintercept_2_cfs = 6 +(-Flow_Diff_Monthly_Avg$ILR_difference_cfs[6] /(Flow_Diff_Monthly_Avg$ILR_difference_cfs[7] - Flow_Diff_Monthly_Avg$ILR_difference_cfs[6]))
ILR_geom_ribbon_data = data.frame(x = c(1,2,3,ILR_xintercept_1_cfs,4,5,6,ILR_xintercept_2_cfs,7,8,9,10,11,12),
                                  y = c(Flow_Diff_Monthly_Avg$ILR_difference_cfs[1:3], 0, Flow_Diff_Monthly_Avg$ILR_difference_cfs[4:6],0,Flow_Diff_Monthly_Avg$ILR_difference_cfs[7:12]))
MAR_ILR_xintercept_1_cfs = 3 +(-Flow_Diff_Monthly_Avg$MAR_ILR_difference_cfs[3] /(Flow_Diff_Monthly_Avg$MAR_ILR_difference_cfs[4] - Flow_Diff_Monthly_Avg$MAR_ILR_difference_cfs[3]))
MAR_ILR_xintercept_2_cfs = 4 +(-Flow_Diff_Monthly_Avg$MAR_ILR_difference_cfs[4] /(Flow_Diff_Monthly_Avg$MAR_ILR_difference_cfs[5] - Flow_Diff_Monthly_Avg$MAR_ILR_difference_cfs[4]))
MAR_ILR_xintercept_3_cfs = 6 +(-Flow_Diff_Monthly_Avg$MAR_ILR_difference_cfs[6] /(Flow_Diff_Monthly_Avg$MAR_ILR_difference_cfs[7] - Flow_Diff_Monthly_Avg$MAR_ILR_difference_cfs[6]))
MAR_ILR_geom_ribbon_data = data.frame(x = c(1,2,3,MAR_ILR_xintercept_1_cfs,4,MAR_ILR_xintercept_2_cfs,5,6,MAR_ILR_xintercept_3_cfs,7,8,9,10,11,12),
                                  y = c(Flow_Diff_Monthly_Avg$MAR_ILR_difference_cfs[1:3], 0, Flow_Diff_Monthly_Avg$MAR_ILR_difference_cfs[4],0,Flow_Diff_Monthly_Avg$MAR_ILR_difference_cfs[5:6] ,0,Flow_Diff_Monthly_Avg$MAR_ILR_difference_cfs[7:12]))

#############################################################################################
#############################################################################################
############################                PLOTS                ############################
#############################################################################################
#############################################################################################

#############################################################################################
########################             MODFLOW INPUT PLOTS             ########################
#############################################################################################
MODFLOW_Seg1_Inflows_diff_melt = melt(MODFLOW_Seg1_Inflows%>%select('Date','MAR_diff','ILR_diff','MAR_ILR_diff'),id.vars = 'Date')
(Seg1_Inflow_diff_Plot = ggplot(data = MODFLOW_Seg1_Inflows_diff_melt, aes(x = Date, y = value*0.000408734569, group = variable, color = variable)) +
    geom_line() +
    geom_point() +
    scale_x_date(limits = c(as.Date('1990-10-01'),
                            as.Date('1995-11-01')),
                 breaks = seq(as.Date("1990/10/1"), by = "2 years", length.out = 22), expand = c(0,0),
                 date_labels = ('%b-%y'))
  )
MODFLOW_Seg32_Inflows_diff_melt = melt(MODFLOW_Seg32_Inflows%>%select('Date','MAR_diff','ILR_diff','MAR_ILR_diff'),id.vars = 'Date')
(Seg32_Inflow_diff_Plot = ggplot(data = MODFLOW_Seg32_Inflows_diff_melt, aes(x = Date, y = value*0.000408734569, group = variable, color = variable)) +
    geom_line() +
    geom_point() +
    scale_x_date(limits = c(as.Date('1990-10-01'),
                            as.Date('2011-11-01')),
                 breaks = seq(as.Date("1990/10/1"), by = "2 years", length.out = 22), expand = c(0,0),
                 date_labels = ('%b-%y'))
)


#############################################################################################
########################            DAILY DIFFERENCE PLOTS           ########################
#############################################################################################
(MAR_Daily_Diff_Plot = ggplot(Flow_Diff_Daily, aes(x = Date, y = MAR_difference_cfs)) +
  geom_line() +
  #geom_line(data =MODFLOW_Seg1_Inflows, aes(x = Date, y = Basecase*0.000408734569/10), color = 'red') +
  geom_hline(yintercept = 0) +
  #scale_y_continuous(limits = c(-50,50), breaks = seq(-50,50,by = 10), expand = c(0,0)) +
  scale_x_date(limits = c(as.Date('1990-10-1'),
                          as.Date('2011-9-30')),
               breaks = seq(as.Date("1990/10/1"), by = "2 years", length.out = 22), expand = c(0,0),
               date_labels = ('%b-%y')) +
   theme(panel.background = element_blank(),
         panel.border = element_rect(fill=NA, color = 'black'),
         axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
         axis.text.y = element_text(size = 8),
         axis.ticks = element_line(size = 0.2),
         plot.title = element_text(hjust = 0.5, size = 10),
         axis.title.x = element_blank(),
         axis.title.y = element_text(size = 8))
)
(ILR_Daily_Diff_Plot = ggplot(Flow_Diff_Daily, aes(x = Date, y = ILR_difference_cfs)) +
    geom_line() +
    geom_hline(yintercept = 0) +
    scale_y_continuous(limits = c(-90,90), breaks = seq(-90,90,by = 30), expand = c(0,0)) +
    scale_x_date(limits = c(as.Date('1990-10-1'),
                            as.Date('2011-9-30')),
                 breaks = seq(as.Date("1990/10/1"), by = "2 years", length.out = 22), expand = c(0,0),
                 date_labels = ('%b-%y')) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(fill=NA, color = 'black'),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
          axis.text.y = element_text(size = 8),
          axis.ticks = element_line(size = 0.2),
          plot.title = element_text(hjust = 0.5, size = 10),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 8))
)
(MAR_ILR_Daily_Diff_Plot = ggplot(Flow_Diff_Daily, aes(x = Date, y = MAR_ILR_difference_cfs)) +
    geom_line() +
    geom_hline(yintercept = 0) +
    scale_y_continuous(limits = c(-100,100), breaks = seq(-100,100,by = 25), expand = c(0,0)) +
    scale_x_date(limits = c(as.Date('1990-10-1'),
                            as.Date('2011-9-30')),
                 breaks = seq(as.Date("1990/10/1"), by = "2 years", length.out = 22), expand = c(0,0),
                 date_labels = ('%b-%y')) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(fill=NA, color = 'black'),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
          axis.text.y = element_text(size = 8),
          axis.ticks = element_line(size = 0.2),
          plot.title = element_text(hjust = 0.5, size = 10),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 8))
)
#############################################################################################
####################        STRESS PERIOD AVERAGE DIFFERENCE PLOTS       ####################
#############################################################################################
(MAR_Monthly_Diff_Plot = ggplot(Flow_Diff_SP_Avg, aes(x = Date, y = MAR_difference_cfs)) +
    geom_hline(yintercept = 0) +
    geom_line() +
    geom_point() +
    scale_y_continuous(limits = c(-40,40), breaks = seq(-40,40,by = 10), expand = c(0,0)) +
   scale_x_date(limits = c(as.Date('1990-10-1'),
                           as.Date('2011-9-30')),
                breaks = seq(as.Date("1990/10/1"), by = "2 years", length.out = 22), expand = c(0,0),
                date_labels = ('%b-%y')) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(fill=NA, color = 'black'),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
          axis.text.y = element_text(size = 8),
          axis.ticks = element_line(size = 0.2),
          plot.title = element_text(hjust = 0.5, size = 10),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 8))
)
(ILR_Monthly_Diff_Plot = ggplot(Flow_Diff_SP_Avg, aes(x = Date, y = ILR_difference_cfs)) +
    geom_hline(yintercept = 0) +
    geom_line() +
    geom_point() +
    scale_y_continuous(limits = c(-40,40), breaks = seq(-40,40,by = 10), expand = c(0,0)) +
    scale_x_date(limits = c(as.Date('1990-10-1'),
                            as.Date('2011-9-30')),
                 breaks = seq(as.Date("1990/10/1"), by = "2 years", length.out = 22), expand = c(0,0),
                 date_labels = ('%b-%y')) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(fill=NA, color = 'black'),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
          axis.text.y = element_text(size = 8),
          axis.ticks = element_line(size = 0.2),
          plot.title = element_text(hjust = 0.5, size = 10),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 8))
)
(MAR_ILR_Monthly_Diff_Plot = ggplot(Flow_Diff_SP_Avg, aes(x = Date, y = MAR_ILR_difference_cfs)) +
    geom_hline(yintercept = 0) +
    geom_line() +
    geom_point() +
    scale_y_continuous(limits = c(-40,40), breaks = seq(-40,40,by = 10), expand = c(0,0)) +
    scale_x_date(limits = c(as.Date('1990-10-1'),
                            as.Date('2011-9-30')),
                 breaks = seq(as.Date("1990/10/1"), by = "2 years", length.out = 22), expand = c(0,0),
                 date_labels = ('%b-%y')) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(fill=NA, color = 'black'),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
          axis.text.y = element_text(size = 8),
          axis.ticks = element_line(size = 0.2),
          plot.title = element_text(hjust = 0.5, size = 10),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 8))
)
#############################################################################################
####################           AVERAGE MONTHLY DIFFERENCE PLOTS          ####################
#############################################################################################
(MAR_Monthly_Avg_Plot = ggplot(data = Flow_Diff_Monthly_Avg) + 
    geom_ribbon(data = MAR_geom_ribbon_data[1:4,], aes(x = x,ymin = y, ymax = 0), fill = 'red', alpha=  0.5) +
    geom_ribbon(data = MAR_geom_ribbon_data[4:13,], aes(x = x,ymin = 0, ymax = y), fill = 'dodgerblue', alpha=  0.5) + 
    geom_hline(yintercept = 0) +
    geom_line(aes(x = seq(1,12), y = MAR_difference_cfs, group = 1)) +
    geom_errorbar(aes(x = seq(1,12), ymin = MAR_difference_cfs-MAR_difference_cfs_SD, 
                      ymax = MAR_difference_cfs+MAR_difference_cfs_SD, group = 1), width = 0.25) +
    geom_point(aes(x = seq(1,12), y = MAR_difference_cfs, group = 1),size = 1.5) +
    scale_x_continuous(limits = c(0.5,12.5), breaks = seq(1,12,by = 1), expand = c(0,0), labels = Flow_Diff_Monthly_Avg$Date) +
    scale_y_continuous(limits = c(-35,35), breaks = seq(-35,35,by = 10), expand = c(0,0)) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(fill=NA, color = 'black'),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
          axis.text.y = element_text(size = 8),
          axis.ticks = element_line(size = 0.2),
          plot.title = element_text(hjust = 0.5, size = 10),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 8))
)
(ILR_Monthly_Avg_Plot = ggplot(data = Flow_Diff_Monthly_Avg) + 
    geom_ribbon(data = ILR_geom_ribbon_data[1:4,], aes(x = x,ymin = 0, ymax = y), fill = 'dodgerblue', alpha=  0.5) +
    geom_ribbon(data = ILR_geom_ribbon_data[4:8,], aes(x = x,ymin = y, ymax = 0), fill = 'red', alpha=  0.5) +
    geom_ribbon(data = ILR_geom_ribbon_data[8:14,], aes(x = x,ymin = 0, ymax = y), fill = 'dodgerblue', alpha=  0.5) +
    geom_hline(yintercept = 0) +
    geom_line(aes(x = seq(1,12), y = ILR_difference_cfs, group = 1)) +
    geom_errorbar(aes(x = seq(1,12), ymin = ILR_difference_cfs-ILR_difference_cfs_SD, 
                      ymax = ILR_difference_cfs+ILR_difference_cfs_SD, group = 1), width = 0.25) +
    geom_point(aes(x = seq(1,12), y = ILR_difference_cfs, group = 1),size = 1.5) +
    scale_x_continuous(limits = c(0.5,12.5), breaks = seq(1,12,by = 1), expand = c(0,0), labels = Flow_Diff_Monthly_Avg$Date) +
    scale_y_continuous(limits = c(-35,35), breaks = seq(-35,35,by = 10), expand = c(0,0)) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(fill=NA, color = 'black'),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
          axis.text.y = element_text(size = 8),
          axis.ticks = element_line(size = 0.2),
          plot.title = element_text(hjust = 0.5, size = 10),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 8))
)
(MAR_ILR_Monthly_Avg_Plot = ggplot(data = Flow_Diff_Monthly_Avg) + 
    geom_ribbon(data = MAR_ILR_geom_ribbon_data[1:4,], aes(x = x,ymin = 0, ymax = y), fill = 'red', alpha=  0.5) +
    geom_ribbon(data = MAR_ILR_geom_ribbon_data[4:6,], aes(x = x,ymin = y, ymax = 0), fill = 'dodgerblue', alpha=  0.5) +
    geom_ribbon(data = MAR_ILR_geom_ribbon_data[6:9,], aes(x = x,ymin = 0, ymax = y), fill = 'red', alpha=  0.5) +
    geom_ribbon(data = MAR_ILR_geom_ribbon_data[9:15,], aes(x = x,ymin = 0, ymax = y), fill = 'dodgerblue', alpha=  0.5) +
    geom_hline(yintercept = 0) +
    geom_line(aes(x = seq(1,12), y = MAR_ILR_difference_cfs, group = 1)) +
    geom_point(aes(x = seq(1,12), y = MAR_ILR_difference_cfs, group = 1),size = 1.5) +
    geom_errorbar(aes(x = seq(1,12), ymin = MAR_ILR_difference_cfs-MAR_ILR_difference_cfs_SD, 
                      ymax = MAR_ILR_difference_cfs+MAR_ILR_difference_cfs_SD, group = 1), width = 0.25) +
    scale_x_continuous(limits = c(0.5,12.5), breaks = seq(1,12,by = 1), expand = c(0,0), labels = Flow_Diff_Monthly_Avg$Date) +
    scale_y_continuous(limits = c(-35,35), breaks = seq(-35,35,by = 10), expand = c(0,0)) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(fill=NA, color = 'black'),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
          axis.text.y = element_text(size = 8),
          axis.ticks = element_line(size = 0.2),
          plot.title = element_text(hjust = 0.5, size = 10),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 8))
)
#############################################################################################
###############           DRY, AVERAGE, AND WET YEAR DIFFERENCE PLOTS         ###############
#############################################################################################
Flow_Diff_SP_Dry_Avg_Wet = subset(Flow_Diff_SP_Avg,select = c('Date','MAR_difference_cfs', 'ILR_difference_cfs', 'MAR_ILR_difference_cfs'), 
                                  Date %in%c(seq(as.Date("2001/1/1"), by = "month", length.out = 12),seq(as.Date("2010/1/1"), by = "month", length.out = 12),seq(as.Date("2006/1/1"), by = "month", length.out = 12)))
Flow_Diff_SP_Dry_Avg_Wet$Year_Type = rep(c('Dry (2001)','Wet (2006)','Average (2010)'), each=12)
Flow_Diff_SP_Dry_Avg_Wet$Year_Type = factor(Flow_Diff_SP_Dry_Avg_Wet$Year_Type, levels = c('Dry (2001)','Average (2010)','Wet (2006)'))
Flow_Diff_SP_Dry_Avg_Wet = Flow_Diff_SP_Dry_Avg_Wet[order(Flow_Diff_SP_Dry_Avg_Wet$Year_Type),]
Flow_Diff_SP_Dry_Avg_Wet$Date = format(Flow_Diff_SP_Dry_Avg_Wet$Date, '%m')

(MAR_Dry_Avg_Wet_Diff_Plot = ggplot(data = Flow_Diff_SP_Dry_Avg_Wet, aes(x = as.numeric(Date), y = MAR_difference_cfs, group = Year_Type, color = Year_Type)) +
   geom_hline(yintercept = 0) +
   geom_line(size = 1) +
   geom_point(size = 1.5) +
   scale_y_continuous(limits = c(-30,30), breaks = seq(-30,30,by = 10), expand = c(0,0)) +
   scale_x_continuous(limits = c(0.5,12.5), breaks = seq(1,12,by = 1), expand = c(0,0), labels = format(seq(as.Date("2001/1/1"), by = "month", length.out = 12),'%b')) +
   scale_color_manual(values = c('red','black','blue')) +
   theme(panel.background = element_blank(),
         panel.border = element_rect(fill=NA, color = 'black'),
         axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
         axis.text.y = element_text(size = 8),
         axis.ticks = element_line(size = 0.2),
         plot.title = element_text(hjust = 0.5, size = 10),
         axis.title.x = element_blank(),
         axis.title.y = element_text(size = 8),
         legend.key = element_blank(),
         legend.title = element_blank(),
         legend.position = c(0.80,0.15),
         legend.background = element_blank())
)
(ILR_Dry_Avg_Wet_Diff_Plot = ggplot(data = Flow_Diff_SP_Dry_Avg_Wet, aes(x = as.numeric(Date), y = ILR_difference_cfs, group = Year_Type, color = Year_Type)) +
    geom_hline(yintercept = 0) +
    geom_line(size = 1) +
    geom_point(size = 1.5) +
    scale_y_continuous(limits = c(-30,30), breaks = seq(-30,30,by = 10), expand = c(0,0)) +
    scale_x_continuous(limits = c(0.5,12.5), breaks = seq(1,12,by = 1), expand = c(0,0), labels = format(seq(as.Date("2001/1/1"), by = "month", length.out = 12),'%b')) +
    scale_color_manual(values = c('red','black','blue')) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(fill=NA, color = 'black'),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
          axis.text.y = element_text(size = 8),
          axis.ticks = element_line(size = 0.2),
          plot.title = element_text(hjust = 0.5, size = 10),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 8),
          legend.key = element_blank(),
          legend.title = element_blank(),
          legend.position = c(0.80,0.15),
          legend.background = element_blank())
)
(MAR_ILR_Dry_Avg_Wet_Diff_Plot = ggplot(data = Flow_Diff_SP_Dry_Avg_Wet, aes(x = as.numeric(Date), y = MAR_ILR_difference_cfs, group = Year_Type, color = Year_Type)) +
    geom_hline(yintercept = 0) +
    geom_line(size = 1) +
    geom_point(size = 1.5) +
    scale_y_continuous(limits = c(-30,30), breaks = seq(-30,30,by = 10), expand = c(0,0)) +
    scale_x_continuous(limits = c(0.5,12.5), breaks = seq(1,12,by = 1), expand = c(0,0), labels = format(seq(as.Date("2001/1/1"), by = "month", length.out = 12),'%b')) +
    scale_color_manual(values = c('red','black','blue')) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(fill=NA, color = 'black'),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
          axis.text.y = element_text(size = 8),
          axis.ticks = element_line(size = 0.2),
          plot.title = element_text(hjust = 0.5, size = 10),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 8),
          legend.key = element_blank(),
          legend.title = element_blank(),
          legend.position = c(0.80,0.15),
          legend.background = element_blank())
)

if (graphics_type == 'pdf'){
  pdf('MAR_Flow_Diff_cfs.pdf', width = 7, height = 3)
} else if (graphics_type == 'png'){
  png('MAR_Flow_Diff_cfs.png', width = 7, height = 3, units = 'in', res = 600 )
}
grid.newpage()
pushViewport(viewport(layout = grid.layout(1,2)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(MAR_Monthly_Avg_Plot + 
      ylab('Streamflow Difference (cfs)'),
      vp = vplayout(1,1))
print(MAR_Dry_Avg_Wet_Diff_Plot +
       ylab(''),
      vp = vplayout(1,2))
graphics.off()

if (graphics_type == 'pdf'){
  pdf('ILR_Flow_Diff_cfs.pdf', width = 7, height = 3)
} else if (graphics_type == 'png'){
  png('ILR_Flow_Diff_cfs.png', width = 7, height = 3, units = 'in', res = 600 )
}
grid.newpage()
pushViewport(viewport(layout = grid.layout(1,2)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(ILR_Monthly_Avg_Plot + 
        ylab('Streamflow Difference (cfs)'),
      vp = vplayout(1,1))
print(ILR_Dry_Avg_Wet_Diff_Plot +
        ylab(''),
      vp = vplayout(1,2))
graphics.off()

if (graphics_type == 'pdf'){
  pdf('MAR_ILR_Flow_Diff_cfs.pdf', width = 7, height = 3)
} else if (graphics_type == 'png'){
  png('MAR_ILR_Flow_Diff_cfs.png', width = 7, height = 3, units = 'in', res = 600 )
}
grid.newpage()
pushViewport(viewport(layout = grid.layout(1,2)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(MAR_ILR_Monthly_Avg_Plot + 
        ylab('Streamflow Difference (cfs)'),
      vp = vplayout(1,1))
print(MAR_ILR_Dry_Avg_Wet_Diff_Plot +
        ylab(''),
      vp = vplayout(1,2))
graphics.off()

ILR_Pumping_Reduction_Vol_Plot = ggplot(ILR_GW_Reduction, aes(x=Year, y = Reduction_TAF)) + 
  geom_line() +
  ylab('Volume (TAF)')+
  ggtitle('ILR Groundwater Pumping Reduction') +
  scale_x_continuous(limits = c(1990.5,2011.5), breaks = seq(1991,2011, by = 2), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,8), breaks = seq(0,8,by = 2), expand = c(0,0)) +
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill=NA, color = 'black'),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 10),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(size = 0.2),
        plot.title = element_text(hjust = 0.5, size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.80,0.15),
        legend.background = element_blank())

ILR_Pumping_Reduction_Pct_Plot = ggplot(ILR_GW_Reduction, aes(x=Year, y = Reduction_pct)) + 
  geom_line() +
  ylab('Percent')+
  ggtitle('ILR Groundwater Pumping Reduction') +
  scale_x_continuous(limits = c(1990.5,2011.5), breaks = seq(1991,2011, by = 2), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,20), breaks = seq(0,20,by = 5), expand = c(0,0)) +
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill=NA, color = 'black'),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 10),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(size = 0.2),
        plot.title = element_text(hjust = 0.5, size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.80,0.15),
        legend.background = element_blank())

png('ILR_Pumping_Reductions.png', width = 7, height = 3, units = 'in', res = 600 )
grid.newpage()
pushViewport(viewport(layout = grid.layout(1,2)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(ILR_Pumping_Reduction_Vol_Plot, vp = vplayout(1,1))
print(ILR_Pumping_Reduction_Pct_Plot, vp = vplayout(1,2))
graphics.off()


# png(paste0('MAR_Flow_Diff_cfs_Monthly_Avg.png'), res = 600, width = 5, height = 4, units = 'in')
# MAR_Monthly_Avg_Plot + ylab('Streamflow Difference (cfs)') + ggtitle('MAR')
# graphics.off()
# png(paste0('ILR_Flow_Diff_cfs_Monthly_Avg.png'), res = 600, width = 5, height = 4, units = 'in')
# ILR_Monthly_Avg_Plot + ylab('Streamflow Difference (cfs)') + ggtitle('ILR')
# graphics.off()
# png(paste0('MAR_ILR_Flow_Diff_cfs_Monthly_Avg.png'), res = 600, width = 5, height = 4, units = 'in')
# MAR_ILR_Monthly_Avg_Plot + ylab('Streamflow Difference (cfs)') + ggtitle('MAR + ILR')
# graphics.off()
# 
# png(paste0('MAR_Flow_Diff_Dry_Avg_Wet.png'), res = 600, width = 5, height = 4, units = 'in')
# MAR_Dry_Avg_Wet_Diff_Plot + ylab('Streamflow Difference (cfs)') + ggtitle('MAR')
# graphics.off()
# png(paste0('ILR_Flow_Diff_Dry_Avg_Wet.png'), res = 600, width = 5, height = 4, units = 'in')
# ILR_Dry_Avg_Wet_Diff_Plot + ylab('Streamflow Difference (cfs)') + ggtitle('ILR')
# graphics.off()
# png(paste0('MAR_ILR_Flow_Diff_Dry_Avg_Wet.png'), res = 600, width = 5, height = 4, units = 'in')
# MAR_ILR_Dry_Avg_Wet_Diff_Plot + ylab('Streamflow Difference (cfs)') + ggtitle('MAR + ILR')
# graphics.off()
