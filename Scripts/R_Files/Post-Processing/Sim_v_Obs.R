rm(list=ls())
library(ggplot2)
library(reshape2)
library(dplyr)
library(grid)
library(rstudioapi)

gages = c('FJ','AS','BY','LS')
#FJ = Fort Jones gage (USGS)
#AS = Above Serpa Lane Bridge (RCD)
#BY = Below Youngs Dam (RCD)
#LS = Lower Shackleford (CADWR)

###########################################################################################################
########################                    USER INPUT                       ##############################
###########################################################################################################
svihm_dir = dirname(dirname(dirname(getActiveDocumentContext()$path)))
ref_dir = file.path(svihm_dir, "SVIHM_Input_Files", "reference_data")
mf_results_dir = file.path(svihm_dir, "MODFLOW", "hist") # historical 1991-2018 wy 
postproc_dir = file.path(svihm_dir, "R_Files", "Post-Processing")

copy_these_files=c("SVIHM_Flow_Obs_Times.obs","Streamflow_FJ_SVIHM.dat")
file.copy(from=file.path(c(ref_dir, mf_results_dir),copy_these_files), to = postproc_dir)

units_cfs = FALSE   #If true, output units will be in cfs. If false output units will be in m^3/day
dir.create(file.path(postproc_dir,'Results'), showWarnings = FALSE)   #Create Results directory if it doesn't exist
out_dir = file.path(postproc_dir,'Results')
options(warn=-1)   # suppress warnings (set to 0 to turn warnings on)
###########################################################################################################
########################                    IMPORT STREAMFLOW                ##############################
###########################################################################################################
Steamflow_Obs_Times = read.table('SVIHM_Flow_Obs_Times.obs', header = T, fill = T) #Where is this file? 
Steamflow_Obs_Times$FJ = as.Date(Steamflow_Obs_Times$FJ, format = '%m/%d/%Y')
Steamflow_Obs_Times$LS = as.Date(Steamflow_Obs_Times$LS, format = '%m/%d/%Y')
Steamflow_Obs_Times$AS = as.Date(Steamflow_Obs_Times$AS, format = '%m/%d/%Y')
Steamflow_Obs_Times$BY = as.Date(Steamflow_Obs_Times$BY, format = '%m/%d/%Y')

#Create .obs file in results
# FJ_obs_file = "C:/Users/Claire/Documents/GitHub/SVIHM/Streamflow_Regression_Model/USGS_11519500_WY_1942_2018.txt"
# FJ_obs_all = read.table(FJ_obs_file, header = T)
# dates = as.Date(FJ_obs_all$Date, format = "%m/%d/%Y")
# FJ_obs_1990_2018 = FJ_obs_all[dates >= as.Date("1990-10-01") & dates < as.Date("2018-10-01"),]
# write.table(FJ_obs_1990_2018,file.path(postproc_dir,'SVIHM_FJ_1990-2011.obs'))

#read 
FJ_obs_cfs = read.table(file.path(postproc_dir,'SVIHM_FJ_1990-2011.obs'), header=TRUE)[c(1,2)]
names(FJ_obs_cfs) = c('Date','Streamflow_obs_FJ_cfs')

FJ_sim_m3day = read.table(file.path(postproc_dir,'Streamflow_FJ_SVIHM.dat'),skip=2)[c(1,3)]
names(FJ_sim_m3day) = c('Date', 'm3day')
FJ_sim_m3day$Date = as.Date(FJ_sim_m3day$Date, origin = '1990-09-30')
FJ_cfs = data.frame(Date = as.Date(FJ_obs_cfs$Date, format = "%m/%d/%Y"), 
                                   Observed = FJ_obs_cfs$Streamflow_obs_FJ_cfs, 
                                   Simulated = FJ_sim_m3day$m3day*0.000408734569)

#UCODE Residuals
UCODE_obs_FJ = FJ_cfs[which(FJ_cfs$Date %in% Steamflow_Obs_Times$FJ),]
FJ_residuals_cfs = data.frame(Residual = UCODE_obs_FJ$Observed - UCODE_obs_FJ$Simulated)
FJ_residuals_m3day = FJ_residuals_cfs*2446.58
FJ_residuals_cfs[FJ_residuals_cfs$Residual<0,] = -log10(-FJ_residuals_cfs[FJ_residuals_cfs$Residual<0,])
FJ_residuals_cfs[FJ_residuals_cfs$Residual>0,] = log10(FJ_residuals_cfs[FJ_residuals_cfs$Residual>0,])
FJ_residuals_m3day[FJ_residuals_m3day$Residual<0,] = -log10(-FJ_residuals_m3day[FJ_residuals_m3day$Residual<0,])
FJ_residuals_m3day[FJ_residuals_m3day$Residual>0,] = log10(FJ_residuals_m3day[FJ_residuals_m3day$Residual>0,])

#########################################################
#Streamflows for Above Serpa Lane, Below Youngs Dam, and Lower Shackleford Creek
for (i in 2:length(gages)){
  obs_cfs_text = readLines(file.path(postproc_dir,paste0('SVIHM_',gages[i],'.obs')))
  obs_cfs = data.frame(Date = as.Date(sapply(lapply(strsplit(obs_cfs_text[seq(2,length(obs_cfs_text))],' '),function(x){x[!x ==""]}),"[[",1),'%m/%d/%Y'))
  obs_cfs$Observed = as.numeric(sapply(lapply(strsplit(obs_cfs_text[seq(2,length(obs_cfs_text))],' '),function(x){x[!x ==""]}),"[[",3))
  idx <- c(1, diff(obs_cfs$Date))  #create grouping value in case data is discontinuous
  i2 <- c(1,which(idx != 1), nrow(obs_cfs)+1)
  obs_cfs$grp <- rep(1:length(diff(i2)), diff(i2))
  sim_trib_m3day = read.table('Streamflow_AS_SVIHM.dat',skip=2)[c(1,3)]  #import Simulation time and streamflow
  sim_trib_m3day$V1 = as.Date(sim_trib_m3day$V1, origin = "1990-09-30")
  names(sim_trib_m3day) = c('Date', 'm3day')
  sim_trib_cfs = sim_trib_m3day
  names(sim_trib_cfs) = c('Date', 'cfs')
  sim_trib_cfs$cfs = sim_trib_cfs$cfs*0.000408734569
  trib_cfs = left_join(sim_trib_cfs,obs_cfs, by = 'Date')
  names(trib_cfs) = c('Date','Simulated','Observed','Obs_grp')
  trib_cfs_UCODE_Obs = trib_cfs[which(trib_cfs$Date %in% Steamflow_Obs_Times$AS),]
  trib_residuals_cfs = na.omit(as.data.frame(trib_cfs_UCODE_Obs$Observed - trib_cfs_UCODE_Obs$Simulated))
  names(trib_residuals_cfs) = 'Residual'
  trib_residuals_m3day = trib_residuals_cfs*2446.58
  trib_residuals_cfs[trib_residuals_cfs$Residual<0,] = -log10(-trib_residuals_cfs[trib_residuals_cfs$Residual<0,])
  trib_residuals_cfs[trib_residuals_cfs$Residual>0,] = log10(trib_residuals_cfs[trib_residuals_cfs$Residual>0,])
  trib_residuals_m3day[trib_residuals_m3day$Residual<0,] = -log10(-trib_residuals_m3day[trib_residuals_m3day$Residual<0,])
  trib_residuals_m3day[trib_residuals_m3day$Residual>0,] = log10(trib_residuals_m3day[trib_residuals_m3day$Residual>0,])
  
  eval(parse(text = paste0(gages[i],'_cfs = trib_cfs')))
  eval(parse(text = paste0(gages[i],'_residuals_cfs = trib_residuals_cfs')))
  eval(parse(text = paste0(gages[i],'_residuals_m3day = trib_residuals_m3day')))
}

###########################################################################################################
#######################          CALCULATE NASH-SUTCLIFFE EFFICIENCIES        #############################
###########################################################################################################
#Fort Jones
NSE_FJ = round(1 - (sum((log10(FJ_cfs$Simulated) - log10(FJ_cfs$Observed))^2))/(sum(((log10(FJ_cfs$Observed) - mean(log10(FJ_cfs$Observed)))^2))),2)

#Above Serpa Lane
AS_cfs_NSE = AS_cfs[complete.cases(AS_cfs),]
AS_cfs_NSE = AS_cfs_NSE[which(AS_cfs_NSE$Simulated>0 & AS_cfs_NSE$Observed>0),]
NSE_AS = round(1 - (sum((log10(AS_cfs_NSE$Simulated) - log10(AS_cfs_NSE$Observed))^2))/(sum(((log10(AS_cfs_NSE$Observed) - mean(log10(AS_cfs_NSE$Observed)))^2))),2)

#Below Youngs Dam
BY_cfs_NSE = BY_cfs[complete.cases(BY_cfs),]
BY_cfs_NSE = BY_cfs_NSE[which(BY_cfs_NSE$Simulated>0 & BY_cfs_NSE$Observed>0),]
NSE_BY = round(1 - (sum((log10(BY_cfs_NSE$Simulated) - log10(BY_cfs_NSE$Observed))^2))/(sum(((log10(BY_cfs_NSE$Observed) - mean(log10(BY_cfs_NSE$Observed)))^2))),2)

#Lower Shackleford Creek
LS_cfs_NSE = LS_cfs[complete.cases(LS_cfs),]
LS_cfs_NSE = LS_cfs_NSE[which(LS_cfs_NSE$Simulated>0 & LS_cfs_NSE$Observed>0),]
NSE_LS = round(1 - (sum((log10(LS_cfs_NSE$Simulated) - log10(LS_cfs_NSE$Observed))^2))/(sum(((log10(LS_cfs_NSE$Observed) - mean(log10(LS_cfs_NSE$Observed)))^2))),2)

###########################################################################################################
########################                       IMPORT HEADS                  ##############################
###########################################################################################################
Head_Data = read.table('HobData_SVIHM.dat',skip = 1)
names(Head_Data) = c('Simulated', 'Observed', 'ID')
Head_Data$Residual = Head_Data$Observed-Head_Data$Simulated

head_regress = lm(Head_Data$Simulated~Head_Data$Observed)
regress_eqn = paste('y = ',round(coef(head_regress)[2],2),'x + ',round(coef(head_regress)[1],2), sep = '')
R2 <- paste("R^2 == ", round(summary(head_regress)$r.square,2))
###########################################################################################################
########################                    STREAMFLOW PLOTS                 ##############################
###########################################################################################################
if (units_cfs==TRUE) {
#USGS Fort Jones Gage
FJ_cfs_melt = melt(FJ_cfs, id.vars = 'Date')
(FJ_streamflow_plot = ggplot(FJ_cfs_melt,aes(x = Date, y = value, col = variable)) + 
  geom_line(size = 1) +
  annotation_logticks() + 
  ylab('Streamflow (cfs)') +
  ggtitle('Streamflow - USGS Gage') +
  scale_y_log10(limits = c(1,1E5), breaks = c(1 %o% 10^(0:5)), expand = c(0,0)) +
  scale_x_date(limits = c(as.Date('1990-10-01'),as.Date('2011-10-01')),date_breaks = '24 months', expand = c(0,0),date_labels = "%b-%y") +
    theme(panel.background = element_blank(), panel.border = element_rect(fill = NA),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), axis.title.x = element_blank(),
        legend.position = c(0.5,0.05), legend.direction = 'horizontal', legend.title = element_blank(),
        legend.key = element_rect(fill = NA, color = NA), legend.background = element_rect(fill = NA),
        legend.key.height = unit(0.1, 'in'),
        plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c('blue', 'red')) +
  annotate("text", x = as.Date('2001-06-01'), y = 4E4, label = paste('NSE = ',NSE_FJ)))
# pdf(paste0(out_dir,'/Streamflow_FJ_cfs.pdf'),width = 8, height = 5)
# FJ_streamflow_plot
# graphics.off()

(FJ_residuals_plot = ggplot(data = FJ_residuals_cfs, aes(x = seq(1,length(FJ_residuals_cfs$Residual)),y=Residual)) + 
  geom_point(shape =21,color = 'black', fill = 'red',stroke = 0.5, size = 2) +
  geom_abline(intercept=0, slope=0, col = 'black') +
  xlab('Observation') +
  ylab('log[Residual (cfs)]') +
  ggtitle('Streamflow Residuals - USGS Gage') +
  scale_y_continuous(limits = c(-5,5), breaks = seq(-5,5), expand = c(0,0)) +
  scale_x_continuous(limits = c(1,round(length(FJ_residuals_cfs$Residual),-2)), breaks = seq(0,round(length(FJ_residuals_cfs$Residual),-2),by = 200),expand = c(0.02,0.02)) +
  theme(panel.background = element_blank(), panel.border = element_rect(fill = NA),
        plot.title = element_text(hjust = 0.5)))
# pdf(paste0(out_dir,'/Residuals_FJ_cfs.pdf'),width = 8, height = 5)
# FJ_residuals_plot
# graphics.off() 

#Streamflow Above Serpa Lane
(AS_streamflow_plot = ggplot(AS_cfs,aes(x = Date)) + 
    geom_line(aes(y = Observed, color = 'Observed'),size = 1) +
    geom_line(aes(y=Simulated, color = 'Simulated'),size = 1) +
    scale_color_manual(values = c('Blue','Red')) +
    scale_y_log10(limits = c(1,1E5), breaks = c(1 %o% 10^(0:5)), expand = c(0,0)) +
    annotation_logticks() + 
    ylab('Streamflow (cfs)') +
    ggtitle('Streamflow - Above Serpa Lane') +
    scale_x_date(limits = c(as.Date('2007-10-01'), as.Date('2011-10-01')),
                 date_breaks = '12 months', expand = c(0,0),date_labels = "%b-%y") +
    theme(panel.background = element_blank(), panel.border = element_rect(fill = NA),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), axis.title.x = element_blank(),
          legend.position = 'none', legend.title = element_blank(), legend.direction = 'horizontal',
          legend.key = element_rect(fill = NA), legend.background = element_rect(fill = NA),
          plot.title = element_text(hjust = 0.5)) +
    annotate("text", x = as.Date('2010-11-01'), y = 4E4, label = paste('NSE = ',NSE_AS)))
# pdf(paste0(out_dir,'/Streamflow_AS_cfs.pdf'),width = 8, height = 5)
# AS_streamflow_plot
# graphics.off()

#Above Serpa Lane Streamflow Residuals
(AS_residuals_plot = ggplot(data = AS_residuals_cfs, aes(x = seq(1,length(AS_residuals_cfs$Residual)),y=Residual)) + 
    geom_point(shape =21,color = 'black', fill = 'red',stroke = 0.5, size = 2) +
    geom_abline(intercept=0, slope=0, col = 'black') +
    xlab('Observation') +
    ylab('log[Residual (cfs)]') +
    ggtitle('Streamflow Residuals - Above Serpa Lane') +
    scale_y_continuous(limits = c(-5,5), breaks = seq(-5,5, by=1), expand = c(0,0)) +
    scale_x_continuous(limits = c(1,round(length(AS_residuals_cfs$Residual),-2)), breaks = seq(0,round(length(AS_residuals_cfs$Residual),-2),by = 100),expand = c(0.02,0.02)) +
    theme(panel.background = element_blank(), panel.border = element_rect(fill = NA),
          plot.title = element_text(hjust = 0.5)))
# pdf(paste0(out_dir,'/Residuals_AS_cfs.pdf'),width = 8, height = 5)
# AS_residuals_plot
# graphics.off()
  
#Streamflow Below Youngs Dam
(BY_streamflow_plot = ggplot(BY_cfs,aes(x = Date)) + 
  geom_line(aes(y=Observed, group = Obs_grp, color = 'Observed'), size = 1) +
  geom_line(aes(y=Simulated, color = 'Simulated'),size = 1) +
  scale_color_manual(values = c('Blue','Red')) +
  scale_y_log10(limits = c(1,1E5), breaks = c(1 %o% 10^(0:5)), expand = c(0,0)) +
  annotation_logticks() + 
  ylab('Streamflow (cfs)') +
  ggtitle('Streamflow - Below Youngs Dam') +
  scale_x_date(limits = c(as.Date('2007-10-01'), as.Date('2011-10-01')),
               date_breaks = '12 months', expand = c(0,0),date_labels = "%b-%y") +
  theme(panel.background = element_blank(), panel.border = element_rect(fill = NA),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), axis.title.x = element_blank(),
        legend.direction = 'horizontal', legend.title = element_blank(),
        legend.key = element_rect(fill = NA), legend.background = element_rect(fill = NA),
        legend.key.height = unit(0.1, 'in'), legend.position = 'none',
        plot.title = element_text(hjust = 0.5)) +
   annotate("text", x = as.Date('2010-11-01'), y = 4E4, label = paste('NSE = ',NSE_BY)))
# pdf(paste0(out_dir,'/Streamflow_BY_cfs.pdf'),width = 8, height = 5)
# BY_streamflow_plot
# graphics.off()

#Below Youngs Dam Streamflow Residuals
(BY_residuals_plot = ggplot(data = BY_residuals_cfs, aes(x = seq(1,length(BY_residuals_cfs$Residual)),y=BY_residuals_cfs$Residual)) + 
    geom_point(shape =21,color = 'black', fill = 'red',stroke = 0.5, size = 2) +
    geom_abline(intercept=0, slope=0, col = 'black') +
    xlab('Observation') +
    ylab('log[Residual (cfs)]') +
    ggtitle('Streamflow Residuals - Below Youngs Dam') +
    scale_y_continuous(limits = c(-5,5), breaks = seq(-5,5), expand = c(0,0)) +
    scale_x_continuous(limits = c(1,round(length(BY_residuals_cfs$Residual),-2)), breaks = seq(0,round(length(BY_residuals_cfs$Residual),-2),by = 100),expand = c(0.02,0.02)) +
    theme(panel.background = element_blank(), panel.border = element_rect(fill = NA),
          plot.title = element_text(hjust = 0.5)))
# pdf(paste0(out_dir,'/Residuals_BY_cfs.pdf'),width = 8, height = 5)
# BY_residuals_plot
# graphics.off()

#Streamflow Lower Shackleford Creek
(LS_streamflow_plot = ggplot(LS_cfs,aes(x = Date)) +
    geom_line(aes(y=Observed, group = Obs_grp, color = 'Observed'),size = 1) +
    geom_line(aes(y = Simulated, color = 'Simulated'), size = 1) +
    scale_color_manual(values = c('Blue', 'Red')) +
    scale_y_log10(limits = c(1,1E5), breaks = c(1 %o% 10^(0:5)), expand = c(0,0)) +
    annotation_logticks() +
    ylab('Streamflow (cfs)') +
    ggtitle('Streamflow - Lower Shackleford') +
    scale_x_date(limits = c(as.Date('2004-10-01'), as.Date('2011-10-01')),
                 date_breaks = '12 months', expand = c(0,0),date_labels = "%b-%y") +
    theme(panel.background = element_blank(), panel.border = element_rect(fill = NA),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), axis.title.x = element_blank(),
          legend.position = 'none', legend.direction = 'horizontal', legend.title = element_blank(),
          legend.key = element_rect(fill = NA), legend.background = element_rect(fill = NA),
          legend.key.height = unit(0.1, 'in'),
          plot.title = element_text(hjust = 0.5)) +
    annotate("text", x = as.Date('2010-4-01'), y = 4E4, label = paste('NSE = ',NSE_LS)))
# pdf(paste0(out_dir,'/Streamflow_LS_cfs.pdf'),width = 8, height = 5)
# LS_streamflow_plot
# graphics.off()

#Shackleford Streamflow Residuals
(LS_residuals_plot = ggplot(data = LS_residuals_cfs, aes(x = seq(1,length(LS_residuals_cfs$Residual)),y=LS_residuals_cfs$Residual)) + 
    geom_point(shape =21,color = 'black', fill = 'red',stroke = 0.5, size = 2) +
    geom_abline(intercept=0, slope=0, col = 'black') +
    xlab('Observation') +
    ylab('log[Residual (cfs)]') +
    ggtitle('Streamflow Residuals - Lower Shackleford') +
    scale_y_continuous(limits = c(-5,5), breaks = seq(-5,5), expand = c(0,0)) +
    scale_x_continuous(limits = c(1,length(LS_residuals_cfs$Residual)), breaks = seq(0,length(LS_residuals_cfs$Residual),by = 100),expand = c(0.02,0.02)) +
    theme(panel.background = element_blank(), panel.border = element_rect(fill = NA),
          plot.title = element_text(hjust = 0.5)))
# pdf(paste0(out_dir,'/Residuals_LS_cfs.pdf'),width = 8, height = 5)
# LS_residuals_plot
# graphics.off()


##########################################################################################################
########################                       HEAD PLOTS                   ##############################
##########################################################################################################
(one2one_plot = ggplot(Head_Data,aes(x = Observed*3.28, y=Simulated*3.28)) + 
  geom_point(size = 1, color = 'navy') +
  geom_abline(intercept=0,slope=1, col = 'black') +
  scale_y_continuous(limits = c(2600,3100), breaks = seq(2600,3100,by=100), expand = c(0,0)) +
  scale_x_continuous(limits = c(2600,3100), breaks = seq(2600,3100,by=100), expand = c(0,0)) +
  ylab('Simulated (ft)') +
  xlab('Observed (ft)') +
  ggtitle('Groundwater Heads') +
  theme(panel.background = element_blank(), panel.border = element_rect(fill = NA),
        plot.title = element_text(hjust = 0.5)) +
  coord_fixed() +
  annotate('text', x = 2790, y = 3020, label = regress_eqn, size = 3.2) +
  annotate('text', x = 2790, y = 2985, label = R2, parse = T, size = 3.2))
# pdf(paste0(out_dir,'/Heads_Sim_vs_Observed_ft.pdf'),width = 8, height = 5)
# one2one_plot
# graphics.off()

(head_residuals_plot = ggplot(Head_Data,aes(x = seq(1,length(Head_Data$Residual)),y=Residual*3.28)) + 
  geom_point(size = 1, color = 'navy') +
  geom_abline(intercept=0, slope=0, col = 'black') +
  scale_y_continuous(limits = c(-60,60), breaks = seq(-60,60,15), expand = c(0,0)) +
  scale_x_continuous(limits = c(1,round(length(Head_Data$Residual),-2)), breaks = seq(0,round(length(Head_Data$Residual),-2),by = 500), expand = c(0.02,0.02)) +
  ylab('Residual (ft)') +
  xlab('Head Observation') +
  ggtitle('Head Residuals') +
  theme(panel.background = element_blank(), panel.border = element_rect(fill = NA),
        plot.title = element_text(hjust = 0.5)))
# pdf(paste0(out_dir,'/Head_Residuals_ft.pdf'),width = 8, height = 5)
# head_residuals_plot
# graphics.off()
} else {
  ##########################################################################################################
  ########################                         m^3/day                    ##############################
  ##########################################################################################################
  
  #USGS Fort Jones Gage
  FJ_cfs_melt = melt(FJ_cfs, id.vars = 'Date')
  (FJ_streamflow_plot = ggplot(FJ_cfs_melt,aes(x = Date, y = value*2446.58, col = variable)) + 
      geom_line(size = 1) +
      annotation_logticks() + 
      ylab(bquote('Streamflow ('*m^3*d^{-1}*')')) +
      ggtitle('Streamflow - USGS Gage') +
      scale_y_log10(limits = c(1E3,1E8), breaks = c(1 %o% 10^(3:8)), expand = c(0,0)) +
      scale_x_date(limits = c(as.Date('1990-10-01'),as.Date('2011-10-01')),date_breaks = '24 months', expand = c(0,0),date_labels = "%b-%y") +
      #scale_x_date(limits = c(as.Date('2003-01-01'),as.Date('2003-12-31')),date_breaks = '1 months', expand = c(0,0),date_labels = "%b-%y") +
      theme(panel.background = element_blank(), panel.border = element_rect(fill = NA),
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), axis.title.x = element_blank(),
            legend.position = c(0.5,0.05), legend.direction = 'horizontal', legend.title = element_blank(),
            legend.key = element_rect(fill = NA, color = NA), legend.background = element_rect(fill = NA),
            legend.key.height = unit(0.1, 'in'),
            plot.title = element_text(hjust = 0.5)) +
      scale_color_manual(values = c('blue', 'red')) +
      annotate("text", x = as.Date('2001-06-01'), y = 4E7, label = paste('NSE = ',NSE_FJ)))
  # pdf(paste0(out_dir,'/Streamflow_FJ_m3day.pdf'),width = 8, height = 5)
  # FJ_streamflow_plot
  # graphics.off()

  (FJ_residuals_plot = ggplot(data = FJ_residuals_m3day, aes(x = seq(1,length(FJ_residuals_m3day$Residual)),y=Residual)) + 
      geom_point(shape =21,color = 'black', fill = 'red',stroke = 0.5, size = 2) +
      geom_abline(intercept=0, slope=0, col = 'black') +
      xlab('Observation') +
      ylab(bquote('log[Residual ('*m^3*d^{-1}*')]')) +
      ggtitle('Streamflow Residuals - USGS Gage') +
      scale_y_continuous(limits = c(-8,8), breaks = seq(-8,8, by = 2), expand = c(0,0)) +
      scale_x_continuous(limits = c(1,round(length(FJ_residuals_m3day$Residual),-2)), breaks = seq(0,round(length(FJ_residuals_m3day$Residual),-2),by = 200),expand = c(0.02,0.02)) +
      theme(panel.background = element_blank(), panel.border = element_rect(fill = NA),
            plot.title = element_text(hjust = 0.5)))
  # pdf(paste0(out_dir,'/Residuals_FJ_m3day.pdf'),width = 8, height = 5)
  # FJ_residuals_plot
  # graphics.off()
  
  #Streamflow Above Serpa Lane
  (AS_streamflow_plot = ggplot(AS_cfs,aes(x = Date)) + 
      geom_line(aes(y = Observed*2446.58, color = 'Observed'),size = 1) +
      geom_line(aes(y=Simulated*2446.58, color = 'Simulated'),size = 1) +
      scale_color_manual(values = c('Blue','Red')) +
      scale_y_log10(limits = c(1E3,1E8), breaks = c(1 %o% 10^(3:8)), expand = c(0,0)) +
      annotation_logticks() + 
      ylab(bquote('Streamflow ('*m^3*d^{-1}*')')) +
      ggtitle('Streamflow - Above Serpa Lane') +
      scale_x_date(limits = c(as.Date('2007-10-01'), as.Date('2011-10-01')),
                   date_breaks = '12 months', expand = c(0,0),date_labels = "%b-%y") +
      theme(panel.background = element_blank(), panel.border = element_rect(fill = NA),
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), axis.title.x = element_blank(),
            legend.position = 'none', legend.title = element_blank(), legend.direction = 'horizontal',
            legend.key = element_rect(fill = NA), legend.background = element_rect(fill = NA),
            plot.title = element_text(hjust = 0.5)) +
      annotate("text", x = as.Date('2010-11-01'), y = 4E7, label = paste('NSE = ',NSE_AS)))
  # pdf(paste0(out_dir,'/Streamflow_AS_m3day.pdf'),width = 8, height = 5)
  # AS_streamflow_plot
  # graphics.off()
  
  #Above Serpa Lane Streamflow Residuals
  (AS_residuals_plot = ggplot(data = AS_residuals_m3day, aes(x = seq(1,length(AS_residuals_m3day$Residual)),y=Residual)) + 
      geom_point(shape =21,color = 'black', fill = 'red',stroke = 0.5, size = 2) +
      geom_abline(intercept=0, slope=0, col = 'black') +
      xlab('Observation') +
      ylab(bquote('log[Residual ('*m^3*d^{-1}*')]')) +
      ggtitle('Streamflow Residuals - Above Serpa Lane') +
      scale_y_continuous(limits = c(-8,8), breaks = seq(-8,8, by=2), expand = c(0,0)) +
      scale_x_continuous(limits = c(1,round(length(AS_residuals_m3day$Residual),-2)), breaks = seq(0,round(length(AS_residuals_m3day$Residual),-2),by = 100),expand = c(0.02,0.02)) +
      theme(panel.background = element_blank(), panel.border = element_rect(fill = NA),
            plot.title = element_text(hjust = 0.5)))
  # pdf(paste0(out_dir,'/Residuals_AS_m3day.pdf'),width = 8, height = 5)
  # AS_residuals_plot
  # graphics.off()
  
  #Streamflow Below Youngs Dam
  (BY_streamflow_plot = ggplot(BY_cfs,aes(x = Date)) + 
      geom_line(aes(y=Observed*2446.58, group = Obs_grp, color = 'Observed'), size = 1) +
      geom_line(aes(y=Simulated*2446.58, color = 'Simulated'),size = 1) +
      scale_color_manual(values = c('Blue','Red')) +
      scale_y_log10(limits = c(1E3,1E8), breaks = c(1 %o% 10^(3:8)), expand = c(0,0)) +
      annotation_logticks() + 
      ylab(bquote('Streamflow ('*m^3*d^{-1}*')')) +
      ggtitle('Streamflow - Below Youngs Dam') +
      scale_x_date(limits = c(as.Date('2007-10-01'), as.Date('2011-10-01')),
                   date_breaks = '12 months', expand = c(0,0),date_labels = "%b-%y") +
      theme(panel.background = element_blank(), panel.border = element_rect(fill = NA),
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), axis.title.x = element_blank(),
            legend.direction = 'horizontal', legend.title = element_blank(),
            legend.key = element_rect(fill = NA), legend.background = element_rect(fill = NA),
            legend.key.height = unit(0.1, 'in'), legend.position = 'none',
            plot.title = element_text(hjust = 0.5)) +
      annotate("text", x = as.Date('2010-11-01'), y = 4E7, label = paste('NSE = ',NSE_BY)))
  # pdf(paste0(out_dir,'/Streamflow_BY_m3day.pdf'),width = 8, height = 5)
  # BY_streamflow_plot
  # graphics.off()
  
  #Below Youngs Dam Streamflow Residuals
  (BY_residuals_plot = ggplot(data = BY_residuals_m3day, aes(x = seq(1,length(BY_residuals_m3day$Residual)),y=BY_residuals_m3day$Residual)) + 
      geom_point(shape =21,color = 'black', fill = 'red',stroke = 0.5, size = 2) +
      geom_abline(intercept=0, slope=0, col = 'black') +
      xlab('Observation') +
      ylab(bquote('log[Residual ('*m^3*d^{-1}*')]')) +
      ggtitle('Streamflow Residuals - Below Youngs Dam') +
      scale_y_continuous(limits = c(-8,8), breaks = seq(-8,8,by=2), expand = c(0,0)) +
      scale_x_continuous(limits = c(1,round(length(BY_residuals_m3day$Residual),-2)), breaks = seq(0,round(length(BY_residuals_m3day$Residual),-2),by = 100),expand = c(0.02,0.02)) +
      theme(panel.background = element_blank(), panel.border = element_rect(fill = NA),
            plot.title = element_text(hjust = 0.5)))
  # pdf(paste0(out_dir,'/Residuals_BY_m3day.pdf'),width = 8, height = 5)
  # BY_residuals_plot
  # graphics.off()
  
  #Streamflow Lower Shackleford Creek
  (LS_streamflow_plot = ggplot(LS_cfs,aes(x = Date)) +
      geom_line(aes(y=Observed*2446.58, group = Obs_grp, color = 'Observed'),size = 1) +
      geom_line(aes(y = Simulated*2446.58, color = 'Simulated'), size = 1) +
      scale_color_manual(values = c('Blue', 'Red')) +
      scale_y_log10(limits = c(1E3,1E8), breaks = c(1 %o% 10^(3:8)), expand = c(0,0)) +
      annotation_logticks() +
      ylab(bquote('Streamflow ('*m^3*d^{-1}*')')) +
      ggtitle('Streamflow - Lower Shackleford') +
      scale_x_date(limits = c(as.Date('2004-10-01'), as.Date('2011-10-01')),
                   date_breaks = '12 months', expand = c(0,0),date_labels = "%b-%y") +
      theme(panel.background = element_blank(), panel.border = element_rect(fill = NA),
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), axis.title.x = element_blank(),
            legend.position = 'none', legend.direction = 'horizontal', legend.title = element_blank(),
            legend.key = element_rect(fill = NA), legend.background = element_rect(fill = NA),
            legend.key.height = unit(0.1, 'in'),
            plot.title = element_text(hjust = 0.5)) +
      annotate("text", x = as.Date('2010-4-01'), y = 4E7, label = paste('NSE = ',NSE_LS)))
  # pdf(paste0(out_dir,'/Streamflow_LS_m3day.pdf'),width = 8, height = 5)
  # LS_streamflow_plot
  # graphics.off()
  
  #Shackleford Streamflow Residuals
  (LS_residuals_plot = ggplot(data = LS_residuals_m3day, aes(x = seq(1,length(LS_residuals_m3day$Residual)),y=LS_residuals_m3day$Residual)) + 
      geom_point(shape =21,color = 'black', fill = 'red',stroke = 0.5, size = 2) +
      geom_abline(intercept=0, slope=0, col = 'black') +
      xlab('Observation') +
      ylab(bquote('log[Residual ('*m^3*d^{-1}*')]')) +
      ggtitle('Streamflow Residuals - Lower Shackleford') +
      scale_y_continuous(limits = c(-8,8), breaks = seq(-8,8, by = 2), expand = c(0,0)) +
      scale_x_continuous(limits = c(1,length(LS_residuals_m3day$Residual)), breaks = seq(0,length(LS_residuals_m3day$Residual),by = 100),expand = c(0.02,0.02)) +
      theme(panel.background = element_blank(), panel.border = element_rect(fill = NA),
            plot.title = element_text(hjust = 0.5)))
  # pdf(paste0(out_dir,'/Residuals_LS_m3day.pdf'),width = 8, height = 5)
  # LS_residuals_plot
  # graphics.off()
  
  
  ##########################################################################################################
  ########################                       HEAD PLOTS                   ##############################
  ##########################################################################################################
  (one2one_plot = ggplot(Head_Data,aes(x = Observed, y=Simulated)) + 
     geom_point(size = 1, color = 'navy') +
     geom_abline(intercept=0,slope=1, col = 'black') +
     scale_y_continuous(limits = c(800,940), breaks = seq(800,940,by=20), expand = c(0,0)) +
     scale_x_continuous(limits = c(800,940), breaks = seq(800,940,by=20), expand = c(0,0)) +
     ylab('Simulated (m)') +
     xlab('Observed (m)') +
     ggtitle('Groundwater Heads') +
     theme(panel.background = element_blank(), panel.border = element_rect(fill = NA),
           plot.title = element_text(hjust = 0.5)) +
     coord_fixed() +
     annotate('text', x = 850, y = 920, label = regress_eqn, size = 3.2) +
     annotate('text', x = 850, y = 905, label = R2, parse = T, size = 3.2))
  # pdf(paste0(out_dir,'/Heads_Sim_vs_Observed_m.pdf'),width = 8, height = 5)
  # one2one_plot
  # graphics.off()
  
  (head_residuals_plot = ggplot(Head_Data,aes(x = seq(1,length(Head_Data$Residual)),y=Residual)) + 
      geom_point(size = 1, color = 'navy') +
      geom_abline(intercept=0, slope=0, col = 'black') +
      scale_y_continuous(limits = c(-16,16), breaks = seq(-16,16,4), expand = c(0,0)) +
      scale_x_continuous(limits = c(1,round(length(Head_Data$Residual),-2)), breaks = seq(0,round(length(Head_Data$Residual),-2),by = 500), expand = c(0.02,0.02)) +
      ylab('Residual (m)') +
      xlab('Observation Number') +
      ggtitle('Head Residuals') +
      theme(panel.background = element_blank(), panel.border = element_rect(fill = NA),
            plot.title = element_text(hjust = 0.5)))
  # pdf(paste0(out_dir,'/Head_Residuals_m.pdf'),width = 8, height = 5)
  # head_residuals_plot
  # graphics.off()
}  
  ##########################################################################################################
  ########################                      COMBO PLOTS                   ##############################
  ##########################################################################################################
  if (units_cfs == TRUE){
    pdf(paste0(out_dir,'/Sim_v_Obs_Summary_cfs.pdf'),width = 8.5, height = 11)
  } else {
    pdf(paste0(out_dir,'/Sim_v_Obs_Summary_m3day.pdf'),width = 8.5, height = 11)
  }
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(5,2)))
  vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
  print(FJ_streamflow_plot, vp = vplayout(1,1))
  print(AS_streamflow_plot, vp = vplayout(2,1))
  print(BY_streamflow_plot, vp = vplayout(3,1))
  print(LS_streamflow_plot, vp = vplayout(4,1))
  print(one2one_plot, vp = vplayout(5,1))
  print(FJ_residuals_plot, vp = vplayout(1,2))
  print(AS_residuals_plot, vp = vplayout(2,2))
  print(BY_residuals_plot, vp = vplayout(3,2))
  print(LS_residuals_plot, vp = vplayout(4,2))
  print(head_residuals_plot, vp = vplayout(5,2))
  graphics.off()
  
  if (units_cfs == TRUE){
    pdf(paste0(out_dir,'/Sim_v_Obs_Summary_Compact_cfs.pdf'),width = 7.5, height = 7.5)
  } else {
    pdf(paste0(out_dir,'/Sim_v_Obs_Summary_Compact_m3day.pdf'),width = 7.5, height = 7.5)
  }
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(3,2)))
  vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
  print(FJ_streamflow_plot, vp = vplayout(1,1))
  print(LS_streamflow_plot, vp = vplayout(2,1))
  print(one2one_plot, vp = vplayout(3,1))
  print(AS_streamflow_plot, vp = vplayout(1,2))
  print(BY_streamflow_plot, vp = vplayout(2,2))
  print(head_residuals_plot, vp = vplayout(3,2))
  graphics.off()
