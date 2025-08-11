#Script for plotting flow differences using random parameter sets

rm(list=ls())
library(ggplot2)
library(ggthemes)
library(reshape2)
library(grid)
library(magrittr)
library(ssh)
library(dplyr)
library(TTR)

# User Input --------------------------------------------------------------
UniRandPar = TRUE
CovRandPar = TRUE
UCODE_Uncert = TRUE
MAR_Comp = TRUE
ILR_Comp = TRUE
MAR_ILR_Comp = TRUE
Transfer_Files_From_Cluster = FALSE
Check_SSWRs = TRUE
out_dir = paste0(getwd(),'/Results/')
fig_format = 'png'   # figure format (png, pdf, jpg)
fig_width =  7.48    # figure width in fig_units
fig_height = 5    # figure height in fig_units
fig_units = 'in'     # units for figure dimensions 
axis_title_size = 7
axis_text_size = 6
line_size = 0.5
point_size = 1.25
wy_types = data.frame(WY_Type = c('Dry', 'Dry', 'Wet', 'Dry', 'Wet', 'Dry', 'Wet'),
                     Rect_Date_Start = as.Date(c('1990-10-01','1993-10-01','1994-10-01','2000-10-01','2005-10-01','2006-10-01','2010-10-01')),
                     Rect_Date_End = as.Date(c('1992-10-01','1994-10-01','1999-10-01','2002-10-01','2006-10-01','2009-10-01','2011-10-01')),
                     Fill = c('#ff8282', '#ff8282', 'skyblue', '#ff8282','skyblue', '#ff8282','skyblue'),
                     Label_Date = c('1991-10-01','1994-04-01','1997-10-01','2001-10-01','2006-04-01','2008-10-01','2011-04-01'))
n_preds = 1260 # 5 Predictions for n_stress Stress Periods
n_stress = 252 # 252 stress periods
Ex_Thresh = 20 #Value above which predicted streamflow differences normalized to the mean are excluded (e.g., exclude simulations where predictions are 20x higher than the ensemble mean)

crit_months = c('Aug','Sep','Oct')   #Critically dry months, used for generating plots
StartingDates = seq(as.Date('1990-10-01'), by = "month", length.out = 252)
Water_Yr_Class_1991_2011 = data.frame(Water_Year = seq(1991,2011),
                                      `Water Year Type` = c('Critical','Critical','Above Normal','Critical','Wet','Wet','Wet','Wet','Wet',
                                                            'Above Normal','Dry','Dry','Above Normal','Below Normal','Above Normal','Wet',
                                                            'Dry','Critical','Dry','Below Normal','Wet'),
                                      Plot_Color = c('darkred','darkred','skyblue','darkred','navyblue','navyblue','navyblue','navyblue','navyblue',
                                                     'skyblue','red','red','skyblue','orange','skyblue','navyblue',
                                                     'red','darkred','red','orange','navyblue'))

SWE = data.frame(Water_Year = seq(1991,2011),
                 Apr_1_SWE_in = c('16.3','19.4','33.3','18.0','41.8','26.2','13.8','45.5','45.9','35.8','10.6','30.9','27.9','33.0',
                                  '22.3','41.4','16.0','34.2','24.7','22.6','38.9'))
#Specify directories where extracted streamflow predicitons are located (Monte Carlo Analyses - Methods 1 and 2)
UniRandPar_Basecase_Dir = paste0(getwd(),'/UniRandPar_out/Basecase_Preds/')    # Uniform - Basecase
UniRandPar_MAR_Dir = paste0(getwd(),'/UniRandPar_out/MAR_Preds/')              # Uniform - MAR
UniRandPar_ILR_Dir = paste0(getwd(),'/UniRandPar_out/ILR_Preds/')              # Uniform - ILR
UniRandPar_MAR_ILR_Dir = paste0(getwd(),'/UniRandPar_out/MAR_ILR_Preds/')      # Uniform - MAR_ILR
CovRandPar_Basecase_Dir = paste0(getwd(),'/CovRandPar_out/Basecase_Preds/')    # Normal - Basecase
CovRandPar_MAR_Dir = paste0(getwd(),'/CovRandPar_out/MAR_Preds/')              # Normal- MAR
CovRandPar_ILR_Dir = paste0(getwd(),'/CovRandPar_out/ILR_Preds/')              # Normal- ILR
CovRandPar_MAR_ILR_Dir = paste0(getwd(),'/CovRandPar_out/MAR_ILR_Preds/')      # Normal- MAR_ILR

#Specify directory where UCODE streamflow predicitons are located (Method 3)
UCODE_MAR_dir = paste0(getwd(),'/UCODE_out/MAR/')
UCODE_ILR_dir = paste0(getwd(),'/UCODE_out/ILR/')
UCODE_MAR_ILR_dir = paste0(getwd(),'/UCODE_out/MAR_ILR/')

#Plottting themes
conv_theme = theme(axis.text = element_text(size=axis_text_size),
                   axis.title = element_text(size=axis_title_size),
                   legend.title = element_blank(),
                   legend.position = c(0.5,0.9),
                   legend.direction = 'horizontal',
                   legend.spacing = unit(-0.2, 'cm'),
                   legend.text.align = 0.5,
                   plot.title = element_text(size = axis_title_size+1, hjust = 0.5))


# Transfer Files From Cluster ---------------------------------------------
if (Transfer_Files_From_Cluster==TRUE){
  session <- ssh_connect("dtolley@aqua.lawr.ucdavis.edu")
  scp_download(session,files = '/zeolite/dtolley/pred_uncert/UniRandPar/Flow_Precitions/basecase/*',to = './UniRandPar_out/Basecase_Preds/')
  scp_download(session,files = '/zeolite/dtolley/pred_uncert/UniRandPar/Flow_Precitions/MAR/*',to = './UniRandPar_out/MAR_Preds/')
  scp_download(session,files = '/zeolite/dtolley/pred_uncert/UniRandPar/Flow_Precitions/ILR/*',to = './UniRandPar_out/ILR_Preds/')
  scp_download(session,files = '/zeolite/dtolley/pred_uncert/UniRandPar/Flow_Precitions/MAR_ILR/*',to = './UniRandPar_out/MAR_ILR_Preds/')

  scp_download(session,files = '/zeolite/dtolley/pred_uncert/covrandpar/Flow_Precitions/basecase/*',to = './CovRandPar_out/Basecase_Preds/')
  scp_download(session,files = '/zeolite/dtolley/pred_uncert/covrandpar/Flow_Precitions/MAR/*',to = './CovRandPar_out/MAR_Preds/')
  scp_download(session,files = '/zeolite/dtolley/pred_uncert/covrandpar/Flow_Precitions/ILR/*',to = './CovRandPar_out/ILR_Preds/')
  scp_download(session,files = '/zeolite/dtolley/pred_uncert/covrandpar/Flow_Precitions/MAR_ILR/*',to = './CovRandPar_out/MAR_ILR_Preds/')

  scp_download(session,files = '/zeolite/dtolley/pred_uncert/UniRandPar/UCODE_uout/*',to = './UniRandPar_out/Basecase_UOUT/')
  scp_download(session,files = '/zeolite/dtolley/pred_uncert/covrandpar/UCODE_uout/*',to = './CovRandPar_out/Basecase_UOUT/')
  
  scp_download(session,files = '/zeolite/dtolley/pred_uncert/UCODE_Pred_Uncert/Results/MAR/*', to = './UCODE_out/MAR/')
  scp_download(session,files = '/zeolite/dtolley/pred_uncert/UCODE_Pred_Uncert/Results/ILR/*', to = './UCODE_out/ILR/')
  scp_download(session,files = '/zeolite/dtolley/pred_uncert/UCODE_Pred_Uncert/Results/MAR_ILR/*' ,to = './UCODE_out/MAR_ILR/')

  for (i in 1:500){
    scp_download(session,files = paste0('/aqua/dtolley/unirandpar/basecase/ParameterSet',i,'/monthly_water_budget.dat'), to = './UniRandPar_out/Basecase_SWBM_Budgets/', verbose = F)
    file.rename(from = './UniRandPar_out/Basecase_SWBM_Budgets/monthly_water_budget.dat', to = paste0('./UniRandPar_out/Basecase_SWBM_Budgets/monthly_water_budget_',i,'.dat'))
    }
  
  ssh_disconnect(session)
}
# Check SSWRs for Monte Carlo Methods -------------------------------------
if (Check_SSWRs == TRUE){
  uni_uout_files = list.files(path = './UniRandPar_out/Basecase_UOUT')
  cov_uout_files = list.files(path = './CovRandPar_out/Basecase_UOUT')
  
  UniRandPar_SSWRs = unlist(lapply(paste0('./UniRandPar_out/Basecase_UOUT/',uni_uout_files), FUN = function(x) read.table(x, header = T, stringsAsFactors = F, skip = 4960, nrows = 1)[7]))
  CovRandPar_SSWRs = unlist(lapply(paste0('./CovRandPar_out/Basecase_UOUT/',cov_uout_files), FUN = function(x) read.table(x, header = T, stringsAsFactors = F, skip = 4960, nrows = 1)[7]))
  
  UniRandPar_SSWRs = data.frame(Param_Set_Num = as.numeric(gsub(x = uni_uout_files, pattern = 'SVIHM_UniRandPar_|.#uout', replacement = '')),
                                SSWR = unlist(lapply(paste0('./UniRandPar_out/Basecase_UOUT/',uni_uout_files), FUN = function(x) read.table(x, header = T, stringsAsFactors = F, skip = 4960, nrows = 1)[7])))
  CovRandPar_SSWRs = data.frame(Param_Set_Num = rep(UniRandPar_SSWRs$Param_Set_Num[which(UniRandPar_SSWRs$Param_Set_Num<=100)],5),
                                SSWR = unlist(lapply(paste0('./CovRandPar_out/Basecase_UOUT/',cov_uout_files), FUN = function(x) read.table(x, header = T, stringsAsFactors = F, skip = 4960, nrows = 1)[7])))
  CovRandPar_SSWRs$Calibration = rep(paste0('Cal ',seq(1,5)),each = 100)
  
  length(UniRandPar_SSWRs$SSWR[which(UniRandPar_SSWRs$SSWR>9.66E4*1.1)])
  length(CovRandPar_SSWRs$SSWR[which(CovRandPar_SSWRs$SSWR[1:100]>9.66E4*1.1)])
  length(CovRandPar_SSWRs$SSWR[which(CovRandPar_SSWRs$SSWR[101:200]>9.66E4*1.1)])
  length(CovRandPar_SSWRs$SSWR[which(CovRandPar_SSWRs$SSWR[201:300]>9.66E4*1.1)])
  length(CovRandPar_SSWRs$SSWR[which(CovRandPar_SSWRs$SSWR[301:400]>9.66E4*1.1)])
  length(CovRandPar_SSWRs$SSWR[which(CovRandPar_SSWRs$SSWR[401:500]>9.66E4*1.1)])
  length(CovRandPar_SSWRs$SSWR[which(CovRandPar_SSWRs$SSWR>9.66E4*1.1)])
  
  (UniRandPar_SSWR_plot = ggplot(UniRandPar_SSWRs, aes(x = Param_Set_Num, y = SSWR)) + 
      geom_point(size = 2, shape = 21, color = 'black', fill = 'white') +
      ggtitle('Method 1\nUncorrelated Random Parameter Sets') +
      scale_y_continuous(limits = c(8E4,1.8E5), breaks = seq(8E4,1.8E5,by = 2E4), labels = (c('8.0E4', '1.0E5', paste0(seq(1.2,1.8, by=0.2),'E5'))), expand = c(0,0)) +
      geom_hline(yintercept = 8.9E4, color = '#66a61e', size = 0.5) + 
      geom_hline(yintercept = 8.9E4*1.1, color = '#66a61e', linetype="dashed", size = 0.5) +
      geom_hline(yintercept = 9.66E4, color = '#e6ab02', size = 0.5) + 
      geom_hline(yintercept = 9.66E4*1.1, color = '#e6ab02', linetype="dashed", size = 0.5) +
      xlab('Realization') +
      ylab('SSWR (-)') +
      theme_few() + 
      theme(plot.title = element_text(hjust = 0.5))
  )
  
  (CovRandPar_SSWR_plot = ggplot(CovRandPar_SSWRs, aes(x = Param_Set_Num, y = SSWR, fill = Calibration)) + 
      geom_point(size = 2, shape = 21, color = 'black') +
      ggtitle('Method 2\nCorrelated Random Parameter Sets') +
      scale_fill_manual(values = c('#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02')) +
      scale_y_continuous(limits = c(8E4,1.8E5), breaks = seq(8E4,1.8E5,by = 2E4), expand = c(0,0)) +
      geom_hline(yintercept = 8.9E4, color = '#66a61e', size = 0.5) + 
      geom_hline(yintercept = 8.9E4*1.1, color = '#66a61e', linetype="dashed", size = 0.5) +
      geom_hline(yintercept = 9.66E4, color = '#e6ab02', size = 0.5) + 
      geom_hline(yintercept = 9.66E4*1.1, color = '#e6ab02', linetype="dashed", size = 0.5) +
      xlab('Realization') +
      theme_few() + 
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = c(0.5,0.95),
            legend.direction = 'horizontal',
            legend.box.background = element_rect(color = 'black'),
            legend.margin = margin(t=0.1,b=0.2,l=0.2,r=1.5),
            legend.title = element_blank(),
            legend.background = element_rect(fill= NA),
            legend.key = element_rect(fill = NA))
  )
  fig_name = 'SSWRs_Dist'
  if (fig_format == 'jpg'){
    jpeg(paste0(out_dir,fig_name,'.jpg'), width = fig_width, height = fig_height, units = fig_units,  res = 600)
  } else if (fig_format == 'png'){
    png(paste0(out_dir,fig_name,'.png'), width = fig_width, height = fig_height, units = fig_units,  res = 600)
  } else if (fig_format == 'pdf'){
    pdf(paste0(out_dir,fig_name,'.pdf'), width = 5, height = 4)
  }
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(1,2)))
  vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
  print(UniRandPar_SSWR_plot +
          theme(plot.title = element_text(size = 8),
                panel.background = element_rect(color = 'black', fill = 'gray90'),
                plot.background = element_rect(color = NA, fill = NA),
                axis.text = element_text(size = 6),
                axis.title = element_text(size = 7),
                plot.margin = margin(t =5, b= 5, l=5, r = -10)),
        vp = vplayout(1,1))
  print(CovRandPar_SSWR_plot +
          theme(plot.title = element_text(size = 8),
                legend.text = element_text(size = 7),
                legend.spacing.x = unit(-1, 'mm'),
                panel.background = element_rect(color = 'black', fill = 'gray90'),
                plot.background = element_rect(color = NA, fill = NA),
                axis.text.y = element_blank(),
                axis.text.x = element_text(size = 6),
                axis.title.y = element_blank(),
                axis.title.x = element_text(size = 7),
                plot.margin = margin(t =5, b= 5, l=20, r = 5)),
        vp = vplayout(1,2))
  graphics.off()
}

# Functions -------------------------------------
#Read Streamflow Predictions (Monte Carlo)
Flow_Preds = function(pred_dir, scenario, randpartype){  #pred_dir is a directory, scenario and randpartype are strings for labeling output
if (randpartype=='uni'){
  filenames = list.files(path = pred_dir,pattern = '.dat')
  param_set_nums = as.numeric(gsub(x = filenames, pattern = 'Flow_Predictions_', replacement = '')%>%gsub(pattern = '.dat', replacement = ''))
  date_info = read.table(paste0(pred_dir,filenames[1]), header = T, stringsAsFactors = F)[,1:3]
  preds = lapply(paste0(pred_dir,filenames), FUN = function(x) read.table(x, header = T, stringsAsFactors = F))
  all_preds = rbind(as.data.frame(sapply(preds,'[[',4)),
                    as.data.frame(sapply(preds,'[[',5)),
                    as.data.frame(sapply(preds,'[[',6)),
                    as.data.frame(sapply(preds,'[[',7)),
                    as.data.frame(sapply(preds,'[[',8)))
  names(all_preds) = param_set_nums
  Pred_Locs = rep(seq(1,5),each = n_stress)
  Date = paste0('01-',date_info$StressPeriod)
  UniRandPar_Preds = data.frame (Pred_Loc = Pred_Locs)
  UniRandPar_Preds = cbind(UniRandPar_Preds,Date,date_info,all_preds)
  UniRandPar_Preds$Date = as.Date(UniRandPar_Preds$Date, '%d-%b-%Y')
  col_order = c(seq(1,5), order(as.numeric(names(UniRandPar_Preds[,seq(-1,-5)])))+5)
  UniRandPar_Preds = UniRandPar_Preds[,col_order]
  return(UniRandPar_Preds) 
  
  } else if (randpartype=='cov'){
    filenames_cal_1 = list.files(path = pred_dir,pattern = 'Cal_1')
    filenames_cal_2 = list.files(path = pred_dir,pattern = 'Cal_2')
    filenames_cal_3 = list.files(path = pred_dir,pattern = 'Cal_3')
    filenames_cal_4 = list.files(path = pred_dir,pattern = 'Cal_4')
    filenames_cal_5 = list.files(path = pred_dir,pattern = 'Cal_5')
    param_set_nums = as.numeric(gsub(x = filenames_cal_1, pattern = 'Flow_Predictions_Cal_1_', replacement = '')%>%gsub(pattern = '.dat', replacement = ''))
    date_info = read.table(paste0(pred_dir,filenames_cal_1[1]), header = T, stringsAsFactors = F)[,1:3]
    preds_cal_1 = lapply(paste0(pred_dir,filenames_cal_1), FUN = function(x) read.table(x, header = T, stringsAsFactors = F))
    preds_cal_2 = lapply(paste0(pred_dir,filenames_cal_2), FUN = function(x) read.table(x, header = T, stringsAsFactors = F))
    preds_cal_3 = lapply(paste0(pred_dir,filenames_cal_3), FUN = function(x) read.table(x, header = T, stringsAsFactors = F))
    preds_cal_4 = lapply(paste0(pred_dir,filenames_cal_4), FUN = function(x) read.table(x, header = T, stringsAsFactors = F))
    preds_cal_5 = lapply(paste0(pred_dir,filenames_cal_5), FUN = function(x) read.table(x, header = T, stringsAsFactors = F))
    all_preds = rbind(as.data.frame(sapply(preds_cal_1,'[[',4)),
                      as.data.frame(sapply(preds_cal_1,'[[',5)),
                      as.data.frame(sapply(preds_cal_1,'[[',6)),
                      as.data.frame(sapply(preds_cal_1,'[[',7)),
                      as.data.frame(sapply(preds_cal_1,'[[',8)),
                      as.data.frame(sapply(preds_cal_2,'[[',4)),
                      as.data.frame(sapply(preds_cal_2,'[[',5)),
                      as.data.frame(sapply(preds_cal_2,'[[',6)),
                      as.data.frame(sapply(preds_cal_2,'[[',7)),
                      as.data.frame(sapply(preds_cal_2,'[[',8)),
                      as.data.frame(sapply(preds_cal_3,'[[',4)),
                      as.data.frame(sapply(preds_cal_3,'[[',5)),
                      as.data.frame(sapply(preds_cal_3,'[[',6)),
                      as.data.frame(sapply(preds_cal_3,'[[',7)),
                      as.data.frame(sapply(preds_cal_3,'[[',8)),
                      as.data.frame(sapply(preds_cal_4,'[[',4)),
                      as.data.frame(sapply(preds_cal_4,'[[',5)),
                      as.data.frame(sapply(preds_cal_4,'[[',6)),
                      as.data.frame(sapply(preds_cal_4,'[[',7)),
                      as.data.frame(sapply(preds_cal_4,'[[',8)),
                      as.data.frame(sapply(preds_cal_5,'[[',4)),
                      as.data.frame(sapply(preds_cal_5,'[[',5)),
                      as.data.frame(sapply(preds_cal_5,'[[',6)),
                      as.data.frame(sapply(preds_cal_5,'[[',7)),
                      as.data.frame(sapply(preds_cal_5,'[[',8)))
    names(all_preds) = param_set_nums
    Pred_Locs = rep(rep(seq(1,5),each = n_stress),5)
    Calibration = rep(rep(paste0('Cal_',seq(1,5)),each = n_stress),each = 5)
    Date = paste0('01-',date_info$StressPeriod)
    CovRandPar_Preds = data.frame (Pred_Loc = Pred_Locs)
    CovRandPar_Preds = cbind(CovRandPar_Preds,Date,date_info,Calibration,all_preds)
    CovRandPar_Preds$Date = as.Date(CovRandPar_Preds$Date, '%d-%b-%Y')
    col_order = c(seq(1,6), order(as.numeric(names(CovRandPar_Preds[,seq(-1,-6)])))+6)
    CovRandPar_Preds = CovRandPar_Preds[,col_order]
    return(CovRandPar_Preds) 
  }
}   
                    
#Read UCODE Linear Uncertainty Output (*.#linunc)
UCODE_linunc = function(pred_dir, cal_num){  #pred_dir is a directory, scenario is a string for labeling output
  linunc_files = list.files(path = pred_dir, pattern = '_linp')    
  linunc_text = readLines(paste0(pred_dir,linunc_files[cal_num]))                #read text file
  conf_int_idx = grep(linunc_text,pattern = 'CONFIDENCE INTERVALS')    #extract line numbers where confidence intervals are located
  ind_conf_int = read.table(paste0(pred_dir,linunc_files[cal_num]), skip = conf_int_idx[1], nrows = n_preds*3, header = T, sep = '', stringsAsFactors = F) #3 times the number of preds (basecase flow, scenario flow, and flow difference)
  simul_conf_int_1 = read.table(paste0(pred_dir,linunc_files[cal_num]), skip = conf_int_idx[2], nrows = n_preds*3, header = T, sep = '', stringsAsFactors = F)  #bonferroni or sheffe d=k
  simul_conf_int_2 = read.table(paste0(pred_dir,linunc_files[cal_num]), skip = conf_int_idx[3], nrows = n_preds*3, header = T, sep = '', stringsAsFactors = F)  #Sheffe d=NP
  all_conf_ints = rbind(ind_conf_int,simul_conf_int_1,simul_conf_int_2)
  all_conf_ints$INT.TYPE = rep(c('Individual', 'Simultaneous_1', 'Simultaneous_2'), each = n_preds*3)
  all_conf_ints$CALIBRATION = paste0('Cal_',cal_num)
  return(all_conf_ints[,seq(-3,-4)])
}
# Basecase ----------------------------------------------------------------
if (UniRandPar==TRUE){
  flow_pred_dir = UniRandPar_Basecase_Dir
  UniRandPar_Preds_Basecase = Flow_Preds(pred_dir = flow_pred_dir, scenario = 'basecase', randpartype = 'uni')
}
if (CovRandPar==TRUE){
  flow_pred_dir = CovRandPar_Basecase_Dir
  CovRandPar_Preds_Basecase = Flow_Preds(pred_dir = flow_pred_dir, scenario = 'basecase', randpartype = 'cov')
}

# MAR ---------------------------------------------------------------------
if (MAR_Comp==TRUE){
  if (UniRandPar==TRUE){
    flow_pred_dir = UniRandPar_MAR_Dir
    UniRandPar_Preds_MAR = Flow_Preds(pred_dir = flow_pred_dir, scenario = 'MAR', randpartype = 'uni')
    param_match = intersect(names(UniRandPar_Preds_MAR),names(UniRandPar_Preds_Basecase))                #Exclude any missing parameter sets from either scenario
    UniRandPar_Diff_MAR = UniRandPar_Preds_MAR[,seq(1,5)]
    UniRandPar_Diff_MAR = cbind(UniRandPar_Diff_MAR,UniRandPar_Preds_MAR[,param_match[seq(-1,-5)]] - UniRandPar_Preds_Basecase[,param_match[seq(-1,-5)]])
    UniRandPar_Diff_MAR = subset(UniRandPar_Diff_MAR, StressPeriod!='Oct-1990')
    UniRandPar_Diff_MAR_norm = cbind(UniRandPar_Diff_MAR[,seq(1,5)], t(apply(UniRandPar_Diff_MAR[,seq(-1,-5)], 1, FUN = function(x) x / mean(x))))  #Normalized difference
    
    UniRandPar_Diff_MAR_crit_months = subset(UniRandPar_Diff_MAR_norm,Month%in%crit_months & Pred_Loc==1) %>% melt(id.vars = names(UniRandPar_Diff_MAR_norm[seq(1,5)]))
    UniRandPar_Diff_MAR_crit_months$Month = factor(UniRandPar_Diff_MAR_crit_months$Month, levels = c('Aug', 'Sep', 'Oct'))
    UniRandPar_Diff_MAR_crit_months = UniRandPar_Diff_MAR_crit_months[order(UniRandPar_Diff_MAR_crit_months$Month),]
    MAR_UniRandPar_Outliers_Plot = ggplot(UniRandPar_Diff_MAR_crit_months, aes(x = as.numeric(variable), y = value, color = Month)) + 
      geom_hline(yintercept = 0) +
      geom_hline(yintercept = 20, linetype = 'dashed') +
      geom_hline(yintercept = -20, linetype = 'dashed') +
      geom_point() + 
      scale_color_brewer(type = 'div', palette = 'Set1') +
      ylab('Normalized Streamflow Difference (-)') +
      xlab('n') +
      scale_y_continuous(limits = c(-50,150), breaks = seq(-50,150,by = 50), expand = c(0,0)) +
      scale_x_continuous(limits = c(1,500), expand = c(0.02,0.02)) +
      theme_few()+
      conv_theme +
      theme()
    
    Excluded_Runs_UniRandPar_MAR = apply(subset(UniRandPar_Diff_MAR_norm, Month%in%crit_months & Pred_Loc==1, select = seq(-1,-5)),2,FUN = function(x) max(abs(x),na.rm = T))    #subset dataframe for Location 1 from Aug-Oct
    Excluded_Runs_UniRandPar_MAR = as.numeric(which(Excluded_Runs_UniRandPar_MAR>Ex_Thresh))   #index simulations where predicted difference is more than 20 times the mean
    Excluded_Runs_UniRandPar_MAR_num = length(Excluded_Runs_UniRandPar_MAR)             #calculate number of simulations where predicted difference is more than 20 times the mean
    UniRandPar_Diff_MAR_clean = UniRandPar_Diff_MAR                                     #make a copy of the data frame
    UniRandPar_Diff_MAR_clean[,Excluded_Runs_UniRandPar_MAR+5] = NaN                    #remove outliers (+5 is to account for info columns (e.g., Month))
    
    UniRandPar_Diff_MAR_convergence = cbind(UniRandPar_Diff_MAR_clean[,seq(1,5)], t(apply(UniRandPar_Diff_MAR_clean[,c(seq(-1,-5), -(Excluded_Runs_UniRandPar_MAR+5))], 1, FUN = function(x) cummean(x))))  #Cumulative Mean Value
    UniRandPar_Diff_MAR_convergence_norm = UniRandPar_Diff_MAR_convergence
    UniRandPar_Diff_MAR_convergence_norm[,seq(-1,-5)] = t(apply(UniRandPar_Diff_MAR_convergence[,seq(-1,-5)], 1, FUN = function(x) x / tail(x,n=1))) #Normalized Cumulative Mean Value 
    }
  if (CovRandPar==TRUE){
    flow_pred_dir = CovRandPar_MAR_Dir
    CovRandPar_Preds_MAR = Flow_Preds(pred_dir = flow_pred_dir, scenario = 'MAR', randpartype = 'cov')
    param_match = intersect(names(CovRandPar_Preds_MAR),names(CovRandPar_Preds_Basecase))                #Exclude any missing parameter sets from either scenario
    CovRandPar_Diff_MAR = CovRandPar_Preds_MAR[,seq(1,6)]
    CovRandPar_Diff_MAR = cbind(CovRandPar_Diff_MAR,CovRandPar_Preds_MAR[,param_match[seq(-1,-6)]] - CovRandPar_Preds_Basecase[,param_match[seq(-1,-6)]])
    CovRandPar_Diff_MAR = subset(CovRandPar_Diff_MAR, StressPeriod!='Oct-1990')
    CovRandPar_Diff_MAR_norm = cbind(CovRandPar_Diff_MAR[,seq(1,6)], t(apply(CovRandPar_Diff_MAR[,seq(-1,-6)], 1, FUN = function(x) x / mean(x))))  #Normalized difference
    
    CovRandPar_Diff_MAR_crit_months = subset(CovRandPar_Diff_MAR_norm,Month%in%crit_months & Pred_Loc==1) %>% melt(id.vars = names(CovRandPar_Diff_MAR_norm[seq(1,6)]))
    CovRandPar_Diff_MAR_crit_months$Month = factor(CovRandPar_Diff_MAR_crit_months$Month, levels = c('Aug', 'Sep', 'Oct'))
    CovRandPar_Diff_MAR_crit_months = CovRandPar_Diff_MAR_crit_months[order(CovRandPar_Diff_MAR_crit_months$Month),]
    MAR_CovRandPar_Outliers_Plot = ggplot(CovRandPar_Diff_MAR_crit_months, aes(x = as.numeric(variable), y = value, color = Month, shape = Calibration)) + 
      geom_hline(yintercept = 0) +
      geom_hline(yintercept = 20, linetype = 'dashed') +
      geom_hline(yintercept = -20, linetype = 'dashed') +
      geom_point() + 
      scale_color_brewer(type = 'div', palette = 'Set1') +
      ylab('Normalized Streamflow Difference (-)') +
      xlab('n') +
      scale_y_continuous(limits = c(-50,150), breaks = seq(-50,150,by = 50), expand = c(0,0)) +
      scale_x_continuous(limits = c(1,100), expand = c(0.02,0.02)) +
      theme_few()+
      conv_theme +
      theme()
    
    #Find runs that exceed threshold ratio and remove them
    for (i in 1:5){ #loop over calibrations
      #subset dataframe for Location 1 from Aug-Oct for each Calibration
      eval(parse(text = paste0("Excluded_Runs_CovRandPar_MAR_Cal_",i," = apply(subset(CovRandPar_Diff_MAR_norm, Month%in%crit_months & Pred_Loc==1 & Calibration == 'Cal_",i,"', select = seq(-1,-6)),2,FUN = function(x) max(abs(x),na.rm = T))")))
      #index simulations where predicted difference is more than 20 times the mean
      eval(parse(text = paste0("Excluded_Runs_CovRandPar_MAR_Cal_",i," = as.numeric(which(Excluded_Runs_CovRandPar_MAR_Cal_",i,">Ex_Thresh))")))
      #calculate number of simulations where predicted difference is more than 20 times the mean
      eval(parse(text = paste0("Excluded_Runs_CovRandPar_MAR_num_",i," = length(Excluded_Runs_CovRandPar_MAR_Cal_",i,")")))
      #make a copy of the data frame
      eval(parse(text = paste0("CovRandPar_Diff_MAR_clean_",i," = subset(CovRandPar_Diff_MAR, Month%in%crit_months & Calibration == 'Cal_",i,"')")))
      #remove outliers (+6 is to account for info columns (e.g., Month and Calibration))
      eval(parse(text = paste0("CovRandPar_Diff_MAR_clean_",i,"[,Excluded_Runs_CovRandPar_MAR_Cal_",i,"+6] = NaN")))
      if (i==1){
        CovRandPar_Diff_MAR_clean = CovRandPar_Diff_MAR_clean_1
        Excluded_Runs_CovRandPar_MAR = Excluded_Runs_CovRandPar_MAR_num_1
      }  else {
        eval(parse(text = paste0("CovRandPar_Diff_MAR_clean = rbind(CovRandPar_Diff_MAR_clean, CovRandPar_Diff_MAR_clean_",i,")")))
        eval(parse(text = paste0("Excluded_Runs_CovRandPar_MAR = c(Excluded_Runs_CovRandPar_MAR, Excluded_Runs_CovRandPar_MAR_num_",i,")")))
      }
      eval(parse(text = paste0("CovRandPar_Diff_MAR_convergence_",i," = cbind(CovRandPar_Diff_MAR_clean_",i,"[,seq(1,6)], t(apply(CovRandPar_Diff_MAR_clean_",i,"[,c(seq(-1,-6), -(Excluded_Runs_CovRandPar_MAR_Cal_",i,"+6))], 1, FUN = function(x) cummean(x))))")))  #Cumulative Mean Value
      eval(parse(text = paste0("CovRandPar_Diff_MAR_convergence_norm_",i," = CovRandPar_Diff_MAR_convergence_",i)))
      eval(parse(text = paste0("CovRandPar_Diff_MAR_convergence_norm_",i,"[,seq(-1,-6)] = t(apply(CovRandPar_Diff_MAR_convergence_",i,"[,seq(-1,-6)], 1, FUN = function(x) x / tail(x,n=1)))"))) #Normalized Cumulative Mean Value 
      }
    CovRandPar_Diff_MAR_convergence_norm_melt = rbind(melt(CovRandPar_Diff_MAR_convergence_norm_1, id.vars = names(CovRandPar_Diff_MAR_convergence_norm_1[seq(1,6)])),
                 melt(CovRandPar_Diff_MAR_convergence_norm_2, id.vars = names(CovRandPar_Diff_MAR_convergence_norm_2[seq(1,6)])),
                 melt(CovRandPar_Diff_MAR_convergence_norm_3, id.vars = names(CovRandPar_Diff_MAR_convergence_norm_2[seq(1,6)])),
                 melt(CovRandPar_Diff_MAR_convergence_norm_4, id.vars = names(CovRandPar_Diff_MAR_convergence_norm_2[seq(1,6)])),
                 melt(CovRandPar_Diff_MAR_convergence_norm_5, id.vars = names(CovRandPar_Diff_MAR_convergence_norm_2[seq(1,6)])))
  }
  if (UCODE_Uncert==TRUE){
    flow_pred_dir = UCODE_MAR_dir
    for (i in 1:5){
      eval(parse(text = paste0('UCODE_MAR_',i,' = UCODE_linunc(flow_pred_dir, ',i,')')))
    }
    UCODE_MAR = rbind(UCODE_MAR_1, UCODE_MAR_2, UCODE_MAR_3, UCODE_MAR_4, UCODE_MAR_5)
    UCODE_MAR_diff = UCODE_MAR[grep(x=UCODE_MAR$PREDICTION.NAME, pattern = 'Diff'),]
    UCODE_MAR_diff$DATE = rep(seq(as.Date('1990-10-01'), as.Date('2011-09-30'), by = 'month'), each=5)
    UCODE_MAR_diff$MONTH = format(UCODE_MAR_diff$DATE, '%b')
    UCODE_MAR_diff$PRED.LOC = rep(seq(5,1),252)
    UCODE_MAR_diff$WATER.YEAR = rep(seq(1991,2011),each = 12*5)  # 12 months for 5 locations
  }
}
# ILR ---------------------------------------------------------------------
if (ILR_Comp==TRUE){
  if (UniRandPar==TRUE){
    flow_pred_dir = UniRandPar_ILR_Dir
    UniRandPar_Preds_ILR = Flow_Preds(pred_dir = flow_pred_dir, scenario = 'ILR', randpartype = 'uni')
    param_match = intersect(names(UniRandPar_Preds_ILR),names(UniRandPar_Preds_Basecase))                #Exclude any missing parameter sets from either scenario
    UniRandPar_Diff_ILR = UniRandPar_Preds_ILR[,seq(1,5)]
    UniRandPar_Diff_ILR = cbind(UniRandPar_Diff_ILR,UniRandPar_Preds_ILR[,param_match[seq(-1,-5)]] - UniRandPar_Preds_Basecase[,param_match[seq(-1,-5)]])
    UniRandPar_Diff_ILR_norm = cbind(UniRandPar_Diff_ILR[,seq(1,5)], t(apply(UniRandPar_Diff_ILR[,seq(-1,-5)], 1, FUN = function(x) x / mean(x))))  #Normalized difference
    
    UniRandPar_Diff_ILR_crit_months = subset(UniRandPar_Diff_ILR_norm,Month%in%crit_months & Pred_Loc==1) %>% melt(id.vars = names(UniRandPar_Diff_ILR_norm[seq(1,5)]))
    UniRandPar_Diff_ILR_crit_months$Month = factor(UniRandPar_Diff_ILR_crit_months$Month, levels = c('Aug', 'Sep', 'Oct'))
    UniRandPar_Diff_ILR_crit_months = UniRandPar_Diff_ILR_crit_months[order(UniRandPar_Diff_ILR_crit_months$Month),]
    ILR_UniRandPar_Outliers_Plot = ggplot(UniRandPar_Diff_ILR_crit_months, aes(x = as.numeric(variable), y = value, color = Month)) + 
      geom_hline(yintercept = 0) +
      geom_hline(yintercept = 20, linetype = 'dashed') +
      geom_hline(yintercept = -20, linetype = 'dashed') +
      geom_point() + 
      scale_color_brewer(type = 'div', palette = 'Set1') +
      ylab('Normalized Streamflow Difference (-)') +
      xlab('n') +
      scale_y_continuous(limits = c(-90,120), breaks = seq(-90,120,by = 30), expand = c(0,0)) +
      scale_x_continuous(limits = c(1,500), expand = c(0.02,0.02)) +
      theme_few()+
      conv_theme +
      theme()
    
    Excluded_Runs_UniRandPar_ILR = apply(subset(UniRandPar_Diff_ILR_norm, Month%in%crit_months & Pred_Loc==1, select = seq(-1,-5)),2,FUN = function(x) max(abs(x),na.rm = T))    #subset dataframe for Location 1 from Aug-Oct
    Excluded_Runs_UniRandPar_ILR = as.numeric(which(Excluded_Runs_UniRandPar_ILR>Ex_Thresh))   #index simulations where predicted difference is more than 20 times the mean
    Excluded_Runs_UniRandPar_ILR_num = length(Excluded_Runs_UniRandPar_ILR)             #calculate number of simulations where predicted difference is more than 20 times the mean
    UniRandPar_Diff_ILR_clean = UniRandPar_Diff_ILR                                     #make a copy of the data frame
    UniRandPar_Diff_ILR_clean[,Excluded_Runs_UniRandPar_ILR+5] = NaN                    #remove outliers (+5 is to account for info columns (e.g., Month))
    
    UniRandPar_Diff_ILR_convergence = cbind(UniRandPar_Diff_ILR_clean[,seq(1,5)], t(apply(UniRandPar_Diff_ILR_clean[,c(seq(-1,-5), -(Excluded_Runs_UniRandPar_ILR+5))], 1, FUN = function(x) cummean(x))))  #Cumulative Mean Value
    UniRandPar_Diff_ILR_convergence_norm = UniRandPar_Diff_ILR_convergence
    UniRandPar_Diff_ILR_convergence_norm[,seq(-1,-5)] = t(apply(UniRandPar_Diff_ILR_convergence[,seq(-1,-5)], 1, FUN = function(x) x / tail(x,n=1))) #Normalized Cumulative Mean Value 
  }
  if (CovRandPar==TRUE){
    flow_pred_dir = CovRandPar_ILR_Dir
    CovRandPar_Preds_ILR = Flow_Preds(pred_dir = flow_pred_dir, scenario = 'ILR', randpartype = 'cov')
    param_match = intersect(names(CovRandPar_Preds_ILR),names(CovRandPar_Preds_Basecase))                #Exclude any missing parameter sets from either scenario
    CovRandPar_Diff_ILR = CovRandPar_Preds_ILR[,seq(1,6)]
    CovRandPar_Diff_ILR = cbind(CovRandPar_Diff_ILR,CovRandPar_Preds_ILR[,param_match[seq(-1,-6)]] - CovRandPar_Preds_Basecase[,param_match[seq(-1,-6)]])
    CovRandPar_Diff_ILR_norm = cbind(CovRandPar_Diff_ILR[,seq(1,6)], t(apply(CovRandPar_Diff_ILR[,seq(-1,-6)], 1, FUN = function(x) x / mean(x))))  #Normalized difference
    
    CovRandPar_Diff_ILR_crit_months = subset(CovRandPar_Diff_ILR_norm,Month%in%crit_months & Pred_Loc==1) %>% melt(id.vars = names(CovRandPar_Diff_ILR_norm[seq(1,6)]))
    CovRandPar_Diff_ILR_crit_months$Month = factor(CovRandPar_Diff_ILR_crit_months$Month, levels = c('Aug', 'Sep', 'Oct'))
    CovRandPar_Diff_ILR_crit_months = CovRandPar_Diff_ILR_crit_months[order(CovRandPar_Diff_ILR_crit_months$Month),]
    ILR_CovRandPar_Outliers_Plot = ggplot(CovRandPar_Diff_ILR_crit_months, aes(x = as.numeric(variable), y = value, color = Month, shape = Calibration)) + 
      geom_hline(yintercept = 0) +
      geom_hline(yintercept = 20, linetype = 'dashed') +
      geom_hline(yintercept = -20, linetype = 'dashed') +
      geom_point() + 
      scale_color_brewer(type = 'div', palette = 'Set1') +
      ylab('Normalized Streamflow Difference (-)') +
      xlab('n') +
      scale_y_continuous(limits = c(-90,120), breaks = seq(-90,120,by = 30), expand = c(0,0)) +
      scale_x_continuous(limits = c(1,100), expand = c(0.02,0.02)) +
      theme_few()+
      conv_theme +
      theme()
    
    #Find runs that exceed threshold ratio and remove them
    for (i in 1:5){ #loop over calibrations
      #subset dataframe for Location 1 from Aug-Oct for each Calibration
      eval(parse(text = paste0("Excluded_Runs_CovRandPar_ILR_Cal_",i," = apply(subset(CovRandPar_Diff_ILR_norm, Month%in%crit_months & Pred_Loc==1 & Calibration == 'Cal_",i,"', select = seq(-1,-6)),2,FUN = function(x) max(abs(x),na.rm = T))")))
      #index simulations where predicted difference is more than 20 times the mean
      eval(parse(text = paste0("Excluded_Runs_CovRandPar_ILR_Cal_",i," = as.numeric(which(Excluded_Runs_CovRandPar_ILR_Cal_",i,">Ex_Thresh))")))
      #calculate number of simulations where predicted difference is more than 20 times the mean
      eval(parse(text = paste0("Excluded_Runs_CovRandPar_ILR_num_",i," = length(Excluded_Runs_CovRandPar_ILR_Cal_",i,")")))
      #make a copy of the data frame
      eval(parse(text = paste0("CovRandPar_Diff_ILR_clean_",i," = subset(CovRandPar_Diff_ILR, Month%in%crit_months & Calibration == 'Cal_",i,"')")))
      #remove outliers (+6 is to account for info columns (e.g., Month and Calibration))
      eval(parse(text = paste0("CovRandPar_Diff_ILR_clean_",i,"[,Excluded_Runs_CovRandPar_ILR_Cal_",i,"+6] = NaN")))
      if (i==1){
        CovRandPar_Diff_ILR_clean = CovRandPar_Diff_ILR_clean_1
        Excluded_Runs_CovRandPar_ILR = Excluded_Runs_CovRandPar_ILR_num_1
      }  else {
        eval(parse(text = paste0("CovRandPar_Diff_ILR_clean = rbind(CovRandPar_Diff_ILR_clean, CovRandPar_Diff_ILR_clean_",i,")")))
        eval(parse(text = paste0("Excluded_Runs_CovRandPar_ILR = c(Excluded_Runs_CovRandPar_ILR, Excluded_Runs_CovRandPar_ILR_num_",i,")")))
      }
      eval(parse(text = paste0("CovRandPar_Diff_ILR_convergence_",i," = cbind(CovRandPar_Diff_ILR_clean_",i,"[,seq(1,6)], t(apply(CovRandPar_Diff_ILR_clean_",i,"[,c(seq(-1,-6), -(Excluded_Runs_CovRandPar_ILR_Cal_",i,"+6))], 1, FUN = function(x) cummean(x))))")))  #Cumulative Mean Value
      eval(parse(text = paste0("CovRandPar_Diff_ILR_convergence_norm_",i," = CovRandPar_Diff_ILR_convergence_",i)))
      eval(parse(text = paste0("CovRandPar_Diff_ILR_convergence_norm_",i,"[,seq(-1,-6)] = t(apply(CovRandPar_Diff_ILR_convergence_",i,"[,seq(-1,-6)], 1, FUN = function(x) x / tail(x,n=1)))"))) #Normalized Cumulative Mean Value 
    }
    CovRandPar_Diff_ILR_convergence_norm_melt = rbind(melt(CovRandPar_Diff_ILR_convergence_norm_1, id.vars = names(CovRandPar_Diff_ILR_convergence_norm_1[seq(1,6)])),
                                                      melt(CovRandPar_Diff_ILR_convergence_norm_2, id.vars = names(CovRandPar_Diff_ILR_convergence_norm_2[seq(1,6)])),
                                                      melt(CovRandPar_Diff_ILR_convergence_norm_3, id.vars = names(CovRandPar_Diff_ILR_convergence_norm_2[seq(1,6)])),
                                                      melt(CovRandPar_Diff_ILR_convergence_norm_4, id.vars = names(CovRandPar_Diff_ILR_convergence_norm_2[seq(1,6)])),
                                                      melt(CovRandPar_Diff_ILR_convergence_norm_5, id.vars = names(CovRandPar_Diff_ILR_convergence_norm_2[seq(1,6)])))
    }
  if (UCODE_Uncert==TRUE){
    flow_pred_dir = UCODE_ILR_dir
    for (i in 1:5){
      eval(parse(text = paste0('UCODE_ILR_',i,' = UCODE_linunc(flow_pred_dir, ',i,')')))
    }
    UCODE_ILR = rbind(UCODE_ILR_1, UCODE_ILR_2, UCODE_ILR_3, UCODE_ILR_4, UCODE_ILR_5)
    UCODE_ILR_diff = UCODE_ILR[grep(x=UCODE_ILR$PREDICTION.NAME, pattern = 'Diff'),]
    UCODE_ILR_diff$DATE = rep(seq(as.Date('1990-10-01'), as.Date('2011-09-30'), by = 'month'), each=5)
    UCODE_ILR_diff$MONTH = format(UCODE_ILR_diff$DATE, '%b')
    UCODE_ILR_diff$PRED.LOC = rep(seq(5,1),252)
    UCODE_ILR_diff$WATER.YEAR = rep(seq(1991,2011),each = 12*5)  # 12 months for 5 locations
  }
}

# MAR_ILR -----------------------------------------------------------------
if (MAR_ILR_Comp==TRUE){
  if (UniRandPar==TRUE){
    flow_pred_dir = UniRandPar_MAR_ILR_Dir
    UniRandPar_Preds_MAR_ILR = Flow_Preds(pred_dir = flow_pred_dir, scenario = 'MAR_ILR', randpartype = 'uni')
    param_match = intersect(names(UniRandPar_Preds_MAR_ILR),names(UniRandPar_Preds_Basecase))                #Exclude any missing parameter sets from either scenario
    UniRandPar_Diff_MAR_ILR = UniRandPar_Preds_MAR_ILR[,seq(1,5)]
    UniRandPar_Diff_MAR_ILR = cbind(UniRandPar_Diff_MAR_ILR,UniRandPar_Preds_MAR_ILR[,param_match[seq(-1,-5)]] - UniRandPar_Preds_Basecase[,param_match[seq(-1,-5)]])
    UniRandPar_Diff_MAR_ILR_norm = cbind(UniRandPar_Diff_MAR_ILR[,seq(1,5)], t(apply(UniRandPar_Diff_MAR_ILR[,seq(-1,-5)], 1, FUN = function(x) x / mean(x))))  #Normalized difference
    
    UniRandPar_Diff_MAR_ILR_crit_months = subset(UniRandPar_Diff_MAR_ILR_norm,Month%in%crit_months & Pred_Loc==1) %>% melt(id.vars = names(UniRandPar_Diff_MAR_ILR_norm[seq(1,5)]))
    UniRandPar_Diff_MAR_ILR_crit_months$Month = factor(UniRandPar_Diff_MAR_ILR_crit_months$Month, levels = c('Aug', 'Sep', 'Oct'))
    UniRandPar_Diff_MAR_ILR_crit_months = UniRandPar_Diff_MAR_ILR_crit_months[order(UniRandPar_Diff_MAR_ILR_crit_months$Month),]
    MAR_ILR_UniRandPar_Outliers_Plot = ggplot(UniRandPar_Diff_MAR_ILR_crit_months, aes(x = as.numeric(variable), y = value, color = Month)) + 
      geom_hline(yintercept = 0) +
      geom_hline(yintercept = 20, linetype = 'dashed') +
      geom_hline(yintercept = -20, linetype = 'dashed') +
      geom_point() + 
      scale_color_brewer(type = 'div', palette = 'Set1') +
      ylab('Normalized Streamflow Difference (-)') +
      xlab('n') +
      scale_y_continuous(limits = c(-60,60), breaks = seq(-60,60,by = 20), expand = c(0,0)) +
      scale_x_continuous(limits = c(1,500), expand = c(0.02,0.02)) +
      theme_few()+
      conv_theme +
      theme()
    
    Excluded_Runs_UniRandPar_MAR_ILR = apply(subset(UniRandPar_Diff_MAR_ILR_norm, Month%in%crit_months & Pred_Loc==1, select = seq(-1,-5)),2,FUN = function(x) max(abs(x),na.rm = T))    #subset dataframe for Location 1 from Aug-Oct
    Excluded_Runs_UniRandPar_MAR_ILR = as.numeric(which(Excluded_Runs_UniRandPar_MAR_ILR>Ex_Thresh))   #index simulations where predicted difference is more than 20 times the mean
    Excluded_Runs_UniRandPar_MAR_ILR_num = length(Excluded_Runs_UniRandPar_MAR_ILR)             #calculate number of simulations where predicted difference is more than 20 times the mean
    UniRandPar_Diff_MAR_ILR_clean = UniRandPar_Diff_MAR_ILR                                     #make a copy of the data frame
    UniRandPar_Diff_MAR_ILR_clean[,Excluded_Runs_UniRandPar_MAR_ILR+5] = NaN                    #remove outliers (+5 is to account for info columns (e.g., Month))
    
    UniRandPar_Diff_MAR_ILR_convergence = cbind(UniRandPar_Diff_MAR_ILR_clean[,seq(1,5)], t(apply(UniRandPar_Diff_MAR_ILR_clean[,c(seq(-1,-5), -(Excluded_Runs_UniRandPar_MAR_ILR+5))], 1, FUN = function(x) cummean(x))))  #Cumulative Mean Value
    UniRandPar_Diff_MAR_ILR_convergence_norm = UniRandPar_Diff_MAR_ILR_convergence
    UniRandPar_Diff_MAR_ILR_convergence_norm[,seq(-1,-5)] = t(apply(UniRandPar_Diff_MAR_ILR_convergence[,seq(-1,-5)], 1, FUN = function(x) x / tail(x,n=1))) #Normalized Cumulative Mean Value 
  }
  if (CovRandPar==TRUE){
    flow_pred_dir = CovRandPar_MAR_ILR_Dir
    CovRandPar_Preds_MAR_ILR = Flow_Preds(pred_dir = flow_pred_dir, scenario = 'MAR_ILR', randpartype = 'cov')
    param_match = intersect(names(CovRandPar_Preds_MAR_ILR),names(CovRandPar_Preds_Basecase))                #Exclude any missing parameter sets from either scenario
    CovRandPar_Diff_MAR_ILR = CovRandPar_Preds_MAR_ILR[,seq(1,6)]
    CovRandPar_Diff_MAR_ILR = cbind(CovRandPar_Diff_MAR_ILR,CovRandPar_Preds_MAR_ILR[,param_match[seq(-1,-6)]] - CovRandPar_Preds_Basecase[,param_match[seq(-1,-6)]])
    CovRandPar_Diff_MAR_ILR_norm = cbind(CovRandPar_Diff_MAR_ILR[,seq(1,6)], t(apply(CovRandPar_Diff_MAR_ILR[,seq(-1,-6)], 1, FUN = function(x) x / mean(x))))  #Normalized difference
    
    CovRandPar_Diff_MAR_ILR_crit_months = subset(CovRandPar_Diff_MAR_ILR_norm,Month%in%crit_months & Pred_Loc==1) %>% melt(id.vars = names(CovRandPar_Diff_MAR_ILR_norm[seq(1,6)]))
    CovRandPar_Diff_MAR_ILR_crit_months$Month = factor(CovRandPar_Diff_MAR_ILR_crit_months$Month, levels = c('Aug', 'Sep', 'Oct'))
    CovRandPar_Diff_MAR_ILR_crit_months = CovRandPar_Diff_MAR_ILR_crit_months[order(CovRandPar_Diff_MAR_ILR_crit_months$Month),]
    MAR_ILR_CovRandPar_Outliers_Plot = ggplot(CovRandPar_Diff_MAR_ILR_crit_months, aes(x = as.numeric(variable), y = value, color = Month, shape = Calibration)) + 
      geom_hline(yintercept = 0) +
      geom_hline(yintercept = 20, linetype = 'dashed') +
      geom_hline(yintercept = -20, linetype = 'dashed') +
      geom_point() + 
      scale_color_brewer(type = 'div', palette = 'Set1') +
      ylab('Normalized Streamflow Difference (-)') +
      xlab('n') +
      scale_y_continuous(limits = c(-60,60), breaks = seq(-60,60,by = 20), expand = c(0,0)) +
      scale_x_continuous(limits = c(1,100), expand = c(0.02,0.02)) +
      theme_few()+
      conv_theme +
      theme()
    
    #Find runs that exceed threshold ratio and remove them
    for (i in 1:5){ #loop over calibrations
      #subset dataframe for Location 1 from Aug-Oct for each Calibration
      eval(parse(text = paste0("Excluded_Runs_CovRandPar_MAR_ILR_Cal_",i," = apply(subset(CovRandPar_Diff_MAR_ILR_norm, Month%in%crit_months & Pred_Loc==1 & Calibration == 'Cal_",i,"', select = seq(-1,-6)),2,FUN = function(x) max(abs(x),na.rm = T))")))
      #index simulations where predicted difference is more than 20 times the mean
      eval(parse(text = paste0("Excluded_Runs_CovRandPar_MAR_ILR_Cal_",i," = as.numeric(which(Excluded_Runs_CovRandPar_MAR_ILR_Cal_",i,">Ex_Thresh))")))
      #calculate number of simulations where predicted difference is more than 20 times the mean
      eval(parse(text = paste0("Excluded_Runs_CovRandPar_MAR_ILR_num_",i," = length(Excluded_Runs_CovRandPar_MAR_ILR_Cal_",i,")")))
      #make a copy of the data frame
      eval(parse(text = paste0("CovRandPar_Diff_MAR_ILR_clean_",i," = subset(CovRandPar_Diff_MAR_ILR, Month%in%crit_months & Calibration == 'Cal_",i,"')")))
      #remove outliers (+6 is to account for info columns (e.g., Month and Calibration))
      eval(parse(text = paste0("CovRandPar_Diff_MAR_ILR_clean_",i,"[,Excluded_Runs_CovRandPar_MAR_ILR_Cal_",i,"+6] = NaN")))
      if (i==1){
        CovRandPar_Diff_MAR_ILR_clean = CovRandPar_Diff_MAR_ILR_clean_1
        Excluded_Runs_CovRandPar_MAR_ILR = Excluded_Runs_CovRandPar_MAR_ILR_num_1
      }  else {
        eval(parse(text = paste0("CovRandPar_Diff_MAR_ILR_clean = rbind(CovRandPar_Diff_MAR_ILR_clean, CovRandPar_Diff_MAR_ILR_clean_",i,")")))
        eval(parse(text = paste0("Excluded_Runs_CovRandPar_MAR_ILR = c(Excluded_Runs_CovRandPar_MAR_ILR, Excluded_Runs_CovRandPar_MAR_ILR_num_",i,")")))
      }
      eval(parse(text = paste0("CovRandPar_Diff_MAR_ILR_convergence_",i," = cbind(CovRandPar_Diff_MAR_ILR_clean_",i,"[,seq(1,6)], t(apply(CovRandPar_Diff_MAR_ILR_clean_",i,"[,c(seq(-1,-6), -(Excluded_Runs_CovRandPar_MAR_ILR_Cal_",i,"+6))], 1, FUN = function(x) cummean(x))))")))  #Cumulative Mean Value
      eval(parse(text = paste0("CovRandPar_Diff_MAR_ILR_convergence_norm_",i," = CovRandPar_Diff_MAR_ILR_convergence_",i)))
      eval(parse(text = paste0("CovRandPar_Diff_MAR_ILR_convergence_norm_",i,"[,seq(-1,-6)] = t(apply(CovRandPar_Diff_MAR_ILR_convergence_",i,"[,seq(-1,-6)], 1, FUN = function(x) x / tail(x,n=1)))"))) #Normalized Cumulative Mean Value 
    }
    CovRandPar_Diff_MAR_ILR_convergence_norm_melt = rbind(melt(CovRandPar_Diff_MAR_ILR_convergence_norm_1, id.vars = names(CovRandPar_Diff_MAR_ILR_convergence_norm_1[seq(1,6)])),
                                                          melt(CovRandPar_Diff_MAR_ILR_convergence_norm_2, id.vars = names(CovRandPar_Diff_MAR_ILR_convergence_norm_2[seq(1,6)])),
                                                          melt(CovRandPar_Diff_MAR_ILR_convergence_norm_3, id.vars = names(CovRandPar_Diff_MAR_ILR_convergence_norm_2[seq(1,6)])),
                                                          melt(CovRandPar_Diff_MAR_ILR_convergence_norm_4, id.vars = names(CovRandPar_Diff_MAR_ILR_convergence_norm_2[seq(1,6)])),
                                                          melt(CovRandPar_Diff_MAR_ILR_convergence_norm_5, id.vars = names(CovRandPar_Diff_MAR_ILR_convergence_norm_2[seq(1,6)])))
  }
  if (UCODE_Uncert==TRUE){
    flow_pred_dir = UCODE_MAR_ILR_dir
    for (i in 1:5){
      eval(parse(text = paste0('UCODE_MAR_ILR_',i,' = UCODE_linunc(flow_pred_dir, ',i,')')))
    }
    UCODE_MAR_ILR = rbind(UCODE_MAR_ILR_1, UCODE_MAR_ILR_2, UCODE_MAR_ILR_3, UCODE_MAR_ILR_4, UCODE_MAR_ILR_5)
    UCODE_MAR_ILR_diff = UCODE_MAR_ILR[grep(x=UCODE_MAR_ILR$PREDICTION.NAME, pattern = 'Diff'),]
    UCODE_MAR_ILR_diff$DATE = rep(seq(as.Date('1990-10-01'), as.Date('2011-09-30'), by = 'month'), each=5)
    UCODE_MAR_ILR_diff$MONTH = format(UCODE_MAR_ILR_diff$DATE, '%b')
    UCODE_MAR_ILR_diff$PRED.LOC = rep(seq(5,1),252)
    UCODE_MAR_ILR_diff$WATER.YEAR = rep(seq(1991,2011),each = 12*5)  # 12 months for 5 locations
  }
}

UniRandPar_diff_norm_test_MAR = subset(UniRandPar_Diff_MAR_clean, Month=='Oct' & Pred_Loc==1 & Date !='1990-10-01', select = names(UniRandPar_Diff_MAR_clean[seq(-1,-5)])) %>% 
  apply(1,shapiro.test) %>%
  sapply("[[", 2) %>%
  as.data.frame()
names(UniRandPar_diff_norm_test_MAR) = 'ShapTest_Sig'



# Normalized Difference Outlier Plots -------------------------------------
fig_name = 'Normalized Streamflow Differences'                                                                           
if (fig_format == 'jpg'){                                                                                     
  jpeg(paste0(out_dir,fig_name,'.jpg'), width = fig_width, height = fig_height, units = fig_units,  res = 600)
} else if (fig_format == 'png'){                                                                              
  png(paste0(out_dir,fig_name,'.png'), width = 7, height = 5, units = 'in',  res = 600) 
} else if (fig_format == 'pdf'){                                                                              
  #pdf(paste0(out_dir,fig_name,'.pdf'), width = 7, height = 5)                                
}                                                                                                             
grid.newpage()                                                                                                
pushViewport(viewport(layout = grid.layout(3,2)))                                                             
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)                                   
print(MAR_UniRandPar_Outliers_Plot +
        theme(axis.title = element_blank(),
              axis.text = element_text(size = 7),
              legend.position = c(0.8,0.9),
              legend.direction = 'horizontal',
              legend.background = element_rect(fill = NA, color = NA),
              legend.text = element_text(size = 7),
              legend.spacing.x = unit(-1,'mm'),
              legend.key = element_rect(fill = NA),
              plot.margin = margin(t=10,b=10,l=10,r=10)), vp = vplayout(1,1))   
print(MAR_CovRandPar_Outliers_Plot +
        theme(axis.title = element_blank(),
              axis.text = element_text(size = 7),
              legend.position = c(0.36,0.86),
              legend.direction = 'horizontal',
              legend.background = element_rect(fill = NA, color = NA),
              legend.text = element_text(size = 7),
              legend.spacing.x = unit(-1,'mm'),
              legend.spacing.y = unit(-6, 'mm'),
              legend.key = element_rect(fill = NA),
              plot.margin = margin(t=10,b=10,l=10,r=10)), vp = vplayout(1,2))
print(ILR_UniRandPar_Outliers_Plot +
        theme(axis.title = element_blank(),
              axis.text = element_text(size = 7),
              legend.position = 'none',
              plot.margin = margin(t=10,b=10,l=10,r=10)), vp = vplayout(2,1))
print(ILR_CovRandPar_Outliers_Plot +
        theme(axis.title = element_blank(),
              axis.text = element_text(size = 7),
              legend.position = 'none',
              plot.margin = margin(t=10,b=10,l=10,r=10)), vp = vplayout(2,2))
print(MAR_ILR_UniRandPar_Outliers_Plot +
        theme(axis.title = element_blank(),
              axis.text = element_text(size = 7),
              legend.position = 'none',
              plot.margin = margin(t=10,b=10,l=10,r=10)), vp = vplayout(3,1))
print(MAR_ILR_CovRandPar_Outliers_Plot +
        theme(axis.title = element_blank(),
              axis.text = element_text(size = 7),
              legend.position = 'none',
              plot.margin = margin(t=10,b=10,l=10,r=10)), vp = vplayout(3,2))
graphics.off()

# Streamflow Difference Normality Test ----------------------------------------------------------
#Uniform Distribution
UniRandPar_diff_norm_test_MAR = subset(UniRandPar_Diff_MAR_clean, Month=='Oct' & Pred_Loc==1, select = names(UniRandPar_Diff_MAR_clean[seq(-1,-5)])) %>% 
#UniRandPar_diff_norm_test_MAR = subset(UniRandPar_Diff_MAR_clean, Month%in%crit_months & Pred_Loc==1, select = names(UniRandPar_Diff_MAR_clean[seq(-1,-5)])) %>% 
    apply(1,shapiro.test) %>%
  sapply("[[", 2) %>%
  as.data.frame()
names(UniRandPar_diff_norm_test_MAR) = 'ShapTest_Sig'
UniRandPar_diff_norm_test_MAR = rbind(NA, UniRandPar_diff_norm_test_MAR)

UniRandPar_diff_norm_test_ILR = subset(UniRandPar_Diff_ILR_clean, Month=='Oct' & Pred_Loc==1, select = names(UniRandPar_Diff_ILR_clean[seq(-1,-5)])) %>% 
#UniRandPar_diff_norm_test_ILR = subset(UniRandPar_Diff_ILR_clean, Month%in%crit_months & Pred_Loc==1, select = names(UniRandPar_Diff_ILR_clean[seq(-1,-5)])) %>% 
    apply(1,shapiro.test) %>%
  sapply("[[", 2) %>%
  as.data.frame()
names(UniRandPar_diff_norm_test_ILR) = 'ShapTest_Sig'

UniRandPar_diff_norm_test_MAR_ILR = subset(UniRandPar_Diff_MAR_ILR_clean, Month=='Oct' & Pred_Loc==1, select = names(UniRandPar_Diff_MAR_ILR_clean[seq(-1,-5)])) %>% 
#UniRandPar_diff_norm_test_MAR_ILR = subset(UniRandPar_Diff_MAR_ILR_clean, Month%in%crit_months & Pred_Loc==1, select = names(UniRandPar_Diff_MAR_ILR_clean[seq(-1,-5)])) %>% 
  apply(1,shapiro.test) %>%
  sapply("[[", 2) %>%
  as.data.frame()
names(UniRandPar_diff_norm_test_MAR_ILR) = 'ShapTest_Sig'

UniRandPar_ShapTest_Sig = data.frame(WY = seq(1991,2011),
                                     Calibration = 'Uniform',
                                     MAR = UniRandPar_diff_norm_test_MAR$ShapTest_Sig,
                                     ILR = UniRandPar_diff_norm_test_ILR$ShapTest_Sig,
                                     MAR_ILR = UniRandPar_diff_norm_test_MAR_ILR$ShapTest_Sig) %>% melt(id.vars = c('WY', 'Calibration'))


CovRandPar_diff_norm_test_MAR = subset(CovRandPar_Diff_MAR_clean, Month=='Oct' & Pred_Loc==1 & Date !='1990-10-01', select = names(CovRandPar_Diff_MAR_clean[seq(-1,-6)])) %>% 
  apply(1,shapiro.test) %>%
  sapply("[[", 2) %>%
  as.data.frame()
names(CovRandPar_diff_norm_test_MAR) = 'ShapTest_Sig'
CovRandPar_diff_norm_test_MAR = c(NA, CovRandPar_diff_norm_test_MAR[1:20,],
                                  NA, CovRandPar_diff_norm_test_MAR[21:40,],
                                  NA, CovRandPar_diff_norm_test_MAR[41:60,],
                                  NA, CovRandPar_diff_norm_test_MAR[61:80,],
                                  NA, CovRandPar_diff_norm_test_MAR[81:100,])  #add NA value becasue first year of MAR all values are 0 which give shapiro.test an error

CovRandPar_diff_norm_test_ILR = subset(CovRandPar_Diff_ILR, Month=='Oct' & Pred_Loc==1, select = names(CovRandPar_Diff_ILR[seq(-1,-6)])) %>% 
  apply(1,shapiro.test) %>%
  sapply("[[", 2) %>%
  as.data.frame()
names(CovRandPar_diff_norm_test_ILR) = 'ShapTest_Sig'

CovRandPar_diff_norm_test_MAR_ILR = subset(CovRandPar_Diff_MAR_ILR_clean, Month=='Oct' & Pred_Loc==1, select = names(CovRandPar_Diff_MAR_ILR_clean[seq(-1,-6)])) %>% 
  apply(1,shapiro.test) %>%
  sapply("[[", 2) %>%
  as.data.frame()
names(CovRandPar_diff_norm_test_MAR_ILR) = 'ShapTest_Sig'

CovRandPar_ShapTest_Sig = data.frame(WY = rep(seq(1991,2011),5),
                                     Calibration = subset(CovRandPar_Diff_ILR_clean, Month=='Oct' & Pred_Loc==1, select = 'Calibration'),
                                     MAR = CovRandPar_diff_norm_test_MAR,
                                     ILR = CovRandPar_diff_norm_test_ILR$ShapTest_Sig,
                                     MAR_ILR = CovRandPar_diff_norm_test_MAR_ILR$ShapTest_Sig) %>% melt(id.vars = c('WY','Calibration'))

ShapTest_Sig = rbind(UniRandPar_ShapTest_Sig,CovRandPar_ShapTest_Sig)

# Cumulative Density Functions  --------------------------------------------
#Create lists of empirical CDFs by water year
UniRandPar_Diff_MAR_cdf = subset(UniRandPar_Diff_MAR_clean, Month=='Oct' & Pred_Loc==1, select = names(UniRandPar_Diff_MAR_clean[seq(-1,-5)])) %>% apply(1,ecdf)
UniRandPar_Diff_ILR_cdf = subset(UniRandPar_Diff_ILR_clean, Month=='Oct' & Pred_Loc==1, select = names(UniRandPar_Diff_ILR_clean[seq(-1,-5)])) %>% apply(1,ecdf)
UniRandPar_Diff_MAR_ILR_cdf = subset(UniRandPar_Diff_MAR_ILR_clean, Month=='Oct' & Pred_Loc==1, select = names(UniRandPar_Diff_MAR_ILR_clean[seq(-1,-5)])) %>% apply(1,ecdf)

UniRandPar_Diff_MAR_mean = subset(UniRandPar_Diff_MAR_clean, Month=='Oct' & Pred_Loc==1, select = names(UniRandPar_Diff_MAR_clean[seq(-1,-5)])) %>% apply(1,FUN = function(x) mean(x, na.rm = T))
UniRandPar_Diff_MAR_sd = subset(UniRandPar_Diff_MAR_clean, Month=='Oct' & Pred_Loc==1, select = names(UniRandPar_Diff_MAR_clean[seq(-1,-5)])) %>% apply(1,FUN = function(x) sd(x, na.rm = T))
UniRandPar_Diff_ILR_mean = subset(UniRandPar_Diff_ILR_clean, Month=='Oct' & Pred_Loc==1, select = names(UniRandPar_Diff_ILR_clean[seq(-1,-5)])) %>% apply(1,FUN = function(x) mean(x, na.rm = T))
UniRandPar_Diff_ILR_sd = subset(UniRandPar_Diff_ILR_clean, Month=='Oct' & Pred_Loc==1, select = names(UniRandPar_Diff_ILR_clean[seq(-1,-5)])) %>% apply(1,FUN = function(x) sd(x, na.rm = T))
UniRandPar_Diff_MAR_ILR_mean = subset(UniRandPar_Diff_MAR_ILR_clean, Month=='Oct' & Pred_Loc==1, select = names(UniRandPar_Diff_MAR_ILR_clean[seq(-1,-5)])) %>% apply(1,FUN = function(x) mean(x, na.rm = T))
UniRandPar_Diff_MAR_ILR_sd = subset(UniRandPar_Diff_MAR_ILR_clean, Month=='Oct' & Pred_Loc==1, select = names(UniRandPar_Diff_MAR_ILR_clean[seq(-1,-5)])) %>% apply(1,FUN = function(x) sd(x, na.rm = T))

UniRandPar_Diff_Stats = data.frame(WY = seq(1991,2011),
                                   MAR_mean = c(NaN,UniRandPar_Diff_MAR_mean),
                                   MAR_sd = c(NaN,UniRandPar_Diff_MAR_sd),
                                   ILR_mean = UniRandPar_Diff_ILR_mean,
                                   ILR_sd = UniRandPar_Diff_ILR_sd,
                                   MAR_ILR_mean = UniRandPar_Diff_MAR_ILR_mean,
                                   MAR_ILR_sd = UniRandPar_Diff_MAR_ILR_sd)

CovRandPar_Diff_MAR_cdf = subset(CovRandPar_Diff_MAR_clean, Month=='Oct' & Pred_Loc==1, select = names(CovRandPar_Diff_MAR_clean[seq(-1,-6)])) %>% apply(1,ecdf)
CovRandPar_Diff_ILR_cdf = subset(CovRandPar_Diff_ILR, Month=='Oct' & Pred_Loc==1, select = names(CovRandPar_Diff_ILR[seq(-1,-6)])) %>% apply(1,ecdf)
CovRandPar_Diff_MAR_ILR_cdf = subset(CovRandPar_Diff_MAR_ILR_clean, Month=='Oct' & Pred_Loc==1, select = names(CovRandPar_Diff_MAR_ILR_clean[seq(-1,-6)])) %>% apply(1,ecdf)

CovRandPar_Diff_MAR_mean = subset(CovRandPar_Diff_MAR_clean, Month=='Oct' & Pred_Loc==1, select = names(CovRandPar_Diff_MAR_clean[seq(-1,-6)])) %>% apply(1,FUN = function(x) mean(x, na.rm = T))
CovRandPar_Diff_MAR_sd = subset(CovRandPar_Diff_MAR_clean, Month=='Oct' & Pred_Loc==1, select = names(CovRandPar_Diff_MAR_clean[seq(-1,-6)])) %>% apply(1,FUN = function(x) sd(x, na.rm = T))
CovRandPar_Diff_ILR_mean = subset(CovRandPar_Diff_ILR, Month=='Oct' & Pred_Loc==1, select = names(CovRandPar_Diff_ILR[seq(-1,-6)])) %>% apply(1,FUN = function(x) mean(x, na.rm = T))
CovRandPar_Diff_ILR_sd = subset(CovRandPar_Diff_ILR, Month=='Oct' & Pred_Loc==1, select = names(CovRandPar_Diff_ILR[seq(-1,-6)])) %>% apply(1,FUN = function(x) sd(x, na.rm = T))
CovRandPar_Diff_MAR_ILR_mean = subset(CovRandPar_Diff_MAR_ILR_clean, Month=='Oct' & Pred_Loc==1, select = names(CovRandPar_Diff_MAR_ILR_clean[seq(-1,-6)])) %>% apply(1,FUN = function(x) mean(x, na.rm = T))
CovRandPar_Diff_MAR_ILR_sd = subset(CovRandPar_Diff_MAR_ILR_clean, Month=='Oct' & Pred_Loc==1, select = names(CovRandPar_Diff_MAR_ILR_clean[seq(-1,-6)])) %>% apply(1,FUN = function(x) sd(x, na.rm = T))

CovRandPar_Diff_Stats = data.frame(WY = rep(seq(1991,2011),5),
                                   Calibration = paste0('Cal_',rep(seq(1,5), each = 21)),
                                   MAR_mean = c(NaN, CovRandPar_Diff_MAR_mean[1:20],NaN,CovRandPar_Diff_MAR_mean[21:40],NaN,CovRandPar_Diff_MAR_mean[41:60],NaN,CovRandPar_Diff_MAR_mean[61:80],NaN,CovRandPar_Diff_MAR_mean[81:100]),
                                   MAR_sd = c(NaN, CovRandPar_Diff_MAR_sd[1:20],NaN,CovRandPar_Diff_MAR_sd[21:40],NaN,CovRandPar_Diff_MAR_sd[41:60],NaN,CovRandPar_Diff_MAR_sd[61:80],NaN,CovRandPar_Diff_MAR_sd[81:100]),
                                   ILR_mean = CovRandPar_Diff_ILR_mean,
                                   ILR_sd = CovRandPar_Diff_ILR_sd,
                                   MAR_ILR_mean = CovRandPar_Diff_MAR_ILR_mean,
                                   MAR_ILR_sd = CovRandPar_Diff_MAR_ILR_sd)

#Plot October Dry (2001) CDFs for MAR
MAR_2001_Locs = seq(11,by=20, length.out = 5)   #Since Oct-1990 has been removed for MAR, there are only 20 years so the indexing for the MAR is different than for the ILR or MAR_ILR scenarios
ECDF_2001_Locs = seq(12,by=21, length.out = 5)
Streamflow_Diff_Vals_MAR_Dry = exp(seq(log(1E-1), log(1E3), length.out = 10000))
RandPar_Diff_MAR_Dry = data.frame(Streamflow_Diff_mday = Streamflow_Diff_Vals_MAR_Dry,
                                  Uniform = UniRandPar_Diff_MAR_cdf[[MAR_2001_Locs[1]]](Streamflow_Diff_Vals_MAR_Dry),  #use function to evaluate at Streamflow_Diff_Vals_MAR_Dry
                                  Uniform_norm = ecdf(rnorm(n = 10000, mean = UniRandPar_Diff_Stats$MAR_mean[ECDF_2001_Locs[1]], sd = UniRandPar_Diff_Stats$MAR_sd[ECDF_2001_Locs[1]]))(Streamflow_Diff_Vals_MAR_Dry),
                                  Cal_1 = CovRandPar_Diff_MAR_cdf[[MAR_2001_Locs[1]]](Streamflow_Diff_Vals_MAR_Dry),  #use function to evaluate at Streamflow_Diff_Vals_MAR_Dry
                                  Cal_1_norm = ecdf(rnorm(n = 10000, mean = CovRandPar_Diff_Stats$MAR_mean[ECDF_2001_Locs[1]], sd = CovRandPar_Diff_Stats$MAR_sd[ECDF_2001_Locs[1]]))(Streamflow_Diff_Vals_MAR_Dry),
                                  Cal_2 = CovRandPar_Diff_MAR_cdf[[MAR_2001_Locs[2]]](Streamflow_Diff_Vals_MAR_Dry),  #use function to evaluate at Streamflow_Diff_Vals_MAR_Dry
                                  Cal_2_norm = ecdf(rnorm(n = 10000, mean = CovRandPar_Diff_Stats$MAR_mean[ECDF_2001_Locs[2]], sd = CovRandPar_Diff_Stats$MAR_sd[ECDF_2001_Locs[2]]))(Streamflow_Diff_Vals_MAR_Dry),
                                  Cal_3 = CovRandPar_Diff_MAR_cdf[[MAR_2001_Locs[3]]](Streamflow_Diff_Vals_MAR_Dry),  #use function to evaluate at Streamflow_Diff_Vals_MAR_Dry
                                  Cal_3_norm = ecdf(rnorm(n = 10000, mean = CovRandPar_Diff_Stats$MAR_mean[ECDF_2001_Locs[3]], sd = CovRandPar_Diff_Stats$MAR_sd[ECDF_2001_Locs[3]]))(Streamflow_Diff_Vals_MAR_Dry),
                                  Cal_4 = CovRandPar_Diff_MAR_cdf[[MAR_2001_Locs[4]]](Streamflow_Diff_Vals_MAR_Dry),  #use function to evaluate at Streamflow_Diff_Vals_MAR_Dry
                                  Cal_4_norm = ecdf(rnorm(n = 10000, mean = CovRandPar_Diff_Stats$MAR_mean[ECDF_2001_Locs[4]], sd = CovRandPar_Diff_Stats$MAR_sd[ECDF_2001_Locs[4]]))(Streamflow_Diff_Vals_MAR_Dry),
                                  Cal_5 = CovRandPar_Diff_MAR_cdf[[MAR_2001_Locs[5]]](Streamflow_Diff_Vals_MAR_Dry),  #use function to evaluate at Streamflow_Diff_Vals_MAR_Dry
                                  Cal_5_norm = ecdf(rnorm(n = 10000, mean = CovRandPar_Diff_Stats$MAR_mean[ECDF_2001_Locs[5]], sd = CovRandPar_Diff_Stats$MAR_sd[ECDF_2001_Locs[5]]))(Streamflow_Diff_Vals_MAR_Dry))

RandPar_CDFs_MAR_Dry_Year_Plot = ggplot(data = NULL) + 
  geom_line(data = RandPar_Diff_MAR_Dry, aes(x =Streamflow_Diff_mday, y = Uniform, color = 'Uniform'), size = 0.5) +
  geom_line(data = RandPar_Diff_MAR_Dry, aes(x =Streamflow_Diff_mday, y = Uniform_norm), color = '#1b9e77', size = 0.5, linetype = 'dashed') +
  geom_line(data = RandPar_Diff_MAR_Dry, aes(x =Streamflow_Diff_mday, y = Cal_1, color = 'Cal_1'), size = 0.5) +
  geom_line(data = RandPar_Diff_MAR_Dry, aes(x =Streamflow_Diff_mday, y = Cal_1_norm), color = '#d95f02', size = 0.5, linetype = 'dashed') +
  geom_line(data = RandPar_Diff_MAR_Dry, aes(x =Streamflow_Diff_mday, y = Cal_2, color = 'Cal_2'), size = 0.5) +
  geom_line(data = RandPar_Diff_MAR_Dry, aes(x =Streamflow_Diff_mday, y = Cal_2_norm),color = '#7570b3', size = 0.5, linetype = 'dashed') +
  geom_line(data = RandPar_Diff_MAR_Dry, aes(x =Streamflow_Diff_mday, y = Cal_3, color = 'Cal_3'), size = 0.5) +
  geom_line(data = RandPar_Diff_MAR_Dry, aes(x =Streamflow_Diff_mday, y = Cal_3_norm),color = '#e7298a', size = 0.5, linetype = 'dashed') +
  geom_line(data = RandPar_Diff_MAR_Dry, aes(x =Streamflow_Diff_mday, y = Cal_4, color = 'Cal_4'), size = 0.5) +
  geom_line(data = RandPar_Diff_MAR_Dry, aes(x =Streamflow_Diff_mday, y = Cal_4_norm),color = '#66a61e', size = 0.5, linetype = 'dashed') +
  geom_line(data = RandPar_Diff_MAR_Dry, aes(x =Streamflow_Diff_mday, y = Cal_5, color = 'Cal_5'), size = 0.5) +
  geom_line(data = RandPar_Diff_MAR_Dry, aes(x =Streamflow_Diff_mday, y = Cal_5_norm),color = '#e6ab02', size = 0.5, linetype = 'dashed') +
  scale_x_continuous(limits = c(0,300), breaks = seq(0,300, by = 100), labels = c('0', '1E2', '2E2', '3E2'), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by = 0.2), expand = c(0,0)) +
  scale_color_manual(breaks = c('Uniform', paste0('Cal_',seq(1,5))), values = c('#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02','#1b9e77')) +
  xlab(bquote("Streamflow Difference ("*m^3*"/day)")) +
  ylab('Dry Year (2001)\nProbability (%)') +
  theme_few() + 
  theme(legend.position = c(0.85,0.25),
        legend.title = element_blank(),
        legend.background = element_rect(fill=NA, color = NA))

#Plot October Dry (2001) CDFs for ILR
Streamflow_Diff_Vals_ILR_Dry = exp(seq(log(1E-1), log(1E3), length.out = 10000))
RandPar_Diff_ILR_Dry = data.frame(Streamflow_Diff_mday = Streamflow_Diff_Vals_ILR_Dry,
                                  Uniform = UniRandPar_Diff_ILR_cdf[[12]](Streamflow_Diff_Vals_ILR_Dry),  #use function to evaluate at Streamflow_Diff_Vals_ILR_Dry
                                  Uniform_norm = ecdf(rnorm(n = 10000, mean = UniRandPar_Diff_Stats$ILR_mean[12], sd = UniRandPar_Diff_Stats$ILR_sd[12]))(Streamflow_Diff_Vals_ILR_Dry),
                                  Cal_1 = CovRandPar_Diff_ILR_cdf[[12]](Streamflow_Diff_Vals_ILR_Dry),  #use function to evaluate at Streamflow_Diff_Vals_ILR_Dry
                                  Cal_1_norm = ecdf(rnorm(n = 10000, mean = CovRandPar_Diff_Stats$ILR_mean[12], sd = CovRandPar_Diff_Stats$ILR_sd[12]))(Streamflow_Diff_Vals_ILR_Dry),
                                  Cal_2 = CovRandPar_Diff_ILR_cdf[[33]](Streamflow_Diff_Vals_ILR_Dry),  #use function to evaluate at Streamflow_Diff_Vals_ILR_Dry
                                  Cal_2_norm = ecdf(rnorm(n = 10000, mean = CovRandPar_Diff_Stats$ILR_mean[33], sd = CovRandPar_Diff_Stats$ILR_sd[33]))(Streamflow_Diff_Vals_ILR_Dry),
                                  Cal_3 = CovRandPar_Diff_ILR_cdf[[54]](Streamflow_Diff_Vals_ILR_Dry),  #use function to evaluate at Streamflow_Diff_Vals_ILR_Dry
                                  Cal_3_norm = ecdf(rnorm(n = 10000, mean = CovRandPar_Diff_Stats$ILR_mean[54], sd = CovRandPar_Diff_Stats$ILR_sd[54]))(Streamflow_Diff_Vals_ILR_Dry),
                                  Cal_4 = CovRandPar_Diff_ILR_cdf[[75]](Streamflow_Diff_Vals_ILR_Dry),  #use function to evaluate at Streamflow_Diff_Vals_ILR_Dry
                                  Cal_4_norm = ecdf(rnorm(n = 10000, mean = CovRandPar_Diff_Stats$ILR_mean[75], sd = CovRandPar_Diff_Stats$ILR_sd[75]))(Streamflow_Diff_Vals_ILR_Dry),
                                  Cal_5 = CovRandPar_Diff_ILR_cdf[[96]](Streamflow_Diff_Vals_ILR_Dry),  #use function to evaluate at Streamflow_Diff_Vals_ILR_Dry
                                  Cal_5_norm = ecdf(rnorm(n = 10000, mean = CovRandPar_Diff_Stats$ILR_mean[96], sd = CovRandPar_Diff_Stats$ILR_sd[96]))(Streamflow_Diff_Vals_ILR_Dry))

RandPar_CDFs_ILR_Dry_Year_Plot = ggplot(data = NULL) + 
  geom_line(data = RandPar_Diff_ILR_Dry, aes(x =Streamflow_Diff_mday, y = Uniform, color = 'Uniform'), size = 0.5) +
  geom_line(data = RandPar_Diff_ILR_Dry, aes(x =Streamflow_Diff_mday, y = Uniform_norm), color = '#1b9e77', size = 0.5, linetype = 'dashed') +
  geom_line(data = RandPar_Diff_ILR_Dry, aes(x =Streamflow_Diff_mday, y = Cal_1, color = 'Cal_1'), size = 0.5) +
  geom_line(data = RandPar_Diff_ILR_Dry, aes(x =Streamflow_Diff_mday, y = Cal_1_norm), color = '#d95f02', size = 0.5, linetype = 'dashed') +
  geom_line(data = RandPar_Diff_ILR_Dry, aes(x =Streamflow_Diff_mday, y = Cal_2, color = 'Cal_2'), size = 0.5) +
  geom_line(data = RandPar_Diff_ILR_Dry, aes(x =Streamflow_Diff_mday, y = Cal_2_norm),color = '#7570b3', size = 0.5, linetype = 'dashed') +
  geom_line(data = RandPar_Diff_ILR_Dry, aes(x =Streamflow_Diff_mday, y = Cal_3, color = 'Cal_3'), size = 0.5) +
  geom_line(data = RandPar_Diff_ILR_Dry, aes(x =Streamflow_Diff_mday, y = Cal_3_norm),color = '#e7298a', size = 0.5, linetype = 'dashed') +
  geom_line(data = RandPar_Diff_ILR_Dry, aes(x =Streamflow_Diff_mday, y = Cal_4, color = 'Cal_4'), size = 0.5) +
  geom_line(data = RandPar_Diff_ILR_Dry, aes(x =Streamflow_Diff_mday, y = Cal_4_norm),color = '#66a61e', size = 0.5, linetype = 'dashed') +
  geom_line(data = RandPar_Diff_ILR_Dry, aes(x =Streamflow_Diff_mday, y = Cal_5, color = 'Cal_5'), size = 0.5) +
  geom_line(data = RandPar_Diff_ILR_Dry, aes(x =Streamflow_Diff_mday, y = Cal_5_norm),color = '#e6ab02', size = 0.5, linetype = 'dashed') +
  scale_x_continuous(limits = c(0,1000), breaks = seq(0,1000, by = 200), labels = c('0', '2E2', '4E2', '6E2', '8E2', '1E3'), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by = 0.2), expand = c(0,0)) +
  scale_color_manual(breaks = c('Uniform', paste0('Cal_',seq(1,5))), values = c('#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02','#1b9e77')) +
  xlab(bquote("Streamflow Difference ("*m^3*"/day)")) +
  ylab('Dry Year (2001)\nProbability (%)') +
  theme_few() + 
  theme(legend.position = c(0.85,0.25),
        legend.title = element_blank(),
        legend.background = element_rect(fill=NA, color = NA))

#Plot October Dry (2001) CDFs for MAR_ILR
Streamflow_Diff_Vals_MAR_ILR_Dry = exp(seq(log(1E-1), log(1000), length.out = 10000))
RandPar_Diff_MAR_ILR_Dry = data.frame(Streamflow_Diff_mday = Streamflow_Diff_Vals_MAR_ILR_Dry,
                                  Uniform = UniRandPar_Diff_MAR_ILR_cdf[[12]](Streamflow_Diff_Vals_MAR_ILR_Dry),  #use function to evaluate at Streamflow_Diff_Vals_MAR_ILR_Dry
                                  Uniform_norm = ecdf(rnorm(n = 10000, mean = UniRandPar_Diff_Stats$MAR_ILR_mean[12], sd = UniRandPar_Diff_Stats$MAR_ILR_sd[12]))(Streamflow_Diff_Vals_MAR_ILR_Dry),
                                  Cal_1 = CovRandPar_Diff_MAR_ILR_cdf[[12]](Streamflow_Diff_Vals_MAR_ILR_Dry),  #use function to evaluate at Streamflow_Diff_Vals_MAR_ILR_Dry
                                  Cal_1_norm = ecdf(rnorm(n = 10000, mean = CovRandPar_Diff_Stats$MAR_ILR_mean[12], sd = CovRandPar_Diff_Stats$MAR_ILR_sd[12]))(Streamflow_Diff_Vals_MAR_ILR_Dry),
                                  Cal_2 = CovRandPar_Diff_MAR_ILR_cdf[[33]](Streamflow_Diff_Vals_MAR_ILR_Dry),  #use function to evaluate at Streamflow_Diff_Vals_MAR_ILR_Dry
                                  Cal_2_norm = ecdf(rnorm(n = 10000, mean = CovRandPar_Diff_Stats$MAR_ILR_mean[33], sd = CovRandPar_Diff_Stats$MAR_ILR_sd[33]))(Streamflow_Diff_Vals_MAR_ILR_Dry),
                                  Cal_3 = CovRandPar_Diff_MAR_ILR_cdf[[54]](Streamflow_Diff_Vals_MAR_ILR_Dry),  #use function to evaluate at Streamflow_Diff_Vals_MAR_ILR_Dry
                                  Cal_3_norm = ecdf(rnorm(n = 10000, mean = CovRandPar_Diff_Stats$MAR_ILR_mean[54], sd = CovRandPar_Diff_Stats$MAR_ILR_sd[54]))(Streamflow_Diff_Vals_MAR_ILR_Dry),
                                  Cal_4 = CovRandPar_Diff_MAR_ILR_cdf[[75]](Streamflow_Diff_Vals_MAR_ILR_Dry),  #use function to evaluate at Streamflow_Diff_Vals_MAR_ILR_Dry
                                  Cal_4_norm = ecdf(rnorm(n = 10000, mean = CovRandPar_Diff_Stats$MAR_ILR_mean[75], sd = CovRandPar_Diff_Stats$MAR_ILR_sd[75]))(Streamflow_Diff_Vals_MAR_ILR_Dry),
                                  Cal_5 = CovRandPar_Diff_MAR_ILR_cdf[[96]](Streamflow_Diff_Vals_MAR_ILR_Dry),  #use function to evaluate at Streamflow_Diff_Vals_MAR_ILR_Dry
                                  Cal_5_norm = ecdf(rnorm(n = 10000, mean = CovRandPar_Diff_Stats$MAR_ILR_mean[96], sd = CovRandPar_Diff_Stats$MAR_ILR_sd[96]))(Streamflow_Diff_Vals_MAR_ILR_Dry))

RandPar_CDFs_MAR_ILR_Dry_Year_Plot = ggplot(data = NULL) + 
  geom_line(data = RandPar_Diff_MAR_ILR_Dry, aes(x =Streamflow_Diff_mday, y = Uniform, color = 'Uniform'), size = 0.5) +
  geom_line(data = RandPar_Diff_MAR_ILR_Dry, aes(x =Streamflow_Diff_mday, y = Uniform_norm), color = '#1b9e77', size = 0.5, linetype = 'dashed') +
  geom_line(data = RandPar_Diff_MAR_ILR_Dry, aes(x =Streamflow_Diff_mday, y = Cal_1, color = 'Cal_1'), size = 0.5) +
  geom_line(data = RandPar_Diff_MAR_ILR_Dry, aes(x =Streamflow_Diff_mday, y = Cal_1_norm), color = '#d95f02', size = 0.5, linetype = 'dashed') +
  geom_line(data = RandPar_Diff_MAR_ILR_Dry, aes(x =Streamflow_Diff_mday, y = Cal_2, color = 'Cal_2'), size = 0.5) +
  geom_line(data = RandPar_Diff_MAR_ILR_Dry, aes(x =Streamflow_Diff_mday, y = Cal_2_norm),color = '#7570b3', size = 0.5, linetype = 'dashed') +
  geom_line(data = RandPar_Diff_MAR_ILR_Dry, aes(x =Streamflow_Diff_mday, y = Cal_3, color = 'Cal_3'), size = 0.5) +
  geom_line(data = RandPar_Diff_MAR_ILR_Dry, aes(x =Streamflow_Diff_mday, y = Cal_3_norm),color = '#e7298a', size = 0.5, linetype = 'dashed') +
  geom_line(data = RandPar_Diff_MAR_ILR_Dry, aes(x =Streamflow_Diff_mday, y = Cal_4, color = 'Cal_4'), size = 0.5) +
  geom_line(data = RandPar_Diff_MAR_ILR_Dry, aes(x =Streamflow_Diff_mday, y = Cal_4_norm),color = '#66a61e', size = 0.5, linetype = 'dashed') +
  geom_line(data = RandPar_Diff_MAR_ILR_Dry, aes(x =Streamflow_Diff_mday, y = Cal_5, color = 'Cal_5'), size = 0.5) +
  geom_line(data = RandPar_Diff_MAR_ILR_Dry, aes(x =Streamflow_Diff_mday, y = Cal_5_norm),color = '#e6ab02', size = 0.5, linetype = 'dashed') +
  scale_x_continuous(limits = c(0,1000), breaks = seq(0,1000, by = 200), labels = c('0', '2E2', '4E2', '6E2', '8E2', '1E3'), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by = 0.2), expand = c(0,0)) +
  scale_color_manual(breaks = c('Uniform', paste0('Cal_',seq(1,5))), values = c('#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02','#1b9e77')) +
  xlab(bquote("Streamflow Difference ("*m^3*"/day)")) +
  ylab('Dry Year (2001)\nProbability (%)') +
  theme_few() + 
  theme(legend.position = c(0.85,0.25),
        legend.title = element_blank(),
        legend.background = element_rect(fill=NA, color = NA))

#Plot October Average (2010) CDFs for MAR
MAR_2010_Locs = seq(20,by=20, length.out = 5)   #Since Oct-1990 has been removed for MAR, there are only 20 years so the indexing for the MAR is different than for the ILR or MAR_ILR scenarios
ECDF_2010_Locs = seq(21,by=21, length.out = 5)
Streamflow_Diff_Vals_MAR_Avg = exp(seq(log(1E1), log(2E4), length.out = 10000))
RandPar_Diff_MAR_Avg = data.frame(Streamflow_Diff_mday = Streamflow_Diff_Vals_MAR_Avg,
                                  Uniform = UniRandPar_Diff_MAR_cdf[[MAR_2010_Locs[1]]](Streamflow_Diff_Vals_MAR_Avg),  #use function to evaluate at Streamflow_Diff_Vals_MAR_Avg
                                  Uniform_norm = ecdf(rnorm(n = 10000, mean = UniRandPar_Diff_Stats$MAR_mean[ECDF_2010_Locs[1]], sd = UniRandPar_Diff_Stats$MAR_sd[ECDF_2010_Locs[1]]))(Streamflow_Diff_Vals_MAR_Avg),
                                  Cal_1 = CovRandPar_Diff_MAR_cdf[[MAR_2010_Locs[1]]](Streamflow_Diff_Vals_MAR_Avg),  #use function to evaluate at Streamflow_Diff_Vals_MAR_Avg
                                  Cal_1_norm = ecdf(rnorm(n = 10000, mean = CovRandPar_Diff_Stats$MAR_mean[ECDF_2010_Locs[1]], sd = CovRandPar_Diff_Stats$MAR_sd[ECDF_2010_Locs[1]]))(Streamflow_Diff_Vals_MAR_Avg),
                                  Cal_2 = CovRandPar_Diff_MAR_cdf[[MAR_2010_Locs[2]]](Streamflow_Diff_Vals_MAR_Avg),  #use function to evaluate at Streamflow_Diff_Vals_MAR_Avg
                                  Cal_2_norm = ecdf(rnorm(n = 10000, mean = CovRandPar_Diff_Stats$MAR_mean[ECDF_2010_Locs[2]], sd = CovRandPar_Diff_Stats$MAR_sd[ECDF_2010_Locs[2]]))(Streamflow_Diff_Vals_MAR_Avg),
                                  Cal_3 = CovRandPar_Diff_MAR_cdf[[MAR_2010_Locs[3]]](Streamflow_Diff_Vals_MAR_Avg),  #use function to evaluate at Streamflow_Diff_Vals_MAR_Avg
                                  Cal_3_norm = ecdf(rnorm(n = 10000, mean = CovRandPar_Diff_Stats$MAR_mean[ECDF_2010_Locs[3]], sd = CovRandPar_Diff_Stats$MAR_sd[ECDF_2010_Locs[3]]))(Streamflow_Diff_Vals_MAR_Avg),
                                  Cal_4 = CovRandPar_Diff_MAR_cdf[[MAR_2010_Locs[4]]](Streamflow_Diff_Vals_MAR_Avg),  #use function to evaluate at Streamflow_Diff_Vals_MAR_Avg
                                  Cal_4_norm = ecdf(rnorm(n = 10000, mean = CovRandPar_Diff_Stats$MAR_mean[ECDF_2010_Locs[4]], sd = CovRandPar_Diff_Stats$MAR_sd[ECDF_2010_Locs[4]]))(Streamflow_Diff_Vals_MAR_Avg),
                                  Cal_5 = CovRandPar_Diff_MAR_cdf[[MAR_2010_Locs[5]]](Streamflow_Diff_Vals_MAR_Avg),  #use function to evaluate at Streamflow_Diff_Vals_MAR_Avg
                                  Cal_5_norm = ecdf(rnorm(n = 10000, mean = CovRandPar_Diff_Stats$MAR_mean[ECDF_2010_Locs[5]], sd = CovRandPar_Diff_Stats$MAR_sd[ECDF_2010_Locs[5]]))(Streamflow_Diff_Vals_MAR_Avg))

RandPar_CDFs_MAR_Avg_Year_Plot = ggplot(data = NULL) + 
  geom_line(data = RandPar_Diff_MAR_Avg, aes(x =Streamflow_Diff_mday, y = Uniform, color = 'Uniform'), size = 0.5) +
  geom_line(data = RandPar_Diff_MAR_Avg, aes(x =Streamflow_Diff_mday, y = Uniform_norm), color = '#1b9e77', size = 0.5, linetype = 'dashed') +
  geom_line(data = RandPar_Diff_MAR_Avg, aes(x =Streamflow_Diff_mday, y = Cal_1, color = 'Cal_1'), size = 0.5) +
  geom_line(data = RandPar_Diff_MAR_Avg, aes(x =Streamflow_Diff_mday, y = Cal_1_norm), color = '#d95f02', size = 0.5, linetype = 'dashed') +
  geom_line(data = RandPar_Diff_MAR_Avg, aes(x =Streamflow_Diff_mday, y = Cal_2, color = 'Cal_2'), size = 0.5) +
  geom_line(data = RandPar_Diff_MAR_Avg, aes(x =Streamflow_Diff_mday, y = Cal_2_norm),color = '#7570b3', size = 0.5, linetype = 'dashed') +
  geom_line(data = RandPar_Diff_MAR_Avg, aes(x =Streamflow_Diff_mday, y = Cal_3, color = 'Cal_3'), size = 0.5) +
  geom_line(data = RandPar_Diff_MAR_Avg, aes(x =Streamflow_Diff_mday, y = Cal_3_norm),color = '#e7298a', size = 0.5, linetype = 'dashed') +
  geom_line(data = RandPar_Diff_MAR_Avg, aes(x =Streamflow_Diff_mday, y = Cal_4, color = 'Cal_4'), size = 0.5) +
  geom_line(data = RandPar_Diff_MAR_Avg, aes(x =Streamflow_Diff_mday, y = Cal_4_norm),color = '#66a61e', size = 0.5, linetype = 'dashed') +
  geom_line(data = RandPar_Diff_MAR_Avg, aes(x =Streamflow_Diff_mday, y = Cal_5, color = 'Cal_5'), size = 0.5) +
  geom_line(data = RandPar_Diff_MAR_Avg, aes(x =Streamflow_Diff_mday, y = Cal_5_norm),color = '#e6ab02', size = 0.5, linetype = 'dashed') +
  scale_x_continuous(limits = c(0,20000), breaks = seq(0,20000, by = 4000), labels = c('0', '4E3', '8E3', '1.2E4', '1.6E4', '2E4'), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by = 0.2), expand = c(0,0)) +
  scale_color_manual(breaks = c('Uniform', paste0('Cal_',seq(1,5))), values = c('#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02','#1b9e77')) +
  xlab(bquote("Streamflow Difference ("*m^3*"/day)")) +
  ylab('Average Year (2010)\nProbability (%)') +
  theme_few() + 
  theme(legend.position = c(0.85,0.25),
        legend.title = element_blank(),
        legend.background = element_rect(fill=NA, color = NA))

#Plot October Average (2010) CDFs for ILR
Streamflow_Diff_Vals_ILR_Avg = exp(seq(log(1E1), log(4E4), length.out = 10000))
RandPar_Diff_ILR_Avg = data.frame(Streamflow_Diff_mday = Streamflow_Diff_Vals_ILR_Avg,
                                  Uniform = UniRandPar_Diff_ILR_cdf[[21]](Streamflow_Diff_Vals_ILR_Avg),  #use function to evaluate at Streamflow_Diff_Vals_ILR_Avg
                                  Uniform_norm = ecdf(rnorm(n = 10000, mean = UniRandPar_Diff_Stats$ILR_mean[21], sd = UniRandPar_Diff_Stats$ILR_sd[21]))(Streamflow_Diff_Vals_ILR_Avg),
                                  Cal_1 = CovRandPar_Diff_ILR_cdf[[21]](Streamflow_Diff_Vals_ILR_Avg),  #use function to evaluate at Streamflow_Diff_Vals_ILR_Avg
                                  Cal_1_norm = ecdf(rnorm(n = 10000, mean = CovRandPar_Diff_Stats$ILR_mean[21], sd = CovRandPar_Diff_Stats$ILR_sd[21]))(Streamflow_Diff_Vals_ILR_Avg),
                                  Cal_2 = CovRandPar_Diff_ILR_cdf[[42]](Streamflow_Diff_Vals_ILR_Avg),  #use function to evaluate at Streamflow_Diff_Vals_ILR_Avg
                                  Cal_2_norm = ecdf(rnorm(n = 10000, mean = CovRandPar_Diff_Stats$ILR_mean[42], sd = CovRandPar_Diff_Stats$ILR_sd[42]))(Streamflow_Diff_Vals_ILR_Avg),
                                  Cal_3 = CovRandPar_Diff_ILR_cdf[[63]](Streamflow_Diff_Vals_ILR_Avg),  #use function to evaluate at Streamflow_Diff_Vals_ILR_Avg
                                  Cal_3_norm = ecdf(rnorm(n = 10000, mean = CovRandPar_Diff_Stats$ILR_mean[63], sd = CovRandPar_Diff_Stats$ILR_sd[63]))(Streamflow_Diff_Vals_ILR_Avg),
                                  Cal_4 = CovRandPar_Diff_ILR_cdf[[84]](Streamflow_Diff_Vals_ILR_Avg),  #use function to evaluate at Streamflow_Diff_Vals_ILR_Avg
                                  Cal_4_norm = ecdf(rnorm(n = 10000, mean = CovRandPar_Diff_Stats$ILR_mean[84], sd = CovRandPar_Diff_Stats$ILR_sd[84]))(Streamflow_Diff_Vals_ILR_Avg),
                                  Cal_5 = CovRandPar_Diff_ILR_cdf[[105]](Streamflow_Diff_Vals_ILR_Avg),  #use function to evaluate at Streamflow_Diff_Vals_ILR_Avg
                                  Cal_5_norm = ecdf(rnorm(n = 10000, mean = CovRandPar_Diff_Stats$ILR_mean[105], sd = CovRandPar_Diff_Stats$ILR_sd[105]))(Streamflow_Diff_Vals_ILR_Avg))

RandPar_CDFs_ILR_Avg_Year_Plot = ggplot(data = NULL) + 
  geom_line(data = RandPar_Diff_ILR_Avg, aes(x =Streamflow_Diff_mday, y = Uniform, color = 'Uniform'), size = 0.5) +
  geom_line(data = RandPar_Diff_ILR_Avg, aes(x =Streamflow_Diff_mday, y = Uniform_norm), color = '#1b9e77', size = 0.5, linetype = 'dashed') +
  geom_line(data = RandPar_Diff_ILR_Avg, aes(x =Streamflow_Diff_mday, y = Cal_1, color = 'Cal_1'), size = 0.5) +
  geom_line(data = RandPar_Diff_ILR_Avg, aes(x =Streamflow_Diff_mday, y = Cal_1_norm), color = '#d95f02', size = 0.5, linetype = 'dashed') +
  geom_line(data = RandPar_Diff_ILR_Avg, aes(x =Streamflow_Diff_mday, y = Cal_2, color = 'Cal_2'), size = 0.5) +
  geom_line(data = RandPar_Diff_ILR_Avg, aes(x =Streamflow_Diff_mday, y = Cal_2_norm),color = '#7570b3', size = 0.5, linetype = 'dashed') +
  geom_line(data = RandPar_Diff_ILR_Avg, aes(x =Streamflow_Diff_mday, y = Cal_3, color = 'Cal_3'), size = 0.5) +
  geom_line(data = RandPar_Diff_ILR_Avg, aes(x =Streamflow_Diff_mday, y = Cal_3_norm),color = '#e7298a', size = 0.5, linetype = 'dashed') +
  geom_line(data = RandPar_Diff_ILR_Avg, aes(x =Streamflow_Diff_mday, y = Cal_4, color = 'Cal_4'), size = 0.5) +
  geom_line(data = RandPar_Diff_ILR_Avg, aes(x =Streamflow_Diff_mday, y = Cal_4_norm),color = '#66a61e', size = 0.5, linetype = 'dashed') +
  geom_line(data = RandPar_Diff_ILR_Avg, aes(x =Streamflow_Diff_mday, y = Cal_5, color = 'Cal_5'), size = 0.5) +
  geom_line(data = RandPar_Diff_ILR_Avg, aes(x =Streamflow_Diff_mday, y = Cal_5_norm),color = '#e6ab02', size = 0.5, linetype = 'dashed') +
  scale_x_continuous(limits = c(0,40000), breaks = seq(0,40000, by = 10000), labels = c('0', '1E4', '2E4', '3E4', '4E4'), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by = 0.2), expand = c(0,0)) +
  scale_color_manual(breaks = c('Uniform', paste0('Cal_',seq(1,5))), values = c('#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02','#1b9e77')) +
  xlab(bquote("Streamflow Difference ("*m^3*"/day)")) +
  ylab('Average Year (2010)\nProbability (%)') +
  theme_few() + 
  theme(legend.position = c(0.85,0.25),
        legend.title = element_blank(),
        legend.background = element_rect(fill=NA, color = NA))

#Plot October Average (2010) CDFs for MAR_ILR
Streamflow_Diff_Vals_MAR_ILR_Avg = exp(seq(log(1E1), log(4E4), length.out = 10000))
RandPar_Diff_MAR_ILR_Avg = data.frame(Streamflow_Diff_mday = Streamflow_Diff_Vals_MAR_ILR_Avg,
                                  Uniform = UniRandPar_Diff_MAR_ILR_cdf[[21]](Streamflow_Diff_Vals_MAR_ILR_Avg),  #use function to evaluate at Streamflow_Diff_Vals_MAR_ILR_Avg
                                  Uniform_norm = ecdf(rnorm(n = 10000, mean = UniRandPar_Diff_Stats$MAR_ILR_mean[21], sd = UniRandPar_Diff_Stats$MAR_ILR_sd[21]))(Streamflow_Diff_Vals_MAR_ILR_Avg),
                                  Cal_1 = CovRandPar_Diff_MAR_ILR_cdf[[21]](Streamflow_Diff_Vals_MAR_ILR_Avg),  #use function to evaluate at Streamflow_Diff_Vals_MAR_ILR_Avg
                                  Cal_1_norm = ecdf(rnorm(n = 10000, mean = CovRandPar_Diff_Stats$MAR_ILR_mean[21], sd = CovRandPar_Diff_Stats$MAR_ILR_sd[21]))(Streamflow_Diff_Vals_MAR_ILR_Avg),
                                  Cal_2 = CovRandPar_Diff_MAR_ILR_cdf[[42]](Streamflow_Diff_Vals_MAR_ILR_Avg),  #use function to evaluate at Streamflow_Diff_Vals_MAR_ILR_Avg
                                  Cal_2_norm = ecdf(rnorm(n = 10000, mean = CovRandPar_Diff_Stats$MAR_ILR_mean[42], sd = CovRandPar_Diff_Stats$MAR_ILR_sd[42]))(Streamflow_Diff_Vals_MAR_ILR_Avg),
                                  Cal_3 = CovRandPar_Diff_MAR_ILR_cdf[[63]](Streamflow_Diff_Vals_MAR_ILR_Avg),  #use function to evaluate at Streamflow_Diff_Vals_MAR_ILR_Avg
                                  Cal_3_norm = ecdf(rnorm(n = 10000, mean = CovRandPar_Diff_Stats$MAR_ILR_mean[63], sd = CovRandPar_Diff_Stats$MAR_ILR_sd[63]))(Streamflow_Diff_Vals_MAR_ILR_Avg),
                                  Cal_4 = CovRandPar_Diff_MAR_ILR_cdf[[84]](Streamflow_Diff_Vals_MAR_ILR_Avg),  #use function to evaluate at Streamflow_Diff_Vals_MAR_ILR_Avg
                                  Cal_4_norm = ecdf(rnorm(n = 10000, mean = CovRandPar_Diff_Stats$MAR_ILR_mean[84], sd = CovRandPar_Diff_Stats$MAR_ILR_sd[84]))(Streamflow_Diff_Vals_MAR_ILR_Avg),
                                  Cal_5 = CovRandPar_Diff_MAR_ILR_cdf[[105]](Streamflow_Diff_Vals_MAR_ILR_Avg),  #use function to evaluate at Streamflow_Diff_Vals_MAR_ILR_Avg
                                  Cal_5_norm = ecdf(rnorm(n = 10000, mean = CovRandPar_Diff_Stats$MAR_ILR_mean[105], sd = CovRandPar_Diff_Stats$MAR_ILR_sd[105]))(Streamflow_Diff_Vals_MAR_ILR_Avg))

RandPar_CDFs_MAR_ILR_Avg_Year_Plot = ggplot(data = NULL) + 
  geom_line(data = RandPar_Diff_MAR_ILR_Avg, aes(x =Streamflow_Diff_mday, y = Uniform, color = 'Uniform'), size = 0.5) +
  geom_line(data = RandPar_Diff_MAR_ILR_Avg, aes(x =Streamflow_Diff_mday, y = Uniform_norm), color = '#1b9e77', size = 0.5, linetype = 'dashed') +
  geom_line(data = RandPar_Diff_MAR_ILR_Avg, aes(x =Streamflow_Diff_mday, y = Cal_1, color = 'Cal_1'), size = 0.5) +
  geom_line(data = RandPar_Diff_MAR_ILR_Avg, aes(x =Streamflow_Diff_mday, y = Cal_1_norm), color = '#d95f02', size = 0.5, linetype = 'dashed') +
  geom_line(data = RandPar_Diff_MAR_ILR_Avg, aes(x =Streamflow_Diff_mday, y = Cal_2, color = 'Cal_2'), size = 0.5) +
  geom_line(data = RandPar_Diff_MAR_ILR_Avg, aes(x =Streamflow_Diff_mday, y = Cal_2_norm),color = '#7570b3', size = 0.5, linetype = 'dashed') +
  geom_line(data = RandPar_Diff_MAR_ILR_Avg, aes(x =Streamflow_Diff_mday, y = Cal_3, color = 'Cal_3'), size = 0.5) +
  geom_line(data = RandPar_Diff_MAR_ILR_Avg, aes(x =Streamflow_Diff_mday, y = Cal_3_norm),color = '#e7298a', size = 0.5, linetype = 'dashed') +
  geom_line(data = RandPar_Diff_MAR_ILR_Avg, aes(x =Streamflow_Diff_mday, y = Cal_4, color = 'Cal_4'), size = 0.5) +
  geom_line(data = RandPar_Diff_MAR_ILR_Avg, aes(x =Streamflow_Diff_mday, y = Cal_4_norm),color = '#66a61e', size = 0.5, linetype = 'dashed') +
  geom_line(data = RandPar_Diff_MAR_ILR_Avg, aes(x =Streamflow_Diff_mday, y = Cal_5, color = 'Cal_5'), size = 0.5) +
  geom_line(data = RandPar_Diff_MAR_ILR_Avg, aes(x =Streamflow_Diff_mday, y = Cal_5_norm),color = '#e6ab02', size = 0.5, linetype = 'dashed') +
  scale_x_continuous(limits = c(0,40000), breaks = seq(0,40000, by = 10000), labels = c('0', '1E4', '2E4', '3E4', '4E4'), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by = 0.2), expand = c(0,0)) +
  scale_color_manual(breaks = c('Uniform', paste0('Cal_',seq(1,5))), values = c('#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02','#1b9e77')) +
  xlab(bquote("Streamflow Difference ("*m^3*"/day)")) +
  ylab('Average Year (2010)\nProbability (%)') +
  theme_few() + 
  theme(legend.position = c(0.85,0.25),
        legend.title = element_blank(),
        legend.background = element_rect(fill=NA, color = NA))

#Plot October Wet (2006) CDFs for MAR
MAR_2006_Locs = seq(16,by=20, length.out = 5)   #Since Oct-1990 has been removed for MAR, there are only 20 years so the indexing for the MAR is different than for the ILR or MAR_ILR scenarios
ECDF_2006_Locs = seq(17,by=21, length.out = 5)
Streamflow_Diff_Vals_MAR_Wet = exp(seq(log(1E1), log(2E4), length.out = 10000))
RandPar_Diff_MAR_Wet = data.frame(Streamflow_Diff_mday = Streamflow_Diff_Vals_MAR_Wet,
                                  Uniform = UniRandPar_Diff_MAR_cdf[[MAR_2010_Locs[1]]](Streamflow_Diff_Vals_MAR_Wet),  #use function to evaluate at Streamflow_Diff_Vals_MAR_Wet
                                  Uniform_norm = ecdf(rnorm(n = 10000, mean = UniRandPar_Diff_Stats$MAR_mean[ECDF_2010_Locs[1]], sd = UniRandPar_Diff_Stats$MAR_sd[ECDF_2010_Locs[1]]))(Streamflow_Diff_Vals_MAR_Wet),
                                  Cal_1 = CovRandPar_Diff_MAR_cdf[[MAR_2010_Locs[1]]](Streamflow_Diff_Vals_MAR_Wet),  #use function to evaluate at Streamflow_Diff_Vals_MAR_Wet
                                  Cal_1_norm = ecdf(rnorm(n = 10000, mean = CovRandPar_Diff_Stats$MAR_mean[ECDF_2010_Locs[1]], sd = CovRandPar_Diff_Stats$MAR_sd[ECDF_2010_Locs[1]]))(Streamflow_Diff_Vals_MAR_Wet),
                                  Cal_2 = CovRandPar_Diff_MAR_cdf[[MAR_2010_Locs[2]]](Streamflow_Diff_Vals_MAR_Wet),  #use function to evaluate at Streamflow_Diff_Vals_MAR_Wet
                                  Cal_2_norm = ecdf(rnorm(n = 10000, mean = CovRandPar_Diff_Stats$MAR_mean[ECDF_2010_Locs[2]], sd = CovRandPar_Diff_Stats$MAR_sd[ECDF_2010_Locs[2]]))(Streamflow_Diff_Vals_MAR_Wet),
                                  Cal_3 = CovRandPar_Diff_MAR_cdf[[MAR_2010_Locs[3]]](Streamflow_Diff_Vals_MAR_Wet),  #use function to evaluate at Streamflow_Diff_Vals_MAR_Wet
                                  Cal_3_norm = ecdf(rnorm(n = 10000, mean = CovRandPar_Diff_Stats$MAR_mean[ECDF_2010_Locs[3]], sd = CovRandPar_Diff_Stats$MAR_sd[ECDF_2010_Locs[3]]))(Streamflow_Diff_Vals_MAR_Wet),
                                  Cal_4 = CovRandPar_Diff_MAR_cdf[[MAR_2010_Locs[4]]](Streamflow_Diff_Vals_MAR_Wet),  #use function to evaluate at Streamflow_Diff_Vals_MAR_Wet
                                  Cal_4_norm = ecdf(rnorm(n = 10000, mean = CovRandPar_Diff_Stats$MAR_mean[ECDF_2010_Locs[4]], sd = CovRandPar_Diff_Stats$MAR_sd[ECDF_2010_Locs[4]]))(Streamflow_Diff_Vals_MAR_Wet),
                                  Cal_5 = CovRandPar_Diff_MAR_cdf[[MAR_2010_Locs[5]]](Streamflow_Diff_Vals_MAR_Wet),  #use function to evaluate at Streamflow_Diff_Vals_MAR_Wet
                                  Cal_5_norm = ecdf(rnorm(n = 10000, mean = CovRandPar_Diff_Stats$MAR_mean[ECDF_2010_Locs[5]], sd = CovRandPar_Diff_Stats$MAR_sd[ECDF_2010_Locs[5]]))(Streamflow_Diff_Vals_MAR_Wet))

RandPar_CDFs_MAR_Wet_Year_Plot = ggplot(data = NULL) + 
  geom_line(data = RandPar_Diff_MAR_Wet, aes(x =Streamflow_Diff_mday, y = Uniform, color = 'Uniform'), size = 0.5) +
  geom_line(data = RandPar_Diff_MAR_Wet, aes(x =Streamflow_Diff_mday, y = Uniform_norm), color = '#1b9e77', size = 0.5, linetype = 'dashed') +
  geom_line(data = RandPar_Diff_MAR_Wet, aes(x =Streamflow_Diff_mday, y = Cal_1, color = 'Cal_1'), size = 0.5) +
  geom_line(data = RandPar_Diff_MAR_Wet, aes(x =Streamflow_Diff_mday, y = Cal_1_norm), color = '#d95f02', size = 0.5, linetype = 'dashed') +
  geom_line(data = RandPar_Diff_MAR_Wet, aes(x =Streamflow_Diff_mday, y = Cal_2, color = 'Cal_2'), size = 0.5) +
  geom_line(data = RandPar_Diff_MAR_Wet, aes(x =Streamflow_Diff_mday, y = Cal_2_norm),color = '#7570b3', size = 0.5, linetype = 'dashed') +
  geom_line(data = RandPar_Diff_MAR_Wet, aes(x =Streamflow_Diff_mday, y = Cal_3, color = 'Cal_3'), size = 0.5) +
  geom_line(data = RandPar_Diff_MAR_Wet, aes(x =Streamflow_Diff_mday, y = Cal_3_norm),color = '#e7298a', size = 0.5, linetype = 'dashed') +
  geom_line(data = RandPar_Diff_MAR_Wet, aes(x =Streamflow_Diff_mday, y = Cal_4, color = 'Cal_4'), size = 0.5) +
  geom_line(data = RandPar_Diff_MAR_Wet, aes(x =Streamflow_Diff_mday, y = Cal_4_norm),color = '#66a61e', size = 0.5, linetype = 'dashed') +
  geom_line(data = RandPar_Diff_MAR_Wet, aes(x =Streamflow_Diff_mday, y = Cal_5, color = 'Cal_5'), size = 0.5) +
  geom_line(data = RandPar_Diff_MAR_Wet, aes(x =Streamflow_Diff_mday, y = Cal_5_norm),color = '#e6ab02', size = 0.5, linetype = 'dashed') +
  scale_x_continuous(limits = c(0,20000), breaks = seq(0,20000, by = 4000), labels = c('0', '4E3', '8E3', '1.2E4', '1.6E4', '2E4'), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by = 0.2), expand = c(0,0)) +
  scale_color_manual(breaks = c('Uniform', paste0('Cal_',seq(1,5))), values = c('#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02','#1b9e77')) +
  xlab(bquote("Streamflow Difference ("*m^3*"/day)")) +
  ylab('Wet Year (2006)\nProbability (%)') +
  theme_few() + 
  theme(legend.position = c(0.85,0.25),
        legend.title = element_blank(),
        legend.background = element_rect(fill=NA, color = NA))

#Plot October Wet (2006) CDFs for ILR
Streamflow_Diff_Vals_ILR_Wet = exp(seq(log(1E1), log(4E4), length.out = 10000))
RandPar_Diff_ILR_Wet = data.frame(Streamflow_Diff_mday = Streamflow_Diff_Vals_ILR_Wet,
                                  Uniform = UniRandPar_Diff_ILR_cdf[[21]](Streamflow_Diff_Vals_ILR_Wet),  #use function to evaluate at Streamflow_Diff_Vals_ILR_Wet
                                  Uniform_norm = ecdf(rnorm(n = 10000, mean = UniRandPar_Diff_Stats$ILR_mean[21], sd = UniRandPar_Diff_Stats$ILR_sd[21]))(Streamflow_Diff_Vals_ILR_Wet),
                                  Cal_1 = CovRandPar_Diff_ILR_cdf[[21]](Streamflow_Diff_Vals_ILR_Wet),  #use function to evaluate at Streamflow_Diff_Vals_ILR_Wet
                                  Cal_1_norm = ecdf(rnorm(n = 10000, mean = CovRandPar_Diff_Stats$ILR_mean[21], sd = CovRandPar_Diff_Stats$ILR_sd[21]))(Streamflow_Diff_Vals_ILR_Wet),
                                  Cal_2 = CovRandPar_Diff_ILR_cdf[[42]](Streamflow_Diff_Vals_ILR_Wet),  #use function to evaluate at Streamflow_Diff_Vals_ILR_Wet
                                  Cal_2_norm = ecdf(rnorm(n = 10000, mean = CovRandPar_Diff_Stats$ILR_mean[42], sd = CovRandPar_Diff_Stats$ILR_sd[42]))(Streamflow_Diff_Vals_ILR_Wet),
                                  Cal_3 = CovRandPar_Diff_ILR_cdf[[63]](Streamflow_Diff_Vals_ILR_Wet),  #use function to evaluate at Streamflow_Diff_Vals_ILR_Wet
                                  Cal_3_norm = ecdf(rnorm(n = 10000, mean = CovRandPar_Diff_Stats$ILR_mean[63], sd = CovRandPar_Diff_Stats$ILR_sd[63]))(Streamflow_Diff_Vals_ILR_Wet),
                                  Cal_4 = CovRandPar_Diff_ILR_cdf[[84]](Streamflow_Diff_Vals_ILR_Wet),  #use function to evaluate at Streamflow_Diff_Vals_ILR_Wet
                                  Cal_4_norm = ecdf(rnorm(n = 10000, mean = CovRandPar_Diff_Stats$ILR_mean[84], sd = CovRandPar_Diff_Stats$ILR_sd[84]))(Streamflow_Diff_Vals_ILR_Wet),
                                  Cal_5 = CovRandPar_Diff_ILR_cdf[[105]](Streamflow_Diff_Vals_ILR_Wet),  #use function to evaluate at Streamflow_Diff_Vals_ILR_Wet
                                  Cal_5_norm = ecdf(rnorm(n = 10000, mean = CovRandPar_Diff_Stats$ILR_mean[105], sd = CovRandPar_Diff_Stats$ILR_sd[105]))(Streamflow_Diff_Vals_ILR_Wet))

RandPar_CDFs_ILR_Wet_Year_Plot = ggplot(data = NULL) + 
  geom_line(data = RandPar_Diff_ILR_Wet, aes(x =Streamflow_Diff_mday, y = Uniform, color = 'Uniform'), size = 0.5) +
  geom_line(data = RandPar_Diff_ILR_Wet, aes(x =Streamflow_Diff_mday, y = Uniform_norm), color = '#1b9e77', size = 0.5, linetype = 'dashed') +
  geom_line(data = RandPar_Diff_ILR_Wet, aes(x =Streamflow_Diff_mday, y = Cal_1, color = 'Cal_1'), size = 0.5) +
  geom_line(data = RandPar_Diff_ILR_Wet, aes(x =Streamflow_Diff_mday, y = Cal_1_norm), color = '#d95f02', size = 0.5, linetype = 'dashed') +
  geom_line(data = RandPar_Diff_ILR_Wet, aes(x =Streamflow_Diff_mday, y = Cal_2, color = 'Cal_2'), size = 0.5) +
  geom_line(data = RandPar_Diff_ILR_Wet, aes(x =Streamflow_Diff_mday, y = Cal_2_norm),color = '#7570b3', size = 0.5, linetype = 'dashed') +
  geom_line(data = RandPar_Diff_ILR_Wet, aes(x =Streamflow_Diff_mday, y = Cal_3, color = 'Cal_3'), size = 0.5) +
  geom_line(data = RandPar_Diff_ILR_Wet, aes(x =Streamflow_Diff_mday, y = Cal_3_norm),color = '#e7298a', size = 0.5, linetype = 'dashed') +
  geom_line(data = RandPar_Diff_ILR_Wet, aes(x =Streamflow_Diff_mday, y = Cal_4, color = 'Cal_4'), size = 0.5) +
  geom_line(data = RandPar_Diff_ILR_Wet, aes(x =Streamflow_Diff_mday, y = Cal_4_norm),color = '#66a61e', size = 0.5, linetype = 'dashed') +
  geom_line(data = RandPar_Diff_ILR_Wet, aes(x =Streamflow_Diff_mday, y = Cal_5, color = 'Cal_5'), size = 0.5) +
  geom_line(data = RandPar_Diff_ILR_Wet, aes(x =Streamflow_Diff_mday, y = Cal_5_norm),color = '#e6ab02', size = 0.5, linetype = 'dashed') +
  scale_x_continuous(limits = c(0,40000), breaks = seq(0,40000, by = 10000), labels = c('0', '1E4', '2E4', '3E4', '4E4'), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by = 0.2), expand = c(0,0)) +
  scale_color_manual(breaks = c('Uniform', paste0('Cal_',seq(1,5))), values = c('#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02','#1b9e77')) +
  xlab(bquote("Streamflow Difference ("*m^3*"/day)")) +
  ylab('Wet Year (2006)\nProbability (%)') +
  theme_few() + 
  theme(legend.position = c(0.85,0.25),
        legend.title = element_blank(),
        legend.background = element_rect(fill=NA, color = NA))

#Plot October Wet (2006) CDFs for MAR_ILR
Streamflow_Diff_Vals_MAR_ILR_Wet = exp(seq(log(1E1), log(5E4), length.out = 10000))
RandPar_Diff_MAR_ILR_Wet = data.frame(Streamflow_Diff_mday = Streamflow_Diff_Vals_MAR_ILR_Wet,
                                  Uniform = UniRandPar_Diff_MAR_ILR_cdf[[21]](Streamflow_Diff_Vals_MAR_ILR_Wet),  #use function to evaluate at Streamflow_Diff_Vals_MAR_ILR_Wet
                                  Uniform_norm = ecdf(rnorm(n = 10000, mean = UniRandPar_Diff_Stats$MAR_ILR_mean[21], sd = UniRandPar_Diff_Stats$MAR_ILR_sd[21]))(Streamflow_Diff_Vals_MAR_ILR_Wet),
                                  Cal_1 = CovRandPar_Diff_MAR_ILR_cdf[[21]](Streamflow_Diff_Vals_MAR_ILR_Wet),  #use function to evaluate at Streamflow_Diff_Vals_MAR_ILR_Wet
                                  Cal_1_norm = ecdf(rnorm(n = 10000, mean = CovRandPar_Diff_Stats$MAR_ILR_mean[21], sd = CovRandPar_Diff_Stats$MAR_ILR_sd[21]))(Streamflow_Diff_Vals_MAR_ILR_Wet),
                                  Cal_2 = CovRandPar_Diff_MAR_ILR_cdf[[42]](Streamflow_Diff_Vals_MAR_ILR_Wet),  #use function to evaluate at Streamflow_Diff_Vals_MAR_ILR_Wet
                                  Cal_2_norm = ecdf(rnorm(n = 10000, mean = CovRandPar_Diff_Stats$MAR_ILR_mean[42], sd = CovRandPar_Diff_Stats$MAR_ILR_sd[42]))(Streamflow_Diff_Vals_MAR_ILR_Wet),
                                  Cal_3 = CovRandPar_Diff_MAR_ILR_cdf[[63]](Streamflow_Diff_Vals_MAR_ILR_Wet),  #use function to evaluate at Streamflow_Diff_Vals_MAR_ILR_Wet
                                  Cal_3_norm = ecdf(rnorm(n = 10000, mean = CovRandPar_Diff_Stats$MAR_ILR_mean[63], sd = CovRandPar_Diff_Stats$MAR_ILR_sd[63]))(Streamflow_Diff_Vals_MAR_ILR_Wet),
                                  Cal_4 = CovRandPar_Diff_MAR_ILR_cdf[[84]](Streamflow_Diff_Vals_MAR_ILR_Wet),  #use function to evaluate at Streamflow_Diff_Vals_MAR_ILR_Wet
                                  Cal_4_norm = ecdf(rnorm(n = 10000, mean = CovRandPar_Diff_Stats$MAR_ILR_mean[84], sd = CovRandPar_Diff_Stats$MAR_ILR_sd[84]))(Streamflow_Diff_Vals_MAR_ILR_Wet),
                                  Cal_5 = CovRandPar_Diff_MAR_ILR_cdf[[105]](Streamflow_Diff_Vals_MAR_ILR_Wet),  #use function to evaluate at Streamflow_Diff_Vals_MAR_ILR_Wet
                                  Cal_5_norm = ecdf(rnorm(n = 10000, mean = CovRandPar_Diff_Stats$MAR_ILR_mean[105], sd = CovRandPar_Diff_Stats$MAR_ILR_sd[105]))(Streamflow_Diff_Vals_MAR_ILR_Wet))

RandPar_CDFs_MAR_ILR_Wet_Year_Plot = ggplot(data = NULL) + 
  geom_line(data = RandPar_Diff_MAR_ILR_Wet, aes(x =Streamflow_Diff_mday, y = Uniform, color = 'Uniform'), size = 0.5) +
  geom_line(data = RandPar_Diff_MAR_ILR_Wet, aes(x =Streamflow_Diff_mday, y = Uniform_norm), color = '#1b9e77', size = 0.5, linetype = 'dashed') +
  geom_line(data = RandPar_Diff_MAR_ILR_Wet, aes(x =Streamflow_Diff_mday, y = Cal_1, color = 'Cal_1'), size = 0.5) +
  geom_line(data = RandPar_Diff_MAR_ILR_Wet, aes(x =Streamflow_Diff_mday, y = Cal_1_norm), color = '#d95f02', size = 0.5, linetype = 'dashed') +
  geom_line(data = RandPar_Diff_MAR_ILR_Wet, aes(x =Streamflow_Diff_mday, y = Cal_2, color = 'Cal_2'), size = 0.5) +
  geom_line(data = RandPar_Diff_MAR_ILR_Wet, aes(x =Streamflow_Diff_mday, y = Cal_2_norm),color = '#7570b3', size = 0.5, linetype = 'dashed') +
  geom_line(data = RandPar_Diff_MAR_ILR_Wet, aes(x =Streamflow_Diff_mday, y = Cal_3, color = 'Cal_3'), size = 0.5) +
  geom_line(data = RandPar_Diff_MAR_ILR_Wet, aes(x =Streamflow_Diff_mday, y = Cal_3_norm),color = '#e7298a', size = 0.5, linetype = 'dashed') +
  geom_line(data = RandPar_Diff_MAR_ILR_Wet, aes(x =Streamflow_Diff_mday, y = Cal_4, color = 'Cal_4'), size = 0.5) +
  geom_line(data = RandPar_Diff_MAR_ILR_Wet, aes(x =Streamflow_Diff_mday, y = Cal_4_norm),color = '#66a61e', size = 0.5, linetype = 'dashed') +
  geom_line(data = RandPar_Diff_MAR_ILR_Wet, aes(x =Streamflow_Diff_mday, y = Cal_5, color = 'Cal_5'), size = 0.5) +
  geom_line(data = RandPar_Diff_MAR_ILR_Wet, aes(x =Streamflow_Diff_mday, y = Cal_5_norm),color = '#e6ab02', size = 0.5, linetype = 'dashed') +
  scale_x_continuous(limits = c(0,40000), breaks = seq(0,40000, by = 10000), labels = c('0', '1E4', '2E4', '3E4', '4E4'), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by = 0.2), expand = c(0,0)) +
  scale_color_manual(breaks = c('Uniform', paste0('Cal_',seq(1,5))), values = c('#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02','#1b9e77')) +
  xlab(bquote("Streamflow Difference ("*m^3*"/day)")) +
  ylab('Wet Year (2006)\nProbability (%)') +
  theme_few() + 
  theme(legend.position = c(0.85,0.25),
        legend.title = element_blank(),
        legend.background = element_rect(fill=NA, color = NA))


# Cumulative Density Plot -------------------------------------------------
fig_name = 'Cumulative Density Plots'                                                                           
if (fig_format == 'jpg'){                                                                                     
  jpeg(paste0(out_dir,fig_name,'.jpg'), width = fig_width, height = fig_height, units = fig_units,  res = 600)
} else if (fig_format == 'png'){                                                                              
  png(paste0(out_dir,fig_name,'.png'), width = fig_width, height = fig_height, units = fig_units,  res = 600) 
} else if (fig_format == 'pdf'){                                                                              
  pdf(paste0(out_dir,fig_name,'.pdf'), width = 7, height = 5)                                
}                                                                                                             
grid.newpage()                                                                                                
pushViewport(viewport(layout = grid.layout(3,3)))                                                             
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)                                   
print(RandPar_CDFs_MAR_Dry_Year_Plot +
        theme(axis.title = element_blank(),
              axis.text = element_text(size = 7),
              legend.position = 'none'), vp = vplayout(1,1))   
print(RandPar_CDFs_ILR_Dry_Year_Plot +
        theme(axis.title = element_blank(),
              axis.text.y = element_blank(),
              axis.text.x = element_text(size = 7),
              legend.position = 'none'), vp = vplayout(1,2))   
print(RandPar_CDFs_MAR_ILR_Dry_Year_Plot +
        theme(axis.title = element_blank(),
              axis.text.y = element_blank(),
              axis.text.x = element_text(size = 7),
              legend.position = 'none'), vp = vplayout(1,3))
print(RandPar_CDFs_MAR_Avg_Year_Plot + 
        theme(axis.title = element_blank(),
              axis.text = element_text(size = 7),
              legend.position = 'none'), vp = vplayout(2,1))   
print(RandPar_CDFs_ILR_Avg_Year_Plot + 
        theme(axis.title = element_blank(),
              axis.text.y = element_blank(),
              axis.text.x = element_text(size = 7),
              legend.position = c(0.8,0.4),
              legend.text = element_text(size = 8),
              legend.key.height = unit(0.1, 'cm')), vp = vplayout(2,2))   
print(RandPar_CDFs_MAR_ILR_Avg_Year_Plot + 
        theme(axis.title = element_blank(),
              axis.text.y = element_blank(),
              axis.text.x = element_text(size = 7),
              legend.position = 'none'), vp = vplayout(2,3))   
print(RandPar_CDFs_MAR_Wet_Year_Plot + 
        theme(axis.title = element_blank(),
              legend.position = 'none',
              axis.text = element_text(size = 7)), vp = vplayout(3,1))   
print(RandPar_CDFs_ILR_Wet_Year_Plot + 
        theme(axis.title = element_blank(),
              legend.position = 'none',
              axis.text.x = element_text(size = 7),
              axis.text.y = element_blank()), vp = vplayout(3,2))   
print(RandPar_CDFs_MAR_ILR_Wet_Year_Plot + 
        theme(axis.title = element_blank(),
              legend.position = 'none',
              axis.text.x = element_text(size = 7),
              axis.text.y = element_blank()), vp = vplayout(3,3)) 
graphics.off()

# Streamflow Plots --------------------------------------------------------
if(UniRandPar==TRUE){
  for (j in 1:5){
    eval(parse(text = paste0('mean_m3day = apply(subset(UniRandPar_Preds_Basecase,Pred_Loc==',j,')[,seq(-1,-5)],1,mean)')))
    eval(parse(text = paste0('sd_m3day = apply(subset(UniRandPar_Preds_Basecase,Pred_Loc==',j,')[,seq(-1,-5)],1,sd)')))
    eval(parse(text = paste0('max_m3day = apply(subset(UniRandPar_Preds_Basecase,Pred_Loc==',j,')[,seq(-1,-5)],1,max)')))
    eval(parse(text = paste0('min_m3day = apply(subset(UniRandPar_Preds_Basecase,Pred_Loc==',j,')[,seq(-1,-5)],1,min)')))
    eval(parse(text = paste0('summary = cbind(subset(UniRandPar_Preds_Basecase,Pred_Loc==',j,')[,seq(1,5)], mean_m3day, sd_m3day, max_m3day, min_m3day)')))
    summary$lower95 = summary$mean_m3day-2*summary$sd_m3day
    summary$upper95 = summary$mean_m3day+2*summary$sd_m3day
    #summary$lower95[which(summary$lower95<0)] = 0
    for(m in 1:length(crit_months)){
      temp_data = subset(summary,Month==crit_months[m])
      Plot = ggplot(data = temp_data, aes(x = Date)) + 
        geom_ribbon(aes(ymin = lower95_m3day, ymax = upper95_m3day), alpha = 0.5, fill = 'orange') + 
        geom_line(aes(y = median_m3day),size = line_size) +
        geom_point(aes(y = median_m3day),size = point_size) +
        ylab(bquote("Streamflow ("*m^3*"/day)")) +
        #scale_x_continuous(limits = c(1991,2011), breaks = seq(1991,2011,by=2), expand = c(0,0)) +
        scale_y_continuous(limits = c(0,200), breaks = seq(0,200,by=50),expand = c(0,0)) +
        theme_few() +
        theme(axis.title.x = element_blank(),
              axis.text.x = element_text(angle = 45, hjust = 1),
              axis.title.y = element_text(size = axis_title_size),
              axis.text = element_text(size = axis_text_size))
      eval(parse(text = paste0(crit_months[m],'_uni_Streamflow_loc_',j,'_plot = Plot')))
    }
      #Violin plots grouped by month
      eval(parse(text = paste0('data_melt = melt(subset(UniRandPar_Preds_Basecase,Pred_Loc==',j,'),id.vars = names(UniRandPar_Diff_MAR_clean[seq(1,5)]))')))
      data_melt$Month = factor(data_melt$Month, levels = format(seq(as.Date('1990-01-01'), as.Date('1990-12-01'), by = 'month'),'%b'))
      data_melt = data_melt[order(data_melt$Month),]
      ViolinPlot = ggplot(data = subset(data_melt,Month==crit_months), aes(x = factor(Month), y = value, group = Month, fill = Month)) + 
        geom_hline(yintercept = 0) +
        geom_violin(trim = TRUE,draw_quantiles = c(0.5)) +
        ylab(bquote("Streamflow ("*m^3*"/day)")) +
        scale_fill_brewer(palette = 'Set2') +
        scale_y_continuous(limits = c(0,200), breaks = seq(0,200,by = 50), expand = c(0,0)) +
        scale_x_discrete(expand = c(0,0)) +
        theme_few() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_text(size = axis_title_size),
              axis.text = element_text(size = axis_text_size),
              axis.text.x = element_text(angle = 45, hjust = 1),
              legend.direction = 'horizontal',
              legend.position = c(0.5,0.9),
              legend.title = element_blank(),
              legend.background = element_rect(fill = NA))
      eval(parse(text = paste0('Aug_Sep_Oct_basecase_uni_loc_',j,'_plot = ViolinPlot')))
  } 
}
if(CovRandPar==TRUE){
    for (j in 1:5){
      eval(parse(text = paste0("mean_m3day = apply(subset(CovRandPar_Preds_Basecase,Pred_Loc== ",j,")[,seq(-1,-6)],1,mean)")))
      eval(parse(text = paste0("sd_m3day = apply(subset(CovRandPar_Preds_Basecase,Pred_Loc== ",j,")[,seq(-1,-6)],1,sd)")))
      eval(parse(text = paste0("max_m3day = apply(subset(CovRandPar_Preds_Basecase,Pred_Loc== ",j,")[,seq(-1,-6)],1,max)")))
      eval(parse(text = paste0("min_m3day = apply(subset(CovRandPar_Preds_Basecase,Pred_Loc== ",j,")[,seq(-1,-6)],1,min)")))
      eval(parse(text = paste0("summary = cbind(subset(CovRandPar_Preds_Basecase,Pred_Loc== ",j,")[,seq(1,6)], mean_m3day, sd_m3day, max_m3day, min_m3day)")))
      summary$lower95 = summary$mean_m3day-2*summary$sd_m3day
      summary$upper95 = summary$mean_m3day+2*summary$sd_m3day  
      #summary$lower95[which(summary$lower95<0)] = 0
      for(m in 1:length(crit_months)){
        temp_data = subset(summary,Month==crit_months[m])
        Plot = ggplot(data = temp_data, aes(x = Date)) + 
          geom_ribbon(aes(ymin = lower95_m3day, ymax = upper95_m3day, fill = Calibration), alpha = 0.25) + 
          geom_line(aes(y = median_m3day, group = Calibration, color = Calibration),size = line_size) +
          geom_point(aes(y = median_m3day, shape = Calibration),size = point_size) +
          ylab(bquote("Streamflow ("*m^3*"/day)")) +
          scale_color_manual(values = c('#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02')) +
          scale_fill_manual(values = c('#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02')) +
          ##scale_x_continuous(limits = c(1991,2011), breaks = seq(1991,2011,by=2), expand = c(0,0)) +
          scale_y_continuous(limits = c(0,200), breaks = seq(0,200,by=50),expand = c(0,0)) +
          theme_few() +
          theme(axis.title.x = element_blank(),
                axis.title.y = element_text(size = axis_title_size),
                axis.text = element_text(size = axis_text_size),
                axis.text.x = element_text(angle = 45, hjust = 1),
                legend.direction = 'horizontal',
                legend.position = c(0.5,0.9),
                legend.title = element_blank(),
                legend.background = element_rect(fill = NA))
        eval(parse(text = paste0(crit_months[m],'_cov_Streamflow_loc_',j,'_plot = Plot')))
      }
      #Violin plots grouped by month
      eval(parse(text = paste0('data_melt = melt(subset(CovRandPar_Preds_Basecase,Pred_Loc==',j,'),id.vars = names(CovRandPar_Diff_MAR_clean[seq(1,6)]))')))
      data_melt$Month = factor(data_melt$Month, levels = format(seq(as.Date('1990-01-01'), as.Date('1990-12-01'), by = 'month'),'%b'))
      data_melt = data_melt[order(data_melt$Month),]
      ViolinPlot = ggplot(data = subset(data_melt,Month==crit_months), aes(x = factor(Month), y = value, fill = Calibration)) + 
        geom_hline(yintercept = 0) +
        geom_violin(trim = TRUE,draw_quantiles = c(0.5), position = position_dodge(width = 0.8)) +
        ylab(bquote("Streamflow ("*m^3*"/day)")) +
        scale_fill_brewer(palette = 'Set1') +
        scale_y_continuous(limits = c(0,200), breaks = seq(0,200,by = 50), expand = c(0,0)) +
        scale_x_discrete(expand = c(0,0)) +
        theme_few() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_text(size = axis_title_size),
              axis.text = element_text(size = axis_text_size),
              axis.text.x = element_text(angle = 45, hjust = 1),
              legend.direction = 'horizontal',
              legend.position = c(0.25,0.9),
              legend.title = element_blank(),
              legend.background = element_rect(fill = NA))
      eval(parse(text = paste0('Aug_Sep_Oct_basecase_cov_loc_',j,'_plot = ViolinPlot')))
    }
}
# MAR Plots ---------------------------------------------------------------
if (MAR_Comp == TRUE){
  if (UniRandPar == TRUE){
    #Normalized Convergence Plots - Prediciton Location 1
    UniRandPar_Diff_MAR_convergence_norm_melt = reshape(UniRandPar_Diff_MAR_convergence_norm, varying = names(UniRandPar_Diff_MAR_convergence_norm[,seq(-1,-5)]),
                                                        v.names = 'Cumulative Mean',
                                                        timevar = 'Parameter Set Number',
                                                        direction = 'long') %>% subset(Month == 'Oct' & Pred_Loc == 1)  # reshape dataframe from plotting
    UniRandPar_Diff_MAR_convergence_norm_melt = left_join(UniRandPar_Diff_MAR_convergence_norm_melt, Water_Yr_Class_1991_2011, by = 'Water_Year')
    UniRandPar_Diff_MAR_convergence_norm_melt$Water.Year.Type = factor(UniRandPar_Diff_MAR_convergence_norm_melt$Water.Year.Type, 
                                                                  levels = c('Wet', 'Above Normal', 'Below Normal', 'Dry', 'Critical' ))
    UniRandPar_Diff_MAR_convergence_norm_melt = UniRandPar_Diff_MAR_convergence_norm_melt[order(UniRandPar_Diff_MAR_convergence_norm_melt$Water.Year.Type),]
    
    
    UniRandPar_MAR_Convergence_Plot = ggplot(UniRandPar_Diff_MAR_convergence_norm_melt, aes(x = `Parameter Set Number`, y = `Cumulative Mean`)) + 
      geom_hline(yintercept = 1) +
      geom_point(aes(color = Water.Year.Type), size = point_size) +
      scale_color_manual(values = c('navyblue', 'skyblue', 'tan1', 'tomato', 'red4')) +
      xlab('n') +
      ylab('MAR\nNormalized Cumulative Mean (-)') +
      ggtitle('MAR Streamflow Difference Convergence\nPrediciton Location 1: October') +
      #scale_x_continuous(limits = c(1,500), breaks = c(1,seq(100,500, by = 100)), expand = c(0,0)) +
      scale_y_continuous(limits = c(0,5), breaks = seq(0,5), expand = c(0,0)) +
      #scale_y_log10(limits = c(0.1,100), expand = c(0,0)) +
      scale_x_log10(limits = c(1,1000), expand = c(0.01,0.01)) +
      theme_few() +
      conv_theme +
      annotation_logticks(size = 0.25, sides = 'b')  

    #Monthly Series
    for (j in 1:5){
      eval(parse(text = paste0('quantiles = apply(subset(UniRandPar_Diff_MAR_clean,Pred_Loc==',j,')[,seq(-1,-5)],1,FUN = function(x) as.data.frame(t(quantile(x, probs = c(0.025, 0.5, 0.975), na.rm = T))))')))
      eval(parse(text = paste0('summary = cbind(subset(UniRandPar_Diff_MAR_clean,Pred_Loc==',j,')[,seq(1,5)], 
                               unlist(lapply(quantiles, `[[`, 1)), 
                               unlist(lapply(quantiles, `[[`, 2)), 
                               unlist(lapply(quantiles, `[[`, 3)))')))
      names(summary) = c(names(UniRandPar_Diff_MAR_clean[seq(1,5)]), 'lower95_m3day', 'median_m3day', 'upper95_m3day')
      for(m in 1:length(crit_months)){
        temp_data = subset(summary,Month==crit_months[m])
        Plot = ggplot(data = temp_data, aes(x = Date)) + 
          geom_hline(yintercept = 0) +
          geom_ribbon(aes(ymin = lower95_m3day, ymax = upper95_m3day), alpha = 0.75, fill = 'dodgerblue') + 
          geom_line(aes(y = median_m3day),size = line_size) +
          geom_point(aes(y = median_m3day),size = point_size) +
          geom_rect(aes(xmin = as.Date('1990-10-01'), xmax = as.Date('1992-10-01'), ymin = -1E4 , ymax = -7.5E3), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('1993-10-01'), xmax = as.Date('1994-10-01'), ymin = -1E4 , ymax = -7.5E3), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('1994-10-01'), xmax = as.Date('1999-10-01'), ymin = -1E4 , ymax = -7.5E3), fill = 'skyblue', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2000-10-01'), xmax = as.Date('2002-10-01'), ymin = -1E4 , ymax = -7.5E3), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2005-10-01'), xmax = as.Date('2006-10-01'), ymin = -1E4 , ymax = -7.5E3), fill = 'skyblue', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2006-10-01'), xmax = as.Date('2009-10-01'), ymin = -1E4 , ymax = -7.5E3), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2010-10-01'), xmax = as.Date('2011-10-01'), ymin = -1E4 , ymax = -7.5E3), fill = 'skyblue', color = 'black', size = 0.2) +
          ylab(bquote("Streamflow Difference ("*m^3*"/day)")) +
          scale_color_manual(values = c('black')) +
          scale_x_date(limits = c(as.Date('1990-10-01'),as.Date('2011-10-01')),
                       breaks = seq(as.Date('1990-10-01'),as.Date('2011-10-01'), by = '2 years'),
                       date_labels = '%Y', expand = c(0.01,0.01)) +
          scale_y_continuous(limits = c(-1E4,7E4), breaks = seq(-1E4,7E4,by=1E4), labels = paste0(seq(-1,7),'E4'),expand = c(0,0)) +
          theme_few() +
          theme(axis.title.x = element_blank(),
                axis.text.x = element_text(angle = 45, hjust = 1),
                axis.text = element_text(size = axis_text_size),
                axis.title.y = element_text(size = axis_title_size))
        eval(parse(text = paste0(crit_months[m],'_MAR_uni_loc_',j,'_plot = Plot')))
      }
    }
  }
  if(CovRandPar==TRUE){
    #Normalized Convergence Plots - Prediciton Location 1
    CovRandPar_Diff_MAR_convergence_norm_melt = subset(CovRandPar_Diff_MAR_convergence_norm_melt, Month == 'Oct' & Pred_Loc == 1)
    CovRandPar_Diff_MAR_convergence_norm_melt = left_join(CovRandPar_Diff_MAR_convergence_norm_melt, Water_Yr_Class_1991_2011, by = 'Water_Year')
    CovRandPar_Diff_MAR_convergence_norm_melt$Water.Year.Type = factor(CovRandPar_Diff_MAR_convergence_norm_melt$Water.Year.Type, 
                                                                       levels = c('Wet', 'Above Normal', 'Below Normal', 'Dry', 'Critical' ))
    CovRandPar_Diff_MAR_convergence_norm_melt = CovRandPar_Diff_MAR_convergence_norm_melt[order(CovRandPar_Diff_MAR_convergence_norm_melt$Water.Year.Type),]

    CovRandPar_MAR_Convergence_Plot = ggplot(CovRandPar_Diff_MAR_convergence_norm_melt, aes(x = as.numeric(variable), y = value)) + 
      geom_hline(yintercept = 1) +
      geom_point(aes(color = Water.Year.Type, shape = Calibration), size = point_size ) +
      scale_color_manual(values = c('navyblue', 'skyblue', 'tan1', 'tomato', 'red4')) +
      xlab('n') +
      ylab('MAR\nNormalized Cumulative Mean (-)') +
      ggtitle('ILR Streamflow Difference Convergence\nPrediciton Location 1: October') +
      #scale_x_continuous(limits = c(1,100), breaks = c(1,seq(10,100, by = 10)), expand = c(0,0)) +
      scale_y_continuous(limits = c(0,5), breaks = seq(0,5), expand = c(0,0)) +
      #scale_y_log10(limits = c(0.1,100), expand = c(0,0)) +
      scale_x_log10(limits = c(1,1000), expand = c(0.01,0.01)) +
      theme_few() +
      conv_theme +
      annotation_logticks(size = 0.25, sides = 'b')  
    
    #Monthly Series
    for (j in 1:5){
      eval(parse(text = paste0('quantiles = apply(subset(CovRandPar_Diff_MAR_clean,Pred_Loc==',j,')[,seq(-1,-6)],1,FUN = function(x) as.data.frame(t(quantile(x, probs = c(0.025, 0.5, 0.975), na.rm = T))))')))
      eval(parse(text = paste0('summary = cbind(subset(CovRandPar_Diff_MAR_clean,Pred_Loc==',j,')[,seq(1,6)], 
                               unlist(lapply(quantiles, `[[`, 1)), 
                               unlist(lapply(quantiles, `[[`, 2)), 
                               unlist(lapply(quantiles, `[[`, 3)))')))
      names(summary) = c(names(CovRandPar_Diff_MAR_clean[seq(1,6)]), 'lower95_m3day', 'median_m3day', 'upper95_m3day')
      for(m in 1:length(crit_months)){
        temp_data = subset(summary,Month==crit_months[m])
        Plot = ggplot(data = temp_data, aes(x = Date)) + 
          geom_hline(yintercept = 0) +
          geom_ribbon(aes(ymin = lower95_m3day, ymax = upper95_m3day, fill = Calibration), alpha = 0.25) +
          geom_line(aes(y = median_m3day, group = Calibration, color = Calibration),size = line_size) +
          geom_point(aes(y = median_m3day, shape = Calibration),size = point_size) +
          geom_rect(aes(xmin = as.Date('1990-10-01'), xmax = as.Date('1992-10-01'), ymin = -1E4 , ymax = -7.5E3), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('1993-10-01'), xmax = as.Date('1994-10-01'), ymin = -1E4 , ymax = -7.5E3), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('1994-10-01'), xmax = as.Date('1999-10-01'), ymin = -1E4 , ymax = -7.5E3), fill = 'skyblue', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2000-10-01'), xmax = as.Date('2002-10-01'), ymin = -1E4 , ymax = -7.5E3), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2005-10-01'), xmax = as.Date('2006-10-01'), ymin = -1E4 , ymax = -7.5E3), fill = 'skyblue', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2006-10-01'), xmax = as.Date('2009-10-01'), ymin = -1E4 , ymax = -7.5E3), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2010-10-01'), xmax = as.Date('2011-10-01'), ymin = -1E4 , ymax = -7.5E3), fill = 'skyblue', color = 'black', size = 0.2) +
          ylab(bquote("Streamflow Difference ("*m^3*"/day)")) +
          scale_color_manual(values = c('#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02')) +
          scale_fill_manual(values = c('#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02')) +
          scale_x_date(limits = c(as.Date('1990-10-01'),as.Date('2011-10-01')),
                       breaks = seq(as.Date('1990-10-01'),as.Date('2011-10-01'), by = '2 years'),
                       date_labels = '%Y', expand = c(0.01,0.01)) +
          scale_y_continuous(limits = c(-1E4,7E4), breaks = seq(-1E4,7E4,by=1E4), labels = paste0(seq(-1,7),'E4'),expand = c(0,0)) +
          theme_few() +
          theme(axis.title.x = element_blank(),
                axis.title.y = element_text(size = axis_title_size),
                axis.text.x = element_text(angle = 45, hjust = 1),
                axis.text = element_text(size = axis_text_size),
                legend.direction = 'horizontal',
                legend.position = c(0.5,0.9),
                legend.title = element_blank(),
                legend.background = element_rect(fill = NA))
        eval(parse(text = paste0(crit_months[m],'_MAR_cov_loc_',j,'_plot = Plot')))
      }
    }
  }
  #Violin plots (Uniform + Normal)
  if (UniRandPar == TRUE & CovRandPar == TRUE ){
    for(j in 1:5){
      eval(parse(text = paste0('temp1 = melt(subset(UniRandPar_Diff_MAR_clean,Pred_Loc==',j,'),id.vars = names(UniRandPar_Diff_MAR_clean[seq(1,5)]))')))
      temp1$Month = factor(temp1$Month, levels = format(seq(as.Date('1990-01-01'), as.Date('1990-12-01'), by = 'month'),'%b'))
      temp1 = temp1[order(temp1$Month),]
      temp1$Calibration = 'Uncorrelated'
      
      eval(parse(text = paste0('temp2 = melt(subset(CovRandPar_Diff_MAR_clean,Pred_Loc==',j,'),id.vars = names(CovRandPar_Diff_MAR_clean[seq(1,6)]))')))
      temp2$Month = factor(temp2$Month, levels = format(seq(as.Date('1990-01-01'), as.Date('1990-12-01'), by = 'month'),'%b'))
      temp2 = temp2[order(temp2$Month),]
      
      Uni_Cov_Rand_Diff_MAR = rbind(temp1, temp2)
      Uni_Cov_Rand_Diff_MAR$Calibration = factor(Uni_Cov_Rand_Diff_MAR$Calibration, levels= c('Uncorrelated',paste0('Cal_',seq(1,5))))
      Uni_Cov_Rand_Diff_MAR = Uni_Cov_Rand_Diff_MAR[order(Uni_Cov_Rand_Diff_MAR$Calibration),]
      
      ViolinPlot_Uni_Cov = ggplot(data = subset(Uni_Cov_Rand_Diff_MAR,Month==crit_months), aes(x = factor(Month), y = value, fill = Calibration)) +
        geom_hline(yintercept = 0) +
        geom_violin(position = position_dodge(width = 0.9),draw_quantiles = c(0.5), size = 0.25) +
        ylab(bquote("Streamflow Difference ("*m^3*"/day)")) +
        scale_fill_manual(breaks = c('Uncorrelated', paste0('Cal_',seq(1,5))), values = c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02'), guide = guide_legend(nrow = 1)) +
        scale_y_continuous(limits = c(-4E5,7E5), breaks = seq(-4E4,1E5,by=2E4), labels = c(paste0(seq(-4,8,by = 2),'E4'),'1E5'), expand = c(0,0)) +
        scale_x_discrete(expand = c(0,0)) +
        coord_cartesian(ylim = c(-2E4,1E5)) +
        theme_few() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_text(size = axis_title_size),
              axis.text = element_text(size = axis_text_size),
              axis.text.x = element_text(angle = 45, hjust = 1),
              legend.direction = 'horizontal',
              legend.position = c(0.5,0.9),
              legend.title = element_blank(),
              legend.background = element_rect(fill = NA))
      eval(parse(text = paste0('Aug_Sep_Oct_MAR_violin_loc_',j,'_plot = ViolinPlot_Uni_Cov')))
    }
  }
  
  if(UCODE_Uncert==TRUE){
    for (j in 1:5){
      for(m in 1:length(crit_months)){
        temp_data = subset(UCODE_MAR_diff,MONTH==crit_months[m] & INT.TYPE=='Simultaneous_2' & PRED.LOC == j & DATE!='1990-10-01')
        Plot = ggplot(data = temp_data, aes(x = DATE)) + 
          geom_hline(yintercept = 0) +
          geom_ribbon(aes(ymin = (PREDICTED.VALUE-(2*STANDARD.DEVIATION)), ymax = (PREDICTED.VALUE+(2*STANDARD.DEVIATION)), fill = CALIBRATION), alpha = 0.25) +
          geom_line(aes(y = PREDICTED.VALUE, group = CALIBRATION, color = CALIBRATION),size = line_size) +
          geom_point(aes(y = PREDICTED.VALUE, shape = CALIBRATION),size = point_size) +
          geom_rect(aes(xmin = as.Date('1990-10-01'), xmax = as.Date('1992-10-01'), ymin = -1E4 , ymax = -7.5E3), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('1993-10-01'), xmax = as.Date('1994-10-01'), ymin = -1E4 , ymax = -7.5E3), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('1994-10-01'), xmax = as.Date('1999-10-01'), ymin = -1E4 , ymax = -7.5E3), fill = 'skyblue', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2000-10-01'), xmax = as.Date('2002-10-01'), ymin = -1E4 , ymax = -7.5E3), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2005-10-01'), xmax = as.Date('2006-10-01'), ymin = -1E4 , ymax = -7.5E3), fill = 'skyblue', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2006-10-01'), xmax = as.Date('2009-10-01'), ymin = -1E4 , ymax = -7.5E3), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2010-10-01'), xmax = as.Date('2011-10-01'), ymin = -1E4 , ymax = -7.5E3), fill = 'skyblue', color = 'black', size = 0.2) +
          ylab(bquote("Streamflow Difference ("*m^3*"/day)")) +
          scale_color_manual(values = c('#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02')) +
          scale_fill_manual(values = c('#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02')) +
          scale_x_date(limits = c(as.Date('1990-10-01'),as.Date('2011-10-01')),
                       breaks = seq(as.Date('1990-10-01'),as.Date('2011-10-01'), by = '2 years'),
                       date_labels = '%Y', expand = c(0.01,0.01)) +
          scale_y_continuous(limits = c(-1E5,7E5), breaks = seq(-1E4,7E4,by=1E4), labels = paste0(seq(-1,7),'E4'),expand = c(0,0)) +
          coord_cartesian(ylim = c(-1E4,7E4)) +
          theme_few() +
          theme(axis.title.x = element_blank(),
                axis.title.y = element_text(size = axis_title_size),
                axis.text = element_text(size = axis_text_size),
                axis.text.x = element_text(angle = 45, hjust = 1),
                legend.direction = 'horizontal',
                legend.position = c(0.5,0.9),
                legend.title = element_blank(),
                legend.background = element_rect(fill = NA))
        eval(parse(text = paste0(crit_months[m],'_MAR_UCODE_loc_',j,'_plot = Plot')))
      }  
    }
  } 
}

# ILR Plots ---------------------------------------------------------------
if (ILR_Comp == TRUE){
  if (UniRandPar == TRUE){
    #Normalized Convergence Plots - Prediciton Location 1
    UniRandPar_Diff_ILR_convergence_norm_melt = reshape(UniRandPar_Diff_ILR_convergence_norm, varying = names(UniRandPar_Diff_ILR_convergence_norm[,seq(-1,-5)]),
                                                        v.names = 'Cumulative Mean',
                                                        timevar = 'Parameter Set Number',
                                                        direction = 'long') %>% subset(Month == 'Oct' & Pred_Loc == 1)  # reshape dataframe from plotting
    UniRandPar_Diff_ILR_convergence_norm_melt = left_join(UniRandPar_Diff_ILR_convergence_norm_melt, Water_Yr_Class_1991_2011, by = 'Water_Year')
    UniRandPar_Diff_ILR_convergence_norm_melt$Water.Year.Type = factor(UniRandPar_Diff_ILR_convergence_norm_melt$Water.Year.Type, 
                                                                       levels = c('Wet', 'Above Normal', 'Below Normal', 'Dry', 'Critical' ))
    UniRandPar_Diff_ILR_convergence_norm_melt = UniRandPar_Diff_ILR_convergence_norm_melt[order(UniRandPar_Diff_ILR_convergence_norm_melt$Water.Year.Type),]
    
    
    UniRandPar_ILR_Convergence_Plot = ggplot(UniRandPar_Diff_ILR_convergence_norm_melt, aes(x = `Parameter Set Number`, y = `Cumulative Mean`)) + 
      geom_hline(yintercept = 1) +
      geom_point(aes(color = Water.Year.Type), size = point_size) +
      scale_color_manual(values = c('navyblue', 'skyblue', 'tan1', 'tomato', 'red4')) +
      xlab('n') +
      ylab('ILR\nNormalized Cumulative Mean (-)') +
      ggtitle('ILR Streamflow Difference Convergence\nPrediciton Location 1: October') +
      #scale_x_continuous(limits = c(1,500), breaks = c(1,seq(100,500, by = 100)), expand = c(0,0)) +
      scale_y_continuous(limits = c(0,5), breaks = seq(0,5), expand = c(0,0)) +
      #scale_y_log10(limits = c(0.1,100), expand = c(0,0)) +
      scale_x_log10(limits = c(1,1000), expand = c(0.01,0.01)) +
      theme_few() +
      conv_theme +
      annotation_logticks(size = 0.25, sides = 'b')  
    
    #Monthly Series
    for (j in 1:5){
      eval(parse(text = paste0('quantiles = apply(subset(UniRandPar_Diff_ILR_clean,Pred_Loc==',j,')[,seq(-1,-5)],1,FUN = function(x) as.data.frame(t(quantile(x, probs = c(0.025, 0.5, 0.975), na.rm = T))))')))
      eval(parse(text = paste0('summary = cbind(subset(UniRandPar_Diff_ILR_clean,Pred_Loc==',j,')[,seq(1,5)], 
                               unlist(lapply(quantiles, `[[`, 1)), 
                               unlist(lapply(quantiles, `[[`, 2)), 
                               unlist(lapply(quantiles, `[[`, 3)))')))
      names(summary) = c(names(UniRandPar_Diff_ILR_clean[seq(1,5)]), 'lower95_m3day', 'median_m3day', 'upper95_m3day')
      for(m in 1:length(crit_months)){
        temp_data = subset(summary,Month==crit_months[m] & Date!='1990-10-01')
        Plot = ggplot(data = temp_data, aes(x = Date)) + 
          geom_hline(yintercept = 0) +
          geom_ribbon(aes(ymin = lower95_m3day, ymax = upper95_m3day), alpha = 0.75, fill = 'dodgerblue') + 
          geom_line(aes(y = median_m3day),size = line_size) +
          geom_point(aes(y = median_m3day),size = point_size) +
          geom_rect(aes(xmin = as.Date('1990-10-01'), xmax = as.Date('1992-10-01'), ymin = -50000 , ymax = -44000), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('1993-10-01'), xmax = as.Date('1994-10-01'), ymin = -50000 , ymax = -44000), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('1994-10-01'), xmax = as.Date('1999-10-01'), ymin = -50000 , ymax = -44000), fill = 'skyblue', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2000-10-01'), xmax = as.Date('2002-10-01'), ymin = -50000 , ymax = -44000), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2005-10-01'), xmax = as.Date('2006-10-01'), ymin = -50000 , ymax = -44000), fill = 'skyblue', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2006-10-01'), xmax = as.Date('2009-10-01'), ymin = -50000 , ymax = -44000), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2010-10-01'), xmax = as.Date('2011-10-01'), ymin = -50000 , ymax = -44000), fill = 'skyblue', color = 'black', size = 0.2) +
          ylab(bquote("Streamflow Difference ("*m^3*"/day)")) +
          scale_color_manual(values = c('black')) +
          scale_x_date(limits = c(as.Date('1990-10-01'),as.Date('2011-10-01')),
                       breaks = seq(as.Date('1990-10-01'),as.Date('2011-10-01'), by = '2 years'),
                       date_labels = '%Y', expand = c(0.01,0.01)) +
          scale_y_continuous(limits = c(-5E4,7E4), breaks = seq(-5E4,7E4,by=2E4), labels = paste0(seq(-5,7,by = 2),'E4'), expand = c(0,0)) +
          theme_few() +
          theme(axis.title.x = element_blank(),
                axis.text.x = element_text(angle = 45, hjust = 1),
                axis.text = element_text(size = axis_text_size),
                axis.title.y = element_text(size = axis_title_size))
        eval(parse(text = paste0(crit_months[m],'_ILR_uni_loc_',j,'_plot = Plot')))
      }
    }
  }
  if(CovRandPar==TRUE){
    #Normalized Convergence Plots - Prediciton Location 1
    CovRandPar_Diff_ILR_convergence_norm_melt = subset(CovRandPar_Diff_ILR_convergence_norm_melt, Month == 'Oct' & Pred_Loc == 1)
    CovRandPar_Diff_ILR_convergence_norm_melt = left_join(CovRandPar_Diff_ILR_convergence_norm_melt, Water_Yr_Class_1991_2011, by = 'Water_Year')
    CovRandPar_Diff_ILR_convergence_norm_melt$Water.Year.Type = factor(CovRandPar_Diff_ILR_convergence_norm_melt$Water.Year.Type, 
                                                                       levels = c('Wet', 'Above Normal', 'Below Normal', 'Dry', 'Critical' ))
    CovRandPar_Diff_ILR_convergence_norm_melt = CovRandPar_Diff_ILR_convergence_norm_melt[order(CovRandPar_Diff_ILR_convergence_norm_melt$Water.Year.Type),]
    
    CovRandPar_ILR_Convergence_Plot = ggplot(CovRandPar_Diff_ILR_convergence_norm_melt, aes(x = as.numeric(variable), y = value)) + 
      geom_hline(yintercept = 1) +
      geom_point(aes(color = Water.Year.Type, shape = Calibration), size = point_size ) +
      scale_color_manual(values = c('navyblue', 'skyblue', 'tan1', 'tomato', 'red4')) +
      xlab('n') +
      ylab('ILR\nNormalized Cumulative Mean (-)') +
      ggtitle('ILR Streamflow Difference Convergence\nPrediciton Location 1: October') +
      #scale_x_continuous(limits = c(1,100), breaks = c(1,seq(10,100, by = 10)), expand = c(0,0)) +
      scale_y_continuous(limits = c(0,5), breaks = seq(0,5), expand = c(0,0)) +
      #scale_y_log10(limits = c(0.1,100), expand = c(0,0)) +
      scale_x_log10(limits = c(1,1000), expand = c(0.01,0.01)) +
      theme_few() +
      conv_theme +
      annotation_logticks(size = 0.25, sides = 'b') 
    
    #Monthly Series
    for (j in 1:5){
      eval(parse(text = paste0('quantiles = apply(subset(CovRandPar_Diff_ILR,Pred_Loc==',j,')[,seq(-1,-6)],1,FUN = function(x) as.data.frame(t(quantile(x, probs = c(0.025, 0.5, 0.975), na.rm = T))))')))
      eval(parse(text = paste0('summary = cbind(subset(CovRandPar_Diff_ILR,Pred_Loc==',j,')[,seq(1,6)], 
                               unlist(lapply(quantiles, `[[`, 1)), 
                               unlist(lapply(quantiles, `[[`, 2)), 
                               unlist(lapply(quantiles, `[[`, 3)))')))
      names(summary) = c(names(CovRandPar_Diff_ILR[seq(1,6)]), 'lower95_m3day', 'median_m3day', 'upper95_m3day')
      for(m in 1:length(crit_months)){
        temp_data = subset(summary,Month==crit_months[m] & Date!='1990-10-01')
        Plot = ggplot(data = temp_data, aes(x = Date)) + 
          geom_hline(yintercept = 0) +
          geom_ribbon(aes(ymin = lower95_m3day, ymax = upper95_m3day, fill = Calibration), alpha = 0.25) +
          geom_line(aes(y = median_m3day, group = Calibration, color = Calibration),size = line_size) +
          geom_point(aes(y = median_m3day, shape = Calibration),size = point_size) +
          geom_rect(aes(xmin = as.Date('1990-10-01'), xmax = as.Date('1992-10-01'), ymin = -50000 , ymax = -44000), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('1993-10-01'), xmax = as.Date('1994-10-01'), ymin = -50000 , ymax = -44000), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('1994-10-01'), xmax = as.Date('1999-10-01'), ymin = -50000 , ymax = -44000), fill = 'skyblue', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2000-10-01'), xmax = as.Date('2002-10-01'), ymin = -50000 , ymax = -44000), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2005-10-01'), xmax = as.Date('2006-10-01'), ymin = -50000 , ymax = -44000), fill = 'skyblue', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2006-10-01'), xmax = as.Date('2009-10-01'), ymin = -50000 , ymax = -44000), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2010-10-01'), xmax = as.Date('2011-10-01'), ymin = -50000 , ymax = -44000), fill = 'skyblue', color = 'black', size = 0.2) +
          ylab(bquote("Streamflow Difference ("*m^3*"/day)")) +
          scale_color_manual(values = c('#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02')) +
          scale_fill_manual(values = c('#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02')) +
          scale_x_date(limits = c(as.Date('1990-10-01'),as.Date('2011-10-01')),
                       breaks = seq(as.Date('1990-10-01'),as.Date('2011-10-01'), by = '2 years'),
                       date_labels = '%Y', expand = c(0.01,0.01)) +
          scale_y_continuous(limits = c(-5E4,7E4), breaks = seq(-5E4,7E4,by=2E4), labels = paste0(seq(-5,7,by = 2),'E4'), expand = c(0,0)) +
          theme_few() +
          theme(axis.title.x = element_blank(),
                axis.title.y = element_text(size = axis_title_size),
                axis.text.x = element_text(angle = 45, hjust = 1),
                axis.text = element_text(size = axis_text_size),
                legend.direction = 'horizontal',
                legend.position = c(0.5,0.9),
                legend.title = element_blank(),
                legend.background = element_rect(fill = NA))
        eval(parse(text = paste0(crit_months[m],'_ILR_cov_loc_',j,'_plot = Plot')))
      }
    }
  }
  #Violin plots (Uniform + Normal)
  if (UniRandPar == TRUE & CovRandPar == TRUE ){
    for (j in 1:5){
      eval(parse(text = paste0('temp1 = melt(subset(UniRandPar_Diff_ILR_clean,Pred_Loc==',j,'),id.vars = names(UniRandPar_Diff_ILR_clean[seq(1,5)]))')))
      temp1$Month = factor(temp1$Month, levels = format(seq(as.Date('1990-01-01'), as.Date('1990-12-01'), by = 'month'),'%b'))
      temp1 = temp1[order(temp1$Month),]
      temp1$Calibration = 'Uncorrelated'
      
      eval(parse(text = paste0('temp2 = melt(subset(CovRandPar_Diff_ILR,Pred_Loc==',j,'),id.vars = names(CovRandPar_Diff_ILR[seq(1,6)]))')))
      temp2$Month = factor(temp2$Month, levels = format(seq(as.Date('1990-01-01'), as.Date('1990-12-01'), by = 'month'),'%b'))
      temp2 = temp2[order(temp2$Month),]
      
      Uni_Cov_Rand_Diff_ILR = rbind(temp1, temp2)
      Uni_Cov_Rand_Diff_ILR$Calibration = factor(Uni_Cov_Rand_Diff_ILR$Calibration, levels= c('Uncorrelated',paste0('Cal_',seq(1,5))))
      Uni_Cov_Rand_Diff_ILR = Uni_Cov_Rand_Diff_ILR[order(Uni_Cov_Rand_Diff_ILR$Calibration),]
      
      ViolinPlot_Uni_Cov = ggplot(data = subset(Uni_Cov_Rand_Diff_ILR,Month==crit_months), aes(x = factor(Month), y = value, fill = Calibration)) +
        geom_hline(yintercept = 0) +
        geom_violin(position = position_dodge(width = 0.9),draw_quantiles = c(0.5), size = 0.25) +
        ylab(bquote("Streamflow Difference ("*m^3*"/day)")) +
        scale_fill_manual(breaks = c('Uncorrelated', paste0('Cal_',seq(1,5))), values = c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02'), guide = guide_legend(nrow = 1)) +
        scale_y_continuous(limits = c(-4E5,7E5), breaks = seq(-4E4,1E5,by=2E4), labels = c(paste0(seq(-4,8,by = 2),'E4'),'1E5'), expand = c(0,0)) +
        scale_x_discrete(expand = c(0,0)) +
        coord_cartesian(ylim = c(-2E4,1E5)) +
        theme_few() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_text(size = axis_title_size),
              axis.text = element_text(size = axis_text_size),
              axis.text.x = element_text(angle = 45, hjust = 1),
              legend.direction = 'horizontal',
              legend.position = c(0.5,0.9),
              legend.title = element_blank(),
              legend.background = element_rect(fill = NA))
      eval(parse(text = paste0('Aug_Sep_Oct_ILR_violin_loc_',j,'_plot = ViolinPlot_Uni_Cov')))
    }
  }
  
  if(UCODE_Uncert==TRUE){
    for (j in 1:5){
      for(m in 1:length(crit_months)){
        temp_data = subset(UCODE_ILR_diff,MONTH==crit_months[m] & INT.TYPE=='Simultaneous_2' & PRED.LOC == j & DATE!='1990-10-01')
        Plot = ggplot(data = temp_data, aes(x = DATE)) + 
          geom_hline(yintercept = 0) +
          geom_ribbon(aes(ymin = (PREDICTED.VALUE-(2*STANDARD.DEVIATION)), ymax = (PREDICTED.VALUE+(2*STANDARD.DEVIATION)), fill = CALIBRATION), alpha = 0.25) +
          geom_line(aes(y = PREDICTED.VALUE, group = CALIBRATION, color = CALIBRATION),size = line_size) +
          geom_point(aes(y = PREDICTED.VALUE, shape = CALIBRATION),size = point_size) +
          geom_rect(aes(xmin = as.Date('1990-10-01'), xmax = as.Date('1992-10-01'), ymin = -50000 , ymax = -44000), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('1993-10-01'), xmax = as.Date('1994-10-01'), ymin = -50000 , ymax = -44000), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('1994-10-01'), xmax = as.Date('1999-10-01'), ymin = -50000 , ymax = -44000), fill = 'skyblue', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2000-10-01'), xmax = as.Date('2002-10-01'), ymin = -50000 , ymax = -44000), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2005-10-01'), xmax = as.Date('2006-10-01'), ymin = -50000 , ymax = -44000), fill = 'skyblue', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2006-10-01'), xmax = as.Date('2009-10-01'), ymin = -50000 , ymax = -44000), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2010-10-01'), xmax = as.Date('2011-10-01'), ymin = -50000 , ymax = -44000), fill = 'skyblue', color = 'black', size = 0.2) +
          ylab(bquote("Streamflow Difference ("*m^3*"/day)")) +
          scale_color_manual(values = c('#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02')) +
          scale_fill_manual(values = c('#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02')) +
          scale_x_date(limits = c(as.Date('1990-10-01'),as.Date('2011-10-01')),
                       breaks = seq(as.Date('1990-10-01'),as.Date('2011-10-01'), by = '2 years'),
                       date_labels = '%Y', expand = c(0.01,0.01)) +
          scale_y_continuous(limits = c(-5E5,7E5), breaks = seq(-5E4,7E4,by=2E4), labels = paste0(seq(-5,7,by = 2),'E4'), expand = c(0,0)) +
          coord_cartesian(ylim = c(-5E4,7E4)) +
          theme_few() +
          theme(axis.title.x = element_blank(),
                axis.title.y = element_text(size = axis_title_size),
                axis.text = element_text(size = axis_text_size),
                axis.text.x = element_text(angle = 45, hjust = 1),
                legend.direction = 'horizontal',
                legend.position = c(0.5,0.9),
                legend.title = element_blank(),
                legend.background = element_rect(fill = NA))
        eval(parse(text = paste0(crit_months[m],'_ILR_UCODE_loc_',j,'_plot = Plot')))
      }  
    }
  } 
}

# MAR_ILR Plots -----------------------------------------------------------
if (MAR_ILR_Comp == TRUE){
  if (UniRandPar == TRUE){
    #Normalized Convergence Plots - Prediciton Location 1
    UniRandPar_Diff_MAR_ILR_convergence_norm_melt = reshape(UniRandPar_Diff_MAR_ILR_convergence_norm, varying = names(UniRandPar_Diff_MAR_ILR_convergence_norm[,seq(-1,-5)]),
                                                        v.names = 'Cumulative Mean',
                                                        timevar = 'Parameter Set Number',
                                                        direction = 'long') %>% subset(Month == 'Oct' & Pred_Loc == 1)  # reshape dataframe from plotting
    UniRandPar_Diff_MAR_ILR_convergence_norm_melt = left_join(UniRandPar_Diff_MAR_ILR_convergence_norm_melt, Water_Yr_Class_1991_2011, by = 'Water_Year')
    UniRandPar_Diff_MAR_ILR_convergence_norm_melt$Water.Year.Type = factor(UniRandPar_Diff_MAR_ILR_convergence_norm_melt$Water.Year.Type, 
                                                                       levels = c('Wet', 'Above Normal', 'Below Normal', 'Dry', 'Critical' ))
    UniRandPar_Diff_MAR_ILR_convergence_norm_melt = UniRandPar_Diff_MAR_ILR_convergence_norm_melt[order(UniRandPar_Diff_MAR_ILR_convergence_norm_melt$Water.Year.Type),]
    
    
    UniRandPar_MAR_ILR_Convergence_Plot = ggplot(UniRandPar_Diff_MAR_ILR_convergence_norm_melt, aes(x = `Parameter Set Number`, y = `Cumulative Mean`)) + 
      geom_hline(yintercept = 1) +
      geom_point(aes(color = Water.Year.Type), size = point_size) +
      scale_color_manual(values = c('navyblue', 'skyblue', 'tan1', 'tomato', 'red4')) +
      xlab('n') +
      ylab('MAR_ILR\nNormalized Cumulative Mean (-)') +
      ggtitle('MAR_ILR Streamflow Difference Convergence\nPrediciton Location 1: October') +
      #scale_x_continuous(limits = c(1,500), breaks = c(1,seq(100,500, by = 100)), expand = c(0,0)) +
      scale_y_continuous(limits = c(0,5), breaks = seq(0,5), expand = c(0,0)) +
      #scale_y_log10(limits = c(0.1,100), expand = c(0,0)) +
      scale_x_log10(limits = c(1,1000), expand = c(0.01,0.01)) +
      theme_few() +
      conv_theme +
      annotation_logticks(size = 0.25, sides = 'b')  
    
    #Monthly Series
    for (j in 1:5){
      eval(parse(text = paste0('quantiles = apply(subset(UniRandPar_Diff_MAR_ILR_clean,Pred_Loc==',j,')[,seq(-1,-5)],1,FUN = function(x) as.data.frame(t(quantile(x, probs = c(0.025, 0.5, 0.975), na.rm = T))))')))
      eval(parse(text = paste0('summary = cbind(subset(UniRandPar_Diff_MAR_ILR_clean,Pred_Loc==',j,')[,seq(1,5)], 
                               unlist(lapply(quantiles, `[[`, 1)), 
                               unlist(lapply(quantiles, `[[`, 2)), 
                               unlist(lapply(quantiles, `[[`, 3)))')))
      names(summary) = c(names(UniRandPar_Diff_MAR_ILR_clean[seq(1,5)]), 'lower95_m3day', 'median_m3day', 'upper95_m3day')
      for(m in 1:length(crit_months)){
        temp_data = subset(summary,Month==crit_months[m] & Date!='1990-10-01')
        Plot = ggplot(data = temp_data, aes(x = Date)) + 
          geom_hline(yintercept = 0) +
          geom_ribbon(aes(ymin = lower95_m3day, ymax = upper95_m3day), alpha = 0.75, fill = 'dodgerblue') + 
          geom_line(aes(y = median_m3day),size = line_size) +
          geom_point(aes(y = median_m3day),size = point_size) +
          geom_rect(aes(xmin = as.Date('1990-10-01'), xmax = as.Date('1992-10-01'), ymin = -50000 , ymax = -44000), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('1993-10-01'), xmax = as.Date('1994-10-01'), ymin = -50000 , ymax = -44000), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('1994-10-01'), xmax = as.Date('1999-10-01'), ymin = -50000 , ymax = -44000), fill = 'skyblue', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2000-10-01'), xmax = as.Date('2002-10-01'), ymin = -50000 , ymax = -44000), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2005-10-01'), xmax = as.Date('2006-10-01'), ymin = -50000 , ymax = -44000), fill = 'skyblue', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2006-10-01'), xmax = as.Date('2009-10-01'), ymin = -50000 , ymax = -44000), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2010-10-01'), xmax = as.Date('2011-10-01'), ymin = -50000 , ymax = -44000), fill = 'skyblue', color = 'black', size = 0.2) +
          ylab(bquote("Streamflow Difference ("*m^3*"/day)")) +
          scale_color_manual(values = c('black')) +
          scale_x_date(limits = c(as.Date('1990-10-01'),as.Date('2011-10-01')),
                       breaks = seq(as.Date('1990-10-01'),as.Date('2011-10-01'), by = '2 years'),
                       date_labels = '%Y', expand = c(0.01,0.01)) +
          scale_y_continuous(limits = c(-5E4,9E4), breaks = seq(-5E4,9E4,by=2E4), labels = paste0(seq(-5,9,by = 2),'E4'), expand = c(0,0)) +
          theme_few() +
          theme(axis.title.x = element_blank(),
                axis.text.x = element_text(angle = 45, hjust = 1),
                axis.text = element_text(size = axis_text_size),
                axis.title.y = element_text(size = axis_title_size))
        eval(parse(text = paste0(crit_months[m],'_MAR_ILR_uni_loc_',j,'_plot = Plot')))
      }
    }
  }
  if(CovRandPar==TRUE){
    #Normalized Convergence Plots - Prediciton Location 1
    CovRandPar_Diff_MAR_ILR_convergence_norm_melt = subset(CovRandPar_Diff_MAR_ILR_convergence_norm_melt, Month == 'Oct' & Pred_Loc == 1)
    CovRandPar_Diff_MAR_ILR_convergence_norm_melt = left_join(CovRandPar_Diff_MAR_ILR_convergence_norm_melt, Water_Yr_Class_1991_2011, by = 'Water_Year')
    CovRandPar_Diff_MAR_ILR_convergence_norm_melt$Water.Year.Type = factor(CovRandPar_Diff_MAR_ILR_convergence_norm_melt$Water.Year.Type, 
                                                                           levels = c('Wet', 'Above Normal', 'Below Normal', 'Dry', 'Critical' ))
    CovRandPar_Diff_MAR_ILR_convergence_norm_melt = CovRandPar_Diff_MAR_ILR_convergence_norm_melt[order(CovRandPar_Diff_MAR_ILR_convergence_norm_melt$Water.Year.Type),]
    
    CovRandPar_MAR_ILR_Convergence_Plot = ggplot(CovRandPar_Diff_MAR_ILR_convergence_norm_melt, aes(x = as.numeric(variable), y = value)) + 
      geom_hline(yintercept = 1) +
      geom_point(aes(color = Water.Year.Type, shape = Calibration), size = point_size ) +
      scale_color_manual(values = c('navyblue', 'skyblue', 'tan1', 'tomato', 'red4')) +
      xlab('n') +
      ylab('MAR_ILR\nNormalized Cumulative Mean (-)') +
      ggtitle('MAR_ILR Streamflow Difference Convergence\nPrediciton Location 1: October') +
      #scale_x_continuous(limits = c(1,100), breaks = c(1,seq(10,100, by = 10)), expand = c(0,0)) +
      scale_y_continuous(limits = c(0,5), breaks = seq(0,5), expand = c(0,0)) +
      #scale_y_log10(limits = c(0.1,100), expand = c(0,0)) +
      scale_x_log10(limits = c(1,1000), expand = c(0.01,0.01)) +
      theme_few() +
      conv_theme +
      annotation_logticks(size = 0.25, sides = 'b') 
    
    #Monthly Series
    for (j in 1:5){
      eval(parse(text = paste0('quantiles = apply(subset(CovRandPar_Diff_MAR_ILR_clean,Pred_Loc==',j,')[,seq(-1,-6)],1,FUN = function(x) as.data.frame(t(quantile(x, probs = c(0.025, 0.5, 0.975), na.rm = T))))')))
      eval(parse(text = paste0('summary = cbind(subset(CovRandPar_Diff_MAR_ILR_clean,Pred_Loc==',j,')[,seq(1,6)], 
                               unlist(lapply(quantiles, `[[`, 1)), 
                               unlist(lapply(quantiles, `[[`, 2)), 
                               unlist(lapply(quantiles, `[[`, 3)))')))
      names(summary) = c(names(CovRandPar_Diff_MAR_ILR_clean[seq(1,6)]), 'lower95_m3day', 'median_m3day', 'upper95_m3day')
      for(m in 1:length(crit_months)){
        temp_data = subset(summary,Month==crit_months[m] & Date!='1990-10-01')
        Plot = ggplot(data = temp_data, aes(x = Date)) + 
          geom_hline(yintercept = 0) +
          geom_ribbon(aes(ymin = lower95_m3day, ymax = upper95_m3day, fill = Calibration), alpha = 0.25) +
          geom_line(aes(y = median_m3day, group = Calibration, color = Calibration),size = line_size) +
          geom_point(aes(y = median_m3day, shape = Calibration),size = point_size) +
          geom_rect(aes(xmin = as.Date('1990-10-01'), xmax = as.Date('1992-10-01'), ymin = -50000 , ymax = -44000), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('1993-10-01'), xmax = as.Date('1994-10-01'), ymin = -50000 , ymax = -44000), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('1994-10-01'), xmax = as.Date('1999-10-01'), ymin = -50000 , ymax = -44000), fill = 'skyblue', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2000-10-01'), xmax = as.Date('2002-10-01'), ymin = -50000 , ymax = -44000), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2005-10-01'), xmax = as.Date('2006-10-01'), ymin = -50000 , ymax = -44000), fill = 'skyblue', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2006-10-01'), xmax = as.Date('2009-10-01'), ymin = -50000 , ymax = -44000), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2010-10-01'), xmax = as.Date('2011-10-01'), ymin = -50000 , ymax = -44000), fill = 'skyblue', color = 'black', size = 0.2) +
          ylab(bquote("Streamflow Difference ("*m^3*"/day)")) +
          scale_color_manual(values = c('#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02')) +
          scale_fill_manual(values = c('#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02')) +
          scale_x_date(limits = c(as.Date('1990-10-01'),as.Date('2011-10-01')),
                       breaks = seq(as.Date('1990-10-01'),as.Date('2011-10-01'), by = '2 years'),
                       date_labels = '%Y', expand = c(0.01,0.01)) +
          scale_y_continuous(limits = c(-5E4,9E5), breaks = seq(-5E4,9E4,by=2E4), labels = paste0(seq(-5,9,by = 2),'E4'), expand = c(0,0)) +
          coord_cartesian(ylim = c(-5E4,9E4)) +
          theme_few() +
          theme(axis.title.x = element_blank(),
                axis.title.y = element_text(size = axis_title_size),
                axis.text.x = element_text(angle = 45, hjust = 1),
                axis.text = element_text(size = axis_text_size),
                legend.direction = 'horizontal',
                legend.position = c(0.5,0.9),
                legend.title = element_blank(),
                legend.background = element_rect(fill = NA))
        eval(parse(text = paste0(crit_months[m],'_MAR_ILR_cov_loc_',j,'_plot = Plot')))
      }
    }
  }
  
  #Violin plots (Uniform + Normal)
  if (UniRandPar == TRUE & CovRandPar == TRUE ){
    for (j in 1:5){
      eval(parse(text = paste0('temp1 = melt(subset(UniRandPar_Diff_MAR_ILR_clean,Pred_Loc==',j,'),id.vars = names(UniRandPar_Diff_MAR_ILR_clean[seq(1,5)]))')))
      temp1$Month = factor(temp1$Month, levels = format(seq(as.Date('1990-01-01'), as.Date('1990-12-01'), by = 'month'),'%b'))
      temp1 = temp1[order(temp1$Month),]
      temp1$Calibration = 'Uncorrelated'
      
      eval(parse(text = paste0('temp2 = melt(subset(CovRandPar_Diff_MAR_ILR_clean,Pred_Loc==',j,'),id.vars = names(CovRandPar_Diff_MAR_ILR_clean[seq(1,6)]))')))
      temp2$Month = factor(temp2$Month, levels = format(seq(as.Date('1990-01-01'), as.Date('1990-12-01'), by = 'month'),'%b'))
      temp2 = temp2[order(temp2$Month),]
      
      Uni_Cov_Rand_Diff_MAR_ILR = rbind(temp1, temp2)
      Uni_Cov_Rand_Diff_MAR_ILR$Calibration = factor(Uni_Cov_Rand_Diff_MAR_ILR$Calibration, levels= c('Uncorrelated',paste0('Cal_',seq(1,5))))
      Uni_Cov_Rand_Diff_MAR_ILR = Uni_Cov_Rand_Diff_MAR_ILR[order(Uni_Cov_Rand_Diff_MAR_ILR$Calibration),]
      
      ViolinPlot_Uni_Cov = ggplot(data = subset(Uni_Cov_Rand_Diff_MAR_ILR,Month==crit_months), aes(x = factor(Month), y = value, fill = Calibration)) +
        geom_hline(yintercept = 0) +
        geom_violin(position = position_dodge(width = 0.9),draw_quantiles = c(0.5), size = 0.25) +
        ylab(bquote("Streamflow Difference ("*m^3*"/day)")) +
        scale_fill_manual(breaks = c('Uncorrelated', paste0('Cal_',seq(1,5))), values = c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02'), guide = guide_legend(nrow = 1)) +
        scale_y_continuous(limits = c(-4E5,7E5), breaks = seq(-4E4,1E5,by=2E4), labels = c(paste0(seq(-4,8,by = 2),'E4'),'1E5'), expand = c(0,0)) +
        scale_x_discrete(expand = c(0,0)) +
        coord_cartesian(ylim = c(-2E4,1E5)) +
        theme_few() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_text(size = axis_title_size),
              axis.text = element_text(size = axis_text_size),
              axis.text.x = element_text(angle = 45, hjust = 1),
              legend.direction = 'horizontal',
              legend.position = c(0.5,0.9),
              legend.title = element_blank(),
              legend.background = element_rect(fill = NA))
      eval(parse(text = paste0('Aug_Sep_Oct_MAR_ILR_violin_loc_',j,'_plot = ViolinPlot_Uni_Cov')))
    }
  }
  if(UCODE_Uncert==TRUE){
    for (j in 1:5){
      for(m in 1:length(crit_months)){
        temp_data = subset(UCODE_MAR_ILR_diff,MONTH==crit_months[m] & INT.TYPE=='Simultaneous_2' & PRED.LOC == j & DATE!='1990-10-01')
        Plot = ggplot(data = temp_data, aes(x = DATE)) + 
          geom_hline(yintercept = 0) +
          geom_ribbon(aes(ymin = (PREDICTED.VALUE-(2*STANDARD.DEVIATION)), ymax = (PREDICTED.VALUE+(2*STANDARD.DEVIATION)), fill = CALIBRATION), alpha = 0.25) +
          geom_line(aes(y = PREDICTED.VALUE, group = CALIBRATION, color = CALIBRATION),size = line_size) +
          geom_point(aes(y = PREDICTED.VALUE, shape = CALIBRATION),size = point_size) +
          geom_rect(aes(xmin = as.Date('1990-10-01'), xmax = as.Date('1992-10-01'), ymin = -50000 , ymax = -44000), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('1993-10-01'), xmax = as.Date('1994-10-01'), ymin = -50000 , ymax = -44000), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('1994-10-01'), xmax = as.Date('1999-10-01'), ymin = -50000 , ymax = -44000), fill = 'skyblue', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2000-10-01'), xmax = as.Date('2002-10-01'), ymin = -50000 , ymax = -44000), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2005-10-01'), xmax = as.Date('2006-10-01'), ymin = -50000 , ymax = -44000), fill = 'skyblue', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2006-10-01'), xmax = as.Date('2009-10-01'), ymin = -50000 , ymax = -44000), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2010-10-01'), xmax = as.Date('2011-10-01'), ymin = -50000 , ymax = -44000), fill = 'skyblue', color = 'black', size = 0.2) +
          ylab(bquote("Streamflow Difference ("*m^3*"/day)")) +
          scale_color_manual(values = c('#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02')) +
          scale_fill_manual(values = c('#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02')) +
          scale_x_date(limits = c(as.Date('1990-10-01'),as.Date('2011-10-01')),
                       breaks = seq(as.Date('1990-10-01'),as.Date('2011-10-01'), by = '2 years'),
                       date_labels = '%Y', expand = c(0.01,0.01)) +
          scale_y_continuous(limits = c(-5E5,9E5), breaks = seq(-5E4,9E4,by=2E4), labels = paste0(seq(-5,9,by = 2),'E4'), expand = c(0,0)) +
          coord_cartesian(ylim = c(-5E4,9E4)) +
          theme_few() +
          theme(axis.title.x = element_blank(),
                axis.title.y = element_text(size = axis_title_size),
                axis.text = element_text(size = axis_text_size),
                axis.text.x = element_text(angle = 45, hjust = 1),
                legend.direction = 'horizontal',
                legend.position = c(0.5,0.9),
                legend.title = element_blank(),
                legend.background = element_rect(fill = NA))
        eval(parse(text = paste0(crit_months[m],'_MAR_ILR_UCODE_loc_',j,'_plot = Plot')))
      }  
    }
  }
}


# Monte Carlo Convergence Plots -------------------------------------------------------
fig_name = 'Monte Carlo Convergence'                                                                           
if (fig_format == 'jpg'){                                                                                     
  jpeg(paste0(out_dir,fig_name,'.jpg'), width = fig_width, height = fig_height, units = fig_units,  res = 600)
} else if (fig_format == 'png'){                                                                              
  png(paste0(out_dir,fig_name,'.png'), width = 7.48, height = 5.9, units = 'in',  res = 600) 
} else if (fig_format == 'pdf'){                                                                              
  #pdf(paste0(out_dir,fig_name,'.pdf'), width = 7.48, height = 5.9)                                
}                                                                                                             
grid.newpage()                                                                                                
pushViewport(viewport(layout = grid.layout(3,2)))                                                             
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)                                   
print(UniRandPar_MAR_Convergence_Plot + theme(axis.title.x = element_blank(),
                                              axis.ticks = element_line(size = 0.25),
                                              plot.title = element_blank(),
                                              legend.text = element_text(size = 6),
                                              legend.background = element_rect(fill = NA, color = NA),
                                              legend.key.width = unit(0.4,'cm'),
                                              legend.position = c(0.6, 0.9)), vp = vplayout(1,1))   
print(CovRandPar_MAR_Convergence_Plot + theme(axis.title = element_blank(),
                                              axis.ticks = element_line(size = 0.25),
                                              plot.title = element_blank(),
                                              legend.text = element_text(size = 6),
                                              legend.background = element_rect(fill = NA, color = NA),
                                              legend.spacing.y = unit(-0.6, 'cm'),
                                              legend.key.width = unit(0.4,'cm'),
                                              legend.position = c(0.63,0.85)), vp = vplayout(1,2))   
print(UniRandPar_ILR_Convergence_Plot + theme(axis.title.x = element_blank(),
                                              axis.ticks = element_line(size = 0.25),
                                              plot.title = element_blank(),
                                              legend.position = 'none'), vp = vplayout(2,1))   
print(CovRandPar_ILR_Convergence_Plot + theme(axis.title = element_blank(),
                                              axis.ticks = element_line(size = 0.25),
                                              plot.title = element_blank(),
                                              legend.position = 'none'), vp = vplayout(2,2))
print(UniRandPar_MAR_ILR_Convergence_Plot + theme(plot.title = element_blank(),
                                              axis.ticks = element_line(size = 0.25),
                                              legend.position = 'none'), vp = vplayout(3,1))   
print(CovRandPar_MAR_ILR_Convergence_Plot + theme(axis.title.y = element_blank(),
                                              axis.ticks = element_line(size = 0.25),
                                              plot.title = element_blank(),
                                              legend.position = 'none'), vp = vplayout(3,2))
graphics.off()

# October MAR Difference Plots ----------------------------------                                             
fig_name = 'Oct_MAR_Flow_Diffs'                                                                           
if (fig_format == 'jpg'){                                                                                     
  jpeg(paste0(out_dir,fig_name,'.jpg'), width = fig_width, height = fig_height, units = fig_units,  res = 600)
} else if (fig_format == 'png'){                                                                              
  png(paste0(out_dir,fig_name,'.png'), width = fig_width, height = fig_height, units = fig_units,  res = 600) 
} else if (fig_format == 'pdf'){                                                                              
  pdf(paste0(out_dir,fig_name,'.pdf'), width = 7.48, height = 4.5 )                                
}                                                                                                             
grid.newpage()                                                                                                
pushViewport(viewport(layout = grid.layout(2,3)))                                                             
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)                                   
print(Oct_MAR_uni_loc_1_plot +                                                                            
        scale_x_date(limits = c(as.Date('1990-10-01'), as.Date('2011-10-01')),                                
                     breaks = seq(as.Date('1990-10-1'), by = "2 years", length.out = 22),                     
                     expand = c(0.01, 0.01)) +                                                                
        #ylab(bquote("Prediction Location 1\nStreamflow Difference ("*m^3*"/day)")) +                                 
        theme(axis.text.x = element_blank(),
              axis.title = element_blank(),
              plot.margin = margin(t=5,b=13,l=5,r=-11),                                                        
              plot.background = element_rect(fill = NA, color = NA)),                                         
      vp = vplayout(1,1))                                                                                     
print(Oct_MAR_cov_loc_1_plot +                                                                            
        scale_x_date(limits = c(as.Date('1990-10-01'), as.Date('2011-10-01')),                                
                     breaks = seq(as.Date('1990-10-1'), by = "2 years", length.out = 22),                     
                     expand = c(0.01, 0.01)) +                                                                
        theme(axis.text.x = element_blank(),                                                                  
              axis.text.y = element_blank(),                                                                  
              axis.title = element_blank(),                                                                 
              legend.position = 'none',                                                                       
              plot.margin = margin(t=5,b=13,l=21,r=-3),                                                        
              plot.background = element_rect(fill = NA, color = NA)),                                         
      vp = vplayout(1,2))                                                                                     
print(Oct_MAR_UCODE_loc_1_plot +                                                                          
        scale_x_date(limits = c(as.Date('1990-10-01'), as.Date('2011-10-01')),                                
                     breaks = seq(as.Date('1990-10-1'), by = "2 years", length.out = 22),                     
                     expand = c(0.01, 0.01)) +                                                                
        theme(axis.text.x = element_blank(),                                                                  
              axis.text = element_blank(),                                                                  
              axis.title.y = element_blank(),                                                                 
              legend.position = 'none',                                                                       
              plot.margin = margin(t=5,b=13,l=13,r=5),                                                         
              plot.background = element_rect(fill = NA, color = NA)),                                         
      vp = vplayout(1,3))                                                                                     
print(Oct_MAR_uni_loc_2_plot +                                                                            
        scale_x_date(limits = c(as.Date('1990-10-01'), as.Date('2011-10-01')),                                
                     breaks = seq(as.Date('1990-10-1'), by = "2 years", length.out = 22), 
                     date_labels = '%b-%y',
                     expand = c(0.01, 0.01)) +                                                                
        #ylab(bquote("Prediction Location 2\nStreamflow Difference ("*m^3*"/day)")) +                              
        theme(axis.title = element_blank(),
              plot.margin = margin(t=-3,b=5,l=5,r=-11),                                                        
              plot.background = element_rect(fill = NA, color = NA)),                                         
      vp = vplayout(2,1))                                                                                     
print(Oct_MAR_cov_loc_2_plot +                                                                            
        scale_x_date(limits = c(as.Date('1990-10-01'), as.Date('2011-10-01')),                                
                     breaks = seq(as.Date('1990-10-1'), by = "2 years", length.out = 22),  
                     date_labels = '%b-%y',
                     expand = c(0.01, 0.01)) +                                                                
        theme(axis.text.y = element_blank(),                                                                  
              axis.title = element_blank(),                                                                 
              legend.key.size = unit(0.3, 'cm'),                                                              
              legend.position = c(0.5,0.9),                                                                   
              legend.text = element_text(size = axis_text_size),
              legend.background = element_rect(color = 'black', size = 0.25),
              legend.margin = margin(t=2,b=2,l=2,r=2),
              plot.margin = margin(t=-3,b=5,l=21,r=-3),                                                        
              plot.background = element_rect(fill = NA, color = NA)),                                         
      vp = vplayout(2,2))                                                                                     
print(Oct_MAR_UCODE_loc_2_plot +                                                                          
        scale_x_date(limits = c(as.Date('1990-10-01'), as.Date('2011-10-01')),                                
                     breaks = seq(as.Date('1990-10-1'), by = "2 years", length.out = 22),  
                     date_labels = '%b-%y',
                     expand = c(0.01, 0.01)) +                                                                
        theme(axis.text.y = element_blank(),                                                                  
              axis.title = element_blank(),                                                                 
              legend.position = 'none',                                                                       
              plot.margin = margin(t=-3,b=5,l=13,r=5),                                                         
              plot.background = element_rect(fill = NA, color = NA)),                                         
      vp = vplayout(2,3))                                                                                     
graphics.off()           


# October ILR Difference Plots ----------------------------------                                             
fig_name = 'Oct_ILR_Flow_Diffs'                                                                           
if (fig_format == 'jpg'){                                                                                     
  jpeg(paste0(out_dir,fig_name,'.jpg'), width = fig_width, height = fig_height, units = fig_units,  res = 600)
} else if (fig_format == 'png'){                                                                              
  png(paste0(out_dir,fig_name,'.png'), width = fig_width, height = fig_height, units = fig_units,  res = 600) 
} else if (fig_format == 'pdf'){                                                                              
  pdf(paste0(out_dir,fig_name,'.pdf'), width = 7.48, height = 4.5 )                                          
}                                                                                                             
grid.newpage()                                                                                                
pushViewport(viewport(layout = grid.layout(2,3)))                                                             
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)                                   
print(Oct_ILR_uni_loc_1_plot +                                                                            
        scale_x_date(limits = c(as.Date('1990-10-01'), as.Date('2011-10-01')),                                
                     breaks = seq(as.Date('1990-10-1'), by = "2 years", length.out = 22),                     
                     expand = c(0.01, 0.01)) +                                                                
        #ylab(bquote("Prediction Location 1\nStreamflow Difference ("*m^3*"/day)")) +                                   
        theme(axis.text.x = element_blank(),
              axis.title = element_blank(), 
              plot.margin = margin(t=5,b=13,l=5,r=-11),                                                        
              plot.background = element_rect(fill = NA, color = NA)),                                         
      vp = vplayout(1,1))                                                                                     
print(Oct_ILR_cov_loc_1_plot +                                                                            
        scale_x_date(limits = c(as.Date('1990-10-01'), as.Date('2011-10-01')),                                
                     breaks = seq(as.Date('1990-10-1'), by = "2 years", length.out = 22),                     
                     expand = c(0.01, 0.01)) +                                                                
        theme(axis.text.x = element_blank(),                                                                  
              axis.text.y = element_blank(),                                                                  
              axis.title = element_blank(),                                                                 
              legend.position = 'none',                                                                       
              plot.margin = margin(t=5,b=13,l=21,r=-3),                                                        
              plot.background = element_rect(fill = NA, color = NA)),                                         
      vp = vplayout(1,2))                                                                                     
print(Oct_ILR_UCODE_loc_1_plot +                                                                          
        scale_x_date(limits = c(as.Date('1990-10-01'), as.Date('2011-10-01')),                                
                     breaks = seq(as.Date('1990-10-1'), by = "2 years", length.out = 22),                     
                     expand = c(0.01, 0.01)) +                                                                
        theme(axis.text.x = element_blank(),                                                                  
              axis.text.y = element_blank(),                                                                  
              axis.title = element_blank(),                                                                 
              legend.position = 'none',                                                                       
              plot.margin = margin(t=5,b=13,l=13,r=5),                                                         
              plot.background = element_rect(fill = NA, color = NA)),                                         
      vp = vplayout(1,3))                                                                                     
print(Oct_ILR_uni_loc_2_plot +                                                                            
        scale_x_date(limits = c(as.Date('1990-10-01'), as.Date('2011-10-01')),                                
                     breaks = seq(as.Date('1990-10-1'), by = "2 years", length.out = 22), 
                     date_labels = '%b-%y',
                     expand = c(0.01, 0.01)) +                                                                
        #ylab(bquote("Prediction Location 2\nStreamflow Difference ("*m^3*"/day)")) +                                     
        theme(axis.title = element_blank(), 
              plot.margin = margin(t=-3,b=5,l=5,r=-11),                                                        
              plot.background = element_rect(fill = NA, color = NA)),                                         
      vp = vplayout(2,1))                                                                                     
print(Oct_ILR_cov_loc_2_plot +                                                                            
        scale_x_date(limits = c(as.Date('1990-10-01'), as.Date('2011-10-01')),                                
                     breaks = seq(as.Date('1990-10-1'), by = "2 years", length.out = 22),  
                     date_labels = '%b-%y',
                     expand = c(0.01, 0.01)) +                                                                
        theme(axis.text.y = element_blank(),                                                                  
              axis.title = element_blank(),                                                                 
              legend.key.size = unit(0.3, 'cm'),                                                              
              legend.position = c(0.5,0.9),                                                                   
              legend.text = element_text(size = axis_text_size),   
              legend.background = element_rect(color = 'black', size = 0.25),
              legend.margin = margin(t=2,b=2,l=2,r=2),
              plot.margin = margin(t=-3,b=5,l=21,r=-3),                                                        
              plot.background = element_rect(fill = NA, color = NA)),                                         
      vp = vplayout(2,2))                                                                                     
print(Oct_ILR_UCODE_loc_2_plot +                                                                          
        scale_x_date(limits = c(as.Date('1990-10-01'), as.Date('2011-10-01')),                                
                     breaks = seq(as.Date('1990-10-1'), by = "2 years", length.out = 22),  
                     date_labels = '%b-%y',
                     expand = c(0.01, 0.01)) +                                                                
        theme(axis.text.y = element_blank(),                                                                  
              axis.title = element_blank(),                                                                 
              legend.position = 'none',                                                                       
              plot.margin = margin(t=-3,b=5,l=13,r=5),                                                         
              plot.background = element_rect(fill = NA, color = NA)),                                         
      vp = vplayout(2,3))                                                                                     
graphics.off()           

# October MAR_ILR Difference Plots ----------------------------------                                             
fig_name = 'Oct_MAR_ILR_Flow_Diffs'                                                                           
if (fig_format == 'jpg'){                                                                                     
  jpeg(paste0(out_dir,fig_name,'.jpg'), width = fig_width, height = fig_height, units = fig_units,  res = 600)
} else if (fig_format == 'png'){                                                                              
  png(paste0(out_dir,fig_name,'.png'), width = fig_width, height = fig_height, units = fig_units,  res = 600) 
} else if (fig_format == 'pdf'){                                                                              
  pdf(paste0(out_dir,fig_name,'.pdf'), width = 7.48, height = 4.5 )                                          
}                                                                                                             
grid.newpage()                                                                                                
pushViewport(viewport(layout = grid.layout(2,3)))                                                             
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)                                   
print(Oct_MAR_ILR_uni_loc_1_plot +                                                                            
        scale_x_date(limits = c(as.Date('1990-10-01'), as.Date('2011-10-01')),                                
                     breaks = seq(as.Date('1990-10-1'), by = "2 years", length.out = 22),                     
                     expand = c(0.01, 0.01)) +                                                                
        #ylab(bquote("Prediciton Location 1\nStreamflow Difference ("*m^3*"/day)")) +                                      
        theme(axis.text.x = element_blank(),
              axis.title = element_blank(),
              plot.margin = margin(t=5,b=13,l=5,r=-11),                                                        
              plot.background = element_rect(fill = NA, color = NA)),                                         
      vp = vplayout(1,1))                                                                                     
print(Oct_MAR_ILR_cov_loc_1_plot +                                                                            
        scale_x_date(limits = c(as.Date('1990-10-01'), as.Date('2011-10-01')),                                
                     breaks = seq(as.Date('1990-10-1'), by = "2 years", length.out = 22),                     
                     expand = c(0.01, 0.01)) +                                                                
        theme(axis.text.x = element_blank(),                                                                  
              axis.text.y = element_blank(),                                                                  
              axis.title = element_blank(),                                                                 
              legend.position = 'none',                                                                       
              plot.margin = margin(t=5,b=13,l=21,r=-3),                                                        
              plot.background = element_rect(fill = NA, color = NA)),                                         
      vp = vplayout(1,2))                                                                                     
print(Oct_MAR_ILR_UCODE_loc_1_plot +                                                                          
        scale_x_date(limits = c(as.Date('1990-10-01'), as.Date('2011-10-01')),                                
                     breaks = seq(as.Date('1990-10-1'), by = "2 years", length.out = 22),                     
                     expand = c(0.01, 0.01)) +                                                                
        theme(axis.text.x = element_blank(),                                                                  
              axis.text.y = element_blank(),                                                                  
              axis.title = element_blank(),                                                                 
              legend.position = 'none',                                                                       
              plot.margin = margin(t=5,b=13,l=13,r=5),                                                         
              plot.background = element_rect(fill = NA, color = NA)),                                         
      vp = vplayout(1,3))                                                                                     
print(Oct_MAR_ILR_uni_loc_2_plot +                                                                            
        scale_x_date(limits = c(as.Date('1990-10-01'), as.Date('2011-10-01')),                                
                     breaks = seq(as.Date('1990-10-1'), by = "2 years", length.out = 22), 
                     date_labels = '%b-%y',
                     expand = c(0.01, 0.01)) +                                                                
        #ylab(bquote("Prediciton Location 2\nStreamflow Difference ("*m^3*"/day)")) +                              
        theme(axis.title = element_blank(),
              plot.margin = margin(t=-3,b=5,l=5,r=-11),                                                        
              plot.background = element_rect(fill = NA, color = NA)),                                         
      vp = vplayout(2,1))                                                                                     
print(Oct_MAR_ILR_cov_loc_2_plot +                                                                            
        scale_x_date(limits = c(as.Date('1990-10-01'), as.Date('2011-10-01')),                                
                     breaks = seq(as.Date('1990-10-1'), by = "2 years", length.out = 22),  
                     date_labels = '%b-%y',
                     expand = c(0.01, 0.01)) +                                                                
        theme(axis.text.y = element_blank(),                                                                  
              axis.title = element_blank(),                                                                 
              legend.key.size = unit(0.3, 'cm'),                                                              
              legend.position = c(0.5,0.9),                                                                   
              legend.text = element_text(size = axis_text_size),     
              legend.background = element_rect(color = 'black', size = 0.25),
              legend.margin = margin(t=2,b=2,l=2,r=2),
              plot.margin = margin(t=-3,b=5,l=21,r=-3),                                                        
              plot.background = element_rect(fill = NA, color = NA)),                                         
      vp = vplayout(2,2))                                                                                     
print(Oct_MAR_ILR_UCODE_loc_2_plot +                                                                          
        scale_x_date(limits = c(as.Date('1990-10-01'), as.Date('2011-10-01')),                                
                     breaks = seq(as.Date('1990-10-1'), by = "2 years", length.out = 22),  
                     date_labels = '%b-%y',
                     expand = c(0.01, 0.01)) +                                                                
        theme(axis.text.y = element_blank(),                                                                  
              axis.title = element_blank(),                                                                 
              legend.position = 'none',                                                                       
              plot.margin = margin(t=-3,b=5,l=13,r=5),                                                         
              plot.background = element_rect(fill = NA, color = NA)),                                         
      vp = vplayout(2,3))                                                                                     
graphics.off()    

# Aug Sep Oct Violin Difference Plots ----------------------------------
fig_name = 'Aug_Sep_Oct_Diff_violin'
if (fig_format == 'jpg'){
  jpeg(paste0(out_dir,fig_name,'.jpg'), width = fig_width, height = fig_height, units = fig_units,  res = 600)
} else if (fig_format == 'png'){
  png(paste0(out_dir,fig_name,'.png'), width = 7.48, height = 4.5, units = 'in',  res = 600)
} else if (fig_format == 'pdf'){
  #pdf(paste0(out_dir,fig_name,'.pdf'), width = 7.48, height = 4.5 )                                
}
grid.newpage()                                                                                                
pushViewport(viewport(layout = grid.layout(2,3)))                                                             
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)                                   
print(Aug_Sep_Oct_MAR_violin_loc_1_plot +                                                                            
        #ylab(bquote("Prediction Location 1\nStreamflow Difference ("*m^3*"/day)")) +
        theme(axis.text.x = element_blank(),
              legend.position = 'none',
              axis.title = element_blank(),
              #panel.background = element_rect(color = 'black', fill = 'gray90'),
              plot.margin = margin(t=5,b=13,l=5,r=-11),                                                        
              plot.background = element_rect(fill = NA, color = NA)),                                         
      vp = vplayout(1,1))                                                                                     
print(Aug_Sep_Oct_ILR_violin_loc_1_plot +                                                                            
        theme(axis.text.x = element_blank(),                                                                  
              axis.text.y = element_blank(),                                                                  
              axis.title = element_blank(),                                                                 
              legend.position = c(0.5, 0.93),
              legend.text = element_text(size = 5.5),
              legend.key.size = unit(0.2, 'cm'),
              legend.direction = 'horizontal',
              legend.background = element_rect(fill = 'white', color = 'black', size = 0.1),
              legend.margin = margin(t=2,b=2,l=2,r=2),
              legend.spacing.x = unit(0.3, 'mm'),
              #panel.background = element_rect(color = 'black', fill = 'gray90'),
              plot.margin = margin(t=5,b=13,l=21,r=-3),                                                        
              plot.background = element_rect(fill = NA, color = NA)),                                         
      vp = vplayout(1,2))                                                                                     
print(Aug_Sep_Oct_MAR_ILR_violin_loc_1_plot +                                                                          
        theme(axis.text.x = element_blank(),                                                                  
              axis.text.y = element_blank(),                                                                  
              axis.title = element_blank(),                                                                 
              legend.position = 'none',
              #panel.background = element_rect(color = 'black', fill = 'gray90'),
              plot.margin = margin(t=5,b=13,l=13,r=5),                                                         
              plot.background = element_rect(fill = NA, color = NA)),                                         
      vp = vplayout(1,3))                                                                                     
print(Aug_Sep_Oct_MAR_violin_loc_2_plot +                                                                            
        #ylab(bquote("Prediction Location 2\nStreamflow Difference ("*m^3*"/day)")) +                           
        theme(plot.margin = margin(t=-3,b=5,l=5,r=-11),
              legend.position = 'none',
              axis.title = element_blank(),
              #panel.background = element_rect(color = 'black', fill = 'gray90'),
              plot.background = element_rect(fill = NA, color = NA)),                                         
      vp = vplayout(2,1))                                                                                     
print(Aug_Sep_Oct_ILR_violin_loc_2_plot +                                                                            
        theme(axis.text.y = element_blank(),                                                                  
              axis.title = element_blank(),                                                                 
              legend.position = 'none',
              #panel.background = element_rect(color = 'black', fill = 'gray90'),
              plot.margin = margin(t=-3,b=5,l=21,r=-3),                                                        
              plot.background = element_rect(fill = NA, color = NA)),                                         
      vp = vplayout(2,2))                                                                                     
print(Aug_Sep_Oct_MAR_ILR_violin_loc_2_plot +                                                                          
        theme(axis.text.y = element_blank(),                                                                  
              axis.title = element_blank(),                                                                 
              legend.position = 'none',
              #panel.background = element_rect(color = 'black', fill = 'gray90'),
              plot.margin = margin(t=-3,b=5,l=13,r=5),                                                         
              plot.background = element_rect(fill = NA, color = NA)),                                         
      vp = vplayout(2,3))                                                                                     
graphics.off() 


# Uncertainty Range and Sensitivity ---------------------------------------
Scenario = c('MAR', 'ILR', 'MAR_ILR')

sppp_header = c('PREDICTION.NAME', 'PLOT.SYMBOL', 'Kx1', 'Kx2', 'Kx3', 'Kx4', 'Sy1', 'Sy3', 'RD_Mult', 'SMDF_Flood',
                'SMDF_A/G_WL', 'SMDF_A/G_CP', 'SMDF_P_WL', 'SMDF_P_CP', 'Kc_Alfalfa_Mult', 'Kc_Pasture_Mult')

UCODE_MAR_files = list.files(path = paste0(getwd(),'/UCODE_out/MAR/'), pattern = 'sppp', full.names = T)
UCODE_ILR_files = list.files(path = paste0(getwd(),'/UCODE_out/ILR/'), pattern = 'sppp', full.names = T)
UCODE_MAR_ILR_files = list.files(path = paste0(getwd(),'/UCODE_out/MAR_ILR/'), pattern = 'sppp', full.names = T)
Oct_Dates = seq(as.Date('1990-10-01'),as.Date('2011-09-30'), by = 'year')
for (j in 1:3){      #loop over scenarios
  for(i in 1:5){     #loop over calibrations
    eval(parse(text = paste0(Scenario[j],"_Cal_",i,"_Oct_sppp = read.table(file = UCODE_",Scenario[j],"_files[",i,"], header = T) %>% subset(grepl(PREDICTION.NAME,pattern = 'Diff_1_Oct'))")))
    eval(parse(text = paste0("names(",Scenario[j],"_Cal_",i,"_Oct_sppp) = sppp_header")))
    eval(parse(text = paste0(Scenario[j],"_Cal_",i,"_Oct_sppp$Date = Oct_Dates")))
    eval(parse(text = paste0(Scenario[j],"_Cal_",i,"_Oct_sppp_melt = melt(",Scenario[j],"_Cal_",i,"_Oct_sppp, id.vars = c('PREDICTION.NAME', 'PLOT.SYMBOL', 'Date'))")))
    
    eval(parse(text = paste0("UniRandPar_quantiles = apply(subset(UniRandPar_Diff_",Scenario[j],"_clean, Month== 'Oct' & Pred_Loc==1 & Date!='1990-10-01')[,seq(-1,-5)],1,FUN = function(x) as.data.frame(t(quantile(x, probs = c(0.025, 0.5, 0.975), na.rm = T))))")))
    eval(parse(text = paste0("UniRandPar_summary = cbind(subset(UniRandPar_Diff_",Scenario[j],"_clean, Month=='Oct' & Pred_Loc==1 & Date!='1990-10-01')[,seq(1,5)], 
                         unlist(lapply(UniRandPar_quantiles, `[[`, 1)), 
                         unlist(lapply(UniRandPar_quantiles, `[[`, 2)), 
                         unlist(lapply(UniRandPar_quantiles, `[[`, 3)))")))
    names(UniRandPar_summary) = c(names(UniRandPar_Diff_MAR_clean[seq(1,5)]), 'lower95_m3day', 'median_m3day', 'upper95_m3day')
    UniRandPar_summary$Method = 'Uncorrelated (Method 1)'
    UniRandPar_summary$Pred_Range_m3day = UniRandPar_summary$upper95_m3day - UniRandPar_summary$lower95_m3day
    
    eval(parse(text = paste0("CovRandPar_quantiles = apply(subset(CovRandPar_Diff_",Scenario[j],"_clean, Month=='Oct' & Date!='1990-10-01' & Pred_Loc==1 & Calibration=='Cal_",i,"')[,seq(-1,-6)],1,FUN = function(x) as.data.frame(t(quantile(x, probs = c(0.025, 0.5, 0.975), na.rm = T))))")))
    eval(parse(text = paste0("CovRandPar_summary = cbind(subset(CovRandPar_Diff_",Scenario[j],"_clean, Month=='Oct' & Date!='1990-10-01' & Pred_Loc==1 & Calibration=='Cal_",i,"')[,seq(1,6)], 
                             unlist(lapply(CovRandPar_quantiles, `[[`, 1)), 
                             unlist(lapply(CovRandPar_quantiles, `[[`, 2)), 
                             unlist(lapply(CovRandPar_quantiles, `[[`, 3)))")))
    names(CovRandPar_summary) = c(names(CovRandPar_Diff_MAR_clean[seq(1,6)]), 'lower95_m3day', 'median_m3day', 'upper95_m3day')
    CovRandPar_summary$Method = 'Correlated (Method 2)'
    CovRandPar_summary$Pred_Range_m3day = CovRandPar_summary$upper95_m3day - CovRandPar_summary$lower95_m3day
    
    eval(parse(text = paste0("UCODE_summary = subset(UCODE_",Scenario[j],"_diff, MONTH=='Oct' & INT.TYPE=='Simultaneous_2' & PRED.LOC == 1 & DATE!='1990-10-01' & CALIBRATION=='Cal_",i,"')")))
    UCODE_summary$Method = 'Linear Prediction (Method 3)'
    UCODE_summary$Pred_Range_m3day = 4*UCODE_summary$STANDARD.DEVIATION    # +/- 2 standard deviations 
    
    eval(parse(text = paste0(Scenario[j],"_Cal_",i,"_Comp = data.frame(Date = c(UniRandPar_summary$Date, CovRandPar_summary$Date, UCODE_summary$DATE),
                              Pred_Value = c(UniRandPar_summary$median_m3day, CovRandPar_summary$median_m3day, UCODE_summary$PREDICTED.VALUE),
                              Upper95 = c(UniRandPar_summary$upper95_m3day, CovRandPar_summary$upper95_m3day, UCODE_summary$PREDICTED.VALUE+2*UCODE_summary$STANDARD.DEVIATION),
                              Lower95 = c(UniRandPar_summary$lower95_m3day, CovRandPar_summary$lower95_m3day, UCODE_summary$PREDICTED.VALUE-2*UCODE_summary$STANDARD.DEVIATION),
                              Pred_Range_m3day = c(UniRandPar_summary$Pred_Range_m3day, CovRandPar_summary$Pred_Range_m3day, UCODE_summary$Pred_Range_m3day),
                              Method = c(UniRandPar_summary$Method, CovRandPar_summary$Method, UCODE_summary$Method))")))
    
    eval(parse(text = paste0(Scenario[j],"_Cal_",i,"_Comp$Method = factor(",Scenario[j],"_Cal_",i,"_Comp$Method, levels = c('Uncorrelated (Method 1)','Correlated (Method 2)', 'Linear Prediction (Method 3)'))")))
    eval(parse(text = paste0(Scenario[j],"_Cal_",i,"_Comp = ",Scenario[j],"_Cal_",i,"_Comp[order(",Scenario[j],"_Cal_",i,"_Comp$Method),]")))
    }
}

#MAR Uncertainty Range and Stacked Sensitivities
for (i in 1:5){
  eval(parse(text = paste0("temp_data = MAR_Cal_",i,"_Comp")))
  eval(parse(text = paste0("temp_sens_data = MAR_Cal_",i,"_Oct_sppp_melt")))
  temp_sens_data$norm_val = temp_sens_data$value / max(abs(temp_sens_data$value))      #normalize values
  temp_sens_data$norm_val = temp_sens_data$norm_val / max(aggregate(.~Date, subset(temp_sens_data, select = c('Date', 'norm_val')), FUN = function(x) sum(abs(x)))[2])   #scale values so the maximum value is equal to one when stacked
  
  Plot = ggplot(data = temp_data, aes(x = Date, y = Pred_Range_m3day, color = Method )) + 
    geom_bar(data = subset(temp_sens_data, variable%in%c('Kx1', 'Kx2', 'Kx3', 'Kx4')), aes(x=Date, y=abs(norm_val*6E4), group = variable, fill = variable),color = 'black', stat = 'identity', position = 'stack', width =100, size = 0.2) +
    geom_line(size = line_size/2) +
    geom_point(size = point_size/2) +
    geom_rect(aes(xmin = as.Date('1990-10-01'), xmax = as.Date('1992-10-01'), ymin = 7.75E4, ymax = 7.99E4), fill = '#ff8282', color = 'black', size = 0.2) +
    geom_rect(aes(xmin = as.Date('1993-10-01'), xmax = as.Date('1994-10-01'), ymin = 7.75E4, ymax = 7.99E4), fill = '#ff8282', color = 'black', size = 0.2) +
    geom_rect(aes(xmin = as.Date('1994-10-01'), xmax = as.Date('1999-10-01'), ymin = 7.75E4, ymax = 7.99E4), fill = 'skyblue', color = 'black', size = 0.2) +
    geom_rect(aes(xmin = as.Date('2000-10-01'), xmax = as.Date('2002-10-01'), ymin = 7.75E4, ymax = 7.99E4), fill = '#ff8282', color = 'black', size = 0.2) +
    geom_rect(aes(xmin = as.Date('2005-10-01'), xmax = as.Date('2006-10-01'), ymin = 7.75E4, ymax = 7.99E4), fill = 'skyblue', color = 'black', size = 0.2) +
    geom_rect(aes(xmin = as.Date('2006-10-01'), xmax = as.Date('2009-10-01'), ymin = 7.75E4, ymax = 7.99E4), fill = '#ff8282', color = 'black', size = 0.2) +
    geom_rect(aes(xmin = as.Date('2010-10-01'), xmax = as.Date('2011-10-01'), ymin = 7.75E4, ymax = 7.99E4), fill = 'skyblue', color = 'black', size = 0.2) +
    ylab(bquote("Width of 95% Confidence Interval ("*m^3*"/day)")) +
    #scale_color_manual(values = c('#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02')) +
    scale_color_brewer(palette = 'Set1') +
    scale_fill_brewer(palette = 'Set2') +
    #scale_fill_manual(values = c('#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02')) +
    scale_x_date(limits = c(as.Date('1990-10-01'),as.Date('2011-10-01')),
                 breaks = seq(as.Date('1990-10-01'),as.Date('2011-10-01'), by = '2 years'),
                 date_labels = '%Y', expand = c(0.01,0.01)) +
    scale_y_continuous(limits = c(-2E4,8E5),
                       breaks = seq(0,8E4,by=2E4),
                       labels = c('0',paste0(seq(2,8, by = 2),'E4')),
                       expand = c(0,0),
                       sec.axis = sec_axis(~., breaks = seq(0,1,by = 0.25) *6E4, labels = seq(0,1,by = 0.25))) +
    coord_cartesian(ylim = c(0,8E4)) +
    theme_few() +
    theme(axis.title.x = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_line(size = 0.25),
          legend.position = 'none',
          legend.title = element_blank(),
          legend.background = element_rect(fill = NA))
  eval(parse(text = paste0("MAR_Range_and_Sens_Cal_",i,"_plot = Plot")))
}

#ILR Uncertainty Range and Stacked Sensitivities
for (i in 1:5){
  eval(parse(text = paste0("temp_data = ILR_Cal_",i,"_Comp")))
  eval(parse(text = paste0("temp_sens_data = subset(ILR_Cal_",i,"_Oct_sppp_melt, Date!= '1990-10-01')")))
  temp_sens_data$norm_val = temp_sens_data$value / max(abs(temp_sens_data$value))      #normalize values
  temp_sens_data$norm_val = temp_sens_data$norm_val / max(aggregate(.~Date, subset(temp_sens_data, select = c('Date', 'norm_val')), FUN = function(x) sum(abs(x)))[2])   #scale values so the maximum value is equal to one when stacked
  
  Plot = ggplot(data = temp_data, aes(x = Date, y = Pred_Range_m3day, color = Method )) + 
    geom_bar(data = subset(temp_sens_data, variable%in%c('Kx1', 'Kx2', 'Kx3', 'Kx4')), aes(x=Date, y=abs(norm_val*6E4), group = variable, fill = variable),color = 'black', stat = 'identity', position = 'stack', width =100, size = 0.2) +
    geom_line(size = line_size/2) +
    geom_point(size = point_size/2) +
    geom_rect(aes(xmin = as.Date('1990-10-01'), xmax = as.Date('1992-10-01'), ymin = 7.75E4, ymax = 7.99E4), fill = '#ff8282', color = 'black', size = 0.2) +
    geom_rect(aes(xmin = as.Date('1993-10-01'), xmax = as.Date('1994-10-01'), ymin = 7.75E4, ymax = 7.99E4), fill = '#ff8282', color = 'black', size = 0.2) +
    geom_rect(aes(xmin = as.Date('1994-10-01'), xmax = as.Date('1999-10-01'), ymin = 7.75E4, ymax = 7.99E4), fill = 'skyblue', color = 'black', size = 0.2) +
    geom_rect(aes(xmin = as.Date('2000-10-01'), xmax = as.Date('2002-10-01'), ymin = 7.75E4, ymax = 7.99E4), fill = '#ff8282', color = 'black', size = 0.2) +
    geom_rect(aes(xmin = as.Date('2005-10-01'), xmax = as.Date('2006-10-01'), ymin = 7.75E4, ymax = 7.99E4), fill = 'skyblue', color = 'black', size = 0.2) +
    geom_rect(aes(xmin = as.Date('2006-10-01'), xmax = as.Date('2009-10-01'), ymin = 7.75E4, ymax = 7.99E4), fill = '#ff8282', color = 'black', size = 0.2) +
    geom_rect(aes(xmin = as.Date('2010-10-01'), xmax = as.Date('2011-10-01'), ymin = 7.75E4, ymax = 7.99E4), fill = 'skyblue', color = 'black', size = 0.2) +
    ylab(bquote("Streamflow Difference ("*m^3*"/day)")) +
    scale_color_brewer(palette = 'Set1') +
    scale_fill_brewer(palette = 'Set2') +
    scale_x_date(limits = c(as.Date('1990-10-01'),as.Date('2011-10-01')),
                 breaks = seq(as.Date('1990-10-01'),as.Date('2011-10-01'), by = '2 years'),
                 date_labels = '%Y', expand = c(0.01,0.01)) +
    scale_y_continuous(limits = c(-2E4,8E5),
                       breaks = seq(0,8E4,by=2E4),
                       labels = c('0',paste0(seq(2,8, by = 2),'E4')),
                       expand = c(0,0),
                       sec.axis = sec_axis(~., breaks = seq(0,1,by = 0.25) *6E4, labels = seq(0,1,by = 0.25))) +
    coord_cartesian(ylim = c(0,8E4)) +
    theme_few() +
    theme(axis.title.x = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_line(size = 0.25),
          legend.position = 'none',
          legend.title = element_blank(),
          legend.background = element_rect(fill = NA))
  eval(parse(text = paste0("ILR_Range_and_Sens_Cal_",i,"_plot = Plot")))
}

#MAR_ILR Uncertainty Range and Stacked Sensitivities
for (i in 1:5){
  eval(parse(text = paste0("temp_data = MAR_ILR_Cal_",i,"_Comp")))
  eval(parse(text = paste0("temp_sens_data = subset(MAR_ILR_Cal_",i,"_Oct_sppp_melt, Date!= '1990-10-01')")))
  temp_sens_data$norm_val = temp_sens_data$value / max(abs(temp_sens_data$value))      #normalize values
  temp_sens_data$norm_val = temp_sens_data$norm_val / max(aggregate(.~Date, subset(temp_sens_data, select = c('Date', 'norm_val')), FUN = function(x) sum(abs(x)))[2])   #scale values so the maximum value is equal to one when stacked
  
  Plot = ggplot(data = temp_data, aes(x = Date, y = Pred_Range_m3day, color = Method )) + 
    geom_bar(data = subset(temp_sens_data, variable%in%c('Kx1', 'Kx2', 'Kx3', 'Kx4')), aes(x=Date, y=abs(norm_val*6E4), group = variable, fill = variable),color = 'black', stat = 'identity', position = 'stack', width =100, size = 0.2) +
    geom_line(size = line_size/2) +
    geom_point(size = point_size/2) +
    geom_rect(aes(xmin = as.Date('1990-10-01'), xmax = as.Date('1992-10-01'), ymin = 7.75E4, ymax = 7.99E4), fill = '#ff8282', color = 'black', size = 0.2) +
    geom_rect(aes(xmin = as.Date('1993-10-01'), xmax = as.Date('1994-10-01'), ymin = 7.75E4, ymax = 7.99E4), fill = '#ff8282', color = 'black', size = 0.2) +
    geom_rect(aes(xmin = as.Date('1994-10-01'), xmax = as.Date('1999-10-01'), ymin = 7.75E4, ymax = 7.99E4), fill = 'skyblue', color = 'black', size = 0.2) +
    geom_rect(aes(xmin = as.Date('2000-10-01'), xmax = as.Date('2002-10-01'), ymin = 7.75E4, ymax = 7.99E4), fill = '#ff8282', color = 'black', size = 0.2) +
    geom_rect(aes(xmin = as.Date('2005-10-01'), xmax = as.Date('2006-10-01'), ymin = 7.75E4, ymax = 7.99E4), fill = 'skyblue', color = 'black', size = 0.2) +
    geom_rect(aes(xmin = as.Date('2006-10-01'), xmax = as.Date('2009-10-01'), ymin = 7.75E4, ymax = 7.99E4), fill = '#ff8282', color = 'black', size = 0.2) +
    geom_rect(aes(xmin = as.Date('2010-10-01'), xmax = as.Date('2011-10-01'), ymin = 7.75E4, ymax = 7.99E4), fill = 'skyblue', color = 'black', size = 0.2) +
    ylab(bquote("Streamflow Difference ("*m^3*"/day)")) +
    scale_color_brewer(palette = 'Set1') +
    scale_fill_brewer(palette = 'Set2') +
    scale_x_date(limits = c(as.Date('1990-10-01'),as.Date('2011-10-01')),
                 breaks = seq(as.Date('1990-10-01'),as.Date('2011-10-01'), by = '2 years'),
                 date_labels = '%Y', expand = c(0.01,0.01)) +
    scale_y_continuous(limits = c(-2E4,8E5),
                       breaks = seq(0,8E4,by=2E4),
                       labels = c('0',paste0(seq(2,8, by = 2),'E4')),
                       expand = c(0,0),
                       sec.axis = sec_axis(~., breaks = seq(0,1,by = 0.25) *6E4, labels = seq(0,1,by = 0.25))) +
    coord_cartesian(ylim = c(0,8E4)) +
    theme_few() +
    theme(axis.title.x = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_line(size = 0.25),
          legend.position = 'none',
          legend.title = element_blank(),
          legend.background = element_rect(fill = NA))
  eval(parse(text = paste0("MAR_ILR_Range_and_Sens_Cal_",i,"_plot = Plot")))
}

fig_name = 'Prediciton_Range_with_Sensitivity'
if (fig_format == 'jpg'){
  jpeg(paste0(out_dir,fig_name,'.jpg'), width = fig_width, height = fig_height, units = fig_units,  res = 600)
} else if (fig_format == 'png'){
  png(paste0(out_dir,fig_name,'.png'), width = 6.75, height = 7.5, units = 'in',  res = 600)
} else if (fig_format == 'pdf'){
  #pdf(paste0(out_dir,fig_name,'.pdf'), width = 7.48, height = 4.5 )                                
}
grid.newpage()                                                                                                
pushViewport(viewport(layout = grid.layout(5,3)))                                                             
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)                                   
for (j in 1:3){      #loop over scenarios
  for(i in 1:5){     #loop over calibrations
    eval(parse(text = paste0("print(",Scenario[j],"_Range_and_Sens_Cal_",i,"_plot, vp = vplayout(",i,",",j,"))")))
  }
}
graphics.off()


# Uncorrelated and Correlated Parameter Set Results Comparison (Oct) ------------
Scenario = c('MAR', 'ILR', 'MAR_ILR')
for (j in 1:2){   #Prediciton Locations
for (i in 1:3){   #Scenarios
eval(parse(text = paste0("quantiles_UniRandPar_",Scenario[i],"_Loc_",j," = apply(subset(UniRandPar_Diff_",Scenario[i],"_clean,Pred_Loc==",j," & Month=='Oct' & Date != '1990-10-01')[,seq(-1,-5)],1,FUN = function(x) as.data.frame(t(quantile(x, probs = c(0.025, 0.5, 0.975), na.rm = T))))")))
eval(parse(text = paste0("summary_UniRandPar_",Scenario[i],"_Loc_",j," = cbind(subset(UniRandPar_Diff_",Scenario[i],"_clean,Pred_Loc==",j," & Month=='Oct'  & Date != '1990-10-01' )[,seq(1,5)], 
                         unlist(lapply(quantiles_UniRandPar_",Scenario[i],"_Loc_",j,", `[[`, 1)), 
                         unlist(lapply(quantiles_UniRandPar_",Scenario[i],"_Loc_",j,", `[[`, 2)), 
                         unlist(lapply(quantiles_UniRandPar_",Scenario[i],"_Loc_",j,", `[[`, 3)))")))
eval(parse(text = paste0("names(summary_UniRandPar_",Scenario[i],"_Loc_",j,") = c(names(UniRandPar_Diff_",Scenario[i],"_clean[seq(1,5)]), 'lower95_m3day', 'median_m3day', 'upper95_m3day')")))

eval(parse(text = paste0("quantiles_CovRandPar_",Scenario[i],"_Loc_",j," = apply(subset(CovRandPar_Diff_",Scenario[i],"_clean,Pred_Loc==",j," & Month=='Oct' & Date != '1990-10-01')[,seq(-1,-6)],1,FUN = function(x) as.data.frame(t(quantile(x, probs = c(0.025, 0.5, 0.975), na.rm = T))))")))
eval(parse(text = paste0("summary_CovRandPar_",Scenario[i],"_Loc_",j," = cbind(subset(CovRandPar_Diff_",Scenario[i],"_clean,Pred_Loc==",j," & Month=='Oct' & Date != '1990-10-01')[,seq(1,6)], 
                         unlist(lapply(quantiles_CovRandPar_",Scenario[i],"_Loc_",j,", `[[`, 1)), 
                         unlist(lapply(quantiles_CovRandPar_",Scenario[i],"_Loc_",j,", `[[`, 2)), 
                         unlist(lapply(quantiles_CovRandPar_",Scenario[i],"_Loc_",j,", `[[`, 3)))")))
eval(parse(text = paste0("names(summary_CovRandPar_",Scenario[i],"_Loc_",j,") = c(names(CovRandPar_Diff_",Scenario[i],"_clean[seq(1,6)]), 'lower95_m3day', 'median_m3day', 'upper95_m3day')")))
eval(parse(text = paste0("CovRandPar_",Scenario[i],"_Loc_",j,"_Summary_Avg = data.frame(Date = unique(summary_CovRandPar_",Scenario[i],"_Loc_1$Date),
                                              median_m3day = aggregate(.~Date, subset(summary_CovRandPar_",Scenario[i],"_Loc_",j,", select = c('Date', 'median_m3day')), FUN = mean)[2],
                                              lower95_m3day = aggregate(.~Date, subset(summary_CovRandPar_",Scenario[i],"_Loc_",j,", select = c('Date', 'lower95_m3day')), FUN = min)[2],
                                              upper95_m3day = aggregate(.~Date, subset(summary_CovRandPar_",Scenario[i],"_Loc_",j,", select = c('Date', 'upper95_m3day')), FUN = max)[2])")))

eval(parse(text = paste0("summary_UniRandPar_",Scenario[i],"_Loc_",j,"$Method = 'Uncorrelated' ")))
eval(parse(text = paste0("CovRandPar_",Scenario[i],"_Loc_",j,"_Summary_Avg$Method = 'Correlated'")))
eval(parse(text = paste0(Scenario[i],"_Loc_",j,"_Comp = rbind(subset(summary_UniRandPar_",Scenario[i],"_Loc_",j,", select = names(CovRandPar_",Scenario[i],"_Loc_",j,"_Summary_Avg)), CovRandPar_",Scenario[i],"_Loc_",j,"_Summary_Avg) ")))

eval(parse(text = paste0("temp_data = ",Scenario[i],"_Loc_",j,"_Comp")))
temp_data$Method = factor(temp_data$Method, levels = c('Uncorrelated', 'Correlated'))
temp_data = temp_data[order(temp_data$Method),]

if (i==1) {
Plot = ggplot(data = NULL) + 
  geom_hline(yintercept = 0) +
  geom_ribbon(data = subset(temp_data, Method=='Uncorrelated'),aes(x = Date, ymin = lower95_m3day, ymax = upper95_m3day, fill = 'Uncorrelated'), show.legend = F, alpha = 0.25) +
  geom_line(data = subset(temp_data, Method=='Uncorrelated'), aes(x = Date, y = median_m3day, color = 'Uncorrelated'), size = line_size) +
  geom_point(data = subset(temp_data, Method=='Uncorrelated'), aes(x = Date, y = median_m3day, color = 'Uncorrelated'),size = point_size, shape = 21) +
  geom_ribbon(data = subset(temp_data, Method=='Correlated'),aes(x = Date, ymin = lower95_m3day, ymax = upper95_m3day, fill = 'Correlated'), show.legend = F,  alpha = 0.25) +
  geom_line(data = subset(temp_data, Method=='Correlated'), aes(x = Date, y = median_m3day, color = 'Correlated'), size = line_size) +
  geom_point(data = subset(temp_data, Method=='Correlated'), aes(x = Date, y = median_m3day, color = 'Correlated'),size = point_size, shape = 22) +
  geom_rect(aes(xmin = as.Date('1990-10-01'), xmax = as.Date('1992-10-01'), ymin = -1E4 , ymax = -7.5E3), fill = '#ff8282', color = 'black', size = 0.2) +
  geom_rect(aes(xmin = as.Date('1993-10-01'), xmax = as.Date('1994-10-01'), ymin = -1E4 , ymax = -7.5E3), fill = '#ff8282', color = 'black', size = 0.2) +
  geom_rect(aes(xmin = as.Date('1994-10-01'), xmax = as.Date('1999-10-01'), ymin = -1E4 , ymax = -7.5E3), fill = 'skyblue', color = 'black', size = 0.2) +
  geom_rect(aes(xmin = as.Date('2000-10-01'), xmax = as.Date('2002-10-01'), ymin = -1E4 , ymax = -7.5E3), fill = '#ff8282', color = 'black', size = 0.2) +
  geom_rect(aes(xmin = as.Date('2005-10-01'), xmax = as.Date('2006-10-01'), ymin = -1E4 , ymax = -7.5E3), fill = 'skyblue', color = 'black', size = 0.2) +
  geom_rect(aes(xmin = as.Date('2006-10-01'), xmax = as.Date('2009-10-01'), ymin = -1E4 , ymax = -7.5E3), fill = '#ff8282', color = 'black', size = 0.2) +
  geom_rect(aes(xmin = as.Date('2010-10-01'), xmax = as.Date('2011-10-01'), ymin = -1E4 , ymax = -7.5E3), fill = 'skyblue', color = 'black', size = 0.2) +
  ylab(bquote("Streamflow Difference ("*m^3*"/day)")) +
  scale_color_manual(values = c('green4', 'darkorchid4'), breaks =levels(temp_data$Method)) +
  scale_fill_manual(values = c('green4', 'darkorchid4')) +
  scale_x_date(limits = c(as.Date('1990-10-01'),as.Date('2011-10-01')),
               breaks = seq(as.Date('1990-10-01'),as.Date('2011-10-01'), by = '2 years'),
               date_labels = '%Y', expand = c(0.01,0.01)) +
    scale_y_continuous(limits = c(-1E4,7E4), breaks = seq(-1E4,7E4,by=2E4), labels = paste0(seq(-1,7, by = 2),'E4'),expand = c(0,0)) +
  theme_few() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = axis_title_size),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = axis_text_size),
        legend.direction = 'horizontal',
        legend.position = c(0.5,0.9),
        legend.title = element_blank(),
        legend.background = element_rect(fill = NA))

eval(parse(text = paste0("Uni_Cov_",Scenario[i],"_Comp_Loc_",j,"_Plot = Plot")))
} else if (i==2){
  Plot = ggplot(data = NULL) + 
    geom_hline(yintercept = 0) +
    geom_ribbon(data = subset(temp_data, Method=='Uncorrelated'),aes(x = Date, ymin = lower95_m3day, ymax = upper95_m3day, fill = 'Uncorrelated'), show.legend = F, alpha = 0.25) +
    geom_line(data = subset(temp_data, Method=='Uncorrelated'), aes(x = Date, y = median_m3day, color = 'Uncorrelated'), size = line_size) +
    geom_point(data = subset(temp_data, Method=='Uncorrelated'), aes(x = Date, y = median_m3day, color = 'Uncorrelated'),size = point_size, shape = 21) +
    geom_ribbon(data = subset(temp_data, Method=='Correlated'),aes(x = Date, ymin = lower95_m3day, ymax = upper95_m3day, fill = 'Correlated'), show.legend = F,  alpha = 0.25) +
    geom_line(data = subset(temp_data, Method=='Correlated'), aes(x = Date, y = median_m3day, color = 'Correlated'), size = line_size) +
    geom_point(data = subset(temp_data, Method=='Correlated'), aes(x = Date, y = median_m3day, color = 'Correlated'),size = point_size, shape = 22) +
    geom_rect(aes(xmin = as.Date('1990-10-01'), xmax = as.Date('1992-10-01'), ymin = -50000 , ymax = -44000), fill = '#ff8282', color = 'black', size = 0.2) +
    geom_rect(aes(xmin = as.Date('1993-10-01'), xmax = as.Date('1994-10-01'), ymin = -50000 , ymax = -44000), fill = '#ff8282', color = 'black', size = 0.2) +
    geom_rect(aes(xmin = as.Date('1994-10-01'), xmax = as.Date('1999-10-01'), ymin = -50000 , ymax = -44000), fill = 'skyblue', color = 'black', size = 0.2) +
    geom_rect(aes(xmin = as.Date('2000-10-01'), xmax = as.Date('2002-10-01'), ymin = -50000 , ymax = -44000), fill = '#ff8282', color = 'black', size = 0.2) +
    geom_rect(aes(xmin = as.Date('2005-10-01'), xmax = as.Date('2006-10-01'), ymin = -50000 , ymax = -44000), fill = 'skyblue', color = 'black', size = 0.2) +
    geom_rect(aes(xmin = as.Date('2006-10-01'), xmax = as.Date('2009-10-01'), ymin = -50000 , ymax = -44000), fill = '#ff8282', color = 'black', size = 0.2) +
    geom_rect(aes(xmin = as.Date('2010-10-01'), xmax = as.Date('2011-10-01'), ymin = -50000 , ymax = -44000), fill = 'skyblue', color = 'black', size = 0.2) +
    ylab(bquote("Streamflow Difference ("*m^3*"/day)")) +
    scale_color_manual(values = c('green4', 'darkorchid4'), breaks =levels(temp_data$Method)) +
    scale_fill_manual(values = c('green4', 'darkorchid4')) +
    scale_x_date(limits = c(as.Date('1990-10-01'),as.Date('2011-10-01')),
                 breaks = seq(as.Date('1990-10-01'),as.Date('2011-10-01'), by = '2 years'),
                 date_labels = '%Y', expand = c(0.01,0.01)) +
    scale_y_continuous(limits = c(-5E4,7E4), breaks = seq(-5E4,7E4,by=2E4), labels = paste0(seq(-5,7,by = 2),'E4'), expand = c(0,0)) +
    theme_few() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_text(size = axis_title_size),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text = element_text(size = axis_text_size),
          legend.direction = 'horizontal',
          legend.position = c(0.5,0.9),
          legend.title = element_blank(),
          legend.background = element_rect(fill = NA))
  eval(parse(text = paste0("Uni_Cov_",Scenario[i],"_Comp_Loc_",j,"_Plot = Plot")))
} else if (i==3){
  Plot = ggplot(data = NULL) + 
    geom_hline(yintercept = 0) +
    geom_ribbon(data = subset(temp_data, Method=='Uncorrelated'),aes(x = Date, ymin = lower95_m3day, ymax = upper95_m3day, fill = 'Uncorrelated'), show.legend = F, alpha = 0.25) +
    geom_line(data = subset(temp_data, Method=='Uncorrelated'), aes(x = Date, y = median_m3day, color = 'Uncorrelated'), size = line_size) +
    geom_point(data = subset(temp_data, Method=='Uncorrelated'), aes(x = Date, y = median_m3day, color = 'Uncorrelated'),size = point_size, shape = 21) +
    geom_ribbon(data = subset(temp_data, Method=='Correlated'),aes(x = Date, ymin = lower95_m3day, ymax = upper95_m3day, fill = 'Correlated'), show.legend = F,  alpha = 0.25) +
    geom_line(data = subset(temp_data, Method=='Correlated'), aes(x = Date, y = median_m3day, color = 'Correlated'), size = line_size) +
    geom_point(data = subset(temp_data, Method=='Correlated'), aes(x = Date, y = median_m3day, color = 'Correlated'),size = point_size, shape = 22) +
    geom_rect(aes(xmin = as.Date('1990-10-01'), xmax = as.Date('1992-10-01'), ymin = -50000 , ymax = -44000), fill = '#ff8282', color = 'black', size = 0.2) +
    geom_rect(aes(xmin = as.Date('1993-10-01'), xmax = as.Date('1994-10-01'), ymin = -50000 , ymax = -44000), fill = '#ff8282', color = 'black', size = 0.2) +
    geom_rect(aes(xmin = as.Date('1994-10-01'), xmax = as.Date('1999-10-01'), ymin = -50000 , ymax = -44000), fill = 'skyblue', color = 'black', size = 0.2) +
    geom_rect(aes(xmin = as.Date('2000-10-01'), xmax = as.Date('2002-10-01'), ymin = -50000 , ymax = -44000), fill = '#ff8282', color = 'black', size = 0.2) +
    geom_rect(aes(xmin = as.Date('2005-10-01'), xmax = as.Date('2006-10-01'), ymin = -50000 , ymax = -44000), fill = 'skyblue', color = 'black', size = 0.2) +
    geom_rect(aes(xmin = as.Date('2006-10-01'), xmax = as.Date('2009-10-01'), ymin = -50000 , ymax = -44000), fill = '#ff8282', color = 'black', size = 0.2) +
    geom_rect(aes(xmin = as.Date('2010-10-01'), xmax = as.Date('2011-10-01'), ymin = -50000 , ymax = -44000), fill = 'skyblue', color = 'black', size = 0.2) +
    ylab(bquote("Streamflow Difference ("*m^3*"/day)")) +
    scale_color_manual(values = c('green4', 'darkorchid4'), breaks =levels(temp_data$Method)) +
    scale_fill_manual(values = c('green4', 'darkorchid4')) +
    scale_x_date(limits = c(as.Date('1990-10-01'),as.Date('2011-10-01')),
                 breaks = seq(as.Date('1990-10-01'),as.Date('2011-10-01'), by = '2 years'),
                 date_labels = '%Y', expand = c(0.01,0.01)) +
    scale_y_continuous(limits = c(-5E4,9E5), breaks = seq(-5E4,9E4,by=2E4), labels = paste0(seq(-5,9,by = 2),'E4'), expand = c(0,0)) +
    coord_cartesian(ylim = c(-5E4,9E4)) +
    theme_few() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_text(size = axis_title_size),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text = element_text(size = axis_text_size),
          legend.direction = 'horizontal',
          legend.position = c(0.5,0.9),
          legend.title = element_blank(),
          legend.background = element_rect(fill = NA))
  eval(parse(text = paste0("Uni_Cov_",Scenario[i],"_Comp_Loc_",j,"_Plot = Plot")))
}
}
}

fig_name = 'Methods_1_2_Comp'                                                                           
if (fig_format == 'jpg'){                                                                                     
  jpeg(paste0(out_dir,fig_name,'.jpg'), width = fig_width, height = fig_height, units = fig_units,  res = 600)
} else if (fig_format == 'png'){                                                                              
  png(paste0(out_dir,fig_name,'.png'), width = 3.25, height = 4, units = fig_units,  res = 600) 
} else if (fig_format == 'pdf'){                                                                              
  pdf(paste0(out_dir,fig_name,'.pdf'), width = 7.48, height = 4.5 )                                          
}                                                                                                             
grid.newpage()                                                                                                
pushViewport(viewport(layout = grid.layout(3,2)))                                                             
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)                                   
print(Uni_Cov_MAR_Comp_Loc_1_Plot + 
        theme(axis.text = element_blank(),
              axis.title = element_blank(),
              legend.text = element_text(size = 6),
              legend.key.size = unit(10, 'pt'),
              legend.direction = 'vertical',
              legend.position = c(0.3, 0.87)),vp = vplayout(1,1))
print(Uni_Cov_ILR_Comp_Loc_1_Plot + 
        theme(axis.text = element_blank(),
              axis.title = element_blank(),
              legend.position = 'none'),vp = vplayout(2,1))
print(Uni_Cov_MAR_ILR_Comp_Loc_1_Plot + 
        theme(axis.text = element_blank(),
              axis.title = element_blank(),
              legend.position = 'none'),vp = vplayout(3,1))
print(Uni_Cov_MAR_Comp_Loc_2_Plot + 
        theme(axis.text = element_blank(),
              axis.title = element_blank(),
              legend.position = 'none'),vp = vplayout(1,2))
print(Uni_Cov_ILR_Comp_Loc_2_Plot + 
        theme(axis.text = element_blank(),
              axis.title = element_blank(),
              legend.position = 'none'),vp = vplayout(2,2))
print(Uni_Cov_MAR_ILR_Comp_Loc_2_Plot + 
        theme(axis.text = element_blank(),
              axis.title = element_blank(),
              legend.position = 'none'),vp = vplayout(3,2))
graphics.off()




# Precipitation vs October Prediciton Uncertainty ----------------------------------------
Scenario = c('MAR', 'ILR', 'MAR_ILR')
SWBM_Monthly_m3$Season = rep(c(rep('Winter', 6), rep('Summer',6)),21)
SWBM_Annual_m3 = aggregate(.~WY+Season,SWBM_Monthly_m3[,!names(SWBM_Monthly_m3)%in%'Month'], FUN = sum)

Oct_Mar_Precip = subset(SWBM_Annual_m3, Season=='Winter', select = 'Precipitation')

test = data.frame(Precip = Oct_Mar_Precip$Precipitation[-21],
                  UniRandPar_MAR = UniRandPar_MAR_summary$median_m3day)

ggplot(test, aes(x = Precip, y = UniRandPar_MAR)) + geom_point()

# Legacy Plotting Code Below ----------------------------------------------------

UniRandPar_Diff_Stats
ggplot(data = NULL) +
  geom_line(data = subset(UniRandPar_Diff_Stats, WY!= 1991), aes(x = StartingDates[seq(13,252, by = 12)], y = MAR_mean), color = 'red') +
  geom_ribbon(data = subset(UniRandPar_Diff_Stats, WY!= 1991), aes(x = StartingDates[seq(13,252, by = 12)], ymin =  MAR_mean-2*MAR_sd, ymax =  MAR_mean+2*MAR_sd), alpha = 0.5, fill = 'red') +
  geom_line(data = summary_UniRandPar_MAR_Loc_1, aes( x = Date, y = median_m3day), color = 'green') +
  geom_ribbon(data = summary_UniRandPar_MAR_Loc_1, aes( x = Date, ymin = lower95_m3day, ymax = upper95_m3day), alpha = 0.5, fill = 'green')

# 
# # October UniRandPar Plots ------------------------------------------------
# #pdf(paste0('UniRandPar_Oct_plots.pdf'), width = 13, height = 12)
# png(paste0('UniRandPar_Oct_plots.png'), width = 8, height = 6, res = 600, units = fig_units)
# grid.newpage()
# pushViewport(viewport(layout = grid.layout(4,3)))
# vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
# print(Oct_uni_Streamflow_loc_1_plot + 
#         theme(axis.text.x = element_blank(),
#               axis.title.y = element_text(size = 18),
#               plot.margin = margin(t =10, b= 10, l=15, r = 10)),
#       vp = vplayout(1,1))
# print(Oct_MAR_uni_loc_1_plot +
#         theme(axis.text.x = element_blank(),
#               axis.title.y = element_text(size = 18),
#               plot.margin = margin(t =10, b= 10, l=15, r = 10)),
#       vp = vplayout(2,1))
# print(Oct_ILR_uni_loc_1_plot +
#         theme(axis.text.x = element_blank(),
#               axis.title.y = element_text(size = 18),
#               plot.margin = margin(t =10, b= 10, l=15, r = 10)),
#       vp = vplayout(3,1))
# print(Oct_MAR_ILR_uni_loc_1_plot +
#         theme(axis.text.x = element_text(size = 18,
#                                          angle = 45,
#                                          hjust = 1,
#                                          vjust = 0.5),
#               axis.title.y = element_text(size = 18),
#               plot.margin = margin(t =10, b= 10, l=15, r = 10)),
#       vp = vplayout(4,1))
# print(Oct_uni_Streamflow_loc_2_plot +
#         theme(axis.text.x = element_blank(),
#               axis.text.y = element_blank(),
#               axis.title.y = element_blank(),
#               plot.margin = margin(t =10, b= 10, l=15, r = 10)),
#       vp = vplayout(1,2))
# print(Oct_MAR_uni_loc_2_plot +
#         theme(axis.text.x = element_blank(),
#               axis.text.y = element_blank(),
#               axis.title.y = element_blank(),
#               plot.margin = margin(t =10, b= 10, l=15, r = 10)),
#       vp = vplayout(2,2))
# print(Oct_ILR_uni_loc_2_plot +
#         theme(axis.title.y = element_blank(),
#               axis.text.y = element_blank(),
#               axis.text.x = element_blank(),
#               plot.margin = margin(t =10, b= 10, l=15, r = 10)),
#       vp = vplayout(3,2))
# print(Oct_MAR_ILR_uni_loc_2_plot +
#         theme(axis.text.x = element_text(size = 18,
#                                          angle = 45,
#                                          hjust = 1,
#                                          vjust = 0.5),
#               axis.text.y = element_blank(),
#               axis.title.y = element_blank(),
#               plot.margin = margin(t =10, b= 10, l=15, r = 10)),
#       vp = vplayout(4,2))
# print(Oct_uni_Streamflow_loc_3_plot +
#         theme(axis.title.y = element_blank(),
#               axis.text.y = element_blank(),
#               axis.text.x = element_blank(),
#               plot.margin = margin(t =10, b= 10, l=15, r = 10)),
#       vp = vplayout(1,3))
# print(Oct_MAR_uni_loc_3_plot +
#         theme(axis.text.x = element_blank(),
#               axis.text.y = element_blank(),
#               axis.title.y = element_blank(),
#               plot.margin = margin(t =10, b= 10, l=15, r = 10)),
#       vp = vplayout(2,3))
# print(Oct_ILR_uni_loc_3_plot +
#         theme(axis.title.y = element_blank(),
#               axis.text.y = element_blank(),
#               axis.text.x = element_blank(),
#               plot.margin = margin(t =10, b= 10, l=15, r = 10)),
#       vp = vplayout(3,3))
# print(Oct_MAR_ILR_uni_loc_3_plot +
#         theme(axis.text.x = element_text(size = 18,
#                                          angle = 45,
#                                          hjust = 1,
#                                          vjust = 0.5),
#               axis.text.y = element_blank(),
#               axis.title.y = element_blank(),
#               plot.margin = margin(t =10, b= 10, l=15, r = 10)),
#       vp = vplayout(4,3))
# # print(Oct_uni_Streamflow_loc_4_plot +
# #         theme(axis.title.y = element_blank(),
# #               axis.text.y = element_blank(),
# #               axis.text.x = element_blank()),
# #       vp = vplayout(1,4))
# # print(Oct_MAR_uni_loc_4_plot + 
# #         theme(axis.title.y = element_blank(),
# #               axis.text.y = element_blank(),
# #               axis.text.x = element_blank()),
# #       vp = vplayout(2,4))
# # print(Oct_ILR_uni_loc_4_plot + 
# #         theme(axis.text.x = element_blank(),
# #               axis.text.y = element_blank(),
# #               axis.title.y = element_blank()),
# #       vp = vplayout(3,4))
# # print(Oct_MAR_ILR_uni_loc_4_plot + 
# #         theme(axis.text.x = element_text(size = 20,
# #                                          angle = 45,
# #                                          hjust = 1,
# #                                          vjust = 0.5),
# #               axis.text.y = element_blank(),
# #               axis.title.y = element_blank()),
# #       vp = vplayout(4,4))
# graphics.off()

# 
# # October CovRandPar Plots ------------------------------------------------
# #pdf(paste0('CovRandPar_Oct_plots.pdf'), width = 17, height = 12)
# png(paste0('CovRandPar_Oct_plots.png'), width = 8, height = 6, res = 600, units = fig_units)
# grid.newpage()
# pushViewport(viewport(layout = grid.layout(4,3)))
# vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
# print(Oct_cov_Streamflow_loc_1_plot + 
#         theme(axis.text.x = element_blank(),
#               axis.title.y = element_text(size = 18),
#               plot.margin = margin(t =10, b= 10, l=15, r = 10),
#               legend.position = 'none'),
#       vp = vplayout(1,1))
# print(Oct_cov_MAR_diff_loc_1_plot +
#         theme(axis.text.x = element_blank(),
#               axis.title.y = element_text(size = 18),
#               plot.margin = margin(t =10, b= 10, l=15, r = 10),
#               legend.position = 'none'),
#       vp = vplayout(2,1))
# print(Oct_cov_ILR_diff_loc_1_plot +
#         theme(axis.text.x = element_blank(),
#               axis.title.y = element_text(size = 18),
#               plot.margin = margin(t =10, b= 10, l=15, r = 10),
#               legend.position = 'none'),
#       vp = vplayout(3,1))
# print(Oct_cov_MAR_ILR_diff_loc_1_plot +
#         theme(axis.text.x = element_text(size = 18,
#                                          angle = 45,
#                                          hjust = 1,
#                                          vjust = 0.5),
#               axis.title.y = element_text(size = 18),
#               plot.margin = margin(t =10, b= 10, l=15, r = 10),
#               legend.position = 'none'),
#       vp = vplayout(4,1))
# print(Oct_cov_Streamflow_loc_2_plot +
#         theme(axis.text.x = element_blank(),
#               axis.text.y = element_blank(),
#               axis.title.y = element_blank(),
#               plot.margin = margin(t =10, b= 10, l=15, r = 10),
#               legend.position = c(0.5,0.9),
#               legend.direction = 'horizontal',
#               legend.text = element_text(size = 20),
#               legend.background = element_rect(fill=NA,color = NA),
#               legend.title = element_blank(),
#               legend.spacing.x = unit(0.2, 'cm'),
#               legend.key.height = unit(0.4, 'cm'),
#               legend.key.width = unit(0.4, 'cm')),
#       vp = vplayout(1,2))
# print(Oct_cov_MAR_diff_loc_2_plot +
#         theme(axis.text.x = element_blank(),
#               axis.text.y = element_blank(),
#               axis.title.y = element_blank(),
#               plot.margin = margin(t =10, b= 10, l=15, r = 10),
#               legend.position = 'none'),
#       vp = vplayout(2,2))
# print(Oct_cov_ILR_diff_loc_2_plot +
#         theme(axis.title.y = element_blank(),
#               axis.text.y = element_blank(),
#               axis.text.x = element_blank(),
#               plot.margin = margin(t =10, b= 10, l=15, r = 10),
#               legend.position = 'none'),
#       vp = vplayout(3,2))
# print(Oct_cov_MAR_ILR_diff_loc_2_plot +
#         theme(axis.text.x = element_text(size = 18,
#                                          angle = 45,
#                                          hjust = 1,
#                                          vjust = 0.5),
#               axis.text.y = element_blank(),
#               axis.title.y = element_blank(),
#               plot.margin = margin(t =10, b= 10, l=15, r = 10),
#               legend.position = 'none'),
#       vp = vplayout(4,2))
# print(Oct_cov_Streamflow_loc_3_plot +
#         theme(axis.title.y = element_blank(),
#               axis.text.y = element_blank(),
#               axis.text.x = element_blank(),
#               plot.margin = margin(t =10, b= 10, l=15, r = 10),
#               legend.position = 'none'),
#       vp = vplayout(1,3))
# print(Oct_cov_MAR_diff_loc_3_plot +
#         theme(axis.text.x = element_blank(),
#               axis.text.y = element_blank(),
#               axis.title.y = element_blank(),
#               plot.margin = margin(t =10, b= 10, l=15, r = 10),
#               legend.position = 'none'),
#       vp = vplayout(2,3))
# print(Oct_cov_ILR_diff_loc_3_plot +
#         theme(axis.title.y = element_blank(),
#               axis.text.y = element_blank(),
#               axis.text.x = element_blank(),
#               plot.margin = margin(t =10, b= 10, l=15, r = 10),
#               legend.position = 'none'),
#       vp = vplayout(3,3))
# print(Oct_cov_MAR_ILR_diff_loc_3_plot +
#         theme(axis.text.x = element_text(size = 18,
#                                          angle = 45,
#                                          hjust = 1,
#                                          vjust = 0.5),
#               axis.text.y = element_blank(),
#               axis.title.y = element_blank(),
#               plot.margin = margin(t =10, b= 10, l=15, r = 10),
#               legend.position = 'none'),
#       vp = vplayout(4,3))
# graphics.off()


# # Monthly UniRandPar Violin Plots -----------------------------------------
# #pdf(paste0('UniRandPar_Monthly_Violin_Plots.pdf'), width = 13, height = 13)
# png(paste0('UniRandPar_Monthly_Violin_Plots.png'), width = 8, height = 6, res = 600, units = fig_units)
# grid.newpage()
# pushViewport(viewport(layout = grid.layout(4,3)))
# vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
# print(Monthly_uni_Streamflow_violin_loc_1_plot + 
#         theme(axis.text.x = element_blank(),
#               plot.margin = margin(t =10, b= 10, l=15, r = 10),
#               legend.position = 'none'),
#       vp = vplayout(1,1))
# print(Monthly_MAR_uni_loc_1_plot + 
#         theme(axis.text.x = element_blank(),
#               plot.margin = margin(t =10, b= 10, l=15, r = 10)),
#       vp = vplayout(2,1))
# print(Monthly_ILR_uni_loc_1_plot +
#         theme(axis.text.x = element_blank(),
#               plot.margin = margin(t =10, b= 10, l=15, r = 10)),
#       vp = vplayout(3,1))
# print(Aug_Sep_Oct_MAR_ILR_uni_loc_1_plot +
#         theme(axis.text.x = element_text(size = 20,
#                                          angle = 45,
#                                          hjust = 1,
#                                          vjust = 0.5),
#               plot.margin = margin(t =10, b= 10, l=15, r = 10)),
#       vp = vplayout(4,1))
# print(Monthly_uni_Streamflow_violin_loc_2_plot +
#         theme(axis.text.x = element_blank(),
#               axis.text.y = element_blank(),
#               axis.title.y = element_blank(),
#               plot.margin = margin(t =10, b= 10, l=15, r = 10),
#               legend.position = c(0.5,0.93),
#               legend.direction = 'horizontal',
#               legend.text = element_text(size = 20),
#               legend.background = element_rect(fill=NA,color = NA),
#               legend.title = element_blank(),
#               legend.spacing.x = unit(0.2, 'cm')),
#       vp = vplayout(1,2))
# print(Monthly_MAR_uni_loc_2_plot +
#         theme(axis.text.x = element_blank(),
#               axis.text.y = element_blank(),
#               axis.title.y = element_blank(),
#               plot.margin = margin(t =10, b= 10, l=15, r = 10)),
#       vp = vplayout(2,2))
# print(Monthly_ILR_uni_loc_2_plot +
#         theme(axis.text.x = element_blank(),
#               axis.text.y = element_blank(),
#               axis.title.y = element_blank(),
#               plot.margin = margin(t =10, b= 10, l=15, r = 10)),
#       vp = vplayout(3,2))
# print(Aug_Sep_Oct_MAR_ILR_uni_loc_2_plot +
#         theme(axis.text.x = element_text(size = 20,
#                                          angle = 45,
#                                          hjust = 1,
#                                          vjust = 0.5),
#               axis.text.y = element_blank(),
#               axis.title.y = element_blank(),
#               plot.margin = margin(t =10, b= 10, l=15, r = 10)),
#       vp = vplayout(4,2))
# print(Monthly_uni_Streamflow_violin_loc_3_plot +
#         theme(axis.title.y = element_blank(),
#               axis.text.y = element_blank(),
#               axis.text.x = element_blank(),
#               plot.margin = margin(t =10, b= 10, l=15, r = 10),
#               legend.position = 'none'),
#       vp = vplayout(1,3))
# print(Monthly_MAR_uni_loc_3_plot +
#         theme(axis.title.y = element_blank(),
#               axis.text.y = element_blank(),
#               axis.text.x = element_blank(),
#               plot.margin = margin(t =10, b= 10, l=15, r = 10)),
#       vp = vplayout(2,3))
# print(Monthly_ILR_uni_loc_3_plot +
#         theme(axis.text.x = element_blank(),
#               axis.text.y = element_blank(),
#               axis.title.y = element_blank(),
#               plot.margin = margin(t =10, b= 10, l=15, r = 10)),
#       vp = vplayout(3,3))
# print(Aug_Sep_Oct_MAR_ILR_uni_loc_3_plot +
#         theme(axis.text.x = element_text(size = 20,
#                                          angle = 45,
#                                          hjust = 1,
#                                          vjust = 0.5),
#               axis.text.y = element_blank(),
#               axis.title.y = element_blank(),
#               plot.margin = margin(t =10, b= 10, l=15, r = 10)),
#       vp = vplayout(4,3))
# graphics.off()
# 
# 
# # Monthly CovRandPar Violin Plots -----------------------------------------
# #pdf(paste0('CovRandPar_Monthly_Violin_Plots.pdf'), width = 22, height = 13)
# png(paste0('CovRandPar_Monthly_Violin_Plots.png'), width = 8, height = 6, res = 600, units = fig_units)
# grid.newpage()
# pushViewport(viewport(layout = grid.layout(4,3)))
# vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
# print( Monthly_Streamflow_cov_loc_1_plot + 
#         theme(axis.text.x = element_blank(),
#               plot.margin = margin(t =10, b= 10, l=15, r = 10),
#               legend.position = 'none'),
#       vp = vplayout(1,1))
# print(Monthly_MAR_cov_loc_1_plot + 
#         theme(axis.text.x = element_blank(),
#               plot.margin = margin(t =10, b= 10, l=15, r = 10),
#               legend.position = 'none'),
#       vp = vplayout(2,1))
# print(Monthly_ILR_cov_loc_1_plot +
#         theme(axis.text.x = element_blank(),
#               plot.margin = margin(t =10, b= 10, l=15, r = 10),
#               legend.position = 'none'),
#       vp = vplayout(3,1))
# print(Monthly_MAR_ILR_cov_loc_1_plot +
#         theme(axis.text.x = element_text(size = 20,
#                                          angle = 45,
#                                          hjust = 1,
#                                          vjust = 0.5),
#               plot.margin = margin(t =10, b= 10, l=15, r = 10),
#               legend.position = 'none'),
#       vp = vplayout(4,1))
# print(Monthly_Streamflow_cov_loc_2_plot +
#         theme(axis.text.x = element_blank(),
#               axis.text.y = element_blank(),
#               axis.title.y = element_blank(),
#               plot.margin = margin(t =10, b= 10, l=15, r = 10),
#               legend.position = c(0.5,0.93),
#               legend.direction = 'horizontal',
#               legend.text = element_text(size = 20),
#               legend.background = element_rect(fill=NA,color = NA),
#               legend.title = element_blank(),
#               legend.spacing.x = unit(0.2, 'cm')),
#       vp = vplayout(1,2))
# print(Monthly_MAR_cov_loc_2_plot +
#         theme(axis.text.x = element_blank(),
#               axis.text.y = element_blank(),
#               axis.title.y = element_blank(),
#               plot.margin = margin(t =10, b= 10, l=15, r = 10),
#               legend.position = 'none'),
#       vp = vplayout(2,2))
# print(Monthly_ILR_cov_loc_2_plot +
#         theme(axis.text.x = element_blank(),
#               axis.text.y = element_blank(),
#               axis.title.y = element_blank(),
#               plot.margin = margin(t =10, b= 10, l=15, r = 10),
#               legend.position = 'none'),
#       vp = vplayout(3,2))
# print(Monthly_MAR_ILR_cov_loc_2_plot +
#         theme(axis.text.x = element_text(size = 20,
#                                          angle = 45,
#                                          hjust = 1,
#                                          vjust = 0.5),
#               axis.text.y = element_blank(),
#               axis.title.y = element_blank(),
#               plot.margin = margin(t =10, b= 10, l=15, r = 10),
#               legend.position = 'none'),
#       vp = vplayout(4,2))
# print(Monthly_Streamflow_cov_loc_3_plot +
#         theme(axis.title.y = element_blank(),
#               axis.text.y = element_blank(),
#               axis.text.x = element_blank(),
#               plot.margin = margin(t =10, b= 10, l=15, r = 10),
#               legend.position = 'none'),
#       vp = vplayout(1,3))
# print(Monthly_MAR_cov_loc_3_plot +
#         theme(axis.title.y = element_blank(),
#               axis.text.y = element_blank(),
#               axis.text.x = element_blank(),
#               plot.margin = margin(t =10, b= 10, l=15, r = 10),
#               legend.position = 'none'),
#       vp = vplayout(2,3))
# print(Monthly_ILR_cov_loc_3_plot +
#         theme(axis.text.x = element_blank(),
#               axis.text.y = element_blank(),
#               axis.title.y = element_blank(),
#               plot.margin = margin(t =10, b= 10, l=15, r = 10),
#               legend.position = 'none'),
#       vp = vplayout(3,3))
# print(Monthly_MAR_ILR_cov_loc_3_plot +
#         theme(axis.text.x = element_text(size = 20,
#                                          angle = 45,
#                                          hjust = 1,
#                                          vjust = 0.5),
#               axis.text.y = element_blank(),
#               axis.title.y = element_blank(),
#               plot.margin = margin(t =10, b= 10, l=15, r = 10),
#               legend.position = 'none'),
#       vp = vplayout(4,3))
# graphics.off()

#Plot October Dry (2001) CDFs for CovRandPar MAR

