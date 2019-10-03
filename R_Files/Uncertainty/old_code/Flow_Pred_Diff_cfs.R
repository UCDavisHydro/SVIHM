#Script for plotting flow differences using random parameter sets

rm(list=ls())
library(ggplot2)
library(ggthemes)
library(reshape2)
library(grid)
library(magrittr)
library(ssh)

# User Input --------------------------------------------------------------
UniRandPar = TRUE
CovRandPar = TRUE
UCODE_Uncert = TRUE
MAR_Comp = TRUE
ILR_Comp = TRUE
MAR_ILR_Comp = TRUE
out_dir = paste0(getwd(),'/Results/')
fig_format = 'png'   # figure format (png, pdf, jpg)
fig_width =  190    # figure width in fig_units
fig_height = 110    # figure height in fig_units
fig_units = 'mm'     # units for figure dimensions 
axis_title_size = 7
axis_text_size = 6
line_size = 0.5
point_size = 1
wy_types = data.frame(WY_Type = c('Dry', 'Dry', 'Wet', 'Dry', 'Wet', 'Dry', 'Wet'),
                     Rect_Date_Start = as.Date(c('1990-10-01','1993-10-01','1994-10-01','2000-10-01','2005-10-01','2006-10-01','2010-10-01')),
                     Rect_Date_End = as.Date(c('1992-10-01','1994-10-01','1999-10-01','2002-10-01','2006-10-01','2009-10-01','2011-10-01')),
                     Fill = c('#ff8282', '#ff8282', 'skyblue', '#ff8282','skyblue', '#ff8282','skyblue'),
                     Label_Date = c('1991-10-01','1994-04-01','1997-10-01','2001-10-01','2006-04-01','2008-10-01','2011-04-01'))

Transfer_Files_From_Cluster = FALSE
Check_SSWRs = TRUE
n_preds = 1260 # 5 Predictions for n_stress Stress Periods
n_stress = 252 # 252 stress periods

crit_months = c('Aug','Sep','Oct')   #Critically dry months, used for generating plots
StartingDates = seq(as.Date('1990-10-01'), by = "month", length.out = 252)

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
      ggtitle('Uniform Distribution (Method 1)') +
      scale_y_continuous(limits = c(8E4,1.8E5), breaks = seq(8E4,1.8E5,by = 2E4), expand = c(0,0)) +
      geom_hline(yintercept = 8.9E4, color = '#984ea3', size = 1) + 
      geom_hline(yintercept = 8.9E4*1.1, color = '#984ea3', linetype="dashed", size = 1) +
      geom_hline(yintercept = 9.66E4, color = '#ff7f00', size = 1) + 
      geom_hline(yintercept = 9.66E4*1.1, color = '#ff7f00', linetype="dashed", size = 1) +
      xlab('Realization') +
      theme_few() + 
      theme(plot.title = element_text(hjust = 0.5))
  )
  
  (CovRandPar_SSWR_plot = ggplot(CovRandPar_SSWRs, aes(x = Param_Set_Num, y = SSWR, fill = Calibration)) + 
      geom_point(size = 2, shape = 21, color = 'black') +
      ggtitle('Normal Distribution (Method 2)') +
      scale_fill_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00')) +
      scale_y_continuous(limits = c(8E4,1.8E5), breaks = seq(8E4,1.8E5,by = 2E4), expand = c(0,0)) +
      geom_hline(yintercept = 8.9E4, color = '#984ea3', size = 1) + 
      geom_hline(yintercept = 8.9E4*1.1, color = '#984ea3', linetype="dashed", size = 1) +
      geom_hline(yintercept = 9.66E4, color = '#ff7f00', size = 1) + 
      geom_hline(yintercept = 9.66E4*1.1, color = '#ff7f00', linetype="dashed", size = 1) +
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
    pdf(paste0(out_dir,fig_name,'.pdf'), width = fig_width, height = fig_height)
  }
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(1,2)))
  vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
  print(UniRandPar_SSWR_plot +
          theme(plot.title = element_text(size = 10),
                panel.background = element_rect(color = 'black', fill = 'gray90'),
                axis.text = element_text(size = 8),
                plot.margin = margin(t =5, b= 5, l=5, r = 5)),
        vp = vplayout(1,1))
  print(CovRandPar_SSWR_plot +
          theme(plot.title = element_text(size = 10),
                panel.background = element_rect(color = 'black', fill = 'gray90'),
                axis.text.y = element_blank(),
                axis.text.x = element_text(size = 8),
                axis.title.y = element_blank(),
                plot.margin = margin(t =5, b= 5, l=5, r = 5)),
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
  }
  if (CovRandPar==TRUE){
    flow_pred_dir = CovRandPar_MAR_Dir
    CovRandPar_Preds_MAR = Flow_Preds(pred_dir = flow_pred_dir, scenario = 'MAR', randpartype = 'cov')
    param_match = intersect(names(CovRandPar_Preds_MAR),names(CovRandPar_Preds_Basecase))                #Exclude any missing parameter sets from either scenario
    CovRandPar_Diff_MAR = CovRandPar_Preds_MAR[,seq(1,6)]
    CovRandPar_Diff_MAR = cbind(CovRandPar_Diff_MAR,CovRandPar_Preds_MAR[,param_match[seq(-1,-6)]] - CovRandPar_Preds_Basecase[,param_match[seq(-1,-6)]])
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
  }
  if (CovRandPar==TRUE){
    flow_pred_dir = CovRandPar_ILR_Dir
    CovRandPar_Preds_ILR = Flow_Preds(pred_dir = flow_pred_dir, scenario = 'ILR', randpartype = 'cov')
    param_match = intersect(names(CovRandPar_Preds_ILR),names(CovRandPar_Preds_Basecase))                #Exclude any missing parameter sets from either scenario
    CovRandPar_Diff_ILR = CovRandPar_Preds_ILR[,seq(1,6)]
    CovRandPar_Diff_ILR = cbind(CovRandPar_Diff_ILR,CovRandPar_Preds_ILR[,param_match[seq(-1,-6)]] - CovRandPar_Preds_Basecase[,param_match[seq(-1,-6)]])
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
  }
  if (CovRandPar==TRUE){
    flow_pred_dir = CovRandPar_MAR_ILR_Dir
    CovRandPar_Preds_MAR_ILR = Flow_Preds(pred_dir = flow_pred_dir, scenario = 'MAR_ILR', randpartype = 'cov')
    param_match = intersect(names(CovRandPar_Preds_MAR_ILR),names(CovRandPar_Preds_Basecase))                #Exclude any missing parameter sets from either scenario
    CovRandPar_Diff_MAR_ILR = CovRandPar_Preds_MAR_ILR[,seq(1,6)]
    CovRandPar_Diff_MAR_ILR = cbind(CovRandPar_Diff_MAR_ILR,CovRandPar_Preds_MAR_ILR[,param_match[seq(-1,-6)]] - CovRandPar_Preds_Basecase[,param_match[seq(-1,-6)]])
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

# Streamflow Plots --------------------------------------------------------
if(UniRandPar==TRUE){
  for (j in 1:5){
    eval(parse(text = paste0('mean_cfs = apply(subset(UniRandPar_Preds_Basecase,Pred_Loc==',j,')[,seq(-1,-5)],1,mean)*0.000408734569')))
    eval(parse(text = paste0('sd_cfs = apply(subset(UniRandPar_Preds_Basecase,Pred_Loc==',j,')[,seq(-1,-5)],1,sd)*0.000408734569')))
    eval(parse(text = paste0('max_cfs = apply(subset(UniRandPar_Preds_Basecase,Pred_Loc==',j,')[,seq(-1,-5)],1,max)*0.000408734569')))
    eval(parse(text = paste0('min_cfs = apply(subset(UniRandPar_Preds_Basecase,Pred_Loc==',j,')[,seq(-1,-5)],1,min)*0.000408734569')))
    eval(parse(text = paste0('summary = cbind(subset(UniRandPar_Preds_Basecase,Pred_Loc==',j,')[,seq(1,5)], mean_cfs, sd_cfs, max_cfs, min_cfs)')))
    summary$lower95 = summary$mean_cfs-2*summary$sd_cfs
    summary$upper95 = summary$mean_cfs+2*summary$sd_cfs
    #summary$lower95[which(summary$lower95<0)] = 0
    for(m in 1:length(crit_months)){
      temp_data = subset(summary,Month==crit_months[m])
      Plot = ggplot(data = temp_data, aes(x = Date)) + 
        geom_ribbon(aes(ymin = lower95, ymax = upper95), alpha = 0.5, fill = 'orange') + 
        geom_line(aes(y = mean_cfs),size = line_size) +
        geom_point(aes(y = mean_cfs),size = point_size) +
        ylab('Streamflow (cfs)') +
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
      eval(parse(text = paste0('data_melt = melt(subset(UniRandPar_Preds_Basecase,Pred_Loc==',j,'),id.vars = names(UniRandPar_Diff_MAR[seq(1,5)]))')))
      data_melt$Month = factor(data_melt$Month, levels = format(seq(as.Date('1990-01-01'), as.Date('1990-12-01'), by = 'month'),'%b'))
      data_melt = data_melt[order(data_melt$Month),]
      ViolinPlot = ggplot(data = subset(data_melt,Month==crit_months), aes(x = factor(Month), y = value*0.000408734569, group = Month, fill = Month)) + 
        geom_hline(yintercept = 0) +
        geom_violin(trim = TRUE,draw_quantiles = c(0.5)) +
        ylab('Streamflow (cfs)') +
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
      eval(parse(text = paste0("mean_cfs = apply(subset(CovRandPar_Preds_Basecase,Pred_Loc== ",j,")[,seq(-1,-6)],1,mean)*0.000408734569")))
      eval(parse(text = paste0("sd_cfs = apply(subset(CovRandPar_Preds_Basecase,Pred_Loc== ",j,")[,seq(-1,-6)],1,sd)*0.000408734569")))
      eval(parse(text = paste0("max_cfs = apply(subset(CovRandPar_Preds_Basecase,Pred_Loc== ",j,")[,seq(-1,-6)],1,max)*0.000408734569")))
      eval(parse(text = paste0("min_cfs = apply(subset(CovRandPar_Preds_Basecase,Pred_Loc== ",j,")[,seq(-1,-6)],1,min)*0.000408734569")))
      eval(parse(text = paste0("summary = cbind(subset(CovRandPar_Preds_Basecase,Pred_Loc== ",j,")[,seq(1,6)], mean_cfs, sd_cfs, max_cfs, min_cfs)")))
      summary$lower95 = summary$mean_cfs-2*summary$sd_cfs
      summary$upper95 = summary$mean_cfs+2*summary$sd_cfs  
      #summary$lower95[which(summary$lower95<0)] = 0
      for(m in 1:length(crit_months)){
        temp_data = subset(summary,Month==crit_months[m])
        Plot = ggplot(data = temp_data, aes(x = Date)) + 
          geom_ribbon(aes(ymin = lower95, ymax = upper95, fill = Calibration), alpha = 0.25) + 
          geom_line(aes(y = mean_cfs, group = Calibration, color = Calibration),size = line_size) +
          geom_point(aes(y = mean_cfs, shape = Calibration),size = point_size) +
          ylab('Streamflow (cfs)') +
          scale_color_brewer(palette = 'Set1', type = 'div') +
          scale_fill_brewer(palette = 'Set1', type = 'div') +
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
      eval(parse(text = paste0('data_melt = melt(subset(CovRandPar_Preds_Basecase,Pred_Loc==',j,'),id.vars = names(CovRandPar_Diff_MAR[seq(1,6)]))')))
      data_melt$Month = factor(data_melt$Month, levels = format(seq(as.Date('1990-01-01'), as.Date('1990-12-01'), by = 'month'),'%b'))
      data_melt = data_melt[order(data_melt$Month),]
      ViolinPlot = ggplot(data = subset(data_melt,Month==crit_months), aes(x = factor(Month), y = value*0.000408734569, fill = Calibration)) + 
        geom_hline(yintercept = 0) +
        geom_violin(trim = TRUE,draw_quantiles = c(0.5), position = position_dodge(width = 0.8)) +
        ylab('Streamflow (cfs)') +
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
    #Monthly Series
    for (j in 1:5){
      eval(parse(text = paste0('mean_cfs = apply(subset(UniRandPar_Diff_MAR,Pred_Loc==',j,')[,seq(-1,-5)],1,mean)*0.000408734569')))
      eval(parse(text = paste0('sd_cfs = apply(subset(UniRandPar_Diff_MAR,Pred_Loc==',j,')[,seq(-1,-5)],1,sd)*0.000408734569')))
      eval(parse(text = paste0('min_cfs = apply(subset(UniRandPar_Diff_MAR,Pred_Loc==',j,')[,seq(-1,-5)],1,min)*0.000408734569')))
      eval(parse(text = paste0('max_cfs = apply(subset(UniRandPar_Diff_MAR,Pred_Loc==',j,')[,seq(-1,-5)],1,max)*0.000408734569')))
      eval(parse(text = paste0('summary = cbind(subset(UniRandPar_Diff_MAR,Pred_Loc==',j,')[,seq(1,5)], mean_cfs, sd_cfs, min_cfs, max_cfs)')))
      summary$lower95 = summary$mean_cfs-2*summary$sd_cfs
      summary$upper95 = summary$mean_cfs+2*summary$sd_cfs  
      for(m in 1:length(crit_months)){
        temp_data = subset(summary,Month==crit_months[m])
        Plot = ggplot(data = temp_data, aes(x = Date)) + 
          geom_hline(yintercept = 0) +
          geom_ribbon(aes(ymin = min_cfs, ymax = max_cfs), alpha = 0.5, fill = 'darkgray') + 
          geom_ribbon(aes(ymin = lower95, ymax = upper95), alpha = 0.75, fill = 'dodgerblue') + 
          geom_line(aes(y = mean_cfs),size = line_size) +
          geom_point(aes(y = mean_cfs),size = point_size) +
          geom_rect(aes(xmin = as.Date('1990-10-01'), xmax = as.Date('1992-10-01'), ymin = -10 , ymax = -8), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('1993-10-01'), xmax = as.Date('1994-10-01'), ymin = -10 , ymax = -8), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('1994-10-01'), xmax = as.Date('1999-10-01'), ymin = -10 , ymax = -8), fill = 'skyblue', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2000-10-01'), xmax = as.Date('2002-10-01'), ymin = -10 , ymax = -8), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2005-10-01'), xmax = as.Date('2006-10-01'), ymin = -10 , ymax = -8), fill = 'skyblue', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2006-10-01'), xmax = as.Date('2009-10-01'), ymin = -10 , ymax = -8), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2010-10-01'), xmax = as.Date('2011-10-01'), ymin = -10 , ymax = -8), fill = 'skyblue', color = 'black', size = 0.2) +
          ylab('Streamflow Difference (cfs)') +
          scale_color_manual(values = c('black')) +
          #scale_x_date(limits = c(1991,2011), breaks = seq(1991,2011,by=2), expand = c(0,0)) +
          scale_y_continuous(limits = c(-200,200), breaks = seq(-200,200,by=10),expand = c(0,0)) +
          coord_cartesian(ylim = c(-10,30)) +
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
    #Monthly Series
    for (j in 1:5){
      eval(parse(text = paste0('mean_cfs = apply(subset(CovRandPar_Diff_MAR,Pred_Loc==',j,')[,seq(-1,-6)],1,mean)*0.000408734569')))
      eval(parse(text = paste0('sd_cfs = apply(subset(CovRandPar_Diff_MAR,Pred_Loc==',j,')[,seq(-1,-6)],1,sd)*0.000408734569')))
      eval(parse(text = paste0('min_cfs = apply(subset(CovRandPar_Diff_MAR,Pred_Loc==',j,')[,seq(-1,-6)],1,min)*0.000408734569')))
      eval(parse(text = paste0('max_cfs = apply(subset(CovRandPar_Diff_MAR,Pred_Loc==',j,')[,seq(-1,-6)],1,max)*0.000408734569')))
      eval(parse(text = paste0('summary = cbind(subset(CovRandPar_Diff_MAR,Pred_Loc==',j,')[,seq(1,6)], mean_cfs, sd_cfs, min_cfs, max_cfs)')))
      summary$lower95 = summary$mean_cfs-2*summary$sd_cfs
      summary$upper95 = summary$mean_cfs+2*summary$sd_cfs  
      for (i in 1:n_stress){
        if (i==1){
          max_cfs_all = max(subset(summary, Date == StartingDates[i], select = 'max_cfs'))
          min_cfs_all = min(subset(summary, Date == StartingDates[i], select = 'min_cfs'))
        } else {
          max_cfs_all[i] = max(subset(summary, Date == StartingDates[i], select = 'max_cfs'))
          min_cfs_all[i] = min(subset(summary, Date == StartingDates[i], select = 'min_cfs'))
        }
      }
      min_max_all = data.frame(Date = StartingDates, Max = max_cfs_all, Min = min_cfs_all)
      for(m in 1:length(crit_months)){
        temp_data = subset(summary,Month==crit_months[m])
        Plot = ggplot(data = temp_data, aes(x = Date)) + 
          geom_hline(yintercept = 0) +
          geom_ribbon(data = subset(min_max_all, format(Date, '%b')==crit_months[m]), aes(x = Date, ymin = Min, ymax = Max),fill = 'darkgray' , alpha = 0.5) +
          geom_ribbon(aes(ymin = lower95, ymax = upper95, fill = Calibration), alpha = 0.25) +
          geom_line(aes(y = mean_cfs, group = Calibration, color = Calibration),size = line_size) +
          geom_point(aes(y = mean_cfs, shape = Calibration),size = point_size) +
          geom_rect(aes(xmin = as.Date('1990-10-01'), xmax = as.Date('1992-10-01'), ymin = -10 , ymax = -8), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('1993-10-01'), xmax = as.Date('1994-10-01'), ymin = -10 , ymax = -8), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('1994-10-01'), xmax = as.Date('1999-10-01'), ymin = -10 , ymax = -8), fill = 'skyblue', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2000-10-01'), xmax = as.Date('2002-10-01'), ymin = -10 , ymax = -8), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2005-10-01'), xmax = as.Date('2006-10-01'), ymin = -10 , ymax = -8), fill = 'skyblue', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2006-10-01'), xmax = as.Date('2009-10-01'), ymin = -10 , ymax = -8), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2010-10-01'), xmax = as.Date('2011-10-01'), ymin = -10 , ymax = -8), fill = 'skyblue', color = 'black', size = 0.2) +
          ylab('Streamflow Difference (cfs)') +
          scale_color_brewer(palette = 'Set1', type = 'div') +
          scale_fill_brewer(palette = 'Set1', type = 'div') +
          ##scale_x_continuous(limits = c(1991,2011), breaks = seq(1991,2011,by=2), expand = c(0,0)) +
          scale_y_continuous(limits = c(-200,200), breaks = seq(-200,200,by=10),expand = c(0,0)) +
          coord_cartesian(ylim = c(-10,30)) +
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
      eval(parse(text = paste0('temp1 = melt(subset(UniRandPar_Diff_MAR,Pred_Loc==',j,'),id.vars = names(UniRandPar_Diff_MAR[seq(1,5)]))')))
      temp1$Month = factor(temp1$Month, levels = format(seq(as.Date('1990-01-01'), as.Date('1990-12-01'), by = 'month'),'%b'))
      temp1 = temp1[order(temp1$Month),]
      temp1$Calibration = 'Uniform'
      
      eval(parse(text = paste0('temp2 = melt(subset(CovRandPar_Diff_MAR,Pred_Loc==',j,'),id.vars = names(CovRandPar_Diff_MAR[seq(1,6)]))')))
      temp2$Month = factor(temp2$Month, levels = format(seq(as.Date('1990-01-01'), as.Date('1990-12-01'), by = 'month'),'%b'))
      temp2 = temp2[order(temp2$Month),]
      
      Uni_Cov_Rand_Diff_MAR = rbind(temp1, temp2)
      Uni_Cov_Rand_Diff_MAR$Calibration = factor(Uni_Cov_Rand_Diff_MAR$Calibration, levels= c('Uniform',paste0('Cal_',seq(1,5))))
      Uni_Cov_Rand_Diff_MAR = Uni_Cov_Rand_Diff_MAR[order(Uni_Cov_Rand_Diff_MAR$Calibration),]
      
      ViolinPlot_Uni_Cov = ggplot(data = subset(Uni_Cov_Rand_Diff_MAR,Month==crit_months), aes(x = factor(Month), y = value*0.000408734569, fill = Calibration)) +
        geom_hline(yintercept = 0) +
        geom_violin(position = position_dodge(width = 0.9),draw_quantiles = c(0.5), size = 0.25) +
        ylab('Streamflow Difference (cfs)') +
        scale_fill_brewer(palette = 'Set1', guide = guide_legend(nrow = 1)) +
        scale_y_continuous(limits = c(-120,120), breaks = seq(-120,120,by = 10), expand = c(0,0)) +
        scale_x_discrete(expand = c(0,0)) +
        coord_cartesian(ylim = c(-20,40)) +
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
        temp_data = subset(UCODE_MAR_diff,MONTH==crit_months[m] & INT.TYPE=='Simultaneous_2' & PRED.LOC == j)
        Plot = ggplot(data = temp_data, aes(x = DATE)) + 
          geom_hline(yintercept = 0) +
          geom_ribbon(aes(ymin = (PREDICTED.VALUE-(2*STANDARD.DEVIATION))*0.000408734569, ymax = (PREDICTED.VALUE+(2*STANDARD.DEVIATION))*0.000408734569, fill = CALIBRATION), alpha = 0.25) +
          geom_line(aes(y = PREDICTED.VALUE*0.000408734569, group = CALIBRATION, color = CALIBRATION),size = line_size) +
          geom_point(aes(y = PREDICTED.VALUE*0.000408734569, shape = CALIBRATION),size = point_size) +
          geom_rect(aes(xmin = as.Date('1990-10-01'), xmax = as.Date('1992-10-01'), ymin = -10 , ymax = -8), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('1993-10-01'), xmax = as.Date('1994-10-01'), ymin = -10 , ymax = -8), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('1994-10-01'), xmax = as.Date('1999-10-01'), ymin = -10 , ymax = -8), fill = 'skyblue', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2000-10-01'), xmax = as.Date('2002-10-01'), ymin = -10 , ymax = -8), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2005-10-01'), xmax = as.Date('2006-10-01'), ymin = -10 , ymax = -8), fill = 'skyblue', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2006-10-01'), xmax = as.Date('2009-10-01'), ymin = -10 , ymax = -8), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2010-10-01'), xmax = as.Date('2011-10-01'), ymin = -10 , ymax = -8), fill = 'skyblue', color = 'black', size = 0.2) +
          ylab('Streamflow Difference (cfs)') +
          scale_color_brewer(palette = 'Set1', type = 'div') +
          scale_fill_brewer(palette = 'Set1', type = 'div') +
          ##scale_x_continuous(limits = c(1991,2011), breaks = seq(1991,2011,by=2), expand = c(0,0)) +
          scale_y_continuous(limits = c(-200,200), breaks = seq(-200,200,by=10),expand = c(0,0)) +
          coord_cartesian(ylim = c(-10,30)) +
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
    #Monthly Series
    for (j in 1:5){
      eval(parse(text = paste0('mean_cfs = apply(subset(UniRandPar_Diff_ILR,Pred_Loc==',j,')[,seq(-1,-5)],1,mean)*0.000408734569')))
      eval(parse(text = paste0('sd_cfs = apply(subset(UniRandPar_Diff_ILR,Pred_Loc==',j,')[,seq(-1,-5)],1,sd)*0.000408734569')))
      eval(parse(text = paste0('min_cfs = apply(subset(UniRandPar_Diff_ILR,Pred_Loc==',j,')[,seq(-1,-5)],1,min)*0.000408734569')))
      eval(parse(text = paste0('max_cfs = apply(subset(UniRandPar_Diff_ILR,Pred_Loc==',j,')[,seq(-1,-5)],1,max)*0.000408734569')))
      eval(parse(text = paste0('summary = cbind(subset(UniRandPar_Diff_ILR,Pred_Loc==',j,')[,seq(1,5)], mean_cfs, sd_cfs, min_cfs, max_cfs)')))
      summary$lower95 = summary$mean_cfs-2*summary$sd_cfs
      summary$upper95 = summary$mean_cfs+2*summary$sd_cfs  
      for(m in 1:length(crit_months)){
        temp_data = subset(summary,Month==crit_months[m])
        Plot = ggplot(data = temp_data, aes(x = Date)) + 
          geom_hline(yintercept = 0) +
          geom_ribbon(aes(ymin = min_cfs, ymax = max_cfs), alpha = 0.5, fill = 'darkgray') + 
          geom_ribbon(aes(ymin = lower95, ymax = upper95), alpha = 0.75, fill = 'dodgerblue') + 
          geom_line(aes(y = mean_cfs),size = line_size) +
          geom_point(aes(y = mean_cfs),size = point_size) +
          geom_rect(aes(xmin = as.Date('1990-10-01'), xmax = as.Date('1992-10-01'), ymin = -20 , ymax = -17), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('1993-10-01'), xmax = as.Date('1994-10-01'), ymin = -20 , ymax = -17), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('1994-10-01'), xmax = as.Date('1999-10-01'), ymin = -20 , ymax = -17), fill = 'skyblue', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2000-10-01'), xmax = as.Date('2002-10-01'), ymin = -20 , ymax = -17), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2005-10-01'), xmax = as.Date('2006-10-01'), ymin = -20 , ymax = -17), fill = 'skyblue', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2006-10-01'), xmax = as.Date('2009-10-01'), ymin = -20 , ymax = -17), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2010-10-01'), xmax = as.Date('2011-10-01'), ymin = -20 , ymax = -17), fill = 'skyblue', color = 'black', size = 0.2) +
          ylab('Streamflow Difference (cfs)') +
          scale_color_manual(values = c('black')) +
          #scale_x_continuous(limits = c(1991,2011), breaks = seq(1991,2011,by=2), expand = c(0,0)) +
          scale_y_continuous(limits = c(-200,200), breaks = seq(-200,200,by=10),expand = c(0,0)) +
          coord_cartesian(ylim = c(-20,40)) +
          theme_few() +
          theme(axis.title.x = element_blank(),
                axis.text = element_text(size = axis_text_size),
                axis.text.x = element_text(angle = 45, hjust = 1),
                axis.title.y = element_text(size = axis_title_size))
        eval(parse(text = paste0(crit_months[m],'_ILR_uni_loc_',j,'_plot = Plot')))
      }
    }
  }
  if(CovRandPar==TRUE){
    #Monthly Series
    for (j in 1:5){
      eval(parse(text = paste0('mean_cfs = apply(subset(CovRandPar_Diff_ILR,Pred_Loc==',j,')[,seq(-1,-6)],1,mean)*0.000408734569')))
      eval(parse(text = paste0('sd_cfs = apply(subset(CovRandPar_Diff_ILR,Pred_Loc==',j,')[,seq(-1,-6)],1,sd)*0.000408734569')))
      eval(parse(text = paste0('min_cfs = apply(subset(CovRandPar_Diff_ILR,Pred_Loc==',j,')[,seq(-1,-6)],1,min)*0.000408734569')))
      eval(parse(text = paste0('max_cfs = apply(subset(CovRandPar_Diff_ILR,Pred_Loc==',j,')[,seq(-1,-6)],1,max)*0.000408734569')))
      eval(parse(text = paste0('summary = cbind(subset(CovRandPar_Diff_ILR,Pred_Loc==',j,')[,seq(1,6)], mean_cfs, sd_cfs, min_cfs, max_cfs)')))
      summary$lower95 = summary$mean_cfs-2*summary$sd_cfs
      summary$upper95 = summary$mean_cfs+2*summary$sd_cfs  
      for (i in 1:n_stress){
        if (i==1){
          max_cfs_all = max(subset(summary, Date == StartingDates[i], select = 'max_cfs'))
          min_cfs_all = min(subset(summary, Date == StartingDates[i], select = 'min_cfs'))
        } else {
          max_cfs_all[i] = max(subset(summary, Date == StartingDates[i], select = 'max_cfs'))
          min_cfs_all[i] = min(subset(summary, Date == StartingDates[i], select = 'min_cfs'))
        }
      }
      min_max_all = data.frame(Date = StartingDates, Max = max_cfs_all, Min = min_cfs_all)
      for(m in 1:length(crit_months)){
        temp_data = subset(summary,Month==crit_months[m])
        Plot = ggplot(data = temp_data, aes(x = Date)) + 
          geom_hline(yintercept = 0) +
          geom_ribbon(data = subset(min_max_all, format(Date, '%b')==crit_months[m]), aes(x = Date, ymin = Min, ymax = Max),fill = 'darkgray' , alpha = 0.5) +
          geom_ribbon(aes(ymin = lower95, ymax = upper95, fill = Calibration), alpha = 0.25) + 
          geom_line(aes(y = mean_cfs, group = Calibration, color = Calibration),size = line_size) +
          geom_point(aes(y = mean_cfs, shape = Calibration),size = point_size) +
          geom_rect(aes(xmin = as.Date('1990-10-01'), xmax = as.Date('1992-10-01'), ymin = -20 , ymax = -17), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('1993-10-01'), xmax = as.Date('1994-10-01'), ymin = -20 , ymax = -17), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('1994-10-01'), xmax = as.Date('1999-10-01'), ymin = -20 , ymax = -17), fill = 'skyblue', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2000-10-01'), xmax = as.Date('2002-10-01'), ymin = -20 , ymax = -17), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2005-10-01'), xmax = as.Date('2006-10-01'), ymin = -20 , ymax = -17), fill = 'skyblue', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2006-10-01'), xmax = as.Date('2009-10-01'), ymin = -20 , ymax = -17), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2010-10-01'), xmax = as.Date('2011-10-01'), ymin = -20 , ymax = -17), fill = 'skyblue', color = 'black', size = 0.2) +
          ylab('Streamflow Difference (cfs)') +
          scale_color_brewer(palette = 'Set1', type = 'div') +
          scale_fill_brewer(palette = 'Set1', type = 'div') +
          #scale_x_continuous(limits = c(1991,2011), breaks = seq(1991,2011,by=2), expand = c(0,0)) +
          scale_y_continuous(limits = c(-200,200), breaks = seq(-200,200,by=10),expand = c(0,0)) +
          coord_cartesian(ylim = c(-20,40)) +
          theme_few() +
          theme(axis.title.x = element_blank(),
                axis.title.y = element_text(size = axis_title_size),
                axis.text = element_text(size = axis_text_size),
                axis.text.x = element_text(angle = 45, hjust = 1),
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
      eval(parse(text = paste0('temp1 = melt(subset(UniRandPar_Diff_ILR,Pred_Loc==',j,'),id.vars = names(UniRandPar_Diff_ILR[seq(1,5)]))')))
      temp1$Month = factor(temp1$Month, levels = format(seq(as.Date('1990-01-01'), as.Date('1990-12-01'), by = 'month'),'%b'))
      temp1 = temp1[order(temp1$Month),]
      temp1$Calibration = 'Uniform'
      
      eval(parse(text = paste0('temp2 = melt(subset(CovRandPar_Diff_ILR,Pred_Loc==',j,'),id.vars = names(CovRandPar_Diff_ILR[seq(1,6)]))')))
      temp2$Month = factor(temp2$Month, levels = format(seq(as.Date('1990-01-01'), as.Date('1990-12-01'), by = 'month'),'%b'))
      temp2 = temp2[order(temp2$Month),]
      
      Uni_Cov_Rand_Diff_ILR = rbind(temp1, temp2)
      Uni_Cov_Rand_Diff_ILR$Calibration = factor(Uni_Cov_Rand_Diff_ILR$Calibration, levels= c('Uniform',paste0('Cal_',seq(1,5))))
      Uni_Cov_Rand_Diff_ILR = Uni_Cov_Rand_Diff_ILR[order(Uni_Cov_Rand_Diff_ILR$Calibration),]
      
      ViolinPlot_Uni_Cov = ggplot(data = subset(Uni_Cov_Rand_Diff_ILR,Month==crit_months), aes(x = factor(Month), y = value*0.000408734569, fill = Calibration)) +
        geom_hline(yintercept = 0) +
        geom_violin(position = position_dodge(width = 0.9),draw_quantiles = c(0.5), size = 0.25) +
        ylab('Streamflow Difference (cfs)') +
        scale_fill_brewer(palette = 'Set1', guide = guide_legend(nrow = 1)) +
        scale_y_continuous(limits = c(-120,120), breaks = seq(-120,120,by = 10), expand = c(0,0)) +
        scale_x_discrete(expand = c(0,0)) +
        coord_cartesian(ylim = c(-20,40)) +
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
        temp_data = subset(UCODE_ILR_diff,MONTH==crit_months[m] & INT.TYPE=='Simultaneous_2' & PRED.LOC == j)
        Plot = ggplot(data = temp_data, aes(x = DATE)) + 
          geom_hline(yintercept = 0) +
          geom_ribbon(aes(ymin = (PREDICTED.VALUE-(2*STANDARD.DEVIATION))*0.000408734569, ymax = (PREDICTED.VALUE+(2*STANDARD.DEVIATION))*0.000408734569, fill = CALIBRATION), alpha = 0.25) +
          geom_line(aes(y = PREDICTED.VALUE*0.000408734569, group = CALIBRATION, color = CALIBRATION),size = line_size) +
          geom_point(aes(y = PREDICTED.VALUE*0.000408734569, shape = CALIBRATION),size = point_size) +
          geom_rect(aes(xmin = as.Date('1990-10-01'), xmax = as.Date('1992-10-01'), ymin = -20 , ymax = -17), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('1993-10-01'), xmax = as.Date('1994-10-01'), ymin = -20 , ymax = -17), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('1994-10-01'), xmax = as.Date('1999-10-01'), ymin = -20 , ymax = -17), fill = 'skyblue', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2000-10-01'), xmax = as.Date('2002-10-01'), ymin = -20 , ymax = -17), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2005-10-01'), xmax = as.Date('2006-10-01'), ymin = -20 , ymax = -17), fill = 'skyblue', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2006-10-01'), xmax = as.Date('2009-10-01'), ymin = -20 , ymax = -17), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2010-10-01'), xmax = as.Date('2011-10-01'), ymin = -20 , ymax = -17), fill = 'skyblue', color = 'black', size = 0.2) +
          ylab('Streamflow Difference (cfs)') +
          scale_color_brewer(palette = 'Set1', type = 'div') +
          scale_fill_brewer(palette = 'Set1', type = 'div') +
          ##scale_x_continuous(limits = c(1991,2011), breaks = seq(1991,2011,by=2), expand = c(0,0)) +
          scale_y_continuous(limits = c(-200,200), breaks = seq(-200,200,by=10),expand = c(0,0)) +
          coord_cartesian(ylim = c(-20,40)) +
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
    #Monthly Series
    for (j in 1:5){
      eval(parse(text = paste0('mean_cfs = apply(subset(UniRandPar_Diff_MAR_ILR,Pred_Loc==',j,')[,seq(-1,-5)],1,mean)*0.000408734569')))
      eval(parse(text = paste0('sd_cfs = apply(subset(UniRandPar_Diff_MAR_ILR,Pred_Loc==',j,')[,seq(-1,-5)],1,sd)*0.000408734569')))
      eval(parse(text = paste0('min_cfs = apply(subset(UniRandPar_Diff_MAR_ILR,Pred_Loc==',j,')[,seq(-1,-5)],1,min)*0.000408734569')))
      eval(parse(text = paste0('max_cfs = apply(subset(UniRandPar_Diff_MAR_ILR,Pred_Loc==',j,')[,seq(-1,-5)],1,max)*0.000408734569')))
      eval(parse(text = paste0('summary = cbind(subset(UniRandPar_Diff_MAR_ILR,Pred_Loc==',j,')[,seq(1,5)], mean_cfs, sd_cfs, min_cfs, max_cfs)')))
      summary$lower95 = summary$mean_cfs-2*summary$sd_cfs
      summary$upper95 = summary$mean_cfs+2*summary$sd_cfs  
      for(m in 1:length(crit_months)){
        temp_data = subset(summary,Month==crit_months[m])
        Plot = ggplot(data = temp_data, aes(x = Date)) + 
          geom_hline(yintercept = 0) +
          geom_ribbon(aes(ymin = min_cfs, ymax = max_cfs), alpha = 0.5, fill = 'darkgray') + 
          geom_ribbon(aes(ymin = lower95, ymax = upper95), alpha = 0.75, fill = 'dodgerblue') + 
          geom_line(aes(y = mean_cfs),size = line_size) +
          geom_point(aes(y = mean_cfs),size = point_size) +
          geom_rect(aes(xmin = as.Date('1990-10-01'), xmax = as.Date('1992-10-01'), ymin = -20 , ymax = -17), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('1993-10-01'), xmax = as.Date('1994-10-01'), ymin = -20 , ymax = -17), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('1994-10-01'), xmax = as.Date('1999-10-01'), ymin = -20 , ymax = -17), fill = 'skyblue', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2000-10-01'), xmax = as.Date('2002-10-01'), ymin = -20 , ymax = -17), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2005-10-01'), xmax = as.Date('2006-10-01'), ymin = -20 , ymax = -17), fill = 'skyblue', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2006-10-01'), xmax = as.Date('2009-10-01'), ymin = -20 , ymax = -17), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2010-10-01'), xmax = as.Date('2011-10-01'), ymin = -20 , ymax = -17), fill = 'skyblue', color = 'black', size = 0.2) +
          ylab('Streamflow Difference (cfs)') +
          scale_color_manual(values = c('black')) +
          #scale_x_continuous(limits = c(1991,2011), breaks = seq(1991,2011,by=2), expand = c(0,0)) +
          scale_y_continuous(limits = c(-200,200), breaks = seq(-200,200,by=10),expand = c(0,0)) +
          coord_cartesian(ylim = c(-20,40)) +
          theme_few() +
          theme(axis.title.x = element_blank(),
                axis.title.y = element_text(size = axis_title_size),
                axis.text = element_text(size = axis_text_size),
                axis.text.x = element_text(angle = 45, hjust = 1))
        eval(parse(text = paste0(crit_months[m],'_MAR_ILR_uni_loc_',j,'_plot = Plot')))
      }
    }
  }
  if(CovRandPar==TRUE){
    #Monthly Series
    for (j in 1:5){
      eval(parse(text = paste0('mean_cfs = apply(subset(CovRandPar_Diff_MAR_ILR,Pred_Loc==',j,')[,seq(-1,-6)],1,mean)*0.000408734569')))
      eval(parse(text = paste0('sd_cfs = apply(subset(CovRandPar_Diff_MAR_ILR,Pred_Loc==',j,')[,seq(-1,-6)],1,sd)*0.000408734569')))
      eval(parse(text = paste0('min_cfs = apply(subset(CovRandPar_Diff_MAR_ILR,Pred_Loc==',j,')[,seq(-1,-6)],1,min)*0.000408734569')))
      eval(parse(text = paste0('max_cfs = apply(subset(CovRandPar_Diff_MAR_ILR,Pred_Loc==',j,')[,seq(-1,-6)],1,max)*0.000408734569')))
      eval(parse(text = paste0('summary = cbind(subset(CovRandPar_Diff_MAR_ILR,Pred_Loc==',j,')[,seq(1,6)], mean_cfs, sd_cfs, min_cfs, max_cfs)')))
      summary$lower95 = summary$mean_cfs-2*summary$sd_cfs
      summary$upper95 = summary$mean_cfs+2*summary$sd_cfs  
      for (i in 1:n_stress){
        if (i==1){
          max_cfs_all = max(subset(summary, Date == StartingDates[i], select = 'max_cfs'))
          min_cfs_all = min(subset(summary, Date == StartingDates[i], select = 'min_cfs'))
        } else {
          max_cfs_all[i] = max(subset(summary, Date == StartingDates[i], select = 'max_cfs'))
          min_cfs_all[i] = min(subset(summary, Date == StartingDates[i], select = 'min_cfs'))
        }
      }
      min_max_all = data.frame(Date = StartingDates, Max = max_cfs_all, Min = min_cfs_all)
      for(m in 1:length(crit_months)){
        temp_data = subset(summary,Month==crit_months[m])
        Plot = ggplot(data = temp_data, aes(x = Date)) + 
          geom_hline(yintercept = 0) +
          geom_ribbon(data = subset(min_max_all, format(Date, '%b')==crit_months[m]), aes(x = Date, ymin = Min, ymax = Max),fill = 'darkgray' , alpha = 0.5) +
          geom_ribbon(aes(ymin = lower95, ymax = upper95, fill = Calibration), alpha = 0.25) + 
          geom_line(aes(y = mean_cfs, group = Calibration, color = Calibration),size = line_size) +
          geom_point(aes(y = mean_cfs, shape = Calibration),size = point_size) +
          geom_rect(aes(xmin = as.Date('1990-10-01'), xmax = as.Date('1992-10-01'), ymin = -20 , ymax = -17), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('1993-10-01'), xmax = as.Date('1994-10-01'), ymin = -20 , ymax = -17), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('1994-10-01'), xmax = as.Date('1999-10-01'), ymin = -20 , ymax = -17), fill = 'skyblue', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2000-10-01'), xmax = as.Date('2002-10-01'), ymin = -20 , ymax = -17), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2005-10-01'), xmax = as.Date('2006-10-01'), ymin = -20 , ymax = -17), fill = 'skyblue', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2006-10-01'), xmax = as.Date('2009-10-01'), ymin = -20 , ymax = -17), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2010-10-01'), xmax = as.Date('2011-10-01'), ymin = -20 , ymax = -17), fill = 'skyblue', color = 'black', size = 0.2) +
          ylab('Streamflow Difference (cfs)') +
          scale_color_brewer(palette = 'Set1', type = 'div') +
          scale_fill_brewer(palette = 'Set1', type = 'div') +
          #scale_x_continuous(limits = c(1991,2011), breaks = seq(1991,2011,by=2), expand = c(0,0)) +
          scale_y_continuous(limits = c(-200,200), breaks = seq(-200,200,by=10),expand = c(0,0)) +
          coord_cartesian(ylim = c(-20,40)) +
          theme_few() +
          theme(axis.title.x = element_blank(),
                axis.title.y = element_text(size = axis_title_size),
                axis.text = element_text(size = axis_text_size),
                axis.text.x = element_text(angle = 45, hjust = 1),
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
      eval(parse(text = paste0('temp1 = melt(subset(UniRandPar_Diff_MAR_ILR,Pred_Loc==',j,'),id.vars = names(UniRandPar_Diff_MAR_ILR[seq(1,5)]))')))
      temp1$Month = factor(temp1$Month, levels = format(seq(as.Date('1990-01-01'), as.Date('1990-12-01'), by = 'month'),'%b'))
      temp1 = temp1[order(temp1$Month),]
      temp1$Calibration = 'Uniform'
      
      eval(parse(text = paste0('temp2 = melt(subset(CovRandPar_Diff_MAR_ILR,Pred_Loc==',j,'),id.vars = names(CovRandPar_Diff_MAR_ILR[seq(1,6)]))')))
      temp2$Month = factor(temp2$Month, levels = format(seq(as.Date('1990-01-01'), as.Date('1990-12-01'), by = 'month'),'%b'))
      temp2 = temp2[order(temp2$Month),]
      
      Uni_Cov_Rand_Diff_MAR_ILR = rbind(temp1, temp2)
      Uni_Cov_Rand_Diff_MAR_ILR$Calibration = factor(Uni_Cov_Rand_Diff_MAR_ILR$Calibration, levels= c('Uniform',paste0('Cal_',seq(1,5))))
      Uni_Cov_Rand_Diff_MAR_ILR = Uni_Cov_Rand_Diff_MAR_ILR[order(Uni_Cov_Rand_Diff_MAR_ILR$Calibration),]
      
      ViolinPlot_Uni_Cov = ggplot(data = subset(Uni_Cov_Rand_Diff_MAR_ILR,Month==crit_months), aes(x = factor(Month), y = value*0.000408734569, fill = Calibration)) +
        geom_hline(yintercept = 0) +
        geom_violin(position = position_dodge(width = 0.9),draw_quantiles = c(0.5), size = 0.25) +
        ylab('Streamflow Difference (cfs)') +
        scale_fill_brewer(palette = 'Set1', guide = guide_legend(nrow = 1)) +
        scale_y_continuous(limits = c(-120,120), breaks = seq(-120,120,by = 10), expand = c(0,0)) +
        scale_x_discrete(expand = c(0,0)) +
        coord_cartesian(ylim = c(-20,40)) +
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
        temp_data = subset(UCODE_MAR_ILR_diff,MONTH==crit_months[m] & INT.TYPE=='Simultaneous_2' & PRED.LOC == j)
        Plot = ggplot(data = temp_data, aes(x = DATE)) + 
          geom_hline(yintercept = 0) +
          geom_ribbon(aes(ymin = (PREDICTED.VALUE-(2*STANDARD.DEVIATION))*0.000408734569, ymax = (PREDICTED.VALUE+(2*STANDARD.DEVIATION))*0.000408734569, fill = CALIBRATION), alpha = 0.25) +
          geom_line(aes(y = PREDICTED.VALUE*0.000408734569, group = CALIBRATION, color = CALIBRATION),size = line_size) +
          geom_point(aes(y = PREDICTED.VALUE*0.000408734569, shape = CALIBRATION),size = point_size) +
          geom_rect(aes(xmin = as.Date('1990-10-01'), xmax = as.Date('1992-10-01'), ymin = -20 , ymax = -17), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('1993-10-01'), xmax = as.Date('1994-10-01'), ymin = -20 , ymax = -17), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('1994-10-01'), xmax = as.Date('1999-10-01'), ymin = -20 , ymax = -17), fill = 'skyblue', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2000-10-01'), xmax = as.Date('2002-10-01'), ymin = -20 , ymax = -17), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2005-10-01'), xmax = as.Date('2006-10-01'), ymin = -20 , ymax = -17), fill = 'skyblue', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2006-10-01'), xmax = as.Date('2009-10-01'), ymin = -20 , ymax = -17), fill = '#ff8282', color = 'black', size = 0.2) +
          geom_rect(aes(xmin = as.Date('2010-10-01'), xmax = as.Date('2011-10-01'), ymin = -20 , ymax = -17), fill = 'skyblue', color = 'black', size = 0.2) +
          ylab('Streamflow Difference (cfs)') +
          scale_color_brewer(palette = 'Set1', type = 'div') +
          scale_fill_brewer(palette = 'Set1', type = 'div') +
          ##scale_x_continuous(limits = c(1991,2011), breaks = seq(1991,2011,by=2), expand = c(0,0)) +
          scale_y_continuous(limits = c(-200,200), breaks = seq(-200,200,by=10),expand = c(0,0)) +
          coord_cartesian(ylim = c(-20,40)) +
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
# October MAR Difference Plots ----------------------------------                                             
fig_name = 'Oct_MAR_Flow_Diffs'                                                                           
if (fig_format == 'jpg'){                                                                                     
  jpeg(paste0(out_dir,fig_name,'.jpg'), width = fig_width, height = fig_height, units = fig_units,  res = 600)
} else if (fig_format == 'png'){                                                                              
  png(paste0(out_dir,fig_name,'.png'), width = fig_width, height = fig_height, units = fig_units,  res = 600) 
} else if (fig_format == 'pdf'){                                                                              
  pdf(paste0(out_dir,fig_name,'.pdf'), width = fig_width, height = fig_height)                                
}                                                                                                             
grid.newpage()                                                                                                
pushViewport(viewport(layout = grid.layout(2,3)))                                                             
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)                                   
print(Oct_MAR_uni_loc_1_plot +                                                                            
        scale_x_date(limits = c(as.Date('1990-10-01'), as.Date('2011-10-01')),                                
                     breaks = seq(as.Date('1990-10-1'), by = "2 years", length.out = 22),                     
                     expand = c(0.01, 0.01)) +                                                                
        ylab('Prediction Location 1\nStreamflow Difference (cfs)') +                                          
        theme(axis.text.x = element_blank(),                                                                  
              plot.margin = margin(t=5,b=13,l=5,r=-11),                                                        
              plot.background = element_rect(fill = NA, color = NA)),                                         
      vp = vplayout(1,1))                                                                                     
print(Oct_MAR_cov_loc_1_plot +                                                                            
        scale_x_date(limits = c(as.Date('1990-10-01'), as.Date('2011-10-01')),                                
                     breaks = seq(as.Date('1990-10-1'), by = "2 years", length.out = 22),                     
                     expand = c(0.01, 0.01)) +                                                                
        theme(axis.text.x = element_blank(),                                                                  
              axis.text.y = element_blank(),                                                                  
              axis.title.y = element_blank(),                                                                 
              legend.position = 'none',                                                                       
              plot.margin = margin(t=5,b=13,l=21,r=-3),                                                        
              plot.background = element_rect(fill = NA, color = NA)),                                         
      vp = vplayout(1,2))                                                                                     
print(Oct_MAR_UCODE_loc_1_plot +                                                                          
        scale_x_date(limits = c(as.Date('1990-10-01'), as.Date('2011-10-01')),                                
                     breaks = seq(as.Date('1990-10-1'), by = "2 years", length.out = 22),                     
                     expand = c(0.01, 0.01)) +                                                                
        theme(axis.text.x = element_blank(),                                                                  
              axis.text.y = element_blank(),                                                                  
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
        ylab('Prediction Location 2\nStreamflow Difference (cfs)') +                                          
        theme(plot.margin = margin(t=-3,b=5,l=5,r=-11),                                                        
              plot.background = element_rect(fill = NA, color = NA)),                                         
      vp = vplayout(2,1))                                                                                     
print(Oct_MAR_cov_loc_2_plot +                                                                            
        scale_x_date(limits = c(as.Date('1990-10-01'), as.Date('2011-10-01')),                                
                     breaks = seq(as.Date('1990-10-1'), by = "2 years", length.out = 22),  
                     date_labels = '%b-%y',
                     expand = c(0.01, 0.01)) +                                                                
        theme(axis.text.y = element_blank(),                                                                  
              axis.title.y = element_blank(),                                                                 
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
              axis.title.y = element_blank(),                                                                 
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
  pdf(paste0(out_dir,fig_name,'.pdf'), width = fig_width, height = fig_height)                                
}                                                                                                             
grid.newpage()                                                                                                
pushViewport(viewport(layout = grid.layout(2,3)))                                                             
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)                                   
print(Oct_ILR_uni_loc_1_plot +                                                                            
        scale_x_date(limits = c(as.Date('1990-10-01'), as.Date('2011-10-01')),                                
                     breaks = seq(as.Date('1990-10-1'), by = "2 years", length.out = 22),                     
                     expand = c(0.01, 0.01)) +                                                                
        ylab('Prediction Location 1\nStreamflow Difference (cfs)') +                                          
        theme(axis.text.x = element_blank(),                                                                  
              plot.margin = margin(t=5,b=13,l=5,r=-11),                                                        
              plot.background = element_rect(fill = NA, color = NA)),                                         
      vp = vplayout(1,1))                                                                                     
print(Oct_ILR_cov_loc_1_plot +                                                                            
        scale_x_date(limits = c(as.Date('1990-10-01'), as.Date('2011-10-01')),                                
                     breaks = seq(as.Date('1990-10-1'), by = "2 years", length.out = 22),                     
                     expand = c(0.01, 0.01)) +                                                                
        theme(axis.text.x = element_blank(),                                                                  
              axis.text.y = element_blank(),                                                                  
              axis.title.y = element_blank(),                                                                 
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
              axis.title.y = element_blank(),                                                                 
              legend.position = 'none',                                                                       
              plot.margin = margin(t=5,b=13,l=13,r=5),                                                         
              plot.background = element_rect(fill = NA, color = NA)),                                         
      vp = vplayout(1,3))                                                                                     
print(Oct_ILR_uni_loc_2_plot +                                                                            
        scale_x_date(limits = c(as.Date('1990-10-01'), as.Date('2011-10-01')),                                
                     breaks = seq(as.Date('1990-10-1'), by = "2 years", length.out = 22), 
                     date_labels = '%b-%y',
                     expand = c(0.01, 0.01)) +                                                                
        ylab('Prediction Location 2\nStreamflow Difference (cfs)') +                                          
        theme(plot.margin = margin(t=-3,b=5,l=5,r=-11),                                                        
              plot.background = element_rect(fill = NA, color = NA)),                                         
      vp = vplayout(2,1))                                                                                     
print(Oct_ILR_cov_loc_2_plot +                                                                            
        scale_x_date(limits = c(as.Date('1990-10-01'), as.Date('2011-10-01')),                                
                     breaks = seq(as.Date('1990-10-1'), by = "2 years", length.out = 22),  
                     date_labels = '%b-%y',
                     expand = c(0.01, 0.01)) +                                                                
        theme(axis.text.y = element_blank(),                                                                  
              axis.title.y = element_blank(),                                                                 
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
              axis.title.y = element_blank(),                                                                 
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
  pdf(paste0(out_dir,fig_name,'.pdf'), width = fig_width, height = fig_height)                                
}                                                                                                             
grid.newpage()                                                                                                
pushViewport(viewport(layout = grid.layout(2,3)))                                                             
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)                                   
print(Oct_MAR_ILR_uni_loc_1_plot +                                                                            
        scale_x_date(limits = c(as.Date('1990-10-01'), as.Date('2011-10-01')),                                
                     breaks = seq(as.Date('1990-10-1'), by = "2 years", length.out = 22),                     
                     expand = c(0.01, 0.01)) +                                                                
        ylab('Prediction Location 1\nStreamflow Difference (cfs)') +                                          
        theme(axis.text.x = element_blank(),                                                                  
              plot.margin = margin(t=5,b=13,l=5,r=-11),                                                        
              plot.background = element_rect(fill = NA, color = NA)),                                         
      vp = vplayout(1,1))                                                                                     
print(Oct_MAR_ILR_cov_loc_1_plot +                                                                            
        scale_x_date(limits = c(as.Date('1990-10-01'), as.Date('2011-10-01')),                                
                     breaks = seq(as.Date('1990-10-1'), by = "2 years", length.out = 22),                     
                     expand = c(0.01, 0.01)) +                                                                
        theme(axis.text.x = element_blank(),                                                                  
              axis.text.y = element_blank(),                                                                  
              axis.title.y = element_blank(),                                                                 
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
              axis.title.y = element_blank(),                                                                 
              legend.position = 'none',                                                                       
              plot.margin = margin(t=5,b=13,l=13,r=5),                                                         
              plot.background = element_rect(fill = NA, color = NA)),                                         
      vp = vplayout(1,3))                                                                                     
print(Oct_MAR_ILR_uni_loc_2_plot +                                                                            
        scale_x_date(limits = c(as.Date('1990-10-01'), as.Date('2011-10-01')),                                
                     breaks = seq(as.Date('1990-10-1'), by = "2 years", length.out = 22), 
                     date_labels = '%b-%y',
                     expand = c(0.01, 0.01)) +                                                                
        ylab('Prediction Location 2\nStreamflow Difference (cfs)') +                                          
        theme(plot.margin = margin(t=-3,b=5,l=5,r=-11),                                                        
              plot.background = element_rect(fill = NA, color = NA)),                                         
      vp = vplayout(2,1))                                                                                     
print(Oct_MAR_ILR_cov_loc_2_plot +                                                                            
        scale_x_date(limits = c(as.Date('1990-10-01'), as.Date('2011-10-01')),                                
                     breaks = seq(as.Date('1990-10-1'), by = "2 years", length.out = 22),  
                     date_labels = '%b-%y',
                     expand = c(0.01, 0.01)) +                                                                
        theme(axis.text.y = element_blank(),                                                                  
              axis.title.y = element_blank(),                                                                 
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
              axis.title.y = element_blank(),                                                                 
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
  png(paste0(out_dir,fig_name,'.png'), width = fig_width, height = fig_height, units = fig_units,  res = 600)
} else if (fig_format == 'pdf'){
  pdf(paste0(out_dir,fig_name,'.pdf'), width = fig_width, height = fig_height)
}
grid.newpage()                                                                                                
pushViewport(viewport(layout = grid.layout(2,3)))                                                             
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)                                   
print(Aug_Sep_Oct_MAR_violin_loc_1_plot +                                                                            
        ylab('Prediction Location 1\nStreamflow Difference (cfs)') + 
        theme(axis.text.x = element_blank(),
              legend.position = c(0.25, 0.92),
              legend.text = element_text(size = 6),
              legend.key.size = unit(0.2, 'cm'),
              legend.direction = 'horizontal',
              plot.margin = margin(t=5,b=13,l=5,r=-11),                                                        
              plot.background = element_rect(fill = NA, color = NA)),                                         
      vp = vplayout(1,1))                                                                                     
print(Aug_Sep_Oct_ILR_violin_loc_1_plot +                                                                            
        theme(axis.text.x = element_blank(),                                                                  
              axis.text.y = element_blank(),                                                                  
              axis.title.y = element_blank(),                                                                 
              legend.position = 'none',                                                                       
              plot.margin = margin(t=5,b=13,l=21,r=-3),                                                        
              plot.background = element_rect(fill = NA, color = NA)),                                         
      vp = vplayout(1,2))                                                                                     
print(Aug_Sep_Oct_MAR_ILR_violin_loc_1_plot +                                                                          
        theme(axis.text.x = element_blank(),                                                                  
              axis.text.y = element_blank(),                                                                  
              axis.title.y = element_blank(),                                                                 
              legend.position = 'none',                                                                       
              plot.margin = margin(t=5,b=13,l=13,r=5),                                                         
              plot.background = element_rect(fill = NA, color = NA)),                                         
      vp = vplayout(1,3))                                                                                     
print(Aug_Sep_Oct_MAR_violin_loc_2_plot +                                                                            
        ylab('Prediction Location 2\nStreamflow Difference (cfs)') +                                          
        theme(plot.margin = margin(t=-3,b=5,l=5,r=-11),
              legend.position = 'none',
              plot.background = element_rect(fill = NA, color = NA)),                                         
      vp = vplayout(2,1))                                                                                     
print(Aug_Sep_Oct_ILR_violin_loc_2_plot +                                                                            
        theme(axis.text.y = element_blank(),                                                                  
              axis.title.y = element_blank(),                                                                 
              legend.position = 'none',                                                                   
              plot.margin = margin(t=-3,b=5,l=21,r=-3),                                                        
              plot.background = element_rect(fill = NA, color = NA)),                                         
      vp = vplayout(2,2))                                                                                     
print(Aug_Sep_Oct_MAR_ILR_violin_loc_2_plot +                                                                          
        theme(axis.text.y = element_blank(),                                                                  
              axis.title.y = element_blank(),                                                                 
              legend.position = 'none',                                                                       
              plot.margin = margin(t=-3,b=5,l=13,r=5),                                                         
              plot.background = element_rect(fill = NA, color = NA)),                                         
      vp = vplayout(2,3))                                                                                     
graphics.off() 

# Legacy Plotting Code Below ----------------------------------------------------


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

