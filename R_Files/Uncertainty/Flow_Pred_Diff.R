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
MAR_Comp = TRUE
ILR_Comp = TRUE
MAR_ILR_Comp = TRUE
Transfer_Files_From_Cluster = FALSE

crit_months = c('Aug','Sep','Oct')   #Critically dry months, used for generating plots

UniRandPar_Basecase_Dir = paste0(getwd(),'/UniRandPar_out/Basecase_Preds/')
UniRandPar_MAR_Dir = paste0(getwd(),'/UniRandPar_out/MAR_Preds/')
UniRandPar_ILR_Dir = paste0(getwd(),'/UniRandPar_out/ILR_Preds/')
UniRandPar_MAR_ILR_Dir = paste0(getwd(),'/UniRandPar_out/MAR_ILR_Preds/')
CovRandPar_Basecase_Dir = paste0(getwd(),'/CovRandPar_out/Basecase_Preds/')
CovRandPar_MAR_Dir = paste0(getwd(),'/CovRandPar_out/MAR_Preds/')
CovRandPar_ILR_Dir = paste0(getwd(),'/CovRandPar_out/ILR_Preds/')
CovRandPar_MAR_ILR_Dir = paste0(getwd(),'/CovRandPar_out/MAR_ILR_Preds/')


# Check SSWRs -------------------------------------------------------------
if (Transfer_Files_From_Cluster==TRUE){
  session <- ssh_connect("dtolley@aqua.lawr.ucdavis.edu")
  scp_download(session,files = '/aqua/dtolley/unirandpar/basecase/UCODE_UOUT/*',to = './UniRandPar_out/Basecase_UOUT/')
  scp_download(session,files = '/aqua/dtolley/covrandpar/basecase/UCODE_UOUT/*',to = './CovRandPar_out/Basecase_UOUT/')
  ssh_disconnect(session)
}

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
  geom_point() +
  ggtitle('Uniform Distribution') +
    scale_y_continuous(limits = c(8.5E4,1.35E5), breaks = seq(8.5E4,1.35E5,by = 1E4), expand = c(0,0)) +
  geom_hline(yintercept = 8.9E4, color = 'purple', size = 0.75) + 
  geom_hline(yintercept = 8.9E4*1.1, color = 'purple', linetype="dashed", size = 0.75) +
  geom_hline(yintercept = 9.66E4, color = 'goldenrod4', size = 0.75) + 
  geom_hline(yintercept = 9.66E4*1.1, color = 'goldenrod4', linetype="dashed", size = 0.75) +
  theme_few() + 
  theme(plot.title = element_text(hjust = 0.5))
)

(CovRandPar_SSWR_plot = ggplot(CovRandPar_SSWRs, aes(x = Param_Set_Num, y = SSWR, col = Calibration)) + 
    geom_point() +
    ggtitle('Normal Distribution') +
    scale_color_manual(values = c('Red','Blue','darkgreen','purple','goldenrod4')) +
    scale_y_continuous(limits = c(8.5E4,1.35E5), breaks = seq(8.5E4,1.35E5,by = 1E4), expand = c(0,0)) +
    geom_hline(yintercept = 8.9E4, color = 'purple', size = 0.75) + 
    geom_hline(yintercept = 8.9E4*1.1, color = 'purple', linetype="dashed", size = 0.75) +
    geom_hline(yintercept = 9.66E4, color = 'goldenrod4', size = 0.75) + 
    geom_hline(yintercept = 9.66E4*1.1, color = 'goldenrod4', linetype="dashed", size = 0.75) +
    theme_few() + 
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = c(0.35,0.95),
          legend.direction = 'horizontal',
          legend.title = element_blank(),
          legend.background = element_rect(fill= NA),
          legend.key = element_rect(fill = NA))
)

png(filename = 'SSWRs_Uniform_Dist.png', width = 5, height = 4, res = 600, units = 'in')
print(UniRandPar_SSWR_plot)
graphics.off()
png(filename = 'SSWRs_Normal_Dist.png', width = 5, height = 4, res = 600, units = 'in')
print(CovRandPar_SSWR_plot)
graphics.off()

# Read Streamflow Prediction Function -------------------------------------
Flow_Preds = function(pred_dir, scenario, randpartype){  #pred_dir is a directory, scenario and randpartype are strings for labeling output
  if (randpartype=='uni'){
  filenames = list.files(path = pred_dir,pattern = '.dat')
  param_set_nums = as.numeric(gsub(x = filenames, pattern = 'Flow_Predictions_', replacement = '')%>%gsub(pattern = '.dat', replacement = ''))
  date_info = read.table(paste0(pred_dir,filenames[1]), header = T, stringsAsFactors = F)[,1:3]
  preds = lapply(paste0(pred_dir,filenames), FUN = function(x) read.table(x, header = T, stringsAsFactors = F))
  
  for (j in 1:5){
    eval(parse(text = paste0("loc_",j,"_preds = as.data.frame(sapply(preds,'[[',",j+3,"))")))
    eval(parse(text = paste0("names(loc_",j,"_preds) = param_set_nums")))
    eval(parse(text = paste0("loc_",j,"_preds = cbind(date_info,loc_",j,"_preds[, order(as.numeric(names(loc_",j,"_preds)))])")))
    date_info <<- date_info
    eval(parse(text = paste0(randpartype,"_",scenario,"_loc_",j,"_preds <<- loc_",j,"_preds")))
  }
  } else if (randpartype=='cov'){
    for(i in 1:5){
    eval(parse(text = paste0("filenames_cal_",i," = list.files(path = pred_dir,pattern = 'Cal_",i,"')")))
    eval(parse(text = paste0("param_set_nums_cal_",i," = as.numeric(gsub(x = filenames_cal_",i,", pattern = 'Flow_Predictions_Cal_",i,"_', replacement = '')%>%gsub(pattern = '.dat', replacement = ''))")))
    date_info = read.table(paste0(pred_dir,filenames_cal_1[1]), header = T, stringsAsFactors = F)[,1:3]
    eval(parse(text = paste0("preds_cal_",i," = lapply(paste0(pred_dir,filenames_cal_",i,"), FUN = function(x) read.table(x, header = T, stringsAsFactors = F))")))
    
    for(j in 1:5){
      eval(parse(text = paste0("cal_",i,"_loc_",j,"_preds = as.data.frame(sapply(preds_cal_",i,",'[[',",j+3,"))")))
      eval(parse(text = paste0("names(cal_",i,"_loc_",j,"_preds) = param_set_nums_cal_",i,"")))
      eval(parse(text = paste0("cal_",i,"_loc_",j,"_preds = cbind(date_info,cal_",i,"_loc_",j,"_preds[, order(as.numeric(names(cal_",i,"_loc_",j,"_preds)))])")))
      date_info <<- date_info
      eval(parse(text = paste0(randpartype,"_",scenario,"_cal_",i,"_loc_",j,"_preds <<- cal_",i,"_loc_",j,"_preds")))
    }
    }
  }
}
###########################################################################################
#####################                   BASECASE                    #######################
###########################################################################################
if (UniRandPar==TRUE){
  flow_pred_dir = UniRandPar_Basecase_Dir
  Flow_Preds(pred_dir = flow_pred_dir, scenario = 'basecase', randpartype = 'uni')
}
if (CovRandPar==TRUE){
  flow_pred_dir = CovRandPar_Basecase_Dir
  Flow_Preds(pred_dir = flow_pred_dir, scenario = 'basecase', randpartype = 'cov')
}

###########################################################################################
#####################                      MAR                      #######################
###########################################################################################
if (MAR_Comp==TRUE){
  if (UniRandPar==TRUE){
    flow_pred_dir = UniRandPar_MAR_Dir
    Flow_Preds(pred_dir = flow_pred_dir, scenario = 'MAR', randpartype = 'uni')
    param_match = intersect(names(uni_basecase_loc_1_preds),names(uni_MAR_loc_1_preds))
    for (j in 1:5){
      eval(parse(text = paste0('temp = subset(uni_MAR_loc_',j,'_preds,select = param_match)[,c(-1,-2,-3)] - subset(uni_basecase_loc_',j,'_preds,select = param_match)[,c(-1,-2,-3)]')))
      eval(parse(text = paste0('uni_MAR_diff_loc_',j,' = cbind(date_info,temp)')))
    }
  }
  if (CovRandPar==TRUE){
    flow_pred_dir = CovRandPar_MAR_Dir
    Flow_Preds(pred_dir = flow_pred_dir, scenario = 'MAR', randpartype = 'cov')
    for (i in 1:5){
      eval(parse(text = paste0('param_match_',i,' = intersect(names(cov_basecase_cal_',i,'_loc_1_preds),names(cov_MAR_cal_',i,'_loc_1_preds))')))
      for (j in 1:5){
        eval(parse(text = paste0('temp = subset(cov_MAR_cal_',i,'_loc_',j,'_preds,select = param_match_',i,')[,c(-1,-2,-3)] - subset(cov_basecase_cal_',i,'_loc_',j,'_preds,select = param_match_',i,')[,c(-1,-2,-3)]')))
        eval(parse(text = paste0('cov_cal_',i,'_MAR_diff_loc_',j,' = cbind(date_info,temp)')))
      }
    }
  }
}

###########################################################################################
#####################                      ILR                      #######################
###########################################################################################
if (ILR_Comp==TRUE){
  if (UniRandPar==TRUE){
    flow_pred_dir = UniRandPar_ILR_Dir
    Flow_Preds(pred_dir = flow_pred_dir, scenario = 'ILR', randpartype = 'uni')
    param_match = intersect(names(uni_basecase_loc_1_preds),names(uni_ILR_loc_1_preds))
    for (j in 1:5){
      eval(parse(text = paste0('temp = subset(uni_ILR_loc_',j,'_preds,select = param_match)[,c(-1,-2,-3)] - subset(uni_basecase_loc_',j,'_preds,select = param_match)[,c(-1,-2,-3)]')))
      eval(parse(text = paste0('uni_ILR_diff_loc_',j,' = cbind(date_info,temp)')))
    }
  }
  if (CovRandPar==TRUE){
    flow_pred_dir = CovRandPar_ILR_Dir
    Flow_Preds(pred_dir = flow_pred_dir, scenario = 'ILR', randpartype = 'cov')
    for (i in 1:5){
      eval(parse(text = paste0('param_match_',i,' = intersect(names(cov_basecase_cal_',i,'_loc_1_preds),names(cov_ILR_cal_',i,'_loc_1_preds))')))
      for (j in 1:5){
        eval(parse(text = paste0('temp = subset(cov_ILR_cal_',i,'_loc_',j,'_preds,select = param_match_',i,')[,c(-1,-2,-3)] - subset(cov_basecase_cal_',i,'_loc_',j,'_preds,select = param_match_',i,')[,c(-1,-2,-3)]')))
        eval(parse(text = paste0('cov_cal_',i,'_ILR_diff_loc_',j,' = cbind(date_info,temp)')))
      }
    }
  }
}
###########################################################################################
###################                      MAR_ILR                      #####################
###########################################################################################
if (MAR_ILR_Comp==TRUE){
  if (UniRandPar==TRUE){
    flow_pred_dir = UniRandPar_MAR_ILR_Dir
    Flow_Preds(pred_dir = flow_pred_dir, scenario = 'MAR_ILR', randpartype = 'uni')
    param_match = intersect(names(uni_MAR_ILR_loc_1_preds),names(uni_ILR_loc_1_preds))
    for (j in 1:5){
      eval(parse(text = paste0('temp = subset(uni_MAR_ILR_loc_',j,'_preds,select = param_match)[,c(-1,-2,-3)] - subset(uni_basecase_loc_',j,'_preds,select = param_match)[,c(-1,-2,-3)]')))
      eval(parse(text = paste0('uni_MAR_ILR_diff_loc_',j,' = cbind(date_info,temp)')))
    }
  }
  if (CovRandPar==TRUE){
    flow_pred_dir = CovRandPar_MAR_ILR_Dir
    Flow_Preds(pred_dir = flow_pred_dir, scenario = 'MAR_ILR', randpartype = 'cov')
    for (i in 1:5){
      eval(parse(text = paste0('param_match_',i,' = intersect(names(cov_basecase_cal_',i,'_loc_1_preds),names(cov_MAR_ILR_cal_',i,'_loc_1_preds))')))
      for (j in 1:5){
        eval(parse(text = paste0('temp = subset(cov_MAR_ILR_cal_',i,'_loc_',j,'_preds,select = param_match_',i,')[,c(-1,-2,-3)] - subset(cov_basecase_cal_',i,'_loc_',j,'_preds,select = param_match_',i,')[,c(-1,-2,-3)]')))
        eval(parse(text = paste0('cov_cal_',i,'_MAR_ILR_diff_loc_',j,' = cbind(date_info,temp)')))
      }
    }
  }
}
###########################################################################################
#################                 STREAMFLOW  PLOTS                   #####################
###########################################################################################
if(UniRandPar==TRUE){
  for (j in 1:5){
    eval(parse(text = paste0('mean_cfs = apply(uni_basecase_loc_',j,'_preds[,c(-1,-2,-3)],1,mean)*0.000408734569')))
    eval(parse(text = paste0('max_cfs = apply(uni_basecase_loc_',j,'_preds[,c(-1,-2,-3)],1,max)*0.000408734569')))
    eval(parse(text = paste0('min_cfs = apply(uni_basecase_loc_',j,'_preds[,c(-1,-2,-3)],1,min)*0.000408734569')))
    eval(parse(text = paste0('summary = cbind(date_info, mean_cfs, max_cfs, min_cfs)')))
      for(m in 1:length(crit_months)){
      temp_data = subset(summary,Month==crit_months[m])
      Plot = ggplot(data = temp_data, aes(x = Water_Year)) + 
        geom_ribbon(aes(ymin = min_cfs, ymax = max_cfs), alpha = 0.75, fill = 'orange') + 
        geom_line(aes(y = mean_cfs),size = 1) +
        ylab('Streamflow (cfs)') +
        #scale_color_manual(values = c('Red','Blue','Burlywood3','Purple','green')) +
        scale_x_continuous(limits = c(1991,2011), breaks = seq(1991,2011,by=2), expand = c(0,0)) +
        scale_y_continuous(limits = c(0,160), breaks = seq(0,160,by=40),expand = c(0,0)) +
        theme(plot.background = element_blank(),
              panel.background = element_rect(fill = NA, color = 'black'),
              panel.border = element_rect(fill = NA, color = 'black'),
              axis.text.y  = element_text(size = 20),
              axis.title.y = element_text(size = 20),
              axis.title.x = element_blank()
        )
      eval(parse(text = paste0(crit_months[m],'_uni_Streamflow_loc_',j,'_plot = Plot')))
      
      #Violin Plot (by year)
      eval(parse(text = paste0('uni_basecase_loc_',j,'_preds_melt = subset(melt(uni_basecase_loc_',j,'_preds,id.vars = names(date_info)),Month==crit_months[',m,'])')))
      eval(parse(text = paste0('temp_data = uni_basecase_loc_',j,'_preds_melt')))
      ViolinPlot = ggplot(data = temp_data, aes(x = Water_Year, y = value*0.000408734569, group = Water_Year)) +
        geom_violin(fill = 'skyblue',draw_quantiles = c(0.5)) +
        ylab('Streamflow (cfs)') +
        ##scale_fill_manual(values = c('Red','Blue','Burlywood3','Purple','green')) +
        scale_x_continuous(limits = c(1990,2012), breaks = seq(1991,2011,by=2), expand = c(0,0)) +
        scale_y_continuous(limits = c(0,160), breaks = seq(0,160,by=40), expand = c(0,0)) +
        theme(plot.background = element_blank(),
              panel.background = element_rect(fill = NA, color = 'black'),
              panel.border = element_rect(fill = NA, color = 'black'),
              axis.text.y  = element_text(size = 20),
              axis.title.y = element_text(size = 20),
              axis.title.x = element_blank()
        )
      eval(parse(text = paste0(crit_months[m],'_uni_Streamflow_violin_loc_',j,'_plot = ViolinPlot'))) 
      
      #Violin Plot (by month)
      eval(parse(text = paste0('temp_data = melt(uni_basecase_loc_',j,'_preds,id.vars = names(date_info))')))
      temp_data$Month = factor(temp_data$Month, levels = format(seq(as.Date('1990-01-01'), as.Date('1990-12-01'), by = 'month'),'%b'))
      temp_data = temp_data[order(temp_data$Month),]
      ViolinPlot = ggplot(data = subset(temp_data,Month==crit_months), aes(x = factor(Month), y = value*0.000408734569, fill = Month)) +
        geom_violin(position = position_dodge(width = 0.8),draw_quantiles = c(0.5)) +
        ylab('Streamflow (cfs)') +
        #scale_fill_manual(values = c('Red','Blue','Burlywood3','Purple','green')) +
        scale_y_continuous(limits = c(0,160), breaks = seq(0,160,by=40), expand = c(0,0)) +
        theme(plot.background = element_blank(),
              panel.background = element_rect(fill = NA, color = 'black'),
              panel.border = element_rect(fill = NA, color = 'black'),
              axis.text.y  = element_text(size = 20),
              axis.title.y = element_text(size = 20),
              axis.title.x = element_blank()
        )
      eval(parse(text = paste0('Monthly_uni_Streamflow_violin_loc_',j,'_plot = ViolinPlot'))) 
    }
  } 
}
if(CovRandPar==TRUE){
  for (i in 1:5){
    for (j in 1:5){
      eval(parse(text = paste0('cal_',i,'_loc_',j,'_mean_cfs = apply(cov_basecase_cal_',i,'_loc_',j,'_preds[,c(-1,-2,-3)],1,mean)*0.000408734569')))
      eval(parse(text = paste0('cal_',i,'_loc_',j,'_max_cfs = apply(cov_basecase_cal_',i,'_loc_',j,'_preds[,c(-1,-2,-3)],1,max)*0.000408734569')))
      eval(parse(text = paste0('cal_',i,'_loc_',j,'_min_cfs = apply(cov_basecase_cal_',i,'_loc_',j,'_preds[,c(-1,-2,-3)],1,min)*0.000408734569')))
      eval(parse(text = paste0('cal_',i,'_loc_',j,' = cbind(date_info, cal_',i,'_loc_',j,'_mean_cfs, cal_',i,'_loc_',j,'_max_cfs, cal_',i,'_loc_',j,'_min_cfs)')))
      eval(parse(text = paste0("names(cal_",i,"_loc_",j,") = c(names(date_info),'mean_cfs', 'max_cfs', 'min_cfs')")))
      }
  }
  for(m in 1:length(crit_months)){
    for (j in 1:5){
      eval(parse(text = paste0("loc_",j,"_ensemble = cbind(date_info,
                                           apply(cbind(cov_basecase_cal_1_loc_",j,"_preds[,c(-1,-2,-3)],
                                                       cov_basecase_cal_2_loc_",j,"_preds[,c(-1,-2,-3)],
                                                       cov_basecase_cal_3_loc_",j,"_preds[,c(-1,-2,-3)],
                                                       cov_basecase_cal_4_loc_",j,"_preds[,c(-1,-2,-3)],
                                                       cov_basecase_cal_5_loc_",j,"_preds[,c(-1,-2,-3)]),1,mean)*0.000408734569,
                                           apply(cbind(cov_basecase_cal_1_loc_",j,"_preds[,c(-1,-2,-3)],
                                                       cov_basecase_cal_2_loc_",j,"_preds[,c(-1,-2,-3)],
                                                       cov_basecase_cal_3_loc_",j,"_preds[,c(-1,-2,-3)],
                                                       cov_basecase_cal_4_loc_",j,"_preds[,c(-1,-2,-3)],
                                                       cov_basecase_cal_5_loc_",j,"_preds[,c(-1,-2,-3)]),1,max)*0.000408734569,
                                           apply(cbind(cov_basecase_cal_1_loc_",j,"_preds[,c(-1,-2,-3)],
                                                       cov_basecase_cal_2_loc_",j,"_preds[,c(-1,-2,-3)],
                                                       cov_basecase_cal_3_loc_",j,"_preds[,c(-1,-2,-3)],
                                                       cov_basecase_cal_4_loc_",j,"_preds[,c(-1,-2,-3)],
                                                       cov_basecase_cal_5_loc_",j,"_preds[,c(-1,-2,-3)]),1,min)*0.000408734569)")))
      eval(parse(text = paste0("names(loc_",j,"_ensemble) = c(names(date_info), 'mean_cfs','max_cfs', 'min_cfs')")))
        Plot = ggplot() + 
            eval(parse(text = paste0("geom_ribbon(data = subset(cal_1_loc_",j,",Month==crit_months[m]),
                                     aes(x = Water_Year, ymin = min_cfs, ymax = max_cfs,fill = 'Cal_1'), alpha = 0.15)"))) +
            eval(parse(text = paste0("geom_ribbon(data = subset(cal_2_loc_",j,",Month==crit_months[m]),
                                     aes(x = Water_Year, ymin = min_cfs, ymax = max_cfs,fill = 'Cal_2'), alpha = 0.15)"))) +
            eval(parse(text = paste0("geom_ribbon(data = subset(cal_3_loc_",j,",Month==crit_months[m]),
                                     aes(x = Water_Year, ymin = min_cfs, ymax = max_cfs,fill = 'Cal_3'), alpha = 0.15)"))) +
            eval(parse(text = paste0("geom_ribbon(data = subset(cal_4_loc_",j,",Month==crit_months[m]),
                                     aes(x = Water_Year, ymin = min_cfs, ymax = max_cfs,fill = 'Cal_4'), alpha = 0.15)"))) +
            eval(parse(text = paste0("geom_ribbon(data = subset(cal_5_loc_",j,",Month==crit_months[m]),
                                     aes(x = Water_Year, ymin = min_cfs, ymax = max_cfs,fill = 'Cal_5'), alpha = 0.15)"))) +
            eval(parse(text = paste0("geom_ribbon(data = subset(loc_",j,"_ensemble,Month==crit_months[m]),
                                     aes(x = Water_Year, ymin = min_cfs, ymax = max_cfs,fill = 'Ensemble'), alpha = 0)"))) +
            eval(parse(text = paste0("geom_line(data = subset(cal_1_loc_",j,",Month==crit_months[m]),
                                     aes(x = Water_Year, y = mean_cfs, color = 'Cal_1'), size = 1)"))) +
            eval(parse(text = paste0("geom_line(data = subset(cal_2_loc_",j,",Month==crit_months[m]),
                                     aes(x = Water_Year, y = mean_cfs, color = 'Cal_2'), size = 1)"))) +
            eval(parse(text = paste0("geom_line(data = subset(cal_3_loc_",j,",Month==crit_months[m]),
                                     aes(x = Water_Year, y = mean_cfs, color = 'Cal_3'), size = 1)"))) +
            eval(parse(text = paste0("geom_line(data = subset(cal_4_loc_",j,",Month==crit_months[m]),
                                     aes(x = Water_Year, y = mean_cfs, color = 'Cal_4'), size = 1)"))) +
            eval(parse(text = paste0("geom_line(data = subset(cal_5_loc_",j,",Month==crit_months[m]),
                                     aes(x = Water_Year, y = mean_cfs, color = 'Cal_5'), size = 1)"))) +
            eval(parse(text = paste0("geom_line(data = subset(loc_",j,"_ensemble,Month==crit_months[m]),
                                     aes(x = Water_Year, y = mean_cfs, color = 'Ensemble'), size = 1)"))) +
          scale_color_manual("",values = c('red','blue','forestgreen','purple','cyan4','black')) +
          scale_fill_manual("",values = c('red','blue','forestgreen','purple','cyan4','black')) +
          ylab('Streamflow (cfs)') +
            scale_x_continuous(limits = c(1991,2011), breaks = seq(1991,2011,by=2), expand = c(0,0)) +
            scale_y_continuous(limits = c(0,160), breaks = seq(0,160,by=40),expand = c(0,0)) +
            theme(plot.background = element_blank(),
                  panel.background = element_rect(fill = NA, color = 'black'),
                  panel.border = element_rect(fill = NA, color = 'black'),
                  axis.text.y  = element_text(size = 20),
                  axis.title.y = element_text(size = 20),
                  axis.title.x = element_blank(),
                  legend.position = c(0.8,0.8),
                  legend.background = element_rect(fill = NA, color = NA),
                  legend.text = element_text(size = 20)
          )
        eval(parse(text = paste0(crit_months[m],'_cov_Streamflow_loc_',j,'_plot = Plot')))
        
        #Violin plots grouped by month (all calibrations)
        eval(parse(text = paste0("loc_",j,"_flows = rbind(cov_basecase_cal_1_loc_",j,"_preds,
                                                          cov_basecase_cal_2_loc_",j,"_preds,
                                                          cov_basecase_cal_3_loc_",j,"_preds,
                                                          cov_basecase_cal_4_loc_",j,"_preds,
                                                          cov_basecase_cal_5_loc_",j,"_preds)")))
        eval(parse(text = paste0("loc_",j,"_flows$Cal = rep(c('Cal_1', 'Cal_2', 'Cal_3', 'Cal_4', 'Cal_5'), each = 252)")))
        eval(parse(text = paste0("loc_",j,"_flows_melt = melt(loc_",j,"_flows, id.vars = c(names(date_info), 'Cal'))")))
        eval(parse(text = paste0("loc_",j,"_flows_melt$Month = factor(loc_",j,"_flows_melt$Month, levels = format(seq(as.Date('1990-01-01'), as.Date('1990-12-01'), by = 'month'),'%b'))")))
        eval(parse(text = paste0("loc_",j,"_flows_melt = loc_",j,"_flows_melt[order(loc_",j,"_flows_melt$Month),]")))
        eval(parse(text = paste0("data_melt = loc_",j,"_flows_melt")))
        
        #Violin plot (all calibrations)
        ViolinPlot_All = ggplot(data = subset(data_melt,Month%in%crit_months), aes(x = factor(Month), y = value*0.000408734569, fill = Cal)) +
            geom_hline(yintercept = 0) +
          geom_violin(position = position_dodge(width = 0.8),draw_quantiles = c(0.5)) +
          ylab('Streamflow (cfs)') +
          scale_y_continuous(limits = c(0,160), breaks = seq(0,160,by = 40), expand = c(0,0)) +
          #scale_fill_manual(values = c('Red','Blue','Burlywood3','Purple','green')) +
          theme(plot.background = element_blank(),
                panel.background = element_rect(fill = NA, color = 'black'),
                panel.border = element_rect(fill = NA, color = 'black'),
                axis.title.x = element_blank(),
                axis.title.y = element_text(size = 20),
                axis.text.x = element_text(size = 20),
                axis.text.y = element_text(size = 20))
        eval(parse(text = paste0('Monthly_Streamflow_cov_loc_',j,'_plot = ViolinPlot_All')))
        
        #Violin Plot (specific months)
        ViolinPlot_All = ggplot(data = subset(data_melt, Month == crit_months[m]), aes(x = Water_Year, y = value*0.000408734569, group = Water_Year)) +
        geom_hline(yintercept = 0) +
        geom_violin(position = position_dodge(width = 0.8),draw_quantiles = c(0.5)) +
        ylab('Streamflow (cfs)') +
          #scale_fill_manual(values = c('Red','Blue','Burlywood3','Purple','green')) +
          scale_y_continuous(limits = c(0,160), breaks = seq(0,160,by = 40), expand = c(0,0)) +
        theme(plot.background = element_blank(),
              panel.background = element_rect(fill = NA, color = 'black'),
              panel.border = element_rect(fill = NA, color = 'black'),
              axis.title.x = element_blank(),
              axis.title.y = element_text(size = 20),
              axis.text.x = element_text(size = 20),
              axis.text.y = element_text(size = 20))
           eval(parse(text = paste0(crit_months[m],'_cov_Streamflow_Violin_loc_',j,'_plot = ViolinPlot_All')))
    }
  }
}

###########################################################################################
###################                   MAR PLOTS                       #####################
###########################################################################################
if (UniRandPar == TRUE){
  #Monthly Series
  for (i in 1:5){
    eval(parse(text = paste0('mean_cfs = apply(uni_MAR_diff_loc_',i,'[,c(-1,-2,-3)],1,mean)*0.000408734569')))
    eval(parse(text = paste0('sd_cfs = apply(uni_MAR_diff_loc_',i,'[,c(-1,-2,-3)],1,sd)*0.000408734569')))
    eval(parse(text = paste0('min_cfs = apply(uni_MAR_diff_loc_',i,'[,c(-1,-2,-3)],1,min)*0.000408734569')))
    eval(parse(text = paste0('max_cfs = apply(uni_MAR_diff_loc_',i,'[,c(-1,-2,-3)],1,max)*0.000408734569')))
    eval(parse(text = paste0('summary = cbind(date_info, mean_cfs, sd_cfs, min_cfs, max_cfs)')))
    for(m in 1:length(crit_months)){
      temp_data = subset(summary,Month==crit_months[m])
      Plot = ggplot(data = temp_data, aes(x = Water_Year)) + 
      geom_ribbon(aes(ymin = min_cfs, ymax = max_cfs), alpha = 0.5, fill = 'darkgray') + 
      geom_ribbon(aes(ymin = (mean_cfs - 2*sd_cfs), ymax = (mean_cfs + 2*sd_cfs)), alpha = 0.75, fill = 'dodgerblue') + 
      geom_line(aes(y = mean_cfs),size = 1) +
      geom_hline(yintercept = 0, alpha = 0.5) +
      ylab('Difference (cfs)') +
      scale_color_manual(values = c('black')) +
      scale_x_continuous(limits = c(1991,2011), breaks = seq(1991,2011,by=2), expand = c(0,0)) +
      scale_y_continuous(limits = c(-30,120), breaks = seq(-30,120,by=10),expand = c(0,0)) +
      coord_cartesian(ylim=c(-20, 40)) +
      theme(plot.background = element_blank(),
            panel.background = element_rect(fill = NA, color = 'black'),
            panel.border = element_rect(fill = NA, color = 'black'),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 20),
            axis.text.x = element_text(size = 20),
            axis.text.y = element_text(size = 20)
            )
      eval(parse(text = paste0(crit_months[m],'_MAR_uni_loc_',i,'_plot = Plot')))
    }
    #Violin plots grouped by month
    eval(parse(text = paste0('data_melt = melt(uni_MAR_diff_loc_',i,',id.vars = names(date_info))')))
    data_melt$Month = factor(data_melt$Month, levels = format(seq(as.Date('1990-01-01'), as.Date('1990-12-01'), by = 'month'),'%b'))
    data_melt = data_melt[order(data_melt$Month),]
    ViolinPlot = ggplot(data = subset(data_melt,Month==crit_months), aes(x = factor(Month), y = value*0.000408734569, group = Month, fill = Month)) + 
      geom_hline(yintercept = 0) +
      geom_violin(trim = TRUE,draw_quantiles = c(0.5)) +
      ylab('Difference (cfs)') +
      #scale_fill_manual(values = c('Red','Blue','Burlywood3','Purple','green')) +
      scale_y_continuous(limits = c(-120,120), breaks = seq(-120,120,by = 10), expand = c(0,0)) +
      coord_cartesian(ylim=c(-20, 40)) +
      theme(plot.background = element_blank(),
            panel.background = element_rect(fill = NA, color = 'black'),
            panel.border = element_rect(fill = NA, color = 'black'),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 20),
            axis.text.x = element_text(size = 20),
            axis.text.y = element_text(size = 20),
            legend.position = 'none')
     eval(parse(text = paste0('Monthly_MAR_uni_loc_',i,'_plot = ViolinPlot')))
  }
}
if(CovRandPar==TRUE){
  #Monthly Series
  for (i in 1:5){
    for (j in 1:5){
      eval(parse(text = paste0('cal_',i,'_loc_',j,'_MAR_diff_mean_cfs = apply(cov_cal_',i,'_MAR_diff_loc_',j,'[,c(-1,-2,-3)],1,mean)*0.000408734569')))
      eval(parse(text = paste0('cal_',i,'_loc_',j,'_MAR_diff_max_cfs = apply(cov_cal_',i,'_MAR_diff_loc_',j,'[,c(-1,-2,-3)],1,max)*0.000408734569')))
      eval(parse(text = paste0('cal_',i,'_loc_',j,'_MAR_diff_min_cfs = apply(cov_cal_',i,'_MAR_diff_loc_',j,'[,c(-1,-2,-3)],1,min)*0.000408734569')))
      eval(parse(text = paste0('cal_',i,'_loc_',j,'_MAR_diff = cbind(date_info, cal_',i,'_loc_',j,'_MAR_diff_mean_cfs, cal_',i,'_loc_',j,'_MAR_diff_max_cfs, cal_',i,'_loc_',j,'_MAR_diff_min_cfs)')))
      eval(parse(text = paste0("names(cal_",i,"_loc_",j,"_MAR_diff) = c(names(date_info),'MAR_diff_mean_cfs', 'MAR_diff_max_cfs', 'MAR_diff_min_cfs')")))
    }
  }
  for(m in 1:length(crit_months)){
    for (j in 1:5){
      eval(parse(text = paste0("loc_",j,"_MAR_diff_ensemble = cbind(date_info,
                   apply(cbind(cov_cal_1_MAR_diff_loc_",j,"[,c(-1,-2,-3)],
                               cov_cal_2_MAR_diff_loc_",j,"[,c(-1,-2,-3)],
                               cov_cal_3_MAR_diff_loc_",j,"[,c(-1,-2,-3)],
                               cov_cal_4_MAR_diff_loc_",j,"[,c(-1,-2,-3)],
                               cov_cal_5_MAR_diff_loc_",j,"[,c(-1,-2,-3)]),1,mean)*0.000408734569,
                   apply(cbind(cov_cal_1_MAR_diff_loc_",j,"[,c(-1,-2,-3)],
                               cov_cal_2_MAR_diff_loc_",j,"[,c(-1,-2,-3)],
                               cov_cal_3_MAR_diff_loc_",j,"[,c(-1,-2,-3)],
                               cov_cal_4_MAR_diff_loc_",j,"[,c(-1,-2,-3)],
                               cov_cal_5_MAR_diff_loc_",j,"[,c(-1,-2,-3)]),1,max)*0.000408734569,
                   apply(cbind(cov_cal_1_MAR_diff_loc_",j,"[,c(-1,-2,-3)],
                               cov_cal_2_MAR_diff_loc_",j,"[,c(-1,-2,-3)],
                               cov_cal_3_MAR_diff_loc_",j,"[,c(-1,-2,-3)],
                               cov_cal_4_MAR_diff_loc_",j,"[,c(-1,-2,-3)],
                               cov_cal_5_MAR_diff_loc_",j,"[,c(-1,-2,-3)]),1,min)*0.000408734569)")))
      eval(parse(text = paste0("names(loc_",j,"_MAR_diff_ensemble) = c(names(date_info), 'MAR_diff_mean_cfs','MAR_diff_max_cfs', 'MAR_diff_min_cfs')")))
      
      Plot = ggplot() + 
        geom_hline(yintercept = 0) +
        eval(parse(text = paste0("geom_ribbon(data = subset(cal_1_loc_",j,"_MAR_diff,Month==crit_months[m]),
                                 aes(x = Water_Year, ymin = MAR_diff_min_cfs, ymax = MAR_diff_max_cfs,fill = 'Cal_1'), alpha = 0.15)"))) +
        eval(parse(text = paste0("geom_ribbon(data = subset(cal_2_loc_",j,"_MAR_diff,Month==crit_months[m]),
                                 aes(x = Water_Year, ymin = MAR_diff_min_cfs, ymax = MAR_diff_max_cfs,fill = 'Cal_2'), alpha = 0.15)"))) +
        eval(parse(text = paste0("geom_ribbon(data = subset(cal_3_loc_",j,"_MAR_diff,Month==crit_months[m]),
                                 aes(x = Water_Year, ymin = MAR_diff_min_cfs, ymax = MAR_diff_max_cfs,fill = 'Cal_3'), alpha = 0.15)"))) +
        eval(parse(text = paste0("geom_ribbon(data = subset(cal_4_loc_",j,"_MAR_diff,Month==crit_months[m]),
                                 aes(x = Water_Year, ymin = MAR_diff_min_cfs, ymax = MAR_diff_max_cfs,fill = 'Cal_4'), alpha = 0.15)"))) +
        eval(parse(text = paste0("geom_ribbon(data = subset(cal_5_loc_",j,"_MAR_diff,Month==crit_months[m]),
                                 aes(x = Water_Year, ymin = MAR_diff_min_cfs, ymax = MAR_diff_max_cfs,fill = 'Cal_5'), alpha = 0.15)"))) +
        eval(parse(text = paste0("geom_ribbon(data = subset(loc_",j,"_MAR_diff_ensemble,Month==crit_months[m]),
                                 aes(x = Water_Year, ymin = MAR_diff_min_cfs, ymax = MAR_diff_max_cfs,fill = 'Ensemble'), alpha = 0)"))) +
        eval(parse(text = paste0("geom_line(data = subset(cal_1_loc_",j,"_MAR_diff,Month==crit_months[m]),
                                 aes(x = Water_Year, y = MAR_diff_mean_cfs, color = 'Cal_1'), size = 1)"))) +
        eval(parse(text = paste0("geom_line(data = subset(cal_2_loc_",j,"_MAR_diff,Month==crit_months[m]),
                                 aes(x = Water_Year, y = MAR_diff_mean_cfs, color = 'Cal_2'), size = 1)"))) +
        eval(parse(text = paste0("geom_line(data = subset(cal_3_loc_",j,"_MAR_diff,Month==crit_months[m]),
                                 aes(x = Water_Year, y = MAR_diff_mean_cfs, color = 'Cal_3'), size = 1)"))) +
        eval(parse(text = paste0("geom_line(data = subset(cal_4_loc_",j,"_MAR_diff,Month==crit_months[m]),
                                 aes(x = Water_Year, y = MAR_diff_mean_cfs, color = 'Cal_4'), size = 1)"))) +
        eval(parse(text = paste0("geom_line(data = subset(cal_5_loc_",j,"_MAR_diff,Month==crit_months[m]),
                                 aes(x = Water_Year, y = MAR_diff_mean_cfs, color = 'Cal_5'), size = 1)"))) +
        eval(parse(text = paste0("geom_line(data = subset(loc_",j,"_MAR_diff_ensemble,Month==crit_months[m]),
                                aes(x = Water_Year, y = MAR_diff_mean_cfs, color = 'Ensemble'), size = 1)"))) +
        scale_color_manual("",values = c('red','blue','forestgreen','purple','cyan4','black')) +
        scale_fill_manual("",values = c('red','blue','forestgreen','purple','cyan4','black')) +
        ylab('Difference (cfs)') +
        scale_x_continuous(limits = c(1991,2011), breaks = seq(1991,2011,by=2), expand = c(0,0)) +
        scale_y_continuous(limits = c(-30,120), breaks = seq(-30,120,by=10),expand = c(0,0)) +
        coord_cartesian(ylim=c(-20, 40)) +
        theme(plot.background = element_blank(),
              panel.background = element_rect(fill = NA, color = 'black'),
              panel.border = element_rect(fill = NA, color = 'black'),
              axis.text.y  = element_text(size = 20),
              axis.title.y = element_text(size = 20),
              axis.title.x = element_blank(),
              legend.position = c(0.8,0.8),
              legend.background = element_rect(fill = NA, color = NA),
              legend.text = element_text(size = 20)
        )
      eval(parse(text = paste0(crit_months[m],'_cov_MAR_diff_loc_',j,'_plot = Plot')))
      
      #Violin plots grouped by month (all calibrations)
      eval(parse(text = paste0("cov_MAR_diff_loc_",j," = rbind(cbind(cov_cal_1_MAR_diff_loc_",j,"),
                                                               cbind(cov_cal_2_MAR_diff_loc_",j,"),
                                                               cbind(cov_cal_3_MAR_diff_loc_",j,"),
                                                               cbind(cov_cal_4_MAR_diff_loc_",j,"),
                                                               cbind(cov_cal_5_MAR_diff_loc_",j,"))")))
      eval(parse(text = paste0("cov_MAR_diff_loc_",j,"$Cal = rep(c('Cal_1', 'Cal_2', 'Cal_3', 'Cal_4', 'Cal_5'), each = 252)")))
      eval(parse(text = paste0("cov_MAR_diff_loc_",j,"_melt = melt(cov_MAR_diff_loc_",j,", id.vars = c(names(date_info), 'Cal'))")))
      eval(parse(text = paste0("cov_MAR_diff_loc_",j,"_melt$Month = factor(cov_MAR_diff_loc_",j,"_melt$Month, levels = format(seq(as.Date('1990-01-01'), as.Date('1990-12-01'), by = 'month'),'%b'))")))
      eval(parse(text = paste0("cov_MAR_diff_loc_",j,"_melt = cov_MAR_diff_loc_",j,"_melt[order(cov_MAR_diff_loc_",j,"_melt$Month),]")))
      eval(parse(text = paste0("data_melt = cov_MAR_diff_loc_",j,"_melt")))
      
      ViolinPlot_All = ggplot(data = subset(data_melt,Month==crit_months), aes(x = factor(Month), y = value*0.000408734569, fill = Cal)) +
        geom_hline(yintercept = 0) +
        geom_violin(position = position_dodge(width = 0.8),draw_quantiles = c(0.5)) +
        ylab('Difference (cfs)') +
        #scale_fill_manual(values = c('Red','Blue','Burlywood3','Purple','green')) +
        scale_y_continuous(limits = c(-120,120), breaks = seq(-120,120,by = 10), expand = c(0,0)) +
        coord_cartesian(ylim=c(-20, 40)) +
        theme(plot.background = element_blank(),
              panel.background = element_rect(fill = NA, color = 'black'),
              panel.border = element_rect(fill = NA, color = 'black'),
              axis.title.x = element_blank(),
              axis.title.y = element_text(size = 20),
              axis.text.x = element_text(size = 20),
              axis.text.y = element_text(size = 20))
      eval(parse(text = paste0('Monthly_MAR_cov_loc_',j,'_plot = ViolinPlot_All')))
      
      #Violin plots grouped by month (single calibration)
      for (i in 1:5){
        eval(parse(text = paste0("cov_cal_",i,"_MAR_diff_loc_",j,"_melt = melt(cov_cal_",i,"_MAR_diff_loc_",j,", id.vars = names(date_info))")))
        eval(parse(text = paste0("cov_cal_",i,"_MAR_diff_loc_",j,"_melt$Month = factor(cov_cal_",i,"_MAR_diff_loc_",j,"_melt$Month, levels = format(seq(as.Date('1990-01-01'), as.Date('1990-12-01'), by = 'month'),'%b'))")))
        eval(parse(text = paste0("cov_cal_",i,"_MAR_diff_loc_",j,"_melt = cov_cal_",i,"_MAR_diff_loc_",j,"_melt[order(cov_cal_",i,"_MAR_diff_loc_",j,"_melt$Month),]")))
        eval(parse(text = paste0("data_melt = cov_cal_",i,"_MAR_diff_loc_",j,"_melt")))
        ViolinPlot = ggplot(data = data_melt, aes(x = factor(Month), y = value*0.000408734569, fill = Month)) +
          geom_hline(yintercept = 0) +
          geom_violin(position = position_dodge(width = 0.8),draw_quantiles = c(0.5)) +
          ylab('Difference (cfs)') +
          #scale_fill_manual(values = c('Red','Blue','Burlywood3','Purple','green')) +
          scale_y_continuous(limits = c(-120,120), breaks = seq(-120,120,by = 10), expand = c(0,0)) +
          coord_cartesian(ylim=c(-20, 40)) +
          theme(plot.background = element_blank(),
                panel.background = element_rect(fill = NA, color = 'black'),
                panel.border = element_rect(fill = NA, color = 'black'),
                axis.title.x = element_blank(),
                axis.title.y = element_text(size = 20),
                axis.text.x = element_text(size = 20),
                axis.text.y = element_text(size = 20),
                legend.position = 'none')
        eval(parse(text = paste0('Monthly_MAR_cov_cal_',i,'_loc_',j,'_plot = ViolinPlot')))
      }
    }
  }
}

###########################################################################################
###################                   ILR  PLOTS                      #####################
###########################################################################################
if (UniRandPar == TRUE){
  for (i in 1:5){
    eval(parse(text = paste0('mean_cfs = apply(uni_ILR_diff_loc_',i,'[,c(-1,-2,-3)],1,mean)*0.000408734569')))
    eval(parse(text = paste0('sd_cfs = apply(uni_ILR_diff_loc_',i,'[,c(-1,-2,-3)],1,sd)*0.000408734569')))
    eval(parse(text = paste0('min_cfs = apply(uni_ILR_diff_loc_',i,'[,c(-1,-2,-3)],1,min)*0.000408734569')))
    eval(parse(text = paste0('max_cfs = apply(uni_ILR_diff_loc_',i,'[,c(-1,-2,-3)],1,max)*0.000408734569')))
    eval(parse(text = paste0('summary = cbind(date_info, mean_cfs, sd_cfs, min_cfs, max_cfs)')))
    for(m in 1:length(crit_months)){
      temp_data = subset(summary,Month==crit_months[m])
      Plot = ggplot(data = temp_data, aes(x = Water_Year)) + 
        geom_ribbon(aes(ymin = min_cfs, ymax = max_cfs), alpha = 0.5, fill = 'darkgray') + 
        geom_ribbon(aes(ymin = (mean_cfs - 2*sd_cfs), ymax = (mean_cfs + 2*sd_cfs)), alpha = 0.75, fill = 'dodgerblue') + 
        geom_line(aes(y = mean_cfs),size = 1) +
        geom_hline(yintercept = 0, alpha = 0.5) +
        ylab('Difference (cfs)') +
        scale_color_manual(values = c('black')) +
        scale_x_continuous(limits = c(1991,2011), breaks = seq(1991,2011,by=2), expand = c(0,0)) +
        scale_y_continuous(limits = c(-30,120), breaks = seq(-30,120,by=10),expand = c(0,0)) +
        coord_cartesian(ylim=c(-20, 40)) +
        theme(plot.background = element_blank(),
              panel.background = element_rect(fill = NA, color = 'black'),
              panel.border = element_rect(fill = NA, color = 'black'),
              axis.title.x = element_blank(),
              axis.title.y = element_text(size = 20),
              axis.text.x = element_text(size = 20),
              axis.text.y = element_text(size = 20)
        )
      eval(parse(text = paste0(crit_months[m],'_ILR_uni_loc_',i,'_plot = Plot')))
    }
    #Violin plots grouped by month
    eval(parse(text = paste0('data_melt = melt(uni_ILR_diff_loc_',i,',id.vars = names(date_info))')))
    data_melt$Month = factor(data_melt$Month, levels = format(seq(as.Date('1990-01-01'), as.Date('1990-12-01'), by = 'month'),'%b'))
    data_melt = data_melt[order(data_melt$Month),]
    ViolinPlot = ggplot(data = subset(data_melt,Month==crit_months), aes(x = factor(Month), y = value*0.000408734569, group = Month, fill = Month)) + 
      geom_hline(yintercept = 0) +
      geom_violin(trim = TRUE,draw_quantiles = c(0.5)) +
      ylab('Difference (cfs)') +
      #scale_fill_manual(values = c('Red','Blue','Burlywood3','Purple','green')) +
      scale_y_continuous(limits = c(-120,120), breaks = seq(-120,120,by = 10), expand = c(0,0)) +
      coord_cartesian(ylim=c(-20, 40)) +
      theme(plot.background = element_blank(),
            panel.background = element_rect(fill = NA, color = 'black'),
            panel.border = element_rect(fill = NA, color = 'black'),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 20),
            axis.text.x = element_text(size = 20),
            axis.text.y = element_text(size = 20),
            legend.position = 'none')
    eval(parse(text = paste0('Monthly_ILR_uni_loc_',i,'_plot = ViolinPlot')))
  }
}
if(CovRandPar==TRUE){
  #Monthly Series
  for (i in 1:5){
    for (j in 1:5){
      eval(parse(text = paste0('cal_',i,'_loc_',j,'_ILR_diff_mean_cfs = apply(cov_cal_',i,'_ILR_diff_loc_',j,'[,c(-1,-2,-3)],1,mean)*0.000408734569')))
      eval(parse(text = paste0('cal_',i,'_loc_',j,'_ILR_diff_max_cfs = apply(cov_cal_',i,'_ILR_diff_loc_',j,'[,c(-1,-2,-3)],1,max)*0.000408734569')))
      eval(parse(text = paste0('cal_',i,'_loc_',j,'_ILR_diff_min_cfs = apply(cov_cal_',i,'_ILR_diff_loc_',j,'[,c(-1,-2,-3)],1,min)*0.000408734569')))
      eval(parse(text = paste0('cal_',i,'_loc_',j,'_ILR_diff = cbind(date_info, cal_',i,'_loc_',j,'_ILR_diff_mean_cfs, cal_',i,'_loc_',j,'_ILR_diff_max_cfs, cal_',i,'_loc_',j,'_ILR_diff_min_cfs)')))
      eval(parse(text = paste0("names(cal_",i,"_loc_",j,"_ILR_diff) = c(names(date_info),'ILR_diff_mean_cfs', 'ILR_diff_max_cfs', 'ILR_diff_min_cfs')")))
    }
  }
  for(m in 1:length(crit_months)){
    for (j in 1:5){
      eval(parse(text = paste0("loc_",j,"_ILR_diff_ensemble = cbind(date_info,
                               apply(cbind(cov_cal_1_ILR_diff_loc_",j,"[,c(-1,-2,-3)],
                               cov_cal_2_ILR_diff_loc_",j,"[,c(-1,-2,-3)],
                               cov_cal_3_ILR_diff_loc_",j,"[,c(-1,-2,-3)],
                               cov_cal_4_ILR_diff_loc_",j,"[,c(-1,-2,-3)],
                               cov_cal_5_ILR_diff_loc_",j,"[,c(-1,-2,-3)]),1,mean)*0.000408734569,
                               apply(cbind(cov_cal_1_ILR_diff_loc_",j,"[,c(-1,-2,-3)],
                               cov_cal_2_ILR_diff_loc_",j,"[,c(-1,-2,-3)],
                               cov_cal_3_ILR_diff_loc_",j,"[,c(-1,-2,-3)],
                               cov_cal_4_ILR_diff_loc_",j,"[,c(-1,-2,-3)],
                               cov_cal_5_ILR_diff_loc_",j,"[,c(-1,-2,-3)]),1,max)*0.000408734569,
                               apply(cbind(cov_cal_1_ILR_diff_loc_",j,"[,c(-1,-2,-3)],
                               cov_cal_2_ILR_diff_loc_",j,"[,c(-1,-2,-3)],
                               cov_cal_3_ILR_diff_loc_",j,"[,c(-1,-2,-3)],
                               cov_cal_4_ILR_diff_loc_",j,"[,c(-1,-2,-3)],
                               cov_cal_5_ILR_diff_loc_",j,"[,c(-1,-2,-3)]),1,min)*0.000408734569)")))
      eval(parse(text = paste0("names(loc_",j,"_ILR_diff_ensemble) = c(names(date_info), 'ILR_diff_mean_cfs','ILR_diff_max_cfs', 'ILR_diff_min_cfs')")))
      
      Plot = ggplot() + 
        geom_hline(yintercept = 0) +
        eval(parse(text = paste0("geom_ribbon(data = subset(cal_1_loc_",j,"_ILR_diff,Month==crit_months[m]),
                                 aes(x = Water_Year, ymin = ILR_diff_min_cfs, ymax = ILR_diff_max_cfs,fill = 'Cal_1'), alpha = 0.15)"))) +
        eval(parse(text = paste0("geom_ribbon(data = subset(cal_2_loc_",j,"_ILR_diff,Month==crit_months[m]),
                                 aes(x = Water_Year, ymin = ILR_diff_min_cfs, ymax = ILR_diff_max_cfs,fill = 'Cal_2'), alpha = 0.15)"))) +
        eval(parse(text = paste0("geom_ribbon(data = subset(cal_3_loc_",j,"_ILR_diff,Month==crit_months[m]),
                                 aes(x = Water_Year, ymin = ILR_diff_min_cfs, ymax = ILR_diff_max_cfs,fill = 'Cal_3'), alpha = 0.15)"))) +
        eval(parse(text = paste0("geom_ribbon(data = subset(cal_4_loc_",j,"_ILR_diff,Month==crit_months[m]),
                                 aes(x = Water_Year, ymin = ILR_diff_min_cfs, ymax = ILR_diff_max_cfs,fill = 'Cal_4'), alpha = 0.15)"))) +
        eval(parse(text = paste0("geom_ribbon(data = subset(cal_5_loc_",j,"_ILR_diff,Month==crit_months[m]),
                                 aes(x = Water_Year, ymin = ILR_diff_min_cfs, ymax = ILR_diff_max_cfs,fill = 'Cal_5'), alpha = 0.15)"))) +
        eval(parse(text = paste0("geom_ribbon(data = subset(loc_",j,"_ILR_diff_ensemble,Month==crit_months[m]),
                                 aes(x = Water_Year, ymin = ILR_diff_min_cfs, ymax = ILR_diff_max_cfs,fill = 'Ensemble'), alpha = 0)"))) +
        eval(parse(text = paste0("geom_line(data = subset(cal_1_loc_",j,"_ILR_diff,Month==crit_months[m]),
                                 aes(x = Water_Year, y = ILR_diff_mean_cfs, color = 'Cal_1'), size = 1)"))) +
        eval(parse(text = paste0("geom_line(data = subset(cal_2_loc_",j,"_ILR_diff,Month==crit_months[m]),
                                 aes(x = Water_Year, y = ILR_diff_mean_cfs, color = 'Cal_2'), size = 1)"))) +
        eval(parse(text = paste0("geom_line(data = subset(cal_3_loc_",j,"_ILR_diff,Month==crit_months[m]),
                                 aes(x = Water_Year, y = ILR_diff_mean_cfs, color = 'Cal_3'), size = 1)"))) +
        eval(parse(text = paste0("geom_line(data = subset(cal_4_loc_",j,"_ILR_diff,Month==crit_months[m]),
                                 aes(x = Water_Year, y = ILR_diff_mean_cfs, color = 'Cal_4'), size = 1)"))) +
        eval(parse(text = paste0("geom_line(data = subset(cal_5_loc_",j,"_ILR_diff,Month==crit_months[m]),
                                 aes(x = Water_Year, y = ILR_diff_mean_cfs, color = 'Cal_5'), size = 1)"))) +
        eval(parse(text = paste0("geom_line(data = subset(loc_",j,"_ILR_diff_ensemble,Month==crit_months[m]),
                                 aes(x = Water_Year, y = ILR_diff_mean_cfs, color = 'Ensemble'), size = 1)"))) +
        scale_color_manual("",values = c('red','blue','forestgreen','purple','cyan4','black')) +
        scale_fill_manual("",values = c('red','blue','forestgreen','purple','cyan4','black')) +
        ylab('Difference (cfs)') +
        scale_x_continuous(limits = c(1991,2011), breaks = seq(1991,2011,by=2), expand = c(0,0)) +
        scale_y_continuous(limits = c(-30,120), breaks = seq(-30,120,by=10),expand = c(0,0)) +
        coord_cartesian(ylim=c(-20, 40)) +
        theme(plot.background = element_blank(),
              panel.background = element_rect(fill = NA, color = 'black'),
              panel.border = element_rect(fill = NA, color = 'black'),
              axis.text.y  = element_text(size = 20),
              axis.title.y = element_text(size = 20),
              axis.title.x = element_blank(),
              legend.position = c(0.8,0.8),
              legend.background = element_rect(fill = NA, color = NA),
              legend.text = element_text(size = 20)
        )
      eval(parse(text = paste0(crit_months[m],'_cov_ILR_diff_loc_',j,'_plot = Plot')))
      
      #Violin plots grouped by month (all calibrations)
      eval(parse(text = paste0("cov_ILR_diff_loc_",j," = rbind(cbind(cov_cal_1_ILR_diff_loc_",j,"),
                               cbind(cov_cal_2_ILR_diff_loc_",j,"),
                               cbind(cov_cal_3_ILR_diff_loc_",j,"),
                               cbind(cov_cal_4_ILR_diff_loc_",j,"),
                               cbind(cov_cal_5_ILR_diff_loc_",j,"))")))
      eval(parse(text = paste0("cov_ILR_diff_loc_",j,"$Cal = rep(c('Cal_1', 'Cal_2', 'Cal_3', 'Cal_4', 'Cal_5'), each = 252)")))
      eval(parse(text = paste0("cov_ILR_diff_loc_",j,"_melt = melt(cov_ILR_diff_loc_",j,", id.vars = c(names(date_info), 'Cal'))")))
      eval(parse(text = paste0("cov_ILR_diff_loc_",j,"_melt$Month = factor(cov_ILR_diff_loc_",j,"_melt$Month, levels = format(seq(as.Date('1990-01-01'), as.Date('1990-12-01'), by = 'month'),'%b'))")))
      eval(parse(text = paste0("cov_ILR_diff_loc_",j,"_melt = cov_ILR_diff_loc_",j,"_melt[order(cov_ILR_diff_loc_",j,"_melt$Month),]")))
      eval(parse(text = paste0("data_melt = cov_ILR_diff_loc_",j,"_melt")))
      
      ViolinPlot_All = ggplot(data = subset(data_melt,Month==crit_months), aes(x = factor(Month), y = value*0.000408734569, fill = Cal)) +
        geom_hline(yintercept = 0) +
        geom_violin(position = position_dodge(width = 0.8),draw_quantiles = c(0.5)) +
        ylab('Difference (cfs)') +
        #scale_fill_manual(values = c('Red','Blue','Burlywood3','Purple','green')) +
        scale_y_continuous(limits = c(-120,120), breaks = seq(-120,120,by = 10), expand = c(0,0)) +
        coord_cartesian(ylim=c(-20, 40)) +
        theme(plot.background = element_blank(),
              panel.background = element_rect(fill = NA, color = 'black'),
              panel.border = element_rect(fill = NA, color = 'black'),
              axis.title.x = element_blank(),
              axis.title.y = element_text(size = 20),
              axis.text.x = element_text(size = 20),
              axis.text.y = element_text(size = 20))
      eval(parse(text = paste0('Monthly_ILR_cov_loc_',j,'_plot = ViolinPlot_All')))
      
      #Violin plots grouped by month (single calibration)
      for (i in 1:5){
        eval(parse(text = paste0("cov_cal_",i,"_ILR_diff_loc_",j,"_melt = melt(cov_cal_",i,"_ILR_diff_loc_",j,", id.vars = names(date_info))")))
        eval(parse(text = paste0("cov_cal_",i,"_ILR_diff_loc_",j,"_melt$Month = factor(cov_cal_",i,"_ILR_diff_loc_",j,"_melt$Month, levels = format(seq(as.Date('1990-01-01'), as.Date('1990-12-01'), by = 'month'),'%b'))")))
        eval(parse(text = paste0("cov_cal_",i,"_ILR_diff_loc_",j,"_melt = cov_cal_",i,"_ILR_diff_loc_",j,"_melt[order(cov_cal_",i,"_ILR_diff_loc_",j,"_melt$Month),]")))
        eval(parse(text = paste0("data_melt = cov_cal_",i,"_ILR_diff_loc_",j,"_melt")))
        ViolinPlot = ggplot(data = data_melt, aes(x = factor(Month), y = value*0.000408734569, fill = Month)) +
          geom_hline(yintercept = 0) +
          geom_violin(position = position_dodge(width = 0.8),draw_quantiles = c(0.5)) +
          ylab('Difference (cfs)') +
          #scale_fill_manual(values = c('Red','Blue','Burlywood3','Purple','green')) +
          scale_y_continuous(limits = c(-120,120), breaks = seq(-120,120,by = 10), expand = c(0,0)) +
          coord_cartesian(ylim=c(-20, 40)) +
          theme(plot.background = element_blank(),
                panel.background = element_rect(fill = NA, color = 'black'),
                panel.border = element_rect(fill = NA, color = 'black'),
                axis.title.x = element_blank(),
                axis.title.y = element_text(size = 20),
                axis.text.x = element_text(size = 20),
                axis.text.y = element_text(size = 20),
                legend.position = 'none')
        eval(parse(text = paste0('Monthly_ILR_cov_cal_',i,'_loc_',j,'_plot = ViolinPlot')))
      }
    }
  }
}
###########################################################################################
###################               MAR_ILR  PLOTS                      #####################
###########################################################################################
if (UniRandPar == TRUE){
  for (i in 1:5){
    eval(parse(text = paste0('mean_cfs = apply(uni_MAR_ILR_diff_loc_',i,'[,c(-1,-2,-3)],1,mean)*0.000408734569')))
    eval(parse(text = paste0('sd_cfs = apply(uni_MAR_ILR_diff_loc_',i,'[,c(-1,-2,-3)],1,sd)*0.000408734569')))
    eval(parse(text = paste0('min_cfs = apply(uni_MAR_ILR_diff_loc_',i,'[,c(-1,-2,-3)],1,min)*0.000408734569')))
    eval(parse(text = paste0('max_cfs = apply(uni_MAR_ILR_diff_loc_',i,'[,c(-1,-2,-3)],1,max)*0.000408734569')))
    eval(parse(text = paste0('summary = cbind(date_info, mean_cfs, sd_cfs, min_cfs, max_cfs)')))
    for(m in 1:length(crit_months)){
      temp_data = subset(summary,Month==crit_months[m])
      Plot = ggplot(data = temp_data, aes(x = Water_Year)) + 
        geom_ribbon(aes(ymin = min_cfs, ymax = max_cfs), alpha = 0.5, fill = 'darkgray') + 
        geom_ribbon(aes(ymin = (mean_cfs - 2*sd_cfs), ymax = (mean_cfs + 2*sd_cfs)), alpha = 0.75, fill = 'dodgerblue') + 
        geom_line(aes(y = mean_cfs),size = 1) +
        geom_hline(yintercept = 0, alpha = 0.5) +
        ylab('Difference (cfs)') +
        scale_color_manual(values = c('black')) +
        scale_x_continuous(limits = c(1991,2011), breaks = seq(1991,2011,by=2), expand = c(0,0)) +
        scale_y_continuous(limits = c(-30,120), breaks = seq(-30,120,by=10),expand = c(0,0)) +
        coord_cartesian(ylim=c(-20, 40)) +
        theme(plot.background = element_blank(),
              panel.background = element_rect(fill = NA, color = 'black'),
              panel.border = element_rect(fill = NA, color = 'black'),
              axis.title.x = element_blank(),
              axis.title.y = element_text(size = 20),
              axis.text.x = element_text(size = 20),
              axis.text.y = element_text(size = 20)
        )
      eval(parse(text = paste0(crit_months[m],'_MAR_ILR_uni_loc_',i,'_plot = Plot')))
    }
    #Violin plots grouped by month
    eval(parse(text = paste0('data_melt = melt(uni_MAR_ILR_diff_loc_',i,',id.vars = names(date_info))')))
    data_melt$Month = factor(data_melt$Month, levels = format(seq(as.Date('1990-01-01'), as.Date('1990-12-01'), by = 'month'),'%b'))
    data_melt = data_melt[order(data_melt$Month),]
    ViolinPlot = ggplot(data = subset(data_melt,Month==crit_months), aes(x = factor(Month), y = value*0.000408734569, group = Month, fill = Month)) + 
      geom_hline(yintercept = 0) +
      geom_violin(trim = TRUE,draw_quantiles = c(0.5)) +
      ylab('Difference (cfs)') +
      #scale_fill_manual(values = c('Red','Blue','Burlywood3','Purple','green')) +
      scale_y_continuous(limits = c(-120,120), breaks = seq(-120,120,by = 10), expand = c(0,0)) +
      coord_cartesian(ylim=c(-20, 40)) +
      theme(plot.background = element_blank(),
            panel.background = element_rect(fill = NA, color = 'black'),
            panel.border = element_rect(fill = NA, color = 'black'),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 20),
            axis.text.x = element_text(size = 20),
            axis.text.y = element_text(size = 20),
            legend.position = 'none')
    eval(parse(text = paste0('Monthly_MAR_ILR_uni_loc_',i,'_plot = ViolinPlot')))
  }
}
if(CovRandPar==TRUE){
  #Monthly Series
  for (i in 1:5){
    for (j in 1:5){
      eval(parse(text = paste0('cal_',i,'_loc_',j,'_MAR_ILR_diff_mean_cfs = apply(cov_cal_',i,'_MAR_ILR_diff_loc_',j,'[,c(-1,-2,-3)],1,mean)*0.000408734569')))
      eval(parse(text = paste0('cal_',i,'_loc_',j,'_MAR_ILR_diff_max_cfs = apply(cov_cal_',i,'_MAR_ILR_diff_loc_',j,'[,c(-1,-2,-3)],1,max)*0.000408734569')))
      eval(parse(text = paste0('cal_',i,'_loc_',j,'_MAR_ILR_diff_min_cfs = apply(cov_cal_',i,'_MAR_ILR_diff_loc_',j,'[,c(-1,-2,-3)],1,min)*0.000408734569')))
      eval(parse(text = paste0('cal_',i,'_loc_',j,'_MAR_ILR_diff = cbind(date_info, cal_',i,'_loc_',j,'_MAR_ILR_diff_mean_cfs, cal_',i,'_loc_',j,'_MAR_ILR_diff_max_cfs, cal_',i,'_loc_',j,'_MAR_ILR_diff_min_cfs)')))
      eval(parse(text = paste0("names(cal_",i,"_loc_",j,"_MAR_ILR_diff) = c(names(date_info),'MAR_ILR_diff_mean_cfs', 'MAR_ILR_diff_max_cfs', 'MAR_ILR_diff_min_cfs')")))
    }
  }
  for(m in 1:length(crit_months)){
    for (j in 1:5){
      eval(parse(text = paste0("loc_",j,"_MAR_ILR_diff_ensemble = cbind(date_info,
                               apply(cbind(cov_cal_1_MAR_ILR_diff_loc_",j,"[,c(-1,-2,-3)],
                               cov_cal_2_MAR_ILR_diff_loc_",j,"[,c(-1,-2,-3)],
                               cov_cal_3_MAR_ILR_diff_loc_",j,"[,c(-1,-2,-3)],
                               cov_cal_4_MAR_ILR_diff_loc_",j,"[,c(-1,-2,-3)],
                               cov_cal_5_MAR_ILR_diff_loc_",j,"[,c(-1,-2,-3)]),1,mean)*0.000408734569,
                               apply(cbind(cov_cal_1_MAR_ILR_diff_loc_",j,"[,c(-1,-2,-3)],
                               cov_cal_2_MAR_ILR_diff_loc_",j,"[,c(-1,-2,-3)],
                               cov_cal_3_MAR_ILR_diff_loc_",j,"[,c(-1,-2,-3)],
                               cov_cal_4_MAR_ILR_diff_loc_",j,"[,c(-1,-2,-3)],
                               cov_cal_5_MAR_ILR_diff_loc_",j,"[,c(-1,-2,-3)]),1,max)*0.000408734569,
                               apply(cbind(cov_cal_1_MAR_ILR_diff_loc_",j,"[,c(-1,-2,-3)],
                               cov_cal_2_MAR_ILR_diff_loc_",j,"[,c(-1,-2,-3)],
                               cov_cal_3_MAR_ILR_diff_loc_",j,"[,c(-1,-2,-3)],
                               cov_cal_4_MAR_ILR_diff_loc_",j,"[,c(-1,-2,-3)],
                               cov_cal_5_MAR_ILR_diff_loc_",j,"[,c(-1,-2,-3)]),1,min)*0.000408734569)")))
      eval(parse(text = paste0("names(loc_",j,"_MAR_ILR_diff_ensemble) = c(names(date_info), 'MAR_ILR_diff_mean_cfs','MAR_ILR_diff_max_cfs', 'MAR_ILR_diff_min_cfs')")))
      
      Plot = ggplot() + 
        geom_hline(yintercept = 0) +
        eval(parse(text = paste0("geom_ribbon(data = subset(cal_1_loc_",j,"_MAR_ILR_diff,Month==crit_months[m]),
                                 aes(x = Water_Year, ymin = MAR_ILR_diff_min_cfs, ymax = MAR_ILR_diff_max_cfs,fill = 'Cal_1'), alpha = 0.15)"))) +
        eval(parse(text = paste0("geom_ribbon(data = subset(cal_2_loc_",j,"_MAR_ILR_diff,Month==crit_months[m]),
                                 aes(x = Water_Year, ymin = MAR_ILR_diff_min_cfs, ymax = MAR_ILR_diff_max_cfs,fill = 'Cal_2'), alpha = 0.15)"))) +
        eval(parse(text = paste0("geom_ribbon(data = subset(cal_3_loc_",j,"_MAR_ILR_diff,Month==crit_months[m]),
                                 aes(x = Water_Year, ymin = MAR_ILR_diff_min_cfs, ymax = MAR_ILR_diff_max_cfs,fill = 'Cal_3'), alpha = 0.15)"))) +
        eval(parse(text = paste0("geom_ribbon(data = subset(cal_4_loc_",j,"_MAR_ILR_diff,Month==crit_months[m]),
                                 aes(x = Water_Year, ymin = MAR_ILR_diff_min_cfs, ymax = MAR_ILR_diff_max_cfs,fill = 'Cal_4'), alpha = 0.15)"))) +
        eval(parse(text = paste0("geom_ribbon(data = subset(cal_5_loc_",j,"_MAR_ILR_diff,Month==crit_months[m]),
                                 aes(x = Water_Year, ymin = MAR_ILR_diff_min_cfs, ymax = MAR_ILR_diff_max_cfs,fill = 'Cal_5'), alpha = 0.15)"))) +
        eval(parse(text = paste0("geom_ribbon(data = subset(loc_",j,"_MAR_ILR_diff_ensemble,Month==crit_months[m]),
                                 aes(x = Water_Year, ymin = MAR_ILR_diff_min_cfs, ymax = MAR_ILR_diff_max_cfs,fill = 'Ensemble'), alpha = 0)"))) +
        eval(parse(text = paste0("geom_line(data = subset(cal_1_loc_",j,"_MAR_ILR_diff,Month==crit_months[m]),
                                 aes(x = Water_Year, y = MAR_ILR_diff_mean_cfs, color = 'Cal_1'), size = 1)"))) +
        eval(parse(text = paste0("geom_line(data = subset(cal_2_loc_",j,"_MAR_ILR_diff,Month==crit_months[m]),
                                 aes(x = Water_Year, y = MAR_ILR_diff_mean_cfs, color = 'Cal_2'), size = 1)"))) +
        eval(parse(text = paste0("geom_line(data = subset(cal_3_loc_",j,"_MAR_ILR_diff,Month==crit_months[m]),
                                 aes(x = Water_Year, y = MAR_ILR_diff_mean_cfs, color = 'Cal_3'), size = 1)"))) +
        eval(parse(text = paste0("geom_line(data = subset(cal_4_loc_",j,"_MAR_ILR_diff,Month==crit_months[m]),
                                 aes(x = Water_Year, y = MAR_ILR_diff_mean_cfs, color = 'Cal_4'), size = 1)"))) +
        eval(parse(text = paste0("geom_line(data = subset(cal_5_loc_",j,"_MAR_ILR_diff,Month==crit_months[m]),
                                 aes(x = Water_Year, y = MAR_ILR_diff_mean_cfs, color = 'Cal_5'), size = 1)"))) +
        eval(parse(text = paste0("geom_line(data = subset(loc_",j,"_MAR_ILR_diff_ensemble,Month==crit_months[m]),
                                 aes(x = Water_Year, y = MAR_ILR_diff_mean_cfs, color = 'Ensemble'), size = 1)"))) +
        scale_color_manual("",values = c('red','blue','forestgreen','purple','cyan4','black')) +
        scale_fill_manual("",values = c('red','blue','forestgreen','purple','cyan4','black')) +
        ylab('Difference (cfs)') +
        scale_x_continuous(limits = c(1991,2011), breaks = seq(1991,2011,by=2), expand = c(0,0)) +
        scale_y_continuous(limits = c(-30,120), breaks = seq(-30,120,by=10),expand = c(0,0)) +
        coord_cartesian(ylim=c(-20, 40)) +
        theme(plot.background = element_blank(),
              panel.background = element_rect(fill = NA, color = 'black'),
              panel.border = element_rect(fill = NA, color = 'black'),
              axis.text.y  = element_text(size = 20),
              axis.title.y = element_text(size = 20),
              axis.title.x = element_blank(),
              legend.position = c(0.8,0.8),
              legend.background = element_rect(fill = NA, color = NA),
              legend.text = element_text(size = 20)
        )
      eval(parse(text = paste0(crit_months[m],'_cov_MAR_ILR_diff_loc_',j,'_plot = Plot')))
      
      #Violin plots grouped by month (all calibrations)
      eval(parse(text = paste0("cov_MAR_ILR_diff_loc_",j," = rbind(cbind(cov_cal_1_MAR_ILR_diff_loc_",j,"),
                               cbind(cov_cal_2_MAR_ILR_diff_loc_",j,"),
                               cbind(cov_cal_3_MAR_ILR_diff_loc_",j,"),
                               cbind(cov_cal_4_MAR_ILR_diff_loc_",j,"),
                               cbind(cov_cal_5_MAR_ILR_diff_loc_",j,"))")))
      eval(parse(text = paste0("cov_MAR_ILR_diff_loc_",j,"$Cal = rep(c('Cal_1', 'Cal_2', 'Cal_3', 'Cal_4', 'Cal_5'), each = 252)")))
      eval(parse(text = paste0("cov_MAR_ILR_diff_loc_",j,"_melt = melt(cov_MAR_ILR_diff_loc_",j,", id.vars = c(names(date_info), 'Cal'))")))
      eval(parse(text = paste0("cov_MAR_ILR_diff_loc_",j,"_melt$Month = factor(cov_MAR_ILR_diff_loc_",j,"_melt$Month, levels = format(seq(as.Date('1990-01-01'), as.Date('1990-12-01'), by = 'month'),'%b'))")))
      eval(parse(text = paste0("cov_MAR_ILR_diff_loc_",j,"_melt = cov_MAR_ILR_diff_loc_",j,"_melt[order(cov_MAR_ILR_diff_loc_",j,"_melt$Month),]")))
      eval(parse(text = paste0("data_melt = cov_MAR_ILR_diff_loc_",j,"_melt")))
      
      ViolinPlot_All = ggplot(data = subset(data_melt,Month==crit_months), aes(x = factor(Month), y = value*0.000408734569, fill = Cal)) +
        geom_hline(yintercept = 0) +
        geom_violin(position = position_dodge(width = 0.8),draw_quantiles = c(0.5)) +
        ylab('Difference (cfs)') +
        #scale_fill_manual(values = c('Red','Blue','Burlywood3','Purple','green')) +
        scale_y_continuous(limits = c(-120,120), breaks = seq(-120,120,by = 10), expand = c(0,0)) +
        coord_cartesian(ylim=c(-20, 40)) +
        theme(plot.background = element_blank(),
              panel.background = element_rect(fill = NA, color = 'black'),
              panel.border = element_rect(fill = NA, color = 'black'),
              axis.title.x = element_blank(),
              axis.title.y = element_text(size = 20),
              axis.text.x = element_text(size = 20),
              axis.text.y = element_text(size = 20))
      eval(parse(text = paste0('Monthly_MAR_ILR_cov_loc_',j,'_plot = ViolinPlot_All')))
      
      #Violin plots grouped by month (single calibration)
      for (i in 1:5){
        eval(parse(text = paste0("cov_cal_",i,"_MAR_ILR_diff_loc_",j,"_melt = melt(cov_cal_",i,"_MAR_ILR_diff_loc_",j,", id.vars = names(date_info))")))
        eval(parse(text = paste0("cov_cal_",i,"_MAR_ILR_diff_loc_",j,"_melt$Month = factor(cov_cal_",i,"_MAR_ILR_diff_loc_",j,"_melt$Month, levels = format(seq(as.Date('1990-01-01'), as.Date('1990-12-01'), by = 'month'),'%b'))")))
        eval(parse(text = paste0("cov_cal_",i,"_MAR_ILR_diff_loc_",j,"_melt = cov_cal_",i,"_MAR_ILR_diff_loc_",j,"_melt[order(cov_cal_",i,"_MAR_ILR_diff_loc_",j,"_melt$Month),]")))
        eval(parse(text = paste0("data_melt = cov_cal_",i,"_MAR_ILR_diff_loc_",j,"_melt")))
        ViolinPlot = ggplot(data = data_melt, aes(x = factor(Month), y = value*0.000408734569, fill = Month)) +
          geom_hline(yintercept = 0) +
          geom_violin(position = position_dodge(width = 0.8),draw_quantiles = c(0.5)) +
          ylab('Difference (cfs)') +
          #scale_fill_manual(values = c('Red','Blue','Burlywood3','Purple','green')) +
          scale_y_continuous(limits = c(-120,120), breaks = seq(-120,120,by = 10), expand = c(0,0)) +
          coord_cartesian(ylim=c(-20, 40)) +
          theme(plot.background = element_blank(),
                panel.background = element_rect(fill = NA, color = 'black'),
                panel.border = element_rect(fill = NA, color = 'black'),
                axis.title.x = element_blank(),
                axis.title.y = element_text(size = 20),
                axis.text.x = element_text(size = 20),
                axis.text.y = element_text(size = 20),
                legend.position = 'none')
        eval(parse(text = paste0('Monthly_MAR_ILR_cov_cal_',i,'_loc_',j,'_plot = ViolinPlot')))
      }
    }
  }
}
###########################################################################################
##################             OCTOBER UNIRANDPAR PLOTS                ####################
###########################################################################################

#pdf(paste0('UniRandPar_Oct_plots.pdf'), width = 13, height = 12)
png(paste0('UniRandPar_Oct_plots.png'), width = 8, height = 6, res = 600, units = 'in')
grid.newpage()
pushViewport(viewport(layout = grid.layout(4,3)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(Oct_uni_Streamflow_loc_1_plot + 
        theme(axis.text.x = element_blank(),
              axis.title.y = element_text(size = 18),
              plot.margin = margin(t =10, b= 10, l=15, r = 10)),
      vp = vplayout(1,1))
print(Oct_MAR_uni_loc_1_plot +
        theme(axis.text.x = element_blank(),
              axis.title.y = element_text(size = 18),
              plot.margin = margin(t =10, b= 10, l=15, r = 10)),
      vp = vplayout(2,1))
print(Oct_ILR_uni_loc_1_plot +
        theme(axis.text.x = element_blank(),
              axis.title.y = element_text(size = 18),
              plot.margin = margin(t =10, b= 10, l=15, r = 10)),
      vp = vplayout(3,1))
print(Oct_MAR_ILR_uni_loc_1_plot +
        theme(axis.text.x = element_text(size = 18,
                                         angle = 45,
                                         hjust = 1,
                                         vjust = 0.5),
              axis.title.y = element_text(size = 18),
              plot.margin = margin(t =10, b= 10, l=15, r = 10)),
      vp = vplayout(4,1))
print(Oct_uni_Streamflow_loc_2_plot +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.title.y = element_blank(),
              plot.margin = margin(t =10, b= 10, l=15, r = 10)),
      vp = vplayout(1,2))
print(Oct_MAR_uni_loc_2_plot +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.title.y = element_blank(),
              plot.margin = margin(t =10, b= 10, l=15, r = 10)),
      vp = vplayout(2,2))
print(Oct_ILR_uni_loc_2_plot +
        theme(axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.text.x = element_blank(),
              plot.margin = margin(t =10, b= 10, l=15, r = 10)),
      vp = vplayout(3,2))
print(Oct_MAR_ILR_uni_loc_2_plot +
        theme(axis.text.x = element_text(size = 18,
                                         angle = 45,
                                         hjust = 1,
                                         vjust = 0.5),
              axis.text.y = element_blank(),
              axis.title.y = element_blank(),
              plot.margin = margin(t =10, b= 10, l=15, r = 10)),
      vp = vplayout(4,2))
print(Oct_uni_Streamflow_loc_3_plot +
        theme(axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.text.x = element_blank(),
              plot.margin = margin(t =10, b= 10, l=15, r = 10)),
      vp = vplayout(1,3))
print(Oct_MAR_uni_loc_3_plot +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.title.y = element_blank(),
              plot.margin = margin(t =10, b= 10, l=15, r = 10)),
      vp = vplayout(2,3))
print(Oct_ILR_uni_loc_3_plot +
        theme(axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.text.x = element_blank(),
              plot.margin = margin(t =10, b= 10, l=15, r = 10)),
      vp = vplayout(3,3))
print(Oct_MAR_ILR_uni_loc_3_plot +
        theme(axis.text.x = element_text(size = 18,
                                         angle = 45,
                                         hjust = 1,
                                         vjust = 0.5),
              axis.text.y = element_blank(),
              axis.title.y = element_blank(),
              plot.margin = margin(t =10, b= 10, l=15, r = 10)),
      vp = vplayout(4,3))
# print(Oct_uni_Streamflow_loc_4_plot +
#         theme(axis.title.y = element_blank(),
#               axis.text.y = element_blank(),
#               axis.text.x = element_blank()),
#       vp = vplayout(1,4))
# print(Oct_MAR_uni_loc_4_plot + 
#         theme(axis.title.y = element_blank(),
#               axis.text.y = element_blank(),
#               axis.text.x = element_blank()),
#       vp = vplayout(2,4))
# print(Oct_ILR_uni_loc_4_plot + 
#         theme(axis.text.x = element_blank(),
#               axis.text.y = element_blank(),
#               axis.title.y = element_blank()),
#       vp = vplayout(3,4))
# print(Oct_MAR_ILR_uni_loc_4_plot + 
#         theme(axis.text.x = element_text(size = 20,
#                                          angle = 45,
#                                          hjust = 1,
#                                          vjust = 0.5),
#               axis.text.y = element_blank(),
#               axis.title.y = element_blank()),
#       vp = vplayout(4,4))
graphics.off()

###########################################################################################
##################             OCTOBER COVRANDPAR PLOTS                ####################
###########################################################################################

#pdf(paste0('CovRandPar_Oct_plots.pdf'), width = 17, height = 12)
png(paste0('CovRandPar_Oct_plots.png'), width = 8, height = 6, res = 600, units = 'in')
grid.newpage()
pushViewport(viewport(layout = grid.layout(4,3)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(Oct_cov_Streamflow_loc_1_plot + 
        theme(axis.text.x = element_blank(),
              axis.title.y = element_text(size = 18),
              plot.margin = margin(t =10, b= 10, l=15, r = 10),
              legend.position = 'none'),
      vp = vplayout(1,1))
print(Oct_cov_MAR_diff_loc_1_plot +
        theme(axis.text.x = element_blank(),
              axis.title.y = element_text(size = 18),
              plot.margin = margin(t =10, b= 10, l=15, r = 10),
              legend.position = 'none'),
      vp = vplayout(2,1))
print(Oct_cov_ILR_diff_loc_1_plot +
        theme(axis.text.x = element_blank(),
              axis.title.y = element_text(size = 18),
              plot.margin = margin(t =10, b= 10, l=15, r = 10),
              legend.position = 'none'),
      vp = vplayout(3,1))
print(Oct_cov_MAR_ILR_diff_loc_1_plot +
        theme(axis.text.x = element_text(size = 18,
                                         angle = 45,
                                         hjust = 1,
                                         vjust = 0.5),
              axis.title.y = element_text(size = 18),
              plot.margin = margin(t =10, b= 10, l=15, r = 10),
              legend.position = 'none'),
      vp = vplayout(4,1))
print(Oct_cov_Streamflow_loc_2_plot +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.title.y = element_blank(),
              plot.margin = margin(t =10, b= 10, l=15, r = 10),
              legend.position = c(0.5,0.9),
              legend.direction = 'horizontal',
              legend.text = element_text(size = 20),
              legend.background = element_rect(fill=NA,color = NA),
              legend.title = element_blank(),
              legend.spacing.x = unit(0.2, 'cm'),
              legend.key.height = unit(0.4, 'cm'),
              legend.key.width = unit(0.4, 'cm')),
      vp = vplayout(1,2))
print(Oct_cov_MAR_diff_loc_2_plot +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.title.y = element_blank(),
              plot.margin = margin(t =10, b= 10, l=15, r = 10),
              legend.position = 'none'),
      vp = vplayout(2,2))
print(Oct_cov_ILR_diff_loc_2_plot +
        theme(axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.text.x = element_blank(),
              plot.margin = margin(t =10, b= 10, l=15, r = 10),
              legend.position = 'none'),
      vp = vplayout(3,2))
print(Oct_cov_MAR_ILR_diff_loc_2_plot +
        theme(axis.text.x = element_text(size = 18,
                                         angle = 45,
                                         hjust = 1,
                                         vjust = 0.5),
              axis.text.y = element_blank(),
              axis.title.y = element_blank(),
              plot.margin = margin(t =10, b= 10, l=15, r = 10),
              legend.position = 'none'),
      vp = vplayout(4,2))
print(Oct_cov_Streamflow_loc_3_plot +
        theme(axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.text.x = element_blank(),
              plot.margin = margin(t =10, b= 10, l=15, r = 10),
              legend.position = 'none'),
      vp = vplayout(1,3))
print(Oct_cov_MAR_diff_loc_3_plot +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.title.y = element_blank(),
              plot.margin = margin(t =10, b= 10, l=15, r = 10),
              legend.position = 'none'),
      vp = vplayout(2,3))
print(Oct_cov_ILR_diff_loc_3_plot +
        theme(axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.text.x = element_blank(),
              plot.margin = margin(t =10, b= 10, l=15, r = 10),
              legend.position = 'none'),
      vp = vplayout(3,3))
print(Oct_cov_MAR_ILR_diff_loc_3_plot +
        theme(axis.text.x = element_text(size = 18,
                                         angle = 45,
                                         hjust = 1,
                                         vjust = 0.5),
              axis.text.y = element_blank(),
              axis.title.y = element_blank(),
              plot.margin = margin(t =10, b= 10, l=15, r = 10),
              legend.position = 'none'),
      vp = vplayout(4,3))
graphics.off()

###########################################################################################
##################          MONTHLY UNIRANDPAR VIOLIN PLOTS            ####################
###########################################################################################

#pdf(paste0('UniRandPar_Monthly_Violin_Plots.pdf'), width = 13, height = 13)
png(paste0('UniRandPar_Monthly_Violin_Plots.png'), width = 8, height = 6, res = 600, units = 'in')
grid.newpage()
pushViewport(viewport(layout = grid.layout(4,3)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(Monthly_uni_Streamflow_violin_loc_1_plot + 
        theme(axis.text.x = element_blank(),
              plot.margin = margin(t =10, b= 10, l=15, r = 10),
              legend.position = 'none'),
      vp = vplayout(1,1))
print(Monthly_MAR_uni_loc_1_plot + 
        theme(axis.text.x = element_blank(),
              plot.margin = margin(t =10, b= 10, l=15, r = 10)),
      vp = vplayout(2,1))
print(Monthly_ILR_uni_loc_1_plot +
        theme(axis.text.x = element_blank(),
              plot.margin = margin(t =10, b= 10, l=15, r = 10)),
      vp = vplayout(3,1))
print(Monthly_MAR_ILR_uni_loc_1_plot +
        theme(axis.text.x = element_text(size = 20,
                                         angle = 45,
                                         hjust = 1,
                                         vjust = 0.5),
              plot.margin = margin(t =10, b= 10, l=15, r = 10)),
      vp = vplayout(4,1))
print(Monthly_uni_Streamflow_violin_loc_2_plot +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.title.y = element_blank(),
              plot.margin = margin(t =10, b= 10, l=15, r = 10),
              legend.position = c(0.5,0.93),
              legend.direction = 'horizontal',
              legend.text = element_text(size = 20),
              legend.background = element_rect(fill=NA,color = NA),
              legend.title = element_blank(),
              legend.spacing.x = unit(0.2, 'cm')),
      vp = vplayout(1,2))
print(Monthly_MAR_uni_loc_2_plot +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.title.y = element_blank(),
              plot.margin = margin(t =10, b= 10, l=15, r = 10)),
      vp = vplayout(2,2))
print(Monthly_ILR_uni_loc_2_plot +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.title.y = element_blank(),
              plot.margin = margin(t =10, b= 10, l=15, r = 10)),
      vp = vplayout(3,2))
print(Monthly_MAR_ILR_uni_loc_2_plot +
        theme(axis.text.x = element_text(size = 20,
                                         angle = 45,
                                         hjust = 1,
                                         vjust = 0.5),
              axis.text.y = element_blank(),
              axis.title.y = element_blank(),
              plot.margin = margin(t =10, b= 10, l=15, r = 10)),
      vp = vplayout(4,2))
print(Monthly_uni_Streamflow_violin_loc_3_plot +
        theme(axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.text.x = element_blank(),
              plot.margin = margin(t =10, b= 10, l=15, r = 10),
              legend.position = 'none'),
      vp = vplayout(1,3))
print(Monthly_MAR_uni_loc_3_plot +
        theme(axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.text.x = element_blank(),
              plot.margin = margin(t =10, b= 10, l=15, r = 10)),
      vp = vplayout(2,3))
print(Monthly_ILR_uni_loc_3_plot +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.title.y = element_blank(),
              plot.margin = margin(t =10, b= 10, l=15, r = 10)),
      vp = vplayout(3,3))
print(Monthly_MAR_ILR_uni_loc_3_plot +
        theme(axis.text.x = element_text(size = 20,
                                         angle = 45,
                                         hjust = 1,
                                         vjust = 0.5),
              axis.text.y = element_blank(),
              axis.title.y = element_blank(),
              plot.margin = margin(t =10, b= 10, l=15, r = 10)),
      vp = vplayout(4,3))
graphics.off()

###########################################################################################
##################          MONTHLY COVRANDPAR VIOLIN PLOTS            ####################
###########################################################################################

#pdf(paste0('CovRandPar_Monthly_Violin_Plots.pdf'), width = 22, height = 13)
png(paste0('CovRandPar_Monthly_Violin_Plots.png'), width = 8, height = 6, res = 600, units = 'in')
grid.newpage()
pushViewport(viewport(layout = grid.layout(4,3)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print( Monthly_Streamflow_cov_loc_1_plot + 
        theme(axis.text.x = element_blank(),
              plot.margin = margin(t =10, b= 10, l=15, r = 10),
              legend.position = 'none'),
      vp = vplayout(1,1))
print(Monthly_MAR_cov_loc_1_plot + 
        theme(axis.text.x = element_blank(),
              plot.margin = margin(t =10, b= 10, l=15, r = 10),
              legend.position = 'none'),
      vp = vplayout(2,1))
print(Monthly_ILR_cov_loc_1_plot +
        theme(axis.text.x = element_blank(),
              plot.margin = margin(t =10, b= 10, l=15, r = 10),
              legend.position = 'none'),
      vp = vplayout(3,1))
print(Monthly_MAR_ILR_cov_loc_1_plot +
        theme(axis.text.x = element_text(size = 20,
                                         angle = 45,
                                         hjust = 1,
                                         vjust = 0.5),
              plot.margin = margin(t =10, b= 10, l=15, r = 10),
              legend.position = 'none'),
      vp = vplayout(4,1))
print(Monthly_Streamflow_cov_loc_2_plot +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.title.y = element_blank(),
              plot.margin = margin(t =10, b= 10, l=15, r = 10),
              legend.position = c(0.5,0.93),
              legend.direction = 'horizontal',
              legend.text = element_text(size = 20),
              legend.background = element_rect(fill=NA,color = NA),
              legend.title = element_blank(),
              legend.spacing.x = unit(0.2, 'cm')),
      vp = vplayout(1,2))
print(Monthly_MAR_cov_loc_2_plot +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.title.y = element_blank(),
              plot.margin = margin(t =10, b= 10, l=15, r = 10),
              legend.position = 'none'),
      vp = vplayout(2,2))
print(Monthly_ILR_cov_loc_2_plot +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.title.y = element_blank(),
              plot.margin = margin(t =10, b= 10, l=15, r = 10),
              legend.position = 'none'),
      vp = vplayout(3,2))
print(Monthly_MAR_ILR_cov_loc_2_plot +
        theme(axis.text.x = element_text(size = 20,
                                         angle = 45,
                                         hjust = 1,
                                         vjust = 0.5),
              axis.text.y = element_blank(),
              axis.title.y = element_blank(),
              plot.margin = margin(t =10, b= 10, l=15, r = 10),
              legend.position = 'none'),
      vp = vplayout(4,2))
print(Monthly_Streamflow_cov_loc_3_plot +
        theme(axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.text.x = element_blank(),
              plot.margin = margin(t =10, b= 10, l=15, r = 10),
              legend.position = 'none'),
      vp = vplayout(1,3))
print(Monthly_MAR_cov_loc_3_plot +
        theme(axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.text.x = element_blank(),
              plot.margin = margin(t =10, b= 10, l=15, r = 10),
              legend.position = 'none'),
      vp = vplayout(2,3))
print(Monthly_ILR_cov_loc_3_plot +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.title.y = element_blank(),
              plot.margin = margin(t =10, b= 10, l=15, r = 10),
              legend.position = 'none'),
      vp = vplayout(3,3))
print(Monthly_MAR_ILR_cov_loc_3_plot +
        theme(axis.text.x = element_text(size = 20,
                                         angle = 45,
                                         hjust = 1,
                                         vjust = 0.5),
              axis.text.y = element_blank(),
              axis.title.y = element_blank(),
              plot.margin = margin(t =10, b= 10, l=15, r = 10),
              legend.position = 'none'),
      vp = vplayout(4,3))
graphics.off()

