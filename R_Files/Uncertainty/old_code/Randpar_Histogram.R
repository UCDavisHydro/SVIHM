rm(list=ls())

library(ggplot2)
library(grid)
library(reshape2)
library(stringr)
library(ggthemes)
library(ggridges)
out_dir = paste0(getwd(),'/Results/')

params = c('kx1','kx2','kx3','kx4','kx5','kx6','kx7','kx8','kx9','kvar1','kvar2','kvar3','kvar4','kvar5','kvar6','kvar7',
           'kvar8','kvar9','sy1','sy2','sy3','sy4','sy5','sy6','sy7','sy8','sy9','ss1','ss2','ss3','ss4','ss5','ss6',
           'ss7','ss8','ss9','MFR5','MFR6','MFR7','MFR8','MFR9','MFR10','MFR11','FRMRSDitch','SVIDDitch','bedk1','bedk2','bedk3',
           'rough1','rough2','rough3','RD_Mult','EIE_Flood','EIE_WL_LU25','EIE_CP_LU25','EIE_WL_LU2','EIE_CP_LU2',
           'kc_alfalfa_mult','kc_grain_mult','kc_pasture_mult','kc_noirr')
cal_params = c('kx1','kx2','kx3','kx4','sy1','sy3','RD_Mult','EIE_Flood','EIE_WL_LU25','EIE_CP_LU25','EIE_WL_LU2','EIE_CP_LU2',
               'kc_alfalfa_mult','kc_pasture_mult')
unirandpar_files = paste0(getwd(),'/Pre-Processing/',list.files(path = paste0(getwd(),'/Pre-Processing/'),pattern = 'unirandpar'))
unirandpars = as.data.frame(t(sapply(lapply(X = unirandpar_files, FUN = function(x) read.table(x, skip = 3, nrows = 61, header = F)),"[[",3)))
names(unirandpars) = params
unirandpars$RandPar_Type = 'Uniform'

covrandpar_cal_1_files = paste0(getwd(),'/Pre-Processing/',list.files(path = paste0(getwd(),'/Pre-Processing/'),pattern = 'cal_1'))
covrandpar_cal_2_files = paste0(getwd(),'/Pre-Processing/',list.files(path = paste0(getwd(),'/Pre-Processing/'),pattern = 'cal_2'))
covrandpar_cal_3_files = paste0(getwd(),'/Pre-Processing/',list.files(path = paste0(getwd(),'/Pre-Processing/'),pattern = 'cal_3'))
covrandpar_cal_4_files = paste0(getwd(),'/Pre-Processing/',list.files(path = paste0(getwd(),'/Pre-Processing/'),pattern = 'cal_4'))
covrandpar_cal_5_files = paste0(getwd(),'/Pre-Processing/',list.files(path = paste0(getwd(),'/Pre-Processing/'),pattern = 'cal_5'))

covrandpar_1 = as.data.frame(t(sapply(lapply(X = covrandpar_cal_1_files, FUN = function(x) read.table(x, skip = 3, nrows = 61, header = F)),"[[",3)))
covrandpar_2 = as.data.frame(t(sapply(lapply(X = covrandpar_cal_2_files, FUN = function(x) read.table(x, skip = 3, nrows = 61, header = F)),"[[",3)))
covrandpar_3 = as.data.frame(t(sapply(lapply(X = covrandpar_cal_3_files, FUN = function(x) read.table(x, skip = 3, nrows = 61, header = F)),"[[",3)))
covrandpar_4 = as.data.frame(t(sapply(lapply(X = covrandpar_cal_4_files, FUN = function(x) read.table(x, skip = 3, nrows = 61, header = F)),"[[",3)))
covrandpar_5 = as.data.frame(t(sapply(lapply(X = covrandpar_cal_5_files, FUN = function(x) read.table(x, skip = 3, nrows = 61, header = F)),"[[",3)))

names(covrandpar_1) = params
names(covrandpar_2) = params
names(covrandpar_3) = params
names(covrandpar_4) = params
names(covrandpar_5) = params

covrandpar_1$RandPar_Type = 'Cal_1' 
covrandpar_2$RandPar_Type = 'Cal_2' 
covrandpar_3$RandPar_Type = 'Cal_3' 
covrandpar_4$RandPar_Type = 'Cal_4' 
covrandpar_5$RandPar_Type = 'Cal_5'

for (i in 1:length(cal_params)){
param_name = cal_params[i]
eval(parse(text = paste0(param_name," = rbind(subset(unirandpars,select = c(param_name,'RandPar_Type')),
            subset(covrandpar_1,select = c(param_name,'RandPar_Type')),
            subset(covrandpar_2,select = c(param_name,'RandPar_Type')),
            subset(covrandpar_3,select = c(param_name,'RandPar_Type')),
            subset(covrandpar_4,select = c(param_name,'RandPar_Type')),
            subset(covrandpar_5,select = c(param_name,'RandPar_Type')))")))
eval(parse(text = paste0(param_name,"$RandPar_Type = factor(",param_name,"$RandPar_Type, levels = c('Uniform', 'Cal_1', 'Cal_2', 'Cal_3','Cal_4','Cal_5'))")))
eval(parse(text = paste0(param_name," = ",param_name,"[order(",param_name,"$RandPar_Type),]")))
}

All_params = rbind(unirandpars,covrandpar_1,covrandpar_2,covrandpar_3,covrandpar_4,covrandpar_5)
All_params_melt = melt(All_params, id.vars = 'RandPar_Type')

All_params_melt$variable = gsub(x = All_params_melt$variable, pattern = 'kx1', replacement = 'Kx1')
All_params_melt$variable = gsub(x = All_params_melt$variable, pattern = 'kx2', replacement = 'Kx2')
All_params_melt$variable = gsub(x = All_params_melt$variable, pattern = 'kx3', replacement = 'Kx3')
All_params_melt$variable = gsub(x = All_params_melt$variable, pattern = 'kx4', replacement = 'Kx4')
All_params_melt$variable = gsub(x = All_params_melt$variable, pattern = 'sy1', replacement = 'Sy1')
All_params_melt$variable = gsub(x = All_params_melt$variable, pattern = 'sy3', replacement = 'Sy3')
All_params_melt$variable = gsub(x = All_params_melt$variable, pattern = 'EIE_Flood', replacement = 'SMDF_Flood')
All_params_melt$variable = gsub(x = All_params_melt$variable, pattern = 'EIE_WL_LU25', replacement = 'SMDF_A/G_WL')
All_params_melt$variable = gsub(x = All_params_melt$variable, pattern = 'EIE_CP_LU25', replacement = 'SMDF_A/G_CP')
All_params_melt$variable = gsub(x = All_params_melt$variable, pattern = 'EIE_WL_LU2', replacement = 'SMDF_P_WL')
All_params_melt$variable = gsub(x = All_params_melt$variable, pattern = 'EIE_CP_LU2', replacement = 'SMDF_P_CP')
All_params_melt$variable = gsub(x = All_params_melt$variable, pattern = 'kc_alfalfa_mult', replacement = 'Kc_Alfalfa_Mult')
All_params_melt$variable = gsub(x = All_params_melt$variable, pattern = 'kc_pasture_mult', replacement = 'Kc_Pasture_Mult')

RandPar_K_plot = ggplot(data = subset(All_params_melt, variable==c('Kx1','Kx2','Kx3','Kx4')), 
                     aes(y = variable, x = value, fill = RandPar_Type, color = RandPar_Type, point_color = RandPar_Type, point_alpha = 1)) + 
  geom_density_ridges(point_size = 0.25, alpha = 0.3, rel_min_height = 0.01, scale = 1, jittered_points=TRUE) + 
  ggtitle('Hydraulic Conductivity') +
  scale_fill_brewer(palette = 'Set1') +
  scale_color_brewer(palette = 'Set1', aesthetics = c('color', 'point_color')) +
  xlab('Hydraulic Conductivity (m/day)') +
  scale_x_log10(limits = c(1,1000),expand = c(0, 0) ) +
  scale_y_discrete(expand = c(0.1, 0.1)) +
  theme_few() +
  annotation_logticks(sides = 'b') +
  theme(panel.background = element_rect(fill = 'gray90'),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 8),
        axis.text = element_text(size = 8),
        plot.title = element_blank(),
        plot.margin = margin(t=10,b=5,l=5,r=,10),
        legend.position = c(0.92,0.6),
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill = NA),
        legend.key.size = unit(0.4,'cm'))

RandPar_Sy_plot = ggplot(data = subset(All_params_melt, variable==c('Sy1','Sy3')), 
                        aes(y = variable, x = value, fill = RandPar_Type, color = RandPar_Type, point_color = RandPar_Type, point_alpha = 1)) + 
  geom_density_ridges(point_size = 0.25, alpha = 0.3, rel_min_height = 0.01, scale = 1, jittered_points=TRUE) + 
  ggtitle('Specific Yield') +
  scale_fill_brewer(palette = 'Set1') +
  scale_color_brewer(palette = 'Set1', aesthetics = c('color', 'point_color')) +
  xlab('Specific Yield (-)') +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(limits = c(0,0.45), breaks = seq(0,0.45,by = 0.1), expand = c(0, 0)) +
  theme_few() +
  theme(panel.background = element_rect(fill = 'gray90'),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 8),
        axis.text = element_text(size = 8),
        plot.title = element_blank(),
        plot.margin = margin(t=10,b=5,l=5,r=,10),
        legend.position = 'none',
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill = NA),
        legend.key.size = unit(0.4,'cm'))

RandPar_SWBM_plot = ggplot(data = subset(All_params_melt, variable==c('RD_Mult','SMDF_Flood','SMDF_A/G_WL','SMDF_A/G_CP','SMDF_P_WL','SMDF_P_CP',
                                                                'Kc_Alfalfa_Mult','Kc_Pasture_Mult')), 
                         aes(y = variable, x = value, fill = RandPar_Type, color = RandPar_Type, point_color = RandPar_Type, point_alpha = 1)) + 
  geom_density_ridges(point_size = 0.25, alpha = 0.3, rel_min_height = 0.01, scale = 1, jittered_points=TRUE) + 
  ggtitle('SWBM Parameters') +
  scale_fill_brewer(palette = 'Set1') +
  scale_color_brewer(palette = 'Set1', aesthetics = c('color', 'point_color')) +
  xlab('Value') +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(limits = c(0,5), breaks = seq(0,5,by = 1), expand = c(0, 0)) +
  theme_few() +
  theme(panel.background = element_rect(fill = 'gray90'),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 8),
        axis.text = element_text(size = 8),
        plot.title = element_blank(),
        plot.margin = margin(t=10,b=5,l=5,r=,10),
        legend.position = 'none',
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill = NA),
        legend.key.size = unit(0.4,'cm'))

png(paste0(out_dir,'RandPar_Histograms.png'), width = 4.75, height = 6.75, res = 600, units = 'in')
grid.newpage()
pushViewport(viewport(layout = grid.layout(14,1)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(RandPar_K_plot,vp = vplayout(1:4,1))
print(RandPar_Sy_plot,vp = vplayout(5:8,1))
print(RandPar_SWBM_plot,vp = vplayout(9:14,1))
graphics.off()
