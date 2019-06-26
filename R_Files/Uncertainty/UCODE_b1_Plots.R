rm(list=ls())

library(ggplot2)
library(grid)
library(reshape2)
library(stringr)
library(ggthemes)
library(ssh)
library(ggridges)

out_dir = paste0(getwd(),'/Results/')
# session <- ssh_connect("dtolley@aqua.lawr.ucdavis.edu")
# Cal_1_b1 = scp_download(session,files = '/aqua/dtolley/UCODE_Linear_Uncert/Basecase/Calibration_1/SVIHM_Cal_1._b1',to = './')
# Cal_2_b1 = scp_download(session,files = '/aqua/dtolley/UCODE_Linear_Uncert/Basecase/Calibration_2/SVIHM_Cal_2._b1',to = './')
# Cal_3_b1 = scp_download(session,files = '/aqua/dtolley/UCODE_Linear_Uncert/Basecase/Calibration_3/SVIHM_Cal_3._b1',to = './')
# Cal_4_b1 = scp_download(session,files = '/aqua/dtolley/UCODE_Linear_Uncert/Basecase/Calibration_4/SVIHM_Cal_4._b1',to = './')
# Cal_5_b1 = scp_download(session,files = '/aqua/dtolley/UCODE_Linear_Uncert/Basecase/Calibration_5/SVIHM_Cal_5._b1',to = './')

cal_params = c('kx1','kx2','kx3','kx4','sy1','sy3','RD_Mult','EIE_Flood','EIE_WL_LU25','EIE_CP_LU25','EIE_WL_LU2','EIE_CP_LU2',
               'kc_alfalfa_mult','kc_pasture_mult')


Params_Cal_1 = read.table('SVIHM_Cal_1._b1', header = F, stringsAsFactors = F, skip = 1)[,c(1:4,19,21,52:58,60)]
names(Params_Cal_1) = cal_params
Params_Cal_2 = read.table('SVIHM_Cal_2._b1', header = F, stringsAsFactors = F, skip = 1)[,c(1:4,19,21,52:58,60)]
names(Params_Cal_2) = cal_params
Params_Cal_3 = read.table('SVIHM_Cal_3._b1', header = F, stringsAsFactors = F, skip = 1)[,c(1:4,19,21,52:58,60)]
names(Params_Cal_3) = cal_params
Params_Cal_4 = read.table('SVIHM_Cal_4._b1', header = F, stringsAsFactors = F, skip = 1)[,c(1:4,19,21,52:58,60)]
names(Params_Cal_4) = cal_params
Params_Cal_5 = read.table('SVIHM_Cal_5._b1', header = F, stringsAsFactors = F, skip = 1)[,c(1:4,19,21,52:58,60)]
names(Params_Cal_5) = cal_params

All_params = rbind(Params_Cal_1, Params_Cal_2, Params_Cal_3, Params_Cal_4, Params_Cal_5)
All_params$Calibration = rep(paste0('Cal ',seq(1,5)),each = 28)
All_params_melt = melt(data = All_params, id.vars = 'Calibration')

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


UCODE_b1_K_plot = ggplot(data = subset(All_params_melt, variable==c('Kx1','Kx2','Kx3','Kx4')), 
                          aes(y = variable, x = value, fill = Calibration, color = Calibration, point_color = Calibration, point_alpha = 1)) + 
  geom_density_ridges(point_size = 0.25, alpha = 0.3, rel_min_height = 0.01, scale = 1, jittered_points=TRUE) + 
  ggtitle('Hydraulic Conductivity') +
  xlab('Hydraulic Conductivity (m/day)') +
  scale_fill_brewer(palette = 'Set1') +
  scale_color_brewer(palette = 'Set1', aesthetics = c('color', 'point_color')) +
  scale_y_discrete(expand = c(0.1, 0.1)) +
  #scale_x_continuous(limits = c(0,300), breaks = seq(0,300,by = 100), expand = c(0, 0)) +
  scale_x_log10(limits = c(1,1000),expand = c(0, 0) ) +
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
  
  
UCODE_b1_Sy_plot = ggplot(data = subset(All_params_melt, variable==c('Sy1','Sy3')), 
                           aes(y = variable, x = value, fill = Calibration, color = Calibration, point_color = Calibration, point_alpha = 1)) + 
    geom_density_ridges(point_size = 0.25, alpha = 0.3, rel_min_height = 0.01, scale = 1, jittered_points=TRUE) + 
  ggtitle('Specific Yield') +
  xlab('Specific Yield (-)') +
  scale_fill_brewer(palette = 'Set1') +
  scale_color_brewer(palette = 'Set1', aesthetics = c('color', 'point_color')) +
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
        legend.key.size = unit(0.4,'cm'))

UCODE_b1_SWBM_plot = ggplot(data = subset(All_params_melt, variable==c('RD_Mult','SMDF_Flood','SMDF_A/G_WL','SMDF_A/G_CP','SMDF_P_WL','SMDF_P_CP',
                                                                       'Kc_Alfalfa_Mult','Kc_Pasture_Mult')), 
                             aes(y = variable, x = value, fill = Calibration, color = Calibration, point_color = Calibration, point_alpha = 1)) + 
    geom_density_ridges(point_size = 0.25, alpha = 0.3, rel_min_height = 0.01, scale = 1, jittered_points=TRUE) + 
    ggtitle('SWBM Parameters') +
    xlab('Value') +
    scale_fill_brewer(palette = 'Set1') +
    scale_color_brewer(palette = 'Set1', aesthetics = c('color', 'point_color')) +
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
          legend.key.size = unit(0.4,'cm'))

png(paste0(out_dir,'UCODE_b1_values.png'), width = 4.75, height = 6.75, res = 600, units = 'in')
grid.newpage()
pushViewport(viewport(layout = grid.layout(14,1)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(UCODE_b1_K_plot,vp = vplayout(1:4,1))
print(UCODE_b1_Sy_plot,vp = vplayout(5:8,1))
print(UCODE_b1_SWBM_plot,vp = vplayout(9:14,1))
graphics.off()
