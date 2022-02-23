rm(list=ls())

library(ggplot2)
library(grid)
library(reshape2)
library(stringr)
library(ggthemes)

session <- ssh_connect("dtolley@aqua.lawr.ucdavis.edu")
Cal_1_b1 = scp_download(session,files = '/aqua/dtolley/UCODE_Linear_Uncert/Basecase/Calibration_1/SVIHM_Cal_1._b1',to = './')
Cal_2_b1 = scp_download(session,files = '/aqua/dtolley/UCODE_Linear_Uncert/Basecase/Calibration_2/SVIHM_Cal_2._b1',to = './')
Cal_3_b1 = scp_download(session,files = '/aqua/dtolley/UCODE_Linear_Uncert/Basecase/Calibration_3/SVIHM_Cal_3._b1',to = './')
Cal_4_b1 = scp_download(session,files = '/aqua/dtolley/UCODE_Linear_Uncert/Basecase/Calibration_4/SVIHM_Cal_4._b1',to = './')
Cal_5_b1 = scp_download(session,files = '/aqua/dtolley/UCODE_Linear_Uncert/Basecase/Calibration_5/SVIHM_Cal_5._b1',to = './')

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

UCODE_b1_K_plot = ggplot(data = subset(All_params_melt, variable==c('kx1','kx2','kx3','kx4')), aes(x = variable, y = value)) + 
  geom_boxplot(aes(fill = Calibration)) +
  scale_y_continuous(limits = c(0,300), expand = c(0,0)) +
  ggtitle('Hydraulic Conductivity') +
  theme_few() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 10),
        legend.position = c(0.9,0.7),
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.5,'cm'))
UCODE_b1_Sy_plot = ggplot(data = subset(All_params_melt, variable==c('sy1','sy3')), aes(x = variable, y = value)) + 
  geom_boxplot(aes(fill = Calibration)) +
  scale_y_continuous(limits = c(0,0.4), expand = c(0,0)) +
  ggtitle('Specific Yield') +
  theme_few() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 10),
        legend.position = 'none',
        legend.title = element_blank())

UCODE_b1_SWBM_plot = ggplot(data = subset(All_params_melt, variable==c('RD_Mult','EIE_Flood','EIE_WL_LU25','EIE_CP_LU25','EIE_WL_LU2','EIE_CP_LU2',
                                                 'kc_alfalfa_mult','kc_pasture_mult')), aes(x = variable, y = value)) + 
  geom_boxplot(aes(fill = Calibration))  +
  scale_y_continuous(limits = c(0.25,1.75),breaks = seq(0.25,1.75,by = 0.5), expand = c(0,0)) +
  ggtitle('SWBM Varaibles') +
  theme_few() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 10),
        legend.position = 'none',
        legend.title = element_blank())

png('UCODE_b1_values.png', width = 5, height = 8, res = 600, units = 'in')
grid.newpage()
pushViewport(viewport(layout = grid.layout(3,1)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(UCODE_b1_K_plot,vp = vplayout(1,1))
print(UCODE_b1_Sy_plot,vp = vplayout(2,1))
print(UCODE_b1_SWBM_plot,vp = vplayout(3,1))
graphics.off()
