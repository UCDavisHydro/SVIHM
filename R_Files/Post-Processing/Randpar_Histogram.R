rm(list=ls())

library(ggplot2)
library(grid)
library(reshape2)

params = c('kx1','kx2','kx3','kx4','kx5','kx6','kx7','kx8','kx9','kvar1','kvar2','kvar3','kvar4','kvar5','kvar6','kvar7',
           'kvar8','kvar9','sy1','sy2','sy3','sy4','sy5','sy6','sy7','sy8','sy9','ss1','ss2','ss3','ss4','ss5','ss6',
           'ss7','ss8','ss9','wel5','wel6','wel7','wel8','wel9','wel10','wel11','wel20','wel21','bedk1','bedk2','bedk3',
           'rough1','rough2','rough3','RD_Mult','EIE_Flood','EIE_WL_LU25','EIE_CP_LU25','EIE_WL_LU2','EIE_CP_LU2',
           'kc_alfalfa_mult','kc_grain_mult','kc_pasture_mult','kc_noirr')
cal_params = c('kx1','kx2','kx3','kx4','sy1','sy3','RD_Mult','EIE_Flood','EIE_WL_LU25','EIE_CP_LU25','EIE_WL_LU2','EIE_CP_LU2',
               'kc_alfalfa_mult','kc_pasture_mult')
unirandpar_files = paste0(getwd(),'/UniRandPar/',list.files(path = paste0(getwd(),'/UniRandPar/'),pattern = 'unirandpar'))
unirandpars = as.data.frame(t(sapply(lapply(X = unirandpar_files, FUN = function(x) read.table(x, skip = 3, nrows = 61, header = F)),"[[",3)))
names(unirandpars) = params
unirandpars$RandPar_Type = 'Uniform'

covrandpar_cal_1_files = paste0(getwd(),'/CovRandPar/',list.files(path = paste0(getwd(),'/CovRandPar/'),pattern = 'cal_1'))
covrandpar_cal_2_files = paste0(getwd(),'/CovRandPar/',list.files(path = paste0(getwd(),'/CovRandPar/'),pattern = 'cal_2'))
covrandpar_cal_3_files = paste0(getwd(),'/CovRandPar/',list.files(path = paste0(getwd(),'/CovRandPar/'),pattern = 'cal_3'))
covrandpar_cal_4_files = paste0(getwd(),'/CovRandPar/',list.files(path = paste0(getwd(),'/CovRandPar/'),pattern = 'cal_4'))
covrandpar_cal_5_files = paste0(getwd(),'/CovRandPar/',list.files(path = paste0(getwd(),'/CovRandPar/'),pattern = 'cal_5'))

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

(kx1_density = ggplot(data = kx1, aes(x = kx1, group = RandPar_Type, fill = RandPar_Type), color = 'black') + 
    geom_density(alpha = 0.5, size = 0.75) +
    ylab('Density') +
    xlab('Hydraulic Conductivity (m/day)') +
    ggtitle('kx1') +
    scale_fill_manual(values = c('Black', 'Red','Blue','Burlywood3','Purple','green')) +
    scale_y_continuous(limits = c(0,0.08), expand = c(0,0)) +
    scale_x_continuous(limits = c(0,300), breaks = seq(0,300,by = 50), expand = c(0,0)) +
    theme(panel.background = element_blank(),
          plot.background = element_blank(),
          panel.border = element_rect(fill = NA, color = 'black'),
          plot.title = element_text(size = 20,hjust = 0.5, face = 'bold'),
          legend.position = c(0.8,0.65),
          legend.background = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 24),
          axis.text = element_text(size = 18),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_blank(),
          plot.margin = margin(t=10,b=10,l=20,r=20))
) 
(kx2_density = ggplot(data = kx2, aes(x = kx2, group = RandPar_Type, fill = RandPar_Type), color = 'black') + 
    geom_density(alpha = 0.5, size = 0.75) +
    ylab('Density') +
    xlab('Hydraulic Conductivity (m/day)') +
    ggtitle('kx2') +
    scale_fill_manual(values = c('Black', 'Red','Blue','Burlywood3','Purple','green')) +
    scale_y_continuous(limits = c(0,0.5), expand = c(0,0)) +
    scale_x_continuous(limits = c(0,15), breaks = seq(0,15,by = 5), expand = c(0,0)) +
    theme(panel.background = element_blank(),
          plot.background = element_blank(),
          panel.border = element_rect(fill = NA, color = 'black'),
          plot.title = element_text(size = 20,hjust = 0.5, face = 'bold'),
          legend.position = 'none',
          axis.text = element_text(size = 18),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_blank(),
          plot.margin = margin(t=10,b=10,l=20,r=20))
) 
(kx3_density = ggplot(data = kx3, aes(x = kx3, group = RandPar_Type, fill = RandPar_Type), color = 'black') + 
    geom_density(alpha = 0.5, size = 0.75) +
    ylab('Density') +
    xlab('Hydraulic Conductivity (m/day)') +
    ggtitle('kx3') +
    scale_y_continuous(limits = c(0,0.20), expand = c(0,0)) +
    scale_x_continuous(limits = c(0,250), breaks = seq(0,250,by = 50), expand = c(0,0)) +
    theme(panel.background = element_blank(),
          plot.background = element_blank(),
          panel.border = element_rect(fill = NA, color = 'black'),
          plot.title = element_text(size = 20,hjust = 0.5, face = 'bold'),
          legend.position = 'none',
          axis.text = element_text(size = 18),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_blank(),
          plot.margin = margin(t=10,b=10,l=20,r=20))
)
(kx4_density = ggplot(data = kx4, aes(x = kx4, group = RandPar_Type, fill = RandPar_Type), color = 'black') + 
    geom_density(alpha = 0.5, size = 0.75) +
    ylab('Density') +
    xlab('Hydraulic Conductivity (m/day)') +
    ggtitle('kx4') +
    scale_fill_manual(values = c('Black', 'Red','Blue','Burlywood3','Purple','green')) +
    scale_color_manual(values = c('Black', 'Red','Blue','Orange','Green','Purple')) +
    scale_y_continuous(limits = c(0,0.20), expand = c(0,0)) +
    scale_x_continuous(limits = c(0,40), breaks = seq(0,40,by = 10), expand = c(0,0)) +
    theme(panel.background = element_blank(),
          plot.background = element_blank(),
          panel.border = element_rect(fill = NA, color = 'black'),
          plot.title = element_text(size = 20,hjust = 0.5, face = 'bold'),
          legend.position = 'none',
          axis.text = element_text(size = 18),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_blank(),
          plot.margin = margin(t=10,b=10,l=20,r=20))
) 
(sy1_density = ggplot(data = sy1, aes(x = sy1, group = RandPar_Type, fill = RandPar_Type), color = 'black') + 
    geom_density(alpha = 0.5, size = 0.75) +
    ylab('Density') +
    xlab('Specific Yield (-)') +
    ggtitle('sy1') +
    scale_fill_manual(values = c('Black', 'Red','Blue','Burlywood3','Purple','green')) +
    scale_y_continuous(limits = c(0,30), expand = c(0,0)) +
    scale_x_continuous(limits = c(0,0.25), breaks = seq(0,0.25,by = 0.05), expand = c(0,0)) +
    theme(panel.background = element_blank(),
          plot.background = element_blank(),
          panel.border = element_rect(fill = NA, color = 'black'),
          plot.title = element_text(size = 20,hjust = 0.5, face = 'bold'),
          legend.position = 'none',
          axis.text = element_text(size = 18),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_blank(),
          plot.margin = margin(t=10,b=10,l=20,r=20))
) 
(sy3_density = ggplot(data = sy3, aes(x = sy3, group = RandPar_Type, fill = RandPar_Type), color = 'black') + 
    geom_density(alpha = 0.5, size = 0.75) +
    ylab('Density') +
    xlab('Specific Yield (-)') +
    ggtitle('sy3') +
    scale_fill_manual(values = c('Black', 'Red','Blue','Burlywood3','Purple','green')) +
    scale_y_continuous(limits = c(0,50), expand = c(0,0)) +
    scale_x_continuous(limits = c(0,0.2), breaks = seq(0,0.2,by = 0.05), expand = c(0,0)) +
    theme(panel.background = element_blank(),
          plot.background = element_blank(),
          panel.border = element_rect(fill = NA, color = 'black'),
          plot.title = element_text(size = 20,hjust = 0.5, face = 'bold'),
          legend.position = 'none',
          axis.text = element_text(size = 18),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_blank(),
          plot.margin = margin(t=10,b=10,l=20,r=20))
) 
(RD_Mult_density = ggplot(data = RD_Mult, aes(x = RD_Mult, group = RandPar_Type, fill = RandPar_Type), color = 'black') + 
    geom_density(alpha = 0.5, size = 0.75) +
    ylab('Density') +
    xlab('Root Zone Depth Multiplier (-)') +
    ggtitle('Root Zone Depth Multiplier') +
    scale_fill_manual(values = c('Black', 'Red','Blue','Burlywood3','Purple','green')) +
    scale_y_continuous(limits = c(0,2), expand = c(0,0)) +
    scale_x_continuous(limits = c(0,6), breaks = seq(0,6), expand = c(0,0)) +
    theme(panel.background = element_blank(),
          plot.background = element_blank(),
          panel.border = element_rect(fill = NA, color = 'black'),
          plot.title = element_text(size = 20,hjust = 0.5, face = 'bold'),
          legend.position = 'none',
          axis.text = element_text(size = 18),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_blank(),
          plot.margin = margin(t=10,b=10,l=20,r=20))
) 
(EIE_Flood_density = ggplot(data = EIE_Flood, aes(x = EIE_Flood, group = RandPar_Type, fill = RandPar_Type), color = 'black') + 
    geom_density(alpha = 0.5, size = 0.75) +
    ylab('Density') +
    xlab('SMDF') +
    ggtitle('Flood SMDF') +
    scale_fill_manual(values = c('Black', 'Red','Blue','Burlywood3','Purple','green')) +
    scale_y_continuous(limits = c(0,10), expand = c(0,0)) +
    scale_x_continuous(limits = c(0.4,1.2), breaks = seq(0.4,1.2, by = 0.2), expand = c(0,0)) +
    theme(panel.background = element_blank(),
          plot.background = element_blank(),
          panel.border = element_rect(fill = NA, color = 'black'),
          plot.title = element_text(size = 20,hjust = 0.5, face = 'bold'),
          legend.position = 'none',
          axis.text = element_text(size = 18),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_blank(),
          plot.margin = margin(t=10,b=10,l=20,r=20))
) 
(EIE_WL_LU25_density = ggplot(data = EIE_WL_LU25, aes(x = EIE_WL_LU25, group = RandPar_Type, fill = RandPar_Type), color = 'black') + 
    geom_density(alpha = 0.5, size = 0.75) +
    ylab('Density') +
    xlab('SMDF') +
    ggtitle('Alfalfa/Grain Wheel Line SMDF') +
    scale_fill_manual(values = c('Black', 'Red','Blue','Burlywood3','Purple','green')) +
    scale_y_continuous(limits = c(0,8), expand = c(0,0)) +
    scale_x_continuous(limits = c(0.75,2), breaks = seq(0.75,2, by = 0.25), expand = c(0,0)) +
    theme(panel.background = element_blank(),
          plot.background = element_blank(),
          panel.border = element_rect(fill = NA, color = 'black'),
          plot.title = element_text(size = 20,hjust = 0.5, face = 'bold'),
          legend.position = 'none',
          axis.text = element_text(size = 18),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_blank(),
          plot.margin = margin(t=10,b=10,l=20,r=20))
) 
(EIE_CP_LU25_density = ggplot(data = EIE_CP_LU25, aes(x = EIE_CP_LU25, group = RandPar_Type, fill = RandPar_Type), color = 'black') + 
    geom_density(alpha = 0.5, size = 0.75) +
    ylab('Density') +
    xlab('SMDF') +
    ggtitle('Alfalfa/Grain Center Pivot SMDF') +
    scale_fill_manual(values = c('Black', 'Red','Blue','Burlywood3','Purple','green')) +
    scale_y_continuous(limits = c(0,8), expand = c(0,0)) +
    scale_x_continuous(limits = c(0.5,2.25), breaks = seq(0.5,2.25, by = 0.25), expand = c(0,0)) +
    theme(panel.background = element_blank(),
          plot.background = element_blank(),
          panel.border = element_rect(fill = NA, color = 'black'),
          plot.title = element_text(size = 20,hjust = 0.5, face = 'bold'),
          legend.position = 'none',
          axis.text = element_text(size = 18),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_blank(),
          plot.margin = margin(t=10,b=10,l=20,r=20))
)
(EIE_WL_LU2_density = ggplot(data = EIE_WL_LU2, aes(x = EIE_WL_LU2, group = RandPar_Type, fill = RandPar_Type), color = 'black') + 
    geom_density(alpha = 0.5, size = 0.75) +
    ylab('Density') +
    xlab('SMDF') +
    ggtitle('Pasture Wheel Line SMDF') +
    scale_fill_manual(values = c('Black', 'Red','Blue','Burlywood3','Purple','green')) +
    scale_y_continuous(limits = c(0,6), expand = c(0,0)) +
    scale_x_continuous(limits = c(0.25,1.75), breaks = seq(0.25,1.75, by = 0.25), expand = c(0,0)) +
    theme(panel.background = element_blank(),
          plot.background = element_blank(),
          panel.border = element_rect(fill = NA, color = 'black'),
          plot.title = element_text(size = 20,hjust = 0.5, face = 'bold'),
          legend.position = 'none',
          axis.text = element_text(size = 18),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_blank(),
          plot.margin = margin(t=10,b=10,l=20,r=20))
)
(EIE_CP_LU2_density = ggplot(data = EIE_CP_LU2, aes(x = EIE_CP_LU2, group = RandPar_Type, fill = RandPar_Type), color = 'black') + 
    geom_density(alpha = 0.5, size = 0.75) +
    ylab('Density') +
    xlab('SMDF') +
    ggtitle('Pasture Center Pivot SMDF') +
    scale_fill_manual(values = c('Black', 'Red','Blue','Burlywood3','Purple','green')) +
    scale_y_continuous(limits = c(0,8), expand = c(0,0)) +
    scale_x_continuous(limits = c(0.4,2.4), breaks = seq(0.4,2.4, by = 0.2), expand = c(0,0)) +
    theme(panel.background = element_blank(),
          plot.background = element_blank(),
          panel.border = element_rect(fill = NA, color = 'black'),
          plot.title = element_text(size = 20,hjust = 0.5, face = 'bold'),
          legend.position = 'none',
          axis.text = element_text(size = 18),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_blank(),
          plot.margin = margin(t=10,b=10,l=20,r=20))
)
(kc_alfalfa_mult_density = ggplot(data = kc_alfalfa_mult, aes(x = kc_alfalfa_mult, group = RandPar_Type, fill = RandPar_Type), color = 'black') + 
    geom_density(alpha = 0.5, size = 0.75) +
    ylab('Density') +
    xlab('Crop Coefficient') +
    ggtitle('Alfalfa Crop Coefficent Multiplier') +
    scale_fill_manual(values = c('Black', 'Red','Blue','Burlywood3','Purple','green')) +
    scale_y_continuous(limits = c(0,16), expand = c(0,0)) +
    scale_x_continuous(limits = c(0.7,1.4), breaks = seq(0.7,1.4, by = 0.1), expand = c(0,0)) +
    theme(panel.background = element_blank(),
          plot.background = element_blank(),
          panel.border = element_rect(fill = NA, color = 'black'),
          plot.title = element_text(size = 20,hjust = 0.5, face = 'bold'),
          legend.position = 'none',
          axis.text = element_text(size = 18),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_blank(),
          plot.margin = margin(t=10,b=10,l=20,r=20))
)
(kc_pasture_mult_density = ggplot(data = kc_pasture_mult, aes(x = kc_pasture_mult, group = RandPar_Type, fill = RandPar_Type), color = 'black') + 
    geom_density(alpha = 0.5, size = 0.75) +
    ylab('Density') +
    xlab('Crop Coefficient') +
    ggtitle('Pasture Crop Coefficent Multiplier') +
    scale_fill_manual(values = c('Black', 'Red','Blue','Burlywood3','Purple','green')) +
    scale_y_continuous(limits = c(0,20), expand = c(0,0)) +
    scale_x_continuous(limits = c(0.75,1.5), breaks = seq(0.75,1.5, by = 0.25), expand = c(0,0)) +
    theme(panel.background = element_blank(),
          plot.background = element_blank(),
          panel.border = element_rect(fill = NA, color = 'black'),
          plot.title = element_text(size = 20,hjust = 0.5, face = 'bold'),
          legend.position = 'none',
          axis.text = element_text(size = 18),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_blank(),
          plot.margin = margin(t=10,b=10,l=20,r=20))
)

pdf(paste0('RandPar_Histograms.pdf'), width = 16, height = 26)
grid.newpage()
pushViewport(viewport(layout = grid.layout(7,2)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(kx1_density, vp = vplayout(1,1))
print(kx2_density, vp = vplayout(2,1))
print(kx3_density, vp = vplayout(3,1))
print(kx4_density, vp = vplayout(4,1))
print(sy1_density, vp = vplayout(5,1))
print(sy3_density, vp = vplayout(6,1))
print(RD_Mult_density, vp = vplayout(7,1))
print(EIE_Flood_density, vp = vplayout(1,2))
print(EIE_WL_LU25_density, vp = vplayout(2,2))
print(EIE_CP_LU25_density, vp = vplayout(3,2))
print(EIE_WL_LU2_density, vp = vplayout(4,2))
print(EIE_CP_LU2_density, vp = vplayout(5,2))
print(kc_alfalfa_mult_density, vp = vplayout(6,2))
print(kc_pasture_mult_density, vp = vplayout(7,2))
graphics.off()
