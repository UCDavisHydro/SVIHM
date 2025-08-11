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
out_dir = paste0(getwd(),'/Results/')
fig_format = 'png'   # figure format (png, pdf, jpg)
fig_width =  7.48    # figure width in fig_units
fig_height = 9    # figure height in fig_units
fig_units = 'in'     # units for figure dimensions 
axis_title_size = 7
axis_text_size = 6
line_size = 0.5
point_size = 1.25
crit_months = c('Aug','Sep','Oct')   #Critically dry months, used for generating plots
Oct_Dates = seq(as.Date('1990-10-01'), by = "year", length.out = 21)
Scenario = c('MAR', 'ILR', 'MAR_ILR')

sppp_header = c('PREDICTION.NAME', 'PLOT.SYMBOL', 'Kx1', 'Kx2', 'Kx3', 'Kx4', 'Sy1', 'Sy3', 'RD_Mult', 'SMDF_Flood',
                'SMDF_A/G_WL', 'SMDF_A/G_CP', 'SMDF_P_WL', 'SMDF_P_CP', 'Kc_Alfalfa_Mult', 'Kc_Pasture_Mult')

UCODE_MAR_files = list.files(path = paste0(getwd(),'/UCODE_out/MAR/'), pattern = 'sppp', full.names = T)
UCODE_ILR_files = list.files(path = paste0(getwd(),'/UCODE_out/ILR/'), pattern = 'sppp', full.names = T)
UCODE_MAR_ILR_files = list.files(path = paste0(getwd(),'/UCODE_out/MAR_ILR/'), pattern = 'sppp', full.names = T)

#Plottting themes
conv_theme = theme(axis.text = element_text(size=axis_text_size),
                   axis.title = element_text(size=axis_title_size),
                   legend.title = element_blank(),
                   legend.position = c(0.5,0.9),
                   legend.direction = 'horizontal',
                   legend.spacing = unit(-0.2, 'cm'),
                   legend.text.align = 0.5,
                   plot.title = element_text(size = axis_title_size+1, hjust = 0.5))
for (k in 1:2){
for (j in 1:3){      #loop over scenarios
  for(i in 1:5){     #loop over calibrations
    eval(parse(text = paste0(Scenario[j],"_Cal_",i,"_Oct_Loc_",k,"_sppp = read.table(file = UCODE_",Scenario[j],"_files[",i,"], header = T) %>% subset(grepl(PREDICTION.NAME,pattern = 'Diff_",k,"_Oct'))")))
    eval(parse(text = paste0("names(",Scenario[j],"_Cal_",i,"_Oct_Loc_",k,"_sppp) = sppp_header")))
    eval(parse(text = paste0(Scenario[j],"_Cal_",i,"_Oct_Loc_",k,"_sppp$Date = Oct_Dates")))
    eval(parse(text = paste0(Scenario[j],"_Cal_",i,"_Oct_Loc_",k,"_sppp_melt = melt(",Scenario[j],"_Cal_",i,"_Oct_Loc_",k,"_sppp, id.vars = c('PREDICTION.NAME', 'PLOT.SYMBOL', 'Date'))")))
    
    eval(parse(text = paste0("plot_data = ",Scenario[j],"_Cal_",i,"_Oct_Loc_",k,"_sppp_melt")))
    plot_data$norm = plot_data$value / max(abs(subset(plot_data, Date!='1990-10-01', select = value)))
    #plot_variables = subset(aggregate(.~variable,subset(plot_data, select = c('variable', 'value', 'norm')), FUN = function(x) max(abs(x))),norm>0.01, select = variable)
    #Plot = ggplot(subset(plot_data, Date!='1990-10-01' & variable%in%plot_variables$variable), aes(x = Date, y = value, color = variable)) + 
    Plot = ggplot(subset(plot_data, Date!='1990-10-01'), aes(x = Date, y = norm, color = variable)) + 
      geom_line() + 
      geom_point() +
      theme_few() +
      scale_x_date(limits = c(as.Date('1990-10-01'), as.Date('2011-10-01')), date_breaks = '2 years', date_labels = '%Y') +
      scale_y_continuous(limits = c(-1,1), breaks = seq(-1,1,by = 0.5)) +
      theme(axis.title = element_blank(),
            axis.text = element_blank(),
            legend.position = 'none')
            #legend.position = c(0.5, 0.25)) +
      guides(col = guide_legend(nrow = 5))
    
    eval(parse(text = paste0(Scenario[j],"_Cal_",i,"_Oct_Loc_",k,"_sppp_plot = Plot")))
  }
}
}

fig_name = 'Prediciton Sensitivities Loc 1 and 2'                                                                           
if (fig_format == 'jpg'){                                                                                     
  jpeg(paste0(out_dir,fig_name,'.jpg'), width = fig_width, height = fig_height, units = fig_units,  res = 600)
} else if (fig_format == 'png'){                                                                              
  png(paste0(out_dir,fig_name,'.png'), width = 7.48, height = 9, units = 'in',  res = 600) 
} else if (fig_format == 'pdf'){                                                                              
  pdf(paste0(out_dir,fig_name,'.pdf'), width = 7, height = 5)                                
}                                                                                                             
grid.newpage()                                                                                                
pushViewport(viewport(layout = grid.layout(5,6)))                                                             
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)                                   

for (j in 1:3) {
for (i in 1:5){
    for (k in 1:2){
      print(j*k)
      #eval(parse(text = paste0("print(",Scenario[j],"_Cal_",i,"_Oct_Loc_",k,"_sppp_plot, vp = vplayout(",i,",",j*k,"))")))
    }
  }
}
graphics.off()

