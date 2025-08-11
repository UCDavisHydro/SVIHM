#Script for plotting streamflow depletion analysis at specific locations. 

# Script Initialization ---------------------------------------------------
rm(list = ls())
library(dplyr)
library(ggplot2)
library(reshape2)
library(ggthemes)
library(grid)

# User Input --------------------------------------------------------------
pumping_rate = -2725.5
recharge_rate = 1000
nsegs = 1837
fig_format = 'png'
timeseries_locs = data.frame(row = c(rep(132,3), rep (184,3)),
                             col = c(164,144,132,68,96,120),
                             Distance_m = c(3340,1370,270,5700,2900,500),
                             ID = c(669,664,711,135,249,345),
                             xsec = c(rep('Hamlin',3), rep('West_Side', 3)))
pumping_dates = transform(data.frame(start = seq(as.Date('1991-04-01'), length.out = 21, by = 'year'),
                                     end = seq(as.Date('1991-08-31'), length.out = 21, by = 'year'),
                                     ymin = 0,
                                     ymax = 1),
                          id = 1:21,
                          tier = rep(1,21))
recharge_dates = transform(data.frame(start = seq(as.Date('1991-01-01'), length.out = 21, by = 'year'),
                                     end = seq(as.Date('1991-06-30'), length.out = 21, by = 'year'),
                                     ymin = 0,
                                     ymax = 1),
                          id = 1:21,
                          tier = rep(1,21))

Water_Yr_Class_1991_2011 = transform(data.frame(Water_Year = seq(1991,2011),
                                      start = seq(as.Date('1990-10-01'), length.out = 21, by = 'year'),
                                      end = seq(as.Date('1991-09-30'), length.out = 21, by = 'year'),
                                      `Water Year Type` = c('Critical','Critical','Above Normal','Critical','Wet','Wet','Wet','Wet','Wet',
                                                            'Above Normal','Dry','Dry','Above Normal','Below Normal','Above Normal','Wet',
                                                            'Dry','Critical','Dry','Below Normal','Wet'),
                                      Plot_Color = c('darkred','darkred','skyblue','darkred','navyblue','navyblue','navyblue','navyblue','navyblue',
                                                     'skyblue','red','red','skyblue','orange','skyblue','navyblue',
                                                     'red','darkred','red','orange','navyblue'),
                                      Simple_Wet_Dry = c('Dry','Dry',NA,'Dry','Wet','Wet','Wet','Wet','Wet',
                                                       NA,'Dry','Dry',NA,NA,NA,'Wet',
                                                       'Dry','Dry','Dry',NA,'Wet')),
                                     id = 1:21,
                                     tier = rep(1,21))
out_dir = paste0('./Depletion_Accretion_Figures/')
# Import Data -------------------------------------------------------------
Streamflow_FJ_daily_BC = read.table(file = './Model_output/Basecase/Streamflow_FJ_SVIHM.dat', skip = 2, header = F)[,c(1,3)]
names(Streamflow_FJ_daily_BC) = c('Date', 'flow_m3day')
Streamflow_FJ_daily_BC$Date = seq(as.Date('1990-10-01'), to = as.Date('2011-09-30'), by = 'day')
Streamflow_FJ_daily_BC$gage = 'River km 37'
Streamflow_Pred_Loc_2_daily_BC = read.table(file = './Model_output/Basecase/Streamflow_Pred_Loc_2.dat', skip = 2, header = F)[,c(1,3)]
names(Streamflow_Pred_Loc_2_daily_BC) = c('Date', 'flow_m3day')
Streamflow_Pred_Loc_2_daily_BC$Date = seq(as.Date('1990-10-01'), to = as.Date('2011-09-30'), by = 'day')
Streamflow_Pred_Loc_2_daily_BC$gage = 'River km 51'
Streamflow_Pred_Loc_3_daily_BC = read.table(file = './Model_output/Basecase/Streamflow_Pred_Loc_3.dat', skip = 2, header = F)[,c(1,3)]
names(Streamflow_Pred_Loc_3_daily_BC) = c('Date', 'flow_m3day')
Streamflow_Pred_Loc_3_daily_BC$Date = seq(as.Date('1990-10-01'), to = as.Date('2011-09-30'), by = 'day')
Streamflow_Pred_Loc_3_daily_BC$gage = 'River km 66'
Streamflow_Pred_Loc_4_daily_BC = read.table(file = './Model_output/Basecase/Streamflow_Pred_Loc_4.dat', skip = 2, header = F)[,c(1,3)]
names(Streamflow_Pred_Loc_4_daily_BC) = c('Date', 'flow_m3day')
Streamflow_Pred_Loc_4_daily_BC$Date = seq(as.Date('1990-10-01'), to = as.Date('2011-09-30'), by = 'day')
Streamflow_Pred_Loc_4_daily_BC$gage = 'River km 76'

Streamflow_BC_all = rbind(Streamflow_FJ_daily_BC,Streamflow_Pred_Loc_2_daily_BC,Streamflow_Pred_Loc_3_daily_BC,Streamflow_Pred_Loc_4_daily_BC)

for (i in 1:nrow(timeseries_locs)){
  for (cell_type in c('Depletion','Accretion')){
    temp_daily = read.table(paste0('./',cell_type,'_Results/Daily_Differences_',timeseries_locs$ID[i],'.dat'), header = T, stringsAsFactors = F)
    temp_daily$Date = as.Date(temp_daily$Date)
    if (cell_type=='Depletion'){
      temp_daily[,-1][temp_daily[-1]>0] = NA  #Replace dates with increased streamflow with NA
    }else{
      temp_daily[,-1][temp_daily[-1]<0] = NA  #Replace dates with decreased streamflow with NA  
    }
    temp_daily$Year = format(temp_daily$Date, '%Y')
    temp_daily$Dist2Str_m = timeseries_locs$Distance_m[i]
    eval(parse(text = paste0('Daily_',cell_type,'_',timeseries_locs$xsec[i],'_',timeseries_locs$Distance_m[i],'m = temp_daily')))
    temp_monthly = read.table(paste0('./',cell_type,'_Results/Monthly_Differences_',timeseries_locs$ID[i],'.dat'), header = T, stringsAsFactors = F)
    temp_monthly$print_date = seq(as.Date(paste0(temp_monthly$Month[2],'-01'),'%b-%Y-%d'), length.out = 252, by = 'month') - 1
    temp_monthly$Dist2Str_m = timeseries_locs$Distance_m[i]
    eval(parse(text = paste0('Monthly_',cell_type,'_',timeseries_locs$xsec[i],'_',timeseries_locs$Distance_m[i],'m = temp_monthly')))
  }
}

Hamlin_Depletion_Daily = rbind(Daily_Depletion_Hamlin_3340m,
                               Daily_Depletion_Hamlin_1370m,
                               Daily_Depletion_Hamlin_270m)
West_Side_Depletion_Daily = rbind(Daily_Depletion_West_Side_5700m,
                                  Daily_Depletion_West_Side_2900m,
                                  Daily_Depletion_West_Side_500m)
Hamlin_Depletion_Monthly = rbind(Monthly_Depletion_Hamlin_3340m,
                                 Monthly_Depletion_Hamlin_1370m,
                                 Monthly_Depletion_Hamlin_270m)
West_Side_Depletion_Monthly = rbind(Monthly_Depletion_West_Side_5700m,
                                    Monthly_Depletion_West_Side_2900m,
                                    Monthly_Depletion_West_Side_500m)
Hamlin_Accretion_Daily = rbind(Daily_Accretion_Hamlin_3340m,
                               Daily_Accretion_Hamlin_1370m,
                               Daily_Accretion_Hamlin_270m)
West_Side_Accretion_Daily = rbind(Daily_Accretion_West_Side_5700m,
                                  Daily_Accretion_West_Side_2900m,
                                  Daily_Accretion_West_Side_500m)
Hamlin_Accretion_Monthly = rbind(Monthly_Accretion_Hamlin_3340m,
                                 Monthly_Accretion_Hamlin_1370m,
                                 Monthly_Accretion_Hamlin_270m)
West_Side_Accretion_Monthly = rbind(Monthly_Accretion_West_Side_5700m,
                                    Monthly_Accretion_West_Side_2900m,
                                    Monthly_Accretion_West_Side_500m)

# Mass Balance ------------------------------------------------------------
monthly_depletion_files = list.files(path = paste0('./Depletion_Results/'), pattern = 'Monthly',full.names = T)   
monthly_accretion_files = list.files(path = paste0('./Accretion_Results/'), pattern = 'Monthly',full.names = T)   
Monthly_Depletion_Data = lapply(monthly_depletion_files, FUN = function(x) read.table(x,header = T))
Monthly_Accretion_Data = lapply(monthly_accretion_files, FUN = function(x) read.table(x,header = T))

MB_Depletion_pct = rep(NaN, 252)
MB_Accretion_pct = rep(NaN, 252)
for (i in 1:252){
MB_Depletion_pct[i] = length(which(sapply(Monthly_Depletion_Data, "[[", 15)[i,]>5))/712*100
MB_Accretion_pct[i] = length(which(sapply(Monthly_Accretion_Data, "[[", 15)[i,]>5))/712*100
}
MB_Fail = melt(data.frame(Date = seq(as.Date('1990-11-01'), length.out = 252, by = 'month') - 1,
                     Depletion = MB_Depletion_pct,
                     Accretion = MB_Accretion_pct), id.vars = 'Date')
MB_Fail_plot = ggplot(MB_Fail, aes(x = Date, y = value, color = variable)) + geom_line(size = 0.2) + geom_point(size = 0.3) + 
  ylab('Percentage of Cells\nwith Recovery Error > 5%') +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100, by = 20), expand = c(0.01, 0)) +
  scale_x_date(limits = c(as.Date('1991-01-01'),as.Date('2012-01-01')), 
               date_labels = '%b-%y',
               date_breaks =  '2 years',  
               expand  = c(0,0)) +
  theme_few() +
    theme(legend.title = element_blank(),
          legend.position = c(0.15, 0.8),
          legend.text = element_text(size = 5),
          legend.key.size = unit(10, 'pt'),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
          axis.title.x = element_blank(),
          axis.title = element_text(size = 6),
          axis.text.y = element_text(size = 6))

MB_Fail_Months = data.frame(Date = format(seq(as.Date('1990-11-01'), length.out = 252, by = 'month') - 1,'%b'),
                            Depletion = MB_Depletion_pct/100*712,
                            Accretion = MB_Accretion_pct/100*712) %>% melt(id.vars = 'Date') 

MB_Fail_Months = aggregate(.~Date + variable, data = MB_Fail_Months, FUN = sum)
MB_Fail_Months$Date = factor(MB_Fail_Months$Date, levels=month.abb)
MB_Fail_Months = MB_Fail_Months[order(MB_Fail_Months$Date),]

MB_Fail_Years = data.frame(Date = format(seq(as.Date('1990-11-01'), length.out = 252, by = 'month') - 1,'%Y'),
                            Depletion = MB_Depletion_pct/100*712,
                            Accretion = MB_Accretion_pct/100*712) %>% melt(id.vars = 'Date') 

MB_Fail_Years = aggregate(.~Date + variable, data = MB_Fail_Years, FUN = sum)


Monthly_MB_Fails_plot = ggplot(MB_Fail_Months, aes(x = Date, y = value, fill = variable)) + geom_bar(stat = 'identity', position = 'dodge') +
  ylab('Number of Cells\nwith Recovery Error > 5%') +
  scale_y_continuous(limits = c(0,2500), breaks = seq(0,2500, by = 500), expand = c(0, 0)) +
  theme_few() +
  theme(legend.title = element_blank(),
        legend.position = c(0.8, 0.8),
        legend.text = element_text(size = 5),
        legend.background = element_rect(fill=NA, color = NA),
        legend.key.size = unit(10, 'pt'),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 6),
        axis.text.y = element_text(size = 6))

Yearly_MB_Fails_plot = ggplot(subset(MB_Fail_Years, Date!='1990')) + geom_bar(aes(x = Date, y = value, fill = variable), stat = 'identity', position = 'dodge') +
  ylab('Number of Cells\nwith Recovery Error > 5%') +
  scale_y_continuous(limits = c(-200,2500), breaks = seq(0,2500, by = 500), expand = c(0.01, 0)) +
  scale_x_discrete(breaks = seq(1991,2012,by = 2)) +
  theme_few() +
  theme(legend.title = element_blank(),
        legend.position = c(0.8, 0.8),
        legend.background = element_rect(fill=NA, color = NA),
        legend.text = element_text(size = 5),
        legend.key.size = unit(10, 'pt'),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 6),
        axis.text.y = element_text(size = 6))

fig_name = paste0('Mass_Balance')                                                                           
if (fig_format == 'jpg'){                                                                                     
  jpeg(paste0(out_dir,fig_name,'.jpg'), width = fig_width, height = fig_height, units = fig_units,  res = 600)
} else if (fig_format == 'png'){                                                                              
  png(paste0(out_dir,fig_name,'.png'), width = 6, height = 3, units = 'in',  res = 600) 
} else if (fig_format == 'pdf'){                                                                              
  pdf(paste0(out_dir,fig_name,'.pdf'), width = 7, height = 8.5 )                                          
}                                                                                                             
grid.newpage()                                                                                                
pushViewport(viewport(layout = grid.layout(2,2)))                                                             
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)                                   
print(MB_Fail_plot,vp = vplayout(1:2,1))
print(Monthly_MB_Fails_plot,vp = vplayout(1,2))
print(Yearly_MB_Fails_plot,vp = vplayout(2,2))
graphics.off()

#Basecase streamflow plots -----------------------------------------------------------------------
Basecase_Streamflow_Dry_Year_plot = ggplot(data = Streamflow_BC_all, aes(x=Date, y = flow_m3day, color = gage, fill = gage, group = gage)) +
  geom_line(size = 1) +
  scale_color_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3')) +
  scale_y_log10(limits = c(1E3,1E7), expand = c(0,0)) +
  annotation_logticks() +
  scale_x_date(limits = c(as.Date('2001-01-01'),as.Date('2002-01-01')), 
               date_labels = '%b-%y',
               date_breaks =  'month',  
               expand  = c(0,0)) +
  theme_few() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(angle = 45, hjust = 1))

Basecase_Streamflow_Avg_Year_plot = ggplot(data = Streamflow_BC_all, aes(x=Date, y = flow_m3day, color = gage, fill = gage, group = gage)) +
  geom_line(size = 1) +
  scale_color_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3')) +
  scale_y_log10(limits = c(1E3,1E7), expand = c(0,0)) +
  annotation_logticks() +
  scale_x_date(limits = c(as.Date('2010-01-01'),as.Date('2011-01-01')), 
               date_labels = '%b-%y',
               date_breaks =  'month',  
               expand  = c(0,0)) +
  theme_few() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

Basecase_Streamflow_Wet_Year_plot = ggplot(data = Streamflow_BC_all, aes(x=Date, y = flow_m3day, color = gage, fill = gage, group = gage)) +
  geom_line(size = 1) +
  scale_color_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3')) +
  scale_y_log10(limits = c(1E3,1E7), expand = c(0,0)) +
  annotation_logticks() +
  scale_x_date(limits = c(as.Date('2006-01-01'),as.Date('2007-01-01')), 
               date_labels = '%b-%y',
               date_breaks =  'month',  
               expand  = c(0,0)) +
  theme_few() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

# All Years Plots ----------------------------------------------------------
Hamlin_Depletion_FJ_all_years_plot = ggplot(data = Hamlin_Depletion_Daily, aes(x = Date, y = FJ_Diff_m3day/pumping_rate, color = factor(Dist2Str_m), shape = factor(Dist2Str_m), fill = factor(Dist2Str_m))) + 
  geom_rect(data = pumping_dates, inherit.aes = F, aes(xmin = start,xmax = end,ymin = ymin,ymax = ymax), fill = 'red', alpha = 0.2) +
  geom_line(size = 0.5) +
  #geom_point(size = 0.25) +
  #scale_shape_manual(values = c(21,22,23),breaks = c('270','1370','3340'), labels = c('270 m','1370 m','3340 m')) +
  scale_color_manual(values = c('#e41a1c', '#4daf4a','#377eb8'), breaks = c('270','1370','3340'), labels = c('270 m','1370 m','3340 m')) +
  #scale_fill_manual(values = c('#e41a1c', '#4daf4a','#377eb8'),breaks = c('270','1370','3340'), labels = c('270 m','1370 m','3340 m')) +
  ylab('SDR') +
  scale_x_date(limits = c(as.Date('1990-10-01'),as.Date('2011-10-01')), 
               date_labels = '%b-%y',
               breaks =  'year',  
               expand  = c(0,0)) +
  scale_y_continuous(limits = c(0,10), breaks = seq(0,10,by = 2), expand = c(0.01,0)) +
  theme_few() + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

Hamlin_Depletion_FJ_2001_plot = ggplot(data = Hamlin_Depletion_Daily, aes(x = Date, y = FJ_Diff_m3day/pumping_rate, color = factor(Dist2Str_m), shape = factor(Dist2Str_m), fill = factor(Dist2Str_m))) + 
  geom_rect(data = pumping_dates, inherit.aes = F, aes(xmin = start,xmax = end,ymin = ymin,ymax = ymax), fill = 'red', alpha = 0.2) +
  geom_line(size = 0.5) +
  #geom_point(size = 0.25) +
  #scale_shape_manual(values = c(21,22,23),breaks = c('270','1370','3340'), labels = c('270 m','1370 m','3340 m')) +
  scale_color_manual(values = c('#e41a1c', '#4daf4a','#377eb8'), breaks = c('270','1370','3340'), labels = c('270 m','1370 m','3340 m')) +
  #scale_fill_manual(values = c('#e41a1c', '#4daf4a','#377eb8'),breaks = c('270','1370','3340'), labels = c('270 m','1370 m','3340 m')) +
  ylab('SDR') +
  scale_x_date(limits = c(as.Date('2001-12-01'),as.Date('2001-12-31')), 
               date_labels = '%m/%d/%y',
               breaks =  seq(as.Date('2001-12-01'), as.Date('2002-01-01'),by = '5 days'),  
               expand  = c(0,0)) +
  scale_y_continuous(limits = c(0,10), breaks = seq(0,10,by = 2), expand = c(0.01,0)) +
  theme_few() + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

Hamlin_Accretion_FJ_all_years_plot = ggplot(data = Hamlin_Accretion_Daily, aes(x = Date, y = FJ_Diff_m3day/recharge_rate, color = factor(Dist2Str_m), shape = factor(Dist2Str_m), fill = factor(Dist2Str_m))) + 
  geom_rect(data = recharge_dates, inherit.aes = F, aes(xmin = start,xmax = end,ymin = ymin,ymax = ymax), fill = 'dodgerblue', alpha = 0.2) +
  geom_line(size = 0.5) +
  #geom_point(size = 0.25) +
  #scale_shape_manual(values = c(21,22,23),breaks = c('270','1370','3340'), labels = c('270 m','1370 m','3340 m')) +
  scale_color_manual(values = c('#e41a1c', '#4daf4a','#377eb8'), breaks = c('270','1370','3340'), labels = c('270 m','1370 m','3340 m')) +
  #scale_fill_manual(values = c('#e41a1c', '#4daf4a','#377eb8'),breaks = c('270','1370','3340'), labels = c('270 m','1370 m','3340 m')) +
  ylab('SAR') +
  scale_x_date(limits = c(as.Date('1990-10-01'),as.Date('2011-10-01')), 
               date_labels = '%b-%y',
               breaks =  'year',  
               expand  = c(0,0)) +
  scale_y_continuous(limits = c(0,12), breaks = seq(0,12,by = 3), expand = c(0.01,0)) +
  theme_few() + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

Hamlin_Accretion_FJ_2001_plot = ggplot(data = Hamlin_Accretion_Daily, aes(x = Date, y = FJ_Diff_m3day/recharge_rate, color = factor(Dist2Str_m), shape = factor(Dist2Str_m), fill = factor(Dist2Str_m))) + 
  geom_rect(data = recharge_dates, inherit.aes = F, aes(xmin = start,xmax = end,ymin = ymin,ymax = ymax), fill = 'dodgerblue', alpha = 0.2) +
  geom_line(size = 0.5) +
  #geom_point(size = 0.25) +
  #scale_shape_manual(values = c(21,22,23),breaks = c('270','1370','3340'), labels = c('270 m','1370 m','3340 m')) +
  scale_color_manual(values = c('#e41a1c', '#4daf4a','#377eb8'), breaks = c('270','1370','3340'), labels = c('270 m','1370 m','3340 m')) +
  #scale_fill_manual(values = c('#e41a1c', '#4daf4a','#377eb8'),breaks = c('270','1370','3340'), labels = c('270 m','1370 m','3340 m')) +
  ylab('SAR') +
  scale_x_date(limits = c(as.Date('2001-12-01'),as.Date('2001-12-31')), 
               date_labels = '%m/%d/%y',
               breaks =  seq(as.Date('2001-12-01'), as.Date('2002-01-01'),by = '5 days'),  
               expand  = c(0,0)) +
  scale_y_continuous(limits = c(0,12), breaks = seq(0,12,by = 3), expand = c(0.01,0)) +
  theme_few() + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

fig_name = paste0('FJ_Hamlin_all_years')                                                                           
if (fig_format == 'jpg'){                                                                                     
  jpeg(paste0(out_dir,fig_name,'.jpg'), width = fig_width, height = fig_height, units = fig_units,  res = 600)
} else if (fig_format == 'png'){                                                                              
  png(paste0(out_dir,fig_name,'.png'), width = 7.48, height = 4, units = 'in',  res = 600) 
} else if (fig_format == 'pdf'){                                                                              
  pdf(paste0(out_dir,fig_name,'.pdf'), width = 7, height = 8.5 )                                          
}                                                                                                             
grid.newpage()                                                                                                
pushViewport(viewport(layout = grid.layout(2,5)))                                                             
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)                                   
print(Hamlin_Depletion_FJ_all_years_plot +
        theme(legend.title = element_blank(),
              legend.position = c(0.9,0.75),
              legend.background = element_rect(fill=NA, color=NA),
              legend.key.height = unit(10,'pt'),
              axis.text.x = element_blank(),
              axis.title.y = element_text(size = 10),
              plot.margin = margin(t=10,b=10,l=5,r=5, unit = 'pt')),vp = vplayout(1,1:4))
print(Hamlin_Depletion_FJ_2001_plot +
        theme(legend.title = element_blank(),
              legend.position = 'none',
              legend.background = element_rect(fill=NA, color=NA),
              legend.key.height = unit(10,'pt'),
              axis.text = element_blank(),
              axis.title = element_blank(),
              plot.margin = margin(t=10,b=10,l=5,r=5, unit = 'pt')),vp = vplayout(1,5))
print(Hamlin_Accretion_FJ_all_years_plot +
        theme(legend.title = element_blank(),
              legend.position = 'none',
              axis.title.y = element_text(size = 10),
              axis.text.x = element_text(size = 6),
              plot.margin = margin(t=0,b=10,l=5,r=5, unit = 'pt')),vp = vplayout(2,1:4))
print(Hamlin_Accretion_FJ_2001_plot +
        theme(legend.title = element_blank(),
              legend.position = 'none',
              axis.title = element_blank(),
              axis.text.x = element_text(size = 6),
              axis.text.y = element_blank(),
              plot.margin = margin(t=0,b=6,l=5,r=5, unit = 'pt')),vp = vplayout(2,5))
graphics.off()

# First 5 Years -----------------------------------------------------
Hamlin_Depletion_5yrs_plot = ggplot(data = subset(Hamlin_Depletion_Daily,format(Date,'%Y')==seq(1991,1995)), aes(x = Date, y = FJ_Diff_m3day/pumping_rate, color = factor(Dist2Str_m), shape = factor(Dist2Str_m), fill = factor(Dist2Str_m))) + 
  geom_rect(data = pumping_dates, inherit.aes = F, aes(xmin = start,xmax = end,ymin = ymin,ymax = ymax), fill = 'red', alpha = 0.2) +
  geom_line(size = 0.25) +
  geom_point(size = 0.5) +
  geom_line(data = subset(Hamlin_Depletion_Monthly, Dist2Str_m == 270), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length'), size = 0.25) + 
  geom_point(data = subset(Hamlin_Depletion_Monthly, Dist2Str_m == 270), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length', shape = 'SR Dry Length', fill = 'SR Dry Length'),size = 0.5) + 
  scale_shape_manual(values = c(21,21,21,22),breaks = c('270','1370','3340','SR Dry Length'), labels = c('270 m','1370 m','3340 m','SR Dry Length')) +
  scale_color_manual(values = c('#e41a1c', '#4daf4a','#377eb8', 'black'), breaks = c('270','1370','3340','SR Dry Length'), labels = c('270 m','1370 m','3340 m','SR Dry Length')) +
  scale_fill_manual(values = c('#e41a1c', '#4daf4a','#377eb8', 'black'),breaks = c('270','1370','3340','SR Dry Length'), labels = c('270 m','1370 m','3340 m','SR Dry Length')) +
  ylab('SDR') +
  scale_x_date(limits = c(as.Date('1991-01-01'),as.Date('1996-01-01')), 
               date_labels = '%b-%y',
               date_breaks =  '6 months',  
               expand  = c(0,0)) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by = 0.25), expand = c(0.01,0),
                     sec.axis = sec_axis(trans = ~.,name = 'Fraction SR Dry', labels = derive())) +
  theme_few() + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

Hamlin_Accretion_5yrs_plot = ggplot(data = subset(Hamlin_Accretion_Daily,format(Date,'%Y')==seq(1991,1995)), aes(x = Date, y = FJ_Diff_m3day/recharge_rate, color = factor(Dist2Str_m), shape = factor(Dist2Str_m), fill = factor(Dist2Str_m))) + 
  geom_rect(data = recharge_dates, inherit.aes = F, aes(xmin = start,xmax = end,ymin = ymin,ymax = ymax), fill = 'dodgerblue', alpha = 0.2) +
  geom_line(size = 0.25) +
  geom_point(size = 0.5) +
  geom_line(data = subset(Hamlin_Accretion_Monthly, Dist2Str_m == 270), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length'),size = 0.25) + 
  geom_point(data = subset(Hamlin_Accretion_Monthly, Dist2Str_m == 270), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length', shape = 'SR Dry Length', fill = 'SR Dry Length'), size = 0.5) + 
  scale_shape_manual(values = c(21,21,21,22),breaks = c('270','1370','3340','SR Dry Length'), labels = c('270 m','1370 m','3340 m','SR Dry Length')) +
  scale_color_manual(values = c('#e41a1c', '#4daf4a','#377eb8', 'black'), breaks = c('270','1370','3340','SR Dry Length'), labels = c('270 m','1370 m','3340 m','SR Dry Length')) +
  scale_fill_manual(values = c('#e41a1c', '#4daf4a','#377eb8', 'black'),breaks = c('270','1370','3340','SR Dry Length'), labels = c('270 m','1370 m','3340 m','SR Dry Length')) +
  ylab('SAR') +
  scale_x_date(limits = c(as.Date('1991-01-01'),as.Date('1996-01-01')), 
               date_labels = '%b-%y',
               date_breaks =  '6 months',  
               expand  = c(0,0)) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by = 0.25), expand = c(0.01,0),
                     sec.axis = sec_axis(trans = ~.,name = 'Fraction SR Dry', labels = derive())) +
  theme_few() + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

fig_name = paste0('FJ_Hamlin_Dep_Acc_First_5_years')                                                                           
if (fig_format == 'jpg'){                                                                                     
  jpeg(paste0(out_dir,fig_name,'.jpg'), width = fig_width, height = fig_height, units = fig_units,  res = 600)
} else if (fig_format == 'png'){                                                                              
  png(paste0(out_dir,fig_name,'.png'), width = 7.48, height = 3, units = 'in',  res = 600) 
} else if (fig_format == 'pdf'){                                                                              
  pdf(paste0(out_dir,fig_name,'.pdf'), width = 7, height = 8.5 )                                          
}                                                                                                             
grid.newpage()                                                                                                
pushViewport(viewport(layout = grid.layout(2,1)))                                                             
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)                                   
print(Hamlin_Depletion_5yrs_plot +
        theme(legend.title = element_blank(),
              legend.text = element_text(size = 6),
              legend.position = c(0.5,1.1),
              legend.direction = 'horizontal',
              legend.background = element_rect(fill='white', color='black', size = 0.2),
              legend.margin = margin(t=1,b=1,l=1,r=1, unit = 'pt'),
              legend.key.height = unit(10,'pt'),
              axis.text.x = element_blank(),
              axis.text.y = element_text(size = 6),
              axis.title.y = element_text(size = 8),
              plot.margin = margin(t=20,b=10,l=5,r=5, unit = 'pt')),vp = vplayout(1,1))
print(Hamlin_Accretion_5yrs_plot +
        theme(legend.title = element_blank(),
              legend.position = 'none',
              axis.text = element_text(size = 6),
              axis.title.y = element_text(size = 8),
              plot.margin = margin(t=0,b=5,l=5,r=5, unit = 'pt'),
              plot.background = element_rect(fill = NA, color = NA)),vp = vplayout(2,1))
graphics.off()
# Dry Avg Wet Years -----------------------------------------------------
Hamlin_Depletion_Dry_plot = ggplot(data = subset(Hamlin_Depletion_Daily,format(Date,'%Y')==2001), aes(x = Date, y = FJ_Diff_m3day/pumping_rate, color = factor(Dist2Str_m), shape = factor(Dist2Str_m), fill = factor(Dist2Str_m))) + 
  geom_rect(data = pumping_dates, inherit.aes = F, aes(xmin = start,xmax = end,ymin = ymin,ymax = ymax), fill = 'red', alpha = 0.2) +
  geom_line(size = 0.25) +
  geom_point(size = 0.3) +
  geom_line(data = subset(Hamlin_Depletion_Monthly, Dist2Str_m == 270), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length'), size = 0.25) + 
  geom_point(data = subset(Hamlin_Depletion_Monthly, Dist2Str_m == 270), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length', shape = 'SR Dry Length', fill = 'SR Dry Length'),size = 0.5) + 
  scale_shape_manual(values = c(21,21,21,22),breaks = c('270','1370','3340','SR Dry Length'), labels = c('270 m','1370 m','3340 m','SR Dry Length')) +
  scale_color_manual(values = c('#e41a1c', '#4daf4a','#377eb8', 'black'), breaks = c('270','1370','3340','SR Dry Length'), labels = c('270 m','1370 m','3340 m','SR Dry Length')) +
  scale_fill_manual(values = c('#e41a1c', '#4daf4a','#377eb8', 'black'),breaks = c('270','1370','3340','SR Dry Length'), labels = c('270 m','1370 m','3340 m','SR Dry Length')) +
  ylab('SDR') +
  scale_x_date(limits = c(as.Date('2001-01-01'),as.Date('2002-01-01')), 
               date_labels = '%b-%y',
               date_breaks =  'months',  
               expand  = c(0,0)) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by = 0.25), expand = c(0.01,0),
                     sec.axis = sec_axis(trans = ~.,name = 'Fraction SR Dry', labels = derive())) +
  theme_few() + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

Hamlin_Depletion_Avg_plot = ggplot(data = subset(Hamlin_Depletion_Daily,format(Date,'%Y')==2010), aes(x = Date, y = FJ_Diff_m3day/pumping_rate, color = factor(Dist2Str_m), shape = factor(Dist2Str_m), fill = factor(Dist2Str_m))) + 
  geom_rect(data = pumping_dates, inherit.aes = F, aes(xmin = start,xmax = end,ymin = ymin,ymax = ymax), fill = 'red', alpha = 0.2) +
  geom_line(size = 0.25) +
  geom_point(size = 0.3) +
  geom_line(data = subset(Hamlin_Depletion_Monthly, Dist2Str_m == 270), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length'), size = 0.25) + 
  geom_point(data = subset(Hamlin_Depletion_Monthly, Dist2Str_m == 270), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length', shape = 'SR Dry Length', fill = 'SR Dry Length'),size = 0.5) + 
  scale_shape_manual(values = c(21,21,21,22),breaks = c('270','1370','3340','SR Dry Length'), labels = c('270 m','1370 m','3340 m','SR Dry Length')) +
  scale_color_manual(values = c('#e41a1c', '#4daf4a','#377eb8', 'black'), breaks = c('270','1370','3340','SR Dry Length'), labels = c('270 m','1370 m','3340 m','SR Dry Length')) +
  scale_fill_manual(values = c('#e41a1c', '#4daf4a','#377eb8', 'black'),breaks = c('270','1370','3340','SR Dry Length'), labels = c('270 m','1370 m','3340 m','SR Dry Length')) +
  ylab('SDR') +
  scale_x_date(limits = c(as.Date('2010-01-01'),as.Date('2011-01-01')), 
               date_labels = '%b-%y',
               date_breaks =  'months',  
               expand  = c(0,0)) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by = 0.25), expand = c(0.01,0),
                     sec.axis = sec_axis(trans = ~.,name = 'Fraction SR Dry', labels = derive())) +
  theme_few() + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

Hamlin_Depletion_Wet_plot = ggplot(data = subset(Hamlin_Depletion_Daily,format(Date,'%Y')==2003), aes(x = Date, y = FJ_Diff_m3day/pumping_rate, color = factor(Dist2Str_m), shape = factor(Dist2Str_m), fill = factor(Dist2Str_m))) + 
  geom_rect(data = pumping_dates, inherit.aes = F, aes(xmin = start,xmax = end,ymin = ymin,ymax = ymax), fill = 'red', alpha = 0.2) +
  geom_line(size = 0.25) +
  geom_point(size = 0.3) +
  geom_line(data = subset(Hamlin_Depletion_Monthly, Dist2Str_m == 270), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length'), size = 0.25) + 
  geom_point(data = subset(Hamlin_Depletion_Monthly, Dist2Str_m == 270), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length', shape = 'SR Dry Length', fill = 'SR Dry Length'),size = 0.5) + 
  scale_shape_manual(values = c(21,21,21,22),breaks = c('270','1370','3340','SR Dry Length'), labels = c('270 m','1370 m','3340 m','SR Dry Length')) +
  scale_color_manual(values = c('#e41a1c', '#4daf4a','#377eb8', 'black'), breaks = c('270','1370','3340','SR Dry Length'), labels = c('270 m','1370 m','3340 m','SR Dry Length')) +
  scale_fill_manual(values = c('#e41a1c', '#4daf4a','#377eb8', 'black'),breaks = c('270','1370','3340','SR Dry Length'), labels = c('270 m','1370 m','3340 m','SR Dry Length')) +
  ylab('SDR') +
  scale_x_date(limits = c(as.Date('2003-01-01'),as.Date('2004-01-01')), 
               date_labels = '%b-%y',
               date_breaks =  'months',  
               expand  = c(0,0)) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by = 0.25), expand = c(0.01,0),
                     sec.axis = sec_axis(trans = ~.,name = 'Fraction SR Dry', labels = derive())) +
  theme_few() + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

Hamlin_Accretion_Dry_plot = ggplot(data = subset(Hamlin_Accretion_Daily,format(Date,'%Y')==2001), aes(x = Date, y = FJ_Diff_m3day/recharge_rate, color = factor(Dist2Str_m), shape = factor(Dist2Str_m), fill = factor(Dist2Str_m))) + 
  geom_rect(data = recharge_dates, inherit.aes = F, aes(xmin = start,xmax = end,ymin = ymin,ymax = ymax), fill = 'dodgerblue', alpha = 0.2) +
  geom_line(size = 0.25) +
  geom_point(size = 0.3) +
  geom_line(data = subset(Hamlin_Accretion_Monthly, Dist2Str_m == 270), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length'), size = 0.25) + 
  geom_point(data = subset(Hamlin_Accretion_Monthly, Dist2Str_m == 270), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length', shape = 'SR Dry Length', fill = 'SR Dry Length'),size = 0.5) + 
  scale_shape_manual(values = c(21,21,21,22),breaks = c('270','1370','3340','SR Dry Length'), labels = c('270 m','1370 m','3340 m','SR Dry Length')) +
  scale_color_manual(values = c('#e41a1c', '#4daf4a','#377eb8', 'black'), breaks = c('270','1370','3340','SR Dry Length'), labels = c('270 m','1370 m','3340 m','SR Dry Length')) +
  scale_fill_manual(values = c('#e41a1c', '#4daf4a','#377eb8', 'black'),breaks = c('270','1370','3340','SR Dry Length'), labels = c('270 m','1370 m','3340 m','SR Dry Length')) +
  ylab('SAR') +
  scale_x_date(limits = c(as.Date('2001-01-01'),as.Date('2002-01-01')), 
               date_labels = '%b-%y',
               date_breaks =  'months',  
               expand  = c(0,0)) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by = 0.25), expand = c(0.01,0),
                     sec.axis = sec_axis(trans = ~.,name = 'Fraction SR Dry', labels = derive())) +
  theme_few() + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

Hamlin_Accretion_Avg_plot = ggplot(data = subset(Hamlin_Accretion_Daily,format(Date,'%Y')==2010), aes(x = Date, y = FJ_Diff_m3day/recharge_rate, color = factor(Dist2Str_m), shape = factor(Dist2Str_m), fill = factor(Dist2Str_m))) + 
  geom_rect(data = recharge_dates, inherit.aes = F, aes(xmin = start,xmax = end,ymin = ymin,ymax = ymax), fill = 'dodgerblue', alpha = 0.2) +
  geom_line(size = 0.25) +
  geom_point(size = 0.3) +
  geom_line(data = subset(Hamlin_Accretion_Monthly, Dist2Str_m == 270), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length'), size = 0.25) + 
  geom_point(data = subset(Hamlin_Accretion_Monthly, Dist2Str_m == 270), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length', shape = 'SR Dry Length', fill = 'SR Dry Length'),size = 0.5) + 
  scale_shape_manual(values = c(21,21,21,22),breaks = c('270','1370','3340','SR Dry Length'), labels = c('270 m','1370 m','3340 m','SR Dry Length')) +
  scale_color_manual(values = c('#e41a1c', '#4daf4a','#377eb8', 'black'), breaks = c('270','1370','3340','SR Dry Length'), labels = c('270 m','1370 m','3340 m','SR Dry Length')) +
  scale_fill_manual(values = c('#e41a1c', '#4daf4a','#377eb8', 'black'),breaks = c('270','1370','3340','SR Dry Length'), labels = c('270 m','1370 m','3340 m','SR Dry Length')) +
  ylab('SAR') +
  scale_x_date(limits = c(as.Date('2010-01-01'),as.Date('2011-01-01')), 
               date_labels = '%b-%y',
               date_breaks =  'months',  
               expand  = c(0,0)) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by = 0.25), expand = c(0.01,0),
                     sec.axis = sec_axis(trans = ~.,name = 'Fraction SR Dry', labels = derive())) +
  theme_few() + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

Hamlin_Accretion_Wet_plot = ggplot(data = subset(Hamlin_Accretion_Daily,format(Date,'%Y')==2003), aes(x = Date, y = FJ_Diff_m3day/recharge_rate, color = factor(Dist2Str_m), shape = factor(Dist2Str_m), fill = factor(Dist2Str_m))) + 
  geom_rect(data = recharge_dates, inherit.aes = F, aes(xmin = start,xmax = end,ymin = ymin,ymax = ymax), fill = 'dodgerblue', alpha = 0.2) +
  geom_line(size = 0.25) +
  geom_point(size = 0.3) +
  geom_line(data = subset(Hamlin_Accretion_Monthly, Dist2Str_m == 270), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length'), size = 0.25) + 
  geom_point(data = subset(Hamlin_Accretion_Monthly, Dist2Str_m == 270), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length', shape = 'SR Dry Length', fill = 'SR Dry Length'),size = 0.5) + 
  scale_shape_manual(values = c(21,21,21,22),breaks = c('270','1370','3340','SR Dry Length'), labels = c('270 m','1370 m','3340 m','SR Dry Length')) +
  scale_color_manual(values = c('#e41a1c', '#4daf4a','#377eb8', 'black'), breaks = c('270','1370','3340','SR Dry Length'), labels = c('270 m','1370 m','3340 m','SR Dry Length')) +
  scale_fill_manual(values = c('#e41a1c', '#4daf4a','#377eb8', 'black'),breaks = c('270','1370','3340','SR Dry Length'), labels = c('270 m','1370 m','3340 m','SR Dry Length')) +
  ylab('SAR') +
  scale_x_date(limits = c(as.Date('2003-01-01'),as.Date('2004-01-01')), 
               date_labels = '%b-%y',
               date_breaks =  'months',  
               expand  = c(0,0)) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by = 0.25), expand = c(0.01,0),
                     sec.axis = sec_axis(trans = ~.,name = 'Fraction SR Dry', labels = derive())) +
  theme_few() + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

fig_name = paste0('FJ_Hamlin_Dep_Acc_Dry_Avg_Wet_years')                                                                           
if (fig_format == 'jpg'){                                                                                     
  jpeg(paste0(out_dir,fig_name,'.jpg'), width = fig_width, height = fig_height, units = fig_units,  res = 600)
} else if (fig_format == 'png'){                                                                              
  png(paste0(out_dir,fig_name,'.png'), width = 6.5, height = 2.75, units = 'in',  res = 600) 
} else if (fig_format == 'pdf'){                                                                              
  pdf(paste0(out_dir,fig_name,'.pdf'), width = 7, height = 8.5 )                                          
}                                                                                                             
grid.newpage()                                                                                                
pushViewport(viewport(layout = grid.layout(2,3)))                                                             
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)                                   
print(Hamlin_Depletion_Dry_plot +
        theme(legend.title = element_blank(),
              legend.text = element_text(size = 6),
              legend.position = 'none',
              legend.background = element_rect(fill='white', color='black', size = 0.2),
              legend.margin = margin(t=1,b=1,l=1,r=1, unit = 'pt'),
              legend.key.height = unit(10,'pt'),
              axis.text = element_blank(),
              axis.title = element_blank()),vp = vplayout(1,1))
print(Hamlin_Depletion_Avg_plot +
        theme(legend.title = element_blank(),
              legend.text = element_text(size = 6),
              legend.position = 'none',
              legend.key.width = unit(15,'pt'),
              legend.key.height = unit(10,'pt'),
              axis.text = element_blank(),
              axis.title = element_blank()),vp = vplayout(1,2))
print(Hamlin_Depletion_Wet_plot +
        theme(legend.title = element_blank(),
              legend.position = 'none',
              axis.title.y = element_blank(),
              axis.text = element_blank()),vp = vplayout(1,3))
print(Hamlin_Accretion_Dry_plot +
        theme(legend.title = element_blank(),
              legend.text = element_text(size = 6),
              legend.position = c(0.75,0.4),
              legend.background = element_rect(fill=NA, color = NA),
              legend.margin = margin(t=1,b=1,l=1,r=1, unit = 'pt'),
              legend.key.size = unit(10,'pt'),
              axis.text = element_blank(),
              axis.title = element_blank()),vp = vplayout(2,1))
print(Hamlin_Accretion_Avg_plot +
        theme(legend.title = element_blank(),
              legend.text = element_text(size = 6),
              legend.position = 'none',
              legend.background = element_rect(fill='white', color='black', size = 0.2),
              legend.margin = margin(t=1,b=1,l=1,r=1, unit = 'pt'),
              legend.key.height = unit(10,'pt'),
              axis.text = element_blank(),
              axis.title = element_blank()),vp = vplayout(2,2))
print(Hamlin_Accretion_Wet_plot +
        theme(legend.title = element_blank(),
              legend.position = 'none',
              axis.text = element_blank(),
              axis.title = element_blank()),vp = vplayout(2,3))
graphics.off()
# Monthly Flux Plots Hamlin 1370
test = subset(Monthly_Depletion_Hamlin_1370m, select = c('Month', 'delta_S','delta_ET','delta_Drns','delta_GWSW'))
test$Month = as.Date(paste0(test$Month,'-01'),'%b-%Y-%d')
test$delta_S = test$delta_S/pumping_rate + 1
test$delta_GWSW = test$delta_GWSW/-pumping_rate
test$delta_Drns = test$delta_Drns/pumping_rate
test$delta_ET = test$delta_ET/pumping_rate

test_melt = melt(test, id.vars = 'Month')

ggplot(subset(test_melt, format(Month,'%Y')==seq(1991,1995)), aes(x = Month, y = value, color = variable)) + geom_line() + geom_point()
# Avg Dry Wet Gages Hamlin 1370 All Gages ---------------------------------
All_Gages_Hamlin_1370_Depletion_melt = melt(subset(Hamlin_Depletion_Daily,Dist2Str_m==1370 & Year==c(2001,2003,2010), select = c('Date','FJ_Diff_m3day',paste0('Pred_Loc_',seq(2,4),'_Diff_m3day'))), id.vars = 'Date')
All_Gages_Hamlin_1370_Accretion_melt = melt(subset(Hamlin_Accretion_Daily,Dist2Str_m==1370 & Year==c(2001,2003,2010), select = c('Date','FJ_Diff_m3day',paste0('Pred_Loc_',seq(2,4),'_Diff_m3day'))), id.vars = 'Date')


Hamlin_1370_depletion_dry_year_plot = ggplot(data = subset(All_Gages_Hamlin_1370_Depletion_melt,format(Date,'%Y')==2001), aes(x = Date, y = value/pumping_rate, color = variable, shape = variable, fill = variable)) + 
  geom_rect(data = pumping_dates, inherit.aes = F, aes(xmin = start,xmax = end,ymin = ymin,ymax = ymax), fill = 'red', alpha = 0.2) +
  geom_line(size = 0.25) +
  geom_point(size = 0.5) +
  geom_line(data = subset(Hamlin_Depletion_Monthly, Dist2Str_m == 1370), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length'), size = 0.25) + 
  geom_point(data = subset(Hamlin_Depletion_Monthly, Dist2Str_m == 1370), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length', shape = 'SR Dry Length', fill = 'SR Dry Length'),size = 0.5) + 
  scale_shape_manual(values = c(21,21,21,21,22),breaks = c('FJ_Diff_m3day', 'Pred_Loc_2_Diff_m3day', 'Pred_Loc_3_Diff_m3day', 'Pred_Loc_4_Diff_m3day','SR Dry Length'), labels = c('River km 37','River km 51','River km 66','River km 76','SR Dry Length')) +
  scale_color_manual(values = c('#1b9e77', '#d95f02','#7570b3','#e7298a', 'black'), breaks = c('FJ_Diff_m3day', 'Pred_Loc_2_Diff_m3day', 'Pred_Loc_3_Diff_m3day', 'Pred_Loc_4_Diff_m3day','SR Dry Length'), labels = c('River km 37','River km 51','River km 66','River km 76','SR Dry Length')) +
  scale_fill_manual(values = c('#1b9e77', '#d95f02','#7570b3','#e7298a', 'black'),breaks = c('FJ_Diff_m3day', 'Pred_Loc_2_Diff_m3day', 'Pred_Loc_3_Diff_m3day', 'Pred_Loc_4_Diff_m3day','SR Dry Length'), labels = c('River km 37','River km 51','River km 66','River km 76','SR Dry Length')) +
  ylab('SDR') +
  scale_x_date(limits = c(as.Date('2001-01-01'),as.Date('2002-01-01')), 
               date_labels = '%b-%y',
               date_breaks =  'months',  
               expand  = c(0,0)) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by = 0.25), expand = c(0.01,0),
                     sec.axis = sec_axis(trans = ~.,name = 'Fraction SR Dry', labels = derive())) +
  theme_few() + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

Hamlin_1370_depletion_avg_year_plot = ggplot(data = subset(All_Gages_Hamlin_1370_Depletion_melt,format(Date,'%Y')==2010), aes(x = Date, y = value/pumping_rate, color = variable, shape = variable, fill = variable)) + 
  geom_rect(data = pumping_dates, inherit.aes = F, aes(xmin = start,xmax = end,ymin = ymin,ymax = ymax), fill = 'red', alpha = 0.2) +
  geom_line(size = 0.25) +
  geom_point(size = 0.5) +
  geom_line(data = subset(Hamlin_Depletion_Monthly, Dist2Str_m == 1370), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length'), size = 0.25) + 
  geom_point(data = subset(Hamlin_Depletion_Monthly, Dist2Str_m == 1370), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length', shape = 'SR Dry Length', fill = 'SR Dry Length'),size = 0.5) + 
  scale_shape_manual(values = c(21,21,21,21,22),breaks = c('FJ_Diff_m3day', 'Pred_Loc_2_Diff_m3day', 'Pred_Loc_3_Diff_m3day', 'Pred_Loc_4_Diff_m3day','SR Dry Length'), labels = c('River km 37','River km 51','River km 66','River km 76','SR Dry Length')) +
  scale_color_manual(values = c('#1b9e77', '#d95f02','#7570b3','#e7298a', 'black'), breaks = c('FJ_Diff_m3day', 'Pred_Loc_2_Diff_m3day', 'Pred_Loc_3_Diff_m3day', 'Pred_Loc_4_Diff_m3day','SR Dry Length'), labels = c('River km 37','River km 51','River km 66','River km 76','SR Dry Length')) +
  scale_fill_manual(values = c('#1b9e77', '#d95f02','#7570b3','#e7298a', 'black'),breaks = c('FJ_Diff_m3day', 'Pred_Loc_2_Diff_m3day', 'Pred_Loc_3_Diff_m3day', 'Pred_Loc_4_Diff_m3day','SR Dry Length'), labels = c('River km 37','River km 51','River km 66','River km 76','SR Dry Length')) +
  ylab('SDR') +
  scale_x_date(limits = c(as.Date('2010-01-01'),as.Date('2011-01-01')), 
               date_labels = '%b-%y',
               date_breaks =  'months',  
               expand  = c(0,0)) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by = 0.25), expand = c(0.01,0),
                     sec.axis = sec_axis(trans = ~.,name = 'Fraction SR Dry', labels = derive())) +
  theme_few() + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

Hamlin_1370_depletion_wet_year_plot = ggplot(data = subset(All_Gages_Hamlin_1370_Depletion_melt,format(Date,'%Y')==2003), aes(x = Date, y = value/pumping_rate, color = variable, shape = variable, fill = variable)) + 
  geom_rect(data = pumping_dates, inherit.aes = F, aes(xmin = start,xmax = end,ymin = ymin,ymax = ymax), fill = 'red', alpha = 0.2) +
  geom_line(size = 0.25) +
  geom_point(size = 0.5) +
  geom_line(data = subset(Hamlin_Depletion_Monthly, Dist2Str_m == 1370), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length'), size = 0.25) + 
  geom_point(data = subset(Hamlin_Depletion_Monthly, Dist2Str_m == 1370), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length', shape = 'SR Dry Length', fill = 'SR Dry Length'),size = 0.5) + 
  scale_shape_manual(values = c(21,21,21,21,22),breaks = c('FJ_Diff_m3day', 'Pred_Loc_2_Diff_m3day', 'Pred_Loc_3_Diff_m3day', 'Pred_Loc_4_Diff_m3day','SR Dry Length'), labels = c('River km 37','River km 51','River km 66','River km 76','SR Dry Length')) +
  scale_color_manual(values = c('#1b9e77', '#d95f02','#7570b3','#e7298a', 'black'), breaks = c('FJ_Diff_m3day', 'Pred_Loc_2_Diff_m3day', 'Pred_Loc_3_Diff_m3day', 'Pred_Loc_4_Diff_m3day','SR Dry Length'), labels = c('River km 37','River km 51','River km 66','River km 76','SR Dry Length')) +
  scale_fill_manual(values = c('#1b9e77', '#d95f02','#7570b3','#e7298a', 'black'),breaks = c('FJ_Diff_m3day', 'Pred_Loc_2_Diff_m3day', 'Pred_Loc_3_Diff_m3day', 'Pred_Loc_4_Diff_m3day','SR Dry Length'), labels = c('River km 37','River km 51','River km 66','River km 76','SR Dry Length')) +
  ylab('SDR') +
  scale_x_date(limits = c(as.Date('2003-01-01'),as.Date('2004-01-01')), 
               date_labels = '%b-%y',
               date_breaks =  'months',  
               expand  = c(0,0)) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by = 0.25), expand = c(0.01,0),
                     sec.axis = sec_axis(trans = ~.,name = 'Fraction SR Dry', labels = derive())) +
  theme_few() + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

Hamlin_1370_accretion_dry_year_plot = ggplot(data = subset(All_Gages_Hamlin_1370_Accretion_melt,format(Date,'%Y')==2001), aes(x = Date, y = value/recharge_rate, color = variable, shape = variable, fill = variable)) + 
  geom_rect(data = recharge_dates, inherit.aes = F, aes(xmin = start,xmax = end,ymin = ymin,ymax = ymax), fill = 'dodgerblue', alpha = 0.2) +
  geom_line(size = 0.25) +
  geom_point(size = 0.5) +
  geom_line(data = subset(Hamlin_Accretion_Monthly, Dist2Str_m == 1370), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length'), size = 0.25) + 
  geom_point(data = subset(Hamlin_Accretion_Monthly, Dist2Str_m == 1370), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length', shape = 'SR Dry Length', fill = 'SR Dry Length'),size = 0.5) + 
  scale_shape_manual(values = c(21,21,21,21,22),breaks = c('FJ_Diff_m3day', 'Pred_Loc_2_Diff_m3day', 'Pred_Loc_3_Diff_m3day', 'Pred_Loc_4_Diff_m3day','SR Dry Length'), labels = c('River km 37','River km 51','River km 66','River km 76','SR Dry Length')) +
  scale_color_manual(values = c('#1b9e77', '#d95f02','#7570b3','#e7298a', 'black'), breaks = c('FJ_Diff_m3day', 'Pred_Loc_2_Diff_m3day', 'Pred_Loc_3_Diff_m3day', 'Pred_Loc_4_Diff_m3day','SR Dry Length'), labels = c('River km 37','River km 51','River km 66','River km 76','SR Dry Length')) +
  scale_fill_manual(values = c('#1b9e77', '#d95f02','#7570b3','#e7298a', 'black'),breaks = c('FJ_Diff_m3day', 'Pred_Loc_2_Diff_m3day', 'Pred_Loc_3_Diff_m3day', 'Pred_Loc_4_Diff_m3day','SR Dry Length'), labels = c('River km 37','River km 51','River km 66','River km 76','SR Dry Length')) +
  ylab('SDR') +
  scale_x_date(limits = c(as.Date('2001-01-01'),as.Date('2002-01-01')), 
               date_labels = '%b-%y',
               date_breaks =  'months',  
               expand  = c(0,0)) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by = 0.25), expand = c(0.01,0),
                     sec.axis = sec_axis(trans = ~.,name = 'Fraction SR Dry', labels = derive())) +
  theme_few() + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

Hamlin_1370_accretion_avg_year_plot = ggplot(data = subset(All_Gages_Hamlin_1370_Accretion_melt,format(Date,'%Y')==2010), aes(x = Date, y = value/recharge_rate, color = variable, shape = variable, fill = variable)) + 
  geom_rect(data = recharge_dates, inherit.aes = F, aes(xmin = start,xmax = end,ymin = ymin,ymax = ymax), fill = 'dodgerblue', alpha = 0.2) +
  geom_line(size = 0.25) +
  geom_point(size = 0.5) +
  geom_line(data = subset(Hamlin_Accretion_Monthly, Dist2Str_m == 1370), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length'), size = 0.25) + 
  geom_point(data = subset(Hamlin_Accretion_Monthly, Dist2Str_m == 1370), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length', shape = 'SR Dry Length', fill = 'SR Dry Length'),size = 0.5) + 
  scale_shape_manual(values = c(21,21,21,21,22),breaks = c('FJ_Diff_m3day', 'Pred_Loc_2_Diff_m3day', 'Pred_Loc_3_Diff_m3day', 'Pred_Loc_4_Diff_m3day','SR Dry Length'), labels = c('River km 37','River km 51','River km 66','River km 76','SR Dry Length')) +
  scale_color_manual(values = c('#1b9e77', '#d95f02','#7570b3','#e7298a', 'black'), breaks = c('FJ_Diff_m3day', 'Pred_Loc_2_Diff_m3day', 'Pred_Loc_3_Diff_m3day', 'Pred_Loc_4_Diff_m3day','SR Dry Length'), labels = c('River km 37','River km 51','River km 66','River km 76','SR Dry Length')) +
  scale_fill_manual(values = c('#1b9e77', '#d95f02','#7570b3','#e7298a', 'black'),breaks = c('FJ_Diff_m3day', 'Pred_Loc_2_Diff_m3day', 'Pred_Loc_3_Diff_m3day', 'Pred_Loc_4_Diff_m3day','SR Dry Length'), labels = c('River km 37','River km 51','River km 66','River km 76','SR Dry Length')) +
  ylab('SDR') +
  scale_x_date(limits = c(as.Date('2010-01-01'),as.Date('2011-01-01')), 
               date_labels = '%b-%y',
               date_breaks =  'months',  
               expand  = c(0,0)) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by = 0.25), expand = c(0.01,0),
                     sec.axis = sec_axis(trans = ~.,name = 'Fraction SR Dry', labels = derive())) +
  theme_few() + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

Hamlin_1370_accretion_wet_year_plot = ggplot(data = subset(All_Gages_Hamlin_1370_Accretion_melt,format(Date,'%Y')==2003), aes(x = Date, y = value/recharge_rate, color = variable, shape = variable, fill = variable)) + 
  geom_rect(data = recharge_dates, inherit.aes = F, aes(xmin = start,xmax = end,ymin = ymin,ymax = ymax), fill = 'dodgerblue', alpha = 0.2) +
  geom_line(size = 0.25) +
  geom_point(size = 0.5) +
  geom_line(data = subset(Hamlin_Accretion_Monthly, Dist2Str_m == 1370), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length'), size = 0.25) + 
  geom_point(data = subset(Hamlin_Accretion_Monthly, Dist2Str_m == 1370), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length', shape = 'SR Dry Length', fill = 'SR Dry Length'),size = 0.5) + 
  scale_shape_manual(values = c(21,21,21,21,22),breaks = c('FJ_Diff_m3day', 'Pred_Loc_2_Diff_m3day', 'Pred_Loc_3_Diff_m3day', 'Pred_Loc_4_Diff_m3day','SR Dry Length'), labels = c('River km 37','River km 51','River km 66','River km 76','SR Dry Length')) +
  scale_color_manual(values = c('#1b9e77', '#d95f02','#7570b3','#e7298a', 'black'), breaks = c('FJ_Diff_m3day', 'Pred_Loc_2_Diff_m3day', 'Pred_Loc_3_Diff_m3day', 'Pred_Loc_4_Diff_m3day','SR Dry Length'), labels = c('River km 37','River km 51','River km 66','River km 76','SR Dry Length')) +
  scale_fill_manual(values = c('#1b9e77', '#d95f02','#7570b3','#e7298a', 'black'),breaks = c('FJ_Diff_m3day', 'Pred_Loc_2_Diff_m3day', 'Pred_Loc_3_Diff_m3day', 'Pred_Loc_4_Diff_m3day','SR Dry Length'), labels = c('River km 37','River km 51','River km 66','River km 76','SR Dry Length')) +
  ylab('SDR') +
  scale_x_date(limits = c(as.Date('2003-01-01'),as.Date('2004-01-01')), 
               date_labels = '%b-%y',
               date_breaks =  'months',  
               expand  = c(0,0)) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by = 0.25), expand = c(0.01,0),
                     sec.axis = sec_axis(trans = ~.,name = 'Fraction SR Dry', labels = derive())) +
  theme_few() + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

fig_name = paste0('Hamlin_1370_Dep_Acc_Dry_Avg_Wet_yearslegend')                                                                           
if (fig_format == 'jpg'){                                                                                     
  jpeg(paste0(out_dir,fig_name,'.jpg'), width = fig_width, height = fig_height, units = fig_units,  res = 600)
} else if (fig_format == 'png'){                                                                              
  png(paste0(out_dir,fig_name,'.png'), width = 6.5, height = 2.75, units = 'in',  res = 600) 
} else if (fig_format == 'pdf'){                                                                              
  pdf(paste0(out_dir,fig_name,'.pdf'), width = 7, height = 8.5 )                                          
}                                                                                                             
grid.newpage()                                                                                                
pushViewport(viewport(layout = grid.layout(2,3)))                                                             
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)                                   
print(Hamlin_1370_depletion_dry_year_plot +
        theme(legend.title = element_blank(),
              legend.text = element_text(size = 6),
              legend.position = 'none',
              legend.background = element_rect(fill='white', color='black', size = 0.2),
              legend.margin = margin(t=1,b=1,l=1,r=1, unit = 'pt'),
              legend.key.height = unit(10,'pt'),
              axis.text = element_blank(),
              axis.title = element_blank()),vp = vplayout(1,1))
print(Hamlin_1370_depletion_avg_year_plot +
        theme(legend.title = element_blank(),
              legend.text = element_text(size = 6),
              legend.position = 'none',
              legend.key.width = unit(15,'pt'),
              legend.key.height = unit(10,'pt'),
              axis.text = element_blank(),
              axis.title = element_blank()),vp = vplayout(1,2))
print(Hamlin_1370_depletion_wet_year_plot +
        theme(legend.title = element_blank(),
              legend.position = 'none',
              axis.title.y = element_blank(),
              axis.text = element_blank()),vp = vplayout(1,3))
# print(Hamlin_1370_accretion_dry_year_plot +
#         theme(legend.title = element_blank(),
#               legend.text = element_text(size = 6),
#               legend.position = 'none',
#               legend.background = element_rect(fill=NA, color = NA),
#               legend.margin = margin(t=1,b=1,l=1,r=1, unit = 'pt'),
#               legend.key.size = unit(10,'pt'),
#               axis.text = element_blank(),
#               axis.title = element_blank()),vp = vplayout(2,1))
print(Hamlin_1370_accretion_avg_year_plot +
        theme(legend.title = element_blank(),
              legend.text = element_text(size = 6),
              legend.direction = 'horizontal',
              legend.position = c(0.5,0.5),
              legend.background = element_rect(fill='white', color='black', size = 0.2),
              legend.margin = margin(t=1,b=1,l=1,r=1, unit = 'pt'),
              legend.key.height = unit(10,'pt'),
              axis.text = element_blank(),
              axis.title = element_blank()),vp = vplayout(2,2))
# print(Hamlin_1370_accretion_wet_year_plot +
#         theme(legend.title = element_blank(),
#               legend.position = 'none',
#               axis.text = element_blank(),
#               axis.title = element_blank()),vp = vplayout(2,3))
graphics.off()

# Source Contribution -----------------------------------------------------
Source_fraction = as.data.frame(apply(X = subset(Monthly_Depletion_Hamlin_270m, select=c('delta_S', 'delta_pumping', 'delta_ET', 'delta_Drns', 'delta_GWSW')),MARGIN = 2, FUN = function(x) cumsum(na.omit(x))))
Source_fraction$Month = as.Date(paste0(Monthly_Depletion_Hamlin_270m$Month[which(!is.na(Monthly_Depletion_Hamlin_270m$delta_S))],'-01'),'%b-%Y-%d')
Source_fraction$delta_S_frac = Source_fraction$delta_S/-Source_fraction$delta_pumping
Source_fraction$delta_ET_frac = Source_fraction$delta_ET/-Source_fraction$delta_pumping
Source_fraction$delta_Drns_frac = Source_fraction$delta_Drns/-Source_fraction$delta_pumping
Source_fraction$delta_GWSW_frac = Source_fraction$delta_GWSW/-Source_fraction$delta_pumping
Source_fraction$MB = Source_fraction$delta_S_frac + Source_fraction$delta_ET_frac + Source_fraction$delta_Drns_frac + Source_fraction$delta_GWSW_frac
test = melt(subset(Source_fraction, select = c('Month', 'delta_S_frac', 'delta_ET_frac', 'delta_Drns_frac', 'delta_GWSW_frac')),id.vars = 'Month')
ggplot(test, aes(x = Month, y= value, color = variable)) + geom_line() + geom_point()

Source_fraction = subset(Monthly_Depletion_Hamlin_270m, select=c('delta_S', 'delta_pumping', 'delta_ET', 'delta_Drns', 'delta_GWSW'))
Source_fraction$Month = as.Date(paste0(Monthly_Depletion_Hamlin_270m$Month,'-01'),'%b-%Y-%d')
Source_fraction$delta_pumping[which(Source_fraction$delta_pumping>-1300)] = 0
Source_fraction$delta_S_frac = Source_fraction$delta_S/-Source_fraction$delta_pumping
Source_fraction$delta_ET_frac = Source_fraction$delta_ET/-Source_fraction$delta_pumping
Source_fraction$delta_Drns_frac = Source_fraction$delta_Drns/-Source_fraction$delta_pumping
Source_fraction$delta_GWSW_frac = Source_fraction$delta_GWSW/-Source_fraction$delta_pumping
Source_fraction$MB = Source_fraction$delta_S_frac + Source_fraction$delta_ET_frac + Source_fraction$delta_Drns_frac + Source_fraction$delta_GWSW_frac
test = melt(subset(Source_fraction, select = c('Month', 'delta_S_frac', 'delta_ET_frac', 'delta_Drns_frac', 'delta_GWSW_frac')),id.vars = 'Month')
ggplot(test, aes(x = Month, y= value, color = variable)) + geom_line() + geom_point()
# DOY Plot for 1, 5, 10, 15, and 20 years ----------------------------------------------------------------
Hamlin_FJ_1_5_10_15_20_yr_plot = ggplot(data = subset(Hamlin_Daily,format(Date,'%Y')%in%c(1995,2002,2006,2007) & Dist2Str_m == '3340'), aes(x = format(Date,'%j'), y = FJ_Diff_m3day/pumping_rate, color = format(Date,'%Y'), shape = format(Date,'%Y'), fill = format(Date,'%Y'), group = format(Date,'%Y'))) +
  geom_rect(data = pumping_dates, inherit.aes = F, aes(xmin = format(start,'%j'),xmax = format(end,'%j'),ymin = ymin,ymax = ymax), fill = 'gray90') +
  geom_line() +
  geom_point() +
  scale_color_brewer(type = 'qual', palette = 'Set1', breaks = c(1995,2002,2006,2007), labels = c('Dry','Wet','Dry','Wet')) +
  scale_fill_brewer(type = 'qual', palette = 'Set1', breaks = c(1995,2002,2006,2007), labels = c('Dry','Wet','Dry','Wet')) +
  ylab('FJ Gage Streamflow Depletion or \nScott River Dry Length Fraction') +
  # scale_x_date(limits = c(as.Date('1991-01-01'),as.Date('1993-01-01')),
  #              date_labels = '%b-%y',
  #              date_breaks =  'months',
  #              expand  = c(0,0)) +
  #scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by = 0.25), expand = c(0.01,0.01)) +
  theme_few() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

Hamlin_FJ_Dry_Year_plot = ggplot(data = subset(Hamlin_Daily,format(Date,'%Y')==c(2001,2002)), aes(x = Date, y = FJ_Diff_m3day/pumping_rate, color = factor(Dist2Str_m), shape = factor(Dist2Str_m), fill = factor(Dist2Str_m))) + 
  geom_rect(data = pumping_dates, inherit.aes = F, aes(xmin = start,xmax = end,ymin = ymin,ymax = ymax), fill = 'gray90') +
  geom_line() +
  geom_point() +
  geom_line(data = subset(Hamlin_Monthly, Dist2Str_m == 270), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length')) + 
  geom_point(data = subset(Hamlin_Monthly, Dist2Str_m == 270), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length', shape = 'SR Dry Length', fill = 'SR Dry Length')) + 
  scale_shape_manual(values = c(21,21,21,22),breaks = c('270','1370','3340','SR Dry Length'), labels = c('270 m','1370 m','3340 m','SR Dry Length')) +
  scale_color_manual(values = c('#e41a1c', '#4daf4a','#377eb8', 'black'), breaks = c('270','1370','3340','SR Dry Length'), labels = c('270 m','1370 m','3340 m','SR Dry Length')) +
  scale_fill_manual(values = c('#e41a1c', '#4daf4a','#377eb8', 'black'),breaks = c('270','1370','3340','SR Dry Length'), labels = c('270 m','1370 m','3340 m','SR Dry Length')) +
  ylab('FJ Gage Streamflow Depletion or \nScott River Dry Length Fraction') +
  scale_x_date(limits = c(as.Date('2001-01-01'),as.Date('2003-01-01')), 
               date_labels = '%b-%y',
               date_breaks =  'months',  
               expand  = c(0,0)) +
  #scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by = 0.25), expand = c(0.01,0.01)) +
  theme_few() + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

West_Side_FJ_Dry_Year_plot = ggplot(data = subset(West_Side_Daily,format(Date,'%Y')==c(2001,2002)), aes(x = Date, y = FJ_Diff_m3day/pumping_rate, color = factor(Dist2Str_m), shape = factor(Dist2Str_m), fill = factor(Dist2Str_m))) + 
  geom_rect(data = pumping_dates, inherit.aes = F, aes(xmin = start,xmax = end,ymin = ymin,ymax = ymax), fill = 'gray90') +
  geom_line() +
  geom_point() +
  geom_line(data = subset(West_Side_Monthly, Dist2Str_m == 500), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length')) + 
  geom_point(data = subset(West_Side_Monthly, Dist2Str_m == 500), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length', shape = 'SR Dry Length', fill = 'SR Dry Length')) + 
  scale_shape_manual(values = c(21,21,21,22), breaks = c('500','2900','5700','SR Dry Length'), labels = c('500 m','2900 m','5700 m','SR Dry Length')) +
  scale_color_manual(values = c('#e41a1c', '#4daf4a','#377eb8', 'black'), breaks = c('500','2900','5700','SR Dry Length'), labels = c('500 m','2900 m','5700 m','SR Dry Length')) +
  scale_fill_manual(values = c('#e41a1c', '#4daf4a','#377eb8', 'black'), breaks = c('500','2900','5700','SR Dry Length'), labels = c('500 m','2900 m','5700 m','SR Dry Length')) +
  ylab('FJ Gage Streamflow Depletion or \nScott River Dry Length Fraction') +
  scale_x_date(limits = c(as.Date('2001-01-01'),as.Date('2003-01-01')), 
               date_labels = '%b-%y',
               date_breaks =  'months',  
               expand  = c(0,0)) +
  #scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by = 0.25), expand = c(0.01,0.01)) +
  theme_few() + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

Hamlin_FJ_Avg_Year_plot = ggplot(data = subset(Hamlin_Daily,format(Date,'%Y')==c(2010,2011)), aes(x = Date, y = FJ_Diff_m3day/pumping_rate, color = factor(Dist2Str_m), shape = factor(Dist2Str_m), fill = factor(Dist2Str_m))) + 
  geom_rect(data = pumping_dates, inherit.aes = F, aes(xmin = start,xmax = end,ymin = ymin,ymax = ymax), fill = 'gray90') +
  geom_line() +
  geom_point() +
  geom_line(data = subset(Hamlin_Monthly, Dist2Str_m == 270), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length')) + 
  geom_point(data = subset(Hamlin_Monthly, Dist2Str_m == 270), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length', shape = 'SR Dry Length', fill = 'SR Dry Length')) + 
  scale_shape_manual(values = c(21,21,21,22),breaks = c('270','1370','3340','SR Dry Length'), labels = c('270 m','1370 m','3340 m','SR Dry Length')) +
  scale_color_manual(values = c('#e41a1c', '#4daf4a','#377eb8', 'black'), breaks = c('270','1370','3340','SR Dry Length'), labels = c('270 m','1370 m','3340 m','SR Dry Length')) +
  scale_fill_manual(values = c('#e41a1c', '#4daf4a','#377eb8', 'black'), breaks = c('270','1370','3340','SR Dry Length'), labels = c('270 m','1370 m','3340 m','SR Dry Length')) +
  ylab('FJ Gage Streamflow Depletion or \nScott River Dry Length Fraction') +
  scale_x_date(limits = c(as.Date('2010-01-01'),as.Date('2012-01-01')), 
               date_labels = '%b-%y',
               date_breaks =  'months',  
               expand  = c(0,0)) +
  #scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by = 0.25), expand = c(0.01,0.01)) +
  theme_few() + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

West_Side_Avg_Dry_Year_plot = ggplot(data = subset(West_Side_Daily,format(Date,'%Y')==c(2010,2011)), aes(x = Date, y = FJ_Diff_m3day/pumping_rate, color = factor(Dist2Str_m), shape = factor(Dist2Str_m), fill = factor(Dist2Str_m))) + 
  geom_rect(data = pumping_dates, inherit.aes = F, aes(xmin = start,xmax = end,ymin = ymin,ymax = ymax), fill = 'gray90') +
  geom_line() +
  geom_point() +
  geom_line(data = subset(West_Side_Monthly, Dist2Str_m == 500), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length')) + 
  geom_point(data = subset(West_Side_Monthly, Dist2Str_m == 500), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length', shape = 'SR Dry Length', fill = 'SR Dry Length')) + 
  scale_shape_manual(values = c(21,21,21,22), breaks = c('500','2900','5700','SR Dry Length'), labels = c('500 m','2900 m','5700 m','SR Dry Length')) +
  scale_color_manual(values = c('#e41a1c', '#4daf4a','#377eb8', 'black'), breaks = c('500','2900','5700','SR Dry Length'), labels = c('500 m','2900 m','5700 m','SR Dry Length')) +
  scale_fill_manual(values = c('#e41a1c', '#4daf4a','#377eb8', 'black'), breaks = c('500','2900','5700','SR Dry Length'), labels = c('500 m','2900 m','5700 m','SR Dry Length')) +
  ylab('FJ Gage Streamflow Depletion or \nScott River Dry Length Fraction') +
  scale_x_date(limits = c(as.Date('2010-01-01'),as.Date('2012-01-01')), 
               date_labels = '%b-%y',
               date_breaks =  'months',  
               expand  = c(0,0)) +
  #scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by = 0.25), expand = c(0.01,0.01)) +
  theme_few() + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

Hamlin_FJ_Wet_Year_plot = ggplot(data = subset(Hamlin_Daily,format(Date,'%Y')==c(2006,2007)), aes(x = Date, y = FJ_Diff_m3day/pumping_rate, color = factor(Dist2Str_m), shape = factor(Dist2Str_m), fill = factor(Dist2Str_m))) + 
  geom_rect(data = pumping_dates, inherit.aes = F, aes(xmin = start,xmax = end,ymin = ymin,ymax = ymax), fill = 'gray90') +
  geom_line() +
  geom_point() +
  geom_line(data = subset(Hamlin_Monthly, Dist2Str_m == 270), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length')) + 
  geom_point(data = subset(Hamlin_Monthly, Dist2Str_m == 270), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length', shape = 'SR Dry Length', fill = 'SR Dry Length')) + 
  scale_shape_manual(values = c(21,21,21,22),breaks = c('270','1370','3340','SR Dry Length'), labels = c('270 m','1370 m','3340 m','SR Dry Length')) +
  scale_color_manual(values = c('#e41a1c', '#4daf4a','#377eb8', 'black'), breaks = c('270','1370','3340','SR Dry Length'), labels = c('270 m','1370 m','3340 m','SR Dry Length')) +
  scale_fill_manual(values = c('#e41a1c', '#4daf4a','#377eb8', 'black'),breaks = c('270','1370','3340','SR Dry Length'), labels = c('270 m','1370 m','3340 m','SR Dry Length')) +
  ylab('FJ Gage Streamflow Depletion or \nScott River Dry Length Fraction') +
  scale_x_date(limits = c(as.Date('2006-01-01'),as.Date('2008-01-01')), 
               date_labels = '%b-%y',
               date_breaks =  'months',  
               expand  = c(0,0)) +
  #scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by = 0.25), expand = c(0.01,0.01)) +
  theme_few() + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

West_Side_Avg_Wet_Year_plot = ggplot(data = subset(West_Side_Daily,format(Date,'%Y')==c(2006,2007)), aes(x = Date, y = FJ_Diff_m3day/pumping_rate, color = factor(Dist2Str_m), shape = factor(Dist2Str_m), fill = factor(Dist2Str_m))) + 
  geom_rect(data = pumping_dates, inherit.aes = F, aes(xmin = start,xmax = end,ymin = ymin,ymax = ymax), fill = 'gray90') +
  geom_line() +
  geom_point() +
  geom_line(data = subset(West_Side_Monthly, Dist2Str_m == 500), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length')) + 
  geom_point(data = subset(West_Side_Monthly, Dist2Str_m == 500), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length', shape = 'SR Dry Length', fill = 'SR Dry Length')) + 
  scale_shape_manual(values = c(21,21,21,22), breaks = c('500','2900','5700','SR Dry Length'), labels = c('500 m','2900 m','5700 m','SR Dry Length')) +
  scale_color_manual(values = c('#e41a1c', '#4daf4a','#377eb8', 'black'), breaks = c('500','2900','5700','SR Dry Length'), labels = c('500 m','2900 m','5700 m','SR Dry Length')) +
  scale_fill_manual(values = c('#e41a1c', '#4daf4a','#377eb8', 'black'), breaks = c('500','2900','5700','SR Dry Length'), labels = c('500 m','2900 m','5700 m','SR Dry Length')) +
  ylab('FJ Gage Streamflow Depletion or \nScott River Dry Length Fraction') +
  scale_x_date(limits = c(as.Date('2006-01-01'),as.Date('2008-01-01')), 
               date_labels = '%b-%y',
               date_breaks =  'months',  
               expand  = c(0,0)) +
  #scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by = 0.25), expand = c(0.01,0.01)) +
  theme_few() + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))


# Hamlin Depletion 270m ----------------------------------
Hamlin_270m_melt = subset(Hamlin_Daily, Dist2Str_m == 270,
                          select =  c('Date', 'FJ_Diff_m3day','Pred_Loc_2_Diff_m3day',
                                      'Pred_Loc_3_Diff_m3day','Pred_Loc_4_Diff_m3day')) %>% melt(id.vars = 'Date')
Hamlin_270m_Dry_Year_plot = ggplot(data = Hamlin_270m_melt, aes(x = Date, y = value/pumping_rate, color = variable, group = variable, shape = variable, fill = variable)) +
  geom_rect(data = pumping_dates, inherit.aes = F, aes(xmin = start,xmax = end,ymin = ymin,ymax = ymax), fill = 'gray90') +
  geom_line() +
  geom_point() +
  geom_line(data = subset(Hamlin_Monthly, Dist2Str_m == 270), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length')) + 
  geom_point(data = subset(Hamlin_Monthly, Dist2Str_m == 270), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length', shape = 'SR Dry Length', fill = 'SR Dry Length')) + 
  scale_shape_manual(values = c(21,21,21,21,22)) +
  scale_color_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','black')) +
  scale_fill_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','black')) +
  scale_x_date(limits = c(as.Date('2001-01-01'),as.Date('2002-01-01')), 
               date_labels = '%b-%y',
               date_breaks =  'months',  
               expand  = c(0,0)) +
  scale_y_continuous(limits = c(0,10), breaks = seq(0,10, by = 1), expand = c(0.01,0.01)) +
  ylab('FJ Gage Streamflow Depletion or \nScott River Dry Length Fraction') +
  theme_few() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

Hamlin_270m_Avg_Year_plot = ggplot(data = Hamlin_270m_melt, aes(x = Date, y = value/pumping_rate, color = variable, group = variable, shape = variable, fill = variable)) +
  geom_rect(data = pumping_dates, inherit.aes = F, aes(xmin = start,xmax = end,ymin = ymin,ymax = ymax), fill = 'gray90') +
  geom_line() +
  geom_point() +
  geom_line(data = subset(Hamlin_Monthly, Dist2Str_m == 270), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length')) + 
  geom_point(data = subset(Hamlin_Monthly, Dist2Str_m == 270), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length', shape = 'SR Dry Length', fill = 'SR Dry Length')) + 
  scale_shape_manual(values = c(21,21,21,21,22)) +
  scale_color_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','black')) +
  scale_fill_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','black')) +
  scale_x_date(limits = c(as.Date('2010-01-01'),as.Date('2011-01-01')), 
               date_labels = '%b-%y',
               date_breaks =  'months',  
               expand  = c(0,0)) +
  scale_y_continuous(limits = c(0,1.6), breaks = seq(0,1.6, by = .2), expand = c(0.01,0.01)) +
  ylab('FJ Gage Streamflow Depletion or \nScott River Dry Length Fraction') +
  theme_few() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

Hamlin_270m_Wet_Year_plot = ggplot(data = Hamlin_270m_melt, aes(x = Date, y = value/pumping_rate, color = variable, group = variable, shape = variable, fill = variable)) +
  geom_rect(data = pumping_dates, inherit.aes = F, aes(xmin = start,xmax = end,ymin = ymin,ymax = ymax), fill = 'gray90') +
  geom_line() +
  geom_point() +
  geom_line(data = subset(Hamlin_Monthly, Dist2Str_m == 270), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length')) + 
  geom_point(data = subset(Hamlin_Monthly, Dist2Str_m == 270), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length', shape = 'SR Dry Length', fill = 'SR Dry Length')) + 
  scale_shape_manual(values = c(21,21,21,21,22)) +
  scale_color_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','black')) +
  scale_fill_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','black')) +
  scale_x_date(limits = c(as.Date('2006-01-01'),as.Date('2007-01-01')), 
               date_labels = '%b-%y',
               date_breaks =  'months',  
               expand  = c(0,0)) +
  scale_y_continuous(limits = c(0,1.6), breaks = seq(0,1.6, by = .2), expand = c(0.01,0.01)) +
  ylab('FJ Gage Streamflow Depletion or \nScott River Dry Length Fraction') +
  theme_few() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))
# Hamlin Depletion 1370m ----------------------------------
Hamlin_1370m_melt = subset(Hamlin_Daily, Dist2Str_m == 1370,
                          select =  c('Date', 'FJ_Diff_m3day','Pred_Loc_2_Diff_m3day',
                                      'Pred_Loc_3_Diff_m3day','Pred_Loc_4_Diff_m3day')) %>% melt(id.vars = 'Date')
Hamlin_1370m_Dry_Year_plot = ggplot(data = Hamlin_1370m_melt, aes(x = Date, y = value/pumping_rate, color = variable, group = variable, shape = variable, fill = variable)) +
  geom_rect(data = pumping_dates, inherit.aes = F, aes(xmin = start,xmax = end,ymin = ymin,ymax = ymax), fill = 'gray90') +
  geom_line() +
  geom_point() +
  geom_line(data = subset(Hamlin_Monthly, Dist2Str_m == 1370), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length')) + 
  geom_point(data = subset(Hamlin_Monthly, Dist2Str_m == 1370), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length', shape = 'SR Dry Length', fill = 'SR Dry Length')) + 
  scale_shape_manual(values = c(21,21,21,21,22)) +
  scale_color_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','black')) +
  scale_fill_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','black')) +
  scale_x_date(limits = c(as.Date('2001-01-01'),as.Date('2002-01-01')), 
               date_labels = '%b-%y',
               date_breaks =  'months',  
               expand  = c(0,0)) +
  scale_y_continuous(limits = c(0,10), breaks = seq(0,10, by = 1), expand = c(0.01,0.01)) +
  ylab('FJ Gage Streamflow Depletion or \nScott River Dry Length Fraction') +
  theme_few() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

Hamlin_1370m_Avg_Year_plot = ggplot(data = Hamlin_1370m_melt, aes(x = Date, y = value/pumping_rate, color = variable, group = variable, shape = variable, fill = variable)) +
  geom_rect(data = pumping_dates, inherit.aes = F, aes(xmin = start,xmax = end,ymin = ymin,ymax = ymax), fill = 'gray90') +
  geom_line() +
  geom_point() +
  geom_line(data = subset(Hamlin_Monthly, Dist2Str_m == 1370), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length')) + 
  geom_point(data = subset(Hamlin_Monthly, Dist2Str_m == 1370), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length', shape = 'SR Dry Length', fill = 'SR Dry Length')) + 
  scale_shape_manual(values = c(21,21,21,21,22)) +
  scale_color_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','black')) +
  scale_fill_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','black')) +
  scale_x_date(limits = c(as.Date('2010-01-01'),as.Date('2011-01-01')), 
               date_labels = '%b-%y',
               date_breaks =  'months',  
               expand  = c(0,0)) +
  scale_y_continuous(limits = c(0,1.6), breaks = seq(0,1.6, by = .2), expand = c(0.01,0.01)) +
  ylab('FJ Gage Streamflow Depletion or \nScott River Dry Length Fraction') +
  theme_few() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

Hamlin_1370m_Wet_Year_plot = ggplot(data = Hamlin_1370m_melt, aes(x = Date, y = value/pumping_rate, color = variable, group = variable, shape = variable, fill = variable)) +
  geom_rect(data = pumping_dates, inherit.aes = F, aes(xmin = start,xmax = end,ymin = ymin,ymax = ymax), fill = 'gray90') +
  geom_line() +
  geom_point() +
  geom_line(data = subset(Hamlin_Monthly, Dist2Str_m == 1370), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length')) + 
  geom_point(data = subset(Hamlin_Monthly, Dist2Str_m == 1370), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length', shape = 'SR Dry Length', fill = 'SR Dry Length')) + 
  scale_shape_manual(values = c(21,21,21,21,22)) +
  scale_color_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','black')) +
  scale_fill_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','black')) +
  scale_x_date(limits = c(as.Date('2006-01-01'),as.Date('2007-01-01')), 
               date_labels = '%b-%y',
               date_breaks =  'months',  
               expand  = c(0,0)) +
  scale_y_continuous(limits = c(0,1.6), breaks = seq(0,1.6, by = .2), expand = c(0.01,0.01)) +
  ylab('FJ Gage Streamflow Depletion or \nScott River Dry Length Fraction') +
  theme_few() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Hamlin Depletion 1370m ----------------------------------
Hamlin_3340m_melt = subset(Hamlin_Daily, Dist2Str_m == 3340,
                           select =  c('Date', 'FJ_Diff_m3day','Pred_Loc_2_Diff_m3day',
                                       'Pred_Loc_3_Diff_m3day','Pred_Loc_4_Diff_m3day')) %>% melt(id.vars = 'Date')
Hamlin_3340m_Dry_Year_plot = ggplot(data = Hamlin_3340m_melt, aes(x = Date, y = value/pumping_rate, color = variable, group = variable, shape = variable, fill = variable)) +
  geom_rect(data = pumping_dates, inherit.aes = F, aes(xmin = start,xmax = end,ymin = ymin,ymax = ymax), fill = 'gray90') +
  geom_line() +
  geom_point() +
  geom_line(data = subset(Hamlin_Monthly, Dist2Str_m == 3340), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length')) + 
  geom_point(data = subset(Hamlin_Monthly, Dist2Str_m == 3340), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length', shape = 'SR Dry Length', fill = 'SR Dry Length')) + 
  scale_shape_manual(values = c(21,21,21,21,22)) +
  scale_color_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','black')) +
  scale_fill_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','black')) +
  scale_x_date(limits = c(as.Date('2001-01-01'),as.Date('2002-01-01')), 
               date_labels = '%b-%y',
               date_breaks =  'months',  
               expand  = c(0,0)) +
  scale_y_continuous(limits = c(0,10), breaks = seq(0,10, by = 1), expand = c(0.01,0.01)) +
  ylab('FJ Gage Streamflow Depletion or \nScott River Dry Length Fraction') +
  theme_few() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

Hamlin_3340m_Avg_Year_plot = ggplot(data = Hamlin_3340m_melt, aes(x = Date, y = value/pumping_rate, color = variable, group = variable, shape = variable, fill = variable)) +
  geom_rect(data = pumping_dates, inherit.aes = F, aes(xmin = start,xmax = end,ymin = ymin,ymax = ymax), fill = 'gray90') +
  geom_line() +
  geom_point() +
  geom_line(data = subset(Hamlin_Monthly, Dist2Str_m == 3340), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length')) + 
  geom_point(data = subset(Hamlin_Monthly, Dist2Str_m == 3340), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length', shape = 'SR Dry Length', fill = 'SR Dry Length')) + 
  scale_shape_manual(values = c(21,21,21,21,22)) +
  scale_color_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','black')) +
  scale_fill_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','black')) +
  scale_x_date(limits = c(as.Date('2010-01-01'),as.Date('2011-01-01')), 
               date_labels = '%b-%y',
               date_breaks =  'months',  
               expand  = c(0,0)) +
  scale_y_continuous(limits = c(0,1.6), breaks = seq(0,1.6, by = .2), expand = c(0.01,0.01)) +
  ylab('FJ Gage Streamflow Depletion or \nScott River Dry Length Fraction') +
  theme_few() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

Hamlin_3340m_Wet_Year_plot = ggplot(data = Hamlin_3340m_melt, aes(x = Date, y = value/pumping_rate, color = variable, group = variable, shape = variable, fill = variable)) +
  geom_rect(data = pumping_dates, inherit.aes = F, aes(xmin = start,xmax = end,ymin = ymin,ymax = ymax), fill = 'gray90') +
  geom_line() +
  geom_point() +
  geom_line(data = subset(Hamlin_Monthly, Dist2Str_m == 3340), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length')) + 
  geom_point(data = subset(Hamlin_Monthly, Dist2Str_m == 3340), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length', shape = 'SR Dry Length', fill = 'SR Dry Length')) + 
  scale_shape_manual(values = c(21,21,21,21,22)) +
  scale_color_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','black')) +
  scale_fill_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','black')) +
  scale_x_date(limits = c(as.Date('2006-01-01'),as.Date('2007-01-01')), 
               date_labels = '%b-%y',
               date_breaks =  'months',  
               expand  = c(0,0)) +
  scale_y_continuous(limits = c(0,1.6), breaks = seq(0,1.6, by = .2), expand = c(0.01,0.01)) +
  ylab('FJ Gage Streamflow Depletion or \nScott River Dry Length Fraction') +
  theme_few() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Hamlin Depletion Combo Plot ---------------------------------------------
fig_name = paste0('Hamlin_',cell_type)                                                                           
if (fig_format == 'jpg'){                                                                                     
  jpeg(paste0(out_dir,fig_name,'.jpg'), width = fig_width, height = fig_height, units = fig_units,  res = 600)
} else if (fig_format == 'png'){                                                                              
  png(paste0(out_dir,fig_name,'.png'), width = 7.48, height = 9, units = 'in',  res = 600) 
} else if (fig_format == 'pdf'){                                                                              
  pdf(paste0(out_dir,fig_name,'.pdf'), width = 7, height = 8.5 )                                          
}                                                                                                             
grid.newpage()                                                                                                
pushViewport(viewport(layout = grid.layout(4,3)))                                                             
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)                                   
print(Basecase_Streamflow_Dry_Year_plot +
        theme(axis.title = element_blank(),
              axis.text = element_blank(),
              legend.position = 'none'),vp = vplayout(1,1))
print(Basecase_Streamflow_Avg_Year_plot +
        theme(axis.title = element_blank(),
              axis.text = element_blank(),
              legend.title = element_blank(),
              legend.position = c(0.3,0.2),
              legend.background = element_rect(fill = NA, color = NA),
              legend.key.height = unit(0.4,'cm'),
              legend.text = element_text(size = 8)),vp = vplayout(1,2))
print(Basecase_Streamflow_Wet_Year_plot +
        theme(axis.title = element_blank(),
              axis.text = element_blank(),
              legend.title = element_blank(),
              legend.position = 'none'),vp = vplayout(1,3))
print(Hamlin_270m_Dry_Year_plot +
        theme(axis.title = element_blank(),
              axis.text = element_blank(),
              legend.position = 'none'),vp = vplayout(2,1))
print(Hamlin_1370m_Dry_Year_plot +
        theme(axis.title = element_blank(),
              axis.text = element_blank(),
              legend.position = 'none'),vp = vplayout(3,1))
print(Hamlin_3340m_Dry_Year_plot +
        theme(axis.title = element_blank(),
              axis.text = element_blank(),
              legend.position = 'none'),vp = vplayout(4,1))
print(Hamlin_270m_Avg_Year_plot +
        theme(axis.title = element_blank(),
              axis.text = element_blank(),
              legend.position = 'none'),vp = vplayout(2,2))
print(Hamlin_1370m_Avg_Year_plot +
        theme(axis.title = element_blank(),
              axis.text = element_blank(),
              legend.position = 'none'),vp = vplayout(3,2))
print(Hamlin_3340m_Avg_Year_plot +
        theme(axis.title = element_blank(),
              axis.text = element_blank(),
              legend.position = 'none'),vp = vplayout(4,2))
print(Hamlin_270m_Wet_Year_plot +
        theme(axis.title = element_blank(),
              axis.text = element_blank(),
              legend.position = 'none'),vp = vplayout(2,3))
print(Hamlin_1370m_Wet_Year_plot +
        theme(axis.title = element_blank(),
              axis.text = element_blank(),
              legend.position = 'none'),vp = vplayout(3,3))
print(Hamlin_3340m_Wet_Year_plot +
        theme(axis.title = element_blank(),
              axis.text = element_blank(),
              legend.position = 'none'),vp = vplayout(4,3))
graphics.off()
# West Side Depletion 500m --------------------------------------
West_Side_500m_melt = subset(West_Side_Daily, Dist2Str_m == 500,
                             select =  c('Date', 'FJ_Diff_m3day','Pred_Loc_2_Diff_m3day',
                                         'Pred_Loc_3_Diff_m3day','Pred_Loc_4_Diff_m3day')) %>% melt(id.vars = 'Date')
West_Side_500m_Dry_Year_plot = ggplot(data = West_Side_500m_melt, aes(x = Date, y = value/pumping_rate, color = variable, group = variable, shape = variable, fill = variable)) +
  geom_rect(data = pumping_dates, inherit.aes = F, aes(xmin = start,xmax = end,ymin = ymin,ymax = ymax), fill = 'gray90') +
  geom_line() +
  geom_point() +
  geom_line(data = subset(West_Side_Monthly, Dist2Str_m == 500), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length')) + 
  geom_point(data = subset(West_Side_Monthly, Dist2Str_m == 500), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length', shape = 'SR Dry Length', fill = 'SR Dry Length')) + 
  scale_shape_manual(values = c(21,21,21,21,22)) +
  scale_color_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','black')) +
  scale_fill_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','black')) +
  scale_x_date(limits = c(as.Date('2001-01-01'),as.Date('2002-01-01')), 
               date_labels = '%b-%y',
               date_breaks =  'months',  
               expand  = c(0,0)) +
  scale_y_continuous(limits = c(0,10), breaks = seq(0,10, by = 1), expand = c(0.01,0.01)) +
  ylab('FJ Gage Streamflow Depletion or \nScott River Dry Length Fraction') +
  theme_few() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

West_Side_500m_Avg_Year_plot = ggplot(data = West_Side_500m_melt, aes(x = Date, y = value/pumping_rate, color = variable, group = variable, shape = variable, fill = variable)) +
  geom_rect(data = pumping_dates, inherit.aes = F, aes(xmin = start,xmax = end,ymin = ymin,ymax = ymax), fill = 'gray90') +
  geom_line() +
  geom_point() +
  geom_line(data = subset(West_Side_Monthly, Dist2Str_m == 500), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length')) + 
  geom_point(data = subset(West_Side_Monthly, Dist2Str_m == 500), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length', shape = 'SR Dry Length', fill = 'SR Dry Length')) + 
  scale_shape_manual(values = c(21,21,21,21,22)) +
  scale_color_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','black')) +
  scale_fill_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','black')) +
  scale_x_date(limits = c(as.Date('2010-01-01'),as.Date('2011-01-01')), 
               date_labels = '%b-%y',
               date_breaks =  'months',  
               expand  = c(0,0)) +
  scale_y_continuous(limits = c(0,1.6), breaks = seq(0,1.6, by = .2), expand = c(0.01,0.01)) +
  ylab('FJ Gage Streamflow Depletion or \nScott River Dry Length Fraction') +
  theme_few() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

West_Side_500m_Wet_Year_plot = ggplot(data = West_Side_500m_melt, aes(x = Date, y = value/pumping_rate, color = variable, group = variable, shape = variable, fill = variable)) +
  geom_rect(data = pumping_dates, inherit.aes = F, aes(xmin = start,xmax = end,ymin = ymin,ymax = ymax), fill = 'gray90') +
  geom_line() +
  geom_point() +
  geom_line(data = subset(West_Side_Monthly, Dist2Str_m == 500), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length')) + 
  geom_point(data = subset(West_Side_Monthly, Dist2Str_m == 500), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length', shape = 'SR Dry Length', fill = 'SR Dry Length')) + 
  scale_shape_manual(values = c(21,21,21,21,22)) +
  scale_color_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','black')) +
  scale_fill_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','black')) +
  scale_x_date(limits = c(as.Date('2006-01-01'),as.Date('2007-01-01')), 
               date_labels = '%b-%y',
               date_breaks =  'months',  
               expand  = c(0,0)) +
  scale_y_continuous(limits = c(0,1.6), breaks = seq(0,1.6, by = .2), expand = c(0.01,0.01)) +
  ylab('FJ Gage Streamflow Depletion or \nScott River Dry Length Fraction') +
  theme_few() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))
# West Side Depletion 2900m --------------------------------------
West_Side_2900m_melt = subset(West_Side_Daily, Dist2Str_m == 2900,
                             select =  c('Date', 'FJ_Diff_m3day','Pred_Loc_2_Diff_m3day',
                                         'Pred_Loc_3_Diff_m3day','Pred_Loc_4_Diff_m3day')) %>% melt(id.vars = 'Date')
West_Side_2900m_Dry_Year_plot = ggplot(data = West_Side_2900m_melt, aes(x = Date, y = value/pumping_rate, color = variable, group = variable, shape = variable, fill = variable)) +
  geom_rect(data = pumping_dates, inherit.aes = F, aes(xmin = start,xmax = end,ymin = ymin,ymax = ymax), fill = 'gray90') +
  geom_line() +
  geom_point() +
  geom_line(data = subset(West_Side_Monthly, Dist2Str_m == 2900), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length')) + 
  geom_point(data = subset(West_Side_Monthly, Dist2Str_m == 2900), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length', shape = 'SR Dry Length', fill = 'SR Dry Length')) + 
  scale_shape_manual(values = c(21,21,21,21,22)) +
  scale_color_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','black')) +
  scale_fill_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','black')) +
  scale_x_date(limits = c(as.Date('2001-01-01'),as.Date('2002-01-01')), 
               date_labels = '%b-%y',
               date_breaks =  'months',  
               expand  = c(0,0)) +
  scale_y_continuous(limits = c(0,10), breaks = seq(0,10, by = 1), expand = c(0.01,0.01)) +
  ylab('FJ Gage Streamflow Depletion or \nScott River Dry Length Fraction') +
  theme_few() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

West_Side_2900m_Avg_Year_plot = ggplot(data = West_Side_2900m_melt, aes(x = Date, y = value/pumping_rate, color = variable, group = variable, shape = variable, fill = variable)) +
  geom_rect(data = pumping_dates, inherit.aes = F, aes(xmin = start,xmax = end,ymin = ymin,ymax = ymax), fill = 'gray90') +
  geom_line() +
  geom_point() +
  geom_line(data = subset(West_Side_Monthly, Dist2Str_m == 2900), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length')) + 
  geom_point(data = subset(West_Side_Monthly, Dist2Str_m == 2900), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length', shape = 'SR Dry Length', fill = 'SR Dry Length')) + 
  scale_shape_manual(values = c(21,21,21,21,22)) +
  scale_color_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','black')) +
  scale_fill_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','black')) +
  scale_x_date(limits = c(as.Date('2010-01-01'),as.Date('2011-01-01')), 
               date_labels = '%b-%y',
               date_breaks =  'months',  
               expand  = c(0,0)) +
  scale_y_continuous(limits = c(0,1.6), breaks = seq(0,1.6, by = .2), expand = c(0.01,0.01)) +
  ylab('FJ Gage Streamflow Depletion or \nScott River Dry Length Fraction') +
  theme_few() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

West_Side_2900m_Wet_Year_plot = ggplot(data = West_Side_2900m_melt, aes(x = Date, y = value/pumping_rate, color = variable, group = variable, shape = variable, fill = variable)) +
  geom_rect(data = pumping_dates, inherit.aes = F, aes(xmin = start,xmax = end,ymin = ymin,ymax = ymax), fill = 'gray90') +
  geom_line() +
  geom_point() +
  geom_line(data = subset(West_Side_Monthly, Dist2Str_m == 2900), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length')) + 
  geom_point(data = subset(West_Side_Monthly, Dist2Str_m == 2900), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length', shape = 'SR Dry Length', fill = 'SR Dry Length')) + 
  scale_shape_manual(values = c(21,21,21,21,22)) +
  scale_color_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','black')) +
  scale_fill_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','black')) +
  scale_x_date(limits = c(as.Date('2006-01-01'),as.Date('2007-01-01')), 
               date_labels = '%b-%y',
               date_breaks =  'months',  
               expand  = c(0,0)) +
  scale_y_continuous(limits = c(0,1.6), breaks = seq(0,1.6, by = .2), expand = c(0.01,0.01)) +
  ylab('FJ Gage Streamflow Depletion or \nScott River Dry Length Fraction') +
  theme_few() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

# West Side Depletion 5700m --------------------------------------
West_Side_5700m_melt = subset(West_Side_Daily, Dist2Str_m == 5700,
                              select =  c('Date', 'FJ_Diff_m3day','Pred_Loc_2_Diff_m3day',
                                          'Pred_Loc_3_Diff_m3day','Pred_Loc_4_Diff_m3day')) %>% melt(id.vars = 'Date')
West_Side_5700m_Dry_Year_plot = ggplot(data = West_Side_5700m_melt, aes(x = Date, y = value/pumping_rate, color = variable, group = variable, shape = variable, fill = variable)) +
  geom_rect(data = pumping_dates, inherit.aes = F, aes(xmin = start,xmax = end,ymin = ymin,ymax = ymax), fill = 'gray90') +
  geom_line() +
  geom_point() +
  geom_line(data = subset(West_Side_Monthly, Dist2Str_m == 5700), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length')) + 
  geom_point(data = subset(West_Side_Monthly, Dist2Str_m == 5700), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length', shape = 'SR Dry Length', fill = 'SR Dry Length')) + 
  scale_shape_manual(values = c(21,21,21,21,22)) +
  scale_color_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','black')) +
  scale_fill_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','black')) +
  scale_x_date(limits = c(as.Date('2001-01-01'),as.Date('2002-01-01')), 
               date_labels = '%b-%y',
               date_breaks =  'months',  
               expand  = c(0,0)) +
  scale_y_continuous(limits = c(0,10), breaks = seq(0,10, by = 1), expand = c(0.01,0.01)) +
  ylab('FJ Gage Streamflow Depletion or \nScott River Dry Length Fraction') +
  theme_few() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

West_Side_5700m_Avg_Year_plot = ggplot(data = West_Side_5700m_melt, aes(x = Date, y = value/pumping_rate, color = variable, group = variable, shape = variable, fill = variable)) +
  geom_rect(data = pumping_dates, inherit.aes = F, aes(xmin = start,xmax = end,ymin = ymin,ymax = ymax), fill = 'gray90') +
  geom_line() +
  geom_point() +
  geom_line(data = subset(West_Side_Monthly, Dist2Str_m == 5700), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length')) + 
  geom_point(data = subset(West_Side_Monthly, Dist2Str_m == 5700), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length', shape = 'SR Dry Length', fill = 'SR Dry Length')) + 
  scale_shape_manual(values = c(21,21,21,21,22)) +
  scale_color_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','black')) +
  scale_fill_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','black')) +
  scale_x_date(limits = c(as.Date('2010-01-01'),as.Date('2011-01-01')), 
               date_labels = '%b-%y',
               date_breaks =  'months',  
               expand  = c(0,0)) +
  scale_y_continuous(limits = c(0,1.6), breaks = seq(0,1.6, by = .2), expand = c(0.01,0.01)) +
  ylab('FJ Gage Streamflow Depletion or \nScott River Dry Length Fraction') +
  theme_few() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

West_Side_5700m_Wet_Year_plot = ggplot(data = West_Side_5700m_melt, aes(x = Date, y = value/pumping_rate, color = variable, group = variable, shape = variable, fill = variable)) +
  geom_rect(data = pumping_dates, inherit.aes = F, aes(xmin = start,xmax = end,ymin = ymin,ymax = ymax), fill = 'gray90') +
  geom_line() +
  geom_point() +
  geom_line(data = subset(West_Side_Monthly, Dist2Str_m == 5700), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length')) + 
  geom_point(data = subset(West_Side_Monthly, Dist2Str_m == 5700), inherit.aes = F, aes(x = print_date, y = global_BC_SR_dry_Length_norm, color = 'SR Dry Length', shape = 'SR Dry Length', fill = 'SR Dry Length')) + 
  scale_shape_manual(values = c(21,21,21,21,22)) +
  scale_color_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','black')) +
  scale_fill_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','black')) +
  scale_x_date(limits = c(as.Date('2006-01-01'),as.Date('2007-01-01')), 
               date_labels = '%b-%y',
               date_breaks =  'months',  
               expand  = c(0,0)) +
  scale_y_continuous(limits = c(0,1.6), breaks = seq(0,1.6, by = .2), expand = c(0.01,0.01)) +
  ylab('FJ Gage Streamflow Depletion or \nScott River Dry Length Fraction') +
  theme_few() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))




