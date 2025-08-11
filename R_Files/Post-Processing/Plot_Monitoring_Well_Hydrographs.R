# Read MODFLOW Binary Head File
rm(list=ls())

library(ggplot2)
library(ggthemes)
library(dplyr)
library(reshape2)

out_dir = paste0(getwd(),'/Results/')
HOB_text = readLines('SVIHM.hob')
well_idx = grep(HOB_text,pattern = '0.000000  1.000000  1  1')
Well_IDs = sapply(strsplit(HOB_text[grep('0.000000  1.000000  1  1',HOB_text)],' '),"[[",1)
num_obs = as.numeric(sapply(strsplit(HOB_text[grep('0.000000  1.000000  1  1',HOB_text)],' '),"[[",8))*-1

for (i in 1:length(well_idx)){
  temp = read.table('SVIHM.hob', skip = well_idx[i]+1, nrows =  num_obs[i])[,1:4]
  temp$V5 = Well_IDs[i]
  if (i==1){
    Heads = temp
  } else {
    Heads = rbind(Heads,temp)
  }
}
names(Heads) = c('Observation_ID', 'Stress_Period', 'Day', 'Observed_Head_m', 'Well_ID')

Sim_Heads = read.table('HobData_SVIHM.dat', header = T, stringsAsFactors = FALSE)
names(Sim_Heads) = c('Simulated_Head_m', 'Observed_Head_m', 'Observation_ID')
Heads = left_join(Heads, subset(Sim_Heads, select = c('Observation_ID','Simulated_Head_m')), by = 'Observation_ID')
num_days = cumsum(as.numeric(diff(seq(as.Date('1990-10-01'), by = 'month', length.out = 253))))
Heads$Date = as.Date(num_days[Heads$Stress_Period] + Heads$Day, origin = '1990-09-30')

Heads_melt = melt(subset(Heads, select = c('Well_ID', 'Date','Observed_Head_m','Simulated_Head_m')), 
                  id.vars = c('Date','Well_ID'))


for (j in 1:length(Well_IDs)){
Hydrograph = ggplot(subset(Heads_melt, Well_ID==Well_IDs[j]), aes(x = Date, y = value)) +
  geom_line(aes(color = variable)) +
  geom_point(aes(color = variable)) +
  ylab('Groundwater Elevation (m)') +
  ggtitle(paste('Well', Well_IDs[j])) +
  scale_color_manual(values = c('Blue', 'Red'), labels = c('Observed', 'Simulated')) +
  #scale_x_date(limits = c(as.Date('1993-10-01'),as.Date('2011-10-01')), 
  scale_x_date(limits = c(as.Date('2005-10-01'),as.Date('2011-10-01')),
               breaks = seq(as.Date('1990-10-01'), length.out = 252, by = 'year'),
               date_labels = '%b-%Y') +
  theme_few() +
  theme(legend.title = element_blank(),
        legend.position = c(0.75,0.95),
        #legend.position = 'none',
        legend.background = element_rect(fill = NA, color = NA),
        legend.direction = 'horizontal',
        axis.text = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        legend.key = element_rect(fill = NA))
png(paste0(out_dir,'Well_',Well_IDs[j],'.png'), width = 4.75, height = 3.25, units = 'in', res = 600)
print(Hydrograph)
graphics.off()
}
