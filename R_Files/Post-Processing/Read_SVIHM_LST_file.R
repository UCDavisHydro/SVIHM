###########################################################################################
##############################          NOTES          ####################################
###########################################################################################
# Fluxes for constant head boundary conditions are read in but not included in plots since
# there are no constant head boundary conditions in SVIHM


###########################################################################################
rm(list=ls())  #Clear workspace
options(warn=-1)   # suppress warnings (set to 0 to turn warnings on)
library(ggplot2)
library(reshape2)
library(grid)
library(magrittr)
dir.create(file.path(getwd(),'Results'), showWarnings = FALSE)   #Create Results directory if it doesn't exist
out_dir = paste0(getwd(),'/Results')
nstress = 252
PRINT_BUDGET = FALSE       # Print monthly water budget to file (TRUE/FALSE)
StartingMonths = seq(as.Date("1990/10/1"), by = "month", length.out = 252)
LST_Name = 'SVIHM_Basecase.lst'
InputText = readLines(LST_Name)  #Read in text file

# Extract Convergence Failures
conv_fail_lines = grep('FAILED TO MEET',InputText)  #find stress periods where convergence failed
conv_fails=InputText[conv_fail_lines]

#Find lines where values are printed (including addition budget prints for time steps where solution did not converge)
STORAGE_Lines = grep('STORAGE =',InputText)
CONSTANT_HEAD_Lines = grep('CONSTANT HEAD =',InputText)
WELLS_Lines = grep('WELLS = ',InputText) 
RECHARGE_Lines = grep('              RECHARGE =  ',InputText) 
if(length(grep('RECHARGE =   0.00000', InputText[RECHARGE_Lines])) > 0){  #Removes weird entry in LST file. Unclear why this happens.
  RECHARGE_Lines = RECHARGE_Lines[-grep('RECHARGE =   0.00000', InputText[RECHARGE_Lines])]
}
ET_SEGMENTS_Lines = grep('ET SEGMENTS = ',InputText) 
STREAM_LEAKAGE_Lines = grep('STREAM LEAKAGE = ',InputText)
DRAINS_Lines = grep('DRAINS = ',InputText)
TOTAL_In_Lines = grep('TOTAL IN =',InputText)
TOTAL_Out_Lines = grep('TOTAL OUT =',InputText)

n_budget_entries = length(STORAGE_Lines)
WB_Components = c('STORAGE', 'CONSTANT_HEAD', 'WELLS', 'RECHARGE', 'ET_SEGMENTS','STREAM_LEAKAGE', 'DRAINS')

#Extract arrays for cumulative volumes and time step rates for different components of the groundwaterwater budget
for (i in 1:length(WB_Components)){
  if(WB_Components[i]%in%c('CONSTANT_HEAD','ET_SEGMENTS','STREAM_LEAKAGE')){
    c1 = 4 #column for extracting cumulative data
    c2 = 8 #column for extracting time step flux data
  } else {
    c1 = 3 #column for extracting cumulative data
    c2 = 6 #column for extracting time step flux data  
  }
  eval(parse(text = paste0(WB_Components[i],"_cumulative = strsplit(InputText[",WB_Components[i],"_Lines],' ') %>%", 
             'lapply(function(x){x[!x ==""]}) %>%',
             'sapply("[[",',c1,') %>%', 
             'as.numeric()')))
  eval(parse(text = paste0(WB_Components[i],"_cumulative_in = ",WB_Components[i],"_cumulative[seq(1,n_budget_entries,2)]")))
  eval(parse(text = paste0(WB_Components[i],"_cumulative_out = ",WB_Components[i],"_cumulative[seq(2,n_budget_entries,2)]")))
  
  eval(parse(text = paste0(WB_Components[i],"_TS_Flux = strsplit(InputText[",WB_Components[i],"_Lines],' ') %>%", 
                           'lapply(function(x){x[!x ==""]}) %>%',
                           'sapply("[[",',c2,') %>%', 
                           'as.numeric()')))
  eval(parse(text = paste0(WB_Components[i],"_TS_Flux_in = ",WB_Components[i],"_TS_Flux[seq(1,n_budget_entries,2)]")))
  eval(parse(text = paste0(WB_Components[i],"_TS_Flux_out = ",WB_Components[i],"_TS_Flux[seq(2,n_budget_entries,2)]")))
}
TOTAL_cumulative_in = strsplit(InputText[TOTAL_In_Lines],' ') %>% 
                         lapply(function(x){x[!x ==""]}) %>%
                         sapply("[[",4) %>%
                         as.numeric()
TOTAL_cumulative_out = strsplit(InputText[TOTAL_Out_Lines],' ') %>% 
  lapply(function(x){x[!x ==""]}) %>%
  sapply("[[",4) %>%
  as.numeric()
TOTAL_TS_Flux_in = strsplit(InputText[TOTAL_In_Lines],' ') %>% 
  lapply(function(x){x[!x ==""]}) %>%
  sapply("[[",8) %>%
  as.numeric()
TOTAL_TS_Flux_out = strsplit(InputText[TOTAL_Out_Lines],' ') %>% 
  lapply(function(x){x[!x ==""]}) %>%
  sapply("[[",8) %>%
  as.numeric()

Timestep_SP_Lines = grep('VOLUMETRIC BUDGET FOR ENTIRE MODEL AT END OF TIME STEP',InputText)
TS = as.numeric(sapply(lapply(strsplit(InputText[Timestep_SP_Lines],' '),function(x){x[!x ==""]}),"[[",11))  #Extract timestep number for printed budget
SP = as.numeric(sapply(lapply(strsplit(InputText[Timestep_SP_Lines],' '),function(x){x[!x ==""]}),"[[",15))  #Extract stress period number for printed budget

extra_rows = which(duplicated(SP))

WB_Components_All = c(WB_Components,'TOTAL')   # All components of the MODFLOW water budget
#Remove stress periods that didn't converge if there are any
for (i in 1:length(WB_Components_All)){
  if(length(extra_rows>0)){           #Remove stress periods that didn't converge if there are any
    TS = TS[-extra_rows]
    SP = SP[-extra_rows]
    eval(parse(text = paste0(WB_Components_All[i],'_cumulative_in = ',WB_Components_All[i],'_cumulative_in[-extra_rows]')))                                 
    eval(parse(text = paste0(WB_Components_All[i],'_cumulative_out = ',WB_Components_All[i],'_cumulative_out[-extra_rows]')))                               
    eval(parse(text = paste0(WB_Components_All[i],'_TS_Flux_in = ',WB_Components_All[i],'_TS_Flux_in[-extra_rows]')))                                       
    eval(parse(text = paste0(WB_Components_All[i],'_TS_Flux_out = ',WB_Components_All[i],'_TS_Flux_out[-extra_rows]')))                                     
  }
  #Net Cumulative Fluxes
  eval(parse(text = paste0(WB_Components_All[i],'_cumulative_net = ',WB_Components_All[i],'_cumulative_in - ',WB_Components_All[i],'_cumulative_out')))
  # Total Inflow Volume for each Stress Period
  eval(parse(text = paste0(WB_Components_All[i],'_SP_Vol_in = c(',WB_Components_All[i],'_cumulative_in[1],diff(',WB_Components_All[i],'_cumulative_in))')))
  # Total Outflow Volume for each Stress Period
  eval(parse(text = paste0(WB_Components_All[i],'_SP_Vol_out = c(',WB_Components_All[i],'_cumulative_out[1],diff(',WB_Components_All[i],'_cumulative_out))')))
  # Net Volume for each Stress Period
  eval(parse(text = paste0(WB_Components_All[i],'_SP_Vol_net = ',WB_Components_All[i],'_SP_Vol_in - ',WB_Components_All[i],'_SP_Vol_out')))
  # Net flux rate at the end of each time step
  eval(parse(text = paste0(WB_Components_All[i],'_TS_Flux_net = ',WB_Components_All[i],'_TS_Flux_in - ',WB_Components_All[i],'_TS_Flux_out')))
  }

#Calculate Mass Balance
Cumulative_Mass_Balance_percent_diff = ((TOTAL_cumulative_in - TOTAL_cumulative_out)/((TOTAL_cumulative_in + TOTAL_cumulative_out)/2))*100
SP_Mass_Balance_percent_diff = ((TOTAL_SP_Vol_in - TOTAL_SP_Vol_out)/((TOTAL_SP_Vol_in + TOTAL_SP_Vol_out)/2))*100
TS_Mass_Balance_percent_diff = ((TOTAL_TS_Flux_in - TOTAL_TS_Flux_out)/((TOTAL_TS_Flux_in + TOTAL_TS_Flux_out)/2))*100

Volumetric_Flux = data.frame(Month = format(seq(as.Date('1990/10/1'), as.Date('2011/9/30'), by = 'month'),'%b-%Y'),
                             STORAGE_in_m3 = STORAGE_SP_Vol_in,
                             STORAGE_out_m3 = STORAGE_SP_Vol_out,
                             STORAGE_net_m3 = STORAGE_SP_Vol_net,
                             CONSTANT_HEAD_in_m3 = CONSTANT_HEAD_SP_Vol_in,
                             CONSTANT_HEAD_out_m3 = CONSTANT_HEAD_SP_Vol_out,
                             CONSTANT_HEAD_net_m3 = CONSTANT_HEAD_SP_Vol_net,
                             WELLS_in_m3 = WELLS_SP_Vol_in,
                             WELLS_out_m3 = WELLS_SP_Vol_out,
                             WELLS_net_m3 = WELLS_SP_Vol_net,
                             RECHARGE_in_m3 = RECHARGE_SP_Vol_in,
                             RECHARGE_out_m3 = RECHARGE_SP_Vol_out,
                             RECHARGE_net_m3 = RECHARGE_SP_Vol_net,
                             ET_SEGMENTS_in_m3 = ET_SEGMENTS_SP_Vol_in,
                             ET_SEGMENTS_out_m3 = ET_SEGMENTS_SP_Vol_out,
                             ET_SEGMENTS_net_m3 = ET_SEGMENTS_SP_Vol_net,
                             STREAM_LEAKAGE_in_m3 = STREAM_LEAKAGE_SP_Vol_in,
                             STREAM_LEAKAGE_out_m3 = STREAM_LEAKAGE_SP_Vol_out,
                             STREAM_LEAKAGE_net_m3 = STREAM_LEAKAGE_SP_Vol_net,
                             DRAINS_in_m3 = DRAINS_SP_Vol_in,
                             DRAINS_out_m3 = DRAINS_SP_Vol_out,
                             DRAINS_net_m3 = DRAINS_SP_Vol_net,
                             TOTAL_in_m3 = TOTAL_SP_Vol_in,
                             TOTAL_out_m3 = TOTAL_SP_Vol_out,
                             TOTAL_net_m3 = TOTAL_SP_Vol_net,
                             Error_Cumulative_percent = Cumulative_Mass_Balance_percent_diff,
                             Error_Stress_Period_percent = SP_Mass_Balance_percent_diff,
                             Error_Timestep_percent = TS_Mass_Balance_percent_diff
                             )
if (PRINT_BUDGET==TRUE){
write.table(Volumetric_Flux, file = paste0(out_dir,'/MODFLOW_Water_Budget.dat'), row.names = F, quote = F)
}
##############################################################################################
##############             CREATE MONTHLY WATER BUDGETS           ############################
##############################################################################################
month_abbv = format(seq(as.Date('1990/10/1'), as.Date('1991/9/30'), by = 'month'),'%b')
Water_Budget_By_Month = Volumetric_Flux
Water_Budget_By_Month$Month = strtrim(Water_Budget_By_Month$Month,3)
for (i in 1:12){
  eval(parse(text = paste0("Water_Budget_",month_abbv[i]," = subset(Water_Budget_By_Month, select = paste0(WB_Components,'_net_m3'), Month == '",month_abbv[i],"')")))
  eval(parse(text = paste0('Water_Budget_',month_abbv[i],'$WY = seq(1991,2011)')))
}
##############################################################################################
##############             CREATE Annual WATER BUDGETS            ############################
##############################################################################################
Water_Budget_By_Year = subset(Volumetric_Flux, select = paste0(WB_Components,'_net_m3'))
Water_Budget_By_Year$WY = rep(seq(1991,2011),each = 12)
Water_Budget_By_Year = aggregate(.~WY, Water_Budget_By_Year, FUN = sum)

Net_Flux_Volume_m3 = data.frame(Storage = STORAGE_SP_Vol_net,
                                Wells =  WELLS_SP_Vol_net,
                                Recharge = RECHARGE_SP_Vol_net,
                                ET = ET_SEGMENTS_SP_Vol_net,
                                Stream_Leakage = STREAM_LEAKAGE_SP_Vol_net,
                                Drains = DRAINS_SP_Vol_net)
Net_Flux_Volume_TAF = data.frame(Storage = STORAGE_SP_Vol_net_TAF,
                                Wells =  WELLS_SP_Vol_net_TAF,
                                Recharge = RECHARGE_SP_Vol_net_TAF,
                                ET = ET_SEGMENTS_SP_Vol_net_TAF,
                                Stream_Leakage = STREAM_LEAKAGE_SP_Vol_net_TAF,
                                Drains = DRAINS_SP_Vol_net_TAF)
names(Net_Flux_Volume_m3) = c("Storage", "Wells","Recharge","ET","Stream Leakage","Drains")
names(Net_Flux_Volume_TAF) = c("Storage", "Wells","Recharge","ET","Stream Leakage","Drains")
Net_Flux_Volume_m3$Water_Year = rep(seq(1991,2011),each = 12)
Net_Flux_Volume_TAF$Water_Year = rep(seq(1991,2011),each = 12)
Annual_Net_Flux_Volume_m3 = aggregate(.~Water_Year,Net_Flux_Volume_m3,FUN = sum)
Annual_Net_Flux_Volume_TAF = aggregate(.~Water_Year,Net_Flux_Volume_TAF,FUN = sum)
Net_Flux_Volume_m3$Month = rep(c('Oct','Nov','Dec','Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep'),21)
Net_Flux_Volume_TAF$Month = rep(c('Oct','Nov','Dec','Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep'),21)
Net_Flux_Volume_m3$Month = factor(Net_Flux_Volume_m3$Month, levels = c('Oct','Nov','Dec','Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep'))
Net_Flux_Volume_m3 = Net_Flux_Volume_m3[order(Net_Flux_Volume_m3$Month),]
Net_Flux_Volume_TAF$Month = factor(Net_Flux_Volume_TAF$Month, levels = c('Oct','Nov','Dec','Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep'))
Net_Flux_Volume_TAF = Net_Flux_Volume_TAF[order(Net_Flux_Volume_TAF$Month),]
##############################################################################################
##############                       PLOTS                        ############################
##############################################################################################
Flux_in_labels = c('Storage', 'Wells', 'Recharge', 'Stream Leakage')
Flux_out_labels = c('Storage', 'Wells', 'ET', 'Stream Leakage', 'Drains')
Flux_net_labels = c('Storage', 'Wells', 'Recharge', 'ET', 'Stream Leakage', 'Drains')

#Model Error
Model_Error_melt = melt(Model_Error, id.vars = 'Date')
png(paste0(out_dir,'/Model_Error.png'), width = 6, height = 4, units = 'in',  res = 600)
(Model_Error_Plot = ggplot(Model_Error_melt, aes(x = as.Date(Date, format = '%Y-%m-%d'), y = value, col = factor(variable, labels = c('Cumulative', 'Last Time Step','Stress Period')))) +
    geom_line() + 
    scale_color_manual(values=c('red', 'blue', 'black')) + 
    scale_x_date(limits = c(as.Date('Oct-01-1990', format = '%b-%m-%y'), as.Date('Oct-01-2011', format = '%b-%m-%y')),
                 breaks = seq(as.Date("1990/10/1"), by = "year", length.out = 22), expand = c(0.01,0.01),
                 date_labels = ('%b-%y')) + 
    scale_y_continuous(limits = c(-0.5,0.5), breaks = seq(-0.5,0.5,by = 0.1), expand = c(0,0)) +
    labs(x = 'Stress Period', y = 'Model Error (%)') +
    ggtitle('Numerical Error') +
    theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(color = 'black', fill = NA),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), plot.title = element_text(hjust = 0.5),
          legend.position = c(0.2, 0.82), legend.key = element_rect(fill = NA, color = NA), legend.background = element_rect(fill = NA)))
graphics.off()


##################             SP Net flux for each term (m3)              ##################

SP_Net_Flux_m3_melt = melt(data.frame(Date = as.character(StartingMonths),
                            Storage = STORAGE_SP_Vol_net,
                            Wells = WELLS_SP_Vol_net,
                            Recharge = RECHARGE_SP_Vol_net,
                            ET = ET_SEGMENTS_SP_Vol_net,
                            Stream_Leakage = STREAM_LEAKAGE_SP_Vol_net,
                            Drains = DRAINS_SP_Vol_net),
                            id.vars = 'Date')
png(paste0(out_dir,'/Net_Flux_SP_Mm3.png'), width = 6, height = 4, units = 'in',  res = 600)
(SP_Net_Flux_m3_Plot = ggplot(SP_Net_Flux_m3_melt, aes(x = as.Date(Date, format = '%Y-%m-%d'), y = value, col = factor(variable, labels = Flux_net_labels))) +
    geom_line(size = 0.75) + 
    #scale_color_manual(values=c('red', 'blue', 'black')) + 
    scale_x_date(limits = c(as.Date('Oct-01-1990', format = '%b-%m-%y'), as.Date('Oct-01-2011', format = '%b-%m-%y')),
                 breaks = seq(as.Date("1990/10/1"), by = "year", length.out = 22), expand = c(0.01,0.01),
                 date_labels = ('%b-%y')) + 
    scale_y_continuous(limits = c(-5E7,5E7), breaks = seq(-5E7,5E7,by = 1E7), expand = c(0,0)) +
    xlab('') +
    ylab(bquote('Volume ('*m^3*')')) +
    ggtitle('Net Volumetric Flux') +
    theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(color = 'black', fill = NA),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), plot.title = element_text(hjust = 0.5),
          legend.position = c(0.5, 0.05), legend.key = element_rect(fill = NA, color = NA), legend.background = element_rect(fill = NA),
          legend.direction = 'horizontal',legend.text = element_text(size=8)))
graphics.off()

##################             SP Net flux for each term (TAF)              ##################

SP_Net_Flux_TAF_melt = melt(data.frame(Date = as.character(StartingMonths),
                                      Storage = STORAGE_SP_Vol_net_TAF,
                                      Wells = WELLS_SP_Vol_net_TAF,
                                      Recharge = RECHARGE_SP_Vol_net_TAF,
                                      ET = ET_SEGMENTS_SP_Vol_net_TAF,
                                      Stream_Leakage = STREAM_LEAKAGE_SP_Vol_net_TAF,
                                      Drains = DRAINS_SP_Vol_net_TAF),
                                      id.vars = 'Date')
png(paste0(out_dir,'/Net_Flux_SP_TAF.png'), width = 6, height = 4, units = 'in',  res = 600)
(SP_Net_Flux_TAF_Plot = ggplot(SP_Net_Flux_TAF_melt, aes(x = as.Date(Date, format = '%Y-%m-%d'), y = value, col = factor(variable, labels = Flux_net_labels))) +
    geom_line(size = 0.75) + 
    #scale_color_manual(values=c('red', 'blue', 'black')) + 
    scale_x_date(limits = c(as.Date('Oct-01-1990', format = '%b-%m-%y'), as.Date('Oct-01-2011', format = '%b-%m-%y')),
                 breaks = seq(as.Date("1990/10/1"), by = "year", length.out = 22), expand = c(0.01,0.01),
                 date_labels = ('%b-%y')) + 
    scale_y_continuous(limits = c(-40,40), breaks = seq(-40,40,by = 10), expand = c(0,0)) +
    xlab('') +
    ylab('Volume (TAF)') +
    ggtitle('Net Volumetric Flux') +
    theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(color = 'black', fill = NA),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), plot.title = element_text(hjust = 0.5),
          legend.position = c(0.5, 0.05), legend.key = element_rect(fill = NA, color = NA), legend.background = element_rect(fill = NA),
          legend.direction = 'horizontal',legend.text = element_text(size=8)))
graphics.off()

##################             Cumulative flux for each term (m3)              ##################

Cumulative_Flux_m3_melt = melt(data.frame(Date = StartingMonths,
                                            Storage = STORAGE_cumulative_in - STORAGE_cumulative_out,
                                            Wells = WELLS_cumulative_in - WELLS_cumulative_out,
                                            Recharge = RECHARGE_cumulative_in - RECHARGE_cumulative_out,
                                            ET = ET_SEGMENTS_cumulative_in - ET_SEGMENTS_cumulative_out,
                                            Stream_Leakage = STREAM_LEAKAGE_cumulative_in - STREAM_LEAKAGE_cumulative_out,
                                            Drains = DRAINS_cumulative_in - DRAINS_cumulative_out),
                                            id.vars = 'Date')

png(paste0(out_dir,'/Cumulative_Flux_SP_Mm3.png'), width = 6, height = 4, units = 'in',  res = 600)
(SP_Cumulative_Flux_m3_Plot = ggplot(Cumulative_Flux_m3_melt, aes(x = as.Date(Date, format = '%Y-%m-%d'), y = value, col = factor(variable, labels = Flux_net_labels))) +
    geom_line(size = 0.75) + 
    #scale_color_manual(values=c('red', 'blue', 'black')) + 
    scale_x_date(limits = c(as.Date('Oct-01-1990', format = '%b-%m-%y'), as.Date('Oct-01-2011', format = '%b-%m-%y')),
                 breaks = seq(as.Date("1990/10/1"), by = "year", length.out = 22), expand = c(0.01,0.01),
                 date_labels = ('%b-%y')) + 
    scale_y_continuous(limits = c(-6E8,6E8), breaks = seq(-6E8,6E8,by = 1E8), expand = c(0,0)) +
    xlab('') +
    ylab(bquote('Cumulative Volume ('*m^3*')')) +
    ggtitle('Cumulative Volumetric Flux') +
    theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(color = 'black', fill = NA),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), plot.title = element_text(hjust = 0.5),
          legend.position = c(0.5, 0.08), legend.key = element_rect(fill = NA, color = NA),
          legend.background = element_rect(fill = NA, color = 'black', size = 0.5),
          legend.direction = 'horizontal',legend.text = element_text(size=8),
          legend.margin = margin(1,2,1,1)))
graphics.off()

##################             Cumulative flux for each term (TAF)              ##################

Cumulative_Flux_TAF_melt = Cumulative_Flux_m3_melt
Cumulative_Flux_TAF_melt$value = Cumulative_Flux_TAF_melt$value * 0.000000810714
png(paste0(out_dir,'/Cumulative_Flux_SP_TAF.png'), width = 6, height = 4, units = 'in',  res = 600)
(SP_Cumulative_Flux_TAF_Plot = ggplot(Cumulative_Flux_TAF_melt, aes(x = as.Date(Date, format = '%Y-%m-%d'), y = value, col = factor(variable, labels = Flux_net_labels))) +
    geom_line(size = 0.75) + 
    #scale_color_manual(values=c('red', 'blue', 'black')) + 
    scale_x_date(limits = c(as.Date('Oct-01-1990', format = '%b-%m-%y'), as.Date('Oct-01-2011', format = '%b-%m-%y')),
                 breaks = seq(as.Date("1990/10/1"), by = "year", length.out = 22), expand = c(0.01,0.01),
                 date_labels = ('%b-%y')) + 
    scale_y_continuous(limits = c(-800,800), breaks = seq(-800,800,by = 200), expand = c(0,0)) +
    xlab('') +
    ylab(bquote('Cumulative Volume ('*m^3*')')) +
    ggtitle('Cumulative Volumetric Flux') +
    theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(color = 'black', fill = NA),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), plot.title = element_text(hjust = 0.5),
          legend.position = c(0.5, 0.08), legend.key = element_rect(fill = NA, color = NA),
          legend.background = element_rect(fill = NA, color = 'black', size = 0.5),
          legend.direction = 'horizontal',legend.text = element_text(size=8),
          legend.margin = margin(1,2,1,1)))
graphics.off()

##############################################################################################
##############                 ANNUAL WATER BUDGET PLOTS            ##########################
##############################################################################################
Annual_netWB_Mm3_melt = melt(Annual_Net_Flux_Volume_m3,id.vars = 'Water_Year')
Annual_netWB_Mm3_melt$value = Annual_netWB_Mm3_melt$value / 1E6 #Convert to Mm3 for plotting
pdf(paste0(out_dir,'/Annual_netWB_Mm3.pdf'), width = 8.5, height = 4)
(Annual_netWB_Mm3_Plot = ggplot(Annual_netWB_Mm3_melt, aes(x = Water_Year, y = value)) +
    geom_bar(aes(fill = variable), position = "stack", stat = 'identity', color = 'black', width = 0.95, size = 0.2 ) +
    scale_x_continuous(limits = c(1990.4,2011.6), expand = c(0,0), breaks = seq(1991,2011))  +
    scale_y_continuous(limits = c(-120,120), breaks = seq(-120,120,by = 30), expand = c(0,0)) +
    xlab('') +
    ylab(bquote('Volume ('*Mm^3*')')) +
    ggtitle('Annual Net Flux') +
    theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(color = 'black', fill = NA),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), 
          plot.title = element_text(hjust = 0.5),
          legend.position = c(0.2, 0.93), legend.key = element_rect(fill = NA, color = NA),
          legend.background = element_rect(fill = NA, color = NA),
          legend.direction = 'horizontal',
          legend.key.height = unit(10,'pt'),
          legend.text = element_text(size = 8, margin = margin(r=4,l=2, unit = 'pt'))
          )
)
graphics.off()

################################################################################################

Annual_netWB_TAF_melt = melt(Annual_Net_Flux_Volume_m3,id.vars = 'Water_Year')
Annual_netWB_TAF_melt$value = Annual_netWB_TAF_melt$value * 0.000000810714 #Convert to TAF for plotting
png(paste0(out_dir,'/Annual_netWB_TAF.png'), width = 8.5, height = 4, units = 'in',  res = 600)
(Annual_netWB_Mm3_Plot = ggplot(Annual_netWB_TAF_melt, aes(x = Water_Year, y = value)) +
    geom_bar(aes(fill = variable), position = "stack", stat = 'identity', color = 'black', width = 0.95 ) +
    scale_x_continuous(limits = c(1990,2012), expand = c(0,0), breaks = seq(1991,2011))  +
    scale_y_continuous(limits = c(-100,100), breaks = seq(-100,100,by = 20), expand = c(0,0)) +
    xlab('') +
    ylab('Volume (TAF)') +
    ggtitle('Annual Net Flux') +
    theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(color = 'black', fill = NA),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), plot.title = element_text(hjust = 0.5),
          legend.position = c(0.5, 0.93), legend.key = element_rect(fill = NA, color = NA),
          legend.background = element_rect(fill = NA, color = 'black', size = 0.5),
          legend.direction = 'horizontal',legend.text = element_text(size=8),
          legend.margin = margin(1,2,1,1)))
graphics.off()

##############################################################################################
##############             WATER BUDGET - DRY YEAR (2001)           ##########################
##############################################################################################
Net_Flux_Volume_Mm3_Dry_Year_melt = melt(subset(Net_Flux_Volume_m3,Water_Year==2001)[,c(-7)],id.vars = 'Month')
Net_Flux_Volume_Mm3_Dry_Year_melt$value = Net_Flux_Volume_Mm3_Dry_Year_melt$value/1E6
pdf(paste0(out_dir,'/Water_Budget_Dry_Year_2001.pdf'), width = 8.5, height = 4)
(Water_Budget_Dry_Year_2001_Plot = ggplot(Net_Flux_Volume_Mm3_Dry_Year_melt, aes(x = Month, y = value)) +
    geom_bar(aes(fill = variable), position = "stack", stat = 'identity', color = 'black', width = 0.95, size = 0.1) +
    scale_x_discrete(expand = c(0.05,0.05))  +
    #scale_y_continuous(limits = c(-30,30), breaks = seq(-30,30,by = 10), expand = c(0,0)) +
    scale_y_continuous(limits = c(-50,50), breaks = seq(-50,50,by = 25), expand = c(0,0)) +
    xlab('') +
    ylab(bquote('Volume ('*Mm^3*')')) +
    ggtitle('Water Budget - Dry Year (2001)') +
    theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(color = 'black', fill = NA),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), 
          axis.text = element_text(size = 12),
          plot.title = element_text(hjust = 0.5),
          legend.position = c(0.2, 0.85), 
          legend.key = element_rect(fill = NA, color = NA),
          legend.background = element_rect(fill = NA, color = NA),
          legend.direction = 'horizontal',
          legend.text = element_text(size = 8, margin = margin(r=4,l=2, unit = 'pt')),
          legend.key.height = unit(10, 'pt') 
          )
)
graphics.off()

##############################################################################################
##############             WATER BUDGET - AVERAGE YEAR (2001)           ##########################
##############################################################################################
Net_Flux_Volume_Mm3_Average_Year_melt = melt(subset(Net_Flux_Volume_m3,Water_Year==2010)[,c(-7)],id.vars = 'Month')
Net_Flux_Volume_Mm3_Average_Year_melt$value = Net_Flux_Volume_Mm3_Average_Year_melt$value/1E6
pdf(paste0(out_dir,'/Water_Budget_Average_Year_2010.pdf'), width = 8.5, height = 4)
(Water_Budget_Average_Year_2010_Plot = ggplot(Net_Flux_Volume_Mm3_Average_Year_melt, aes(x = Month, y = value)) +
    geom_bar(aes(fill = variable), position = "stack", stat = 'identity', color = 'black', width = 0.95, size = 0.1) +
    scale_x_discrete(expand = c(0.05,0.05))  +
    #scale_y_continuous(limits = c(-30,30), breaks = seq(-30,30,by = 10), expand = c(0,0)) +
    scale_y_continuous(limits = c(-50,50), breaks = seq(-50,50,by = 25), expand = c(0,0)) +
    xlab('') +
    ylab(bquote('Volume ('*Mm^3*')')) +
    ggtitle('Water Budget - Average Year (2010)') +
    theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(color = 'black', fill = NA),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), 
          axis.text = element_text(size = 12),
          plot.title = element_text(hjust = 0.5),
          legend.position = c(0.5, 0.93), legend.key = element_rect(fill = NA, color = NA),
          legend.background = element_rect(fill = NA, color = 'black', size = 0.5),
          legend.direction = 'horizontal',legend.text = element_text(size=8),
          legend.margin = margin(1,2,1,1)
          )
)
graphics.off()

##############################################################################################
##############             WATER BUDGET - WET YEAR (2006)           ##########################
##############################################################################################
Net_Flux_Volume_Mm3_Wet_Year_melt = melt(subset(Net_Flux_Volume_m3,Water_Year==2006)[,c(-7)],id.vars = 'Month')
Net_Flux_Volume_Mm3_Wet_Year_melt$value = Net_Flux_Volume_Mm3_Wet_Year_melt$value/1E6
pdf(paste0(out_dir,'/Water_Budget_Wet_Year_2006.pdf'), width = 8.5, height = 4)
(Water_Budget_Wet_Year_2006_Plot = ggplot(Net_Flux_Volume_Mm3_Wet_Year_melt, aes(x = Month, y = value)) +
    geom_bar(aes(fill = variable), position = "stack", stat = 'identity', color = 'black', width = 0.95, size = 0.1) +
    scale_x_discrete(expand = c(0.05,0.05))  +
    scale_y_continuous(limits = c(-50,50), breaks = seq(-50,50,by = 25), expand = c(0,0)) +
    xlab('') +
    ylab(bquote('Volume ('*Mm^3*')')) +
    ggtitle('Water Budget - Wet Year (2006)') +
    theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(color = 'black', fill = NA),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), 
          axis.text = element_text(size = 12),
          plot.title = element_text(hjust = 0.5),
          legend.position = c(0.5, 0.93), legend.key = element_rect(fill = NA, color = NA),
          legend.background = element_rect(fill = NA, color = 'black', size = 0.5),
          legend.direction = 'horizontal',legend.text = element_text(size=8),
          legend.margin = margin(1,2,1,1)
          )
)
graphics.off()


#Combine Dry, Average, and Wet Year plots
pdf(paste(out_dir,'/Water_Budget_Combined.pdf',sep=''),width = 8.5, height = 6)
grid.newpage()
pushViewport(viewport(layout = grid.layout(3, 1)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(Water_Budget_Dry_Year_2001_Plot + 
        theme(axis.text.x = element_blank(),
              plot.title = element_blank(),
              plot.margin = margin(t=8,b=-8,l=3,r=3)) +
        annotate('text', x = 11, y = 40, label = 'Dry Year (2001)', size = 5),
      vp = vplayout(1,1))
print(Water_Budget_Average_Year_2010_Plot +
        theme(axis.text.x = element_blank(),
              plot.title = element_blank(),
              legend.position = 'none',
              plot.margin = margin(t=8,b=-8,l=3,r=3)) +
        annotate('text', x = 11, y = 40, label = 'Average Year (2010)', size = 5),
      vp = vplayout(2,1))
print(Water_Budget_Wet_Year_2006_Plot +
      theme(plot.title = element_blank(),
            legend.position = 'none',
            plot.margin = margin(t=8,b=-8,l=3,r=3)) +
      annotate('text', x = 11, y = 40, label = 'Wet Year (2006)', size = 5),
      vp = vplayout(3,1))
graphics.off()

##############################################################################################
##############                 MONTHLY STORAGE CHANGE               ##########################
##############################################################################################
Monthly_Net_Storage_Mm3 = data.frame(Date = StartingMonths,
                                     Storage_Change = STORAGE_cumulative_net/1E6,
                                     Year = rep(seq(1991,2011),each = 12))
(Monthly_Storage_Plot = ggplot(data = Monthly_Net_Storage_Mm3, aes(x = StartingMonths, y = -(Storage_Change - Storage_Change[1]))) +
    geom_rect(aes(xmin = as.Date('1990/10/1'), xmax = as.Date('1992/09/30'), ymin = -60 , ymax = 60), fill = '#ff8282') +
    geom_rect(aes(xmin = as.Date('1993/10/1'), xmax = as.Date('1994/09/30'), ymin = -60 , ymax = 60), fill = '#ff8282') +
    geom_rect(aes(xmin = as.Date('1994/10/1'), xmax = as.Date('1999/09/30'), ymin = -60 , ymax = 60), fill = 'skyblue') +
    geom_rect(aes(xmin = as.Date('2000/10/1'), xmax = as.Date('2002/09/30'), ymin = -60 , ymax = 60), fill = '#ff8282') +
    geom_rect(aes(xmin = as.Date('2005/10/1'), xmax = as.Date('2006/09/30'), ymin = -60 , ymax = 60), fill = 'skyblue') +
    geom_rect(aes(xmin = as.Date('2006/10/1'), xmax = as.Date('2009/09/30'), ymin = -60 , ymax = 60), fill = '#ff8282') +
    geom_rect(aes(xmin = as.Date('2010/10/1'), xmax = as.Date('2011/09/30'), ymin = -60 , ymax = 60), fill = 'skyblue') +
    geom_hline(yintercept = 0, size = 0.25) +
    geom_line(size = 0.5) +
    geom_point(size = 0.75) +
    scale_x_date(limits = c(as.Date('Oct-01-1990', format = '%b-%m-%y'),
                            as.Date('Oct-01-2011', format = '%b-%m-%y')),
                 breaks = seq(as.Date("1990/10/1"), by = "2 years", length.out = 22), expand = c(0,0),
                 date_labels = ('%b-%y'))  +
    scale_y_continuous(limits = c(-60,60), breaks = seq(-60,60,by = 20), expand = c(0,0)) +
      ylab(ylab(bquote('Relative Aquifer Storage ('*Mm^3*')'))) +
    ggtitle('Monthly Relative Aquifer Storage') +
    theme(panel.background = element_blank(),
          panel.border = element_rect(fill=NA, color = 'black'),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
          axis.text.y = element_text(size = 8),
          axis.ticks = element_line(size = 0.2),
          plot.title = element_text(hjust = 0.5, size = 10),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 8) 
          )
)

##############################################################################################
##############                  ANNUAL STORAGE CHANGE               ##########################
##############################################################################################
Annual_Net_Storage_Mm3 = data.frame(Storage_Change = STORAGE_cumulative_net/1E6,
                                     Year = rep(seq(1991,2011),each = 12))
Annual_Net_Storage_Mm3 = aggregate(.~Year, data = Annual_Net_Storage_Mm3, FUN = mean)
(Annual_Storage_Plot = ggplot(data = Annual_Net_Storage_Mm3, aes(x = Year, y = -(Storage_Change-Storage_Change[1]))) +
    #shade areas for wet and dry/critical years
     geom_rect(aes(xmin = 1990.5, xmax = 1992.5, ymin = -30 , ymax = 30), fill = '#ff8282') +
     geom_rect(aes(xmin = 1993.5, xmax = 1994.5, ymin = -30 , ymax = 30), fill = '#ff8282') +
     geom_rect(aes(xmin = 1994.5, xmax = 1999.5, ymin = -30 , ymax = 30), fill = 'skyblue') +
     geom_rect(aes(xmin = 2000.5, xmax = 2002.5, ymin = -30 , ymax = 30), fill = '#ff8282') +
     geom_rect(aes(xmin = 2005.5, xmax = 2006.5, ymin = -30 , ymax = 30), fill = 'skyblue') +
     geom_rect(aes(xmin = 2006.5, xmax = 2009.5, ymin = -30 , ymax = 30), fill = '#ff8282') +
     geom_rect(aes(xmin = 2010.5, xmax = 2011.5, ymin = -30 , ymax = 30), fill = 'skyblue') +
    geom_hline(yintercept = 0, size = 0.25) +
    geom_line(size = 0.5) +
    geom_point(size = 0.75) +
    scale_x_continuous(limits = c(1990.5,2011.5), breaks = seq(1991,2011,by = 2), expand = c(0,0)) +
    scale_y_continuous(limits = c(-30,30), breaks = seq(-30,30,by = 10), expand = c(0,0)) +
    ylab(ylab(bquote('Relative Aquifer Storage ('*Mm^3*')'))) +
    ggtitle('Annual Relative Aquifer Storage') +
    theme(panel.background = element_blank(),
          panel.border = element_rect(fill=NA, color = 'black'),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
          axis.text.y = element_text(size = 8),
          axis.ticks = element_line(size = 0.2),
          plot.title = element_text(hjust = 0.5, size = 10),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 8) 
    )
)
##############################################################################################
###########      PLOT MONTHLY AND ANNUAL AQUIFER STORAGE CHANGE        #######################
##############################################################################################
pdf(paste(out_dir,'/Aquifer_Storage_Change.pdf',sep=''),width = 3, height = 4)
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 1)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(Monthly_Storage_Plot, vp = vplayout(1,1))
print(Annual_Storage_Plot, vp = vplayout(2,1))
graphics.off()

##############################################################################################
##############                 MONTHLY STREAM LEAKAGE               ##########################
##############################################################################################
Monthly_Stream_Leakage_Mm3 = data.frame(Date = StartingMonths,
                                     Stream_Leakage = STREAM_LEAKAGE_SP_Vol_net/1E6,
                                     Year = rep(seq(1991,2011),each = 12))
(Monthly_Stream_Leakage_Plot = ggplot(data = Monthly_Stream_Leakage_Mm3, aes(x = StartingMonths, y = Stream_Leakage)) +
    geom_rect(aes(xmin = as.Date('1990/10/1'), xmax = as.Date('1992/09/30'), ymin = -30 , ymax = 30), fill = '#ff8282') +
    geom_rect(aes(xmin = as.Date('1993/10/1'), xmax = as.Date('1994/09/30'), ymin = -30 , ymax = 30), fill = '#ff8282') +
    geom_rect(aes(xmin = as.Date('1994/10/1'), xmax = as.Date('1999/09/30'), ymin = -30 , ymax = 30), fill = 'skyblue') +
    geom_rect(aes(xmin = as.Date('2000/10/1'), xmax = as.Date('2002/09/30'), ymin = -30 , ymax = 30), fill = '#ff8282') +
    geom_rect(aes(xmin = as.Date('2005/10/1'), xmax = as.Date('2006/09/30'), ymin = -30 , ymax = 30), fill = 'skyblue') +
    geom_rect(aes(xmin = as.Date('2006/10/1'), xmax = as.Date('2009/09/30'), ymin = -30 , ymax = 30), fill = '#ff8282') +
    geom_rect(aes(xmin = as.Date('2010/10/1'), xmax = as.Date('2011/09/30'), ymin = -30 , ymax = 30), fill = 'skyblue') +
    geom_hline(yintercept = 0, size = 0.25) +
    geom_line(size = 0.5) +
    geom_point(size = 0.75) +
    scale_x_date(limits = c(as.Date('Oct-01-1990', format = '%b-%m-%y'),
                            as.Date('Oct-01-2011', format = '%b-%m-%y')),
                 breaks = seq(as.Date("1990/10/1"), by = "2 years", length.out = 22), expand = c(0,0),
                 date_labels = ('%b-%y'))  +
    scale_y_continuous(limits = c(-30,30), breaks = seq(-30,30,by = 10), expand = c(0,0)) +
    ylab(ylab(bquote('Net Stream-Aquifer Flux ('*Mm^3*')'))) +
    ggtitle('Monthly Net Stream-Aquifer Flux') +
    theme(panel.background = element_blank(),
          panel.border = element_rect(fill=NA, color = 'black'),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
          axis.text.y = element_text(size = 8),
          axis.ticks = element_line(size = 0.2),
          plot.title = element_text(hjust = 0.5, size = 10),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 8) 
    )
)

##############################################################################################
##############                  ANNUAL STREAM LEAKAGE               ##########################
##############################################################################################
Annual_Stream_Leakage_Mm3 = data.frame(Stream_Leakage = STREAM_LEAKAGE_SP_Vol_net/1E6,
                                    Year = rep(seq(1991,2011),each = 12))
Annual_Stream_Leakage_Mm3 = aggregate(.~Year, data = Annual_Stream_Leakage_Mm3, FUN = sum)
(Annual_Stream_Leakage_Plot = ggplot(data = Annual_Stream_Leakage_Mm3, aes(x = Year, y = Stream_Leakage)) +
    #shade areas for wet and dry/critical years
    geom_rect(aes(xmin = 1990.5, xmax = 1992.5, ymin = -60 , ymax = 60), fill = '#ff8282') +
    geom_rect(aes(xmin = 1993.5, xmax = 1994.5, ymin = -60 , ymax = 60), fill = '#ff8282') +
    geom_rect(aes(xmin = 1994.5, xmax = 1999.5, ymin = -60 , ymax = 60), fill = 'skyblue') +
    geom_rect(aes(xmin = 2000.5, xmax = 2002.5, ymin = -60 , ymax = 60), fill = '#ff8282') +
    geom_rect(aes(xmin = 2005.5, xmax = 2006.5, ymin = -60 , ymax = 60), fill = 'skyblue') +
    geom_rect(aes(xmin = 2006.5, xmax = 2009.5, ymin = -60 , ymax = 60), fill = '#ff8282') +
    geom_rect(aes(xmin = 2010.5, xmax = 2011.5, ymin = -60 , ymax = 60), fill = 'skyblue') +
    geom_hline(yintercept = 0, size = 0.25) +
    geom_line(size = 0.5) +
    geom_point(size = 0.75) +
    scale_x_continuous(limits = c(1990.5,2011.5), breaks = seq(1991,2011,by = 2), expand = c(0,0)) +
    scale_y_continuous(limits = c(-60,60), breaks = seq(-60,60,by = 20), expand = c(0,0)) +
    ylab(ylab(bquote('Annual Net Stream-Aquifer Flux ('*Mm^3*')'))) +
    ggtitle('Net Stream-Aquifer Flux') +
    theme(panel.background = element_blank(),
          panel.border = element_rect(fill=NA, color = 'black'),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
          axis.text.y = element_text(size = 8),
          axis.ticks = element_line(size = 0.2),
          plot.title = element_text(hjust = 0.5, size = 10),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 8) 
    )
)

##############################################################################################
##############              MONTHLY GROUNDWATER PUMPING             ##########################
##############################################################################################
Monthly_Groundwater_Pumping_Mm3 = data.frame(Date = StartingMonths,
                                        Groundwater_Pumping = WELLS_SP_Vol_out/1E6,
                                        Year = rep(seq(1991,2011),each = 12))
(Monthly_Groundwater_Pumping_Plot = ggplot(data = Monthly_Groundwater_Pumping_Mm3, aes(x = StartingMonths, y = Groundwater_Pumping)) +
    geom_rect(aes(xmin = as.Date('1990/10/1'), xmax = as.Date('1992/09/30'), ymin = -0 , ymax = 20), fill = '#ff8282') +
    geom_rect(aes(xmin = as.Date('1993/10/1'), xmax = as.Date('1994/09/30'), ymin = -0 , ymax = 20), fill = '#ff8282') +
    geom_rect(aes(xmin = as.Date('1994/10/1'), xmax = as.Date('1999/09/30'), ymin = -0 , ymax = 20), fill = 'skyblue') +
    geom_rect(aes(xmin = as.Date('2000/10/1'), xmax = as.Date('2002/09/30'), ymin = -0 , ymax = 20), fill = '#ff8282') +
    geom_rect(aes(xmin = as.Date('2005/10/1'), xmax = as.Date('2006/09/30'), ymin = -0 , ymax = 20), fill = 'skyblue') +
    geom_rect(aes(xmin = as.Date('2006/10/1'), xmax = as.Date('2009/09/30'), ymin = -0 , ymax = 20), fill = '#ff8282') +
    geom_rect(aes(xmin = as.Date('2010/10/1'), xmax = as.Date('2011/09/30'), ymin = -0 , ymax = 20), fill = 'skyblue') +
    geom_hline(yintercept = 0, size = 0.25) +
    geom_line(size = 0.5) +
    geom_point(size = 0.75) +
    scale_x_date(limits = c(as.Date('Oct-01-1990', format = '%b-%m-%y'),
                            as.Date('Oct-01-2011', format = '%b-%m-%y')),
                 breaks = seq(as.Date("1990/10/1"), by = "2 years", length.out = 22), expand = c(0,0),
                 date_labels = ('%b-%y'))  +
    scale_y_continuous(limits = c(0,20), breaks = seq(0,20,by = 5), expand = c(0,0)) +
    ylab(ylab(bquote('Monthly Groundwater Pumping ('*Mm^3*')'))) +
    ggtitle('Monthly Groundwater Pumping') +
    theme(panel.background = element_blank(),
          panel.border = element_rect(fill=NA, color = 'black'),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
          axis.text.y = element_text(size = 8),
          axis.ticks = element_line(size = 0.2),
          plot.title = element_text(hjust = 0.5, size = 10),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 8) 
    )
)

##############################################################################################
##############               ANNUAL GROUNDWATER PUMPING             ##########################
##############################################################################################
Annual_Groundwater_Pumping_Mm3 = data.frame(Groundwater_Pumping = WELLS_SP_Vol_out/1E6,
                                       Year = rep(seq(1991,2011),each = 12))
Annual_Groundwater_Pumping_Mm3 = aggregate(.~Year, data = Annual_Groundwater_Pumping_Mm3, FUN = sum)
(Annual_Groundwater_Pumping_Plot = ggplot(data = Annual_Groundwater_Pumping_Mm3, aes(x = Year, y = Groundwater_Pumping)) +
    #shade areas for wet and dry/critical years
    geom_rect(aes(xmin = 1990.5, xmax = 1992.5, ymin = 0 , ymax = 70), fill = '#ff8282') +
    geom_rect(aes(xmin = 1993.5, xmax = 1994.5, ymin = 0 , ymax = 70), fill = '#ff8282') +
    geom_rect(aes(xmin = 1994.5, xmax = 1999.5, ymin = 0 , ymax = 70), fill = 'skyblue') +
    geom_rect(aes(xmin = 2000.5, xmax = 2002.5, ymin = 0 , ymax = 70), fill = '#ff8282') +
    geom_rect(aes(xmin = 2005.5, xmax = 2006.5, ymin = 0 , ymax = 70), fill = 'skyblue') +
    geom_rect(aes(xmin = 2006.5, xmax = 2009.5, ymin = 0 , ymax = 70), fill = '#ff8282') +
    geom_rect(aes(xmin = 2010.5, xmax = 2011.5, ymin = 0 , ymax = 70), fill = 'skyblue') +
    geom_hline(yintercept = 0, size = 0.25) +
    geom_line(size = 0.5) +
    geom_point(size = 0.75) +
    scale_x_continuous(limits = c(1990.5,2011.5), breaks = seq(1991,2011,by = 2), expand = c(0,0)) +
    scale_y_continuous(limits = c(0,70), breaks = seq(0,70,by = 10), expand = c(0,0)) +
    ylab(ylab(bquote('Groundwater Pumping ('*Mm^3*')'))) +
    ggtitle('Annual Groundwater Pumping') +
    theme(panel.background = element_blank(),
          panel.border = element_rect(fill=NA, color = 'black'),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
          axis.text.y = element_text(size = 8),
          axis.ticks = element_line(size = 0.2),
          plot.title = element_text(hjust = 0.5, size = 10),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 8) 
    )
)



##############################################################################################
##############                MONTHLY WATER BUDGET PLOTS            ##########################
##############################################################################################
# October_netWB_Mm3_melt = melt(October_netWB_m3,id.vars = 'Date')
# October_netWB_Mm3_melt$value = October_netWB_Mm3_melt$value / 1E6   #Convert to Mm3 for plotting
# png(paste0(out_dir,'/October_netWB_Mm3.png'), width = 6, height = 4, units = 'in',  res = 600)
# (October_netWB_m3_Plot = ggplot(October_netWB_Mm3_melt, aes(x = as.Date(Date, format = '%Y-%m-%d'), y = value)) +
#     geom_bar(aes(fill = variable),position = 'dodge', stat = 'identity', color = 'black') +
#      scale_x_date(limits = c(as.Date('Oct-01-1990', format = '%b-%m-%y'), as.Date('Oct-01-2011', format = '%b-%m-%y')),
#                   breaks = seq(as.Date("1990/10/1"), by = "year", length.out = 22), expand = c(0.01,0.01),
#                   date_labels = ('%b-%y'))  +
#      scale_y_continuous(limits = c(-12,12), breaks = seq(-12,12,by = 2), expand = c(0,0)) +
#      xlab('') +
#      ylab(bquote('Volume ('*Mm^3*')')) +
#      ggtitle('October Volumetric Flux') +
#      theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#            panel.background = element_rect(color = 'black', fill = NA),
#            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), plot.title = element_text(hjust = 0.5),
#            legend.position = c(0.5, 0.93), legend.key = element_rect(fill = NA, color = NA),
#            legend.background = element_rect(fill = NA, color = 'black', size = 0.5),
#            legend.direction = 'horizontal',legend.text = element_text(size=8),
#            legend.margin = margin(1,2,1,1)))
# graphics.off()
# ################################################################################################
# October_netWB_TAF_melt = melt(October_netWB_TAF,id.vars = 'Date')
# png(paste0(out_dir,'/October_netWB_TAF.png'), width = 6, height = 4, units = 'in',  res = 600)
# (October_netWB_TAF_Plot = ggplot(October_netWB_TAF_melt, aes(x = as.Date(Date, format = '%Y-%m-%d'), y = value)) +
#     geom_bar(aes(fill = variable),position = 'dodge', stat = 'identity', color = 'black') +
#     scale_x_date(limits = c(as.Date('Oct-01-1990', format = '%b-%m-%y'), as.Date('Oct-01-2011', format = '%b-%m-%y')),
#                  breaks = seq(as.Date("1990/10/1"), by = "year", length.out = 22), expand = c(0.01,0.01),
#                  date_labels = ('%b-%y'))  +
#     scale_y_continuous(limits = c(-10,10), breaks = seq(-10,10,by = 2), expand = c(0,0)) +
#     xlab('') +
#     ylab('Volume (TAF)') +
#     ggtitle('October Volumetric Flux') +
#     theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#           panel.background = element_rect(color = 'black', fill = NA),
#           axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), plot.title = element_text(hjust = 0.5),
#           legend.position = c(0.5, 0.93), legend.key = element_rect(fill = NA, color = NA),
#           legend.background = element_rect(fill = NA, color = 'black', size = 0.5),
#           legend.direction = 'horizontal',legend.text = element_text(size=8),
#           legend.margin = margin(1,2,1,1)))
# graphics.off()
# 
# #################################################################################################
# 
# November_netWB_Mm3_melt = melt(November_netWB_m3,id.vars = 'Date')
# November_netWB_Mm3_melt$value = November_netWB_Mm3_melt$value / 1E6   #Convert to Mm3 for plotting
# png(paste0(out_dir,'/November_netWB_Mm3.png'), width = 6, height = 4, units = 'in',  res = 600)
# (November_netWB_m3_Plot = ggplot(November_netWB_Mm3_melt, aes(x = as.Date(Date, format = '%Y-%m-%d'), y = value)) +
#     geom_bar(aes(fill = variable),position = 'dodge', stat = 'identity', color = 'black') +
#     scale_x_date(limits = c(as.Date('Oct-01-1990', format = '%b-%m-%y'), as.Date('Oct-01-2011', format = '%b-%m-%y')),
#                  breaks = seq(as.Date("1990/10/1"), by = "year", length.out = 22), expand = c(0.01,0.01),
#                  date_labels = ('%b-%y'))  +
#     scale_y_continuous(limits = c(-20,20), breaks = seq(-20,20,by = 5), expand = c(0,0)) +
#     xlab('') +
#     ylab(bquote('Volume ('*Mm^3*')')) +
#     ggtitle('November Volumetric Flux') +
#     theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#           panel.background = element_rect(color = 'black', fill = NA),
#           axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), plot.title = element_text(hjust = 0.5),
#           legend.position = c(0.5, 0.93), legend.key = element_rect(fill = NA, color = NA),
#           legend.background = element_rect(fill = NA, color = 'black', size = 0.5),
#           legend.direction = 'horizontal',legend.text = element_text(size=8),
#           legend.margin = margin(1,2,1,1)))
# graphics.off()
# ################################################################################################
# November_netWB_TAF_melt = melt(November_netWB_TAF,id.vars = 'Date')
# png(paste0(out_dir,'/November_netWB_TAF.png'), width = 6, height = 4, units = 'in',  res = 600)
# (November_netWB_TAF_Plot = ggplot(November_netWB_TAF_melt, aes(x = as.Date(Date, format = '%Y-%m-%d'), y = value)) +
#     geom_bar(aes(fill = variable),position = 'dodge', stat = 'identity', color = 'black') +
#     scale_x_date(limits = c(as.Date('Oct-01-1990', format = '%b-%m-%y'), as.Date('Oct-01-2011', format = '%b-%m-%y')),
#                  breaks = seq(as.Date("1990/10/1"), by = "year", length.out = 22), expand = c(0.01,0.01),
#                  date_labels = ('%b-%y'))  +
#     scale_y_continuous(limits = c(-15,15), breaks = seq(-15,15,by = 5), expand = c(0,0)) +
#     xlab('') +
#     ylab('Volume (TAF)') +
#     ggtitle('November Volumetric Flux') +
#     theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#           panel.background = element_rect(color = 'black', fill = NA),
#           axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), plot.title = element_text(hjust = 0.5),
#           legend.position = c(0.5, 0.93), legend.key = element_rect(fill = NA, color = NA),
#           legend.background = element_rect(fill = NA, color = 'black', size = 0.5),
#           legend.direction = 'horizontal',legend.text = element_text(size=8),
#           legend.margin = margin(1,2,1,1)))
# graphics.off()
# 
# #################################################################################################
# 
# December_netWB_Mm3_melt = melt(December_netWB_m3,id.vars = 'Date')
# December_netWB_Mm3_melt$value = December_netWB_Mm3_melt$value / 1E6   #Convert to Mm3 for plotting
# png(paste0(out_dir,'/December_netWB_Mm3.png'), width = 6, height = 4, units = 'in',  res = 600)
# (December_netWB_m3_Plot = ggplot(December_netWB_Mm3_melt, aes(x = as.Date(Date, format = '%Y-%m-%d'), y = value)) +
#     geom_bar(aes(fill = variable),position = 'dodge', stat = 'identity', color = 'black') +
#     scale_x_date(limits = c(as.Date('Oct-01-1990', format = '%b-%m-%y'), as.Date('Oct-01-2011', format = '%b-%m-%y')),
#                  breaks = seq(as.Date("1990/10/1"), by = "year", length.out = 22), expand = c(0.01,0.01),
#                  date_labels = ('%b-%y'))  +
#     scale_y_continuous(limits = c(-40,40), breaks = seq(-40,40,by = 10), expand = c(0,0)) +
#     xlab('') +
#     ylab(bquote('Volume ('*m^3*')')) +
#     ggtitle('December Volumetric Flux') +
#     theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#           panel.background = element_rect(color = 'black', fill = NA),
#           axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), plot.title = element_text(hjust = 0.5),
#           legend.position = c(0.5, 0.93), legend.key = element_rect(fill = NA, color = NA),
#           legend.background = element_rect(fill = NA, color = 'black', size = 0.5),
#           legend.direction = 'horizontal',legend.text = element_text(size=8),
#           legend.margin = margin(1,2,1,1)))
# graphics.off()
# ################################################################################################
# December_netWB_TAF_melt = melt(December_netWB_TAF,id.vars = 'Date')
# png(paste0(out_dir,'/December_netWB_TAF.png'), width = 6, height = 4, units = 'in',  res = 600)
# (December_netWB_TAF_Plot = ggplot(December_netWB_TAF_melt, aes(x = as.Date(Date, format = '%Y-%m-%d'), y = value)) +
#     geom_bar(aes(fill = variable),position = 'dodge', stat = 'identity', color = 'black') +
#     scale_x_date(limits = c(as.Date('Oct-01-1990', format = '%b-%m-%y'), as.Date('Oct-01-2011', format = '%b-%m-%y')),
#                  breaks = seq(as.Date("1990/10/1"), by = "year", length.out = 22), expand = c(0.01,0.01),
#                  date_labels = ('%b-%y'))  +
#     scale_y_continuous(limits = c(-30,30), breaks = seq(-30,30,by = 5), expand = c(0,0)) +
#     xlab('') +
#     ylab('Volume (TAF)') +
#     ggtitle('December Volumetric Flux') +
#     theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#           panel.background = element_rect(color = 'black', fill = NA),
#           axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), plot.title = element_text(hjust = 0.5),
#           legend.position = c(0.5, 0.93), legend.key = element_rect(fill = NA, color = NA),
#           legend.background = element_rect(fill = NA, color = 'black', size = 0.5),
#           legend.direction = 'horizontal',legend.text = element_text(size=8),
#           legend.margin = margin(1,2,1,1)))
# graphics.off()
# 
# #################################################################################################
# 
# January_netWB_Mm3_melt = melt(January_netWB_m3,id.vars = 'Date')
# January_netWB_Mm3_melt$value = January_netWB_Mm3_melt$value / 1E6   #Convert to Mm3 for plotting
# png(paste0(out_dir,'/January_netWB_Mm3.png'), width = 6, height = 4, units = 'in',  res = 600)
# (January_netWB_m3_Plot = ggplot(January_netWB_Mm3_melt, aes(x = as.Date(Date, format = '%Y-%m-%d'), y = value)) +
#     geom_bar(aes(fill = variable),position = 'dodge', stat = 'identity', color = 'black') +
#     scale_x_date(limits = c(as.Date('Oct-01-1990', format = '%b-%m-%y'), as.Date('Oct-01-2011', format = '%b-%m-%y')),
#                  breaks = seq(as.Date("1990/10/1"), by = "year", length.out = 22), expand = c(0.01,0.01),
#                  date_labels = ('%b-%y'))  +
#     scale_y_continuous(limits = c(-40,40), breaks = seq(-40,40,by = 10), expand = c(0,0)) +
#     xlab('') +
#     ylab(bquote('Volume ('*Mm^3*')')) +
#     ggtitle('January Volumetric Flux') +
#     theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#           panel.background = element_rect(color = 'black', fill = NA),
#           axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), plot.title = element_text(hjust = 0.5),
#           legend.position = c(0.5, 0.07), legend.key = element_rect(fill = NA, color = NA),
#           legend.background = element_rect(fill = NA, color = 'black', size = 0.5),
#           legend.direction = 'horizontal',legend.text = element_text(size=8),
#           legend.margin = margin(1,2,1,1)))
# graphics.off()
# 
# ################################################################################################
# 
# January_netWB_TAF_melt = melt(January_netWB_TAF,id.vars = 'Date')
# png(paste0(out_dir,'/January_netWB_TAF.png'), width = 6, height = 4, units = 'in',  res = 600)
# (January_netWB_TAF_Plot = ggplot(January_netWB_TAF_melt, aes(x = as.Date(Date, format = '%Y-%m-%d'), y = value)) +
#     geom_bar(aes(fill = variable),position = 'dodge', stat = 'identity', color = 'black') +
#     scale_x_date(limits = c(as.Date('Oct-01-1990', format = '%b-%m-%y'), as.Date('Oct-01-2011', format = '%b-%m-%y')),
#                  breaks = seq(as.Date("1990/10/1"), by = "year", length.out = 22), expand = c(0.01,0.01),
#                  date_labels = ('%b-%y'))  +
#     scale_y_continuous(limits = c(-35,35), breaks = seq(-35,35,by = 5), expand = c(0,0)) +
#     xlab('') +
#     ylab('Volume (TAF)') +
#     ggtitle('January Volumetric Flux') +
#     theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#           panel.background = element_rect(color = 'black', fill = NA),
#           axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), plot.title = element_text(hjust = 0.5),
#           legend.position = c(0.5, 0.07), legend.key = element_rect(fill = NA, color = NA),
#           legend.background = element_rect(fill = NA, color = 'black', size = 0.5),
#           legend.direction = 'horizontal',legend.text = element_text(size=8),
#           legend.margin = margin(1,2,1,1)))
# graphics.off()
# 
# #################################################################################################
# 
# February_netWB_Mm3_melt = melt(February_netWB_m3,id.vars = 'Date')
# February_netWB_Mm3_melt$value = February_netWB_Mm3_melt$value / 1E6   #Convert to Mm3 for plotting
# png(paste0(out_dir,'/February_netWB_Mm3.png'), width = 6, height = 4, units = 'in',  res = 600)
# (February_netWB_m3_Plot = ggplot(February_netWB_Mm3_melt, aes(x = as.Date(Date, format = '%Y-%m-%d'), y = value)) +
#     geom_bar(aes(fill = variable),position = 'dodge', stat = 'identity', color = 'black') +
#     scale_x_date(limits = c(as.Date('Oct-01-1990', format = '%b-%m-%y'), as.Date('Oct-01-2011', format = '%b-%m-%y')),
#                  breaks = seq(as.Date("1990/10/1"), by = "year", length.out = 22), expand = c(0.01,0.01),
#                  date_labels = ('%b-%y'))  +
#     scale_y_continuous(limits = c(-50,50), breaks = seq(-50,50,by = 25), expand = c(0,0)) +
#     xlab('') +
#     ylab(bquote('Volume ('*m^3*')')) +
#     ggtitle('February Volumetric Flux') +
#     theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#           panel.background = element_rect(color = 'black', fill = NA),
#           axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), plot.title = element_text(hjust = 0.5),
#           legend.position = c(0.5, 0.07), legend.key = element_rect(fill = NA, color = NA),
#           legend.background = element_rect(fill = NA, color = 'black', size = 0.5),
#           legend.direction = 'horizontal',legend.text = element_text(size=8),
#           legend.margin = margin(1,2,1,1)))
# graphics.off()
# 
# ################################################################################################
# 
# February_netWB_TAF_melt = melt(February_netWB_TAF,id.vars = 'Date')
# png(paste0(out_dir,'/February_netWB_TAF.png'), width = 6, height = 4, units = 'in',  res = 600)
# (February_netWB_TAF_Plot = ggplot(February_netWB_TAF_melt, aes(x = as.Date(Date, format = '%Y-%m-%d'), y = value)) +
#     geom_bar(aes(fill = variable),position = 'dodge', stat = 'identity', color = 'black') +
#     scale_x_date(limits = c(as.Date('Oct-01-1990', format = '%b-%m-%y'), as.Date('Oct-01-2011', format = '%b-%m-%y')),
#                  breaks = seq(as.Date("1990/10/1"), by = "year", length.out = 22), expand = c(0.01,0.01),
#                  date_labels = ('%b-%y'))  +
#     scale_y_continuous(limits = c(-35,35), breaks = seq(-35,35,by = 5), expand = c(0,0)) +
#     xlab('') +
#     ylab('Volume (TAF)') +
#     ggtitle('February Volumetric Flux') +
#     theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#           panel.background = element_rect(color = 'black', fill = NA),
#           axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), plot.title = element_text(hjust = 0.5),
#           legend.position = c(0.5, 0.07), legend.key = element_rect(fill = NA, color = NA),
#           legend.background = element_rect(fill = NA, color = 'black', size = 0.5),
#           legend.direction = 'horizontal',legend.text = element_text(size=8),
#           legend.margin = margin(1,2,1,1)))
# graphics.off()
# 
# #################################################################################################
# 
# March_netWB_Mm3_melt = melt(March_netWB_m3,id.vars = 'Date')
# March_netWB_Mm3_melt$value = March_netWB_Mm3_melt$value / 1E6   #Convert to Mm3 for plotting
# png(paste0(out_dir,'/March_netWB_Mm3.png'), width = 6, height = 4, units = 'in',  res = 600)
# (March_netWB_m3_Plot = ggplot(March_netWB_Mm3_melt, aes(x = as.Date(Date, format = '%Y-%m-%d'), y = value)) +
#     geom_bar(aes(fill = variable),position = 'dodge', stat = 'identity', color = 'black') +
#     scale_x_date(limits = c(as.Date('Oct-01-1990', format = '%b-%m-%y'), as.Date('Oct-01-2011', format = '%b-%m-%y')),
#                  breaks = seq(as.Date("1990/10/1"), by = "year", length.out = 22), expand = c(0.01,0.01),
#                  date_labels = ('%b-%y'))  +
#     scale_y_continuous(limits = c(-15,15), breaks = seq(-15,15,by = 5), expand = c(0,0)) +
#     xlab('') +
#     ylab(bquote('Volume ('*Mm^3*')')) +
#     ggtitle('March Volumetric Flux') +
#     theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#           panel.background = element_rect(color = 'black', fill = NA),
#           axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), plot.title = element_text(hjust = 0.5),
#           legend.position = c(0.5, 0.93), legend.key = element_rect(fill = NA, color = NA),
#           legend.background = element_rect(fill = NA, color = 'black', size = 0.5),
#           legend.direction = 'horizontal',legend.text = element_text(size=8),
#           legend.margin = margin(1,2,1,1)))
# graphics.off()
# 
# ################################################################################################
# 
# March_netWB_TAF_melt = melt(March_netWB_TAF,id.vars = 'Date')
# png(paste0(out_dir,'/March_netWB_TAF.png'), width = 6, height = 4, units = 'in',  res = 600)
# (March_netWB_TAF_Plot = ggplot(March_netWB_TAF_melt, aes(x = as.Date(Date, format = '%Y-%m-%d'), y = value)) +
#     geom_bar(aes(fill = variable),position = 'dodge', stat = 'identity', color = 'black') +
#     scale_x_date(limits = c(as.Date('Oct-01-1990', format = '%b-%m-%y'), as.Date('Oct-01-2011', format = '%b-%m-%y')),
#                  breaks = seq(as.Date("1990/10/1"), by = "year", length.out = 22), expand = c(0.01,0.01),
#                  date_labels = ('%b-%y'))  +
#     scale_y_continuous(limits = c(-10,10), breaks = seq(-10,10,by = 2), expand = c(0,0)) +
#     xlab('') +
#     ylab('Volume (TAF)') +
#     ggtitle('March Volumetric Flux') +
#     theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#           panel.background = element_rect(color = 'black', fill = NA),
#           axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), plot.title = element_text(hjust = 0.5),
#           legend.position = c(0.5, 0.93), legend.key = element_rect(fill = NA, color = NA),
#           legend.background = element_rect(fill = NA, color = 'black', size = 0.5),
#           legend.direction = 'horizontal',legend.text = element_text(size=8),
#           legend.margin = margin(1,2,1,1)))
# graphics.off()
# 
# #################################################################################################
# 
# April_netWB_Mm3_melt = melt(April_netWB_m3,id.vars = 'Date')
# April_netWB_Mm3_melt$value = April_netWB_Mm3_melt$value / 1E6   #Convert to Mm3 for plotting
# png(paste0(out_dir,'/April_netWB_Mm3.png'), width = 6, height = 4, units = 'in',  res = 600)
# (April_netWB_m3_Plot = ggplot(April_netWB_Mm3_melt, aes(x = as.Date(Date, format = '%Y-%m-%d'), y = value)) +
#     geom_bar(aes(fill = variable),position = 'dodge', stat = 'identity', color = 'black') +
#     scale_x_date(limits = c(as.Date('Oct-01-1990', format = '%b-%m-%y'), as.Date('Oct-01-2011', format = '%b-%m-%y')),
#                  breaks = seq(as.Date("1990/10/1"), by = "year", length.out = 22), expand = c(0.01,0.01),
#                  date_labels = ('%b-%y'))  +
#     scale_y_continuous(limits = c(-15,15), breaks = seq(-15,15,by = 5), expand = c(0,0)) +
#     xlab('') +
#     ylab(bquote('Volume ('*Mm^3*')')) +
#     ggtitle('April Volumetric Flux') +
#     theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#           panel.background = element_rect(color = 'black', fill = NA),
#           axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), plot.title = element_text(hjust = 0.5),
#           legend.position = c(0.5, 0.93), legend.key = element_rect(fill = NA, color = NA),
#           legend.background = element_rect(fill = NA, color = 'black', size = 0.5),
#           legend.direction = 'horizontal',legend.text = element_text(size=8),
#           legend.margin = margin(1,2,1,1)))
# graphics.off()
# 
# ################################################################################################
# 
# April_netWB_TAF_melt = melt(April_netWB_TAF,id.vars = 'Date')
# png(paste0(out_dir,'/April_netWB_TAF.png'), width = 6, height = 4, units = 'in',  res = 600)
# (April_netWB_TAF_Plot = ggplot(April_netWB_TAF_melt, aes(x = as.Date(Date, format = '%Y-%m-%d'), y = value)) +
#     geom_bar(aes(fill = variable),position = 'dodge', stat = 'identity', color = 'black') +
#     scale_x_date(limits = c(as.Date('Oct-01-1990', format = '%b-%m-%y'), as.Date('Oct-01-2011', format = '%b-%m-%y')),
#                  breaks = seq(as.Date("1990/10/1"), by = "year", length.out = 22), expand = c(0.01,0.01),
#                  date_labels = ('%b-%y'))  +
#     scale_y_continuous(limits = c(-15,15), breaks = seq(-15,15,by = 5), expand = c(0,0)) +
#     xlab('') +
#     ylab('Volume (TAF)') +
#     ggtitle('April Volumetric Flux') +
#     theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#           panel.background = element_rect(color = 'black', fill = NA),
#           axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), plot.title = element_text(hjust = 0.5),
#           legend.position = c(0.5, 0.93), legend.key = element_rect(fill = NA, color = NA),
#           legend.background = element_rect(fill = NA, color = 'black', size = 0.5),
#           legend.direction = 'horizontal',legend.text = element_text(size=8),
#           legend.margin = margin(1,2,1,1), legend.key.size = unit(0.25,'inches')))
# graphics.off()
# 
# #################################################################################################
# 
# May_netWB_Mm3_melt = melt(May_netWB_m3,id.vars = 'Date')
# May_netWB_Mm3_melt$value = May_netWB_Mm3_melt$value / 1E6   #Convert to Mm3 for plotting
# png(paste0(out_dir,'/May_netWB_Mm3.png'), width = 6, height = 4, units = 'in',  res = 600)
# (May_netWB_m3_Plot = ggplot(May_netWB_Mm3_melt, aes(x = as.Date(Date, format = '%Y-%m-%d'), y = value)) +
#     geom_bar(aes(fill = variable),position = 'dodge', stat = 'identity', color = 'black') +
#     scale_x_date(limits = c(as.Date('Oct-01-1990', format = '%b-%m-%y'), as.Date('Oct-01-2011', format = '%b-%m-%y')),
#                  breaks = seq(as.Date("1990/10/1"), by = "year", length.out = 22), expand = c(0.01,0.01),
#                  date_labels = ('%b-%y'))  +
#     scale_y_continuous(limits = c(-10,10), breaks = seq(-10,10,by = 2), expand = c(0,0)) +
#     xlab('') +
#     ylab(bquote('Volume ('*Mm^3*')')) +
#     ggtitle('May Volumetric Flux') +
#     theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#           panel.background = element_rect(color = 'black', fill = NA),
#           axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), plot.title = element_text(hjust = 0.5),
#           legend.position = c(0.5, 0.93), legend.key = element_rect(fill = NA, color = NA),
#           legend.background = element_rect(fill = NA, color = 'black', size = 0.5),
#           legend.direction = 'horizontal',legend.text = element_text(size=8),
#           legend.margin = margin(1,2,1,1)))
# graphics.off()
# 
# ################################################################################################
# 
# May_netWB_TAF_melt = melt(May_netWB_TAF,id.vars = 'Date')
# png(paste0(out_dir,'/May_netWB_TAF.png'), width = 6, height = 4, units = 'in',  res = 600)
# (May_netWB_TAF_Plot = ggplot(May_netWB_TAF_melt, aes(x = as.Date(Date, format = '%Y-%m-%d'), y = value)) +
#     geom_bar(aes(fill = variable),position = 'dodge', stat = 'identity', color = 'black') +
#     scale_x_date(limits = c(as.Date('Oct-01-1990', format = '%b-%m-%y'), as.Date('Oct-01-2011', format = '%b-%m-%y')),
#                  breaks = seq(as.Date("1990/10/1"), by = "year", length.out = 22), expand = c(0.01,0.01),
#                  date_labels = ('%b-%y'))  +
#     scale_y_continuous(limits = c(-10,10), breaks = seq(-10,10,by = 2), expand = c(0,0)) +
#     xlab('') +
#     ylab('Volume (TAF)') +
#     ggtitle('May Volumetric Flux') +
#     theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#           panel.background = element_rect(color = 'black', fill = NA),
#           axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), plot.title = element_text(hjust = 0.5),
#           legend.position = c(0.5, 0.93), legend.key = element_rect(fill = NA, color = NA),
#           legend.background = element_rect(fill = NA, color = 'black', size = 0.5),
#           legend.direction = 'horizontal',legend.text = element_text(size=8),
#           legend.margin = margin(1,2,1,1), legend.key.size = unit(0.25,'inches')))
# graphics.off()
# 
# #################################################################################################
# 
# June_netWB_Mm3_melt = melt(June_netWB_m3,id.vars = 'Date')
# June_netWB_Mm3_melt$value = June_netWB_Mm3_melt$value / 1E6   #Convert to Mm3 for plotting
# png(paste0(out_dir,'/June_netWB_Mm3.png'), width = 6, height = 4, units = 'in',  res = 600)
# (June_netWB_m3_Plot = ggplot(June_netWB_Mm3_melt, aes(x = as.Date(Date, format = '%Y-%m-%d'), y = value)) +
#     geom_bar(aes(fill = variable),position = 'dodge', stat = 'identity', color = 'black') +
#     scale_x_date(limits = c(as.Date('Oct-01-1990', format = '%b-%m-%y'), as.Date('Oct-01-2011', format = '%b-%m-%y')),
#                  breaks = seq(as.Date("1990/10/1"), by = "year", length.out = 22), expand = c(0.01,0.01),
#                  date_labels = ('%b-%y'))  +
#     scale_y_continuous(limits = c(-15,15), breaks = seq(-15,15,by = 5), expand = c(0,0)) +
#     xlab('') +
#     ylab(bquote('Volume ('*Mm^3*')')) +
#     ggtitle('June Volumetric Flux') +
#     theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#           panel.background = element_rect(color = 'black', fill = NA),
#           axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), plot.title = element_text(hjust = 0.5),
#           legend.position = c(0.5, 0.93), legend.key = element_rect(fill = NA, color = NA),
#           legend.background = element_rect(fill = NA, color = 'black', size = 0.5),
#           legend.direction = 'horizontal',legend.text = element_text(size=8),
#           legend.margin = margin(1,2,1,1)))
# graphics.off()
# 
# ################################################################################################
# 
# June_netWB_TAF_melt = melt(June_netWB_TAF,id.vars = 'Date')
# png(paste0(out_dir,'/June_netWB_TAF.png'), width = 6, height = 4, units = 'in',  res = 600)
# (June_netWB_TAF_Plot = ggplot(June_netWB_TAF_melt, aes(x = as.Date(Date, format = '%Y-%m-%d'), y = value)) +
#     geom_bar(aes(fill = variable),position = 'dodge', stat = 'identity', color = 'black') +
#     scale_x_date(limits = c(as.Date('Oct-01-1990', format = '%b-%m-%y'), as.Date('Oct-01-2011', format = '%b-%m-%y')),
#                  breaks = seq(as.Date("1990/10/1"), by = "year", length.out = 22), expand = c(0.01,0.01),
#                  date_labels = ('%b-%y'))  +
#     scale_y_continuous(limits = c(-10,10), breaks = seq(-10,10,by = 2), expand = c(0,0)) +
#     xlab('') +
#     ylab('Volume (TAF)') +
#     ggtitle('June Volumetric Flux') +
#     theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#           panel.background = element_rect(color = 'black', fill = NA),
#           axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), plot.title = element_text(hjust = 0.5),
#           legend.position = c(0.5, 0.93), legend.key = element_rect(fill = NA, color = NA),
#           legend.background = element_rect(fill = NA, color = 'black', size = 0.5),
#           legend.direction = 'horizontal',legend.text = element_text(size=8),
#           legend.margin = margin(1,2,1,1), legend.key.size = unit(0.25,'inches')))
# graphics.off()
# 
# #################################################################################################
# 
# July_netWB_Mm3_melt = melt(July_netWB_m3,id.vars = 'Date')
# July_netWB_Mm3_melt$value = July_netWB_Mm3_melt$value / 1E6   #Convert to Mm3 for plotting
# png(paste0(out_dir,'/July_netWB_Mm3.png'), width = 6, height = 4, units = 'in',  res = 600)
# (July_netWB_m3_Plot = ggplot(July_netWB_Mm3_melt, aes(x = as.Date(Date, format = '%Y-%m-%d'), y = value)) +
#     geom_bar(aes(fill = variable),position = 'dodge', stat = 'identity', color = 'black') +
#     scale_x_date(limits = c(as.Date('Oct-01-1990', format = '%b-%m-%y'), as.Date('Oct-01-2011', format = '%b-%m-%y')),
#                  breaks = seq(as.Date("1990/10/1"), by = "year", length.out = 22), expand = c(0.01,0.01),
#                  date_labels = ('%b-%y'))  +
#     scale_y_continuous(limits = c(-20,20), breaks = seq(-20,20,by = 5), expand = c(0,0)) +
#     xlab('') +
#     ylab(bquote('Volume ('*Mm^3*')')) +
#     ggtitle('July Volumetric Flux') +
#     theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#           panel.background = element_rect(color = 'black', fill = NA),
#           axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), plot.title = element_text(hjust = 0.5),
#           legend.position = c(0.5, 0.93), legend.key = element_rect(fill = NA, color = NA),
#           legend.background = element_rect(fill = NA, color = 'black', size = 0.5),
#           legend.direction = 'horizontal',legend.text = element_text(size=8),
#           legend.margin = margin(1,2,1,1)))
# graphics.off()
# 
# ################################################################################################
# 
# July_netWB_TAF_melt = melt(August_netWB_TAF,id.vars = 'Date')
# png(paste0(out_dir,'/August_netWB_TAF.png'), width = 6, height = 4, units = 'in',  res = 600)
# (July_netWB_TAF_Plot = ggplot(July_netWB_TAF_melt, aes(x = as.Date(Date, format = '%Y-%m-%d'), y = value)) +
#     geom_bar(aes(fill = variable),position = 'dodge', stat = 'identity', color = 'black') +
#     scale_x_date(limits = c(as.Date('Oct-01-1990', format = '%b-%m-%y'), as.Date('Oct-01-2011', format = '%b-%m-%y')),
#                  breaks = seq(as.Date("1990/10/1"), by = "year", length.out = 22), expand = c(0.01,0.01),
#                  date_labels = ('%b-%y'))  +
#     scale_y_continuous(limits = c(-15,15), breaks = seq(-15,15,by = 5), expand = c(0,0)) +
#     xlab('') +
#     ylab('Volume (TAF)') +
#     ggtitle('July Volumetric Flux') +
#     theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#           panel.background = element_rect(color = 'black', fill = NA),
#           axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), plot.title = element_text(hjust = 0.5),
#           legend.position = c(0.5, 0.93), legend.key = element_rect(fill = NA, color = NA),
#           legend.background = element_rect(fill = NA, color = 'black', size = 0.5),
#           legend.direction = 'horizontal',legend.text = element_text(size=8),
#           legend.margin = margin(1,2,1,1), legend.key.size = unit(0.25,'inches')))
# graphics.off()
# 
# #################################################################################################
# 
# August_netWB_Mm3_melt = melt(August_netWB_m3,id.vars = 'Date')
# August_netWB_Mm3_melt$value = August_netWB_Mm3_melt$value / 1E6   #Convert to Mm3 for plotting
# png(paste0(out_dir,'/August_netWB_Mm3.png'), width = 6, height = 4, units = 'in',  res = 600)
# (August_netWB_m3_Plot = ggplot(August_netWB_Mm3_melt, aes(x = as.Date(Date, format = '%Y-%m-%d'), y = value)) +
#     geom_bar(aes(fill = variable),position = 'dodge', stat = 'identity', color = 'black') +
#     scale_x_date(limits = c(as.Date('Oct-01-1990', format = '%b-%m-%y'), as.Date('Oct-01-2011', format = '%b-%m-%y')),
#                  breaks = seq(as.Date("1990/10/1"), by = "year", length.out = 22), expand = c(0.01,0.01),
#                  date_labels = ('%b-%y'))  +
#     scale_y_continuous(limits = c(-20,20), breaks = seq(-20,20,by = 5), expand = c(0,0)) +
#     xlab('') +
#     ylab(bquote('Volume ('*Mm^3*')')) +
#     ggtitle('August Volumetric Flux') +
#     theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#           panel.background = element_rect(color = 'black', fill = NA),
#           axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), plot.title = element_text(hjust = 0.5),
#           legend.position = c(0.5, 0.93), legend.key = element_rect(fill = NA, color = NA),
#           legend.background = element_rect(fill = NA, color = 'black', size = 0.5),
#           legend.direction = 'horizontal',legend.text = element_text(size=8),
#           legend.margin = margin(1,2,1,1)))
# graphics.off()
# 
# ################################################################################################
# 
# August_netWB_TAF_melt = melt(August_netWB_TAF,id.vars = 'Date')
# png(paste0(out_dir,'/August_netWB_TAF.png'), width = 6, height = 4, units = 'in',  res = 600)
# (August_netWB_TAF_Plot = ggplot(August_netWB_TAF_melt, aes(x = as.Date(Date, format = '%Y-%m-%d'), y = value)) +
#     geom_bar(aes(fill = variable),position = 'dodge', stat = 'identity', color = 'black') +
#     scale_x_date(limits = c(as.Date('Oct-01-1990', format = '%b-%m-%y'), as.Date('Oct-01-2011', format = '%b-%m-%y')),
#                  breaks = seq(as.Date("1990/10/1"), by = "year", length.out = 22), expand = c(0.01,0.01),
#                  date_labels = ('%b-%y'))  +
#     scale_y_continuous(limits = c(-15,15), breaks = seq(-15,15,by = 5), expand = c(0,0)) +
#     xlab('') +
#     ylab('Volume (TAF)') +
#     ggtitle('August Volumetric Flux') +
#     theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#           panel.background = element_rect(color = 'black', fill = NA),
#           axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), plot.title = element_text(hjust = 0.5),
#           legend.position = c(0.5, 0.93), legend.key = element_rect(fill = NA, color = NA),
#           legend.background = element_rect(fill = NA, color = 'black', size = 0.5),
#           legend.direction = 'horizontal',legend.text = element_text(size=8),
#           legend.margin = margin(1,2,1,1), legend.key.size = unit(0.25,'inches')))
# graphics.off()
# 
# ################################################################################################
# 
# September_netWB_Mm3_melt = melt(September_netWB_m3,id.vars = 'Date')
# September_netWB_Mm3_melt$value = September_netWB_Mm3_melt$value / 1E6   #Convert to Mm3 for plotting
# png(paste0(out_dir,'/September_netWB_Mm3.png'), width = 6, height = 4, units = 'in',  res = 600)
# (September_netWB_m3_Plot = ggplot(September_netWB_Mm3_melt, aes(x = as.Date(Date, format = '%Y-%m-%d'), y = value)) +
#     geom_bar(aes(fill = variable),position = 'dodge', stat = 'identity', color = 'black') +
#     scale_x_date(limits = c(as.Date('Oct-01-1990', format = '%b-%m-%y'), as.Date('Oct-01-2011', format = '%b-%m-%y')),
#                  breaks = seq(as.Date("1990/10/1"), by = "year", length.out = 22), expand = c(0.01,0.01),
#                  date_labels = ('%b-%y'))  +
#     scale_y_continuous(limits = c(-20,20), breaks = seq(-20,20,by = 5), expand = c(0,0)) +
#     xlab('') +
#     ylab(bquote('Volume ('*Mm^3*')')) +
#     ggtitle('September Volumetric Flux') +
#     theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#           panel.background = element_rect(color = 'black', fill = NA),
#           axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), plot.title = element_text(hjust = 0.5),
#           legend.position = c(0.5, 0.93), legend.key = element_rect(fill = NA, color = NA),
#           legend.background = element_rect(fill = NA, color = 'black', size = 0.5),
#           legend.direction = 'horizontal',legend.text = element_text(size=8),
#           legend.margin = margin(1,2,1,1)))
# graphics.off()
# 
# ################################################################################################
# 
# September_netWB_TAF_melt = melt(September_netWB_TAF,id.vars = 'Date')
# png(paste0(out_dir,'/September_netWB_TAF.png'), width = 6, height = 4, units = 'in',  res = 600)
# (September_netWB_TAF_Plot = ggplot(September_netWB_TAF_melt, aes(x = as.Date(Date, format = '%Y-%m-%d'), y = value)) +
#     geom_bar(aes(fill = variable),position = 'dodge', stat = 'identity', color = 'black') +
#     scale_x_date(limits = c(as.Date('Oct-01-1990', format = '%b-%m-%y'), as.Date('Oct-01-2011', format = '%b-%m-%y')),
#                  breaks = seq(as.Date("1990/10/1"), by = "year", length.out = 22), expand = c(0.01,0.01),
#                  date_labels = ('%b-%y'))  +
#     scale_y_continuous(limits = c(-15,15), breaks = seq(-15,15,by = 5), expand = c(0,0)) +
#     xlab('') +
#     ylab('Volume (TAF)') +
#     ggtitle('September Volumetric Flux') +
#     theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#           panel.background = element_rect(color = 'black', fill = NA),
#           axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), plot.title = element_text(hjust = 0.5),
#           legend.position = c(0.5, 0.93), legend.key = element_rect(fill = NA, color = NA),
#           legend.background = element_rect(fill = NA, color = 'black', size = 0.5),
#           legend.direction = 'horizontal',legend.text = element_text(size=8),
#           legend.margin = margin(1,2,1,1), legend.key.size = unit(0.25,'inches')))
# graphics.off()