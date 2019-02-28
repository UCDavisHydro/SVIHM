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
#library(data.table)
dir.create(file.path(getwd(),'Results'), showWarnings = FALSE)   #Create Results directory if it doesn't exist
out_dir = paste0(getwd(),'/Results')
source('Read_LST.R')

###########################################################################################
########################                 USER INPUT                 #######################
###########################################################################################
LST_Name = 'SVIHM.lst'
LST_MAR_Name = 'SVIHM_MAR.lst'
LST_ILR_Name = 'SVIHM_ILR.lst'
LST_MAR_ILR_Name = 'SVIHM_MAR_ILR.lst'

RCH_Name = 'SVIHM.rch'
RCH_MAR_Name = 'SVIHM_MAR.rch'
RCH_ILR_Name = 'SVIHM_ILR.rch'
RCH_MAR_ILR_Name = 'SVIHM_MAR_ILR.rch'

WB_Components_MODFLOW = c('STORAGE', 'CONSTANT_HEAD', 'WELLS', 'RECHARGE', 'ET_SEGMENTS','STREAM_LEAKAGE', 'DRAINS')
nstress = 252
PRINT_BUDGET = TRUE             # Print monthly water budget to file (TRUE/FALSE)
Print_SWBM_by_landuse = TRUE    # Print 21 year average, dry year (2001), average year (2010), and wet year (2006) SWBM by landuse
COMPARE_MAR_BUDGET = FALSE       # Compare basecase budget with MAR scenario water budget (TRUE/FALSE)
COMPARE_ILR_BUDGET = FALSE       # Compare basecase budget with ILR scenario water budget (TRUE/FALSE)
COMPARE_MAR_ILR_BUDGET = FALSE   # Compare basecase budget with MAR_ILR scenario water budget (TRUE/FALSE)
CHK_RCH = FALSE                   # Compare basecase recharge output by SWBM, input to MODFLOW, and ultimately used by MODFLOW (TRUE/FALSE)
CHK_RCH_MAR = FALSE              # Compare MAR recharge output by SWBM, input to MODFLOW, and ultimately used by MODFLOW (TRUE/FALSE)
CHK_RCH_ILR = FALSE              # Compare ILR recharge output by SWBM, input to MODFLOW, and ultimately used by MODFLOW (TRUE/FALSE)
CHK_RCH_MAR_ILR = FALSE          # Compare MAR_ILR recharge output by SWBM, input to MODFLOW, and ultimately used by MODFLOW (TRUE/FALSE)
StartingMonths = seq(as.Date("1990/10/1"), by = "month", length.out = 252)
Dry_Avg_Wet_Yrs = c(2001,2010,2006)

###############################################################################################
#############                    IMPORT SWBM BUDGET                   #########################
###############################################################################################
SWBM_Terms = c('Precipitation', 'SW Irrigation', 'GW Irrigation', 'ET', 'Recharge', 'Storage')
SWBM_Budget_Monthly = read.table('monthly_water_budget.dat', header = T)
names(SWBM_Budget_Monthly) = c('Month',SWBM_Terms)

if (Print_SWBM_by_landuse==T){
  source('SWBM_Landuse.R')
  budget_terms = c('pET', 'aET', 'irrigation', 'surfacewater', 'groundwater', 'recharge', 'deficiency')
  header_names = c('pET', 'aET', 'Irrigation', 'SW_Irrigation', 'GW_Irrigation', 'GW_Recharge', 'Deficiency')
  landuse_cats = c('Alfalfa','Grain','Pasture','ET/NoIrr','NoET/NoIrr')
  SWBM_output_by_landuse(budget_terms, landuse_cats, header_names)
}

###############################################################################################
#############                   IMPORT MODFLOW BUDGET                 #########################
###############################################################################################
MODFLOW_Budget(LST_Name, WB_Components_MODFLOW)
if (PRINT_BUDGET==TRUE){
write.table(MODFLOW_Budget_Monthly, file = paste0(out_dir,'/MODFLOW_Water_Budget.dat'), row.names = F, quote = F)
}

###############################################################################################
#############        COMPARE RECHARGE BETWEEN SWBM AND MODFLOW        #########################
###############################################################################################
if (CHK_RCH==TRUE){
#COMPARE SWBM -> RCH File
#Compare RCH File -> MODFLOW Budget
#Compare SWBM - MODFLOW Budget
  numdays = as.numeric(diff(seq(as.Date('1990-10-1'), by = 'month', length.out = 253)))
  rch_text = readLines('SVIHM.rch')
  rch_start = grep('10e14.6', rch_text)
  rch_in = array(data = NA, dim = 252)
  
  #####
  rch_out_monthly = read.table('monthly_recharge_volume.dat', skip = 2, header = F, row.names = 1)
  names(rch_out_monthly) = as.character(seq(1,2119))
  rch_zones = as.matrix(read.table('Recharge_Zones_SVIHM.txt', header = F))
  #####
  
  SWBM_RCH_pct_diff = array(data = NA, dim = 252)
  RCH_LST_pct_diff = array(data = NA, dim = 252)
  SWBM_LST_pct_diff = array(data = NA, dim = 252)
  for (i in 1:252) {
    eval(parse(text = paste0("rch_in[",i,"] = sum(matrix(as.numeric(unlist(lapply(strsplit(rch_text[(rch_start[",i,"]+1):(rch_start[",i,"]+9240)],' '),function(x){x[!x =='']}))),nrow = 440,ncol=210,byrow = T))*100*100*numdays[",i,"]")))
    eval(parse(text = paste0("SWBM_RCH_pct_diff[",i,"] = ((SWBM_Budget_Monthly$Recharge[",i,"] + rch_in[",i,"])/-SWBM_Budget_Monthly$Recharge[",i,"])*100")))
    eval(parse(text = paste0("RCH_LST_pct_diff[",i,"] = ((rch_in[",i,"] - MODFLOW_Budget_Monthly$RECHARGE_net_m3[",i,"])/rch_in[",i,"])*100")))
    eval(parse(text = paste0("SWBM_LST_pct_diff[",i,"] = ((SWBM_Budget_Monthly$Recharge[",i,"] + MODFLOW_Budget_Monthly$RECHARGE_net_m3[",i,"])/-SWBM_Budget_Monthly$Recharge[",i,"])*100")))
  }
  basecase_RCH_pct_diffs = data.frame(Month = StartingMonths,
                                  SWBM_RCH = SWBM_RCH_pct_diff,
                                  RCH_LST = RCH_LST_pct_diff,
                                  SWBM_LST = SWBM_LST_pct_diff)
  basecase_RCH_pct_diffs_melt = melt(basecase_RCH_pct_diffs, id.vars = 'Month')
  (Recharge_Percent_Diffs_Plot = ggplot(data = basecase_RCH_pct_diffs, aes(x = Month)) +
      geom_line(aes(y = SWBM_RCH), color = 'red') +
      geom_line(aes(y = RCH_LST), color = 'black') +
      geom_line(aes(y = SWBM_LST), color = 'blue')
    
  )
}
##############################################################################################
##############             CREATE MONTHLY WATER BUDGETS           ############################
##############################################################################################

#SWBM
SWBM_Budget_Monthly$Month = format(seq(as.Date("1990/10/1"), by = "month", length.out = 252),'%b-%Y')
SWBM_Budget_Monthly$WY = rep(seq(1991,2011),each = 12)

#MODFLOW
month_abbv = format(seq(as.Date('1990/10/1'), as.Date('1991/9/30'), by = 'month'),'%b')
MODFLOW_Budget_Monthly$Month = strtrim(MODFLOW_Budget_Monthly$Month,3)
for (i in 1:12){
  eval(parse(text = paste0("Water_Budget_",month_abbv[i]," = subset(MODFLOW_Budget_Monthly, select = paste0(WB_Components_MODFLOW,'_net_m3'), Month == '",month_abbv[i],"')")))
  eval(parse(text = paste0('Water_Budget_',month_abbv[i],'$WY = seq(1991,2011)')))
}
##############################################################################################
##############             CREATE ANNUAL WATER BUDGETS            ############################
##############################################################################################

#SWBM
SWBM_Budget_Annual = aggregate(.~WY,SWBM_Budget_Monthly[,!names(SWBM_Budget_Monthly)%in%'Month'], FUN = sum)

#MODFLOW
Water_Budget_MODFLOW_Annual = subset(MODFLOW_Budget_Monthly, select = paste0(WB_Components_MODFLOW,'_net_m3'))
Water_Budget_MODFLOW_Annual$Water_Year = rep(seq(1991,2011),each = 12)
Water_Budget_MODFLOW_Annual = aggregate(.~Water_Year, Water_Budget_MODFLOW_Annual, FUN = sum)

##############################################################################################
##############################################################################################
##############                       PLOTS                        ############################
##############################################################################################
##############################################################################################
SWBM_flux_labels = c('Precipitation','ET','SW Irrigation', 'Recharge','GW Irrigation','Storage')
MODFLOW_flux_labels = c('Recharge','ET','Storage','Drains','Stream Leakage','Wells', 'Canal Seepage/MFR')

SWBM_colors = c('lightblue1', 'red', 'darkcyan', 'mediumblue','darkgreen', 'goldenrod' )
MODFLOW_colors = c('mediumblue', 'red', 'goldenrod', 'mediumorchid2','dodgerblue1', 'darkgreen', 'salmon' )
##############################################################################################
###############              MODFLOW MODEL ERROR                  ############################
##############################################################################################
Model_Error_melt = melt(data.frame(Month = seq(as.Date('1990/11/1'), as.Date('2011/10/1'), by = 'month') - 1,
                                   Error_Cumulative_percent = MODFLOW_Budget_Monthly$Error_Cumulative_percent,
                                   Error_Stress_Period_percent = MODFLOW_Budget_Monthly$Error_Stress_Period_percent,
                                   Error_Timestep_percent = MODFLOW_Budget_Monthly$Error_Timestep_percent),
                        id.vars = 'Month')

png(paste0(out_dir,'/Model_Error.png'), width = 6, height = 4, units = 'in',  res = 600)
(Model_Error_Plot = ggplot(Model_Error_melt, aes(x = Month, y = value, col = factor(variable, labels = c('Cumulative', 'Last Time Step','Stress Period')))) +
    geom_line() + 
    scale_color_manual(values=c('red', 'blue', 'black')) + 
    scale_x_date(limits = c(as.Date('Oct-01-1990', format = '%b-%m-%y'), as.Date('Oct-01-2011', format = '%b-%m-%y')),
                 breaks = seq(as.Date("1990/10/1"), by = "year", length.out = 22), expand = c(0.01,0.01),
                 date_labels = ('%b-%y')) + 
    scale_y_continuous(limits = c(-0.5,0.5), breaks = seq(-0.5,0.5,by = 0.1), expand = c(0,0)) +
    labs(x = 'Stress Period', y = 'Model Error (%)') +
    ggtitle('Numerical Error') +
    theme(legend.title=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(color = 'black', fill = NA),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7),
          plot.title = element_text(hjust = 0.5),
          legend.position = c(0.1, 0.82),
          axis.title.x = element_blank(),
          legend.key = element_rect(fill = NA, color = NA),
          legend.background = element_rect(fill = NA)))
graphics.off()

##############################################################################################
###############            SWBM CUMULATIVE NET FLUX               ############################
##############################################################################################
Volumetric_Flux_SWBM_Cumulative_Mm3 = cbind(StartingMonths,cumsum(SWBM_Budget_Monthly[,!names(SWBM_Budget_Monthly)%in%c('WY','Month')]/1E6))
Volumetric_Flux_SWBM_Cumulative_Mm3_melt = melt(Volumetric_Flux_SWBM_Cumulative_Mm3,id.vars = 'StartingMonths')
Volumetric_Flux_SWBM_Cumulative_Mm3_melt$variable = factor(Volumetric_Flux_SWBM_Cumulative_Mm3_melt$variable, levels = SWBM_flux_labels)
Volumetric_Flux_SWBM_Cumulative_Mm3_melt = Volumetric_Flux_SWBM_Cumulative_Mm3_melt[order(Volumetric_Flux_SWBM_Cumulative_Mm3_melt$variable),]

#png(paste0(out_dir,'/Cumulative_Flux_SWBM_Mm3.png'), width = 6, height = 4, units = 'in',  res = 600)
(Cumulative_Volumetric_Flux_SWBM_Mm3_Plot = ggplot(Volumetric_Flux_SWBM_Cumulative_Mm3_melt, aes(x = StartingMonths, y = value, col = factor(variable, labels = SWBM_flux_labels))) +
    geom_line(size = 0.75) + 
    scale_color_manual(values=SWBM_colors) + 
    scale_x_date(limits = c(as.Date('Oct-01-1990', format = '%b-%m-%y'), as.Date('Oct-01-2011', format = '%b-%m-%y')),
                 breaks = seq(as.Date("1990/10/1"), by = "year", length.out = 22), expand = c(0.01,0.01),
                 date_labels = ('%b-%y')) + 
    scale_y_continuous(limits = c(-3000,3000), breaks = seq(-3000,3000,by = 1000), expand = c(0,0)) +
    xlab('') +
    ylab(bquote('Cumulative Volume ('*Mm^3*')')) +
    ggtitle('SWBM Cumulative Flux') +
    theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(color = 'black', fill = NA),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), plot.title = element_text(hjust = 0.5),
          legend.position = c(0.5, 0.08), legend.key = element_rect(fill = NA, color = NA),
          legend.background = element_rect(fill = NA, color = 'black', size = 0.5),
          legend.direction = 'horizontal',legend.text = element_text(size=8),
          legend.margin = margin(1,2,1,1)))
#graphics.off()

#png(paste0(out_dir,'/Cumulative_Flux_SWBM_TAF.png'), width = 6, height = 4, units = 'in',  res = 600)
(Cumulative_Volumetric_Flux_SWBM_TAF_Plot = ggplot(Volumetric_Flux_SWBM_Cumulative_Mm3_melt, aes(x = StartingMonths, y = value*810.714, col = factor(variable, labels = SWBM_flux_labels))) +  # 810.74 is the conversion from Mm3 to TAF
    geom_line(size = 0.75) + 
    scale_color_manual(values=SWBM_colors) + 
    scale_x_date(limits = c(as.Date('Oct-01-1990', format = '%b-%m-%y'), as.Date('Oct-01-2011', format = '%b-%m-%y')),
                 breaks = seq(as.Date("1990/10/1"), by = "year", length.out = 22), expand = c(0.01,0.01),
                 date_labels = ('%b-%y')) + 
    scale_y_continuous(limits = c(-3E6,3E6), breaks = seq(-3E6,3E6,by = 1E6), expand = c(0,0)) +
    xlab('') +
    ylab('Cumulative Volume (TAF)') +
    ggtitle('SWBM Cumulative Flux') +
    theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(color = 'black', fill = NA),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), plot.title = element_text(hjust = 0.5),
          legend.position = c(0.5, 0.08), legend.key = element_rect(fill = NA, color = NA),
          legend.background = element_rect(fill = NA, color = 'black', size = 0.5),
          legend.direction = 'horizontal',legend.text = element_text(size=8),
          legend.margin = margin(1,2,1,1)))
#graphics.off()

##############################################################################################
###############          MODFLOW CUMULATIVE NET FLUX              ############################
##############################################################################################
MODFLOW_Budget_Monthly_Cumulative = data.frame(Month =seq(as.Date('1990/11/1'), as.Date('2011/10/1'),
                                                             by = 'month') - 1,
                                                  Recharge = cumsum(MODFLOW_Budget_Monthly$RECHARGE_net_m3),
                                                  ET = cumsum(MODFLOW_Budget_Monthly$ET_SEGMENTS_net_m3),
                                                  Storage = cumsum(MODFLOW_Budget_Monthly$STORAGE_net_m3),
                                                  Drains = cumsum(MODFLOW_Budget_Monthly$DRAINS_net_m3),
                                                  Stream_Leakage = cumsum(MODFLOW_Budget_Monthly$STREAM_LEAKAGE_net_m3),
                                                  Wells = -cumsum(MODFLOW_Budget_Monthly$WELLS_out_m3),
                                                  Canals_MFR = cumsum(MODFLOW_Budget_Monthly$WELLS_in_m3))
names(MODFLOW_Budget_Monthly_Cumulative) = c('Month',MODFLOW_flux_labels)
MODFLOW_Budget_Monthly_Cumulative_melt = melt(MODFLOW_Budget_Monthly_Cumulative, id.vars = 'Month')
MODFLOW_Budget_Monthly_Cumulative_melt$variable = factor(MODFLOW_Budget_Monthly_Cumulative_melt$variable, levels = MODFLOW_flux_labels)
MODFLOW_Budget_Monthly_Cumulative_melt = MODFLOW_Budget_Monthly_Cumulative_melt[order(MODFLOW_Budget_Monthly_Cumulative_melt$variable),]

#png(paste0(out_dir,'/Cumulative_Flux_MODFLOW_Mm3.png'), width = 6, height = 4, units = 'in',  res = 600)
(Cumulative_MODFLOW_Budget_Monthly_Mm3_Plot = ggplot(MODFLOW_Budget_Monthly_Cumulative_melt, aes(x = Month, y = value/1E6, col = factor(variable, labels = MODFLOW_flux_labels))) +
    geom_line(size = 0.75) + 
    scale_color_manual(values=MODFLOW_colors) + 
    scale_x_date(limits = c(as.Date('Oct-01-1990', format = '%b-%m-%y'), as.Date('Oct-01-2011', format = '%b-%m-%y')),
                 breaks = seq(as.Date("1990/10/1"), by = "year", length.out = 22), expand = c(0.01,0.01),
                 date_labels = ('%b-%y')) + 
    scale_y_continuous(limits = c(-1200,1200), breaks = seq(-1200,1200,by = 300), expand = c(0,0)) +
    xlab('') +
    ylab(bquote('Cumulative Volume ('*Mm^3*')')) +
    ggtitle('MODFLOW Cumulative Flux') +
    theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(color = 'black', fill = NA),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), plot.title = element_text(hjust = 0.5),
          legend.position = c(0.5, 0.08), legend.key = element_rect(fill = NA, color = NA),
          legend.background = element_rect(fill = NA, color = 'black', size = 0.5),
          legend.direction = 'horizontal',legend.text = element_text(size=8),
          legend.margin = margin(1,2,1,1)))
#graphics.off()

#png(paste0(out_dir,'/Cumulative_Flux_MODFLOW_TAF.png'), width = 6, height = 4, units = 'in',  res = 600)
(Cumulative_MODFLOW_Budget_Monthly_TAF_Plot = ggplot(MODFLOW_Budget_Monthly_Cumulative_melt, aes(x = Month, y = value*0.000810714/1000, col = factor(variable, labels = MODFLOW_flux_labels))) +
    geom_line(size = 0.75) + 
    scale_color_manual(values=MODFLOW_colors) + 
    scale_x_date(limits = c(as.Date('Oct-01-1990', format = '%b-%m-%y'), as.Date('Oct-01-2011', format = '%b-%m-%y')),
                 breaks = seq(as.Date("1990/10/1"), by = "year", length.out = 22), expand = c(0.01,0.01),
                 date_labels = ('%b-%y')) + 
    scale_y_continuous(limits = c(-900,900), breaks = seq(-900,900,by = 300), expand = c(0,0)) +
    xlab('') +
    ylab('Cumulative Volume (TAF)') +
    ggtitle('MODFLOW Cumulative Flux') +
    theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(color = 'black', fill = NA),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), plot.title = element_text(hjust = 0.5),
          legend.position = c(0.5, 0.08), legend.key = element_rect(fill = NA, color = NA),
          legend.background = element_rect(fill = NA, color = 'black', size = 0.5),
          legend.direction = 'horizontal',legend.text = element_text(size=8),
          legend.margin = margin(1,2,1,1)))
#graphics.off()

##############################################################################################
##############              SWBM ANNUAL WATER BUDGET PLOT           ##########################
##############################################################################################
SWBM_Budget_Annual_melt = melt(SWBM_Budget_Annual, id.vars = 'WY')
SWBM_Budget_Annual_melt$variable = factor(SWBM_Budget_Annual_melt$variable, levels = SWBM_flux_labels)
SWBM_Budget_Annual_melt = SWBM_Budget_Annual_melt[order(SWBM_Budget_Annual_melt$variable),]

#pdf(paste0(out_dir,'/Water_Budget_Annual_SWBM_Mm3.pdf'), width = 8.5, height = 4)
(SWBM_Annual_Budget_Plot_Mm3 = ggplot(SWBM_Budget_Annual_melt, aes(x = WY, y = value/1E6)) +
    geom_bar(aes(fill = variable), position = "stack", stat = 'identity', color = 'black', width = 0.95, size = 0.1) +
    scale_x_continuous(limits = c(1990.4,2011.6), breaks = seq(1991,2011,by = 2),expand = c(0,0))  +
    scale_y_continuous(limits = c(-300,300), breaks = seq(-300,300,by = 100), expand = c(0,0)) +
    xlab('') +
    ylab(bquote('Volume ('*Mm^3*')')) +
    ggtitle('SWBM Annual Water Budget') +
    scale_fill_manual(values = SWBM_colors)+
    theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(color = 'black', fill = NA),
          plot.background = element_rect(color = NA, fill = NA),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), 
          axis.text = element_text(size = 8),
          plot.title = element_text(hjust = 0.5),
          legend.position = c(0.25, 0.95), 
          legend.key = element_rect(fill = NA, color = NA),
          legend.background = element_rect(fill = NA, color = NA),
          legend.direction = 'horizontal',
          legend.text = element_text(size = 6, margin = margin(r=1,l=1, unit = 'pt')),
          legend.key.height = unit(10,'pt')
    )
)
#graphics.off()

#pdf(paste0(out_dir,'/Water_Budget_Annual_SWBM_TAF.pdf'), width = 8.5, height = 4)
(SWBM_Annual_Budget_Plot_TAF = ggplot(SWBM_Budget_Annual_melt, aes(x = WY, y = value*0.000810714/1000)) +
    geom_bar(aes(fill = variable), position = "stack", stat = 'identity', color = 'black', width = 0.95, size = 0.1) +
    scale_x_continuous(limits = c(1990.4,2011.6), breaks = seq(1991,2011,by = 2),expand = c(0,0))  +
    scale_y_continuous(limits = c(-300,300), breaks = seq(-300,300,by = 100), expand = c(0,0)) +
    xlab('') +
    ylab('Volume (TAF)') +
    ggtitle('SWBM Annual Water Budget') +
    scale_fill_manual(values = SWBM_colors)+
    theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(color = 'black', fill = NA),
          plot.background = element_rect(color = NA, fill = NA),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), 
          axis.text = element_text(size = 8),
          plot.title = element_text(hjust = 0.5),
          legend.position = c(0.25, 0.95), 
          legend.key = element_rect(fill = NA, color = NA),
          legend.background = element_rect(fill = NA, color = NA),
          legend.direction = 'horizontal',
          legend.text = element_text(size = 6, margin = margin(r=1,l=1, unit = 'pt')),
          legend.key.height = unit(10,'pt')
    )
)
#graphics.off()

##############################################################################################
##############             MODFLOW ANNUAL WATER BUDGET PLOT         ##########################
##############################################################################################
Water_Budget_MODFLOW_Annual_Plotting = data.frame(Water_Year = rep(seq(1991,2011),each = 12),
                                 Recharge = MODFLOW_Budget_Monthly$RECHARGE_net_m3,
                                 ET = MODFLOW_Budget_Monthly$ET_SEGMENTS_net_m3,
                                 Storage = MODFLOW_Budget_Monthly$STORAGE_net_m3,
                                 Drains = MODFLOW_Budget_Monthly$DRAINS_net_m3,
                                 `Stream Leakage` = MODFLOW_Budget_Monthly$STREAM_LEAKAGE_net_m3,
                                 Wells = -MODFLOW_Budget_Monthly$WELLS_out_m3,              #negative sign since flux is out
                                 `Canal Seepage/MFR` = MODFLOW_Budget_Monthly$WELLS_in_m3)
names(Water_Budget_MODFLOW_Annual_Plotting) = c('Water Year', MODFLOW_flux_labels)
Water_Budget_MODFLOW_Annual_Plotting = aggregate(.~`Water Year`,Water_Budget_MODFLOW_Annual_Plotting, FUN = sum)
Water_Budget_MODFLOW_Annual_Plotting_melt = melt(Water_Budget_MODFLOW_Annual_Plotting, id.vars = 'Water Year')
Water_Budget_MODFLOW_Annual_Plotting_melt$variable = factor(Water_Budget_MODFLOW_Annual_Plotting_melt$variable, levels = c('Recharge','ET','Storage','Drains','Stream Leakage','Wells', 'Canal Seepage/MFR'))
Water_Budget_MODFLOW_Annual_Plotting_melt = Water_Budget_MODFLOW_Annual_Plotting_melt[order(Water_Budget_MODFLOW_Annual_Plotting_melt$variable),]

#pdf(paste0(out_dir,'/Water_Budget_Annual_MODFLOW_Mm3.pdf'), width = 8.5, height = 4)
(MODFLOW_Annual_Budget_Plot_Mm3 = ggplot(Water_Budget_MODFLOW_Annual_Plotting_melt, aes(x = `Water Year`, y = value/1E6)) +
    geom_bar(aes(fill = variable), position = "stack", stat = 'identity', color = 'black', width = 0.95, size = 0.1) +
    scale_x_continuous(limits = c(1990.4,2011.6), breaks = seq(1991,2011,by = 2),expand = c(0,0))  +
    scale_y_continuous(limits = c(-150,150), breaks = seq(-150,150,by = 50), expand = c(0,0)) +
    xlab('') +
    ylab(bquote('Volume ('*Mm^3*')')) +
    ggtitle('Annual MODFLOW Water Budget') +
    scale_fill_manual(values = MODFLOW_colors)+
    theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(color = 'black', fill = NA),
          plot.background = element_rect(color = NA, fill = NA),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), 
          axis.text = element_text(size = 8),
          plot.title = element_text(hjust = 0.5),
          legend.position = c(0.25, 0.95), 
          legend.key = element_rect(fill = NA, color = NA),
          legend.background = element_rect(fill = NA, color = NA),
          legend.direction = 'horizontal',
          legend.text = element_text(size = 6, margin = margin(r=1,l=1, unit = 'pt')),
          legend.key.height = unit(10,'pt')
    )
)
#graphics.off()

#pdf(paste0(out_dir,'/Water_Budget_Annual_MODFLOW_TAF.pdf'), width = 8.5, height = 4)
(MODFLOW_Annual_Budget_Plot_TAF = ggplot(Water_Budget_MODFLOW_Annual_Plotting_melt, aes(x = `Water Year`, y = value*0.000810714/1000)) +
    geom_bar(aes(fill = variable), position = "stack", stat = 'identity', color = 'black', width = 0.95, size = 0.1) +
    scale_x_continuous(limits = c(1990.4,2011.6), breaks = seq(1991,2011,by = 2),expand = c(0,0))  +
    scale_y_continuous(limits = c(-150,150), breaks = seq(-150,150,by = 50), expand = c(0,0)) +
    xlab('') +
    ylab('Volume (TAF)') +
    ggtitle('Annual MODFLOW Water Budget') +
    scale_fill_manual(values = MODFLOW_colors)+
    theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(color = 'black', fill = NA),
          plot.background = element_rect(color = NA, fill = NA),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), 
          axis.text = element_text(size = 8),
          plot.title = element_text(hjust = 0.5),
          legend.position = c(0.25, 0.95), 
          legend.key = element_rect(fill = NA, color = NA),
          legend.background = element_rect(fill = NA, color = NA),
          legend.direction = 'horizontal',
          legend.text = element_text(size = 6, margin = margin(r=1,l=1, unit = 'pt')),
          legend.key.height = unit(10,'pt')
    )
)
#graphics.off()

###############################################################################################
#############            COMBINED ANNUAL WATER BUDGET PLOTS           #########################
###############################################################################################
pdf(paste(out_dir,'/Water_Budget_Annual_Combined.pdf',sep=''),width = 4, height = 5)
grid.newpage()
pushViewport(viewport(layout = grid.layout(2,1)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(SWBM_Annual_Budget_Plot_Mm3 + 
        ylab(bquote('Volume ('*Mm^3*')')) +
        theme(axis.text.x = element_blank(),
              axis.title.y = element_text(size = 8),
              plot.title = element_blank(),
              plot.margin = margin(t=5, b=-10,l=3, r=3),
              legend.key.height = unit(3,'pt'),
              legend.key.width = unit(6, 'pt'),
              legend.text = element_text(size = 6, margin = margin(r=1,l=1, unit = 'pt')),
              legend.position = c(0.3,0.94)) +
        annotate('text', x = 1996, y = -275, label = 'Annual SWBM Water Budget', size = 2.5),
      vp = vplayout(1,1))
print(MODFLOW_Annual_Budget_Plot_Mm3 +
        geom_rect(aes(xmin = 1990.5, xmax = 1992.5, ymin = -149 , ymax = -135), fill = '#ff8282', color = 'black', size = 0.2) +
        geom_rect(aes(xmin = 1993.5, xmax = 1994.5, ymin = -149 , ymax = -135), fill = '#ff8282', color = 'black', size = 0.2) +
        geom_rect(aes(xmin = 1994.5, xmax = 1999.5, ymin = -149 , ymax = -135), fill = 'skyblue', color = 'black', size = 0.2) +
        geom_rect(aes(xmin = 2000.5, xmax = 2002.5, ymin = -149 , ymax = -135), fill = '#ff8282', color = 'black', size = 0.2) +
        geom_rect(aes(xmin = 2005.5, xmax = 2006.5, ymin = -149 , ymax = -135), fill = 'skyblue', color = 'black', size = 0.2) +
        geom_rect(aes(xmin = 2006.5, xmax = 2009.5, ymin = -149 , ymax = -135), fill = '#ff8282', color = 'black', size = 0.2) +
        geom_rect(aes(xmin = 2010.5, xmax = 2011.5, ymin = -149 , ymax = -135), fill = 'skyblue', color = 'black', size = 0.2) +
        theme(plot.title = element_blank(),
              axis.title.y = element_text(size = 8),
              plot.margin = margin(t=5, b=-10,l=3, r=3),
              legend.key.height = unit(3,'pt'),
              legend.key.width = unit(6, 'pt'),
              legend.text = element_text(size = 6, margin = margin(r=1,l=1, unit = 'pt')),
              legend.position = c(0.4,0.94)) +
        annotate('text', x = 1996, y = -125, label = 'Annual MODFLOW Water Budget', size = 2.5) +
        annotate('text', x = 1991.5, y = -142, label = 'Dry', size = 1.4) +
        annotate('text', x = 1994, y = -142, label = 'Dry', size = 1.4) +
        annotate('text', x = 1997, y = -142, label = 'Wet', size = 1.4) +
        annotate('text', x = 2001.5, y = -142, label = 'Dry', size = 1.4) +
        annotate('text', x = 2006, y = -142, label = 'Wet', size = 1.4) +
        annotate('text', x = 2008, y = -142, label = 'Dry', size = 1.4) +
        annotate('text', x = 2011, y = -142, label = 'Wet', size = 1.4),
      vp = vplayout(2,1))
graphics.off()

###############################################################################################
#############         PLOT SWBM DRY, AVERAGE, WET YEAR BUDGETS        #########################
###############################################################################################
for (i in 1:length(Dry_Avg_Wet_Yrs)){
  eval(parse(text = paste0("SWBM_Water_Budget_",Dry_Avg_Wet_Yrs[i]," = subset(SWBM_Budget_Monthly, WY == ",Dry_Avg_Wet_Yrs[i],", 
                           select = c('Month','Precipitation','ET','GW Irrigation','Recharge','SW Irrigation','Storage'))")))
  eval(parse(text = paste0("SWBM_Water_Budget_",Dry_Avg_Wet_Yrs[i],"$Month = strtrim(SWBM_Water_Budget_",Dry_Avg_Wet_Yrs[i],"$Month,3)")))
  eval(parse(text = paste0("SWBM_Water_Budget_",Dry_Avg_Wet_Yrs[i],"$Month = factor(SWBM_Water_Budget_",Dry_Avg_Wet_Yrs[i],"$Month, levels = month_abbv)")))
  eval(parse(text = paste0("SWBM_Water_Budget_",Dry_Avg_Wet_Yrs[i]," = SWBM_Water_Budget_",Dry_Avg_Wet_Yrs[i],"[order(SWBM_Water_Budget_",Dry_Avg_Wet_Yrs[i],"$Month),]")))
  eval(parse(text = paste0("SWBM_Water_Budget_",Dry_Avg_Wet_Yrs[i],"_melt = melt(SWBM_Water_Budget_",Dry_Avg_Wet_Yrs[i],", id.vars = 'Month')")))
}

#pdf(paste0(out_dir,'/SWBM_Budget_Dry_Year_2001.pdf'), width = 8.5, height = 3)
(SWBM_Budget_Dry_Year_2001_Plot = ggplot(SWBM_Water_Budget_2001_melt, aes(x = Month, y = value/1E6)) +
    geom_bar(aes(fill = variable), position = "stack", stat = 'identity', color = 'black', width = 0.95, size = 0.1) +
    scale_x_discrete(expand = c(0.05,0.05))  +
    scale_y_continuous(limits = c(-30,30), breaks = seq(-30,30,by = 10), expand = c(0,0)) +
    xlab('') +
    ylab(bquote('Volume ('*Mm^3*')')) +
    ggtitle('Dry Year (2001)') +
    scale_fill_manual(values = SWBM_colors)+
    theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(color = 'black', fill = NA),
          plot.background = element_rect(color = NA, fill = NA),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), 
          axis.text = element_text(size = 8),
          plot.title = element_text(hjust = 0.5),
          legend.position = c(0.2, 0.93), 
          legend.key = element_rect(fill = NA, color = NA),
          legend.background = element_rect(fill = NA, color = NA),
          legend.direction = 'horizontal',
          legend.text = element_text(size = 6, margin = margin(r=1,l=1, unit = 'pt')),
          legend.key.height = unit(10,'pt')
    )
)
#graphics.off()

#pdf(paste0(out_dir,'/SWBM_Budget_Average_year_2010.pdf'), width = 8.5, height = 4)
(SWBM_Budget_Average_Year_2010_Plot = ggplot(SWBM_Water_Budget_2010_melt, aes(x = Month, y = value/1E6)) +
    geom_bar(aes(fill = variable), position = "stack", stat = 'identity', color = 'black', width = 0.95, size = 0.1) +
    scale_x_discrete(expand = c(0.05,0.05))  +
    scale_y_continuous(limits = c(-30,30), breaks = seq(-30,30,by = 10), expand = c(0,0)) +
    xlab('') +
    ylab(bquote('Volume ('*Mm^3*')')) +
    ggtitle('Average Year (2010)') +
    scale_fill_manual(values = SWBM_colors)+
    theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(color = 'black', fill = NA),
          plot.background = element_rect(color = NA, fill = NA),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), 
          axis.text = element_text(size = 8),
          plot.title = element_text(hjust = 0.5),
          legend.position = c(0.2, 0.93), 
          legend.key = element_rect(fill = NA, color = NA),
          legend.background = element_rect(fill = NA, color = NA),
          legend.direction = 'horizontal',
          legend.text = element_text(size = 6, margin = margin(r=1,l=1, unit = 'pt')),
          legend.key.height = unit(10,'pt')
    )
)
#graphics.off()

#pdf(paste0(out_dir,'/SWBM_Budget_Wet_Year_2006.pdf'), width = 8.5, height = 4)
(SWBM_Budget_Wet_Year_2006_Plot = ggplot(SWBM_Water_Budget_2006_melt, aes(x = Month, y = value/1E6)) +
    geom_bar(aes(fill = variable), position = "stack", stat = 'identity', color = 'black', width = 0.95, size = 0.1) +
    scale_x_discrete(expand = c(0.05,0.05))  +
    scale_y_continuous(limits = c(-80,80), breaks = seq(-80,80,by = 25), expand = c(0,0)) +
    xlab('') +
    ylab(bquote('Volume ('*Mm^3*')')) +
    ggtitle('Wet Year (2006)') +
    scale_fill_manual(values = SWBM_colors)+
    theme(legend.title=element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(color = 'black', fill = NA),
          plot.background = element_rect(color = NA, fill = NA),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), 
          axis.text = element_text(size = 8),
          plot.title = element_text(hjust = 0.5),
          legend.position = c(0.2, 0.93), 
          legend.key = element_rect(fill = NA, color = NA),
          legend.background = element_rect(fill = NA, color = NA),
          legend.direction = 'horizontal',
          legend.text = element_text(size = 6, margin = margin(r=1,l=1, unit = 'pt')),
          legend.key.height = unit(10,'pt')
    )
)
#graphics.off()

###############################################################################################
#############       PLOT MODFLOW DRY, AVERAGE, WET YEAR BUDGETS       #########################
###############################################################################################
for (i in 1:length(Dry_Avg_Wet_Yrs)){
eval(parse(text = paste0("MODFLOW_Water_Budget_",Dry_Avg_Wet_Yrs[i]," = subset(MODFLOW_Budget_Monthly, Water_Year == ",Dry_Avg_Wet_Yrs[i],", 
                                                            select = c('Month', 'RECHARGE_net_m3','ET_SEGMENTS_net_m3',
                                                                       'STORAGE_net_m3','DRAINS_net_m3','STREAM_LEAKAGE_net_m3',
                                                                       'WELLS_out_m3', 'WELLS_in_m3'))")))
eval(parse(text = paste0("names(MODFLOW_Water_Budget_",Dry_Avg_Wet_Yrs[i],") = c('Month', 'Recharge','ET','Storage','Drains','Stream Leakage','Wells', 'Canal Seepage/MFR')")))
eval(parse(text = paste0("MODFLOW_Water_Budget_",Dry_Avg_Wet_Yrs[i],"$Month = strtrim(MODFLOW_Water_Budget_",Dry_Avg_Wet_Yrs[i],"$Month,3)")))
eval(parse(text = paste0("MODFLOW_Water_Budget_",Dry_Avg_Wet_Yrs[i],"$Month = factor(MODFLOW_Water_Budget_",Dry_Avg_Wet_Yrs[i],"$Month, levels = month_abbv)")))
eval(parse(text = paste0("MODFLOW_Water_Budget_",Dry_Avg_Wet_Yrs[i]," = MODFLOW_Water_Budget_",Dry_Avg_Wet_Yrs[i],"[order(MODFLOW_Water_Budget_",Dry_Avg_Wet_Yrs[i],"$Month),]")))
eval(parse(text = paste0("MODFLOW_Water_Budget_",Dry_Avg_Wet_Yrs[i],"$Wells = -MODFLOW_Water_Budget_",Dry_Avg_Wet_Yrs[i],"$Wells")))
eval(parse(text = paste0("MODFLOW_Water_Budget_",Dry_Avg_Wet_Yrs[i],"_melt = melt(MODFLOW_Water_Budget_",Dry_Avg_Wet_Yrs[i],", id.vars = 'Month')")))
}

#pdf(paste0(out_dir,'/MODFLOW_Budget_Dry_Year_2001.pdf'), width = 8.5, height = 3)
(MODFLOW_Budget_Dry_Year_2001_Plot = ggplot(MODFLOW_Water_Budget_2001_melt, aes(x = Month, y = value/1E6)) +
    geom_bar(aes(fill = variable), position = "stack", stat = 'identity', color = 'black', width = 0.95, size = 0.1) +
    scale_x_discrete(expand = c(0.05,0.05))  +
    scale_y_continuous(limits = c(-20,20), breaks = seq(-20,20,by = 10), expand = c(0,0)) +
    xlab('') +
    ylab(bquote('Volume ('*Mm^3*')')) +
    ggtitle('Dry Year (2001)') +
    scale_fill_manual(values = c('blue', 'red', 'goldenrod', 'mediumorchid2','dodgerblue1', 'green4', 'salmon'  ))+
    theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(color = 'black', fill = NA),
          plot.background = element_rect(color = NA, fill = NA),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), 
          axis.text = element_text(size = 8),
          plot.title = element_text(hjust = 0.5),
          legend.position = c(0.2, 0.93), 
          legend.key = element_rect(fill = NA, color = NA),
          legend.background = element_rect(fill = NA, color = NA),
          legend.direction = 'horizontal',
          legend.text = element_text(size = 6, margin = margin(r=1,l=1, unit = 'pt')),
          legend.key.height = unit(10,'pt')
    )
)
#graphics.off()

#pdf(paste0(out_dir,'/MODFLOW_Budget_Average_year_2010.pdf'), width = 8.5, height = 4)
(MODFLOW_Budget_Average_Year_2010_Plot = ggplot(MODFLOW_Water_Budget_2010_melt, aes(x = Month, y = value/1E6)) +
    geom_bar(aes(fill = variable), position = "stack", stat = 'identity', color = 'black', width = 0.95, size = 0.1) +
    scale_x_discrete(expand = c(0.05,0.05))  +
    scale_y_continuous(limits = c(-20,20), breaks = seq(-20,20,by = 10), expand = c(0,0)) +
    xlab('') +
    ylab(bquote('Volume ('*Mm^3*')')) +
    ggtitle('Average Year (2010)') +
    scale_fill_manual(values = c('blue', 'red', 'goldenrod', 'mediumorchid2','dodgerblue1', 'green4', 'salmon' ))+
    theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(color = 'black', fill = NA),
          plot.background = element_rect(color = NA, fill = NA),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), 
          axis.text = element_text(size = 8),
          plot.title = element_text(hjust = 0.5),
          legend.position = c(0.2, 0.93), 
          legend.key = element_rect(fill = NA, color = NA),
          legend.background = element_rect(fill = NA, color = NA),
          legend.direction = 'horizontal',
          legend.text = element_text(size = 6, margin = margin(r=1,l=1, unit = 'pt')),
          legend.key.height = unit(10,'pt')
    )
)
#graphics.off()

#pdf(paste0(out_dir,'/MODFLOW_Budget_Wet_Year_2006.pdf'), width = 8.5, height = 4)
(MODFLOW_Budget_Wet_Year_2006_Plot = ggplot(MODFLOW_Water_Budget_2006_melt, aes(x = Month, y = value/1E6)) +
    geom_bar(aes(fill = variable), position = "stack", stat = 'identity', color = 'black', width = 0.95, size = 0.1) +
    scale_x_discrete(expand = c(0.05,0.05))  +
    scale_y_continuous(limits = c(-50,50), breaks = seq(-50,50,by = 25), expand = c(0,0)) +
    xlab('') +
    ylab(bquote('Volume ('*Mm^3*')')) +
    ggtitle('Wet Year (2006)') +
    scale_fill_manual(values = c('blue', 'red', 'goldenrod', 'mediumorchid2','dodgerblue1', 'green4', 'salmon' ))+
    theme(legend.title=element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(color = 'black', fill = NA),
          plot.background = element_rect(color = NA, fill = NA),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), 
          axis.text = element_text(size = 8),
          plot.title = element_text(hjust = 0.5),
          legend.position = c(0.2, 0.93), 
          legend.key = element_rect(fill = NA, color = NA),
          legend.background = element_rect(fill = NA, color = NA),
          legend.direction = 'horizontal',
          legend.text = element_text(size = 6, margin = margin(r=1,l=1, unit = 'pt')),
          legend.key.height = unit(10,'pt')
    )
)
#graphics.off()

###############################################################################################
#########           COMBINED DRY, AVERAGE, WET YEAR BUDGETS PLOTS          ####################
###############################################################################################
pdf(paste(out_dir,'/Water_Budget_Monthly_Wet_Avg_Dry_Combined.pdf',sep=''),width = 5.5, height = 6.5)
grid.newpage()
pushViewport(viewport(layout = grid.layout(3,2)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(SWBM_Budget_Dry_Year_2001_Plot + 
        theme(axis.text.x = element_blank(),
              axis.title.y = element_text(size = 6),
              axis.text.y = element_text(size = 6),
              axis.ticks = element_line(size = 0.25),
              plot.title = element_blank(),
              plot.margin = margin(t=5, b=0,l=1, r=-3),
              legend.key.height = unit(2.5,'pt'),
              legend.key.width = unit(5, 'pt'),
              legend.text = element_text(size = 5, margin = margin(r=1,l=1, unit = 'pt')),
              legend.position = c(0.33,0.94)) +
        annotate('text', x = 2, y = -22, label = 'SWBM\nDry Year\n(2001)', size = 2.4),
      vp = vplayout(1,1))
print(SWBM_Budget_Average_Year_2010_Plot +
        theme(axis.text.x = element_blank(),
              axis.title.y = element_text(size = 6),
              axis.text.y = element_text(size = 6),
              axis.ticks = element_line(size = 0.25),
              plot.title = element_blank(),
              plot.margin = margin(t=0, b=0,l=1, r=-3),
              legend.position = 'none') + 
        annotate('text', x = 2, y = -22, label = 'SWBM\nAvg Year\n(2010)', size = 2.4),
      vp = vplayout(2,1))
print(SWBM_Budget_Wet_Year_2006_Plot +
        scale_y_continuous(limits = c(-80,80), breaks = seq(-80,80,by = 20), expand = c(0,0)) +
        #coord_cartesian(ylim=c(-30,30)) + 
        theme(plot.title = element_blank(),
              axis.ticks = element_line(size = 0.25),
              axis.title.y = element_text(size = 6),
              axis.text.y = element_text(size = 6),
              plot.margin = margin(t=0, b=-12,l=1, r=-3),
              legend.position = 'none') +
        annotate('text', x = 7, y = -60, label = 'SWBM\nWet Year\n(2006)', size = 2.4),
      vp = vplayout(3,1))
print(MODFLOW_Budget_Dry_Year_2001_Plot + 
        scale_y_continuous(limits = c(-20,20), breaks = seq(-20,20,by = 10), expand = c(0,0)) +
        theme(axis.title.y = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_text(size = 6),
              axis.ticks = element_line(size = 0.25),
              plot.title = element_blank(),
              plot.margin = margin(t=5, b=0,l=7, r=1),
              legend.key.height = unit(2.5,'pt'),
              legend.key.width = unit(5, 'pt'),
              legend.text = element_text(size = 5, margin = margin(r=1,l=1, unit = 'pt')),
              legend.position = c(0.45,0.94)) +
        annotate('text', x = 2, y = -15, label = 'MODFLOW\nDry Year\n(2001)', size = 2.4),
      vp = vplayout(1,2))
print(MODFLOW_Budget_Average_Year_2010_Plot +
        scale_y_continuous(limits = c(-20,20), breaks = seq(-20,20,by = 10), expand = c(0,0)) +
        theme(axis.title.y = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_text(size = 6),
              axis.ticks = element_line(size = 0.25),
              plot.title = element_blank(),
              plot.margin = margin(t=0, b=0,l=7, r=1),
              legend.position = 'none') + 
        annotate('text', x = 2, y = -15, label = 'MODFLOW\nAvg Year\n(2010)', size = 2.4),
      vp = vplayout(2,2))
print(MODFLOW_Budget_Wet_Year_2006_Plot +
        scale_y_continuous(limits = c(-50,50), breaks = seq(-50,50,by = 25), expand = c(0,0)) +
        #coord_cartesian(ylim=c(-30,30)) +
        #scale_y_continuous(limits = c(-30,30), breaks = seq(-30,30,by = 10), expand = c(0,0)) +
        theme(axis.title.y = element_blank(),
              axis.text.y = element_text(size = 6),
              axis.ticks = element_line(size = 0.25),
              plot.title = element_blank(),
              plot.margin = margin(t=0, b=-12,l=7, r=1),
              legend.position = 'none') + 
        annotate('text', x = 7, y = -38, label = 'MODFLOW\nWet Year\n(2006)', size = 2.4),
      vp = vplayout(3,2))
graphics.off()

##############################################################################################
##############              MONTHLY SWBM STORAGE CHANGE             ##########################
##############################################################################################
#pdf(paste(out_dir,'/Storage_Monthly_SWBM_Mm3.pdf',sep=''),width = 5.5, height = 6.5)
(Monthly_Storage_Plot_SWBM_Mm3 = ggplot(data = SWBM_Budget_Monthly,
                               aes(x = as.Date(paste0(Month,'-01'),'%b-%Y-%d'),
                                   y = (-cumsum(SWBM_Budget_Monthly$Storage)/1E6)-mean((-cumsum(SWBM_Budget_Monthly$Storage)/1E6)))) +
    geom_hline(yintercept = 0, size = 0.25) +
    geom_line(size = 0.5) +
    geom_point(size = 0.75) +
    # geom_rect(aes(xmin = as.Date('1990-10-01'), xmax = as.Date('1992-10-01'), ymin = -60 , ymax = -52), fill = '#ff8282', color = 'black', size = 0.2) +
    # geom_rect(aes(xmin = as.Date('1993-10-01'), xmax = as.Date('1994-10-01'), ymin = -60 , ymax = -52), fill = '#ff8282', color = 'black', size = 0.2) +
    # geom_rect(aes(xmin = as.Date('1994-10-01'), xmax = as.Date('1999-10-01'), ymin = -60 , ymax = -52), fill = 'skyblue', color = 'black', size = 0.2) +
    # geom_rect(aes(xmin = as.Date('2000-10-01'), xmax = as.Date('2002-10-01'), ymin = -60 , ymax = -52), fill = '#ff8282', color = 'black', size = 0.2) +
    # geom_rect(aes(xmin = as.Date('2005-10-01'), xmax = as.Date('2006-10-01'), ymin = -60 , ymax = -52), fill = 'skyblue', color = 'black', size = 0.2) +
    # geom_rect(aes(xmin = as.Date('2006-10-01'), xmax = as.Date('2009-10-01'), ymin = -60 , ymax = -52), fill = '#ff8282', color = 'black', size = 0.2) +
    # geom_rect(aes(xmin = as.Date('2010-10-01'), xmax = as.Date('2011-10-01'), ymin = -60 , ymax = -52), fill = 'skyblue', color = 'black', size = 0.2) +
    # annotate('text', x = as.Date('1991-10-01'), y = -55, label = 'Dry', size = 2.5) +
    # annotate('text', x = as.Date('1994-06-01'), y = -55, label = 'Dry', size = 2.5) +
    # annotate('text', x = as.Date('1997-10-01'), y = -55, label = 'Wet', size = 2.5) +
    # annotate('text', x = as.Date('2001-10-01'), y = -55, label = 'Dry', size = 2.5) +
    # annotate('text', x = as.Date('2006-06-01'), y = -55, label = 'Wet', size = 2.5) +
    # annotate('text', x = as.Date('2008-10-01'), y = -55, label = 'Dry', size = 2.5) +
    # annotate('text', x = as.Date('2011-06-01'), y = -55, label = 'Wet', size = 2.5) +
    scale_x_date(limits = c(as.Date('Oct-01-1990', format = '%b-%m-%y'),
                            as.Date('Oct-01-2011', format = '%b-%m-%y')),
                 breaks = seq(as.Date("1990/10/1"), by = "2 years", length.out = 22), expand = c(0,0),
                 date_labels = ('%b-%y'))  +
    scale_y_continuous(limits = c(-30,30), breaks = seq(-30,30,by = 10), expand = c(0,0)) +
    ylab(ylab(bquote('Relative Soil Storage ('*Mm^3*')'))) +
    ggtitle('Monthly Relative Soil Storage') +
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
#graphics.off()

##############################################################################################
################      MONTHLY SWBM STORAGE CHANGE (PERCENT OF TOTAL)     #####################
##############################################################################################
#pdf(paste(out_dir,'/Storage_Monthly_SWBM_Mm3.pdf',sep=''),width = 5.5, height = 6.5)
(Monthly_Storage_Plot_SWBM_Mm3 = ggplot(data = SWBM_Budget_Monthly,
                                        aes(x = as.Date(paste0(Month,'-01'),'%b-%Y-%d'),
                                            y = ((-cumsum(SWBM_Budget_Monthly$Storage)/1E6)-mean((-cumsum(SWBM_Budget_Monthly$Storage)/1E6)))/(160235466/1E6))) +
   geom_hline(yintercept = 0, size = 0.25) +
   geom_line(size = 0.5) +
   geom_point(size = 0.75) +
   # geom_rect(aes(xmin = as.Date('1990-10-01'), xmax = as.Date('1992-10-01'), ymin = -60 , ymax = -52), fill = '#ff8282', color = 'black', size = 0.2) +
   # geom_rect(aes(xmin = as.Date('1993-10-01'), xmax = as.Date('1994-10-01'), ymin = -60 , ymax = -52), fill = '#ff8282', color = 'black', size = 0.2) +
   # geom_rect(aes(xmin = as.Date('1994-10-01'), xmax = as.Date('1999-10-01'), ymin = -60 , ymax = -52), fill = 'skyblue', color = 'black', size = 0.2) +
   # geom_rect(aes(xmin = as.Date('2000-10-01'), xmax = as.Date('2002-10-01'), ymin = -60 , ymax = -52), fill = '#ff8282', color = 'black', size = 0.2) +
   # geom_rect(aes(xmin = as.Date('2005-10-01'), xmax = as.Date('2006-10-01'), ymin = -60 , ymax = -52), fill = 'skyblue', color = 'black', size = 0.2) +
   # geom_rect(aes(xmin = as.Date('2006-10-01'), xmax = as.Date('2009-10-01'), ymin = -60 , ymax = -52), fill = '#ff8282', color = 'black', size = 0.2) +
   # geom_rect(aes(xmin = as.Date('2010-10-01'), xmax = as.Date('2011-10-01'), ymin = -60 , ymax = -52), fill = 'skyblue', color = 'black', size = 0.2) +
   # annotate('text', x = as.Date('1991-10-01'), y = -55, label = 'Dry', size = 2.5) +
   # annotate('text', x = as.Date('1994-06-01'), y = -55, label = 'Dry', size = 2.5) +
   # annotate('text', x = as.Date('1997-10-01'), y = -55, label = 'Wet', size = 2.5) +
   # annotate('text', x = as.Date('2001-10-01'), y = -55, label = 'Dry', size = 2.5) +
 # annotate('text', x = as.Date('2006-06-01'), y = -55, label = 'Wet', size = 2.5) +
 # annotate('text', x = as.Date('2008-10-01'), y = -55, label = 'Dry', size = 2.5) +
 # annotate('text', x = as.Date('2011-06-01'), y = -55, label = 'Wet', size = 2.5) +
 scale_x_date(limits = c(as.Date('Oct-01-1990', format = '%b-%m-%y'),
                         as.Date('Oct-01-2011', format = '%b-%m-%y')),
              breaks = seq(as.Date("1990/10/1"), by = "2 years", length.out = 22), expand = c(0,0),
              date_labels = ('%b-%y'))  +
   scale_y_continuous(limits = c(-30,30), breaks = seq(-30,30,by = 10), expand = c(0,0)) +
   ylab(ylab(bquote('Relative Soil Storage ('*Mm^3*')'))) +
   ggtitle('Monthly Relative Soil Storage') +
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
#graphics.off()
##############################################################################################
##############             MONTHLY MODFLOW STORAGE CHANGE           ##########################
##############################################################################################
#pdf(paste(out_dir,'/Storage_Monthly_MODFLOW_Mm3.pdf',sep=''),width = 5.5, height = 6.5)
(Monthly_Storage_Plot_MODFLOW_Mm3 = ggplot(data = MODFLOW_Budget_Monthly,
                               aes(x = as.Date(paste0(Month,'-01'),'%b-%Y-%d'), y = -cumsum(STORAGE_net_m3)/1E6)) +
   geom_hline(yintercept = 0, size = 0.25) +
   geom_line(size = 0.5) +
   geom_point(size = 0.75) +
   # geom_rect(aes(xmin = as.Date('1990-10-01'), xmax = as.Date('1992-10-01'), ymin = -60 , ymax = -52), fill = '#ff8282', color = 'black', size = 0.2) +
   # geom_rect(aes(xmin = as.Date('1993-10-01'), xmax = as.Date('1994-10-01'), ymin = -60 , ymax = -52), fill = '#ff8282', color = 'black', size = 0.2) +
   # geom_rect(aes(xmin = as.Date('1994-10-01'), xmax = as.Date('1999-10-01'), ymin = -60 , ymax = -52), fill = 'skyblue', color = 'black', size = 0.2) +
   # geom_rect(aes(xmin = as.Date('2000-10-01'), xmax = as.Date('2002-10-01'), ymin = -60 , ymax = -52), fill = '#ff8282', color = 'black', size = 0.2) +
   # geom_rect(aes(xmin = as.Date('2005-10-01'), xmax = as.Date('2006-10-01'), ymin = -60 , ymax = -52), fill = 'skyblue', color = 'black', size = 0.2) +
   # geom_rect(aes(xmin = as.Date('2006-10-01'), xmax = as.Date('2009-10-01'), ymin = -60 , ymax = -52), fill = '#ff8282', color = 'black', size = 0.2) +
   # geom_rect(aes(xmin = as.Date('2010-10-01'), xmax = as.Date('2011-10-01'), ymin = -60 , ymax = -52), fill = 'skyblue', color = 'black', size = 0.2) +
   # annotate('text', x = as.Date('1991-10-01'), y = -55, label = 'Dry', size = 2.5) +
   # annotate('text', x = as.Date('1994-06-01'), y = -55, label = 'Dry', size = 2.5) +
   # annotate('text', x = as.Date('1997-10-01'), y = -55, label = 'Wet', size = 2.5) +
   # annotate('text', x = as.Date('2001-10-01'), y = -55, label = 'Dry', size = 2.5) +
   # annotate('text', x = as.Date('2006-06-01'), y = -55, label = 'Wet', size = 2.5) +
   # annotate('text', x = as.Date('2008-10-01'), y = -55, label = 'Dry', size = 2.5) +
   # annotate('text', x = as.Date('2011-06-01'), y = -55, label = 'Wet', size = 2.5) +
   scale_x_date(limits = c(as.Date('Oct-01-1990', format = '%b-%m-%y'),
                           as.Date('Oct-01-2011', format = '%b-%m-%y')),
                breaks = seq(as.Date("1990/10/1"), by = "2 years", length.out = 22), expand = c(0,0),
                date_labels = ('%b-%y'))  +
   scale_y_continuous(limits = c(-50,50), breaks = seq(-50,50,by = 25), expand = c(0,0)) +
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
#graphics.off()

##############################################################################################
##############            COMBINED MONTHLY STORAGE PLOT             ##########################
##############################################################################################
pdf(paste(out_dir,'/Storage_Monthly_Mm3.pdf',sep=''),width = 5.5, height = 6.5)
grid.newpage()
pushViewport(viewport(layout = grid.layout(2,1)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(Monthly_Storage_Plot_SWBM_Mm3 +
        theme(axis.text.x = element_blank(),
              plot.margin = margin(t=5, b=25,l=5, r=5)
        ),
      vp = vplayout(1,1))
print(Monthly_Storage_Plot_MODFLOW_Mm3 +
      geom_rect(aes(xmin = as.Date('1990-10-01'), xmax = as.Date('1992-10-01'), ymin = -50 , ymax = -44), fill = '#ff8282', color = 'black', size = 0.2) +
      geom_rect(aes(xmin = as.Date('1993-10-01'), xmax = as.Date('1994-10-01'), ymin = -50 , ymax = -44), fill = '#ff8282', color = 'black', size = 0.2) +
      geom_rect(aes(xmin = as.Date('1994-10-01'), xmax = as.Date('1999-10-01'), ymin = -50 , ymax = -44), fill = 'skyblue', color = 'black', size = 0.2) +
      geom_rect(aes(xmin = as.Date('2000-10-01'), xmax = as.Date('2002-10-01'), ymin = -50 , ymax = -44), fill = '#ff8282', color = 'black', size = 0.2) +
      geom_rect(aes(xmin = as.Date('2005-10-01'), xmax = as.Date('2006-10-01'), ymin = -50 , ymax = -44), fill = 'skyblue', color = 'black', size = 0.2) +
      geom_rect(aes(xmin = as.Date('2006-10-01'), xmax = as.Date('2009-10-01'), ymin = -50 , ymax = -44), fill = '#ff8282', color = 'black', size = 0.2) +
      geom_rect(aes(xmin = as.Date('2010-10-01'), xmax = as.Date('2011-10-01'), ymin = -50 , ymax = -44), fill = 'skyblue', color = 'black', size = 0.2) +
      annotate('text', x = as.Date('1991-10-01'), y = -47, label = 'Dry', size = 2.5, hjust = 0.5) +
      annotate('text', x = as.Date('1994-04-01'), y = -47, label = 'Dry', size = 2.5, hjust = 0.5) +
      annotate('text', x = as.Date('1997-04-01'), y = -47, label = 'Wet', size = 2.5, hjust = 0.5) +
      annotate('text', x = as.Date('2001-10-01'), y = -47, label = 'Dry', size = 2.5, hjust = 0.5) +
      annotate('text', x = as.Date('2006-04-01'), y = -47, label = 'Wet', size = 2.5, hjust = 0.5) +
      annotate('text', x = as.Date('2008-04-01'), y = -47, label = 'Dry', size = 2.5, hjust = 0.5) +
      annotate('text', x = as.Date('2011-04-01'), y = -47, label = 'Wet', size = 2.5, hjust = 0.5) +
        theme(plot.margin = margin(t=-5, b=5,l=5, r=5)),
      vp = vplayout(2,1))
graphics.off()

##############################################################################################
##############                 SWBM ANNUAL STORAGE CHANGE           ##########################
##############################################################################################
#pdf(paste(out_dir,'/Storage_Annual_SWBM_Mm3.pdf',sep=''),width = 5.5, height = 6.5)
(Annual_Storage_Plot_SWBM_Mm3 = ggplot(data = SWBM_Budget_Annual,
                                        aes(x = WY,
                                            y = -(Storage/1E6 - mean(Storage)/1E6 ))) +
 geom_hline(yintercept = 0, size = 0.25) +
 geom_line(size = 0.5) +
 geom_point(size = 0.75) +
 # geom_rect(aes(xmin = 1990.5, xmax = 1992.5, ymin = -20 , ymax = -17), fill = '#ff8282') +
 # geom_rect(aes(xmin = 1993.5, xmax = 1994.5, ymin = -20 , ymax = -17), fill = '#ff8282') +
 # geom_rect(aes(xmin = 1994.5, xmax = 1999.5, ymin = -20 , ymax = -17), fill = 'skyblue') +
 # geom_rect(aes(xmin = 2000.5, xmax = 2002.5, ymin = -20 , ymax = -17), fill = '#ff8282') +
 # geom_rect(aes(xmin = 2005.5, xmax = 2006.5, ymin = -20 , ymax = -17), fill = 'skyblue') +
 # geom_rect(aes(xmin = 2006.5, xmax = 2009.5, ymin = -20 , ymax = -17), fill = '#ff8282') +
 # geom_rect(aes(xmin = 2010.5, xmax = 2011.5, ymin = -20 , ymax = -17), fill = 'skyblue') +
 # annotate('text', x = 1991.5, y = -18.5, label = 'Dry', size = 2.5, hjust = 0.5) +
 # annotate('text', x = 1994, y = -18.5, label = 'Dry', size = 2.5, hjust = 0.5) +
 # annotate('text', x = 1997, y = -18.5, label = 'Wet', size = 2.5, hjust = 0.5) +
 # annotate('text', x = 2001.5, y = -18.5, label = 'Dry', size = 2.5, hjust = 0.5) +
 # annotate('text', x = 2006, y = -18.5, label = 'Wet', size = 2.5, hjust = 0.5) +
 # annotate('text', x = 2008, y = -18.5, label = 'Dry', size = 2.5, hjust = 0.5) +
 # annotate('text', x = 2011, y = -18.5, label = 'Wet', size = 2.5, hjust = 0.5) +  
 scale_x_continuous(limits = c(1990.5,2011.5), breaks = seq(1991,2011,by = 2), expand = c(0,0)) +
 scale_y_continuous(limits = c(-20,20), breaks = seq(-20,20,by = 10), expand = c(0,0)) +
 ylab(ylab(bquote('Relative Soil Storage ('*Mm^3*')'))) +
 ggtitle('Annual Relative Soil Storage') +
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
#graphics.off()

##############################################################################################
##############              MODFLOW ANNUAL STORAGE CHANGE           ##########################
##############################################################################################
#pdf(paste(out_dir,'/Storage_Annual_MODFLOW_Mm3.pdf',sep=''),width = 5.5, height = 6.5)
(Annual_Storage_Plot_MODFLOW_Mm3 = ggplot(data = subset(Water_Budget_MODFLOW_Annual_Plotting_melt, variable=='Storage'),
                                       aes(x = `Water Year`,
                                           y = -value/1E6)) +
   geom_hline(yintercept = 0, size = 0.25) +
   geom_line(size = 0.5) +
   geom_point(size = 0.75) +
   # geom_rect(aes(xmin = 1990.5, xmax = 1992.5, ymin = -40 , ymax = -36), fill = '#ff8282') +
   # geom_rect(aes(xmin = 1993.5, xmax = 1994.5, ymin = -40 , ymax = -36), fill = '#ff8282') +
   # geom_rect(aes(xmin = 1994.5, xmax = 1999.5, ymin = -40 , ymax = -36), fill = 'skyblue') +
   # geom_rect(aes(xmin = 2000.5, xmax = 2002.5, ymin = -40 , ymax = -36), fill = '#ff8282') +
   # geom_rect(aes(xmin = 2005.5, xmax = 2006.5, ymin = -40 , ymax = -36), fill = 'skyblue') +
   # geom_rect(aes(xmin = 2006.5, xmax = 2009.5, ymin = -40 , ymax = -36), fill = '#ff8282') +
   # geom_rect(aes(xmin = 2010.5, xmax = 2011.5, ymin = -40 , ymax = -36), fill = 'skyblue') +
   # annotate('text', x = 1991.5, y = -38, label = 'Dry', size = 2.5, hjust = 0.5) +
   # annotate('text', x = 1994, y = -38, label = 'Dry', size = 2.5, hjust = 0.5) +
   # annotate('text', x = 1997, y = -38, label = 'Wet', size = 2.5, hjust = 0.5) +
   # annotate('text', x = 2001.5, y = -38, label = 'Dry', size = 2.5, hjust = 0.5) +
   # annotate('text', x = 2006, y = -38, label = 'Wet', size = 2.5, hjust = 0.5) +
   # annotate('text', x = 2008, y = -38, label = 'Dry', size = 2.5, hjust = 0.5) +
   # annotate('text', x = 2011, y = -38, label = 'Wet', size = 2.5, hjust = 0.5) +  
   scale_x_continuous(limits = c(1990.5,2011.5), breaks = seq(1991,2011,by = 2), expand = c(0,0)) +
   scale_y_continuous(limits = c(-40,40), breaks = seq(-40,40,by = 20), expand = c(0,0)) +
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
#graphics.off()

##############################################################################################
##############             COMBINED ANNUAL STORAGE PLOT             ##########################
##############################################################################################
pdf(paste(out_dir,'/Storage_Annual_Mm3.pdf',sep=''),width = 5.5, height = 6.5)
grid.newpage()
pushViewport(viewport(layout = grid.layout(2,1)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(Annual_Storage_Plot_SWBM_Mm3 +
        theme(axis.text.x = element_blank(),
              plot.margin = margin(t=5, b=25,l=5, r=5)
        ),
      vp = vplayout(1,1))
print(Annual_Storage_Plot_MODFLOW_Mm3 +
      geom_rect(aes(xmin = 1990.5, xmax = 1992.5, ymin = -40 , ymax = -36), fill = '#ff8282') +
      geom_rect(aes(xmin = 1993.5, xmax = 1994.5, ymin = -40 , ymax = -36), fill = '#ff8282') +
      geom_rect(aes(xmin = 1994.5, xmax = 1999.5, ymin = -40 , ymax = -36), fill = 'skyblue') +
      geom_rect(aes(xmin = 2000.5, xmax = 2002.5, ymin = -40 , ymax = -36), fill = '#ff8282') +
      geom_rect(aes(xmin = 2005.5, xmax = 2006.5, ymin = -40 , ymax = -36), fill = 'skyblue') +
      geom_rect(aes(xmin = 2006.5, xmax = 2009.5, ymin = -40 , ymax = -36), fill = '#ff8282') +
      geom_rect(aes(xmin = 2010.5, xmax = 2011.5, ymin = -40 , ymax = -36), fill = 'skyblue') +
      annotate('text', x = 1991.5, y = -38, label = 'Dry', size = 2.5, hjust = 0.5) +
      annotate('text', x = 1994, y = -38, label = 'Dry', size = 2.5, hjust = 0.5) +
      annotate('text', x = 1997, y = -38, label = 'Wet', size = 2.5, hjust = 0.5) +
      annotate('text', x = 2001.5, y = -38, label = 'Dry', size = 2.5, hjust = 0.5) +
      annotate('text', x = 2006, y = -38, label = 'Wet', size = 2.5, hjust = 0.5) +
      annotate('text', x = 2008, y = -38, label = 'Dry', size = 2.5, hjust = 0.5) +
      annotate('text', x = 2011, y = -38, label = 'Wet', size = 2.5, hjust = 0.5) +
      theme(plot.margin = margin(t=-5, b=5,l=5, r=5)),
      vp = vplayout(2,1))
graphics.off()

##############################################################################################
##############                   COMPARE MAR BUDGET                 ##########################
##############################################################################################
if (COMPARE_MAR_BUDGET==TRUE){
  # SWBM Budget
  SWBM_MAR_Budget_Monthly = read.table('monthly_water_budget_MAR.dat', header = T)
  names(SWBM_MAR_Budget_Monthly) = c('Month',SWBM_Terms)  
  SWBM_MAR_Budget_Monthly$Month = format(seq(as.Date("1990/10/1"), by = "month", length.out = 252),'%b-%Y')
  SWBM_MAR_Budget_Monthly$WY = rep(seq(1991,2011),each = 12)
  SWBM_MAR_Budget_Annual = aggregate(.~WY,SWBM_MAR_Budget_Monthly[,!names(SWBM_MAR_Budget_Monthly)%in%'Month'], FUN = sum)
  SWBM_MAR_Monthly_Diff = subset(SWBM_MAR_Budget_Monthly, select = SWBM_Terms) - subset(SWBM_Budget_Monthly, select = SWBM_Terms)
  SWBM_MAR_Monthly_Diff$WY = rep(seq(1991,2011),each = 12)
  SWBM_MAR_Annual_Diff = aggregate(.~WY, SWBM_MAR_Monthly_Diff, FUN = sum)                           
  SWBM_MAR_Annual_Diff_melt = melt(SWBM_MAR_Annual_Diff, id.vars = 'WY')
  SWBM_MAR_Annual_Diff_melt$variable = factor(SWBM_MAR_Annual_Diff_melt$variable, levels = SWBM_flux_labels)
  SWBM_MAR_Annual_Diff_melt = SWBM_MAR_Annual_Diff_melt[order(SWBM_MAR_Annual_Diff_melt$variable),]
  
  #pdf(paste0(out_dir,'/Water_Budget_Annual_MAR_Diff_SWBM_TAF.pdf'), width = 8.5, height = 4)
  (SWBM_Annual_MAR_Diff_Plot_TAF = ggplot(SWBM_MAR_Annual_Diff_melt, aes(x = WY, y = value*0.000810714/1000)) +
      geom_bar(aes(fill = variable), position = "stack", stat = 'identity', color = 'black', width = 0.95, size = 0.1) +
      scale_x_continuous(limits = c(1990.4,2011.6), breaks = seq(1991,2011,by = 2),expand = c(0,0))  +
      scale_y_continuous(limits = c(-10,10), breaks = seq(-10,10,by = 5), expand = c(0,0)) +
      xlab('') +
      ylab('Volume (TAF)') +
      ggtitle('SWBM Annual Water Budget Difference: MAR') +
      scale_fill_manual(values = SWBM_colors)+
      theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_rect(color = 'black', fill = NA),
            plot.background = element_rect(color = NA, fill = NA),
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), 
            axis.text = element_text(size = 8),
            plot.title = element_text(hjust = 0.5),
            legend.position = c(0.25, 0.95), 
            legend.key = element_rect(fill = NA, color = NA),
            legend.background = element_rect(fill = NA, color = NA),
            legend.direction = 'horizontal',
            legend.text = element_text(size = 6, margin = margin(r=1,l=1, unit = 'pt')),
            legend.key.height = unit(10,'pt')
      )
  )
  #graphics.off()
  
  MODFLOW_Budget(LST_MAR_Name, WB_Components_MODFLOW,'MAR')
  Water_Budget_MODFLOW_MAR_Annual_Plotting = data.frame(Water_Year = rep(seq(1991,2011),each = 12),
                                                    Recharge = MODFLOW_Budget_Monthly_MAR$RECHARGE_net_m3,
                                                    ET = MODFLOW_Budget_Monthly_MAR$ET_SEGMENTS_net_m3,
                                                    Storage = MODFLOW_Budget_Monthly_MAR$STORAGE_net_m3,
                                                    Drains = MODFLOW_Budget_Monthly_MAR$DRAINS_net_m3,
                                                    `Stream Leakage` = MODFLOW_Budget_Monthly_MAR$STREAM_LEAKAGE_net_m3,
                                                    Wells = -MODFLOW_Budget_Monthly_MAR$WELLS_out_m3,              #negative sign since flux is out
                                                    `Canal Seepage/MFR` = MODFLOW_Budget_Monthly_MAR$WELLS_in_m3)
  names(Water_Budget_MODFLOW_MAR_Annual_Plotting) = c('Water Year', MODFLOW_flux_labels)
  Water_Budget_MODFLOW_MAR_Annual_Plotting = aggregate(.~`Water Year`,Water_Budget_MODFLOW_MAR_Annual_Plotting, FUN = sum)
  Water_Budget_MODFLOW_MAR_Annual_Diff = Water_Budget_MODFLOW_MAR_Annual_Plotting - Water_Budget_MODFLOW_Annual_Plotting
  Water_Budget_MODFLOW_MAR_Annual_Diff$`Water Year` = seq(1991,2011)
  Water_Budget_MODFLOW_MAR_Annual_Diff_Plotting_melt = melt(Water_Budget_MODFLOW_MAR_Annual_Diff, id.vars = 'Water Year')
  Water_Budget_MODFLOW_MAR_Annual_Diff_Plotting_melt$variable = factor(Water_Budget_MODFLOW_MAR_Annual_Diff_Plotting_melt$variable, levels = c('Recharge','ET','Storage','Drains','Stream Leakage','Wells', 'Canal Seepage/MFR'))
  Water_Budget_MODFLOW_MAR_Annual_Diff_Plotting_melt = Water_Budget_MODFLOW_MAR_Annual_Diff_Plotting_melt[order(Water_Budget_MODFLOW_MAR_Annual_Diff_Plotting_melt$variable),]
  
  #pdf(paste0(out_dir,'/Water_Budget_Annual_MAR_Diff_MODFLOW_Mm3.pdf'), width = 8.5, height = 4)
  (MODFLOW_Annual_MAR_Diff_Budget_Plot_TAF = ggplot(Water_Budget_MODFLOW_MAR_Annual_Diff_Plotting_melt, aes(x = `Water Year`, y = value*0.000810714/1000)) +
      geom_bar(aes(fill = variable), position = "stack", stat = 'identity', color = 'black', width = 0.95, size = 0.1) +
      scale_x_continuous(limits = c(1990.4,2011.6), breaks = seq(1991,2011,by = 2),expand = c(0,0))  +
      scale_y_continuous(limits = c(-10,10), breaks = seq(-10,10,by = 5), expand = c(0,0)) +
      xlab('') +
      ylab('Volume (TAF)') +
      ggtitle('MODFLOW Annual Water Budget Difference: MAR') +
      scale_fill_manual(values = MODFLOW_colors)+
      theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_rect(color = 'black', fill = NA),
            plot.background = element_rect(color = NA, fill = NA),
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), 
            axis.text = element_text(size = 8),
            plot.title = element_text(hjust = 0.5),
            legend.position = c(0.25, 0.95), 
            legend.key = element_rect(fill = NA, color = NA),
            legend.background = element_rect(fill = NA, color = NA),
            legend.direction = 'horizontal',
            legend.text = element_text(size = 6, margin = margin(r=1,l=1, unit = 'pt')),
            legend.key.height = unit(10,'pt')
      )
  )
  #graphics.off()
  }

##############################################################################################
##############                   COMPARE ILR BUDGET                 ##########################
##############################################################################################
if (COMPARE_ILR_BUDGET==TRUE){
  # SWBM Budget
  SWBM_ILR_Budget_Monthly = read.table('monthly_water_budget_ILR.dat', header = T)
  names(SWBM_ILR_Budget_Monthly) = c('Month',SWBM_Terms)  
  SWBM_ILR_Budget_Monthly$Month = format(seq(as.Date("1990/10/1"), by = "month", length.out = 252),'%b-%Y')
  SWBM_ILR_Budget_Monthly$WY = rep(seq(1991,2011),each = 12)
  SWBM_ILR_Budget_Annual = aggregate(.~WY,SWBM_ILR_Budget_Monthly[,!names(SWBM_ILR_Budget_Monthly)%in%'Month'], FUN = sum)
  SWBM_ILR_Monthly_Diff = subset(SWBM_ILR_Budget_Monthly, select = SWBM_Terms) - subset(SWBM_Budget_Monthly, select = SWBM_Terms)
  SWBM_ILR_Monthly_Diff$WY = rep(seq(1991,2011),each = 12)
  SWBM_ILR_Annual_Diff = aggregate(.~WY, SWBM_ILR_Monthly_Diff, FUN = sum)                           
  SWBM_ILR_Annual_Diff_melt = melt(SWBM_ILR_Annual_Diff, id.vars = 'WY')
  SWBM_ILR_Annual_Diff_melt$variable = factor(SWBM_ILR_Annual_Diff_melt$variable, levels = SWBM_flux_labels)
  SWBM_ILR_Annual_Diff_melt = SWBM_ILR_Annual_Diff_melt[order(SWBM_ILR_Annual_Diff_melt$variable),]
  
  #pdf(paste0(out_dir,'/Water_Budget_Annual_ILR_Diff_SWBM_Mm3.pdf'), width = 8.5, height = 4)
  (SWBM_Annual_ILR_Diff_Plot_Mm3 = ggplot(SWBM_ILR_Annual_Diff_melt, aes(x = WY, y = value/1E6)) +
      geom_bar(aes(fill = variable), position = "stack", stat = 'identity', color = 'black', width = 0.95, size = 0.1) +
      scale_x_continuous(limits = c(1990.4,2011.6), breaks = seq(1991,2011,by = 2),expand = c(0,0))  +
      scale_y_continuous(limits = c(-25,25), breaks = seq(-25,25,by = 5), expand = c(0,0)) +
      xlab('') +
      ylab(bquote('Volume ('*Mm^3*')')) +
      ggtitle('SWBM Annual Water Budget Difference: ILR') +
      scale_fill_manual(values = SWBM_colors)+
      theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_rect(color = 'black', fill = NA),
            plot.background = element_rect(color = NA, fill = NA),
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), 
            axis.text = element_text(size = 8),
            plot.title = element_text(hjust = 0.5),
            legend.position = c(0.25, 0.95), 
            legend.key = element_rect(fill = NA, color = NA),
            legend.background = element_rect(fill = NA, color = NA),
            legend.direction = 'horizontal',
            legend.text = element_text(size = 6, margin = margin(r=1,l=1, unit = 'pt')),
            legend.key.height = unit(10,'pt')
      )
  )
  #graphics.off()
  
  MODFLOW_Budget(LST_ILR_Name, WB_Components_MODFLOW,'ILR')
  Water_Budget_MODFLOW_ILR_Annual_Plotting = data.frame(Water_Year = rep(seq(1991,2011),each = 12),
                                                        Recharge = MODFLOW_Budget_Monthly_ILR$RECHARGE_net_m3,
                                                        ET = MODFLOW_Budget_Monthly_ILR$ET_SEGMENTS_net_m3,
                                                        Storage = MODFLOW_Budget_Monthly_ILR$STORAGE_net_m3,
                                                        Drains = MODFLOW_Budget_Monthly_ILR$DRAINS_net_m3,
                                                        `Stream Leakage` = MODFLOW_Budget_Monthly_ILR$STREAM_LEAKAGE_net_m3,
                                                        Wells = -MODFLOW_Budget_Monthly_ILR$WELLS_out_m3,              #negative sign since flux is out
                                                        `Canal Seepage/MFR` = MODFLOW_Budget_Monthly_ILR$WELLS_in_m3)
  names(Water_Budget_MODFLOW_ILR_Annual_Plotting) = c('Water Year', MODFLOW_flux_labels)
  Water_Budget_MODFLOW_ILR_Annual_Plotting = aggregate(.~`Water Year`,Water_Budget_MODFLOW_ILR_Annual_Plotting, FUN = sum)
  Water_Budget_MODFLOW_ILR_Annual_Diff = Water_Budget_MODFLOW_ILR_Annual_Plotting - Water_Budget_MODFLOW_Annual_Plotting
  Water_Budget_MODFLOW_ILR_Annual_Diff$`Water Year` = seq(1991,2011)
  Water_Budget_MODFLOW_ILR_Annual_Diff_Plotting_melt = melt(Water_Budget_MODFLOW_ILR_Annual_Diff, id.vars = 'Water Year')
  Water_Budget_MODFLOW_ILR_Annual_Diff_Plotting_melt$variable = factor(Water_Budget_MODFLOW_ILR_Annual_Diff_Plotting_melt$variable, levels = c('Recharge','ET','Storage','Drains','Stream Leakage','Wells', 'Canal Seepage/MFR'))
  Water_Budget_MODFLOW_ILR_Annual_Diff_Plotting_melt = Water_Budget_MODFLOW_ILR_Annual_Diff_Plotting_melt[order(Water_Budget_MODFLOW_ILR_Annual_Diff_Plotting_melt$variable),]
  
  #pdf(paste0(out_dir,'/Water_Budget_Annual_ILR_Diff_MODFLOW_Mm3.pdf'), width = 8.5, height = 4)
  (MODFLOW_Annual_ILR_Diff_Budget_Plot_TAF = ggplot(Water_Budget_MODFLOW_ILR_Annual_Diff_Plotting_melt, aes(x = `Water Year`, y = value*0.000810714/1000)) +
      geom_bar(aes(fill = variable), position = "stack", stat = 'identity', color = 'black', width = 0.95, size = 0.1) +
      scale_x_continuous(limits = c(1990.4,2011.6), breaks = seq(1991,2011,by = 2),expand = c(0,0))  +
      scale_y_continuous(limits = c(-20,20), breaks = seq(-20,20,by = 10), expand = c(0,0)) +
      xlab('') +
      ylab('Volume (TAF)') +
      ggtitle('MODFLOW Annual Water Budget Difference: ILR') +
      scale_fill_manual(values = MODFLOW_colors)+
      theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_rect(color = 'black', fill = NA),
            plot.background = element_rect(color = NA, fill = NA),
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), 
            axis.text = element_text(size = 8),
            plot.title = element_text(hjust = 0.5),
            legend.position = c(0.25, 0.95), 
            legend.key = element_rect(fill = NA, color = NA),
            legend.background = element_rect(fill = NA, color = NA),
            legend.direction = 'horizontal',
            legend.text = element_text(size = 6, margin = margin(r=1,l=1, unit = 'pt')),
            legend.key.height = unit(10,'pt')
      )
  )
  #graphics.off()
}

##############################################################################################
##############                 COMPARE MAR_ILR BUDGET             ##########################
##############################################################################################
if (COMPARE_MAR_ILR_BUDGET==TRUE){
  # SWBM Budget
  SWBM_MAR_ILR_Budget_Monthly = read.table('monthly_water_budget_MAR_ILR.dat', header = T)
  names(SWBM_MAR_ILR_Budget_Monthly) = c('Month',SWBM_Terms)  
  SWBM_MAR_ILR_Budget_Monthly$Month = format(seq(as.Date("1990/10/1"), by = "month", length.out = 252),'%b-%Y')
  SWBM_MAR_ILR_Budget_Monthly$WY = rep(seq(1991,2011),each = 12)
  SWBM_MAR_ILR_Budget_Annual = aggregate(.~WY,SWBM_MAR_ILR_Budget_Monthly[,!names(SWBM_MAR_ILR_Budget_Monthly)%in%'Month'], FUN = sum)
  SWBM_MAR_ILR_Monthly_Diff = subset(SWBM_MAR_ILR_Budget_Monthly, select = SWBM_Terms) - subset(SWBM_Budget_Monthly, select = SWBM_Terms)
  SWBM_MAR_ILR_Monthly_Diff$WY = rep(seq(1991,2011),each = 12)
  SWBM_MAR_ILR_Annual_Diff = aggregate(.~WY, SWBM_MAR_ILR_Monthly_Diff, FUN = sum)                           
  SWBM_MAR_ILR_Annual_Diff_melt = melt(SWBM_MAR_ILR_Annual_Diff, id.vars = 'WY')
  SWBM_MAR_ILR_Annual_Diff_melt$variable = factor(SWBM_MAR_ILR_Annual_Diff_melt$variable, levels = SWBM_flux_labels)
  SWBM_MAR_ILR_Annual_Diff_melt = SWBM_MAR_ILR_Annual_Diff_melt[order(SWBM_MAR_ILR_Annual_Diff_melt$variable),]
  
  #pdf(paste0(out_dir,'/Water_Budget_Annual_MAR_ILR_Diff_SWBM_Mm3.pdf'), width = 8.5, height = 4)
  (SWBM_Annual_MAR_ILR_Diff_Plot_Mm3 = ggplot(SWBM_MAR_ILR_Annual_Diff_melt, aes(x = WY, y = value/1E6)) +
      geom_bar(aes(fill = variable), position = "stack", stat = 'identity', color = 'black', width = 0.95, size = 0.1) +
      scale_x_continuous(limits = c(1990.4,2011.6), breaks = seq(1991,2011,by = 2),expand = c(0,0))  +
      #scale_y_continuous(limits = c(-20,20), breaks = seq(-20,20,by = 10), expand = c(0,0)) +
      xlab('') +
      ylab(bquote('Volume ('*Mm^3*')')) +
      ggtitle('SWBM Annual Water Budget Difference: MAR_ILR') +
      scale_fill_manual(values = SWBM_colors)+
      theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_rect(color = 'black', fill = NA),
            plot.background = element_rect(color = NA, fill = NA),
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), 
            axis.text = element_text(size = 8),
            plot.title = element_text(hjust = 0.5),
            legend.position = c(0.25, 0.95), 
            legend.key = element_rect(fill = NA, color = NA),
            legend.background = element_rect(fill = NA, color = NA),
            legend.direction = 'horizontal',
            legend.text = element_text(size = 6, margin = margin(r=1,l=1, unit = 'pt')),
            legend.key.height = unit(10,'pt')
      )
  )
  #graphics.off()
  
  MODFLOW_Budget(LST_MAR_ILR_Name, WB_Components_MODFLOW,'MAR_ILR')
  Water_Budget_MODFLOW_MAR_ILR_Annual_Plotting = data.frame(Water_Year = rep(seq(1991,2011),each = 12),
                                                        Recharge = MODFLOW_Budget_Monthly_MAR_ILR$RECHARGE_net_m3,
                                                        ET = MODFLOW_Budget_Monthly_MAR_ILR$ET_SEGMENTS_net_m3,
                                                        Storage = MODFLOW_Budget_Monthly_MAR_ILR$STORAGE_net_m3,
                                                        Drains = MODFLOW_Budget_Monthly_MAR_ILR$DRAINS_net_m3,
                                                        `Stream Leakage` = MODFLOW_Budget_Monthly_MAR_ILR$STREAM_LEAKAGE_net_m3,
                                                        Wells = -MODFLOW_Budget_Monthly_MAR_ILR$WELLS_out_m3,              #negative sign since flux is out
                                                        `Canal Seepage/MFR` = MODFLOW_Budget_Monthly_MAR_ILR$WELLS_in_m3)
  names(Water_Budget_MODFLOW_MAR_ILR_Annual_Plotting) = c('Water Year', MODFLOW_flux_labels)
  Water_Budget_MODFLOW_MAR_ILR_Annual_Plotting = aggregate(.~`Water Year`,Water_Budget_MODFLOW_MAR_ILR_Annual_Plotting, FUN = sum)
  Water_Budget_MODFLOW_MAR_ILR_Annual_Diff = Water_Budget_MODFLOW_MAR_ILR_Annual_Plotting - Water_Budget_MODFLOW_Annual_Plotting
  Water_Budget_MODFLOW_MAR_ILR_Annual_Diff$`Water Year` = seq(1991,2011)
  Water_Budget_MODFLOW_MAR_ILR_Annual_Diff_Plotting_melt = melt(Water_Budget_MODFLOW_MAR_ILR_Annual_Diff, id.vars = 'Water Year')
  Water_Budget_MODFLOW_MAR_ILR_Annual_Diff_Plotting_melt$variable = factor(Water_Budget_MODFLOW_MAR_ILR_Annual_Diff_Plotting_melt$variable, levels = c('Recharge','ET','Storage','Drains','Stream Leakage','Wells', 'Canal Seepage/MFR'))
  Water_Budget_MODFLOW_MAR_ILR_Annual_Diff_Plotting_melt = Water_Budget_MODFLOW_MAR_ILR_Annual_Diff_Plotting_melt[order(Water_Budget_MODFLOW_MAR_ILR_Annual_Diff_Plotting_melt$variable),]
  
  #pdf(paste0(out_dir,'/Water_Budget_Annual_MAR_ILR_Diff_MODFLOW_Mm3.pdf'), width = 8.5, height = 4)
  (MODFLOW_Annual_MAR_ILR_Diff_Budget_Plot_TAF = ggplot(Water_Budget_MODFLOW_MAR_ILR_Annual_Diff_Plotting_melt, aes(x = `Water Year`, y = value*0.000810714/1000)) +
      geom_bar(aes(fill = variable), position = "stack", stat = 'identity', color = 'black', width = 0.95, size = 0.1) +
      scale_x_continuous(limits = c(1990.4,2011.6), breaks = seq(1991,2011,by = 2),expand = c(0,0))  +
      scale_y_continuous(limits = c(-30,30), breaks = seq(-30,30,by = 10), expand = c(0,0)) +
      xlab('') +
      ylab('Volume (TAF)') +
      ggtitle('MODFLOW Annual Water Budget Difference: MAR_ILR') +
      scale_fill_manual(values = MODFLOW_colors)+
      theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_rect(color = 'black', fill = NA),
            plot.background = element_rect(color = NA, fill = NA),
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), 
            axis.text = element_text(size = 8),
            plot.title = element_text(hjust = 0.5),
            legend.position = c(0.25, 0.95), 
            legend.key = element_rect(fill = NA, color = NA),
            legend.background = element_rect(fill = NA, color = NA),
            legend.direction = 'horizontal',
            legend.text = element_text(size = 6, margin = margin(r=1,l=1, unit = 'pt')),
            legend.key.height = unit(10,'pt')
      )
  )
  #graphics.off()
}
# 
# ##############################################################################################
# ##############                 MONTHLY STREAM LEAKAGE               ##########################
# ##############################################################################################
# Monthly_Stream_Leakage_Mm3 = data.frame(Date = StartingMonths,
#                                      Stream_Leakage = STREAM_LEAKAGE_SP_Vol_net/1E6,
#                                      Year = rep(seq(1991,2011),each = 12))
# (Monthly_Stream_Leakage_Plot = ggplot(data = Monthly_Stream_Leakage_Mm3, aes(x = StartingMonths, y = Stream_Leakage)) +
#     geom_rect(aes(xmin = as.Date('1990/10/1'), xmax = as.Date('1992/09/30'), ymin = -30 , ymax = 30), fill = '#ff8282') +
#     geom_rect(aes(xmin = as.Date('1993/10/1'), xmax = as.Date('1994/09/30'), ymin = -30 , ymax = 30), fill = '#ff8282') +
#     geom_rect(aes(xmin = as.Date('1994/10/1'), xmax = as.Date('1999/09/30'), ymin = -30 , ymax = 30), fill = 'skyblue') +
#     geom_rect(aes(xmin = as.Date('2000/10/1'), xmax = as.Date('2002/09/30'), ymin = -30 , ymax = 30), fill = '#ff8282') +
#     geom_rect(aes(xmin = as.Date('2005/10/1'), xmax = as.Date('2006/09/30'), ymin = -30 , ymax = 30), fill = 'skyblue') +
#     geom_rect(aes(xmin = as.Date('2006/10/1'), xmax = as.Date('2009/09/30'), ymin = -30 , ymax = 30), fill = '#ff8282') +
#     geom_rect(aes(xmin = as.Date('2010/10/1'), xmax = as.Date('2011/09/30'), ymin = -30 , ymax = 30), fill = 'skyblue') +
#     geom_hline(yintercept = 0, size = 0.25) +
#     geom_line(size = 0.5) +
#     geom_point(size = 0.75) +
#     scale_x_date(limits = c(as.Date('Oct-01-1990', format = '%b-%m-%y'),
#                             as.Date('Oct-01-2011', format = '%b-%m-%y')),
#                  breaks = seq(as.Date("1990/10/1"), by = "2 years", length.out = 22), expand = c(0,0),
#                  date_labels = ('%b-%y'))  +
#     scale_y_continuous(limits = c(-30,30), breaks = seq(-30,30,by = 10), expand = c(0,0)) +
#     ylab(ylab(bquote('Net Stream-Aquifer Flux ('*Mm^3*')'))) +
#     ggtitle('Monthly Net Stream-Aquifer Flux') +
#     theme(panel.background = element_blank(),
#           panel.border = element_rect(fill=NA, color = 'black'),
#           axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
#           axis.text.y = element_text(size = 8),
#           axis.ticks = element_line(size = 0.2),
#           plot.title = element_text(hjust = 0.5, size = 10),
#           axis.title.x = element_blank(),
#           axis.title.y = element_text(size = 8) 
#     )
# )
# 
# ##############################################################################################
# ##############                  ANNUAL STREAM LEAKAGE               ##########################
# ##############################################################################################
# Annual_Stream_Leakage_Mm3 = data.frame(Stream_Leakage = STREAM_LEAKAGE_SP_Vol_net/1E6,
#                                     Year = rep(seq(1991,2011),each = 12))
# Annual_Stream_Leakage_Mm3 = aggregate(.~Year, data = Annual_Stream_Leakage_Mm3, FUN = sum)
# (Annual_Stream_Leakage_Plot = ggplot(data = Annual_Stream_Leakage_Mm3, aes(x = Year, y = Stream_Leakage)) +
#     #shade areas for wet and dry/critical years
#     geom_rect(aes(xmin = 1990.5, xmax = 1992.5, ymin = -60 , ymax = 60), fill = '#ff8282') +
#     geom_rect(aes(xmin = 1993.5, xmax = 1994.5, ymin = -60 , ymax = 60), fill = '#ff8282') +
#     geom_rect(aes(xmin = 1994.5, xmax = 1999.5, ymin = -60 , ymax = 60), fill = 'skyblue') +
#     geom_rect(aes(xmin = 2000.5, xmax = 2002.5, ymin = -60 , ymax = 60), fill = '#ff8282') +
#     geom_rect(aes(xmin = 2005.5, xmax = 2006.5, ymin = -60 , ymax = 60), fill = 'skyblue') +
#     geom_rect(aes(xmin = 2006.5, xmax = 2009.5, ymin = -60 , ymax = 60), fill = '#ff8282') +
#     geom_rect(aes(xmin = 2010.5, xmax = 2011.5, ymin = -60 , ymax = 60), fill = 'skyblue') +
#     geom_hline(yintercept = 0, size = 0.25) +
#     geom_line(size = 0.5) +
#     geom_point(size = 0.75) +
#     scale_x_continuous(limits = c(1990.5,2011.5), breaks = seq(1991,2011,by = 2), expand = c(0,0)) +
#     scale_y_continuous(limits = c(-60,60), breaks = seq(-60,60,by = 20), expand = c(0,0)) +
#     ylab(ylab(bquote('Annual Net Stream-Aquifer Flux ('*Mm^3*')'))) +
#     ggtitle('Net Stream-Aquifer Flux') +
#     theme(panel.background = element_blank(),
#           panel.border = element_rect(fill=NA, color = 'black'),
#           axis.text.x = element_text(angle = 45, hjust = 1, vjus 0.7, size = 8),
#           axis.text.y = element_text(size = 8),
#           axis.ticks = element_line(size = 0.2),
#           plot.title = element_text(hjust = 0.5, size = 10),
#           axis.title.x = element_blank(),
#           axis.title.y = element_text(size = 8) 
#     )
# )
# 
# ##############################################################################################
# ##############              MONTHLY GROUNDWATER PUMPING             ##########################
# ##############################################################################################
# Monthly_Groundwater_Pumping_Mm3 = data.frame(Date = StartingMonths,
#                                         Groundwater_Pumping = WELLS_SP_Vol_out/1E6,
#                                         Year = rep(seq(1991,2011),each = 12))
# (Monthly_Groundwater_Pumping_Plot = ggplot(data = Monthly_Groundwater_Pumping_Mm3, aes(x = StartingMonths, y = Groundwater_Pumping)) +
#     geom_rect(aes(xmin = as.Date('1990/10/1'), xmax = as.Date('1992/09/30'), ymin = -0 , ymax = 20), fill = '#ff8282') +
#     geom_rect(aes(xmin = as.Date('1993/10/1'), xmax = as.Date('1994/09/30'), ymin = -0 , ymax = 20), fill = '#ff8282') +
#     geom_rect(aes(xmin = as.Date('1994/10/1'), xmax = as.Date('1999/09/30'), ymin = -0 , ymax = 20), fill = 'skyblue') +
#     geom_rect(aes(xmin = as.Date('2000/10/1'), xmax = as.Date('2002/09/30'), ymin = -0 , ymax = 20), fill = '#ff8282') +
#     geom_rect(aes(xmin = as.Date('2005/10/1'), xmax = as.Date('2006/09/30'), ymin = -0 , ymax = 20), fill = 'skyblue') +
#     geom_rect(aes(xmin = as.Date('2006/10/1'), xmax = as.Date('2009/09/30'), ymin = -0 , ymax = 20), fill = '#ff8282') +
#     geom_rect(aes(xmin = as.Date('2010/10/1'), xmax = as.Date('2011/09/30'), ymin = -0 , ymax = 20), fill = 'skyblue') +
#     geom_hline(yintercept = 0, size = 0.25) +
#     geom_line(size = 0.5) +
#     geom_point(size = 0.75) +
#     scale_x_date(limits = c(as.Date('Oct-01-1990', format = '%b-%m-%y'),
#                             as.Date('Oct-01-2011', format = '%b-%m-%y')),
#                  breaks = seq(as.Date("1990/10/1"), by = "2 years", length.out = 22), expand = c(0,0),
#                  date_labels = ('%b-%y'))  +
#     scale_y_continuous(limits = c(0,20), breaks = seq(0,20,by = 5), expand = c(0,0)) +
#     ylab(ylab(bquote('Monthly Groundwater Pumping ('*Mm^3*')'))) +
#     ggtitle('Monthly Groundwater Pumping') +
#     theme(panel.background = element_blank(),
#           panel.border = element_rect(fill=NA, color = 'black'),
#           axis.text.x = element_text(angle = 45, hjust = 1, vjus 0.7, size = 8),
#           axis.text.y = element_text(size = 8),
#           axis.ticks = element_line(size = 0.2),
#           plot.title = element_text(hjust = 0.5, size = 10),
#           axis.title.x = element_blank(),
#           axis.title.y = element_text(size = 8) 
#     )
# )
# 
# ##############################################################################################
# ##############               ANNUAL GROUNDWATER PUMPING             ##########################
# ##############################################################################################
# Annual_Groundwater_Pumping_Mm3 = data.frame(Groundwater_Pumping = WELLS_SP_Vol_out/1E6,
#                                        Year = rep(seq(1991,2011),each = 12))
# Annual_Groundwater_Pumping_Mm3 = aggregate(.~Year, data = Annual_Groundwater_Pumping_Mm3, FUN = sum)
# (Annual_Groundwater_Pumping_Plot = ggplot(data = Annual_Groundwater_Pumping_Mm3, aes(x = Year, y = Groundwater_Pumping)) +
#     #shade areas for wet and dry/critical years
#     geom_rect(aes(xmin = 1990.5, xmax = 1992.5, ymin = 0 , ymax = 70), fill = '#ff8282') +
#     geom_rect(aes(xmin = 1993.5, xmax = 1994.5, ymin = 0 , ymax = 70), fill = '#ff8282') +
#     geom_rect(aes(xmin = 1994.5, xmax = 1999.5, ymin = 0 , ymax = 70), fill = 'skyblue') +
#     geom_rect(aes(xmin = 2000.5, xmax = 2002.5, ymin = 0 , ymax = 70), fill = '#ff8282') +
#     geom_rect(aes(xmin = 2005.5, xmax = 2006.5, ymin = 0 , ymax = 70), fill = 'skyblue') +
#     geom_rect(aes(xmin = 2006.5, xmax = 2009.5, ymin = 0 , ymax = 70), fill = '#ff8282') +
#     geom_rect(aes(xmin = 2010.5, xmax = 2011.5, ymin = 0 , ymax = 70), fill = 'skyblue') +
#     geom_hline(yintercept = 0, size = 0.25) +
#     geom_line(size = 0.5) +
#     geom_point(size = 0.75) +
#     scale_x_continuous(limits = c(1990.5,2011.5), breaks = seq(1991,2011,by = 2), expand = c(0,0)) +
#     scale_y_continuous(limits = c(0,70), breaks = seq(0,70,by = 10), expand = c(0,0)) +
#     ylab(ylab(bquote('Groundwater Pumping ('*Mm^3*')'))) +
#     ggtitle('Annual Groundwater Pumping') +
#     theme(panel.background = element_blank(),
#           panel.border = element_rect(fill=NA, color = 'black'),
#           axis.text.x = element_text(angle = 45, hjust = 1, vjus 0.7, size = 8),
#           axis.text.y = element_text(size = 8),
#           axis.ticks = element_line(size = 0.2),
#           plot.title = element_text(hjust = 0.5, size = 10),
#           axis.title.x = element_blank(),
#           axis.title.y = element_text(size = 8) 
#     )
# )

