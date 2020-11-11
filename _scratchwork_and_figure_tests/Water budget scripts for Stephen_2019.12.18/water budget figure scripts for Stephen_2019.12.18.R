#Figures for AGU 2019



# setup -------------------------------------------------------------------


rm(list=ls())
library(ggplot2)
library(reshape2)
library(dplyr)
library(grid)
library(rstudioapi)
library(plyr)
library(cowplot)
library(zoo)
library(dataRetrieval) #for usgs data
library(lubridate)
library(scales) #to label water budgets
library(colorspace)
library(magrittr)
library(data.table) #melt function
library(plotly)
library(gridExtra)



# Directory names

# Model subsystem directories - folders containing model results 
#svihm_dir = "C:/Users/Claire/Documents/GitHub/SVIHM"
svihm_dir = "C:/Users/cmkouba/Documents/GitHub/SVIHM"
swbm_dir = file.path(svihm_dir,"SWBM") 
mf_dir =  file.path(svihm_dir, "MODFLOW","hist") #historical model period#
mf_dir =  file.path(svihm_dir, "MODFLOW","pvar_a_extr_95_07") #historical model period


# Project Directory
#gsp_dir = "C:/Users/Claire/Documents/GitHub/SiskiyouGSP2022" #overall project folder
gsp_dir = "C:/Users/cmkouba/Documents/GitHub/SiskiyouGSP2022" #overall project folder
#Budget and figure directories
budget_dir = file.path(gsp_dir, "GSP_Analyses", "Scott_Water_Budget") #Store tables, intermediate products
# dir.create(budget_dir, showWarnings = FALSE)   #Create Results directory if it doesn't exist
figure_out_dir = file.path(budget_dir, "Figures") #store figures

#Source sub-scripts for reading Modflow outputs
source(file.path(budget_dir, 'MODFLOW_Budget.R'))
source(file.path(budget_dir, 'SVIHM_SFR_inputs.R'))

#read in water year types for annual water budgets
wat_year_type = read.csv(file.path(gsp_dir,"GSP_Analyses", "Scott_Groundwater_Conditions", "scott water year types.csv"))




# Water budget terms
start_wy = 1991
end_wy = 2018
nstress = 336

#Water budget labels and colors
SWBM_flux_labels = c('Precip','ET','SW_Irr', 'Recharge','GW_Irr','Storage')
SWBM_colors = c('deepskyblue2', 'orange2', 'blue', 'green4','midnightblue', 'darkgray' )
swbm_legend = data.frame("Component" = SWBM_flux_labels, "Color" = SWBM_colors)

MODFLOW_flux_labels = c('Recharge','ET','Storage','Drains','Stream_Leakage','Wells', 'Canal_Seepage_and_MFR')
MODFLOW_colors = c('green4', 'orange2', 'darkgray', 'mediumorchid2','royalblue', 'midnightblue', 'brown' )
aq_legend = data.frame("Component" = MODFLOW_flux_labels, "Color" = MODFLOW_colors)


Streamflow_flux_labels = c('Inflow', 'Overland Flow', 'Farmers Ditch', 'SVID Ditch', 'Stream leakage', 'Outflow', 'Storage')
Streamflow_colors = c('turquoise2', 'sienna3', 'green2', 'green4', 'royalblue', 'lightgoldenrod1', 'darkgray')  
sfr_legend = data.frame("Component" = Streamflow_flux_labels, "Color" = Streamflow_colors)


# 1. Single-year water budgets----------------------------------------------------------------
# Example of a water budget for a wet, dry, and normal year (two types of normal, 2010 and 2015)


# 1a) import budgets ----------------------------------------------------------


# 1ai) Import SWBM
monthly_water_budget_hist = read.table(file.path(swbm_dir,"hist","monthly_water_budget.dat"), header = TRUE)
swbm_monthly = monthly_water_budget_hist

#Process for plotting
start_month = as.Date(paste0(start_wy-1, "-10-01")); end_month = as.Date(paste0(end_wy, "-09-01"))
swbm_monthly$Month = seq(start_month, end_month, by="month")
swbm_monthly$Stress_Period = NULL
swbm_monthly_melt = melt(swbm_monthly, id.vars = "Month")
swbm_monthly_melt$variable = factor(swbm_monthly_melt$variable, levels = SWBM_flux_labels)
swbm_monthly_melt = swbm_monthly_melt[order(swbm_monthly_melt$variable),]

# 1aii) Import aquifer (MODFLOW)

# #Modflow import setup
# PRINT_BUDGET=TRUE
# source(file.path(budget_dir, 'MODFLOW_Budget.R'))
#LST_Name = 'SVIHM.lst'
#WB_Components_MODFLOW = c('STORAGE', 'CONSTANT_HEAD', 'WELLS', 'RECHARGE', 'ET_SEGMENTS','STREAM_LEAKAGE', 'DRAINS')
##Specify out_dir
#aq_monthly_raw = MODFLOW_Budget(mf_dir, LST_Name, WB_Components_MODFLOW, end_wy=end_wy)
#if (PRINT_BUDGET==TRUE){
# write.table(aq_monthly_raw, file = file.path(budget_dir,'MODFLOW_Water_Budget.dat'), row.names = F, quote = F)
#}

# or, if you've run the modflow import setup before, you can read in:
# scenarios for Geeta paper
#aq_monthly_raw = read.table(file.path(budget_dir, "MODFLOW_Water_Budget_alt.dat"),header = T)
#aq_monthly_raw = read.table(file.path(budget_dir, "MODFLOW_Water_Budget_hist.dat"),header = T)
aq_monthly_raw = read.table(file.path(budget_dir, "MODFLOW_Water_Budget.dat"),header = T)
aq_monthly_raw$Month = as.Date(paste0("01-",aq_monthly_raw$Month), format = "%d-%b-%Y")

#Process for plotting
aq_monthly = data.frame(Month = aq_monthly_raw$Month,
                        Recharge = aq_monthly_raw$RECHARGE_net_m3,
                        ET = aq_monthly_raw$ET_SEGMENTS_net_m3,
                        Storage = aq_monthly_raw$STORAGE_net_m3,
                        Drains = aq_monthly_raw$DRAINS_net_m3,
                        `Stream Leakage` = aq_monthly_raw$STREAM_LEAKAGE_net_m3,
                        Wells = -aq_monthly_raw$WELLS_out_m3,              #negative sign since flux is out
                        `Canal Seepage/MFR` = aq_monthly_raw$WELLS_in_m3)
names(aq_monthly) = c('Month', MODFLOW_flux_labels)
aq_monthly_melt = melt(aq_monthly, id.vars = 'Month')
aq_monthly_melt$variable = factor(aq_monthly_melt$variable, levels = MODFLOW_flux_labels)
aq_monthly_melt = aq_monthly_melt[order(aq_monthly_melt$variable),]

#aq_monthly_melt_alt = aq_monthly_melt

# 1aiii) Import STREAMFLOW Budget

source(file.path(budget_dir, 'SVIHM_SFR_inputs.R'))

#Import streamflow budget and add columns from modflow
Streamflow_Monthly_m3 = SVIHM_SFR_inputs(file.path(mf_dir,'SVIHM.sfr'), nstress)
Streamflow_Monthly_m3$Stream_Leakage_m3 = -aq_monthly_raw$STREAM_LEAKAGE_net_m3 #made negative to convert from MODFLOW to SFR budget
FJ_Outflow = read.table(file.path(mf_dir,'Streamflow_FJ_SVIHM.dat'), skip = 2)[,3]
FJ_Outflow = data.frame(Month = format(seq(as.Date('1990-10-01'), as.Date(paste0(end_wy,'-09-30')), by = 'day'),format = '%b-%Y'),
                        FJ_Flow_m3 = FJ_Outflow)
FJ_Outflow = aggregate(.~Month, FJ_Outflow, FUN = sum)
FJ_Outflow$Month = as.Date(paste0('01-',FJ_Outflow$Month), '%d-%b-%Y')
FJ_Outflow = FJ_Outflow[order(FJ_Outflow$Month),]
Streamflow_Monthly_m3$Outflow_m3 = -FJ_Outflow$FJ_Flow_m3
Streamflow_Monthly_m3$Storage = -rowSums(Streamflow_Monthly_m3[,-1])


#Process streamflow for plotting
# Streamflow_Monthly_m3$WY = rep(seq(1991,end_wy), each = 12)
Streamflow_Monthly_m3_melt = melt(Streamflow_Monthly_m3, id.vars = c('Date'))
Streamflow_Monthly_m3_melt$Month = format(Streamflow_Monthly_m3_melt$Date,format = '%b')
Streamflow_Monthly_m3_melt$Month = factor(Streamflow_Monthly_m3_melt$Month, levels = c(month.abb[10:12],month.abb[1:9]))
# Streamflow_Monthly_m3_melt = Streamflow_Monthly_m3_melt[order(Streamflow_Monthly_m3_melt$Month),]


# 1b) Plot budgets ----------------------------------------------------------

wy = 2017

#Plot Streamflow
sfr_wy = ggplot(data = Streamflow_Monthly_m3_melt, 
                aes(x = Date, y = value/10^6), group = variable) +
  geom_bar(aes(fill = variable), stat="identity", position="stack") +
  scale_fill_manual(values=c(Inflows_m3="turquoise2", Drain_Inflow_m3= "sienna3",
                               Farmers_Div_m3="green2", SVID_Div_m3="green4",
                               Stream_Leakage_m3="royalblue", Outflow_m3="lightgoldenrod1",
                               Storage_m3="darkgray"))+
  labs(y=expression(paste("Volume (km"^"3",")")))+
  scale_x_date(date_breaks = "2 month", labels=date_format("%b"),
               limits = as.Date(c(paste0(wy-1,'-10-01'),paste0(wy,'-10-01'))))+
  scale_y_continuous(breaks = seq(-200,200, by=50),limits = c(-200, 200))+
  theme(legend.position = "none")

#Plot SWBM
swbm_wy = ggplot(data = swbm_monthly_melt, 
                 aes(x = Month, y = value/10^6), fill = variable) +
  geom_bar(aes(fill=variable), position = "stack", stat = 'identity') +
  scale_fill_manual( values=c("Precip"="deepskyblue2", "ET"="orange2",
                               "SW_Irr"="blue", "Recharge"="green4","GW_Irr" = "midnightblue",
                               "Storage"="darkgray"))+
  labs(y=expression(paste("Volume (km"^"3",")")))+
  scale_x_date(date_breaks = "2 month", labels=date_format("%b"),
               limits = as.Date(c(paste0(wy-1,'-10-01'),paste0(wy,'-10-01'))))+
  scale_y_continuous(breaks = seq(-40,40, by=20), limits = c(-40, 40))+
  theme(legend.position = "none")

#Plot Aquifer
aq_wy = ggplot(data = aq_monthly_melt, 
               aes(x = Month, y = value/10^6, fill = variable)) +
  geom_bar( position = "stack", stat = 'identity') +
  scale_fill_manual(values=c("Recharge"="green4", "ET"="orange2",
                               "Storage"="darkgray",  "Drains"="mediumorchid2",
                               "Stream_Leakage"="royalblue",
                               "Wells"="midnightblue", "Canal_Seepage_and_MFR"="brown"))+
  labs(y=expression(paste("Volume (km"^"3",")")))+
  scale_x_date(date_breaks = "2 month", labels=date_format("%b"),
               limits = as.Date(c(paste0(wy-1,'-10-01'),paste0(wy,'-10-01'))))+
  scale_y_continuous(breaks = seq(-40,40, by=20),limits = c(-40, 40))+
  theme(legend.position = "none")


  

#produce graphics

# agu_wys = c(2014, 2017, 2010, 2015)

png(file.path(figure_out_dir,paste(wy,"wy budgets barcharts.png")), height = 3.75, width = 14.6, 
    units = "in", res = 300)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1,3)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
  print(sfr_wy, vp = vplayout(1,1))
  print(swbm_wy, vp = vplayout(1,2))
  print(aq_wy, vp = vplayout(1,3))
graphics.off()



# 1c) Plot single water year by component -------------------------------------------------------

# #aside: table of totals for aq package
# aq_yearly_melt = aggregate(aq_monthly_melt$value, by=list(aq_monthly_melt$wy, aq_monthly_melt$variable), FUN="sum")
# aq_yearly_melt_agu_wys=aq_yearly_melt[aq_yearly_melt$Group.1 %in% c(2010, 2014, 2015,2017),]
# colnames(aq_yearly_melt_agu_wys) = c("wy", "variable", "value")
# View(round(dcast(aq_yearly_melt_agu_wys, wy ~ variable )/10^6,1))

#subset for comparing 4 years
agu_wys = c(2014, 2017, 2010, 2015)
aq_monthly_melt$wy = wtr_yr(aq_monthly_melt$Month)
aq_monthly_melt$mo = month.abb[month(aq_monthly_melt$Month)]
aq_monthly_melt$mo = factor(x = aq_monthly_melt$mo, levels = month.abb[c(10:12, 1:9)])

aq_wys = aq_monthly_melt[aq_monthly_melt$wy %in% agu_wys,]
aq_wys$wy = factor(x = aq_wys$wy)

pump_wys = aq_wys[aq_wys$variable == "Wells",]
rch_wys = aq_wys[aq_wys$variable == "Recharge",]
leak_wys = aq_wys[aq_wys$variable == "Stream_Leakage",]
stor_wys = aq_wys[aq_wys$variable=="Storage",]

#Plot pumping
pump=ggplot(data = pump_wys, aes(x = mo, y= -value/10^6, group = wy))+ #negative to make pumping positive for plotting
  labs(x = "none",  y = expression(paste("Volume (km"^"3","/month)")), 
       title="Well Pumping")+
  geom_line(aes(colour = wy, size=2))+
  scale_colour_manual(values = c("2014"="orangered", "2017"="deepskyblue3",
                                 "2010"="darkgoldenrod1", "2015"="darkgoldenrod3"))+
  theme_bw()+
  theme(legend.position="none", axis.title.x=element_blank(),
        axis.text = element_text(size=18), 
        axis.title=element_text(size=20), plot.title = element_text(size=20))
#Plot recharge
rch = ggplot(data = rch_wys, aes(x = mo, y= value/10^6, group = wy))+ 
  labs(x = "Month in water year", y = expression(paste("Volume (km"^"3","/month)")), 
       title="Recharge")+
  geom_line(aes(colour = wy, size=2))+
  scale_colour_manual(values = c("2014"="orangered", "2017"="deepskyblue3",
                                 "2010"="darkgoldenrod1", "2015"="darkgoldenrod3"))+
  theme_bw()+
  theme(legend.position="none", axis.title.x=element_blank(),
        axis.text = element_text(size=18), 
        axis.title=element_text(size=20), plot.title = element_text(size=20))
#Plot stream leakage
leak = ggplot(data = leak_wys, aes(x = mo, y= -value/10^6, group = wy))+ #negative to make leakage positive for plotting
  labs(x = "Month in water year", y = expression(paste("Volume (km"^"3","/month)")),
       title="Stream Leakage")+
  geom_line(aes(colour = wy, size=2))+
  geom_hline(yintercept=0, linetype="solid", color="black")+
  geom_text(label="Net recharge to aquifer",x=9, y=10, size=6)+
  geom_text(label="Net discharge to stream",x=9, y=-10, size=6)+
  scale_colour_manual(values = c("2014"="orangered", "2017"="deepskyblue3",
                                 "2010"="darkgoldenrod1", "2015"="darkgoldenrod3"))+
  theme_bw()+
  theme(legend.position="none", axis.title.x=element_blank(),
        axis.text = element_text(size=18), 
        axis.title=element_text(size=20), plot.title = element_text(size=20))
#Plot water in aquifer storage
stor = ggplot(data = stor_wys, aes(x = mo, y= value/10^6, group = wy))+ 
  labs(x = "Month in water year", y = expression(paste("Volume (km"^"3","/month)")),
       title="Storage")+
  geom_line(aes(colour = wy, size=2))+
  geom_hline(yintercept=0, linetype="solid", color="black")+
  geom_text(label="Declining water levels",x=6, y=11, size = 6)+
  geom_text(label="Rising water levels",x=7.5, y=-13, size=6)+
  scale_colour_manual(values = c("2014"="orangered", "2017"="deepskyblue3",
                                 "2010"="darkgoldenrod1", "2015"="darkgoldenrod3"))+
  theme_bw()+
  theme(legend.position="none", axis.text = element_text(size=18), 
        axis.title=element_text(size=20), plot.title = element_text(size=20))
# #make legend for water year types for powerpoint
# png(file.path(figure_out_dir, "wy_type_legend.png"), height = 5, width = 7,
#     units = "in", res = 300)
# plot(1,0)
# legend(x="center",lwd=rep(5,4), cex=2, title="Water year (type)",
#        col = c("orangered", "deepskyblue3", "darkgoldenrod1", "darkgoldenrod3"),
#        legend = c("2014 (dry)", "2017 (wet)", "2010 (average; spread)", "2015 (average; conc.)"))
# dev.off()

# agu_wys = c(2014, 2017, 2010, 2015)
png(file.path(figure_out_dir,"components.png"), height = 20, width = 7, 
    units = "in", res = 300)
grid.newpage()
pushViewport(viewport(layout = grid.layout(4,1)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(pump, vp = vplayout(1,1))
print(rch, vp = vplayout(2,1))
print(stor, vp=vplayout(3,1))
print(leak, vp = vplayout(4,1))
graphics.off()



# 2. Water budget barcharts - full model period ---------------------------

# TO DO: EDIT THIS SO YOU'RE NOT IMPORTING THE SAME DATA TWICE, YOU MORON

# # Setup
# LST_Name = 'SVIHM.lst'
# RCH_Name = 'SVIHM.rch'
# 
# WB_Components_MODFLOW = c('STORAGE', 'CONSTANT_HEAD', 'WELLS', 'RECHARGE', 'ET_SEGMENTS','STREAM_LEAKAGE', 'DRAINS')
# nstress = 336
# end_wy = 2018
# StartingMonths = seq(as.Date("1990/10/1"), by = "month", length.out = nstress)
# fig_format = 'png'               #output format for figures (pdf, png, or jpg)
# standardize_y_annual_taf_plot = TRUE # 
# Print_SWBM_by_landuse = FALSE     # Print 28 year average, dry year (2001), average year (2015), and wet year (2006) SWBM by landuse
# 
# 
# # 2a) Import budgets ------------------------------------------------------
# 
# 
# # 2ai) Import SWBM Budget 
# SWBM_Terms = c('Precipitation', 'SW Irrigation', 'GW Irrigation', 'ET', 'Recharge', 'Storage')
# SWBM_Monthly_m3 = read.table(file.path(swbm_dir,'monthly_water_budget.dat'), header = T)
# names(SWBM_Monthly_m3) = c('Month',SWBM_Terms)
# 
# #NOTE: to print this after an update, edit the SWBM_Landuse.R file to change the ending water year and the number of years (ctrl-F for 2018 and 28)
# if (Print_SWBM_by_landuse==T){
#   source(file.path(budget_dir,'SWBM_Landuse.R'))
#   budget_terms = c('pET', 'aET', 'irrigation', 'surfacewater', 'groundwater', 'recharge', 'deficiency')
#   header_names = c('pET', 'aET', 'Irrigation', 'SW_Irrigation', 'GW_Irrigation', 'GW_Recharge', 'Deficiency')
#   landuse_cats = c('Alfalfa','Grain','Pasture','ET/NoIrr','NoET/NoIrr')
#   SWBM_Landuse(in_dir = in_swbm_dir, budget_terms, landuse_cats, header_names, end_wy)
# }
# 
# # 2aii) Import MODFLOW Budget
# 
# source(file.path(budget_dir, 'MODFLOW_Budget.R'))
# MODFLOW_Monthly_m3 = MODFLOW_Budget(mf_dir, LST_Name, WB_Components_MODFLOW, end_wy=end_wy)
# if (PRINT_BUDGET==TRUE){
#   write.table(MODFLOW_Monthly_m3, file = file.path(budget_dir,'MODFLOW_Water_Budget.dat'), row.names = F, quote = F)
# }
# 
# 
# # 2aiii) Import STREAMFLOW Budget 
# 
# source(file.path(budget_dir, 'SVIHM_SFR_inputs.R'))
# 
# Streamflow_Monthly_m3 = SVIHM_SFR_inputs(file.path(mf_dir,'SVIHM.sfr'), nstress)
# Streamflow_Monthly_m3$Stream_Leakage_m3 = -MODFLOW_Monthly_m3$STREAM_LEAKAGE_net_m3 #made negative to convert from MODFLOW to SFR budget
# FJ_Outflow = read.table(file.path(mf_dir,'Streamflow_FJ_SVIHM.dat'), skip = 2)[,3]
# FJ_Outflow = data.frame(Month = format(seq(as.Date('1990-10-01'), as.Date(paste0(end_wy,'-09-30')), by = 'day'),format = '%b-%Y'),
#                         FJ_Flow_m3 = FJ_Outflow)
# FJ_Outflow = aggregate(.~Month, FJ_Outflow, FUN = sum)
# FJ_Outflow$Month = as.Date(paste0('01-',FJ_Outflow$Month), '%d-%b-%Y')
# FJ_Outflow = FJ_Outflow[order(FJ_Outflow$Month),]
# Streamflow_Monthly_m3$Outflow_m3 = -FJ_Outflow$FJ_Flow_m3
# Streamflow_Monthly_m3$Storage = -rowSums(Streamflow_Monthly_m3[,-1])
# 
# Streamflow_Monthly_m3$WY = rep(seq(1991,end_wy), each = 12)
# Streamflow_Monthly_m3_melt = melt(Streamflow_Monthly_m3, id.vars = c('Date', 'WY'))
# 
# Streamflow_Monthly_m3_melt$Month = format(Streamflow_Monthly_m3_melt$Date,format = '%b')
# Streamflow_Monthly_m3_melt$Month = factor(Streamflow_Monthly_m3_melt$Month, levels = c(month.abb[10:12],month.abb[1:9]))
# Streamflow_Monthly_m3_melt = Streamflow_Monthly_m3_melt[order(Streamflow_Monthly_m3_melt$Month),]


# 2b) Create monthly and annual water budgets ----------------------------------------------------------

#SFR monthly was created during the read-file process above.

# #SWBM monthly
# swbm_monthly$Month = format(seq(as.Date("1990/10/1"), by = "month", length.out = nstress),'%b-%Y')
# swbm_monthly$WY = rep(seq(1991,end_wy),each = 12)

# #MODFLOW monthly
# month_abbv = format(seq(as.Date('1990/10/1'), as.Date('1991/9/30'), by = 'month'),'%b')
# MODFLOW_Monthly_m3$Month = strtrim(MODFLOW_Monthly_m3$Month,3)
# for (i in 1:12){
#   eval(parse(text = paste0("MODFLOW_",month_abbv[i],"_m3 = subset(MODFLOW_Monthly_m3, select = paste0(WB_Components_MODFLOW,'_net_m3'), Month == '",month_abbv[i],"')")))
#   eval(parse(text = paste0('MODFLOW_',month_abbv[i],'_m3$WY = seq(1991,end_wy)')))
# }


#SFR Annual
Streamflow_Annual_m3 = aggregate(.~WY, Streamflow_Monthly_m3[,-1], FUN = sum)
Streamflow_Annual_m3_melt = melt(Streamflow_Annual_m3,id.vars = 'WY')

#SWBM Annual
SWBM_Annual_m3 = aggregate(.~WY,SWBM_Monthly_m3[,!names(SWBM_Monthly_m3)%in%'Month'], FUN = sum)

#MODFLOW Annual
MODFLOW_Annual_m3 = subset(MODFLOW_Monthly_m3, select = paste0(WB_Components_MODFLOW,'_net_m3'))
MODFLOW_Annual_m3$Water_Year = rep(seq(1991,end_wy),each = 12)
MODFLOW_Annual_m3 = aggregate(.~Water_Year, MODFLOW_Annual_m3, FUN = sum)



# 2c) Plot subsystems -----------------------------------------------------


# Original colors from Gus' plots
# SWBM_flux_labels = c('Precipitation','ET','SW Irrigation', 'Recharge','GW Irrigation','Storage')
# MODFLOW_flux_labels = c('Recharge','ET','Storage','Drains','Stream Leakage','Wells', 'Canal Seepage/MFR')
# Streamflow_flux_labels = c('Inflow', 'Overland Flow', 'Farmers Ditch', 'SVID Ditch', 'Stream leakage', 'Outflow', 'Storage')
# 
# SWBM_colors = c('lightblue1', 'red', 'darkcyan', 'mediumblue','darkgreen', 'goldenrod' )
# MODFLOW_colors = c('mediumblue', 'red', 'goldenrod', 'mediumorchid2','dodgerblue1', 'darkgreen', 'salmon' )
# Streamflow_colors = c('turquoise2', 'sienna3', 'green2', 'green4', 'dodgerblue1', 'lightgoldenrod1', 'goldenrod')  

SWBM_flux_labels = c('Precipitation','ET','SW Irrigation', 'Recharge','GW Irrigation','Storage')
MODFLOW_flux_labels = c('Recharge','ET','Storage','Drains','Stream Leakage','Wells', 'Canal Seepage/MFR')
Streamflow_flux_labels = c('Inflow', 'Overland Flow', 'Farmers Ditch', 'SVID Ditch', 'Stream leakage', 'Outflow', 'Storage')

SWBM_colors = c('deepskyblue2', 'orange2', 'blue', 'green4','midnightblue', 'mistyrose' )
MODFLOW_colors = c('green4', 'orange2', 'mistyrose', 'mediumorchid2','royalblue', 'midnightblue', 'brown' )
Streamflow_colors = c('turquoise2', 'sienna3', 'green2', 'green4', 'royalblue', 'lightgoldenrod1', 'mistyrose')  


#2ci) Plot format parameters

#for TAF plots ONLY: y-axis limits, axis ticks, and water year type locations

if(standardize_y_annual_taf_plot == TRUE){
  sfr_ylim = c(-800,800);   sfr_breaks = seq(-800,800,200)
  swbm_ylim=sfr_ylim; mf_ylim=sfr_ylim
  swbm_breaks=sfr_breaks; mf_breaks=sfr_breaks
  wet_dry_y = -700
  
} else {  
  sfr_ylim = c(-800,800)
  sfr_breaks = seq(-800,800,200)
  swbm_ylim=c(-300,300)
  swbm_breaks = seq(-300,300, 100)
  mf_ylim=c(-150,150)
  mf_breaks = seq(-150,150,50)
  wet_dry_y = -142}
#currently using the IQR method. Able to switch to the StDev method easily
wat_year_type$wy_midpoint = as.Date(paste0(wat_year_type$wy,"-04-01") )
wet_years = wat_year_type[wat_year_type$iqr_wd == "wet" & wat_year_type$wy >= 1991,]
dry_years = wat_year_type[wat_year_type$iqr_wd == "dry" & wat_year_type$wy >= 1991,]

# 2cii) Soil Zone Annual Budget Plot

SWBM_Annual_m3_melt = melt(SWBM_Annual_m3, id.vars = 'WY')
SWBM_Annual_m3_melt$variable = factor(SWBM_Annual_m3_melt$variable, levels = SWBM_flux_labels)
SWBM_Annual_m3_melt = SWBM_Annual_m3_melt[order(SWBM_Annual_m3_melt$variable),]

SWBM_Annual_TAF_Plot = ggplot(SWBM_Annual_m3_melt, aes(x = WY, y = value*0.000810714/1000)) + #convert to TAF
  geom_bar(aes(fill = variable), position = "stack", stat = 'identity', color = 'black', width = 0.95, size = 0.1) +
  scale_x_continuous(limits = c(1990.4,end_wy+0.6), breaks = seq(1991,end_wy,by = 2),expand = c(0,0))  +
  scale_y_continuous(limits = swbm_ylim, breaks = swbm_breaks, expand = c(0,0)) +
  xlab('') +
  ylab('Volume (TAF)') +
  ggtitle('Soil Zone Annual Budget') +
  scale_fill_manual(values = SWBM_colors)+
  theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(color = 'black', fill = NA),
        plot.background = element_rect(color = NA, fill = NA),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), 
        axis.text = element_text(size = 8),
        plot.title = element_text(hjust = 0.5),
        legend.position = c(0.27, 0.94), 
        legend.key = element_rect(fill = NA, color = NA),
        legend.background = element_rect(fill = NA, color = NA),
        legend.direction = 'horizontal',
        legend.text = element_text(size = 6, margin = margin(r=1,l=1, unit = 'pt')),
        legend.key.height = unit(10,'pt'))

# 2ciii) Streamflow Annual Budget Plot 


Streamflow_Annual_TAF_Plot =  ggplot(Streamflow_Annual_m3_melt, aes(x = WY, y = value*0.000000810714)) + #  conversion from m3 to TAF
  geom_bar(aes(fill = variable), position = "stack", stat = 'identity', color = 'black', width = 0.95, size = 0.1) +
  scale_fill_manual(values = Streamflow_colors, labels = Streamflow_flux_labels) +
  scale_y_continuous(limits = sfr_ylim, breaks = sfr_breaks, expand = c(0,0) ) +
  scale_x_continuous(limits = c(1990.4,end_wy+.6), breaks = seq(1991,end_wy,by=2), expand = c(0,0)) +
  xlab('') +
  ylab('Volume (TAF)') +
  ggtitle('Surface Water Annual Budget') +
  theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(color = 'black', fill = NA),
        plot.background = element_rect(color = NA, fill = NA),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), 
        axis.text = element_text(size = 8),
        plot.title = element_text(hjust = 0.5),
        legend.position = c(0.38, 0.92), 
        legend.key = element_rect(fill = NA, color = NA),
        legend.background = element_rect(fill = NA, color = NA),
        legend.direction = 'horizontal',
        legend.text = element_text(size = 6, margin = margin(r=1,l=1, unit = 'pt')),
        legend.key.height = unit(10,'pt'))

# 2civ). Aquifer Annual Budget Plot 

MODFLOW_Annual_m3_Plotting = data.frame(Water_Year = rep(seq(1991,end_wy),each = 12),
                                        Recharge = MODFLOW_Monthly_m3$RECHARGE_net_m3,
                                        ET = MODFLOW_Monthly_m3$ET_SEGMENTS_net_m3,
                                        Storage = MODFLOW_Monthly_m3$STORAGE_net_m3,
                                        Drains = MODFLOW_Monthly_m3$DRAINS_net_m3,
                                        `Stream Leakage` = MODFLOW_Monthly_m3$STREAM_LEAKAGE_net_m3,
                                        Wells = -MODFLOW_Monthly_m3$WELLS_out_m3,              #negative sign since flux is out
                                        `Canal Seepage/MFR` = MODFLOW_Monthly_m3$WELLS_in_m3)
names(MODFLOW_Annual_m3_Plotting) = c('Water Year', MODFLOW_flux_labels)
MODFLOW_Annual_m3_Plotting = aggregate(.~`Water Year`,MODFLOW_Annual_m3_Plotting, FUN = sum)
MODFLOW_Annual_m3_Plotting_melt = melt(MODFLOW_Annual_m3_Plotting, id.vars = 'Water Year')
MODFLOW_Annual_m3_Plotting_melt$variable = factor(MODFLOW_Annual_m3_Plotting_melt$variable, levels = c('Recharge','ET','Storage','Drains','Stream Leakage','Wells', 'Canal Seepage/MFR'))
MODFLOW_Annual_m3_Plotting_melt = MODFLOW_Annual_m3_Plotting_melt[order(MODFLOW_Annual_m3_Plotting_melt$variable),]

MODFLOW_Annual_Budget_TAF_Plot = ggplot(MODFLOW_Annual_m3_Plotting_melt, aes(x = `Water Year`, y = value*0.000810714/1000)) +
  geom_bar(aes(fill = variable), position = "stack", stat = 'identity', color = 'black', width = 0.95, size = 0.1) +
  scale_x_continuous(limits = c(1990.4,end_wy+.6), breaks = seq(1991,end_wy,by = 2),expand = c(0,0))  +
  scale_y_continuous(limits = mf_ylim, breaks = mf_breaks, expand = c(0,0)) +
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
        legend.position = c(0.38, 0.93), 
        legend.key = element_rect(fill = NA, color = NA),
        legend.background = element_rect(fill = NA, color = NA),
        legend.direction = 'horizontal',
        legend.text = element_text(size = 6, margin = margin(r=1,l=1, unit = 'pt')),
        legend.key.height = unit(10,'pt'))


# 2d) Combined Annual Water Budget Plots TAF ------------------------------------------------


fig_name = 'Water_Budget_Annual_Combined_TAF'
if (fig_format == 'jpg'){
  jpeg(file.path(figure_out_dir,paste0(fig_name,'.jpg')), width = 7, height = 9, units = 'in',  res = 600)
} else if (fig_format == 'png'){
  png(file.path(figure_out_dir,paste0(fig_name,'.png')), width = 7, height = 9, units = 'in',  res = 600)
} else if (fig_format == 'pdf'){
  pdf(file.path(figure_out_dir,paste0(fig_name,'.pdf')), width = 7, height = 9)
}
grid.newpage()
pushViewport(viewport(layout = grid.layout(3,1)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(Streamflow_Annual_TAF_Plot +
        theme(axis.text.x = element_blank(),
              plot.title = element_blank(),
              axis.title.y = element_text(size = 8),
              plot.margin = margin(t=5, b=0,l=8, r=3),
              legend.key.height = unit(3,'pt'),
              legend.key.width = unit(6, 'pt'),
              legend.text = element_text(size = 6, margin = margin(r=1,l=1, unit = 'pt')),
              legend.position = c(0.24,0.94)) +
        annotate('text', x = 1996, y = -700, label = 'Annual Surface Water Budget', size = 4.5),
      vp = vplayout(1,1))
print(SWBM_Annual_TAF_Plot + 
        theme(axis.text.x = element_blank(),
              axis.title.y = element_text(size = 8),
              plot.title = element_blank(),
              plot.margin = margin(t=5, b=-10,l=8, r=3),
              legend.key.height = unit(3,'pt'),
              legend.key.width = unit(6, 'pt'),
              legend.text = element_text(size = 6, margin = margin(r=1,l=1, unit = 'pt')),
              legend.position = c(0.19,0.94)) +
        annotate('text', x = 1995, y = -700, label = 'Annual Soil Zone Budget', size = 4.5),
      vp = vplayout(2,1))
print(MODFLOW_Annual_Budget_TAF_Plot +
        ##Note: these rectangles were used to indicate wet and dry years. They were specific to the y-axes in an older figure.
        # geom_rect(aes(xmin = 1990.5, xmax = 1992.5, ymin = -149 , ymax = -135), fill = '#ff8282', color = 'black', size = 0.2) +
        # geom_rect(aes(xmin = 1993.5, xmax = 1994.5, ymin = -149 , ymax = -135), fill = '#ff8282', color = 'black', size = 0.2) +
        # geom_rect(aes(xmin = 1994.5, xmax = 1999.5, ymin = -149 , ymax = -135), fill = 'skyblue', color = 'black', size = 0.2) +
        # geom_rect(aes(xmin = 2000.5, xmax = 2002.5, ymin = -149 , ymax = -135), fill = '#ff8282', color = 'black', size = 0.2) +
        # geom_rect(aes(xmin = 2005.5, xmax = 2006.5, ymin = -149 , ymax = -135), fill = 'skyblue', color = 'black', size = 0.2) +
        # geom_rect(aes(xmin = 2006.5, xmax = 2009.5, ymin = -149 , ymax = -135), fill = '#ff8282', color = 'black', size = 0.2) +
        # geom_rect(aes(xmin = 2010.5, xmax = 2011.5, ymin = -149 , ymax = -135), fill = 'skyblue', color = 'black', size = 0.2) +
        geom_point(data = wet_years, aes(x = wy, y=wet_dry_y), color = 'paleturquoise', shape = 15, size = 5)+
        geom_point(data = dry_years, aes(x = wy, y=wet_dry_y), color = '#ff8282', shape = 15, size = 5)+
        theme(plot.title = element_blank(),
              axis.title.y = element_text(size = 8),
              plot.margin = margin(t=-5, b=-10,l=8, r=3),
              legend.key.height = unit(3,'pt'),
              legend.key.width = unit(6, 'pt'),
              legend.text = element_text(size = 6, margin = margin(r=1,l=1, unit = 'pt')),
              legend.position = c(0.25,0.94)) +
        annotate('text', x = 2014, y = 700, label = 'Annual Aquifer Budget', size = 4.5) +
        annotate('text', x = 1991.5, y = wet_dry_y+150, label = 'Dry Year', size = 2, angle = 45) +
        annotate('text', x = 1995.5, y = wet_dry_y+150, label = 'Wet Year', size = 2, angle = 45),
      # annotate('text', x = 1997, y = wet_dry_y, label = 'Wet', size = 1.4) +
      # annotate('text', x = 2001.5, y = wet_dry_y, label = 'Dry', size = 1.4) +
      # annotate('text', x = 2006, y = wet_dry_y, label = 'Wet', size = 1.4) +
      # annotate('text', x = 2008, y = wet_dry_y, label = 'Dry', size = 1.4) +
      # annotate('text', x = 2011, y = wet_dry_y, label = 'Wet', size = 1.4),
      vp = vplayout(3,1))
graphics.off()



# 3. Change in aquifer storage plots -------------------------------------

#Open figure file
pdf_dir = budget_dir
figure_out_dir = pdf_dir # from swbm scenario plotting script

png(file.path(figure_out_dir, 'aq_storage.png'),width = 3.2, height = 2.5, units = "in", res = 200)

#only show last 11 years
plot_start_date = as.Date('2008-10-01')
aq_monthly_trunc = aq_monthly[aq_monthly$Month >= plot_start_date,]

#hacking something together for Geeta

stor_monthly_alt = aq_monthly_melt_alt[aq_monthly_melt_alt$variable == "Storage",]
stor_monthly_alt$value_cumsum = cumsum(stor_monthly_alt$value)
stor_monthly_alt$variable = "scen_95_07"
stor_monthly_hist = aq_monthly_melt_hist[aq_monthly_melt_hist$variable == "Storage",]
stor_monthly_hist$value_cumsum = cumsum(stor_monthly_hist$value)
stor_monthly_hist$variable = "hist"
stor = rbind(stor_monthly_hist, stor_monthly_alt)
stor_trunc = stor[stor$Month >= plot_start_date,]
colnames(stor_trunc) = c("Month", "Scenario", "value","value_cumsum")

#Plot hacked together for geeta
(Monthly_Storage_Plot_MODFLOW_TAF = ggplot(data = stor_trunc,
                                           aes(x = as.Date(Month),
                                               y = -value_cumsum/10^6,
                                               group = Scenario)) +
    #Add annotations to reflect the appropriate water year
    geom_rect(xmin=as.Date('2009-10-01'), xmax=as.Date('2010-09-30'), ymin = -60, ymax=-50, fill = "darkgoldenrod1", alpha = 0.7)+
    geom_rect(xmin=as.Date('2013-10-01'), xmax=as.Date('2014-09-30'), ymin = -60, ymax=-50, fill = "orangered", alpha = 0.7)+
    geom_rect(xmin=as.Date('2014-10-01'), xmax=as.Date('2015-09-30'), ymin = -60, ymax=-50, fill = "darkgoldenrod3", alpha = 0.4)+
    geom_rect(xmin=as.Date('2016-10-01'), xmax=as.Date('2017-09-30'), ymin = -60, ymax=-50, fill = "deepskyblue3", alpha = 0.4)+
    annotate(geom = "text", size = 2, x=as.Date('2010-04-01'), y=-55, label = "2010")+
    annotate(geom = "text", size = 2, x=as.Date('2014-04-01'), y=-55, label = "2014")+
    annotate(geom = "text", size = 2, x=as.Date('2015-04-01'), y=-55, label = "2015")+
    annotate(geom = "text", size = 2, x=as.Date('2017-04-01'), y=-55, label = "2017")+
    #Add gridlines, storage line and point, and edit axes
    #geom_vline(mapping=NULL, linetype = 2, size = 0.5, colour="gray", xintercept = seq(as.Date("1990-10-01"), as.Date("2018-10-01"), by = "5 year"))+
    #geom_hline(mapping=NULL, linetype = 2, size = 0.5, colour="gray", yintercept = seq(-50,50, by = 25))+
    geom_hline(yintercept = 0, size = 0.25) +
    geom_line(size = 0.5, aes( color=Scenario))  +
    geom_point(size = 0.75, aes(color = Scenario)) +
    scale_x_date(limits = c(as.Date('Oct-01-2008', format = '%b-%m-%y'),
                            as.Date(paste0('Oct-01-',end_wy), format = '%b-%m-%y')),
                 breaks = seq(plot_start_date, by = "2 years", length.out = nstress/12+1), expand = c(0,0),
                 date_labels = ('%b-%y'))  +
    scale_y_continuous(limits = c(-60,60), breaks = seq(-50,50,by = 25), expand = c(0,0)) +
    # Edit labels and themes
    scale_color_discrete(name="Scenario",
                        breaks=c("hist","scen_95_07"),
                        labels=c("Historical", "Altered")) +
    ylab(ylab(bquote('Relative Aquifer Storage (million cubic m)'))) +
    ggtitle('a) Historical Monthly Aquifer Storage') +
    theme(#panel.background = element_blank(),
      panel.border = element_rect(fill=NA, color = 'black'),
      axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
      axis.text.y = element_text(size = 8),
      axis.ticks = element_line(size = 0.4),
      plot.title = element_text(size = 9),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 8),
      legend.position = c(0.45, 0.82),
      legend.text=element_text(size=7),
      legend.title=element_text(size=7.5),
      legend.key.size = unit(0.3, "cm")
    )
)
graphics.off()



#Plot
(Monthly_Storage_Plot_MODFLOW_TAF = ggplot(data = aq_monthly_trunc,
                                           aes(x = seq(plot_start_date,as.Date(paste0(end_wy,'-09-30')),by = 'month'), 
                                               y = -cumsum(Storage)/10^6 )) +
    #Add annotations to reflect the appropriate water year
    geom_rect(xmin=as.Date('2009-10-01'), xmax=as.Date('2010-09-30'), ymin = -60, ymax=-50, fill = "darkgoldenrod1", alpha = 0.7)+
    geom_rect(xmin=as.Date('2013-10-01'), xmax=as.Date('2014-09-30'), ymin = -60, ymax=-50, fill = "orangered", alpha = 0.7)+
    geom_rect(xmin=as.Date('2014-10-01'), xmax=as.Date('2015-09-30'), ymin = -60, ymax=-50, fill = "darkgoldenrod3", alpha = 0.4)+
    geom_rect(xmin=as.Date('2016-10-01'), xmax=as.Date('2017-09-30'), ymin = -60, ymax=-50, fill = "deepskyblue3", alpha = 0.4)+
    annotate(geom = "text", size = 2, x=as.Date('2010-04-01'), y=-55, label = "2010")+
    annotate(geom = "text", size = 2, x=as.Date('2014-04-01'), y=-55, label = "2014")+
    annotate(geom = "text", size = 2, x=as.Date('2015-04-01'), y=-55, label = "2015")+
    annotate(geom = "text", size = 2, x=as.Date('2017-04-01'), y=-55, label = "2017")+
    #Add gridlines, storage line and point, and edit axes
    geom_vline(mapping=NULL, linetype = 2, size = 0.5, colour="gray", xintercept = seq(as.Date("1990-10-01"), as.Date("2018-10-01"), by = "5 year"))+
    geom_hline(mapping=NULL, linetype = 2, size = 0.5, colour="gray", yintercept = seq(-50,50, by = 25))+
    geom_hline(yintercept = 0, size = 0.25) +
    geom_line(size = 0.5)  +
    geom_point(size = 0.75) +
    scale_x_date(limits = c(as.Date('Oct-01-2008', format = '%b-%m-%y'),
                            as.Date(paste0('Oct-01-',end_wy), format = '%b-%m-%y')),
                 breaks = seq(plot_start_date, by = "2 years", length.out = nstress/12+1), expand = c(0,0),
                 date_labels = ('%b-%y'))  +
    scale_y_continuous(limits = c(-60,60), breaks = seq(-50,50,by = 25), expand = c(0,0)) +
    # Edit labels and themes
    ylab(ylab(bquote('Relative Aquifer Storage (cubic km)'))) +
    ggtitle('a) Historical Monthly Aquifer Storage') +
    theme(#panel.background = element_blank(),
          panel.border = element_rect(fill=NA, color = 'black'),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
          axis.text.y = element_text(size = 8),
          axis.ticks = element_line(size = 0.4),
          plot.title = element_text(size = 9),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 8) 
    )
)
graphics.off()

#3.1 change in storage tables
stor_tab = aq_monthly[,c("Month", "Storage")]
stor_tab$Storage_Cumulative = cumsum(stor_tab$Storage)
write.csv(stor_tab, "change in storage.csv")


# 4. Monthly, seasonal SWBM plot, full model period -----------------------


make_legend_symbol_table = function(){
  
  label = c("Precipitation","Surface water irrigation", "Groundwater irrigation", "Evapotranspiration","Recharge","Change in storage")
  color = c("deepskyblue", "blue", "gray36", "orange2", "green4", "darkgray")
  components = data.frame(label,color)
  components$color = as.character(components$color)
  components$abbrev = c("Precip", "SW_Irr", "GW_Irr", "ET", "Recharge", "Storage")
  
  return(components)
}

plot_water_budget_overview = function(mwb, scenario_name, output_type = "pdf"){
  #Label budget components
  P = mwb$Precip
  SW = mwb$SW_Irr
  GW = mwb$GW_Irr
  ET = mwb$ET
  R = mwb$Recharge
  S = mwb$Storage
  
  #Plot parameters
  vol_lim = c(min(mwb[2:7]), max(mwb[2:7]))
  x_lim = c(240, dim(mwb)[1])# x_lim = c(1, dim(mwb)[1])
  #Symbols and legend labels
  components = make_legend_symbol_table()
  
  #OUtput file setup
  setwd(figure_out_dir)
  if(output_type == "pdf"){
    pdfname = paste0(scenario_name, "_overview.pdf")
    pdf(pdfname, 7.5,9)
    par(mfrow=c(2,1))
  } else if(output_type =="png"){
    pngname = paste0(scenario_name, "plot1_overview.png")
    png(pngname, width = 8, height = 5.5, units = "in", res = 200) 
  }
  
  
  #Initialize Panel 1
  plot(x = NA, y = NA, xlim = x_lim,  ylim = vol_lim,
       axes = FALSE, xlab = "Year", ylab = "", #Suppress y-axis label text
       main = paste(scenario_name, "monthly water budget"),
       sub = "Negative values indicate water leaving modeled soil volume", cex = 0.8)
  
  #Manual stylized grid lines (plotted first to not obscure data)
  abline(v = seq(from = 4, to = 336, by=12),
         lty = 3, lwd = 0.5, col = "lightgray")
  abline(h = pretty(vol_lim), v = seq(from = 52, to = 336, by=12),#60), # Jan of 95, 00, 05, 10
         lty = 1, col = "darkgray")
  abline(h = 0, col = "black")
  #Manual y-label and axis labels, and a box (plot border)
  mtext(side = 2, expression(paste("Water volume (cubic meters x 10"^"7", ")")), 
        las = 0, line = 2) #manual y-label
  axis(1, at = seq(from = 4, to = 336, by=12), labels = 1991:2018)
  axis(2, at = pretty(vol_lim), pretty(vol_lim)/10^7)
  box()
  
  #Plot water budget components over time. Ordered to optimize visibility
  lines(R, col = components$color[5], lwd = 2.5) 
  lines(P, col = components$color[1], lwd = 2.5)
  lines(GW, col = components$color[3], lwd = 2.5)
  lines(SW, col = components$color[2], lwd = 2.5)
  lines(ET, col = components$color[4], lwd = 2.5)
  
  #New png file if making pngs
  if(output_type =="png"){
    dev.off()
    pngname = paste0(scenario_name, "plot2_overview.png")
    png(pngname, width = 9, height = 5, units = "in", res = 200) #plot for a poster
  }
  
  #Initialize Panel 2
  plot(x = NA, y = NA, xlim = x_lim,  ylim = vol_lim,
       axes = FALSE, xlab = "Year", ylab = "", #Suppress y-axis text
       main = paste0(scenario_name, " monthly change in soil water storage"),
       sub = "Negative values indicate water leaving modeled soil volume", cex = 0.8)
  #Manual stylized grid lines (plotted first to not obscure data)
  abline(v = seq(from = 4, to = 336, by=12),
         lty = 3, lwd = 0.5, col = "lightgray")
  abline(h = pretty(vol_lim), v = seq(from = 52, to = 336, by=60), # Jan of 95, 00, 05, 10
         lty = 1, col = "darkgray")
  abline(h = 0, col = "black")
  #Manual y-label and axis labels
  mtext(side = 2, expression(paste("Change in storage (cubic meters x 10"^"7", ")")), 
        las = 0, line = 2) #manual y-label
  axis(1, at = seq(from = 4, to = 336, by=12), #Jan of years ending in 5 and 0
       labels = 1991:2018)
  axis(2, at = pretty(vol_lim), pretty(vol_lim)/10^7)
  box()
  
  #Plot change in storage over time
  lines(S, type = "l", col = components$color[6], lwd = 2.5)
  
  #Add legend to the bottom panel
  legend(x = "topleft", lwd = 2, ncol = 2, bg= "white",
         col = components[,2], legend = components[,1])
  
  dev.off()
}

#Read in SWBM scenario outputs: monthly water budget 

monthly_water_budget_hist = read.table(file.path(swbm_dir,"hist","monthly_water_budget.dat"), header = TRUE)
mwb_hist = monthly_water_budget_hist

#Plot historical model period as monthly line plot
# Exports to pdf in "figure_out_dir" directory specified above
plot_water_budget_overview(mwb = mwb_hist, scenario_name= "Historical", output_type = "pdf")


