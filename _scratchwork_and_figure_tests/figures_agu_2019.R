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


# 1. Set up directory names and global plot settings
# Project Directory
# gsp_dir <- here::here() #assigns gsp_dir when knitting

gsp_dir = "C:/Users/Claire/Documents/GitHub/SiskiyouGSP2022"
svihm_dir = "C:/Users/Claire/Documents/GitHub/SVIHM"
# #CK's computer: during figure development, when working in RStudio, check for here() path going to Documents folder. If so, correct it.
# gsp_dir_pieces = unlist(strsplit(gsp_dir, "/")); if(tail(gsp_dir_pieces, n=1) %in% c("Documents","SVIHM")){ gsp_dir = "~/GitHub/SiskiyouGSP2022"}
fig_dir = file.path(gsp_dir, "GSP_Figures")
dms_dir = file.path(gsp_dir, "Data_Management_System")
dms_archive_dir = "C:/Users/Claire/Box/SiskiyouGSP2022_DMS"

#Budget directories
swbm_dir = file.path(svihm_dir,"SWBM")
budget_dir = file.path(gsp_dir, "GSP_Analyses", "Scott_Water_Budget")
in_mf_dir =  "C:/Users/Claire/Documents/GitHub/SVIHM/MODFLOW/hist"   #directory where model ouput files are located

scott_conditions_dir = file.path(gsp_dir, "GSP_Analyses", "Scott_Groundwater_Conditions")

agu_figure_dir = "C:/Users/Claire/Documents/UCD/Presentations or Talks or Workshops or funding/2019.12.09 AGU/figures"

# 1.5. Load standardized colors and units for data layer processing during data load
source(file.path(fig_dir, "figure_color_settings.R"))

# 2. load spatial and tabular data
local_layers_path = file.path(fig_dir, "scott_figure_layers.RData")
#update_workspace = TRUE # If you want to update the saved layers (during figure development)
if(!exists('update_workspace')){update_workspace=FALSE}
source(file.path(fig_dir,"Scott_load_environment.R"))

# 3. Load standardized colors and units
source(file.path(fig_dir, "figure_color_settings.R"))
# Set tmap settings for all figures
tmap_options(max.raster = c(plot = 1e9, view = 1e9), unit = mapunits)
tmap_mode("plot")

# 4. set wd for image test files
setwd(file.path(dms_dir, "scratch_work"))

# 5. water budget terms
start_wy = 1991
end_wy = 2018
nstress = 336
SWBM_flux_labels = c('Precip','ET','SW_Irr', 'Recharge','GW_Irr','Storage')
SWBM_colors = c('deepskyblue2', 'orange2', 'blue', 'green4','midnightblue', 'darkgray' )
swbm_legend = data.frame("Component" = SWBM_flux_labels, "Color" = SWBM_colors)

MODFLOW_flux_labels = c('Recharge','ET','Storage','Drains','Stream_Leakage','Wells', 'Canal_Seepage_and_MFR')
MODFLOW_colors = c('green4', 'orange2', 'darkgray', 'mediumorchid2','royalblue', 'midnightblue', 'brown' )
aq_legend = data.frame("Component" = MODFLOW_flux_labels, "Color" = MODFLOW_colors)


Streamflow_flux_labels = c('Inflow', 'Overland Flow', 'Farmers Ditch', 'SVID Ditch', 'Stream leakage', 'Outflow', 'Storage')
Streamflow_colors = c('turquoise2', 'sienna3', 'green2', 'green4', 'royalblue', 'lightgoldenrod1', 'darkgray')  
sfr_legend = data.frame("Component" = Streamflow_flux_labels, "Color" = Streamflow_colors)


# Sim v Obs directories
ref_dir = file.path(svihm_dir, "SVIHM_Input_Files", "reference_data")
mf_results_dir = file.path(svihm_dir, "MODFLOW", "hist") # historical 1991-2018 wy 
postproc_dir = file.path(svihm_dir, "R_Files", "Post-Processing")


# Figure 1. water budget----------------------------------------------------------------

# Example of a water budget for a wet, dry, and normal year (two types of normal, 2010 and 2015)

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
  setwd(pdf_dir)
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


# __IMPORT BUDGETS ----------------------------------------------------------


# Import SWBM
monthly_water_budget_hist = read.table(file.path(swbm_dir,"hist","monthly_water_budget.dat"), header = TRUE)
swbm_monthly = monthly_water_budget_hist

#Process for plotting
start_month = as.Date(paste0(start_wy-1, "-10-01")); end_month = as.Date(paste0(end_wy, "-09-01"))
swbm_monthly$Month = seq(start_month, end_month, by="month")
swbm_monthly$Stress_Period = NULL
swbm_monthly_melt = melt(swbm_monthly, id.vars = "Month")
swbm_monthly_melt$variable = factor(swbm_monthly_melt$variable, levels = SWBM_flux_labels)
swbm_monthly_melt = swbm_monthly_melt[order(swbm_monthly_melt$variable),]

# Import aquifer (MODFLOW)

# #Modflow import setup
# source(file.path(budget_dir, 'MODFLOW_Budget.R'))
# LST_Name = 'SVIHM.lst'
# WB_Components_MODFLOW = c('STORAGE', 'CONSTANT_HEAD', 'WELLS', 'RECHARGE', 'ET_SEGMENTS','STREAM_LEAKAGE', 'DRAINS')
# #Specify out_dir
# aq_monthly_all = MODFLOW_Budget(in_mf_dir, LST_Name, WB_Components_MODFLOW, end_wy=end_wy)
# if (PRINT_BUDGET==TRUE){
#   write.table(aq_monthly_all, file = file.path(out_dir,'MODFLOW_Water_Budget.dat'), row.names = F, quote = F)
# }

# or, if you've run the modflow import setup before, you can read in:
aq_monthly_all = read.table(file.path(budget_dir, "Results", "MODFLOW_Water_Budget.dat"),
                        header = T)
aq_monthly_all$Month = as.Date(paste0("01-",aq_monthly_all$Month), format = "%d-%b-%Y")
#Process for plotting
aq_monthly = data.frame(Month = aq_monthly_all$Month,
                        Recharge = aq_monthly_all$RECHARGE_net_m3,
                        ET = aq_monthly_all$ET_SEGMENTS_net_m3,
                        Storage = aq_monthly_all$STORAGE_net_m3,
                        Drains = aq_monthly_all$DRAINS_net_m3,
                        `Stream Leakage` = aq_monthly_all$STREAM_LEAKAGE_net_m3,
                        Wells = -aq_monthly_all$WELLS_out_m3,              #negative sign since flux is out
                        `Canal Seepage/MFR` = aq_monthly_all$WELLS_in_m3)
names(aq_monthly) = c('Month', MODFLOW_flux_labels)
aq_monthly_melt = melt(aq_monthly, id.vars = 'Month')
aq_monthly_melt$variable = factor(aq_monthly_melt$variable, levels = MODFLOW_flux_labels)
aq_monthly_melt = aq_monthly_melt[order(aq_monthly_melt$variable),]

# 2c. Import STREAMFLOW Budget

source(file.path(budget_dir, 'SVIHM_SFR_inputs.R'))

#Import streamflow budget and add columns from modflow
Streamflow_Monthly_m3 = SVIHM_SFR_inputs(file.path(in_mf_dir,'SVIHM.sfr'), nstress)
Streamflow_Monthly_m3$Stream_Leakage_m3 = -aq_monthly_all$STREAM_LEAKAGE_net_m3 #made negative to convert from MODFLOW to SFR budget
FJ_Outflow = read.table(file.path(in_mf_dir,'Streamflow_FJ_SVIHM.dat'), skip = 2)[,3]
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


# __1a. PLOT BUDGETS ----------------------------------------------------------


#Plot Streamflow
sfr_wy10 = ggplot(data = Streamflow_Monthly_m3_melt, 
                aes(x = Date, y = value*0.000810714/1000), group = variable) +
  geom_bar(aes(fill = variable), stat="identity", position="stack") +
  scale_fill_manual(values=c(Inflows_m3="turquoise2", Drain_Inflow_m3= "sienna3",
                               Farmers_Div_m3="green2", SVID_Div_m3="green4",
                               Stream_Leakage_m3="royalblue", Outflow_m3="lightgoldenrod1",
                               Storage_m3="darkgray"))+
  labs(y="Volume (TAF)")+
  scale_x_date(date_breaks = "2 month", labels=date_format("%b-%Y"),
               limits = as.Date(c(paste0(wy-1,'-10-01'),paste0(wy,'-10-01'))))+
  scale_y_continuous(breaks = seq(-150,150, by=50),limits = c(-150, 150))+
  theme(legend.position = "none")

#Plot SWBM
swbm_wy10 = ggplot(data = swbm_monthly_melt, 
                 aes(x = Month, y = value*0.000810714/1000), fill = variable) +
  geom_bar(aes(fill=variable), position = "stack", stat = 'identity') +
  scale_fill_manual( values=c("Precip"="deepskyblue2", "ET"="orange2",
                               "SW_Irr"="blue", "Recharge"="green4","GW_Irr" = "midnightblue",
                               "Storage"="darkgray"))+
  labs(y="Volume (TAF)")+
  scale_x_date(date_breaks = "2 month", labels=date_format("%b-%Y"),
               limits = as.Date(c(paste0(wy-1,'-10-01'),paste0(wy,'-10-01'))))+
  scale_y_continuous(breaks = seq(-30,30, by=10),limits = c(-30, 30))+
  theme(legend.position = "none")

#Plot Aquifer
aq_wy10 = ggplot(data = aq_monthly_melt, 
               aes(x = Month, y = value*0.000810714/1000, fill = variable)) +
  geom_bar( position = "stack", stat = 'identity') +
  scale_fill_manual(values=c("Recharge"="green4", "ET"="orange2",
                               "Storage"="darkgray",  "Drains"="mediumorchid2",
                               "Stream_Leakage"="royalblue",
                               "Wells"="midnightblue", "Canal_Seepage_and_MFR"="brown"))+
  labs(y="Volume (TAF)")+
  scale_x_date(date_breaks = "2 month", labels=date_format("%b-%Y"),
               limits = as.Date(c(paste0(wy-1,'-10-01'),paste0(wy,'-10-01'))))+
  scale_y_continuous(breaks = seq(-30,30, by=10),limits = c(-30, 30))+
  theme(legend.position = "none")


  

#produce graphics
# agu_wys = c(2014, 2017, 2010, 2015)
png(file.path(agu_figure_dir,"test5.png"), height = 11, width = 10, 
    units = "in", res = 300)
grid.newpage()
pushViewport(viewport(layout = grid.layout(4,3)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
  print(sfr_wy14, vp = vplayout(1,1))
  print(swbm_wy14, vp = vplayout(1,2))
  print(aq_wy14, vp = vplayout(1,3))
  print(sfr_wy17, vp = vplayout(2,1))
  print(swbm_wy17, vp = vplayout(2,2))
  print(aq_wy17, vp = vplayout(2,3))
  print(sfr_wy10, vp = vplayout(3,1))
  print(swbm_wy10, vp = vplayout(3,2))
  print(aq_wy10, vp = vplayout(3,3))
  print(sfr_wy15, vp = vplayout(4,1))
  print(swbm_wy15, vp = vplayout(4,2))
  print(aq_wy15, vp = vplayout(4,3))
graphics.off()



# __1b. PLOT COMPONENTS -------------------------------------------------------

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

#Plot pumping
pump=ggplot(data = pump_wys, aes(x = mo, y= -value/10^6, group = wy))+ #negative to make pumping positive for plotting
  labs(x = "none", y = "Volume (km3/month)", title="Well Pumping")+
  geom_line(aes(colour = wy))+#, size=2))+
  scale_colour_manual(values = c("2014"="orangered", "2017"="deepskyblue3",
                                 "2010"="darkgoldenrod1", "2015"="darkgoldenrod3"))+
  theme_bw()+
  theme(legend.position="none", axis.title.x=element_blank())
#Plot recharge
rch = ggplot(data = rch_wys, aes(x = mo, y= value/10^6, group = wy))+ 
  labs(y = "Volume (km3/month)", title="Recharge")+
  geom_line(aes(colour = wy))+#, size=2))+
  scale_colour_manual(values = c("2014"="orangered", "2017"="deepskyblue3",
                                 "2010"="darkgoldenrod1", "2015"="darkgoldenrod3"))+
  theme_bw()+
  theme(legend.position="none", axis.title.x=element_blank())
#Plot stream leakage
leak = ggplot(data = leak_wys, aes(x = mo, y= -value/10^6, group = wy))+ #negative to make leakage positive for plotting
  labs(x = "Month in water year", y = "Volume (km3/month)", title="Stream Leakage")+
  geom_line(aes(colour = wy))+#, size=2))+
  geom_hline(yintercept=0, linetype="solid", color="black")+
  geom_text(label="Net recharge to aquifer",x=10, y=10)+
  geom_text(label="Net discharge to stream",x=10, y=-10)+
  scale_colour_manual(values = c("2014"="orangered", "2017"="deepskyblue3",
                                 "2010"="darkgoldenrod1", "2015"="darkgoldenrod3"))+
  theme_bw()+
  theme(legend.position="none")

# #make legend for water year types for powerpoint
# png(file.path(agu_figure_dir, "wy_type_legend.png"), height = 5, width = 7,
#     units = "in", res = 300)
# plot(1,0)
# legend(x="center",lwd=rep(5,4), cex=2, title="Water year (type)",
#        col = c("orangered", "deepskyblue3", "darkgoldenrod1", "darkgoldenrod3"),
#        legend = c("2014 (dry)", "2017 (wet)", "2010 (average; spread)", "2015 (average; conc.)"))
# dev.off()

# agu_wys = c(2014, 2017, 2010, 2015)
png(file.path(agu_figure_dir,"components.png"), height = 8, width = 5, 
    units = "in", res = 300)
grid.newpage()
pushViewport(viewport(layout = grid.layout(3,1)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(pump, vp = vplayout(1,1))
print(rch, vp = vplayout(2,1))
print(leak, vp = vplayout(3,1))
graphics.off()


# Figure 2. hydrographs ----------------------------------------------------------------

# Example of a river hydrograph and rainfall record in a wet, dry, and 2 types of normal year

#Water year months and functions
water_year_months <- c("October", "November", "December", "January", "February", "March", "April", "May", "June", "July", "August", "September")
water_year_months_abbrev <- c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep")
wtr_yr <- function(dates, start_month=10) {
  # Convert dates into POSIXlt
  dates.posix = as.POSIXlt(dates)
  # Year offset
  offset = ifelse(dates.posix$mon >= start_month - 1, 1, 0)
  # Water year
  adj.year = dates.posix$year + 1900 + offset
  # Return the water year
  adj.year
}


# __2b. annual precip figure ----------------------------------------------




fj_stream_and_precip= function(wy = 1991, legend =F){
  wy_start_date = as.Date(paste0(wy-1,"-10-01"))
  nextwy_start_date = as.Date(paste0(wy,"-10-01"))
  
  #set margins for 2 y axes and labels
  par(mar = c(5,5,2,5))
  
  #Make river hydrograph
  #subset stream data
  fjd_wy = fjd[fjd$Date >= wy_start_date & fjd$Date < nextwy_start_date,]
  #subset rainfall data
  rain_wy = rain[rain$Date >= wy_start_date & rain$Date < nextwy_start_date,]
  
  #Plot river hydrograph
  plot(fjd_wy$Date, fjd_wy$Flow, type="l", lwd = 2, col = "blue",
       log = "y", yaxt = "n", xaxt = "n", ylim = c(2, max_flow),
       main = paste("Water Year", wy), xlab = "Date in water year", 
       ylab = "Average Daily Flow (cfs)")
  axis(side = 2, at = 10^(0:4), las = 2, labels = c("1", "10", "100", "1000", "10,000"))
  axis(side = 2, tck = -.01, at = rep(1:10, 5) * rep(10^(0:4), each = 10), labels = NA)
  x_ticks = unique(floor_date(seq(wy_start_date, nextwy_start_date, by="day"), unit = "month"))
  x_ticks_labels = format(x_ticks, "%b %d")
  axis(side=1, tck = -.02, srt=45, at = x_ticks, labels = x_ticks_labels)
  # text(x = x_ticks, y = 0.5, labels = x_ticks_labels, srt = 45, pos = 1, xpd = TRUE, )
  abline(h = 10^(0:4), v = seq(wy_start_date, nextwy_start_date, by="month"),
         lty = 3, col = "gray")
  # abline(h = 40, col = "brown", lty = 2, lwd = 2) # add FS water right on plot
  
  #Add annotations to plot
  #a) indicate total precip and water year type (have to do this before adding new axis for some reason)
  total_rainfall = sum(rain_wy$PRCP_in)
  total_flow = fj_annual$vol_m3[fj_annual$wy == wy]
  summary_text_rain = paste("Total rainfall: ", round(total_rainfall, 1), "in")
  summary_text_flow = paste( "Total flow:", round(total_flow/10^6), "cubic km")
  text(x=nextwy_start_date - 130, y = 30000, cex = 0.8, pos = 4,
       labels = summary_text_rain)
  text(x=nextwy_start_date - 130, y = 20000, cex = 0.8, pos=4, labels = summary_text_flow)
  #b) add center of rainfall and note
  rainfall_x = as.numeric(rain_wy$Date)
  rain_center = as.Date(sum(rainfall_x*rain_wy$PRCP_in) / total_rainfall, origin = "1970-01-01")
  abline(v=rain_center, lwd = 2, lty = 2, col = "gray60")
  summary_text_center = paste("Rain center of mass:", format(as.Date(rain_center), "%b %d"))
  text(x=nextwy_start_date - 130, y = 13000, cex = 0.8, pos=4, labels = summary_text_center)
  #c) Calculate max 30-day rainfall density and note
  percent_30day = rollapply(rain_wy$PRCP_in, width = 30, FUN = "sum", align = "left")/total_rainfall * 100
  summary_text_density = paste0("Max rain density: ", round(max(percent_30day)),
                               "% / 30 days")
  text(x=nextwy_start_date - 130, y = 9000, cex = 0.8, pos=4, labels = summary_text_density)
  # d) add legend
  if(legend){
    legend(x = "topleft", lwd =c(2,NA, 2), lty = c(1, NA, 2), pch = c(NA, 15, NA),
           col = c("blue", "gray30", "gray60"), box.col = NA, 
           legend = c("Fort Jones Gauge Flow", "Fort Jones Precipitation", "Rain center of mass"))
  }
  #Add rainfall to river flow plot
  par(new = T)
  
  # Plot rainfall record
  barplot(height = rain_wy$PRCP_in, ylim = c(0, log10(max_flow)), #customized tweaking to make horizontal lines match up 
          xlab = NA, ylab = NA, yaxt="n", xaxt="n", border = NA, col="gray30")
  axis(side=4)
  mtext(side=4, line = 3, "Daily Precipitation (in)")
  
  
}


#Retrieve fort jones stream gage data
fj_num = "11519500"
fjd_all = readNWISdv(siteNumbers = fj_num, parameterCd="00060" )
fjd_all = renameNWISColumns(fjd_all)
#Only run analyses for post-1977 water year
fjd = fjd_all[fjd_all$Date >= as.Date("1976-10-01"),]
#add water year
fjd$wy = year(fjd$Date); fjd$wy[month(fjd$Date) > 9] = fjd$wy[month(fjd$Date) > 9]+1
#calculate max flow for plot
max_flow = max(fjd$Flow)

#aggregate to annual 
fjd$m3_day = fjd$Flow * 2446.58 #convert from cfs to cubic meters per day
fj_annual = aggregate(fjd$m3_day, by=list(fjd$wy), FUN=sum)
colnames(fj_annual) = c("wy", "vol_m3")

#Pull rain data from model run
rain_path = "C:/Users/Claire/Documents/GitHub/SVIHM/SVIHM_Input_Files/Scenario_Development/precip_regressed_2019.08.19.txt"
rain = read.table(rain_path, header = F)
colnames(rain) = c("PRCP_m", "Date")
rain$Date = as.Date(rain$Date, format = "%d/%m/%Y")
rain$PRCP_in = rain$PRCP_m * 39.3701
# rain_annual = aggregate(rain$PRCP_in, by=list(wtr_yr(rain$Date)), FUN="sum")

#make appendix
# pdf( file.path(agu_figure_dir, "FJ Stream Gage Water Year Hydrographs.pdf"), 8.5,11/2)
# for(wy in 1991:2018){
#   fj_stream_and_precip(wy, legend = T)
# }
# dev.off() #close pdf

#make AGU figures for specific water years
agu_wys = c(2014, 2017, 2010, 2015)
# png(file.path(agu_figure_dir, "FJ Stream and Precip_vert.png"), 
    # width = 5,height = 11, 
    # units = "in",res = 300)
par(mfrow = c(4,1))
for(wy in agu_wys){
  png(file.path(agu_figure_dir, paste(wy,"FJ Stream and Precip.png")), width = 8.5,height = 11/2, units = "in",res = 300)
  fj_stream_and_precip(wy, legend = F)
  dev.off()
}
# dev.off()

# Figure 3. Sim v obs ----------------------------------------------------------------

# Example of sim vs obs river hydrograph and heads


gages = c('FJ','AS','BY','LS')
#FJ = Fort Jones gage (USGS)
#AS = Above Serpa Lane Bridge (RCD)
#BY = Below Youngs Dam (RCD)
#LS = Lower Shackleford (CADWR)

# _user input --------------------------------------------------------------

copy_these_files=c("SVIHM_Flow_Obs_Times.obs","Streamflow_FJ_SVIHM.dat")
file.copy(from=file.path(c(ref_dir, mf_results_dir),copy_these_files), to = file.path(postproc_dir, "Results"))

units_cfs = FALSE   #If true, output units will be in cfs. If false output units will be in m^3/day
# dir.create(file.path(postproc_dir,'Results'), showWarnings = FALSE)   #Create Results directory if it doesn't exist
out_dir = file.path(postproc_dir,'Results')
# options(warn=-1)   # suppress warnings (set to 0 to turn warnings on)

Steamflow_Obs_Times = read.table(file.path(postproc_dir,"Results",'SVIHM_Flow_Obs_Times.obs'), header = T, fill = T) #Where is this file? 
Steamflow_Obs_Times$FJ = as.Date(Steamflow_Obs_Times$FJ, format = '%m/%d/%Y')
Steamflow_Obs_Times$LS = as.Date(Steamflow_Obs_Times$LS, format = '%m/%d/%Y')
Steamflow_Obs_Times$AS = as.Date(Steamflow_Obs_Times$AS, format = '%m/%d/%Y')
Steamflow_Obs_Times$BY = as.Date(Steamflow_Obs_Times$BY, format = '%m/%d/%Y')

# _import Fort Jones streamflow  observations -------------------------------------------------------


#Create .obs file in results
# FJ_obs_file = "C:/Users/Claire/Documents/GitHub/SVIHM/Streamflow_Regression_Model/USGS_11519500_WY_1942_2018.txt"
# FJ_obs_all = read.table(FJ_obs_file, header = T)
# dates = as.Date(FJ_obs_all$Date, format = "%m/%d/%Y")
# FJ_obs_1990_2018 = FJ_obs_all[dates >= as.Date("1990-10-01") & dates < as.Date("2018-10-01"),]
# write.table(FJ_obs_1990_2018,file.path(postproc_dir,'SVIHM_FJ_1990-2018.obs'))

#MAke a new one for 2018?

#read 
FJ_obs_cfs = read.table(file.path(postproc_dir,"Results",'SVIHM_FJ_1990-2018.obs'), header=TRUE)[c(1,2)]
names(FJ_obs_cfs) = c('Date','Streamflow_obs_FJ_cfs')

FJ_sim_m3day = read.table(file.path(postproc_dir,"Results",'Streamflow_FJ_SVIHM.dat'),skip=2)[c(1,3)]
names(FJ_sim_m3day) = c('Date', 'm3day')
FJ_sim_m3day$Date = as.Date(FJ_sim_m3day$Date, origin = '1990-09-30')
FJ_m3day = data.frame(Date = as.Date(FJ_obs_cfs$Date, format = "%m/%d/%Y"), 
                    Observed = FJ_obs_cfs$Streamflow_obs_FJ_cfs/0.000408734569, 
                    Simulated = FJ_sim_m3day$m3day)


#CURRENTLY: MAKING THIS BEAUTIFUL
plot(FJ_m3day$Date, FJ_m3day$Observed, log="y",
     lwd=2, type = "l", col = "blue", 
     xlim = as.Date(c("1990-10-01", "2011-09-30")), ylim = c(4000,6.8E7),
     ylab = "Average daily flow (m3 per day)",
     xlab = "Date")
#axis magic
lines(FJ_m3day$Date, FJ_m3day$Simulated, lwd = 2, col = "red")

#Inspect each water year


# _import other 3 mainstem flow observations -----------------------------------------------

#recreate it from the .flowobs file


for (i in 2:length(gages)){
  obs_cfs_text = readLines(file.path(postproc_dir,paste0('SVIHM_',gages[i],'.obs')))
  obs_cfs = data.frame(Date = as.Date(sapply(lapply(strsplit(obs_cfs_text[seq(2,length(obs_cfs_text))],' '),function(x){x[!x ==""]}),"[[",1),'%m/%d/%Y'))
  obs_cfs$Observed = as.numeric(sapply(lapply(strsplit(obs_cfs_text[seq(2,length(obs_cfs_text))],' '),function(x){x[!x ==""]}),"[[",3))
  idx <- c(1, diff(obs_cfs$Date))  #create grouping value in case data is discontinuous
  i2 <- c(1,which(idx != 1), nrow(obs_cfs)+1)
  obs_cfs$grp <- rep(1:length(diff(i2)), diff(i2))
  sim_trib_m3day = read.table('Streamflow_AS_SVIHM.dat',skip=2)[c(1,3)]  #import Simulation time and streamflow
  sim_trib_m3day$V1 = as.Date(sim_trib_m3day$V1, origin = "1990-09-30")
  names(sim_trib_m3day) = c('Date', 'm3day')
  sim_trib_cfs = sim_trib_m3day
  names(sim_trib_cfs) = c('Date', 'cfs')
  sim_trib_cfs$cfs = sim_trib_cfs$cfs*0.000408734569
  trib_cfs = left_join(sim_trib_cfs,obs_cfs, by = 'Date')
  names(trib_cfs) = c('Date','Simulated','Observed','Obs_grp')
  trib_cfs_UCODE_Obs = trib_cfs[which(trib_cfs$Date %in% Steamflow_Obs_Times$AS),]
  trib_residuals_cfs = na.omit(as.data.frame(trib_cfs_UCODE_Obs$Observed - trib_cfs_UCODE_Obs$Simulated))
  names(trib_residuals_cfs) = 'Residual'
  trib_residuals_m3day = trib_residuals_cfs*2446.58
  trib_residuals_cfs[trib_residuals_cfs$Residual<0,] = -log10(-trib_residuals_cfs[trib_residuals_cfs$Residual<0,])
  trib_residuals_cfs[trib_residuals_cfs$Residual>0,] = log10(trib_residuals_cfs[trib_residuals_cfs$Residual>0,])
  trib_residuals_m3day[trib_residuals_m3day$Residual<0,] = -log10(-trib_residuals_m3day[trib_residuals_m3day$Residual<0,])
  trib_residuals_m3day[trib_residuals_m3day$Residual>0,] = log10(trib_residuals_m3day[trib_residuals_m3day$Residual>0,])
  
  eval(parse(text = paste0(gages[i],'_cfs = trib_cfs')))
  eval(parse(text = paste0(gages[i],'_residuals_cfs = trib_residuals_cfs')))
  eval(parse(text = paste0(gages[i],'_residuals_m3day = trib_residuals_m3day')))
}


# Figure 4. Nash-Sutcliffe  ---------------------------------------------------------


# Figure 5. ET check






# scratchwork -------------------------------------------------------------


#subset Fort Jones rain gage data
# station_id_number = "USC00043182" #Ft Jones station
# noaa_fj = noaa[noaa$STATION == station_id_number,]
# #Process NOAA data - convert to inches, select for single station, aggregate to water year
# noaa$PRCP_in <- noaa$PRCP/25.4
# noaa_fj$DATE <- as.Date(noaa_fj$DATE)
# noaa_fj$water_year <- wtr_yr(noaa_fj$DATE)

#Subset rainfall date vector
# noaa_fj_wy = noaa_fj[noaa_fj$DATE >= wy_start_date & noaa_fj$DATE < nextwy_start_date,]
# noaa_fj_wy=noaa_fj_wy[,c("DATE", "PRCP_in")]
# #Fill in gaps with NA values
# dates = seq(from = wy_start_date, by="days", length.out=as.numeric(nextwy_start_date - wy_start_date))
# rain_wy = data.frame(date = dates)
# rain_wy = merge(rain_wy, noaa_fj_wy, by.x = "date", by.y = "DATE")


# theme(legend.title=element_blank(),
#       panel.grid.major = element_line(colour="darkgray", linetype = "dashed"),#element_blank(),
#       panel.grid.minor = element_blank(),
#       panel.background = element_rect(color = 'black', fill = NA),
#       plot.background = element_rect(color = NA, fill = NA),
#       axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), 
#       axis.text = element_text(size = 8),
#       # plot.title = element_text(hjust = 0.5),
#       legend.position = c(0.35, 0.94),
#       legend.key = element_rect(fill = NA, color = NA),
#       legend.background = element_rect(fill = NA, color = NA),
#       legend.direction = 'horizontal',
#       legend.text = element_text(size = 6, margin = margin(r=1,l=1, unit = 'pt')),
#       # legend.key.height = unit(10,'pt')
# )

