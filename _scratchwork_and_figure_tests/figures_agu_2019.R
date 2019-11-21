#Figures for AGU 2019

rm(list=ls())
library(ggplot2)
library(reshape2)
library(dplyr)
library(grid)
library(rstudioapi)

library(plyr)
library(cowplot)
library(zoo)

# 1. Set up directory names and global plot settings
# Project Directory
# gsp_dir <- here::here() #assigns gsp_dir when knitting

gsp_dir = "C:/Users/Claire/Documents/GitHub/SiskiyouGSP2022"
# #CK's computer: during figure development, when working in RStudio, check for here() path going to Documents folder. If so, correct it.
# gsp_dir_pieces = unlist(strsplit(gsp_dir, "/")); if(tail(gsp_dir_pieces, n=1) %in% c("Documents","SVIHM")){ gsp_dir = "~/GitHub/SiskiyouGSP2022"}
fig_dir = file.path(gsp_dir, "GSP_Figures")
dms_dir = file.path(gsp_dir, "Data_Management_System")
dms_archive_dir = "C:/Users/Claire/Box/SiskiyouGSP2022_DMS"
scott_conditions_dir = file.path(gsp_dir, "GSP_Analyses", "Scott_Groundwater_Conditions")

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



# Figure 1. water budget----------------------------------------------------------------

# Example of a water budget for a wet, dry, and normal year (two types of normal, 2010 and 2015)



# Figure 2. hydrographs ----------------------------------------------------------------

# Example of a river hydrograph and rainfall record in a wet, dry, and 2 types of normal year

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

fj_precip_record_figure = function(){
  
  #Process NOAA data - convert to inches, select for single station, aggregate to water year
  noaa$PRCP_in <- noaa$PRCP/25.4
  station_id_number = "USC00043182" #Ft Jones station
  noaa_fj = noaa[noaa$STATION == station_id_number,]
  noaa_fj$DATE <- as.Date(noaa_fj$DATE)
  noaa_fj$water_year <- wtr_yr(noaa_fj$DATE)
  
  station_id_number = "USC00049866" # yreka station
  noaa_yr = noaa[noaa$STATION == station_id_number,]
  noaa_yr$DATE <- as.Date(noaa_yr$DATE)
  noaa_yr$water_year <- wtr_yr(noaa_yr$DATE)
  
  #Plot daily precip data for each water year
  # wys = unique(noaa_station$water_year)
  wys = 1991:2018
  par(mfrow=c(2,1))
  for(wy in wys){
    dates_fj = noaa_fj$DATE >= as.Date(paste0(wy-1, "-10-01")) & noaa_fj$DATE < as.Date(paste0(wy, "-10-01"))
    dates_yr = noaa_yr$DATE >= as.Date(paste0(wy-1, "-10-01")) & noaa_yr$DATE < as.Date(paste0(wy, "-10-01"))

    plot(noaa_fj$DATE[dates_fj], noaa_fj$PRCP_in[dates_fj], type = "l", ylim = c(0,3),
         xlab = paste("date in water year", wy), ylab = "FJ daily precip (in)")
    
    plot(noaa_yr$DATE[dates_yr], noaa_yr$PRCP_in[dates_yr], type = "l",  ylim = c(0,3),
         xlab = paste("date in water year", wy), ylab = "YR daily precip (in)")
    
  }

  
    annual_precip <- ggplot(noaa_station_wateryear, aes(x = water_year, y = PRCP_in)) +
    geom_bar(stat = "identity", color="black", fill="lightsteelblue1")+
    
    labs(title = "Annual water year precipitation with 10-year rolling and long-term means (18 in.)",
         subtitle = as.character(unique(noaa_station$NAME)),
         y = "Annual Precipitation (in)",
         x = "Water Year (October 1 to September 30)") +
    geom_path( aes(y=rollmean(PRCP_in, k=10, na.pad = TRUE, na.rm=TRUE, align="right")), color = "midnightblue")+ #trailing 10-year average
    geom_hline(aes(yintercept=mean(PRCP_in)), linetype="dashed")+
    scale_linetype_manual(name = "Averages", values=c("black","midnightblue"))+#, color = c("black", "midnightblue"))+
    # theme(legend.position=c(0.5,0.5))
    theme_minimal()
  

  #Arrange in 3-panel figure
  fig.plot <- plot_grid(annual_precip, monthly_precip, 
                        nrow=2,labels = c("A", "B"))
  fig.plot
}

fj_precip_record_figure()


fj_streamflow_by_wy_figure= function(){
  #Retrieve fort jones gage data
  library(dataRetrieval)
  library(lubridate)
  fj_num = "11519500"
  fjd_all = readNWISdv(siteNumbers = fj_num, parameterCd="00060" )
  fjd_all = renameNWISColumns(fjd_all)
  #Only run analyses for post-1977 water year
  fjd = fjd_all[fjd_all$Date >= as.Date("1976-10-01"),]
  
  #add water year
  fjd$wy = year(fjd$Date); fjd$wy[month(fjd$Date) > 9] = fjd$wy[month(fjd$Date) > 9]+1
  
  #calculate aggregate daily average flow, by month, for whole record
  daily_avg_by_month = aggregate(fjd$Flow, by = list(month(fjd$Date)), FUN = mean)
  colnames(daily_avg_by_month) = c("month", "daily_avg_cfs")
  #calculate average daily flow by day of year, for whole record
  fjd$dowy = as.numeric(fjd$Date - as.Date(paste0(fjd$wy-1, "-10-01"))) #day of water year
  daily_avg_by_dowy = aggregate(fjd$Flow, by = list(fjd$dowy), FUN = mean)
  colnames(daily_avg_by_dowy)=c("day_of_wy", "daily_avg_cfs")
  
  max_flow = max(fjd$Flow)
  pdf( "FJ Stream Gage Water Year Hydrographs.pdf", 8.5,11/2)
  for(i in 1977:2018){
    
    # png(paste(i,"FJ Stream Gage Water Year Hydrographs.png"), width = 8.5,height = 11/2, units = "in",res = 300)
    wy_start_date = as.Date(paste0(i-1,"-10-01"))
    nextwy_start_date = as.Date(paste0(i,"-10-01"))
    fjd_wy = fjd[fjd$Date >= wy_start_date & fjd$Date < nextwy_start_date,]
    plot(fjd_wy$Date, fjd_wy$Flow, type="l", lwd = 2, col = "blue",
         log = "y", yaxt = "n", ylim = c(1, max_flow),
         main = paste("Water Year", i), xlab = "Month in Water year", 
         ylab = "Average Daily Flow (cfs)")
    axis(side = 2, at = 10^(0:4), las = 2, labels = c("1", "10", "100", "1000", "10,000"))
    axis(side = 2, tck = -.01, at = rep(1:9, 5) * rep(10^(0:4), each = 10), labels = NA)
    abline(h = 10^(0:4), v = seq(wy_start_date, nextwy_start_date, by="month"),
           lty = 3, col = "gray")
    abline(h = 40, col = "brown", lty = 2, lwd = 2)
    # monthly_avg_xvector = as.Date(paste0(c(rep(i,9),rep(i-1,3)), "-",daily_avg_by_month$month,"-01"))
    # month_order = c(10:12, 1:9)
    # lines(monthly_avg_xvector[month_order], daily_avg_by_month$daily_avg_cfs[month_order], 
    #       type = "l", lwd = 2, col = "gray")
    
    # daily_avg_xvector = wy_start_date + daily_avg_by_dowy$day_of_wy
    # lines(daily_avg_xvector, daily_avg_by_dowy$daily_avg_cfs, type = "l", lwd = 2, col = "royalblue")
    # dev.off()
  }
  dev.off()
}


# Figure 3. Sim v obs ----------------------------------------------------------------

# Example of sim vs obs river hydrograph and heads


gages = c('FJ','AS','BY','LS')
#FJ = Fort Jones gage (USGS)
#AS = Above Serpa Lane Bridge (RCD)
#BY = Below Youngs Dam (RCD)
#LS = Lower Shackleford (CADWR)

# _user input --------------------------------------------------------------

svihm_dir = dirname(dirname(dirname(getActiveDocumentContext()$path)))
ref_dir = file.path(svihm_dir, "SVIHM_Input_Files", "reference_data")
mf_results_dir = file.path(svihm_dir, "MODFLOW", "hist") # historical 1991-2018 wy 
postproc_dir = file.path(svihm_dir, "R_Files", "Post-Processing")


copy_these_files=c("SVIHM_Flow_Obs_Times.obs","Streamflow_FJ_SVIHM.dat")
file.copy(from=file.path(c(ref_dir, mf_results_dir),copy_these_files), to = postproc_dir)

units_cfs = FALSE   #If true, output units will be in cfs. If false output units will be in m^3/day
dir.create(file.path(postproc_dir,'Results'), showWarnings = FALSE)   #Create Results directory if it doesn't exist
out_dir = file.path(postproc_dir,'Results')
options(warn=-1)   # suppress warnings (set to 0 to turn warnings on)

Steamflow_Obs_Times = read.table('SVIHM_Flow_Obs_Times.obs', header = T, fill = T) #Where is this file? 
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
# write.table(FJ_obs_1990_2018,file.path(postproc_dir,'SVIHM_FJ_1990-2011.obs'))

#read 
FJ_obs_cfs = read.table(file.path(postproc_dir,'SVIHM_FJ_1990-2011.obs'), header=TRUE)[c(1,2)]
names(FJ_obs_cfs) = c('Date','Streamflow_obs_FJ_cfs')

FJ_sim_m3day = read.table(file.path(postproc_dir,'Streamflow_FJ_SVIHM.dat'),skip=2)[c(1,3)]
names(FJ_sim_m3day) = c('Date', 'm3day')
FJ_sim_m3day$Date = as.Date(FJ_sim_m3day$Date, origin = '1990-09-30')
FJ_cfs = data.frame(Date = as.Date(FJ_obs_cfs$Date, format = "%m/%d/%Y"), 
                    Observed = FJ_obs_cfs$Streamflow_obs_FJ_cfs, 
                    Simulated = FJ_sim_m3day$m3day*0.000408734569)


# _import other 3 mainstem flow observations -----------------------------------------------

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