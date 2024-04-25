library(RSVP)
library(viridis)
library(hydroGOF)
library(RMODFLOW)
library(ggplot2)
library(reshape2)
library(sf)
library(colorspace)

## Graphs promised in 5/3 SWRC presentation
# Flow differences: time series at FJ gauge (zoomed in for specific water year)
# Map of flow differences in Oct 2021 and Oct 2022, between BAU (0 curtail) and historical basecase
# Drawdown maps between historical and BAU scenarios, Fall 2021 and Fall 2022
# Overall groundwater storage time series: 2 lines on same graph (historical and BAU)


#/////////////////-
# V I S U A L S

#-------------------------------------------------------------------------------------------------#
# Settings ----------------------------------------------------------------

origin_date <- as.Date('1990-09-30')

create_sp_charts = FALSE  # Many SPs, very slow

# Directories
# run_dir <- file.path('../../Run/')
# swbm_dir = file.path(run_dir, 'SWBM')
# mf_dir <- file.path(run_dir, 'MODFLOW')

# # Plot 1: 2018 calibrated basecase vs updated 2023 basecase versions
# s1 = "basecase_2023.06.05"
# s1_dir <- file.path('../../Scenarios',s1)
# swbm1_dir = file.path(s1_dir, 'SWBM')
# mf1_dir <- file.path(s1_dir, 'MODFLOW')
# s5 = "basecase_2018"
# s5_dir = "C:/Users/Claire/Documents/GitHub/SVIHM/Scenarios/basecase"
# swbm5_dir = s5_dir
# mf5_dir = s5_dir



# # Plot 2: 2022 what-if curtailment scenarios
# s1 = "basecase_2023.06.05"
# s1_dir <- file.path('../../Scenarios',s1)
# swbm1_dir = file.path(s1_dir, 'SWBM')
# mf1_dir <- file.path(s1_dir, 'MODFLOW')
#
# s2 = "curtail_00_pct_all_years"
# s2_dir <- file.path('../../Scenarios', s2)
# swbm2_dir = file.path(s2_dir, 'SWBM')
# mf2_dir <- file.path(s2_dir, 'MODFLOW')

# # more scenarios?
# s3 = "curtail_50_pct_2022"
# s3_dir <- file.path('../../Scenarios',s3)
# swbm3_dir = file.path(s3_dir, 'SWBM')
# mf3_dir <- file.path(s3_dir, 'MODFLOW')
# s4 = "curtail_30_pct_2022"
# s4_dir <- file.path('../../Scenarios',s4)
# swbm4_dir = file.path(s4_dir, 'SWBM')
# mf4_dir <- file.path(s4_dir, 'MODFLOW')


# ### Plot 3: 2023 curtailment forecasts
# s1 = "basecase_2023.06.05_curtail_00_pct_2023"
# s1_dir <- file.path('../../Scenarios', s1)
# swbm1_dir = file.path(s1_dir, 'SWBM')
# mf1_dir <- file.path(s1_dir, 'MODFLOW')
# s2 = "basecase_2023.06.05_curtail_10_pct_2023"
# s2_dir <- file.path('../../Scenarios',s2)
# swbm2_dir = file.path(s2_dir, 'SWBM')
# mf2_dir <- file.path(s2_dir, 'MODFLOW')
# # more scenarios?
# s3 = "basecase_2023.06.05_curtail_30_pct_2023"
# s3_dir <- file.path('../../Scenarios',s3)
# swbm3_dir = file.path(s3_dir, 'SWBM')
# mf3_dir <- file.path(s3_dir, 'MODFLOW')
# s4 = "basecase_2023.06.05_curtail_50_pct_2023"
# s4_dir <- file.path('../../Scenarios',s4)
# swbm4_dir = file.path(s4_dir, 'SWBM')
# mf4_dir <- file.path(s4_dir, 'MODFLOW')


### Plot 4: Flow difference maps for 2024 MAR applications
s1 = "basecase_noMAR_thru_2024.03.31"
s1_dir <- file.path('../../Scenarios', s1)
swbm1_dir = file.path(s1_dir, 'SWBM')
mf1_dir <- file.path(s1_dir, 'MODFLOW')
s2 = "basecase_thru_2024.03.31"
s2_dir <- file.path('../../Scenarios',s2)
swbm2_dir = file.path(s2_dir, 'SWBM')
mf2_dir <- file.path(s2_dir, 'MODFLOW')



# TODO automate finding latest version
update_dir <- latest_dir(data_dir['update_dir','loc'])  #file.path('../../SVIHM_Input_Files/Updates/2022-04-13/')
plot_data_dir = file.path('../../SVIHM_Input_Files/reference_data_for_plots/')


plots1_dir <- file.path(s1_dir, 'Plots')
plots2_dir <- file.path(s2_dir, 'Plots')
#plots3_dir <- file.path(s3_dir, "Plots")
#plots4_dir <- file.path(s4_dir, "Plots")
out_dir = file.path('../../Scenarios', "_Comparison_Plots")


# if (!dir.exists(plots1_dir)) {dir.create(plots1_dir, recursive = T)}
# if (!dir.exists(plots2_dir)) {dir.create(plots2_dir, recursive = T)}
# if (!dir.exists(plots3_dir)) {dir.create(plots3_dir, recursive = T)}
# if (!dir.exists(plots4_dir)) {dir.create(plots4_dir, recursive = T)}

# info from general_inputs.txt
gen_inputs1 = strsplit(readLines(file.path(swbm1_dir, "general_inputs.txt")), "  ")

#assumes same number of stress periods in scenarios 1 and 2
wy_start = as.numeric(gen_inputs1[[1]][2])
start_date = as.Date(paste0(wy_start-1,"-10-01"))
n_stress = as.numeric(gen_inputs1[[2]][3])

m3day_to_cfs = 1 * 35.3147 * 1/(60*60*24)


#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
# Read in Data ------------------------------------------------------------

#-- Observed

#-- FJ
fj_obs <- read.csv(file.path(update_dir, list.files(update_dir, pattern = 'FJ (USGS 11519500)*')),
                   stringsAsFactors = F)
fj_obs$Date <- as.Date(fj_obs$Date)

# #-- Serpa Lane (Not in GITHUB - stored locally #TODO permissions)
# sl_obs <- read.table('c:/Users/lelan/Box/Research/Scott Valley/Data/Streamflow/Scott River Above Serpa Lane.txt',
#                      header=T)
# sl_obs$Date <- as.Date(sl_obs$Date, format = '%m/%d/%Y')
# sl_obs$Flow <- sl_obs$Streamflow_cfs
#
# #-- Below Youngs Dam (Not in GITHUB - stored locally #TODO permissions)
# by_obs <- read.table('c:/Users/lelan/Box/Research/Scott Valley/Data/Streamflow/Scott River Below Youngs Dam.txt',
#                      header=T)
# by_obs$Date <- as.Date(by_obs$Date, format = '%m/%d/%Y')
# by_obs$Flow <- by_obs$Streamflow_cfs
#
# #-- Group surface water
# streams <- list(fj_obs, sl_obs, by_obs)
# stream_names <- c('Fort Jones', 'Serpa Lane', 'Below Youngs Dam')
# stream_short <- c('FJ', 'AS', 'BY')

#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
#-- Modeled

#-- HOB data
# hob_locs <- read.csv(file.path(data_dir['ref_data_dir','loc'], 'hob_wells.csv'),
#                      row.names=1, stringsAsFactors = F)
# hob1 <- import_HOB(hob_input = file.path(mf1_dir, 'SVIHM.hob'),
#                   hob_output = file.path(mf1_dir, 'HobData_SVIHM.dat'),
#                   origin_date = origin_date)
# hob1 <- hob1[order(hob1$row, hob1$column),]
#
# hob2 <- import_HOB(hob_input = file.path(mf2_dir, 'SVIHM.hob'),
#                    hob_output = file.path(mf2_dir, 'HobData_SVIHM.dat'),
#                    origin_date = origin_date)
# hob2 <- hob1[order(hob2$row, hob2$column),]


#-- SFR Data (Turn into function?)
sfr_locs <- read.csv(file.path(data_dir['ref_data_dir','loc'], 'sfr_gages.csv'),
                     row.names=1, stringsAsFactors = F)
streams_sim1 <- list(import_sfr_gauge(file.path(mf1_dir, 'Streamflow_FJ_SVIHM.dat'), origin_date = origin_date),
                    import_sfr_gauge(file.path(mf1_dir, 'Streamflow_AS_SVIHM.dat'), origin_date = origin_date),
                    import_sfr_gauge(file.path(mf1_dir, 'Streamflow_BY_SVIHM.dat'), origin_date = origin_date))
streams_sim2 <- list(import_sfr_gauge(file.path(mf2_dir, 'Streamflow_FJ_SVIHM.dat'), origin_date = origin_date),
                     import_sfr_gauge(file.path(mf2_dir, 'Streamflow_AS_SVIHM.dat'), origin_date = origin_date),
                     import_sfr_gauge(file.path(mf2_dir, 'Streamflow_BY_SVIHM.dat'), origin_date = origin_date))
streams_sim3 <- list(import_sfr_gauge(file.path(mf3_dir, 'Streamflow_FJ_SVIHM.dat'), origin_date = origin_date),
                     import_sfr_gauge(file.path(mf3_dir, 'Streamflow_AS_SVIHM.dat'), origin_date = origin_date),
                     import_sfr_gauge(file.path(mf3_dir, 'Streamflow_BY_SVIHM.dat'), origin_date = origin_date))
streams_sim4 <- list(import_sfr_gauge(file.path(mf4_dir, 'Streamflow_FJ_SVIHM.dat'), origin_date = origin_date),
                     import_sfr_gauge(file.path(mf4_dir, 'Streamflow_AS_SVIHM.dat'), origin_date = origin_date),
                     import_sfr_gauge(file.path(mf4_dir, 'Streamflow_BY_SVIHM.dat'), origin_date = origin_date))

streams_sim5 <- list(import_sfr_gauge(file.path(mf5_dir, 'Streamflow_FJ_SVIHM.dat'), origin_date = origin_date),
                     import_sfr_gauge(file.path(mf5_dir, 'Streamflow_AS_SVIHM.dat'), origin_date = origin_date),
                     import_sfr_gauge(file.path(mf5_dir, 'Streamflow_BY_SVIHM.dat'), origin_date = origin_date))

#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
# Streamflow Comparison Maps ------------------------------------------------------

flow_units = "Flow Diff. (cfs)" # "Flow Diff. (1000 m3/day)", "Flow (cfs)", "Flow (1000 m3/day)"
# Read and process streamflow data - output from modflow
save_sfr_array = function(scen_dir){
  plots_dir <- file.path(scen_dir, 'Plots')
  mf_dir_i <- file.path(scen_dir, 'MODFLOW')


  sfr_glob_text = readLines(file.path(mf_dir_i, "Streamflow_Global.dat"))
  start_rows = grep("STREAM LISTING", sfr_glob_text) + 5 #one start for each stress period
  n_reach = start_rows[2]-start_rows[1]-8  # 8 extra header rows at each timestep

  colname_list = c("LAYER", "ROW","COL", "STREAM_SEG_NO", "RCH_NO", "FLOW_INTO_STRM_RCH",
                   "FLOW_TO_AQUIFER", "FLOW_OUT_OF_STRM_RCH","OVRLND_RUNOFF","DIRECT PRECIP",
                   "STREAM_ET","STREAM_HEAD", "STREAM_DEPTH", "STREAM_WIDTH",
                   "STREAMBED_CONDCTNC","STREAMBED_GRADIENT")

  # Initialize array
  reach_array = array(data=NA, dim = c(length(start_rows), n_reach, 16))
  # Process SFR values into an array of row, column, and stress period
  for(i in 1:length(start_rows)){
    start_row = start_rows[i];
    sfr_stress = sfr_glob_text[start_row:(start_row+n_reach-1)]
    for(j in 1:n_reach){
      sfr_reach = unlist(strsplit(trimws(sfr_stress[j]), " ")) #split on space character
      sfr_reach = sfr_reach[nchar(sfr_reach)>0] #this produces a lot of blank strings; get rid of those
      reach_array[i,j,] = sfr_reach
    }
    # Save giant reach file as .Rdata
    # saveRDS(object=reach_array,file=file.path(plot_data_dir,"sfr_reach_array.Rdata"))
    saveRDS(object=reach_array,file=file.path(plots_dir,"sfr_reach_array.rds"))
  }
}

# if it's daily sfr data:
aggregate_daily_sfr_array_to_monthly = function(sfr_array){
  # assumes SFR array
}

# Read in reach arrays
# Reading SFR data takes ~5 mins. Save to an .RDS file for convenience
if(!file.exists(file.path(plots1_dir, "sfr_reach_array.RDS"))){
  save_sfr_array(scen_dir = s1_dir)
}
if(!file.exists(file.path(plots2_dir, "sfr_reach_array.RDS"))){
  save_sfr_array(scen_dir = s2_dir)
}
if(!file.exists(file.path(plots3_dir, "sfr_reach_array.RDS"))){
  save_sfr_array(scen_dir = s3_dir)
}
if(!file.exists(file.path(plots4_dir, "sfr_reach_array.RDS"))){
  save_sfr_array(scen_dir = s4_dir)
}

reach_array1 = readRDS(file.path(s1_dir,"Plots","sfr_reach_array.RDS"))
reach_array2 = readRDS(file.path(s2_dir,"Plots","sfr_reach_array.RDS"))
reach_array3 = readRDS(file.path(s3_dir,"Plots","sfr_reach_array.RDS"))
reach_array4 = readRDS(file.path(s4_dir,"Plots","sfr_reach_array.RDS"))



# Set up for SFR stream network maps

# Check flow max
max(as.numeric(as.character(reach_array1[,,8]))) # max flow out
max(as.numeric(as.character(reach_array2[,,8]))) # max flow out
max(as.numeric(as.character(reach_array3[,,8]))) # max flow out
# Breaks for flow
if(flow_units == "Flow (1000 m3/day)"){flow_breaks_manual = c(0, 2.5, 20, 50, 100, 300, 700, 6500)*1000 }
if(flow_units == "Flow (cfs)"){flow_breaks_manual = c(0, 2.5, 20, 50, 100, 300, 700, 6500)*1000 * m3day_to_cfs}
if(flow_units == "Flow Diff. (1000 m3/day)"){flow_breaks_manual = c(0, 2.5, 20, 50, 100, 300, 700, 6500)*1000 }
if(flow_units == "Flow Diff. (cfs)"){flow_breaks_manual = c(0, 1, 5, 10, 15, 20, 100, 3000)}

#Set color palette
n_classes = 7
pal = rev(sequential_hcl(n_classes, palette = "ag_GrnYl"))
# Read in GIS data
seg = st_read(dsn = plot_data_dir, layer = "SFR_segments_sugar_pts")
seg = st_transform(seg, crs = st_crs(3310))
# make identifier for each seg point
seg$row_col = paste(seg$row, seg$column, sep="_")
seg$flow_out = NA; seg$depth = NA; seg$color =NA

#read in Bulletin 118 groundwater basin boundary shapefile
basin = st_read(dsn = plot_data_dir, layer ="SGMA_B118_SV")
basin = st_transform(basin, crs = st_crs(3310))
#generate background color polygon
bg_poly = st_buffer(x = basin, dist=1e5)


#### Plot streamflow differences

#make a pdf appendix of each timestep of dry or wet

#make table of months and years for each stress period
stress_period_table = data.frame(stress_period=1:n_stress); sp_tab = stress_period_table
sp_tab$date = seq.Date(from = start_date, by = "month", length.out=n_stress)
sp_tab$month = month(sp_tab$date)
sp_tab$water_year = year(sp_tab$date); sp_tab$water_year[sp_tab$month>9] = year(sp_tab$date[sp_tab$month>9])+1

#to make a pdf appendix with each stress period plotted:
pdf_name = paste0("sfr_diff", s2, "minus",s1, ".pdf")
pdf(file.path(plots1_dir, pdf_name), width=8.5, height=11)
for(i in 1:length(start_rows)){

  #to make a png figure with manually selected stress periods plotted
  # png(file.path(out_dir, "wet_dry_stream_4yrs.png"),
  #     width=7.5, height=16, units = "in", res=300)
  # par(mfrow=c(4,1), mar = c(1,1,1,1))
  # for(i in c(287, 323, 239, 299)){ #Aug of 2014 (wet), 2017 (dry), 2010, and 2015 (avg, spread and conc)

  stress_period_array1 = data.frame(reach_array1[i,,])
  spa1 = stress_period_array1
  stress_period_array2 = data.frame(reach_array2[i,,])
  spa2 = stress_period_array2


  title_text = c(paste(month.abb[sp_tab$month[i]],"of water year",sp_tab$water_year[i]),
                 paste(s1, "minus", s2))
  process_stress_period_array_matrix = function(spa){
    #process matrix a bit
    colnames(spa)=colname_list
    spa$row_col = paste(spa$ROW, spa$COL, sep="_")
    spa$FLOW_OUT_OF_STRM_RCH = as.numeric(as.character(spa$FLOW_OUT_OF_STRM_RCH))
    spa$STREAM_DEPTH = as.numeric(as.character(spa$STREAM_DEPTH))
    return(spa)
  }
  spa1 = process_stress_period_array_matrix(spa1)
  spa2 = process_stress_period_array_matrix(spa2)

  keep_cols = c("LAYER", "ROW", "COL", "STREAM_SEG_NO", "RCH_NO","row_col")
  diff = spa1[,keep_cols]
  diff$Flow_out_of_strm_rch_s1 = spa1$FLOW_OUT_OF_STRM_RCH
  diff$Flow_out_of_strm_rch_s2 = spa2$FLOW_OUT_OF_STRM_RCH
  diff$flow_out_diff = diff$Flow_out_of_strm_rch_s2 - diff$Flow_out_of_strm_rch_s1
  # par(mfrow = c(1,2)) #if plotting both flow and depth

  # Plot Flowrate Difference
  seg$flow_out_diff = diff$flow_out_diff[match(seg$row_col, diff$row_col)]
  # seg$color = "dodgerblue"
  #all flow segments with flow out of < 1 cfs are considered dry
  # seg$color[seg$flow_out/2446.6 < 1] = "salmon" #convert m^3/day to cfs for threshold comparison
  seg$color[is.na(seg$row_col)]="black"
  # plot(basin, border="darkgray", main=title_text,
  #      sub=paste("stress period",sp_tab$stress_period[i]))
  # plot(seg, col=seg$color, pch=19, cex=0.2, add=T)
  #plot basin polygon as background
  plot(basin$geometry,main=title_text, sub=paste("stress period",sp_tab$stress_period[i]))
  plot(bg_poly$geometry, col="burlywood1",add=T)
  plot(basin$geometry, border="black", add=T
       #col = "gray20",
       # main=title_text, sub=paste("stress period",sp_tab$stress_period[i]))
  )
  #plot river reach centroids, colored according to depth
  plot(seg$geometry,pch=19, cex=1, add=T,
       col=pal[cut(na.omit(seg$flow_out_diff), include.lowest=T,
                   breaks = flow_breaks_manual)])
  # generate legend labels and add legend
  legend_labels = paste(flow_breaks_manual[1:n_classes],
                        flow_breaks_manual[2:(n_classes+1)], sep="-")
  legend(x="bottomleft", fill = pal, title=flow_units,
         legend = legend_labels)#, cex=2.5)

  #legend in CFS
  # legend_labels = paste(round(flow_breaks_manual[1:n_classes]/2446.6),
  #                       round(flow_breaks_manual[2:(n_classes+1)]/2446.6), sep="-")
  # legend(x="bottomleft", fill = pal, title="Flow (cfs)",
  #        legend = legend_labels)


  # # Flow depth
  # seg$depth = spa$STREAM_DEPTH[match(seg$row_col, spa$row_col)]
  # # seg$color = "dodgerblue"
  # #all flow segments with flow out of < 1 cfs are considered dry
  # # seg$color[seg$depth*39.3701 < 3] = "salmon" #convert m to inches for threshold comparison
  # seg$color[is.na(seg$row_col)]="black"
  # #plot basin polygon as background
  # plot(basin, border="darkgray", main=title_text, col = "darkgray",
  #      sub=paste("stress period",sp_tab$stress_period[i]))
  # #plot river reach centroids, colored according to depth
  # plot(seg, col=pal[cut(na.omit(seg$depth), breaks = depth_breaks_manual_7, include.lowest=T)],
  #      pch=19, cex=0.2, add=T)
  # #generate legend labels and add legend
  # legend_labels = paste(depth_breaks_manual_7[1:n_classes], depth_breaks_manual_7[2:(n_classes+1)], sep="-")
  # legend(x="bottomleft", fill = pal, title="Flow depth (m)",
  #        legend = legend_labels)

}
dev.off()


# -------------------------------------------------------------------------#
# -------------------------------------------------------------------------#
# FJ Flow Comparison 1: SWBM 2018 vs 2023 updated basecase ------------------------------------------------------


fjsim1 = streams_sim1[[1]]
# fjsim2 = streams_sim2[[1]]
# fjsim3 = streams_sim3[[1]]
# fjsim4 = streams_sim4[[1]]
fjsim5 = streams_sim5[[1]]


png(filename = file.path(out_dir, "basecase 2018 vs updated 2023 basecase.png"),
    # filename = "prelim fj comparison, 0 curtail, basecase and obs.png",
    height = 11/2, width = 18, units = "in", res = 300)

flow_units = "Flow (cfs)"
date_lims = as.Date(c("2010-01-01","2018-10-31"))
plot(x = fjsim1$Date, y = fjsim1$Flow_cfs, type = "l", log = "y",
     yaxt = "n", xaxt = "n", lwd=2, col = NA,
     main = "Fort Jones Flow Comparison: Observed vs \n 2018 calibrated basecase, and updated 2023 basecase",
     xlab = "Date", ylab = flow_units,
     xlim = date_lims)
# lines(x = fjsim2$Date, y = fjsim2$Flow_cfs, col = 'dodgerblue', lwd = 2)
lines(x = fj_obs$Date, y = fj_obs$Flow, col = "black", lwd = 2)
lines(x = fjsim1$Date, y = fjsim1$Flow_cfs, col = "dodgerblue", lwd = 2)

# lines(x = fjsim3$Date, y = fjsim3$Flow_cfs, col = "green4", lwd = 2)
# lines(x = fjsim4$Date, y = fjsim4$Flow_cfs, col = "goldenrod", lwd = 2)
# lines(x = curtail_flows_line$dates, y = curtail_flows_line$flow_cfs, lwd = 2, lty = 2, col = "blue")

lines(x = fjsim5$Date, y = fjsim5$Flow_cfs, col = "red", lwd = 2)
axis(side = 1, at = seq.Date(from = date_lims[1], to = date_lims[2], by = "year"),
     labels = strftime(seq.Date(from = date_lims[1], to = date_lims[2], by = "year"), format = "%b-%y"),
     crt = 45)
abline(v = seq.Date(from = date_lims[1], to = date_lims[2], by = "year"), lty = 2, col = "gray")
# abline(h = seq(from = 0, by = 50, length.out = 10), lty = 2, col = "gray")
abline(h = (10^c(0,1,2,3,4)), lty = 2, col = "gray")
axis(side = 2, 10^c(0,1,2,3,4))
axis(side = 2, at = 1:9 * sort(rep(10^c(0,1,2,3,4),9)), labels = NA)


legend_tab = data.frame(descrip = c("FJ Obs.", #"Sim. 0% curtail, 2022",
                                    "Basecase 2018 (through WY 2018)",
                                    "Updated Basecase (June 5, 2023)"#, "Sim. 50% curtail", "Sim. 30% curtail",
                                    ),
                        color = c("black","red","dodgerblue"##"green4", "goldenrod",
                                  # "gray30"
                                  ),
                        lty = c(1,1,1))

legend(x = "bottomleft", legend = legend_tab$descrip, lty = legend_tab$lty,
       col = legend_tab$color, lwd = 2,  cex = .7, horiz=T)

dev.off()

# plot(x = fjsim2$Date, y = fjsim2$Flow_cfs, type = "l", log = "y",
#      main = "Fort Jones Flow, basecase (2022 curtailments)",
#      xlab = "Date", ylab = flow_units,
#      xlim = date_lims)
# axis(side = 1, at = seq.Date(from = date_lims[1], to = date_lims[2], by = "month"),
#      labels = strftime(seq.Date(from = date_lims[1], to = date_lims[2], by = "month"), format = "%b-%y"),
#      crt = 45)
# abline(v = seq.Date(from = date_lims[1], to = date_lims[2], by = "3 months"), lty = 2, col = "gray")




# -------------------------------------------------------------------------#
# -------------------------------------------------------------------------#
# FJ Flow Comp. 2: SWBM 2022 what if curtailment scenarios ------------------------------------------------------


fjsim1 = streams_sim1[[1]]
fjsim2 = streams_sim2[[1]]
# fjsim3 = streams_sim3[[1]]
# fjsim4 = streams_sim4[[1]]
# fjsim5 = streams_sim5[[1]]


png(filename = file.path(out_dir, "basecase vs 00 pct curtail 2022.png"),
    # filename = "prelim fj comparison, 0 curtail, basecase and obs.png",
    height = 11/2, width = 8.5, units = "in", res = 300)

flow_units = "Flow (cfs)"
date_lims = as.Date(c("2020-01-01","2022-12-31"))
plot(x = fjsim1$Date, y = fjsim1$Flow_cfs, type = "l", log = "y", yaxt = "n", lwd=2,
     main = "Fort Jones Flow: Curtailment practices in 2022", col = "dodgerblue",
     xlab = "Date", ylab = flow_units,
     xlim = date_lims)
lines(x = fjsim2$Date, y = fjsim2$Flow_cfs, col = 'red', lwd = 2)
lines(x = fj_obs$Date, y = fj_obs$Flow, col = "black", lwd = 2)
# lines(x = fjsim3$Date, y = fjsim3$Flow_cfs, col = "green4", lwd = 2)
# lines(x = fjsim4$Date, y = fjsim4$Flow_cfs, col = "goldenrod", lwd = 2)
# lines(x = curtail_flows_line$dates, y = curtail_flows_line$flow_cfs, lwd = 2, lty = 2, col = "blue")

# lines(x = fjsim5$Date, y = fjsim5$Flow_cfs, col = "gray30", lwd = 2, lty = 2)
axis(side = 1, at = seq.Date(from = date_lims[1], to = date_lims[2], by = "3 months"),
     labels = strftime(seq.Date(from = date_lims[1], to = date_lims[2], by = "3 months"), format = "%b-%y"),
     crt = 45)
abline(v = seq.Date(from = date_lims[1], to = date_lims[2], by = "3 months"), lty = 2, col = "gray")
# abline(h = seq(from = 0, by = 50, length.out = 10), lty = 2, col = "gray")
abline(h = (10^c(0,1,2,3,4)), lty = 2, col = "gray")
axis(side = 2, 10^c(0,1,2,3,4))
axis(side = 2, at = 1:9 * sort(rep(10^c(0,1,2,3,4),9)), labels = NA)


legend_tab = data.frame(descrip = c("FJ Obs.", "Sim. 2022 basecase", "Sim. 0% curtail, 2022"
                                    #, "Sim. 50% curtail", "Sim. 30% curtail",
                                    ),
                        color = c("black","dodgerblue", "red"#,"green4", "goldenrod",
                                  ),
                        lty = c(1,1,1))

legend(x = "bottomleft", legend = legend_tab$descrip, lty = legend_tab$lty,
       col = legend_tab$color, lwd = 2,  cex = .7, ncol=2)#, horiz=T)

dev.off()


# -------------------------------------------------------------------------#
# -------------------------------------------------------------------------#
# FJ Flow Comp. 3: 2023 forecast scenarios ------------------------------------------------------


fjsim1 = streams_sim1[[1]]
fjsim2 = streams_sim2[[1]]
fjsim3 = streams_sim3[[1]]
fjsim4 = streams_sim4[[1]]
# fjsim5 = streams_sim5[[1]]

# make table of emergency minimum flow requirements
date1 = as.Date("2023-01-01"); date2 = as.Date("2024-01-01")
months_day1 = seq.Date(from = date1, to = date2, by = "month")
flow_vals = c(200,200,200,150,150,125,50,30,33,40,60,150)
curtail_flows = data.frame(start_date = months_day1[1:12],
                           end_date = months_day1[2:13]-1,
                           flow_cfs = flow_vals)
starts_only = curtail_flows[,c("start_date","flow_cfs")]; colnames(starts_only) = c("dates", "flow_cfs")
ends_only = curtail_flows[,c("end_date","flow_cfs")]; colnames(ends_only) = c("dates", "flow_cfs")
curtail_flows_line = rbind(starts_only, ends_only)
curtail_flows_line = curtail_flows_line[order(curtail_flows_line$dates),]

# make table of 2019 flow data transposed into 2023
last_date_2023 = max(fj_obs$Date)
last_date_month = month(last_date_2023); last_date_day = day(last_date_2023)
first_date_2019 = as.Date(paste0("2019-",last_date_month, "-",last_date_day))+1
last_date_2019 = as.Date("2019-12-31")
first_dummy_date = last_date_2023+1; last_dummy_date = as.Date("2023-12-31")
dummy_dates_2023 = seq.Date(from = first_dummy_date, to = last_dummy_date, by = "day")

fj_flow_2019 = fj_obs[fj_obs$Date>= first_date_2019 & fj_obs$Date <= last_date_2019,]
fj_dummy_flow = data.frame(Date = dummy_dates_2023, Flow = fj_flow_2019$Flow)


flow_units = "Flow (cfs)"
date_lims = as.Date(c("2023-01-01","2023-12-31"))

png(filename = file.path(out_dir, "fj comparison 2023 forecast_2023 only.png"),
    # filename = "prelim fj comparison, 0 curtail, basecase and obs.png",
    height = 11/2, width = 8.5, units = "in", res = 300)
plot(x = fjsim1$Date, y = fjsim1$Flow_cfs, type = "l", log = "y", yaxt = "n", lwd=2,
     main = "Fort Jones Flow, 2023 Forecasts", col = "red",
     xlab = "Date", ylab = flow_units,
     xlim = date_lims)
# lines(x = fjsim2$Date, y = fjsim2$Flow_cfs, col = 'darkorchid', lwd = 2)
lines(x = fj_obs$Date, y = fj_obs$Flow, col = "black", lwd = 2)
lines(x = fjsim3$Date, y = fjsim3$Flow_cfs, col = "green4", lwd = 2)
lines(x = fjsim4$Date, y = fjsim4$Flow_cfs, col = "goldenrod", lwd = 2)
lines(x = curtail_flows_line$dates, y = curtail_flows_line$flow_cfs, lwd = 2, lty = 2, col = "blue")
lines(x = fj_dummy_flow$Date, y = fj_dummy_flow$Flow, lty = 2)

# axis(side = 1, at = seq.Date(from = date_lims[1], to = date_lims[2], by = "3 months"),
#      labels = strftime(seq.Date(from = date_lims[1], to = date_lims[2], by = "3 months"), format = "%b-%y"),
#      crt = 45)
abline(v = seq.Date(from = date_lims[1], to = date_lims[2], by = "months"), lty = 2, col = "gray")
# abline(h = seq(from = 0, by = 50, length.out = 10), lty = 2, col = "gray")
abline(h = (10^c(0,1,2,3,4)), lty = 2, col = "gray")
axis(side = 2, 10^c(0,1,2,3,4))
axis(side = 2, at = 1:9 * sort(rep(10^c(0,1,2,3,4),9)), labels = NA)

legend_tab = data.frame(descrip = c("FJ Historical Obs.", "FJ Flow, 2019 Obs.",
                                    "Sim. 0% LCS curtail", #"Sim. 10% LCS curtail",
                                    "Sim. 30% LCS curtail",
                                    "Sim. 50% LCS curtail", "Order WR 2021-0083-DWR flows"),# "Basecase 2018"),
                        color = c("black","black", "red", #"darkorchid",
                                  "green4", "goldenrod", "blue"),
                        lwd = c(2,1,2,2,2,2),
                        lty = c(1,2,1,1,1,2))


legend(x = "bottomleft", legend = legend_tab$descrip, lty = legend_tab$lty,
       col = legend_tab$color, lwd = 2,  cex = .6, ncol=2)#, horiz=T)

dev.off()



# -------------------------------------------------------------------------#
# -------------------------------------------------------------------------#
# Drawdown maps ------------------------------------------------------



# -------------------------------------------------------------------------#
# -------------------------------------------------------------------------#
# Groundwater change in storage ------------------------------------------------------

#________________________________________________________________________________________
# 3b. Import MODFLOW Budget -----------------------------------------------------------------
#________________________________________________________________________________________

MODFLOW_Budget = function(in_dir, filename, mf_bud_terms, suffix,
                          start_date = as.Date("1990-10-01"), end_date = as.Date("2018-09-30"),
                          nstress = 336, start_wy = 1991, end_wy = 2018){
  library(magrittr)
  InputText = readLines(file.path(in_dir,filename))  #Read in text file
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
  #mf_bud_terms = c('STORAGE', 'CONSTANT_HEAD', 'WELLS', 'RECHARGE', 'ET_SEGMENTS','STREAM_LEAKAGE', 'DRAINS')

  #Extract arrays for cumulative volumes and time step rates for different components of the groundwaterwater budget
  for (i in 1:length(mf_bud_terms)){
    if(mf_bud_terms[i]%in%c('CONSTANT_HEAD','ET_SEGMENTS','STREAM_LEAKAGE')){
      c1 = 4 #column for extracting cumulative data
      c2 = 8 #column for extracting time step flux data
    } else {
      c1 = 3 #column for extracting cumulative data
      c2 = 6 #column for extracting time step flux data
    }
    eval(parse(text = paste0(mf_bud_terms[i],"_cumulative = strsplit(InputText[",mf_bud_terms[i],"_Lines],' ') %>%",
                             'lapply(function(x){x[!x ==""]}) %>%',
                             'sapply("[[",',c1,') %>%',
                             'as.numeric()')))
    eval(parse(text = paste0(mf_bud_terms[i],"_cumulative_in = ",mf_bud_terms[i],"_cumulative[seq(1,n_budget_entries,2)]")))
    eval(parse(text = paste0(mf_bud_terms[i],"_cumulative_out = ",mf_bud_terms[i],"_cumulative[seq(2,n_budget_entries,2)]")))
    eval(parse(text = paste0(mf_bud_terms[i],"_TS_Flux = strsplit(InputText[",mf_bud_terms[i],"_Lines],' ') %>%",
                             'lapply(function(x){x[!x ==""]}) %>%',
                             'sapply("[[",',c2,') %>%',
                             'as.numeric()')))
    eval(parse(text = paste0(mf_bud_terms[i],"_TS_Flux_in = ",mf_bud_terms[i],"_TS_Flux[seq(1,n_budget_entries,2)]")))
    eval(parse(text = paste0(mf_bud_terms[i],"_TS_Flux_out = ",mf_bud_terms[i],"_TS_Flux[seq(2,n_budget_entries,2)]")))
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
  # SP = as.numeric(sapply(lapply(strsplit(InputText[Timestep_SP_Lines],' '),function(x){x[!x ==""]}),"[[",15))  #Extract stress period number for printed budget
  # extra_rows = which(duplicated(SP))
  mf_bud_terms_All = c(mf_bud_terms,'TOTAL')   # All components of the MODFLOW water budget
  #Remove stress periods that didn't converge if there are any
  if(length(extra_rows>0)){
    TS = TS[-extra_rows]
    SP = SP[-extra_rows]
    for (i in 1:length(mf_bud_terms_All)){
      eval(parse(text = paste0(mf_bud_terms_All[i],'_cumulative_in = ',mf_bud_terms_All[i],'_cumulative_in[-extra_rows]')))
      eval(parse(text = paste0(mf_bud_terms_All[i],'_cumulative_out = ',mf_bud_terms_All[i],'_cumulative_out[-extra_rows]')))
      eval(parse(text = paste0(mf_bud_terms_All[i],'_TS_Flux_in = ',mf_bud_terms_All[i],'_TS_Flux_in[-extra_rows]')))
      eval(parse(text = paste0(mf_bud_terms_All[i],'_TS_Flux_out = ',mf_bud_terms_All[i],'_TS_Flux_out[-extra_rows]')))
    }
  }
  for (i in 1:length(mf_bud_terms_All)){
    #Net Cumulative Fluxes
    eval(parse(text = paste0(mf_bud_terms_All[i],'_cumulative_net = ',mf_bud_terms_All[i],'_cumulative_in - ',mf_bud_terms_All[i],'_cumulative_out')))
    # Total Inflow Volume for each Stress Period
    eval(parse(text = paste0(mf_bud_terms_All[i],'_SP_Vol_in = c(',mf_bud_terms_All[i],'_cumulative_in[1],diff(',mf_bud_terms_All[i],'_cumulative_in))')))
    # Total Outflow Volume for each Stress Period
    eval(parse(text = paste0(mf_bud_terms_All[i],'_SP_Vol_out = c(',mf_bud_terms_All[i],'_cumulative_out[1],diff(',mf_bud_terms_All[i],'_cumulative_out))')))
    # Net Volume for each Stress Period
    eval(parse(text = paste0(mf_bud_terms_All[i],'_SP_Vol_net = ',mf_bud_terms_All[i],'_SP_Vol_in - ',mf_bud_terms_All[i],'_SP_Vol_out')))
    # Net flux rate at the end of each time step
    eval(parse(text = paste0(mf_bud_terms_All[i],'_TS_Flux_net = ',mf_bud_terms_All[i],'_TS_Flux_in - ',mf_bud_terms_All[i],'_TS_Flux_out')))
  }
  #Calculate Mass Balance
  Cumulative_Mass_Balance_percent_diff = ((TOTAL_cumulative_in - TOTAL_cumulative_out)/((TOTAL_cumulative_in + TOTAL_cumulative_out)/2))*100
  SP_Mass_Balance_percent_diff = ((TOTAL_SP_Vol_in - TOTAL_SP_Vol_out)/((TOTAL_SP_Vol_in + TOTAL_SP_Vol_out)/2))*100
  TS_Mass_Balance_percent_diff = ((TOTAL_TS_Flux_in - TOTAL_TS_Flux_out)/((TOTAL_TS_Flux_in + TOTAL_TS_Flux_out)/2))*100
  MODFLOW_Budget_Monthly = data.frame(Month = format(seq(start_date, end_date, by = 'month'),'%b-%Y'),
                                      Water_Year = rep(seq(start_wy, end_wy),each=12),
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
  if (missing(suffix)){
    Cumulative_Mass_Balance_percent_diff <<- Cumulative_Mass_Balance_percent_diff
    SP_Mass_Balance_percent_diff <<- SP_Mass_Balance_percent_diff
    TS_Mass_Balance_percent_diff <<- TS_Mass_Balance_percent_diff
    return(MODFLOW_Budget_Monthly)
  } else {
    eval(parse(text = paste0('Cumulative_Mass_Balance_percent_diff_',suffix,' <<- Cumulative_Mass_Balance_percent_diff')))
    eval(parse(text = paste0('SP_Mass_Balance_percent_diff_',suffix,' <<- SP_Mass_Balance_percent_diff')))
    eval(parse(text = paste0('TS_Mass_Balance_percent_diff_',suffix,' <<- TS_Mass_Balance_percent_diff')))
    return(MODFLOW_Budget_Monthly)
  }
}


modflow_file_name = file.path(plots1_dir, paste(scenario_id,"MODFLOW_Water_Budget.csv"))
Print_SWBM_by_landuse = FALSE     # Print 28 year average, dry year (2001), average year (2015), and wet year (2006) SWBM by landuse
LST_Name = 'SVIHM.lst'
WB_Components_MODFLOW = c('STORAGE', 'CONSTANT_HEAD', 'WELLS', 'RECHARGE', 'ET_SEGMENTS','STREAM_LEAKAGE', 'DRAINS')
PRINT_BUDGET = TRUE              # Print monthly water budget to file (TRUE/FALSE)

if(file.exists(modflow_file_name)){
  MODFLOW_Monthly_m3 = read.csv(file = modflow_file_name, header = T)

} else if(!file.exists(modflow_file_name)){
  MODFLOW_Monthly_m3 = MODFLOW_Budget(in_dir = mf1_dir,
                                      filename = LST_Name,
                                      mf_bud_terms = WB_Components_MODFLOW,
                                      start_date = start_date, end_date = end_date,
                                      nstress = n_stress,
                                      start_wy = year(start_date)+1,
                                      end_wy = year(end_date))

  MODFLOW_Monthly_m3$Year = substr(MODFLOW_Monthly_m3$Month, 5,9)
  MODFLOW_Monthly_m3$Month = strtrim(MODFLOW_Monthly_m3$Month,3)

  # Make tables for each month of the record
  # month_abbv = format(seq(as.Date('1990/10/1'), as.Date('1991/9/30'), by = 'month'),'%b')
  # for (i in 1:12){
  #   eval(parse(text = paste0("MODFLOW_",month_abbv[i],"_m3 = subset(MODFLOW_Monthly_m3, select = paste0(WB_Components_MODFLOW,'_net_m3'), Month == '",month_abbv[i],"')")))
  #   eval(parse(text = paste0('MODFLOW_',month_abbv[i],'_m3$Water_Year = seq(1991,end_wy)')))
  # }

  if (PRINT_BUDGET==TRUE){
    write.csv(MODFLOW_Monthly_m3, file = modflow_file_name, row.names = F, quote = F)
  }

}


# Monthly dataframes for specific purposes
MODFLOW_Monthly_m3_for_annual = data.frame(Water_Year = MODFLOW_Monthly_m3$Water_Year,
                                           Recharge = MODFLOW_Monthly_m3$RECHARGE_net_m3,
                                           ET = MODFLOW_Monthly_m3$ET_SEGMENTS_net_m3,
                                           Storage = MODFLOW_Monthly_m3$STORAGE_net_m3,
                                           Drains = MODFLOW_Monthly_m3$DRAINS_net_m3,
                                           Stream_Leakage = MODFLOW_Monthly_m3$STREAM_LEAKAGE_net_m3,
                                           Wells = -1*(MODFLOW_Monthly_m3$WELLS_out_m3),              #negative sign since flux is out
                                           Canal_Seepage_MFR = MODFLOW_Monthly_m3$WELLS_in_m3)

#monthly dataframe for GSP plots
MODFLOW_Monthly_m3 = data.frame(Month = MODFLOW_Monthly_m3$Month,
                                Water_Year = MODFLOW_Monthly_m3$Water_Year,
                                Recharge = MODFLOW_Monthly_m3$RECHARGE_net_m3,
                                ET = MODFLOW_Monthly_m3$ET_SEGMENTS_net_m3,
                                Storage = MODFLOW_Monthly_m3$STORAGE_net_m3,
                                Drains = MODFLOW_Monthly_m3$DRAINS_net_m3,
                                Stream_Leakage = MODFLOW_Monthly_m3$STREAM_LEAKAGE_net_m3,
                                Wells = -1*(MODFLOW_Monthly_m3$WELLS_out_m3),              #negative sign since flux is out
                                Canal_Seepage_MFR = MODFLOW_Monthly_m3$WELLS_in_m3)
#Make annual budget
# MODFLOW_Annual_m3 = subset(MODFLOW_Monthly_m3, select = paste0(WB_Components_MODFLOW,'_net_m3'))
# MODFLOW_Annual_m3_raw$Water_Year = rep(seq(1991,end_wy),each = 12)
MODFLOW_Annual_m3 = aggregate(.~Water_Year, MODFLOW_Monthly_m3_for_annual, FUN = sum)

MODFLOW_Annual_m3_melt = melt(MODFLOW_Annual_m3, id.vars = 'Water_Year')
MODFLOW_Annual_m3_melt$variable = factor(MODFLOW_Annual_m3_melt$variable, levels = c('Recharge','ET','Storage','Drains','Stream_Leakage','Wells', 'Canal_Seepage_MFR'))
MODFLOW_Annual_m3_melt = MODFLOW_Annual_m3_melt[order(MODFLOW_Annual_m3_melt$variable),]



water_budget_monthly = function(MODFLOW_Monthly_m3, output_format = "vector"){

  m3_to_TAF = 35.31 / 43560 / 1000 #ft3/m3 and feet per acre
  MODFLOW_Monthly_m3_melt = melt(MODFLOW_Monthly_m3, id.vars = c('Water_Year', "Month"))
  MODFLOW_Monthly_m3_melt$variable = factor(MODFLOW_Monthly_m3_melt$variable, levels = c('Recharge','ET','Storage','Drains','Stream_Leakage','Wells', 'Canal_Seepage_MFR'))
  MODFLOW_Monthly_m3_melt = MODFLOW_Monthly_m3_melt[order(MODFLOW_Monthly_m3_melt$variable),]
  MODFLOW_Monthly_m3_melt$Month = factor(x = MODFLOW_Monthly_m3_melt$Month, levels = month.abb[c(10:12, 1:9)])

  wys = c(2014, 2017, 2010, 2015)

  aq_wys = MODFLOW_Monthly_m3_melt[MODFLOW_Monthly_m3_melt$Water_Year %in% wys,]
  aq_wys$Water_Year = factor(x = aq_wys$Water_Year)

  pump_wys = aq_wys[aq_wys$variable == "Wells",]
  rch_wys = aq_wys[aq_wys$variable == "Recharge",]
  leak_wys = aq_wys[aq_wys$variable == "Stream_Leakage",]
  stor_wys = aq_wys[aq_wys$variable=="Storage",]

  pump=ggplot(data = pump_wys, aes(x = Month, y= -value * m3_to_TAF, group = Water_Year))+ #negative to make pumping positive for plotting
    labs(x = "none",  y = "Volume (TAF)", #y = expression(paste("Volume (km"^"3","/month)")),
         title="Well Pumping", color = "Water Year")+
    ylim(0,30)+
    geom_line(aes(colour = Water_Year), size = 1.5)+
    scale_colour_manual(labels = c("2010 (Avg. Total Rainfall)","2014 (Dry Year)","2015 (Avg. Total Rainfall)","2017 (Wet Year)"),
                        values = c("2014"="orangered", "2017"="deepskyblue3",
                                   "2010"="darkgoldenrod1", "2015"="darkgoldenrod3"))+
    theme_bw()+ theme(#legend.position="none",
      axis.title.x=element_blank(), axis.text = element_text(size=12),
      axis.title=element_text(size=14), plot.title = element_text(size=14))

  rch=ggplot(data = rch_wys, aes(x = Month, y= value * m3_to_TAF, group = Water_Year))+
    labs(x = "none",  y = "Volume (TAF)", #y = expression(paste("Volume (km"^"3","/month)")),
         title="Recharge Through Soil Zone")+
    ylim(0,30)+
    geom_line(aes(colour = Water_Year), size = 1.5)+
    scale_colour_manual(values = c("2014"="orangered", "2017"="deepskyblue3",
                                   "2010"="darkgoldenrod1", "2015"="darkgoldenrod3"))+
    theme_bw()+ theme(legend.position="none", axis.title.x=element_blank(),
                      axis.text = element_text(size=12),
                      axis.title=element_text(size=14), plot.title = element_text(size=14))

  leak=ggplot(data = leak_wys, aes(x = Month, y= -value * m3_to_TAF, group = Water_Year))+ #negative to make leaking negative
    labs(x = "none",  y = "Volume (TAF)", #y = expression(paste("Volume (km"^"3","/month)")),
         title="Stream Leakage")+
    ylim(-15,15)+
    geom_line(aes(colour = Water_Year), size=1.5)+
    geom_hline(yintercept=0, linetype="solid", color="black")+
    geom_text(label="Net recharge to aquifer",x=9, y=8, size=4)+
    geom_text(label="Net discharge to stream",x=9, y=-8, size=4)+
    scale_colour_manual(values = c("2014"="orangered", "2017"="deepskyblue3",
                                   "2010"="darkgoldenrod1", "2015"="darkgoldenrod3"))+
    theme_bw()+ theme(legend.position="none", axis.title.x=element_blank(),
                      axis.text = element_text(size=12),
                      axis.title=element_text(size=14), plot.title = element_text(size=14))


  stor = ggplot(data = stor_wys, aes(x = Month, y= -value* m3_to_TAF, group = Water_Year))+ # negative so neg vals mean dropping WLs
    labs(x = "none", #y = expression(paste("Volume (km"^"3","/month)")),
         y = "Volume (TAF)", title="Change in Aquifer Storage")+
    ylim(-10,20)+
    geom_line(aes(colour = Water_Year), size=1.5)+
    geom_hline(yintercept=0, linetype="solid", color="black")+
    geom_text(label="Rising water levels",x=10, y=9, size=4)+
    geom_text(label="Declining water levels",x=5, y=-9, size = 4)+
    scale_colour_manual(values = c("2014"="orangered", "2017"="deepskyblue3",
                                   "2010"="darkgoldenrod1", "2015"="darkgoldenrod3"))+
    theme_bw()+
    theme(legend.position="none", axis.text = element_text(size=12), axis.title.x=element_blank(),
          axis.title=element_text(size=14), plot.title = element_text(size=14))

  # from Public Meetings script
  # fig_width = 7
  # fig_height = 5
  # png(file.path(fig_dir,"pump.png"), width = fig_width, height = fig_height, units = "in", res = 300)
  # pump
  # dev.off()
  # png(file.path(fig_dir,"rch.png"), width = fig_width, height = fig_height, units = "in", res = 300)
  # rch
  # dev.off()
  # png(file.path(fig_dir,"leak.png"), width = fig_width, height = fig_height, units = "in", res = 300)
  # leak
  # dev.off()
  # png(file.path(fig_dir,"stor.png"), width = fig_width, height = fig_height, units = "in", res = 300)
  # stor
  # dev.off()


  if(output_format == "png"){png(file.path(agu_figure_dir,"components2.png"), height = 20, width = 7,
                                 units = "in", res = 300)}
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(4,1)))
  vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
  print(pump + theme(legend.justification=c(0,1),
                     legend.position = c(.05,.95),
                     # legend.background = element_blank(),
                     legend.key = element_blank()),
        vp = vplayout(1,1))
  print(rch, vp = vplayout(2,1))
  print(stor, vp=vplayout(3,1))
  print(leak, vp = vplayout(4,1))
  if(output_format == "png"){dev.off()}
}

# png(filename = "water_budget_monthly_tester.png", width = 7, height = 9, units = "in", res = 300)
# scenario_directory = file.path("C:/Users/Claire/Documents/GitHub/SVIHM/Scenarios", "basecase")
# data_tables = import_water_budget_files(scenario_dir = scenario_directory)
# water_budget_monthly(MODFLOW_Monthly_m3 = data_tables$monthly$mf)
# dev.off()



# -------------------------------------------------------------------------#
# -------------------------------------------------------------------------#
# Water Budget Comparison ------------------------------------------------------


SWBM_Terms = c('Precipitation', 'SW Irrigation', 'GW Irrigation', 'ET', 'Recharge', 'Runoff', 'Storage','Error')
SWBM_colors = c('lightblue1',  'darkcyan', 'midnightblue', 'goldenrod','green4', 'mistyrose','black','gray')

convert_monthly_to_annual = function(SWBM_Monthly_m3){
  SWBM_Annual_m3 = aggregate(.~WY,SWBM_Monthly_m3[,!names(SWBM_Monthly_m3)%in% c('Month',"Stress_Period")], FUN = sum)
  return(SWBM_Annual_m3)
}


read_process_swbm_monthly_budget = function(swbm_dir){
  SWBM_Monthly_m3 = read.table(file.path(swbm_dir,'monthly_water_budget.dat'), header = T)
  names(SWBM_Monthly_m3_s1) = c('Month',SWBM_Terms)
  n_stress = nrow(SWBM_Monthly_m3)
  SWBM_Monthly_m3$Month = seq.Date(from = origin_date+1, by = "month", length.out = n_stress)
  SWBM_Monthly_m3$WY = year(SWBM_Monthly_m3$Month)
  SWBM_Monthly_m3$WY[month(SWBM_Monthly_m3$Month)>9] = year(SWBM_Monthly_m3$Month[month(SWBM_Monthly_m3$Month)>9]) +1
  # SWBM_Monthly_m3$Month = format(seq(origin_date, by = "month", length.out = n_stress),'%b-%Y')

  return(SWBM_Monthly_m3)
}

SWBM_Monthly_m3_s1 = read_process_swbm_monthly_budget(swbm_dir = swbm1_dir)
SWBM_Monthly_m3_s2 = read_process_swbm_monthly_budget(swbm_dir = swbm2_dir)

SWBM_Annual_m3_s1 = convert_monthly_to_annual(SWBM_Monthly_m3 = SWBM_Monthly_m3_s1)
SWBM_Annual_m3_s2 = convert_monthly_to_annual (SWBM_Monthly_m3 = SWBM_Monthly_m3_s2)

swbm_annual_m3_diff = SWBM_Annual_m3_s1
swbm_annual_m3_diff[,2:9] = SWBM_Annual_m3_s1[,2:9] - SWBM_Annual_m3_s2[,2:9]
swbm_annual_m3_rel_diff = swbm_annual_m3_diff
swbm_annual_m3_rel_diff[,2:9] = swbm_annual_m3_diff[,2:9] / SWBM_Annual_m3_s1[,2:9]

swbm_monthly_m3_diff = SWBM_Monthly_m3_s1
swbm_monthly_m3_diff[,2:9] = SWBM_Monthly_m3_s1[,2:9] - SWBM_Monthly_m3_s2[,2:9]
swbm_monthly_m3_rel_diff = swbm_monthly_m3_diff
swbm_monthly_m3_rel_diff[,2:9] = swbm_monthly_m3_diff[,2:9] / SWBM_Monthly_m3_s1[,2:9]

sm_dates = SWBM_Monthly_m3_s1[SWBM_Monthly_m3_s1$Month >= as.Date("2022-03-01"),]
smd_dates = swbm_monthly_m3_diff[swbm_monthly_m3_diff$Month >= as.Date("2022-03-01"),]

sw_tot_rel_diff = sum(smd_dates$SW_Irr, na.rm=T) / sum(sm_dates$SW_Irr, na.rm=T)
gw_tot_rel_diff = sum(smd_dates$GW_Irr, na.rm=T) / sum(sm_dates$GW_Irr, na.rm=T)

smrd_dates = swbm_monthly_m3_rel_diff[swbm_monthly_m3_rel_diff$Month >= as.Date("2022-03-01"),]
mean(smrd_dates$SW_Irr*100, na.rm=T)
mean(smrd_dates$GW_Irr*100, na.rm=T)

#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
# Volumetric Budget - SWBM - Monthly bar graph ------------------------------------------------------

# make data long-format
keep_cols = !(colnames(SWBM_Monthly_m3_s1) %in% c("WY", "Stress_Period"))
SWBM_Monthly_m3_s1_melt =  melt(SWBM_Monthly_m3_s1[,keep_cols],
                                id.vars = 'Month')
SWBM_Monthly_m3_s2_melt =  melt(SWBM_Monthly_m3_s2[,keep_cols], id.vars = 'Month')

plot_one_wy_monthly = function(SWBM_Monthly_m3_melt, plot_wy = 2022, save_as_pdf = F){
  # Set water year
  if(save_as_pdf==T){
    pdf(file = file.path(out_dir, "Monthly Budget Plots.pdf"), width = 8.5, height = 11/2)
  }
  for(wy in plot_wy){#1991:2021){ # wy = 1995
    this_wy = wy
    datelims = as.Date(paste0(c(this_wy-1, this_wy), "-10-01"))
    # Make monthly line plot
    # SWBM_Monthly_Mm3_Plot =
    print(ggplot(SWBM_Monthly_m3_melt, aes(x = Month, y = value/1E6, color=variable)) +
            geom_line(size=2) +
            scale_color_manual(values = SWBM_colors) +
            scale_x_continuous(limits = datelims) +
            xlab('') +
            ylab(bquote('Volume ('*Mm^3*')')) +
            ggtitle(paste('Soil Zone Monthly Budget, WY', this_wy)) +
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
                  legend.key.height = unit(10,'pt'))
    )
  }
  if(save_as_pdf==TRUE){dev.off()}
}

plot_one_wy_monthly(SWBM_Monthly_m3_melt = SWBM_Monthly_m3_s1_melt, plot_wy = 2022)
