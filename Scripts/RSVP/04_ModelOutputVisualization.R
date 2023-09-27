library(RSVP)
library(viridis)
library(hydroGOF)
library(RMODFLOW)
library(ggplot2)
library(reshape2)
library(sf)
library(colorspace)

#/////////////////-
# V I S U A L S

#-------------------------------------------------------------------------------------------------#
# Settings ----------------------------------------------------------------

origin_date <- as.Date('1990-09-30')

create_sp_charts = FALSE  # Many SPs, very slow

# Directories
run_dir <- file.path('../../Run/')
swbm_dir = file.path(run_dir, 'SWBM')
mf_dir <- file.path(run_dir, 'MODFLOW')

update_dir <- latest_dir(data_dir['update_dir','loc'])
plot_data_dir = file.path('../../SVIHM_Input_Files/reference_data_for_plots/')


out_dir <- file.path(run_dir, 'Plots')

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = T)
}

# info from general_inputs.txt
gen_inputs = strsplit(readLines(file.path(swbm_dir, "general_inputs.txt")), "  ")
wy_start = as.numeric(gen_inputs[[1]][2])
start_date = as.Date(paste0(wy_start-1,"-10-01"))
n_stress = as.numeric(gen_inputs[[2]][3])

# Output controls
WRITE_SW_BUDGET = T # saves soil water budget tables as .csvs

#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
# Read in Data ------------------------------------------------------------

#-- Observed

#-- FJ
fj_obs <- read.csv(file.path(update_dir, list.files(update_dir, pattern = 'FJ (USGS 11519500)*')),
                   stringsAsFactors = F)
fj_obs$Date <- as.Date(fj_obs$Date)

#-- Serpa Lane (Not in GITHUB - stored locally #TODO permissions)
sl_obs <- read.table('c:/Users/lelan/Box/Research/Scott Valley/Data/Streamflow/Scott River Above Serpa Lane.txt',
                     header=T)
sl_obs$Date <- as.Date(sl_obs$Date, format = '%m/%d/%Y')
sl_obs$Flow <- sl_obs$Streamflow_cfs

#-- Below Youngs Dam (Not in GITHUB - stored locally #TODO permissions)
by_obs <- read.table('c:/Users/lelan/Box/Research/Scott Valley/Data/Streamflow/Scott River Below Youngs Dam.txt',
                     header=T)
by_obs$Date <- as.Date(by_obs$Date, format = '%m/%d/%Y')
by_obs$Flow <- by_obs$Streamflow_cfs

#-- Group surface water
streams <- list(fj_obs, sl_obs, by_obs)
stream_names <- c('Fort Jones', 'Serpa Lane', 'Below Youngs Dam')
stream_short <- c('FJ', 'AS', 'BY')

#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
#-- Modeled

#-- HOB data
hob_locs <- read.csv(file.path(data_dir['ref_data_dir','loc'], 'hob_wells.csv'),
                     row.names=1, stringsAsFactors = F)
hob <- import_HOB(hob_input = file.path(mf_dir, 'SVIHM.hob'),
                  hob_output = file.path(mf_dir, 'HobData_SVIHM.dat'),
                  origin_date = origin_date)
hob <- hob[order(hob$row, hob$column),]

#-- SFR Data (Turn into function?)
sfr_locs <- read.csv(file.path(data_dir['ref_data_dir','loc'], 'sfr_gages.csv'),
                     row.names=1, stringsAsFactors = F)
streams_sim <- list(import_sfr_gauge(file.path(mf_dir, 'Streamflow_FJ_SVIHM.dat'), origin_date = origin_date),
                    import_sfr_gauge(file.path(mf_dir, 'Streamflow_AS_SVIHM.dat'), origin_date = origin_date),
                    import_sfr_gauge(file.path(mf_dir, 'Streamflow_BY_SVIHM.dat'), origin_date = origin_date))

#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
# Statistics --------------------------------------------------------------

#-------------------------------------------------------------------------------------------------#
#-- Processing

stream_combined <- ts_obs_sim_combine(streams, streams_sim, stream_names, val_cols = c('Flow','Flow_cfs'))
#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
#-- Evaluation
cal_split <- as.Date('2012-10-01')
hob_compare <- calc_split_sample_stats(hob$date, hob$sim, hob$hobs,
                                       cal_split,
                                       FUNs = list(NSE, rmse, KGE),
                                       FUN_names = c('NSE', 'RMSE', 'KGE'))
sfrcompare <- calc_split_sample_stats.grouped(stream_combined$Date, stream_combined$sim,
                                              stream_combined$obs, stream_combined$group,
                                              cal_split,
                                              FUNs = list(NSE, rmse, KGE),
                                              FUN_names = c('NSE', 'RMSE', 'KGE'))
#-- Write Out
write.csv(hob_compare, file.path(out_dir, 'hob_compare.csv'))
write.csv(sfrcompare, file.path(out_dir, 'sfr_compare.csv'))
#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
# Groundwater Level Plots -------------------------------------------------

#-- Loop over wells & Plot
pdf(file.path(out_dir,'hobs_plots.pdf'), width=11, height=8.5)
for (well in unique(hob$wellnam)) {
  # Report
  message('Plotting ', well)
  # Set up
  w <- gsub('_','',well)
  wsub <- hob[hob$wellnam == well,]
  well_title <- paste('Well: ', w,
                      '\nRow: ', wsub$row[1],
                      '  |  Col: ', wsub$column[1],
                      '  |  Lay: ', wsub$layer[1])
  # Plot
  plot.gw.hydrograph_wMap(wsub$date,
                       wsub$hobs,
                       wsub$sim,
                       xloc = hob_locs[w,'x'],
                       yloc = hob_locs[w,'y'],
                       map_xs = hob_locs[,'x'],
                       map_ys = hob_locs[,'x'],
                       ylabel = 'Groundwater Elevation (m)',
                       title = well_title, map_x_offset = 0, map_y_offset = 0)
}
# Scatterplots
plot.scatterbox(obs = hob$hobs, sim = hob$sim, groups = rep('Wells', nrow(hob)),
                xlab = 'Observed GW Elevation (m)', ylab = 'Simulated GW Elevation (m)', log = F)

dev.off()
#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
# Streamflow Plots --------------------------------------------------------

#-- Loop over wells & Plot
pdf(file.path(out_dir,'sfr_plots.pdf'), width=11, height=8.5)
i <- 1
lapply(streams_sim, function(x) {
  # Report
  message('Plotting ', stream_names[i])
  # Set up
  gag_title <- paste0('Gage: ',stream_names[i], '\n', attr(x,'info'))
  # Plot
  plot.stream.hydrograph_wMap(list(streams[[i]]$Date, x$Date),
                       streams[[i]]$Flow,
                       x$Flow_cfs,
                       xloc = sfr_locs[stream_short[i],'x'],
                       yloc = sfr_locs[stream_short[i],'y'],
                       map_xs = sfr_locs$x,
                       map_ys = sfr_locs$y,
                       ylabel = 'Streamflow (cfs)',
                       title = gag_title, map_x_offset=0, map_y_offset=0)
  plot.stream.hydrograph_wMap(list(streams[[i]]$Date, x$Date),
                       streams[[i]]$Flow,
                       x$Flow_cfs,
                       log='y',
                       xloc = sfr_locs[stream_short[i],'x'],
                       yloc = sfr_locs[stream_short[i],'y'],
                       map_xs = sfr_locs$x,
                       map_ys = sfr_locs$y,
                       ylabel = 'Streamflow, log10 (cfs)',
                       title = gag_title, map_x_offset=0, map_y_offset=0)
  i <<- i + 1
})
# Scatterplots
plot.scatterbox(obs = stream_combined$obs, sim = stream_combined$sim, groups = stream_combined$group,
                xlab = 'Observed Streamflow (cfs)', ylab = 'Simulated Streamflow (cfs)', log = F)
plot.scatterbox(obs = stream_combined$obs, sim = stream_combined$sim, groups = stream_combined$group,
                xlab = 'Observed Streamflow (cfs)', ylab = 'Simulated Streamflow (cfs)', log = T)
dev.off()

#-------------------------------------------------------------------------------------------------#

# Pre-post Plots
pdf(file.path(out_dir,'FJ_pre_post.pdf'), width=11, height=8.5)
pre_title <- print.split_sample_stats(sfrcompare, 'Fort Jones', 'Pre')
post_title <- print.split_sample_stats(sfrcompare, 'Fort Jones', 'Post')
plot.pre_post_compare(dates = stream_combined[stream_combined$group == 'Fort Jones','Date'],
                      sim = stream_combined[stream_combined$group == 'Fort Jones','sim'],
                      obs = stream_combined[stream_combined$group == 'Fort Jones', 'obs'],
                      split_date = cal_split,
                      ylabel = 'Streamflow (cfs)',
                      log = 'y',
                      pre_title = pre_title,
                      post_title = post_title)
dev.off()

#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
# Streamflow Maps ------------------------------------------------------

# mf_dir = "C:/Users/Claire/Documents/GitHub/SVIHM/Scenarios/basecase"

# Read and process streamflow data - output from modflow
sfr_glob_text = readLines(file.path(mf_dir, "Streamflow_Global.dat"))
start_rows = grep("STREAM LISTING", sfr_glob_text) + 5 #one start for each stress period
n_reach = start_rows[2]-start_rows[1]-8  # 8 extra header rows at each timestep

colname_list = c("LAYER", "ROW","COL", "STREAM_SEG_NO", "RCH_NO", "FLOW_INTO_STRM_RCH",
                 "FLOW_TO_AQUIFER", "FLOW_OUT_OF_STRM_RCH","OVRLND_RUNOFF","DIRECT PRECIP",
                 "STREAM_ET","STREAM_HEAD", "STREAM_DEPTH", "STREAM_WIDTH",
                 "STREAMBED_CONDCTNC","STREAMBED_GRADIENT")

# Reading SFR data takes ~5 mins. Save to an .RDS file for convenience
if(!file.exists(file.path(out_dir, "sfr_reach_array.RDS"))){
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
  }
  # Save giant reach file as .Rdata
  # saveRDS(object=reach_array,file=file.path(plot_data_dir,"sfr_reach_array.Rdata"))
  saveRDS(object=reach_array,file=file.path(out_dir,"sfr_reach_array.rds"))
} else { reach_array = readRDS(file.path(out_dir,"sfr_reach_array.RDS"))}


# #Calculate breaks (for the legend and color coding)

# Breaks for flow
max(as.numeric(as.character(reach_array[,,8]))) # max flow out
# breaks_flow = c(10^(0:7))
# max(as.numeric(as.character(reach_array[,,13])))

# #calculate breaks for depth
# depth = as.numeric(as.character(reach_array[,,13]))
# depth_sorted = sort(depth)
# n_classes = 7
# n_breaks = n_classes+1
# breaks_index = round(1:n_breaks/n_breaks*length(depth))
# breaks_values = depth_sorted[breaks_index]
#
# depth_breaks_manual_7 = c(0,0.01, 0.05, 0.1, 0.2, 0.5, 1, 4.05)
# depth_breaks_manual_5 = c(0, 0.05, 0.1, 0.3, 4.05)

# #calculate breaks for flow out
# flowout = as.numeric(as.character(reach_array[,,8]))
# flowout_sorted = sort(flowout)
# n_classes = 7
# n_breaks = n_classes+1
# breaks_index = round(1:n_breaks/n_breaks*length(flowout))
# breaks_values = flowout_sorted[breaks_index]

flow_breaks_manual = c(0, 2.5, 20, 50, 100, 300, 700, 6350)*1000

#Set color palette
n_classes = 7
pal = rev(sequential_hcl(n_classes, palette = "ag_GrnYl"))



# Read in GIS data
seg = st_read(dsn = plot_data_dir, layer = "SFR_segments_sugar_pts")
seg = st_transform(seg, crs = st_crs(3310))

seg$row_col = paste(seg$row, seg$column, sep="_") # make identifier for each seg point
seg$flow_out = NA
seg$depth = NA
seg$color =NA

#read in Bulletin 118 groundwater basin boundary shapefile
basin = st_read(dsn = plot_data_dir, layer ="SGMA_B118_SV")
basin = st_transform(basin, crs = st_crs(3310))
#generate background color polygon
bg_poly = st_buffer(x = basin, dist=1e5)


#### Plot streamflow

#make a pdf appendix of each timestep of dry or wet

#make table of months and years for each stress period
stress_period_table = data.frame(stress_period=1:n_stress); sp_tab = stress_period_table
sp_tab$date = seq.Date(from = start_date, by = "month", length.out=n_stress)
sp_tab$month = month(sp_tab$date)
sp_tab$water_year = year(sp_tab$date); sp_tab$water_year[sp_tab$month>9] = year(sp_tab$date[sp_tab$month>9])+1

#to make a pdf appendix with each stress period plotted:
pdf(file.path(out_dir, "wet_dry_stream_flipbook.pdf"), width=8.5, height=11)
for(i in 1:length(start_rows)){

#to make a png figure with manually selected stress periods plotted
# png(file.path(out_dir, "wet_dry_stream_4yrs.png"),
#     width=7.5, height=16, units = "in", res=300)
# par(mfrow=c(4,1), mar = c(1,1,1,1))
# for(i in c(287, 323, 239, 299)){ #Aug of 2014 (wet), 2017 (dry), 2010, and 2015 (avg, spread and conc)

  stress_period_array = data.frame(reach_array[i,,])
  spa = stress_period_array
  title_text = paste(month.abb[sp_tab$month[i]],"of water year",sp_tab$water_year[i])
  #process matrix a bit
  colnames(spa)=colname_list
  spa$row_col = paste(spa$ROW, spa$COL, sep="_")
  spa$FLOW_OUT_OF_STRM_RCH = as.numeric(as.character(spa$FLOW_OUT_OF_STRM_RCH))
  spa$STREAM_DEPTH = as.numeric(as.character(spa$STREAM_DEPTH))

  # par(mfrow = c(1,2)) #if plotting both flow and depth

  # Flowrate
  seg$flow_out = spa$FLOW_OUT_OF_STRM_RCH[match(seg$row_col, spa$row_col)]
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
       col=pal[cut(na.omit(seg$flow_out), include.lowest=T,
                   breaks = flow_breaks_manual)])
  # generate legend labels and add legend
  legend_labels = paste(flow_breaks_manual[1:n_classes]/1000,
                        flow_breaks_manual[2:(n_classes+1)]/1000, sep="-")
  legend(x="bottomleft", fill = pal, title="Flow (1000 m3/day)",
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


#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
# Volumetric Budget - SWBM - Annual bar graph ------------------------------------------------------
# swbm_dir = run_dir
# swbm_dir_18 = "C:/Users/Claire/Documents/GitHub/SVIHM/Scenarios/basecase"
# SWBM_Terms = c('Precipitation', 'SW Irrigation', 'GW Irrigation', 'ET', 'Recharge', 'Storage')
# SWBM_colors = c('lightblue1',  'darkcyan', 'midnightblue', 'goldenrod','green4', 'black' )

# plot setup
SWBM_Terms = c('Precipitation', 'SW Irrigation', 'GW Irrigation', 'ET', 'Recharge', 'Runoff', 'Storage','Error')
SWBM_colors = c('lightblue1',  'darkcyan', 'blue4', 'goldenrod','green4', 'mistyrose','black','gray' )
# Read in and clean data frame
SWBM_Monthly_m3 = read.table(file.path(swbm_dir,'monthly_water_budget.dat'), header = T)
names(SWBM_Monthly_m3) = c('Month',SWBM_Terms)
SWBM_Monthly_m3$Month = seq.Date(from = origin_date+1, by = "month", length.out = n_stress)

if(WRITE_SW_BUDGET == TRUE){write.csv(x = SWBM_Monthly_m3, file = file.path(out_dir, "SWBM Monthly Budget.csv"), row.names=F, quote=F)}
n_stress = nrow(SWBM_Monthly_m3)

# process dates for plotting - aggregate to annual
SWBM_Monthly_m3$WY = year(SWBM_Monthly_m3$Month)
SWBM_Monthly_m3$WY[month(SWBM_Monthly_m3$Month)>9] = year(SWBM_Monthly_m3$Month[month(SWBM_Monthly_m3$Month)>9]) +1
SWBM_Monthly_m3$Month = format(seq(origin_date, by = "month", length.out = n_stress),'%b-%Y')
SWBM_Annual_m3 = aggregate(.~WY,SWBM_Monthly_m3[,!names(SWBM_Monthly_m3)%in% c('Month',"Stress_Period")], FUN = sum)
# write.csv(x = SWBM_Annual_m3, file=file.path(out_dir, "SWBM Annual Budget_2018.csv"), row.names = F, quote = F)
if(WRITE_SW_BUDGET == TRUE){write.csv(x = SWBM_Annual_m3, file=file.path(out_dir, "SWBM Annual Budget.csv"), row.names = F, quote = F)}

# melt data for plot
SWBM_Annual_m3_melt = melt(SWBM_Annual_m3, id.vars = 'WY')
SWBM_Annual_m3_melt$variable = factor(SWBM_Annual_m3_melt$variable, levels = SWBM_Terms)
SWBM_Annual_m3_melt = SWBM_Annual_m3_melt[order(SWBM_Annual_m3_melt$variable),]

# make plots in metric and TAF units
SWBM_Annual_Mm3_Plot = ggplot(SWBM_Annual_m3_melt, aes(x = WY, y = value/1E6)) +
  geom_bar(aes(fill = variable), position = "stack",
           stat = 'identity', color = 'black', width = 0.95, size = 0.1) +
  # scale_x_continuous(limits = range(SWBM_Annual_m3$WY),
                     #breaks = seq(1991,2011,by = 2),
                     # expand = c(0,0))  +
  scale_y_continuous(limits = c(-300,300), breaks = seq(-300,300,by = 100), expand = c(0,0)) +
  xlab('') +
  ylab(bquote('Volume ('*Mm^3*')')) +
  ggtitle('Soil Zone Annual Budget') +
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
        legend.key.height = unit(10,'pt'))

# ggsave(filename = file.path(out_dir, "Annual Budget Barplot.png"), plot = SWBM_Annual_Mm3_Plot)


SWBM_Annual_TAF_Plot = ggplot(SWBM_Annual_m3_melt, aes(x = WY, y = value*0.000810714/1000)) +
  geom_bar(aes(fill = variable), position = "stack", stat = 'identity', color = 'black', width = 0.95, size = 0.1) +
  scale_x_continuous(limits = c(1990.4,2011.6), breaks = seq(1991,2011,by = 2),expand = c(0,0))  +
  scale_y_continuous(limits = c(-300,300), breaks = seq(-300,300,by = 100), expand = c(0,0)) +
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
        legend.position = c(0.25, 0.95),
        legend.key = element_rect(fill = NA, color = NA),
        legend.background = element_rect(fill = NA, color = NA),
        legend.direction = 'horizontal',
        legend.text = element_text(size = 6, margin = margin(r=1,l=1, unit = 'pt')),
        legend.key.height = unit(10,'pt'))




# SWBM Budget troubleshooting 4/6/2023

# old_swbm_dir = "C:/Users/Claire/Documents/GitHub/SVIHM/Scenarios/basecase"
# new_swbm_dir = swbm_dir
#
# old_poly = read.csv(file.path(data_dir["ref_data_dir","loc"],"polygons_table_ref.csv"),
#                     header = T)
# new_poly = read.table(file.path(new_swbm_dir, "polygons_table.txt"), header = T)
#
# table(old_poly$SWBM_IRR)
# table(new_poly$SWBM_IRR)
#
# old_wb=read.table(file.path(old_swbm_dir, "monthly_water_budget.dat"), header = T)
# new_wb = read.table(file.path(new_swbm_dir, "monthly_water_budget.dat"), header = T)
# new_wb = new_wb[1:336,]
#
#
# stream_new_over_old = (new_wb$SW_Irr-old_wb$SW_Irr)/old_wb$SW_Irr
# gw_new_over_old = new_wb$GW_Irr/old_wb$GW_Irr
# summary(new_wb$GW_Irr); summary(old_wb$GW_Irr)
# summary(new_wb$SW_Irr); summary(old_wb$SW_Irr)
# summary(new_wb$Precip); summary(old_wb$Precip)

swbm_dir_18 = "C:/Users/Claire/Documents/GitHub/SVIHM/Scenarios/basecase"
SWBM_Monthly_m3_18 = read.table(file.path(swbm_dir_18,'monthly_water_budget.dat'), header = T)
summary(SWBM_Monthly_m3$Precip[1:nrow(SWBM_Monthly_m3_18)] / SWBM_Monthly_m3_18$Precip)
summary(SWBM_Monthly_m3$SW_Irr[1:nrow(SWBM_Monthly_m3_18)] / SWBM_Monthly_m3_18$SW_Irr)
hist(SWBM_Monthly_m3$GW_Irr[1:nrow(SWBM_Monthly_m3_18)] / SWBM_Monthly_m3_18$GW_Irr)
summary(SWBM_Monthly_m3$ET[1:nrow(SWBM_Monthly_m3_18)] / SWBM_Monthly_m3_18$ET)


#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
# Volumetric Budget - SWBM - Monthly bar graph ------------------------------------------------------

# plot setup
SWBM_Terms = c('Precipitation', 'SW Irrigation', 'GW Irrigation', 'ET', 'Recharge', 'Runoff', 'Storage','Error')
SWBM_colors = c('darkorchid1',  'darkcyan', 'midnightblue', 'goldenrod','green4', 'coral1','black', NA )
# Read in and clean data frame
SWBM_Monthly_m3 = read.table(file.path(swbm_dir,'monthly_water_budget.dat'), header = T)
names(SWBM_Monthly_m3) = c('Month',SWBM_Terms)
n_stress = nrow(SWBM_Monthly_m3)

# process dates for plotting
SWBM_Monthly_m3$Month = seq.Date(from = origin_date, by = "month", length.out = n_stress)
SWBM_Monthly_m3$Error=NULL
# SWBM_Monthly_m3_for_melt = SWBM_Monthly_m3[,colnames(SWBM_Monthly_m3!="Error")]
SWBM_Monthly_m3_melt = melt(SWBM_Monthly_m3, id.vars = 'Month')

# Set water year
pdf(file = file.path(out_dir, "Monthly Budget Plots.pdf"), width = 8.5, height = 11/2)
for(wy in 1991:2023){ # wy = 1995
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
dev.off()



#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
# Volumetric Budget - MF -------------------------------------------------------

mfnam <- rmf_read_nam(file.path(mf_dir,'SVIHM.nam'))
mfdis <- rmf_read_dis(file.path(mf_dir,'SVIHM.dis'), nam=mfnam)

bud <- rmf_read_budget(file.path(mf_dir,'SVIHM.lst'))

# Plot!
pdf(file.path(out_dir,'VBudget.pdf'), width=11, height=8.5)
# Over time
p <- rmf_plot(bud, dis = mfdis, type='bar')  # Hacky fake plot
pdates <- mftime2date(sp=1,ts=ggplot_build(p)$layout$panel_params[[1]]$x$breaks, origin_date)
p <- rmf_plot(bud, dis = mfdis, type='bar') + scale_x_continuous("nstp", labels = pdates)
print(p)
p <- rmf_plot(bud, dis = mfdis, type='bar', net=T) + scale_x_continuous("nstp", labels = pdates)
print(p)
# Error
p <- rmf_plot(bud, dis=mfdis, what='difference') + scale_x_continuous("nstp", labels = pdates)
print(p)
p <- rmf_plot(bud, dis=mfdis, what='discrepancy') + scale_x_continuous("nstp", labels = pdates)
print(p)
# Cumulative, gross and net
p <- rmf_plot(bud, dis = mfdis, timesteps=-1, what='cumulative')
print(p)
p <- rmf_plot(bud, dis = mfdis, timesteps=-1, what='cumulative', net=T) +
  geom_text(aes(label = value), color='black', vjust=-1.0)
print(p)
p <- rmf_plot(bud, dis = mfdis, type='bar', what='rates', fluxes=c('storage'), net=T) +
  scale_x_continuous("nstp", labels = pdates)
print(p)
p <- rmf_plot(bud, dis = mfdis, type='bar', what='cumulative', fluxes=c('storage'), net=T) +
     scale_x_continuous("nstp", labels = pdates)
print(p)
# Done
dev.off()

#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
# Change in Storage - MF and SWBM -------------------------------------------------------


# change_in_storage_figure = function(SWBM_Monthly_m3, MODFLOW_Monthly_m3,
#                                     wy_annotations = F, end_wy = 2018,
#                                     start_date = as.Date("1990-10-01"),end_date = as.Date("2018-09-30")){
#
#   SWBM_Monthly_Storage_Plot = function(SWBM_Monthly_m3, end_wy = 2018,
#                                        start_date = as.Date("1990-10-01"),end_date = as.Date("2018-09-30")){
#
#     ggplot(data = SWBM_Monthly_m3,
#            aes(x = as.Date(paste0(Month,'-01'),'%b-%Y-%d'),
#                # y = (-cumsum(SWBM_Monthly_m3$Storage)/1E6)-mean((-cumsum(SWBM_Monthly_m3$Storage)/1E6)))) +
#                y = (-cumsum(SWBM_Monthly_m3$Storage*m3_to_TAF))-mean((-cumsum(SWBM_Monthly_m3$Storage* m3_to_TAF))))) +
#       geom_vline(mapping=NULL, linetype = 2, size = 0.5, colour="gray", xintercept = seq(as.Date("1990-10-01"), as.Date("2018-10-01"), by = "5 year"))+
#       geom_hline(mapping=NULL, linetype = 2, size = 0.5, colour="gray", yintercept = seq(-20,20, by = 5))+
#       geom_hline(yintercept = 0, size = 0.25) +
#       geom_line(size = 0.5) +
#       geom_point(size = 0.75) +
#       scale_x_date(limits = c(as.Date('Oct-01-1990', format = '%b-%m-%y'),
#                               as.Date(paste0('Oct-01-',end_wy), format = '%b-%m-%y')),
#                    breaks = seq(as.Date("1990/10/1"), by = "2 years", length.out = nstress/12+1), expand = c(0,0),
#                    date_labels = ('%b-%y'))  +
#       scale_y_continuous(limits = c(-20,20), breaks = seq(-20,20,by = 5), expand = c(0,0)) +
#       ylab(ylab(bquote('Relative Soil Storage (TAF)'))) +
#       ggtitle('Monthly Relative Soil Storage') +
#       theme(panel.background = element_blank(),
#             panel.border = element_rect(fill=NA, color = 'black'),
#             axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
#             axis.text.y = element_text(size = 8),
#             axis.ticks = element_line(size = 0.2),
#             plot.title = element_text(hjust = 0.5, size = 10),
#             axis.title.x = element_blank(),
#             axis.title.y = element_text(size = 8)
#       )
#   }
#   MODFLOW_Monthly_Storage_Change = function(MODFLOW_Monthly_m3, end_wy = 2018,
#                                             start_date = as.Date("1990-10-01"),end_date = as.Date("2018-09-30")){
#     MODFLOW_Monthly_Storage_Change_Plot = ggplot(data = MODFLOW_Monthly_m3,
#                                                  aes(x = seq(start_date,as.Date(paste0(end_wy,'-09-30')),by = 'month'),
#                                                      y = -cumsum(Storage)*m3_to_TAF)) +
#       geom_vline(mapping=NULL, linetype = 2, size = 0.5, colour="gray", xintercept = seq(start_date, end_date, by = "5 year"))+
#       geom_hline(mapping=NULL, linetype = 2, size = 0.5, colour="gray", yintercept = seq(-50,50, by = 25))+
#       geom_hline(yintercept = 0, size = 0.25) +
#       geom_line(size = 0.5)  +
#       geom_point(size = 0.75) +
#       scale_x_date(limits = c(as.Date('Oct-01-1990', format = '%b-%m-%y'),
#                               as.Date(paste0('Oct-01-',end_wy), format = '%b-%m-%y')),
#                    breaks = seq(as.Date("1990/10/1"), by = "2 years", length.out = nstress/12+1), expand = c(0,0),
#                    date_labels = ('%b-%y'))  +
#       scale_y_continuous(limits = c(-50,50), breaks = seq(-50,50,by = 25), expand = c(0,0)) +
#       ylab(ylab(bquote('Relative Aquifer Storage (TAF)'))) +
#       ggtitle('Monthly Relative Aquifer Storage') +
#       theme(panel.background = element_blank(),
#             panel.border = element_rect(fill=NA, color = 'black'),
#             axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
#             axis.text.y = element_text(size = 8),
#             axis.ticks = element_line(size = 0.2),
#             plot.title = element_text(hjust = 0.5, size = 10),
#             axis.title.x = element_blank(),
#             axis.title.y = element_text(size = 8)
#       )
#     return(MODFLOW_Monthly_Storage_Change_Plot)
#   }
#
#   if(wy_annotations == T){
#     MODFLOW_Monthly_Storage_Change(SWBM_Monthly_m3)
#   } else {
#     grid.newpage()
#     pushViewport(viewport(layout = grid.layout(2,1)))
#     vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
#     print(SWBM_Monthly_Storage_Plot(SWBM_Monthly_m3, end_wy = 2018),vp = vplayout(1,1))
#     print(MODFLOW_Monthly_Storage_Change(MODFLOW_Monthly_m3, end_wy = 2018), vp = vplayout(2,1))
#
#   }

#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
# Heads/DTW ---------------------------------------------------------------

# Read in MF Files

if (create_sp_charts) {

  mfbas <- RMODFLOW::rmf_read_bas(file.path(mf_dir,'SVIHM.bas'), nam=mfnam, dis=mfdis)
  mfhds <- RMODFLOW::rmf_read_hed(file.path(mf_dir,'SVIHM.hds'), dis=mfdis)

  #-- Loop over SP plotting
  # Heads
  pdf(file.path(out_dir,'Head_Maps.pdf'), width=8.5, height=11)
  for (sp in 1:length(mfdis$perlen)) { #length(mfdis$perlen)) {
    ts <- mfdis$perlen[sp]
    message(paste('Writing: SP:'), sprintf("%03d", sp), '| TS:',sprintf("%02d", ts))

    # Find in time
    outdate <- mftime2date(sp, ts, origin_date)

    p <- rmf_plot(mfhds, dis=mfdis, bas=mfbas, k=1, kper=sp, kstp=ts, colour_palette = viridis, legend='Heads') +
         rmf_plot(mfhds, dis=mfdis, bas=mfbas, k=1, kper=sp, kstp=ts, type='contour', label=F, add=T) +
         ggplot2::ggtitle(paste('SVIHM |', outdate,'| SP: ', sp, '- TS:', ts))
    print(p)

  }
  dev.off()

  # Depth to Water (DTW)
  pdf(file.path(out_dir,'DTW_Maps.pdf'), width=8.5, height=11, onefile = T)
  for (sp in 1:length(mfdis$perlen)) {
    ts <- mfdis$perlen[sp]
    message(paste('DTW - Plotting: SP:'), sp, '| TS:',ts)

    # Find in time
    outdate <- mftime2date(sp, ts, origin_date)

    # Prepare
    l <- ifelse(sp == 1, 0, cumsum(mfdis$nstp)[sp-1]) + ifelse(sp < 0, dis$nstp[sp], ts)
    wt <- rmf_convert_hed_to_water_table(mfhds, l = l)
    dtw <- mfdis$top - wt

    p <- rmf_plot(dtw, dis=mfdis, bas=mfbas, k=1, kper=sp, kstp=ts, colour_palette = viridis, legend='Depth to Water') +
         rmf_plot(dtw, dis=mfdis, bas=mfbas, k=1, kper=sp, kstp=ts, type='contour', label=F, add=T) +
         ggplot2::ggtitle(paste('SVIHM |', outdate,'| SP: ', sp, '- TS:', ts))
    print(p)
  }
  dev.off()

  # Flooded Cells
  pdf(file.path(out_dir,'FloodedCells_Maps.pdf'), width=8.5, height=11, onefile = T)
  for (sp in 1:length(mfdis$perlen)) {
    ts <- mfdis$perlen[sp]
    message(paste('FC - Plotting: SP:'), sp, '| TS:',ts)

    # Find in time
    outdate <- mftime2date(sp, ts, origin_date)

    # Prepare
    l <- ifelse(sp == 1, 0, cumsum(mfdis$nstp)[sp-1]) + ifelse(sp < 0, dis$nstp[sp], ts)
    wt <- rmf_convert_hed_to_water_table(mfhds, l = l)
    fc <- wt - mfdis$top
    fc[fc < 0] <- 0

    p <- rmf_plot(fc, dis=mfdis, bas=mfbas, k=1, kper=sp, kstp=ts, colour_palette = inferno, legend='Water Depth Above Surface') +
      ggplot2::ggtitle(paste('SVIHM |', outdate,'| SP: ', sp, '- TS:', ts))

    print(p)
  }
  dev.off()

}
#-------------------------------------------------------------------------------------------------#


# Daily Out by Field Comparison -------------------------------------------

basecase_2018_dir = "C:/Users/Claire/Documents/GitHub/SVIHM/Scenarios/basecase"
basecase_dir = file.path(run_dir,"SWBM")

daily_out_lines = readLines(file.path(basecase_dir, "print_daily.txt"))
daily_out_lines = daily_out_lines[2:length(daily_out_lines)]# remove 1st info row
daily_out_items = unlist(strsplit(daily_out_lines, "  "))
daily_out_items = daily_out_items[!grepl(daily_out_items, pattern = "!")]# remove a comment with !
daily_out_df = as.data.frame(matrix(data = daily_out_items, ncol = 2, byrow =T))
colnames(daily_out_df) = c("poly_num", "descrip")
file_names = daily_out_df$descrip

poly_tab_basecase = read.table(file.path(basecase_dir, "polygons_table.txt"), header = T)

comp_dir = file.path('../../Scenarios', "_Comparison_Plots")

pdf(file = file.path(comp_dir, "swbm basecase comparison 16.pdf"), width = 8.5, height = 11)
for(i in 1:nrow(daily_out_df)){
  poly_num = daily_out_df$poly_num[i]
  irr = poly_tab_basecase$SWBM_IRR[poly_tab_basecase$SWBM_id==poly_num]
  wat_src = poly_tab_basecase$WATERSOURC[poly_tab_basecase$SWBM_id==poly_num]
  crop = poly_tab_basecase$SWBM_LU[poly_tab_basecase$SWBM_id==poly_num]
  file_name = daily_out_df$descrip[i]

  daily_2018 = read.table(file = file.path(basecase_2018_dir, paste0(file_name, "_daily_out.dat")),
                          header = T)
  daily_2018$month = seq.Date(from = start_date, length.out = nrow(daily_2018), by = "day")
  daily = read.table(file = file.path(basecase_dir, paste0(file_name, "_daily_out.dat")),
                     header = T)
  daily$month = seq.Date(from = start_date, length.out = nrow(daily), by = "day")
  daily = daily[daily$month<= max(daily_2018$month),]# crop to 2018 data

  # aet_diff = daily$aET - daily_2018$actualET
  # hist(aet_diff)

  par(mfrow = c(3,2))

  # cumulative pET plot
  plot(x = daily_2018$month, y = cumsum(daily_2018$ET), type = "l", lwd=3,
       main = paste0(file_name," cumulative ET; crop ", crop, "; Wat. Src.",wat_src , "; Irr ",irr), #xaxt="n",
       xlab = "Date",ylab="cumulative ET (m)")
 lines(daily$month, cumsum(daily$pET), col = "red", lwd=3)
 grid()
 # legend(x = "topleft", col=c("black","red"), lwd=2,
 #        legend = c("basecase 2018", "basecase dev"))

 # cumulative aET lines
 lines(x = daily_2018$month, y = cumsum(daily_2018$actualET), lwd=1,
      lty = 2, col = "darkgray")
 lines(daily$month, cumsum(daily$aET), col = "pink", lwd=1, lty = 2)
 grid()
 legend(x = "topleft", col=c("black","red", "darkgray", "pink"), lwd=c(3,3,1,1), lty = c(1,1,2,2),
        legend = c("basecase 2018 pET", "basecase dev pET", "basecase 2018 aET", "basecase dev aET"))
 # # cumulative aET plot
 # plot(x = daily_2018$month, y = cumsum(daily_2018$actualET), type = "l", lwd=2,
 #      main = paste0(file_name," cumulative aET"), #xaxt="n",
 #      xlab = "Date",ylab="cumulative aET (m)")
 # lines(daily$month, cumsum(daily$aET), col = "red", lwd=2)
 # grid()
 # legend(x = "topleft", col=c("black","red"), lwd=2,
 #        legend = c("basecase 2018", "basecase dev"))

 # cumulative precip plot
 plot(x = daily_2018$month, y = cumsum(daily_2018$precip_adj), type = "l", lwd=2,
      main = paste0(file_name," cumulative precip"), #xaxt="n",
      xlab = "Date",ylab="cumulative precip (m)")
 lines(daily$month, cumsum(daily$effective_precip), col = "red", lwd=2)
 grid()
 legend(x = "topleft", col=c("black","red"), lwd=2,
        legend = c("basecase 2018", "basecase dev"))


 # soil moisture tracker plot
 # plot(x = daily_2018$month, y = daily_2018$moisture, type = "l", lwd=3,
 #      main = paste0(file_name," soil moisture"), #xaxt="n",
 #      xlab = "Date",ylab="soil moisture (m)")
 plot(x = daily$month, y = daily$swc, type = "l", lwd=3, col = NA,
      main = paste0(file_name," soil moisture"), #xaxt="n",
      xlab = "Date",ylab="soil moisture (m)")
 lines(daily_2018$month, daily_2018$moisture, col = "black", lwd=3)
 lines(daily$month, daily$swc, col = "red", lwd=1)
 grid()
 legend(x = "bottomright", col=c("black","red"), lwd=c(3,1),
        legend = c("basecase 2018", "basecase dev"))

 # recharge plot
 plot(x = daily_2018$month, y = cumsum(daily_2018$rch), type = "l", lwd=2,
      main = paste0(file_name," recharge"), #xaxt="n",
      xlab = "Date",ylab="soil moisture (m)")
 lines(daily$month, cumsum(daily$rch), col = "red", lwd=2)
 grid()
 legend(x = "topleft", col=c("black","red"), lwd=2,
        legend = c("basecase 2018", "basecase dev"))



 # cumulative gw irr plot
 plot(x = daily_2018$month, y = cumsum(daily_2018$well), type = "l", lwd=2,
      main = paste0(file_name," cumulative GW Irr"), #xaxt="n",
      xlab = "Date",ylab="cumulative GW irr (m)")
 lines(daily$month, cumsum(daily$GW_irr), col = "red", lwd=2)
 grid()
 legend(x = "topleft", col=c("black","red"), lwd=2,
        legend = c("basecase 2018", "basecase dev"))


 # cumulative sw irr plot
 plot(x = daily_2018$month, y = cumsum(daily_2018$irrig - daily_2018$well),
      type = "l", lwd=2,
      main = paste0(file_name," cumulative SW irr"), #xaxt="n",
      xlab = "Date",ylab="cumulative SW irr (m)")
 lines(daily$month, cumsum(daily$SW_irrig), col = "red", lwd=2)
 grid()
 legend(x = "topleft", col=c("black","red"), lwd=2,
        legend = c("basecase 2018", "basecase dev"))


 if(abs(sum(daily_2018$actualET) - sum(daily$aET)) > 2){
   print(paste("more than 5 m diff in cum total for", file_name))
 }
}
dev.off()


