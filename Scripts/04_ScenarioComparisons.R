library(RSVP)
library(viridis)
library(RMODFLOW)
library(ggplot2)
library(colorspace)
library(magrittr)

#-------------------------------------------------------------------------------------------------#
# Settings ----------------------------------------------------------------

origin_date <- as.Date('1990-09-30')

create_sp_charts = FALSE  # Many SPs, very slow

# Lists of scenarios
scen_names <- c('Basecase', 'Natveg', 'PRMS')
rel_dir <- '../..'
scen_dirs <- file.path(rel_dir, c('Run_baseupdate', 'Run_natveg', 'Run_prms'))

update_dir <- latest_dir(data_dir['update_dir','loc'])
plot_data_dir <- file.path('../../SVIHM_Input_Files/reference_data_for_plots/')

out_dir = data_dir["scenario_dir","loc"] #file.path("../../")

if (!dir.exists(out_dir)) {dir.create(out_dir, recursive = T)}

# info from svihm.swbm
rep_swbm_file <- file.path(scen_dirs[1],'SWBM',"svihm.swbm")

discretization_settings <- read_swbm_block(rep_swbm_file, block_name = "DISCRETIZATION")
wy_start = discretization_settings[['WYSTART']]
n_stress = discretization_settings[['NMONTHS']]

m3day_to_cfs = 1 * 35.3147 * 1/(60*60*24)

#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
# Read in Data ------------------------------------------------------------

#-- Observed FJ
fj_obs <- read.csv(file.path(update_dir, list.files(update_dir, pattern = 'FJ (USGS 11519500)*')),
                   stringsAsFactors = F)
fj_obs$Date <- as.Date(fj_obs$Date)

#-- Modeled FJ
sfr_locs <- read.csv(file.path(data_dir['ref_data_dir','loc'], 'sfr_gages.csv'),
                     row.names=1, stringsAsFactors = F)
fj_sim <- list()
for (i in 1:length(scen_names)) {
  fj_sim[[i]] <- import_sfr_gauge(file.path(scen_dirs[i], 'MODFLOW/Streamflow_FJ_SVIHM.dat'), origin_date = origin_date)
}
#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#

# FJ Flow Comparison Dates
plot_start_date <- as.Date('2020-10-01')
plot_end_date <-  as.Date('2021-12-31')

# Subset dates...
fj_obs_sub <- subset.DateTwoSided(fj_obs, start = plot_start_date, end = plot_end_date, date_col = 'Date')
fj_sim_sub <- lapply(fj_sim, subset.DateTwoSided, start = plot_start_date, end = plot_end_date, date_col = 'Date')

# Simulation Plot colors
simcolors <- c('black','green3', 'orangered3')

#-- Timeseries Plot
#par(mar=c(4,4,2,1))  #bottom, left, top, right
plot.ts_setup(fj_obs_sub$Date, fj_obs_sub$Flow,
              interval = 'month',
              xlabel='Calendar Year Date',
              ylabel='Flow (cfs)',
              log='y')
lines(fj_obs$Date, fj_obs$Flow, col='dodgerblue2', lwd = 2)
for (i in 1:length(scen_names)) {
  lines(fj_sim_sub[[i]]$Date, fj_sim_sub[[i]]$Flow_cfs, col=simcolors[i], lwd = 2)
}

# Add a base R legend
legend("topleft", legend = c("FJ Observed", scen_names),
       col = c("dodgerblue2", simcolors), lwd = 2, bty = "n", y.intersp = 1.2)

#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
# Export Flows to view in SVIHM_Analysis.exe
shortest <- min(unlist(lapply(fj_sim, nrow)))
fj_comb <- do.call(cbind, lapply(fj_sim, function(df) {df[1:shortest,c('Date','Flow_cfs')]}))
names(fj_comb) <- paste0(c('Date_',''), rep(scen_names, rep(2,length(scen_names))))
names(fj_comb)[1] <- 'Date'
write.csv(fj_comb[,c('Date',scen_names)], file.path(out_dir,'scen_combined_fj.csv'), row.names = F)

#-------------------------------------------------------------------------------------------------#


#-------------------------------------------------------------------------------------------------#
# Budget comparison (only can do two models at a time)
models_to_compare <- c(1,2)
mf_budgets <- lapply(models_to_compare, function(x) {
  mfnam <- rmf_read_nam(file.path(scen_dirs[x],'MODFLOW/SVIHM.nam'))
  mfdis <- rmf_read_dis(file.path(scen_dirs[x],'MODFLOW/SVIHM.dis'), nam=mfnam)
  bud <- rmf_read_budget(file.path(scen_dirs[x],'MODFLOW/SVIHM.lst'))
  return(bud)
})

