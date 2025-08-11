#Script for processing streamflow depletion analysis. 

#This script will do all of the processing on the cluster, and therefore needs to be located in the same folder as
#'Start_Streamflow_Depletion.sbatch'. SLURM array jobs are used to run the models, and the $SLURM_ARRAY_TASK_ID determines where
#the pumping wells are located.

#SLURM_ARRAY_TASK_ID = Integer value for model run associated with a specific cell
#nsegs = number of stream segments in model. Used for reading data from 'Global_Streamflow.dat'

# Script Initialization ---------------------------------------------------
library(dplyr)
source('MODFLOW_Budget.R')    #Function for extracting MODFLOW Budget
source('SVIHM_Global_Streamflow.R')
args = commandArgs(trailingOnly=TRUE)
SLURM_ARRAY_TASK_ID = as.numeric(args[1])
nsegs = as.numeric(args[2])
pumping_rate = as.numeric(args[3])

# User Input --------------------------------------------------------------
SFR_file = './SVIHM/SWBM_depletion_accretion/input/SVIHM_SFR_template.txt'

Stream_Network = read.table('./Basecase_out/Cal_4/SVIHM.sfr', skip = 1, nrows = nsegs)
Tailings_segs = c(1,3)
SR_segs = c(4,5,9,10,12,14,23,25,26,30)
Trib_segs = c(2,6,7,8,11,13,15,16,17,18,19,20,21,22,24,27,28,29)
names(Stream_Network) = c('lay', 'row', 'col', 'seg', 'reach', 'length', 'bed_z', 'slope', 'bed_thick', 'bed_k')

WB_Components_MODFLOW = c('STORAGE', 'CONSTANT_HEAD', 'WELLS', 'RECHARGE', 'ET_SEGMENTS','STREAM_LEAKAGE', 'DRAINS')
Streamflow_out_headers = c('lay', 'row', 'col', 'seg', 'reach', 'flow_in', 'flow2aq', 'flow_out', 'runoff',
                           'direct_precip', 'stream_et', 'head', 'depth', 'width', 'conductance', 'gradient')

if (pumping_rate<0) {
  cell_type = 'Depletion'   #Depletion if pumping rate is negative
} else {
  cell_type = 'Accretion'   #Accretion if pumping rate is positive
}

print('User Input Read')
# Import Basecase Files --------------------------------------------
print('Reading SFR File')
SFR_data = read.table(file = SFR_file, skip = 1, header = F)[,c(4,5,6)]
names(SFR_data) = c('seg', 'reach', 'length')

print('Reading Basecase Global Streamflow File')
#Global Streamflow for the last timestep in the stress period
Streamflow_BC = SVIHM_Global_Streamflow(filename = './Model_output/Basecase/Streamflow_Global.dat',
                                        nsegs = nsegs,
                                        start_date = as.Date('1990-10-01'),
                                        nstress = 252)
Streamflow_BC = Streamflow_BC[,!names(Streamflow_BC)%in%c('runoff', 'direct_precip', 'stream_et', 'head', 'conductance', 'gradient')]
print('Aggregating Basecase Global Streamflow Data')
#Calculate net global fluxes for all reaches for each stress period
Stream_fluxes_BC = aggregate(.~Month, data = subset(Streamflow_BC, select = c('flow_in','flow2aq','flow_out', 'Month')), FUN = sum)
names(Stream_fluxes_BC) = c('Month','global_net_flow_in' ,'global_net_flow2aq', 'global_net_flow_out')
Stream_Storage_BC = data.frame(global_storage_m3 = Streamflow_BC$depth * Streamflow_BC$width,
                               Month = Streamflow_BC$Month, stringsAsFactors = F)
Stream_Storage_BC = aggregate(.~Month,data = Stream_Storage_BC, FUN = sum)

#Calculate length of dry reaches
SR_segs_length_m = sum(subset(SFR_data, seg%in%SR_segs, select = 'length'))
Trib_segs_length_m = sum(subset(SFR_data, seg%in%Trib_segs, select = 'length'))
Stream_segs_length_m = sum(subset(SFR_data, select = 'length'))

#Main stem of Scott River (excluding tailings section)
dry_reaches_SR_BC = subset(Streamflow_BC, flow_out < 2446.58 & seg%in%SR_segs, select = c('seg', 'reach', 'flow_out', 'Month'))
dry_reaches_SR_BC = left_join(dry_reaches_SR_BC, Stream_Network, by = c('seg', 'reach'))
dry_reaches_SR_BC = aggregate(.~Month,data = dry_reaches_SR_BC, FUN = sum) %>% subset(select = c('length', 'Month'))
dry_reaches_SR_BC$length = dry_reaches_SR_BC$length/SR_segs_length_m
names(dry_reaches_SR_BC) = c('global_SR_dry_length_norm', 'Month')

#All Tributaries
dry_reaches_Trib_BC = subset(Streamflow_BC, flow_out < 2446.58 & seg%in%Trib_segs, select = c('seg', 'reach', 'flow_out', 'Month'))
dry_reaches_Trib_BC = left_join(dry_reaches_Trib_BC, Stream_Network, by = c('seg', 'reach'))
dry_reaches_Trib_BC = aggregate(.~Month,data = dry_reaches_Trib_BC, FUN = sum) %>% subset(select = c('length', 'Month'))
dry_reaches_Trib_BC$length = dry_reaches_Trib_BC$length/Trib_segs_length_m
names(dry_reaches_Trib_BC) = c('global_Trib_dry_length_norm', 'Month')

#Main stem of Scott River (excluding tailings section) and tributaries
dry_reaches_all_BC = subset(Streamflow_BC, flow_out < 2446.58 & (seg%in%SR_segs | seg%in%Trib_segs), select = c('seg', 'reach', 'flow_out', 'Month'))
dry_reaches_all_BC = left_join(dry_reaches_all_BC, Stream_Network, by = c('seg', 'reach'))
dry_reaches_all_BC = aggregate(.~Month,data = dry_reaches_all_BC, FUN = sum) %>% subset(select = c('length', 'Month'))
dry_reaches_all_BC$length = dry_reaches_all_BC$length/Stream_segs_length_m
names(dry_reaches_all_BC) = c('global_dry_length_norm', 'Month')

#Effects at specific reach (FJ gage: seg == 30 reach ==29)
Streamflow_BC_FJ_gage = subset(Streamflow_BC, seg==30 & reach==29)

#Add in global values for each stress period
Streamflow_BC_FJ_gage = left_join(Streamflow_BC_FJ_gage, Stream_fluxes_BC, by = 'Month') %>%
  left_join(Stream_Storage_BC, by = 'Month') %>%
  left_join(dry_reaches_SR_BC, by = 'Month') %>%
  left_join(dry_reaches_Trib_BC, by = 'Month') %>%
  left_join(dry_reaches_all_BC, by = 'Month')
Streamflow_BC_FJ_gage[is.na(Streamflow_BC_FJ_gage)] = 0   #Replace NA values for months with zero dry reaches

#Daily Streamflow Data
print('Reading Basecase Daily Streamflow Data at Fort Jones Gage')
Streamflow_FJ_daily_BC = read.table(file = './Model_output/Basecase/Streamflow_FJ_SVIHM.dat', skip = 2, header = F)[,c(1,3)]
names(Streamflow_FJ_daily_BC) = c('Date', 'flow_m3day')
Streamflow_FJ_daily_BC$Date = seq(as.Date('1990-10-01'), to = as.Date('2011-09-30'), by = 'day')

print('Reading Basecase Daily Streamflow Data at Above Serpa Lane Gage')
Streamflow_AS_daily_BC = read.table(file = './Model_output/Basecase/Streamflow_AS_SVIHM.dat', skip = 2, header = F)[,c(1,3)]
names(Streamflow_AS_daily_BC) = c('Date', 'flow_m3day')
Streamflow_AS_daily_BC$Date = seq(as.Date('1990-10-01'), to = as.Date('2011-09-30'), by = 'day') 

print('Reading Basecase Daily Streamflow Data at Below Youngs Dam Gage')
Streamflow_BY_daily_BC = read.table(file = './Model_output/Basecase/Streamflow_BY_SVIHM.dat', skip = 2, header = F)[,c(1,3)]
names(Streamflow_BY_daily_BC) = c('Date', 'flow_m3day')
Streamflow_BY_daily_BC$Date = seq(as.Date('1990-10-01'), to = as.Date('2011-09-30'), by = 'day') 

print('Reading Basecase Daily Streamflow Data at Lower Shackleford Creek Gage')
Streamflow_LS_daily_BC = read.table(file = './Model_output/Basecase/Streamflow_LS_SVIHM.dat', skip = 2, header = F)[,c(1,3)]
names(Streamflow_LS_daily_BC) = c('Date', 'flow_m3day')
Streamflow_LS_daily_BC$Date = seq(as.Date('1990-10-01'), to = as.Date('2011-09-30'), by = 'day') 

print('Reading Basecase Daily Streamflow Data at Prediction Location 2')
Streamflow_Pred_Loc_2_daily_BC = read.table(file = './Model_output/Basecase/Streamflow_Pred_Loc_2.dat', skip = 2, header = F)[,c(1,3)]
names(Streamflow_Pred_Loc_2_daily_BC) = c('Date', 'flow_m3day')
Streamflow_Pred_Loc_2_daily_BC$Date = seq(as.Date('1990-10-01'), to = as.Date('2011-09-30'), by = 'day') 

print('Reading Basecase Daily Streamflow Data at Prediction Location 3')
Streamflow_Pred_Loc_3_daily_BC = read.table(file = './Model_output/Basecase/Streamflow_Pred_Loc_3.dat', skip = 2, header = F)[,c(1,3)]
names(Streamflow_Pred_Loc_3_daily_BC) = c('Date', 'flow_m3day')
Streamflow_Pred_Loc_3_daily_BC$Date = seq(as.Date('1990-10-01'), to = as.Date('2011-09-30'), by = 'day')

print('Reading Basecase Daily Streamflow Data at Prediction Location 4')
Streamflow_Pred_Loc_4_daily_BC = read.table(file = './Model_output/Basecase/Streamflow_Pred_Loc_4.dat', skip = 2, header = F)[,c(1,3)]
names(Streamflow_Pred_Loc_4_daily_BC) = c('Date', 'flow_m3day')
Streamflow_Pred_Loc_4_daily_BC$Date = seq(as.Date('1990-10-01'), to = as.Date('2011-09-30'), by = 'day') 

print('Reading Basecase Daily Streamflow Data at Prediction Location 5')
Streamflow_Pred_Loc_5_daily_BC = read.table(file = './Model_output/Basecase/Streamflow_Pred_Loc_5.dat',skip = 2, header = F)[,c(1,3)]
names(Streamflow_Pred_Loc_5_daily_BC) = c('Date', 'flow_m3day')
Streamflow_Pred_Loc_5_daily_BC$Date = seq(as.Date('1990-10-01'), to = as.Date('2011-09-30'), by = 'day') 

print('Reading Basecase MODFLOW Fluxes')
#MODFLOW fluxes for last timestep in stress period for basecase condition
MF_Fluxes_BC = MODFLOW_Budget(filename = './Model_output/Basecase/SVIHM.lst',
                              mf_bud_terms = WB_Components_MODFLOW,
                              output_type = 'rate')

# Import Depletion/Accretion Files --------------------------------------------

#Global Streamflow for the last timestep in the stress period
print(paste('Reading',cell_type,'Global Streamflow File'))
Streamflow_pumping = SVIHM_Global_Streamflow(filename = paste0('./Model_output/',cell_type,'/Global_Streamflow/Streamflow_Global_',SLURM_ARRAY_TASK_ID,'.dat'),
                                             nsegs = nsegs, 
                                             start_date = as.Date('1990-10-01'), 
                                             nstress = 252)
Streamflow_pumping = Streamflow_pumping[,!names(Streamflow_pumping)%in%c('runoff', 'direct_precip', 'stream_et', 'head', 'conductance', 'gradient')]

#Calculate net global fluxes for all reaches for each stress period
print(paste('Aggregating',cell_type,'Global Streamflow File'))
Stream_fluxes_pumping = aggregate(.~Month, data = subset(Streamflow_pumping, select = c('flow_in','flow2aq','flow_out', 'Month')), FUN = sum)
names(Stream_fluxes_pumping) = c('Month','global_net_flow_in' ,'global_net_flow2aq', 'global_net_flow_out')
Stream_Storage_pumping = data.frame(global_storage_m3 = Streamflow_pumping$depth * Streamflow_pumping$width,
                                    Month = Streamflow_pumping$Month, stringsAsFactors = F)
Stream_Storage_pumping = aggregate(.~Month,data = Stream_Storage_pumping, FUN = sum)

#Calculate length of dry reaches
#Main stem of Scott River (excluding tailings section)
dry_reaches_SR_pumping = subset(Streamflow_pumping, flow_out < 2446.58 & seg%in%SR_segs, select = c('seg', 'reach', 'flow_out', 'Month'))
dry_reaches_SR_pumping = left_join(dry_reaches_SR_pumping, Stream_Network, by = c('seg', 'reach'))
dry_reaches_SR_pumping = aggregate(.~Month,data = dry_reaches_SR_pumping, FUN = sum) %>% subset(select = c('length', 'Month'))
dry_reaches_SR_pumping$length = dry_reaches_SR_pumping$length/SR_segs_length_m
names(dry_reaches_SR_pumping) = c('global_SR_dry_length_norm', 'Month')

#All tributaries
dry_reaches_Trib_pumping = subset(Streamflow_pumping, flow_out < 2446.58 & seg%in%Trib_segs, select = c('seg', 'reach', 'flow_out', 'Month'))
dry_reaches_Trib_pumping = left_join(dry_reaches_Trib_pumping, Stream_Network, by = c('seg', 'reach'))
dry_reaches_Trib_pumping = aggregate(.~Month,data = dry_reaches_Trib_pumping, FUN = sum) %>% subset(select = c('length', 'Month'))
dry_reaches_Trib_pumping$length = dry_reaches_Trib_pumping$length/Trib_segs_length_m
names(dry_reaches_Trib_pumping) = c('global_Trib_dry_length_norm', 'Month')

#Main stem of Scott River (excluding tailings section) and all tributaries
dry_reaches_all_pumping = subset(Streamflow_pumping, flow_out < 2446.58 & (seg%in%SR_segs | seg%in%Trib_segs), select = c('seg', 'reach', 'flow_out', 'Month'))
dry_reaches_all_pumping = left_join(dry_reaches_all_pumping, Stream_Network, by = c('seg', 'reach'))
dry_reaches_all_pumping = aggregate(.~Month,data = dry_reaches_all_pumping, FUN = sum) %>% subset(select = c('length', 'Month'))
dry_reaches_all_pumping$length = dry_reaches_all_pumping$length/Stream_segs_length_m
names(dry_reaches_all_pumping) = c('global_dry_length_norm', 'Month')

#Effects at specific reach (FJ gage: seg == 30 reach ==29)
Streamflow_pumping_FJ_gage = subset(Streamflow_pumping, seg==30 & reach==29)

#Add in global values for each stress period
Streamflow_pumping_FJ_gage = left_join(Streamflow_pumping_FJ_gage, Stream_fluxes_pumping, by = 'Month') %>%
  left_join(Stream_Storage_pumping, by = 'Month') %>%
  left_join(dry_reaches_SR_pumping, by = 'Month') %>%
  left_join(dry_reaches_Trib_pumping, by = 'Month') %>%
  left_join(dry_reaches_all_pumping, by = 'Month')
Streamflow_pumping_FJ_gage[is.na(Streamflow_pumping_FJ_gage)] = 0   #Replace NA values for months with zero dry reaches

#Daily Streamflow Data
print(paste('Reading',cell_type,'Daily Streamflow Data at Fort Jones Gage'))
Streamflow_FJ_daily_pumping = read.table(file = paste0('./Model_output/',cell_type,'/FJ_Streamflow/Streamflow_FJ_SVIHM_',SLURM_ARRAY_TASK_ID,'.dat'), skip = 2, header = F)[,c(1,3)]
names(Streamflow_FJ_daily_pumping) = c('Date', 'flow_m3day')
Streamflow_FJ_daily_pumping$Date = seq(as.Date('1990-10-01'), to = as.Date('2011-09-30'), by = 'day')

print(paste('Reading',cell_type,'Daily Streamflow Data at Above Serpa Lane Gage'))
Streamflow_AS_daily_pumping = read.table(file = paste0('./Model_output/',cell_type,'/AS_Streamflow/Streamflow_AS_SVIHM_',SLURM_ARRAY_TASK_ID,'.dat'), skip = 2, header = F)[,c(1,3)]
names(Streamflow_AS_daily_pumping) = c('Date', 'flow_m3day')
Streamflow_AS_daily_pumping$Date = seq(as.Date('1990-10-01'), to = as.Date('2011-09-30'), by = 'day') 

print(paste('Reading',cell_type,'Daily Streamflow Data at Below Youngs Dam Gage'))
Streamflow_BY_daily_pumping = read.table(file = paste0('./Model_output/',cell_type,'/BY_Streamflow/Streamflow_BY_SVIHM_',SLURM_ARRAY_TASK_ID,'.dat'), skip = 2, header = F)[,c(1,3)]
names(Streamflow_BY_daily_pumping) = c('Date', 'flow_m3day')
Streamflow_BY_daily_pumping$Date = seq(as.Date('1990-10-01'), to = as.Date('2011-09-30'), by = 'day') 

print(paste('Reading',cell_type,'Daily Streamflow Data at Lower Shackleford Creek Gage'))
Streamflow_LS_daily_pumping = read.table(file = paste0('./Model_output/',cell_type,'/LS_Streamflow/Streamflow_LS_SVIHM_',SLURM_ARRAY_TASK_ID,'.dat'), skip = 2, header = F)[,c(1,3)]
names(Streamflow_LS_daily_pumping) = c('Date', 'flow_m3day')
Streamflow_LS_daily_pumping$Date = seq(as.Date('1990-10-01'), to = as.Date('2011-09-30'), by = 'day') 

print(paste('Reading',cell_type,'Daily Streamflow Data at Prediciton Location 2'))
Streamflow_Pred_Loc_2_daily_pumping = read.table(file = paste0('./Model_output/',cell_type,'/Pred_Loc_2_Streamflow/Streamflow_Pred_Loc_2_',SLURM_ARRAY_TASK_ID,'.dat'), skip = 2, header = F)[,c(1,3)]
names(Streamflow_Pred_Loc_2_daily_pumping) = c('Date', 'flow_m3day')
Streamflow_Pred_Loc_2_daily_pumping$Date = seq(as.Date('1990-10-01'), to = as.Date('2011-09-30'), by = 'day') 

print(paste('Reading',cell_type,'Daily Streamflow Data at Prediciton Location 3'))
Streamflow_Pred_Loc_3_daily_pumping = read.table(file = paste0('./Model_output/',cell_type,'/Pred_Loc_3_Streamflow/Streamflow_Pred_Loc_3_',SLURM_ARRAY_TASK_ID,'.dat'), skip = 2, header = F)[,c(1,3)]
names(Streamflow_Pred_Loc_3_daily_pumping) = c('Date', 'flow_m3day')
Streamflow_Pred_Loc_3_daily_pumping$Date = seq(as.Date('1990-10-01'), to = as.Date('2011-09-30'), by = 'day')

print(paste('Reading',cell_type,'Daily Streamflow Data at Prediciton Location 4'))
Streamflow_Pred_Loc_4_daily_pumping = read.table(file = paste0('./Model_output/',cell_type,'/Pred_Loc_4_Streamflow/Streamflow_Pred_Loc_4_',SLURM_ARRAY_TASK_ID,'.dat'), skip = 2, header = F)[,c(1,3)]
names(Streamflow_Pred_Loc_4_daily_pumping) = c('Date', 'flow_m3day')
Streamflow_Pred_Loc_4_daily_pumping$Date = seq(as.Date('1990-10-01'), to = as.Date('2011-09-30'), by = 'day') 

print(paste('Reading',cell_type,'Daily Streamflow Data at Prediciton Location 5'))
Streamflow_Pred_Loc_5_daily_pumping = read.table(file = paste0('./Model_output/',cell_type,'/Pred_Loc_5_Streamflow/Streamflow_Pred_Loc_5_',SLURM_ARRAY_TASK_ID,'.dat'),skip = 2, header = F)[,c(1,3)]
names(Streamflow_Pred_Loc_5_daily_pumping) = c('Date', 'flow_m3day')
Streamflow_Pred_Loc_5_daily_pumping$Date = seq(as.Date('1990-10-01'), to = as.Date('2011-09-30'), by = 'day')

#MODFLOW fluxes for last timestep in stress period for pumping condition
print(paste('Reading',cell_type,'MODFLOW Fluxes'))
MF_Fluxes_pumping = MODFLOW_Budget(filename = paste0('./Model_output/',cell_type,'/LST_Files/SVIHM_',SLURM_ARRAY_TASK_ID,'.lst'),
                                   mf_bud_terms = WB_Components_MODFLOW,
                                   output_type = 'rate')

# Calculate Pumping-Basecase Values (Monthly) -------------------------------------------
print(paste('Calculating Flux Differences at',cell_type,'Cell'))
Pumping_BC_Diffs_Monthly = data.frame(Month = MF_Fluxes_BC$Month,
                                      BC_FJ_flow_out = Streamflow_BC_FJ_gage$flow_out,
                                      delta_S = MF_Fluxes_pumping$STORAGE_net_m3day - MF_Fluxes_BC$STORAGE_net_m3day,
                                      delta_pumping = MF_Fluxes_pumping$WELLS_net_m3day - MF_Fluxes_BC$WELLS_net_m3day,
                                      delta_ET = MF_Fluxes_pumping$ET_SEGMENTS_net_m3 - MF_Fluxes_BC$ET_SEGMENTS_net_m3,
                                      delta_Drns = MF_Fluxes_pumping$DRAINS_net_m3 - MF_Fluxes_BC$DRAINS_net_m3,
                                      delta_gage_Q = Streamflow_pumping_FJ_gage$flow_out - Streamflow_BC_FJ_gage$flow_out,
                                      delta_GWSW = Streamflow_pumping_FJ_gage$global_net_flow2aq - Streamflow_BC_FJ_gage$global_net_flow2aq,
                                      global_BC_SR_dry_Length_norm = Streamflow_BC_FJ_gage$global_SR_dry_length_norm,
                                      global_BC_Trib_dry_Length_norm = Streamflow_BC_FJ_gage$global_Trib_dry_length_norm,
                                      global_BC_dry_Length_norm = Streamflow_BC_FJ_gage$global_dry_length_norm,
                                      delta_global_SR_dry_length_norm = Streamflow_pumping_FJ_gage$global_SR_dry_length_norm - Streamflow_BC_FJ_gage$global_SR_dry_length_norm,
                                      delta_global_Trib_dry_length_norm = Streamflow_pumping_FJ_gage$global_Trib_dry_length_norm - Streamflow_BC_FJ_gage$global_Trib_dry_length_norm,
                                      delta_global_dry_length_norm = Streamflow_pumping_FJ_gage$global_dry_length_norm - Streamflow_BC_FJ_gage$global_dry_length_norm)

#Mass Balance relative to pumping rate
Pumping_BC_Diffs_Monthly$Mass_Balance_pct = abs((Pumping_BC_Diffs_Monthly$delta_GWSW + Pumping_BC_Diffs_Monthly$delta_ET + 
                                               Pumping_BC_Diffs_Monthly$delta_Drns + Pumping_BC_Diffs_Monthly$delta_S + 
                                               Pumping_BC_Diffs_Monthly$delta_pumping)/pumping_rate)*100
MB_Fail = which(Pumping_BC_Diffs_Monthly$Mass_Balance_pct>5)
Pumping_BC_Diffs_Monthly[MB_Fail,c(-1,-2,-9,-10,-11,-15)] = NaN  #Remove depletion/accretion data for months where the mass balance error is greater than 5% of the pumping rate

# Calculate Pumping-Basecase Values (Daily) -------------------------------------------
Pumping_BC_Diffs_Daily = data.frame(Date = Streamflow_FJ_daily_pumping$Date,
                                    FJ_Diff_m3day = Streamflow_FJ_daily_pumping$flow_m3day - Streamflow_FJ_daily_BC$flow_m3day,
                                    AS_Diff_m3day = Streamflow_AS_daily_pumping$flow_m3day - Streamflow_AS_daily_BC$flow_m3day,
                                    BY_Diff_m3day = Streamflow_BY_daily_pumping$flow_m3day - Streamflow_BY_daily_BC$flow_m3day,
                                    LS_Diff_m3day = Streamflow_LS_daily_pumping$flow_m3day - Streamflow_LS_daily_BC$flow_m3day,
                                    Pred_Loc_2_Diff_m3day = Streamflow_Pred_Loc_2_daily_pumping$flow_m3day - Streamflow_Pred_Loc_2_daily_BC$flow_m3day,
                                    Pred_Loc_3_Diff_m3day = Streamflow_Pred_Loc_3_daily_pumping$flow_m3day - Streamflow_Pred_Loc_3_daily_BC$flow_m3day,
                                    Pred_Loc_4_Diff_m3day = Streamflow_Pred_Loc_4_daily_pumping$flow_m3day - Streamflow_Pred_Loc_4_daily_BC$flow_m3day,
                                    Pred_Loc_5_Diff_m3day = Streamflow_Pred_Loc_5_daily_pumping$flow_m3day - Streamflow_Pred_Loc_5_daily_BC$flow_m3day)

# Output ------------------------------------------------------------------
#Monthly Differences
print('Writing Monthly Output to File')
write.table(x = Pumping_BC_Diffs_Monthly, file = paste0('./',cell_type,'_Results/Monthly_Differences_',SLURM_ARRAY_TASK_ID,'.dat'), 
            quote = F, append = F,col.names = T, row.names = F)    
#Daily Differences
print('Writing Daily Output to File')
write.table(x = Pumping_BC_Diffs_Daily, file = paste0('./',cell_type,'_Results/Daily_Differences_',SLURM_ARRAY_TASK_ID,'.dat'),
            quote = F, append = F,col.names = T, row.names = F)

#Basecase flows for stream segments (for plotting in ArcMap)
if (SLURM_ARRAY_TASK_ID == 0){
  Dates = format(seq(as.Date('1990-10-01'),length.out = 252, by = 'month'),'%b-%Y')
  for (i in 1:252){
    temp = subset(Streamflow_BC, Month==Dates[i])
    temp$rowcol = paste0(temp$row,temp$col)
    if (i==1){
      flow_out = subset(temp, select = c('rowcol','flow_out'))
    } else{
      flow_out = cbind(flow_out,temp$flow_out)
    }
  }
  
  flow_out[,-1] = round(flow_out[,-1],digits = 0)
  names(flow_out) = c('rowcol', Dates)
  write.table(file = './Basecase_Seg_Flows.txt', x = flow_out, append = F, col.names = T, row.names = F, quote = F)
}