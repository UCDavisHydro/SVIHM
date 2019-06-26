rm(list=ls())  #Clear workspace
args = commandArgs(trailingOnly=TRUE)

# test if there is at least one argument: if not, return an error
if (length(args)==0) {
  stop("Filename argument must be supplied (LST file)\n", call.=FALSE)
} 
nstress = 252
StartingMonths = seq(as.Date("1990/10/1"), by = "month", length.out = 253)
num_days = diff(as.numeric(StartingMonths))
LST_Name = args[1]
InputText = readLines(LST_Name)  #Read in text file

# Extract Convergence Failures
conv_fail_lines = grep('FAILED TO MEET',InputText)  #find stress periods where convergence failed
conv_fails=InputText[conv_fail_lines]

#Find lines where values are printed (including addition budget prints for time steps where solution did not converge)
DRAINS_Lines = grep('DRAINS = ',InputText)

n_budget_entries = length(DRAINS_Lines)

DRAINS_cumulative_in = as.numeric(sapply(lapply(strsplit(InputText[DRAINS_Lines],' '),function(x){x[!x ==""]}),"[[",3)[seq(1,n_budget_entries,2)]) #Cumulative In
DRAINS_cumulative_out = as.numeric(sapply(lapply(strsplit(InputText[DRAINS_Lines],' '),function(x){x[!x ==""]}),"[[",3)[seq(2,n_budget_entries,2)]) #Cumulative Out
DRAINS_cumulative_net = DRAINS_cumulative_in - DRAINS_cumulative_out
DRAINS_TS_Flux_in = as.numeric(sapply(lapply(strsplit(InputText[DRAINS_Lines],' '),function(x){x[!x ==""]}),"[[",6)[seq(1,n_budget_entries,2)]) #Cumulative In
DRAINS_TS_Flux_out = as.numeric(sapply(lapply(strsplit(InputText[DRAINS_Lines],' '),function(x){x[!x ==""]}),"[[",6)[seq(2,n_budget_entries,2)]) #Cumulative Out

Timestep_SP_Lines = grep('VOLUMETRIC BUDGET FOR ENTIRE MODEL AT END OF TIME STEP',InputText)
TS = as.numeric(sapply(lapply(strsplit(InputText[Timestep_SP_Lines],' '),function(x){x[!x ==""]}),"[[",11))  #Extract timestep number for printed budget
SP = as.numeric(sapply(lapply(strsplit(InputText[Timestep_SP_Lines],' '),function(x){x[!x ==""]}),"[[",15))  #Extract stress period number for printed budget

extra_rows = which(duplicated(SP))

#Remove stress periods that didn't converge if there are any
if(length(extra_rows>0)){
TS = TS[-extra_rows]
SP = SP[-extra_rows]
DRAINS_cumulative_in = DRAINS_cumulative_in[-extra_rows]
DRAINS_cumulative_out = DRAINS_cumulative_out[-extra_rows]
DRAINS_TS_Flux_in = DRAINS_TS_Flux_in[-extra_rows]
DRAINS_TS_Flux_out = DRAINS_TS_Flux_out[-extra_rows]
}

#Convert cumulative volumes (m^3) for each stress period
DRAINS_SP_Vol_in = c(DRAINS_cumulative_in[1],diff(DRAINS_cumulative_in))
DRAINS_SP_Vol_out = c(DRAINS_cumulative_out[1],diff(DRAINS_cumulative_out))

#Calcualte net volume (m^3) for each stress period
DRAINS_SP_Vol_net = DRAINS_SP_Vol_in - DRAINS_SP_Vol_out


#Calcualte net volume (TAF) for each stress period
DRAINS_SP_Vol_net_TAF = DRAINS_SP_Vol_net * 0.000000810714

#Calculate net flux rate for timestep at end of stress period
DRAINS_TS_Flux_net = DRAINS_TS_Flux_in - DRAINS_TS_Flux_out

#CALCULAUTE AVERAGE MONTHLY FLUXES (m^3/day)
DRAINS_Flux_in_monthly_avg = DRAINS_SP_Vol_in / TS
DRAINS_Flux_out_monthly_avg = DRAINS_SP_Vol_out / TS
DRAINS_Flux_net_monthly_avg = DRAINS_SP_Vol_net / TS

DRAINS_SP_rate = (DRAINS_SP_Vol_net/num_days)*-1

write.table(DRAINS_SP_rate, file = 'Drains_m3day.txt', row.names = F, quote = F, col.names = 'Drain flow rate (m^3)')
if (exists('DRAINS_SP_rate')){
  print('Update of drain flow input file SUCCEEDED')
} else {
  print('Update of drain flow input file FAILED')
}