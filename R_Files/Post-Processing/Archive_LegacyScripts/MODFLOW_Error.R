#Function for extracting mass balance error from MODFLOW listing file
#filename is a string containing the name of the listing file (e.g., 'SVIHM.lst')
#suffix is an optional string tag added to saved dataframes for differentiation (e.g., MAR, ILR, etc.)
rm(list=ls())
library(ggplot2)
library(reshape2)
library(grid)
library(magrittr)

MODFLOW_Error = function(filename, suffix){
  InputText = readLines(filename)  #Read in text file
  # Extract Convergence Failures
  conv_fail_lines = grep('FAILED TO MEET',InputText)  #find stress periods where convergence failed
  conv_fails=InputText[conv_fail_lines]
  #Find lines where values are printed (including addition budget prints for time steps where solution did not converge
  TOTAL_In_Lines = grep('TOTAL IN =',InputText)
  TOTAL_Out_Lines = grep('TOTAL OUT =',InputText)
  n_budget_entries = length(TOTAL_In_Lines)
  
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
  #Remove stress periods that didn't converge if there are any
  if(length(extra_rows>0)){           
    TS = TS[-extra_rows]
    SP = SP[-extra_rows]
    TOTAL_cumulative_in = TOTAL_cumulative_in[-extra_rows]                      
    TOTAL_cumulative_out = TOTAL_cumulative_out[-extra_rows]                             
    TOTAL_TS_Flux_in = TOTAL_TS_Flux_in[-extra_rows]                                      
    TOTAL_TS_Flux_out = TOTAL_TS_Flux_out[-extra_rows]                                    
  }
    #Net Cumulative Fluxes
    TOTAL_cumulative_net = TOTAL_cumulative_in - TOTAL_cumulative_out
    # Total Inflow Volume for each Stress Period
    TOTAL_SP_Vol_in = c(TOTAL_cumulative_in[1],diff(TOTAL_cumulative_in))
    # Total Outflow Volume for each Stress Period
    TOTAL_SP_Vol_out = c(TOTAL_cumulative_out[1],diff(TOTAL_cumulative_out))
    # Net Volume for each Stress Period
    TOTAL_SP_Vol_net = TOTAL_SP_Vol_in - TOTAL_SP_Vol_out
    # Net flux rate at the end of each time step
    TOTAL_TS_Flux_net = TOTAL_TS_Flux_in - TOTAL_TS_Flux_out
  #Calculate Mass Balance
  Cumulative_Mass_Balance_percent_diff = ((TOTAL_cumulative_in - TOTAL_cumulative_out)/((TOTAL_cumulative_in + TOTAL_cumulative_out)/2))*100
  SP_Mass_Balance_percent_diff = ((TOTAL_SP_Vol_in - TOTAL_SP_Vol_out)/((TOTAL_SP_Vol_in + TOTAL_SP_Vol_out)/2))*100
  TS_Mass_Balance_percent_diff = ((TOTAL_TS_Flux_in - TOTAL_TS_Flux_out)/((TOTAL_TS_Flux_in + TOTAL_TS_Flux_out)/2))*100
  return(list(Cumulative = Cumulative_Mass_Balance_percent_diff, SP = SP_Mass_Balance_percent_diff, TS = TS_Mass_Balance_percent_diff))
}

filenames = list.files(pattern = '.lst')
MF_Error = lapply(X = filenames, FUN = function(x) MODFLOW_Error(x, suffix = 'basecase'))

#Cumulative_Error = as.data.frame(sapply(MF_Error,"[[",'Cumulative'))
#Cumulative_Error$SP = seq(1,252)
Cumulative_Error_max = apply(sapply(MF_Error,"[[",'Cumulative'),1,max)
Cumulative_Error_min = apply(sapply(MF_Error,"[[",'Cumulative'),1,min)
Cumulative_Error_min_max = data.frame(SP = seq(1,252), Max_Error = Cumulative_Error_max, Min_Error = Cumulative_Error_min)
Cumulative_Error_min_max_melt = melt(Cumulative_Error_min_max,id.vars = 'SP')
#Cumulative_Error_melt = melt(Cumulative_Error,id.vars = 'SP')
#SP_Error = as.data.frame(sapply(MF_Error,"[[",'SP'))
#SP_Error$SP = seq(1,252)
SP_Error_max = apply(sapply(MF_Error,"[[",'SP'),1,max)
SP_Error_min = apply(sapply(MF_Error,"[[",'SP'),1,min)
SP_Error_min_max = data.frame(SP = seq(1,252), Max_Error = SP_Error_max, Min_Error = SP_Error_min)
SP_Error_min_max_melt = melt(SP_Error_min_max,id.vars = 'SP')
#SP_Error_melt = melt(SP_Error, id.vars = 'SP')
#TS_Error = as.data.frame(sapply(MF_Error,"[[",'SP'))
#TS_Error$SP = seq(1,252)
TS_Error_max = apply(sapply(MF_Error,"[[",'TS'),1,max)
TS_Error_min = apply(sapply(MF_Error,"[[",'TS'),1,min)
TS_Error_min_max = data.frame(SP = seq(1,252), Max_Error = TS_Error_max, Min_Error = TS_Error_min)
TS_Error_min_max_melt = melt(TS_Error_min_max,id.vars = 'SP')
#TS_Error_melt = melt(TS_Error, id.vars = 'TS')

pdf(paste0('Cumulative_Error_min_max.pdf'), width = 6, height = 4)
(Cumulative_Error_plot = ggplot(data = Cumulative_Error_min_max_melt, aes(x = SP,y = value, group = variable, color = variable)) + geom_line())
graphics.off()

pdf(paste0('SP_Error_min_max.pdf'), width = 6, height = 4)
(SP_Error_plot = ggplot(data = SP_Error_min_max_melt, aes(x = SP,y = value, group = variable, color = variable)) + geom_line())
graphics.off()

pdf(paste0('TS_Error_min_max.pdf'), width = 6, height = 4)
(TS_Error_plot = ggplot(data = TS_Error_min_max_melt, aes(x = SP,y = value, group = variable, color = variable)) + geom_line())
graphics.off()
