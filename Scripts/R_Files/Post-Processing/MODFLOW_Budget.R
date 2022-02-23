#Function for extracting water budget terms from MODFLOW listing file
#filename is a string containing the name of the listing file (e.g., 'SVIHM.lst')
#mf_bud_terms is an array of strings that contains the different budget terms contained in the listing file
#suffix is an optional string tag added to saved dataframes for differentiation (e.g., MAR, ILR, etc.)
MODFLOW_Budget = function(filename, mf_bud_terms, suffix){
  library(magrittr)
  InputText = readLines(filename)  #Read in text file
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
  SP = as.numeric(sapply(lapply(strsplit(InputText[Timestep_SP_Lines],' '),function(x){x[!x ==""]}),"[[",15))  #Extract stress period number for printed budget
  extra_rows = which(duplicated(SP))
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
  MODFLOW_Budget_Monthly = data.frame(Month = format(seq(as.Date('1990/10/1'), as.Date('2011/9/30'), by = 'month'),'%b-%Y'),
                                       Water_Year = rep(seq(1991,2011),each=12),
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