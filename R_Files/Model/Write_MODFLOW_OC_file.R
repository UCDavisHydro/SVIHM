# General Comments --------------------------------------------------------

# Writes output control (*.oc) file that controls when data are exported from a MODFLOW run
#Currently configured for monthly stress periods with daily timesteps

# Script Initialization ---------------------------------------------------
rm(list=ls())  #Clear workspace

# User Input --------------------------------------------------------------
nstress = 252
Starting_Date = as.Date('1990-10-01')
Months = seq(Starting_Date, length.out = nstress, by = 'month')
Print_Months = grepl(x = format(seq(Starting_Date, length.out = nstress, by = 'month'),'%b'), pattern = 'Aug|Sep|Oct')
nDays = diff(as.numeric(seq(Starting_Date, length.out = nstress+1, by = 'month')))
             
# Write OC file -------------------------------------------------------
write(x = '  HEAD SAVE UNIT 30', file = 'SVIHM_AugSepOct.oc', append = F)
write(x = '  HEAD PRINT FORMAT 0', file = 'SVIHM_AugSepOct.oc', append = T)
write(x = '  DRAWDOWN SAVE UNIT 31', file = 'SVIHM_AugSepOct.oc', append = T)
write(x = '  DRAWDOWN PRINT FORMAT 0', file = 'SVIHM_AugSepOct.oc', append = T)
write(x = '  COMPACT BUDGET AUX', file = 'SVIHM_AugSepOct.oc', append = T)
for (i in 1:nstress){
  for (j in 1:nDays[i]){
    write(x = paste0('  PERIOD ',i,'  STEP ',j), file = 'SVIHM_AugSepOct.oc', append = T)
    if (j==nDays[i] && Print_Months[i]==TRUE){
      write(x = '     SAVE BUDGET', file = 'SVIHM_AugSepOct.oc', append = T)
      write(x = '     PRINT BUDGET', file = 'SVIHM_AugSepOct.oc', append = T)
    }
  }
}

