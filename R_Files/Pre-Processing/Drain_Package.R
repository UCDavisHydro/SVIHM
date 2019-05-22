#Script that writes drain package input file for discahrge zone cells in SVIHMv3.1
rm(list=ls())  #Clear workspace
NSP = 252
setwd('C:/Users/dtolley/Scott_Valley/Modeling/100m_Grid/SVIHMv3.1/Flooding_Fix_No_ET_Drains/')

DZ_Cells = read.table('C:/Users/dtolley/Scott_Valley/Modeling/100m_Grid/SVIHMv3.1/ET_Cells_Discharge_Zone.txt', header = T, sep = ',')
Model_Surface = matrix(t(read.table('../Layer_1_top_z.txt')),nrow = 440, ncol = 210, byrow = T)
Elevation = matrix(NaN,length(DZ_Cells$row))
for (i in 1:length(DZ_Cells$row)){
  Elevation[i] = Model_Surface[DZ_Cells$row[i],DZ_Cells$column[i]]
}
Conductance = matrix(10000,length(DZ_Cells$row))
Layer = matrix(1,length(DZ_Cells$row))
Drains = cbind(Layer, DZ_Cells$row, DZ_Cells$column, round(Elevation,2), Conductance)
rep_drains = matrix(-1, 251)   # repeat value of -1 for n-1 Stress periods to resuse drains specified in first stress period
write('# MODFLOW Drain Package File - Drains applied at land surface within discharge zone',file = 'SVIHM.drn', append = F)
# write('PARAMETER  0  0', file = 'SVIHMv3.1.drn', append = T) #MXACTD IDRNCB
write('        2869        50', file = 'SVIHMv3.1.drn', append = T) #MXACTD IDRNCB
for (i in 1:NSP){
  write(paste('         2869         0                      Stress Period',i), file = 'SVIHM.drn', append = T, sep = NA)  #ITMP  NP
  cat(sprintf(' '), file = 'SVIHM.drn', append = T)
  cat(sprintf("%10i%10i%10i%10.2f%10.3e\n", Drains[,1], Drains[,2],Drains[,3],Drains[,4],Drains[,5]), file = 'SVIHM.drn', append = T)
}
