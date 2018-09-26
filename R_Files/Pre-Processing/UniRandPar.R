#UniRandPar.R - R script that generates random uniform distributions of parameters from multiple calibrations
rm(list=ls())
num_cals = 5
options(warn=-1)   #suppress warnings that are created when writing UCODE parameter files
param_template = read.table('C:/Users/dtolley/Scott_Valley/Modeling/100m_Grid/SVIHMv3.1/UCODE/UCODE_Input_Files/SVIHMv3.1_postcal.param1',skip = 2, nrows = 61, header = T)

file_locs = file.path('C:/Users/dtolley/Scott_Valley/Modeling/100m_Grid/SVIHMv3.1',paste0('Calibration_',seq(1,num_cals),'/SVIHMv3.1_Calibration_',seq(1,num_cals),'_out._paopt'))

Cal_Data_Raw = lapply(file_locs, function(x) read.table(x,skip = 1, stringsAsFactors = F))

for (i in 1:num_cals){
  if (i==1){
    Cal_Data = cbind(Parameter = Cal_Data_Raw[[i]][1], Calibrated_Value = Cal_Data_Raw[[i]][2])
  } else {
    Cal_Data = cbind(Cal_Data,Cal_Data_Raw[[i]][2])
  }
}
names(Cal_Data) = c('Parameter',paste0('Cal_',seq(1,num_cals)))

Cal_Range = data.frame(Parameter = Cal_Data$Parameter)
Cal_Range$Min = apply(X = Cal_Data[,-1], FUN = min, MARGIN = 1)
Cal_Range$Max = apply(X = Cal_Data[,-1], FUN = max, MARGIN = 1)

for (i in 1:500){
  eval(parse(text = paste0('Cal_Range$UniRandParVal_',i,'[which((Cal_Range$Max - Cal_Range$Min)==0)]=Cal_Range$Min[which((Cal_Range$Max - Cal_Range$Min)==0)]')))  # Non-adjustable parameters remain the same
  Cal_Par_Range = Cal_Range$Max[which((Cal_Range$Max - Cal_Range$Min)!=0)] - Cal_Range$Min[which((Cal_Range$Max - Cal_Range$Min)!=0)]
  NewParVals = Cal_Par_Range*runif(n=length(which((Cal_Range$Max - Cal_Range$Min)!=0)), min=0, max=1) + Cal_Range$Min[which((Cal_Range$Max - Cal_Range$Min)!=0)]
  eval(parse(text = paste0('Cal_Range$UniRandParVal_',i,'[which((Cal_Range$Max - Cal_Range$Min)!=0)] = NewParVals')))
  eval(parse(text = paste0('param_template$StartValue = Cal_Range$UniRandParVal_',i)))
  write(file = paste0('SVIHM.unirandpar',i),x = 'BEGIN  Parameter_Data  Table',append = F)
  write(file = paste0('SVIHM.unirandpar',i),x = 'NROW=61  NCOL=8  COLUMNLABELS',append = T)
  write.table(file = paste0('SVIHM.unirandpar',i), x = param_template, col.names = T, row.names = F, sep = '  ', quote = F, append = T)
  write(file = paste0('SVIHM.unirandpar',i),x = 'END  Parameter_Data',append = T)
  }




