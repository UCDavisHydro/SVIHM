#Script for extracting streamflow from SVIHM at the end of every stress period for 5 locations along the Scott River.
rm(list=ls())
library(lubridate)
if (file.exists('Cal_Num.dat')){
  Cal_Num  = as.numeric(readLines('Cal_Num.dat'))  
}
Counter  = as.numeric(readLines('Counter.dat'))
StreamflowText = readLines('Streamflow_Global.dat')

Pred_Loc_1_idx = grep('1   61   10    30    29', StreamflowText)
Pred_Loc_2_idx = grep('1   95  118    23    10', StreamflowText)
Pred_Loc_3_idx = grep('1  210  123    12    39', StreamflowText)
Pred_Loc_4_idx = grep('1  296  131     9    10', StreamflowText)
Pred_Loc_5_idx = grep('1  390  151     1    72', StreamflowText)

Pred_Loc_1_Flows_m3day = as.numeric(sapply(lapply(strsplit(StreamflowText[Pred_Loc_1_idx],' '),function(x){x[!x ==""]}),"[[",8))  #Flow out of segment
Pred_Loc_2_Flows_m3day = as.numeric(sapply(lapply(strsplit(StreamflowText[Pred_Loc_2_idx],' '),function(x){x[!x ==""]}),"[[",8))  #Flow out of segment
Pred_Loc_3_Flows_m3day = as.numeric(sapply(lapply(strsplit(StreamflowText[Pred_Loc_3_idx],' '),function(x){x[!x ==""]}),"[[",8))  #Flow out of segment
Pred_Loc_4_Flows_m3day = as.numeric(sapply(lapply(strsplit(StreamflowText[Pred_Loc_4_idx],' '),function(x){x[!x ==""]}),"[[",8))  #Flow out of segment
Pred_Loc_5_Flows_m3day = as.numeric(sapply(lapply(strsplit(StreamflowText[Pred_Loc_5_idx],' '),function(x){x[!x ==""]}),"[[",8))  #Flow out of segment

SP_dates = format(seq(as.Date('1990/10/1'), by = 'month', length.out = 252),'%b-%Y')

Pred_Loc_Flows = data.frame(StressPeriod = SP_dates,
                            Month = rep(format(seq(as.Date('2002/10/1'),by = 'month', length.out = 12),'%b'),21),
                            Water_Year = rep(seq(1991,2011),each = 12),
                            Pred_Loc_1_m3day = Pred_Loc_1_Flows_m3day,
                            Pred_Loc_2_m3day = Pred_Loc_2_Flows_m3day,
                            Pred_Loc_3_m3day = Pred_Loc_3_Flows_m3day,
                            Pred_Loc_4_m3day = Pred_Loc_4_Flows_m3day,
                            Pred_Loc_5_m3day = Pred_Loc_5_Flows_m3day)

if (file.exists('Cal_Num.dat')){
  write.table(x = Pred_Loc_Flows, file = paste0('Flow_Predictions_Cal_',Cal_Num,'_',Counter,'.dat'), col.names = TRUE, row.names = FALSE, quote = FALSE)  
} else {
  write.table(x = Pred_Loc_Flows, file = paste0('Flow_Predictions_',Counter,'.dat'), col.names = TRUE, row.names = FALSE, quote = FALSE)
}
write.table(x = Pred_Loc_Flows, file = paste0('Flow_Predictions_Cal_',Cal_Num,'_',Counter,'.dat'), col.names = TRUE, row.names = FALSE, quote = FALSE)


