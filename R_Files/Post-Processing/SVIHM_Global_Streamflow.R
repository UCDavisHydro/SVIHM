# General Comments --------------------------------------------------------

# Updates cell locations for streamflow depletion/accretion analysis

# Script Initialization ---------------------------------------------------
rm(list=ls())  #Clear workspace


# User Input --------------------------------------------------------------
SFR_file = 'SVIHM.sfr'
Streamflow_out_file = 'Streamflow_Global.txt'
Streamflow_out_headers = c('lay', 'row,', 'col', 'seg', 'reach', 'flow_in', 'flow2aq', 'flow_out', 'runoff',
                           'direct_precip', 'stream_et', 'head', 'depth', 'width', 'conductance', 'gradient')

# Read Transient Data -----------------------------------------------------
nsegs = as.numeric(read.table(file = 'SVIHM.sfr', header = F, nrows = 1)[1]*-1)
SFR_global_out_text = readLines('Streamflow_Global.dat')  #read global streamflow data text
SP_idx = grep('Stream Listing',SFR_global_out_text, ignore.case = T) + 5  #lines where data blocks start 
for (i in seq(1,length(SP_idx))){
  SP1 = read.table(file = 'Streamflow_Global.dat', skip = )
  
  
  
  
  temp_text = SFR_global_out_text[seq(SP_idx[i],SP_idx[i]+nsegs-1)]
  if (i==1){
  row = as.numeric(sapply(lapply(strsplit(SFR_global_out_text[seq(SP_idx[i],SP_idx[i]+nsegs-1)],' '),function(x){x[!x ==""]}),"[[",2))  
  col = as.numeric(sapply(lapply(strsplit(SFR_global_out_text[seq(SP_idx[i],SP_idx[i]+nsegs-1)],' '),function(x){x[!x ==""]}),"[[",3))  
  rowcol = paste(row,col,sep = '')
  seg = as.numeric(sapply(lapply(strsplit(SFR_global_out_text[seq(SP_idx[i],SP_idx[i]+nsegs-1)],' '),function(x){x[!x ==""]}),"[[",4))
  rch = as.numeric(sapply(lapply(strsplit(SFR_global_out_text[seq(SP_idx[i],SP_idx[i]+nsegs-1)],' '),function(x){x[!x ==""]}),"[[",5))
  rch_flow_in = cbind(row,col,rowcol,seg,rch)
  rch_flow_2_aq = cbind(row,col,rowcol,seg,rch)
  rch_flow_out = cbind(row,col,rowcol,seg,rch)
  }
  rch_flw_in_temp = as.numeric(sapply(lapply(strsplit(SFR_global_out_text[seq(SP_idx[i],SP_idx[i]+nsegs-1)],' '),function(x){x[!x ==""]}),"[[",6))  #Flow
  rch_flow_2_aq_temp = as.numeric(sapply(lapply(strsplit(SFR_global_out_text[seq(SP_idx[i],SP_idx[i]+nsegs-1)],' '),function(x){x[!x ==""]}),"[[",7))  #Flow
  rch_flw_out_temp = as.numeric(sapply(lapply(strsplit(SFR_global_out_text[seq(SP_idx[i],SP_idx[i]+nsegs-1)],' '),function(x){x[!x ==""]}),"[[",8))  #Flow
  
  rch_flow_in = cbind(rch_flow_in,rch_flw_in_temp)  
  rch_flow_2_aq = cbind(rch_flow_2_aq,rch_flow_2_aq_temp)  
  rch_flow_out = cbind(rch_flow_out,rch_flw_out_temp)  
  }
colnames(rch_flow_in) = c('row','col','rowcol','seg','rch',paste('SP',seq(1,252),sep = ''))
colnames(rch_flow_2_aq) = c('row','col','rowcol','seg','rch',paste('SP',seq(1,252),sep = ''))
colnames(rch_flow_out) = c('row','col','rowcol','seg','rch',paste('SP',seq(1,252),sep = ''))

write.table(rch_flow_in, file = 'Global_Reach_Flow_In.csv', quote = F, sep = ',', row.names = F)
write.table(rch_flow_2_aq, file = 'Global_Reach_Flow_2_aquifer.csv', quote = F, sep = ',', row.names = F)
write.table(rch_flow_out, file = 'Global_Reach_Flow_Out.csv', quote = F, sep = ',', row.names = F)


