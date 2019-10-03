# General Comments --------------------------------------------------------
#file = file name and path (if not located in same working directory)
#nsegs = number of SFR segments
#start_date = date simulation is started on (SVIHM uses monthly stress periods)
#nstress = number of stress periods
SVIHM_Global_Streamflow = function(filename, nsegs, start_date, nstress){
  Streamflow_out_headers = c('lay', 'row', 'col', 'seg', 'reach', 'flow_in', 'flow2aq', 'flow_out', 'runoff',
                             'direct_precip', 'stream_et', 'head', 'depth', 'width', 'conductance', 'gradient')
  SFR_global_out_text = readLines(filename)  #read global streamflow data text
  SP_Months = seq(as.Date(start_date), length.out = nstress, by = 'month')
  SP_idx = grep('Stream Listing',SFR_global_out_text, ignore.case = T) + 4 #lines where data blocks start 
  for (i in seq(1,length(SP_idx))){
    temp_data = read.table(file = filename, skip = SP_idx[i], nrows = nsegs)
    names(temp_data) = Streamflow_out_headers
    temp_data$Month = format(SP_Months[i],'%b-%Y')
    if (i==1){
      SVIHM_Global_Streamflow = temp_data
    } else {
      SVIHM_Global_Streamflow = rbind(SVIHM_Global_Streamflow, temp_data)
    }
  }
  return(SVIHM_Global_Streamflow)
}



