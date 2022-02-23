# Function for extracting monitoring well data when using grid cell locations
#H is is a matrix of heads (can be multi layered)
#Current Wells if a logical (TRUE mean use only the most recent monitoring well locations)

Extract_Monitoring_Well_Heads = function(H, Current_Wells){
  mon_wells = read.table('SVIHM_Observation_Well_Locs.txt', header = T)
  if (exists('Current_Wells')){
    if (Current_Wells){
      mon_wells = subset(mon_wells, Current == 'TRUE')  
    }
  }
  for (i in 1:length(mon_wells$Well_ID)){
    if (i==1){
      mon_well_heads = H[mon_wells$Row[i], mon_wells$Column[i], mon_wells$Layer[i]]
    }  else{
       mon_well_heads = c(mon_well_heads, H[mon_wells$Row[i], mon_wells$Column[i], mon_wells$Layer[i]])
    }
  }
  return(data.frame(Well_ID = mon_wells$Well_ID, Head = round(mon_well_heads,digits = 2)))
}