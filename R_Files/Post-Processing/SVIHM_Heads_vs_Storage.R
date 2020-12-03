# Read MODFLOW Binary Head File
rm(list=ls())
library(ggplot2)
start_time <- Sys.time()

# User Inputs -------------------------------------------------------------

NLAY = 2                 # Number of layers in model
NSP = 336                # Number of stress peridos for which heads are printed
NROW = 440               # Number of rows in 2020-12-02 version of SVIHM
NCOL = 210               # Number of columns in 2020-12-02 version of SVIHM
filename = 'SVIHM.hds'   #Name of binary head file
No_FLow_Val = 9999       #Value of no flow cells
Print_All_Heads = FALSE  #Flag for printing all groundwater heads
Print_Mon_Wells = FALSE   #Flag for printing heads at monitoring well locations 
Evaluate_Storage = TRUE  #Flag for evalutating relationship between heads and storage
# Directories
svihm_dir = "C:/Users/Claire/Documents/GitHub/SVIHM"
analysis_dir = file.path(svihm_dir, "R_Files", "Post-Processing")
scenario_dir = file.path(svihm_dir, "MODFLOW", "basecase")
output_dir = 'Results/'  #Output directory
source(file.path(analysis_dir, 'Extract_Monitoring_Well_Heads.R'))    #Function for extracting heads from matrix
# source(file.path(analysis_dir,'Read_LST.R'))                         #Function for extracting MODFLOW Budget Terms

# Read Heads --------------------------------------------------------------

fid = file(file.path(scenario_dir, filename), "rb")
bytes = 0                                     #Bytes counter
p=1

H_all=array(data = NA, dim = c(NROW, NCOL, NLAY, NSP))

for(k in 1:NSP){
  print(paste0('Reading Heads for Stress Period ', k))
  KSPT = readBin(fid, integer(), n = 1, size = 4); bytes = bytes + 4                          #Time step number in stress period 
  KPER = readBin(fid, integer(), n = 1, size = 4); bytes = bytes + 4                          #Stress period number
  PERTIM = readBin(fid, numeric(), n = 1, size = 4); bytes = bytes + 4                        #Time in the current stress period
  TOTIM = readBin(fid, numeric(), n = 1, size = 4); bytes = bytes + 4                         #Total elapsed time
  DESC =  readBin(readBin(fid, "raw", n=16L, size=1L, endian="little"),
                  "character", n=1L, endian="little"); bytes = bytes + 16            #Description of the array
  NCOL = readBin(fid, integer(), n = 1, size = 4); bytes = bytes + 4                          #Number of columns in the model
  NROW = readBin(fid, integer(), endian = "little", size = 4); bytes = bytes + 4              #Number of rows in the model
  ILAY = readBin(fid, integer(), endian = "little", size = 4); bytes = bytes + 4              #Current layer number
  H = array(data = NA, dim = c(NROW, NCOL, NLAY))
  H1 = matrix(readBin(fid, numeric(), n=92400, size = 4), nrow = NROW, ncol = NCOL, byrow = T)  #Read in head matrix
  H[,,1] = H1
  for (i in 2:NLAY){ # Read in data for remaining layers
    KSPT = readBin(fid, integer(), n = 1, size = 4); bytes = bytes + 4                          #Time step number in stress period 
    KPER = readBin(fid, integer(), n = 1, size = 4); bytes = bytes + 4                          #Stress period number
    PERTIM = readBin(fid, numeric(), n = 1, size = 4); bytes = bytes + 4                        #Time in the current stress period
    TOTIM = readBin(fid, numeric(), n = 1, size = 4); bytes = bytes + 4                         #Total elapsed time
    DESC =  readBin(readBin(fid, "raw", n=16L, size=1L, endian="little"),
                    "character", n=1L, endian="little"); bytes = bytes + 16            #Description of the array
    NCOL = readBin(fid, integer(), n = 1, size = 4); bytes = bytes + 4                          #Number of columns in the model
    NROW = readBin(fid, integer(), endian = "little", size = 4); bytes = bytes + 4              #Number of rows in the model
    ILAY = readBin(fid, integer(), endian = "little", size = 4); bytes = bytes + 4              #Current layer number  
    H_temp = matrix(readBin(fid, numeric(), n=92400, size = 4), nrow = NROW, ncol = NCOL, byrow = T)  #Read in head matrix
    eval(parse(text = paste0('H[,,NLAY] = H_temp')))
  }
  H[H==No_FLow_Val] = NaN
  
  if (Print_Mon_Wells==T){
    if (k==1){
      mon_well_heads = Extract_Monitoring_Well_Heads(H, Current_Wells = T)
    }  else {
      mon_well_heads = cbind(mon_well_heads, Extract_Monitoring_Well_Heads(H, Current_Wells = T)[2])
    } 
  }
  
  H_all[,,,k] = H
}


# Print Monitoring Well Heads ---------------------------------------------
if (Print_Mon_Wells==T){names(mon_well_heads) = c('Well_ID', paste0('Head_SP', seq(1,NSP)))}
if (Print_Mon_Wells==T){
  write.table(mon_well_heads, file = paste0(output_dir,'Monitoring_Well_Heads.txt'), quote = F, row.names = F, sep = ',')
}


# Map GW Elev and DTW -----------------------------------------------------

# Read in top of layer 1 (ground surface elevation)
# Read in the whole .dis file
dis_text = readLines(con=file.path(scenario_dir, "SVIHM.dis"))
#Find data indices for the ground surface in the .dis file
start_index = which(grepl(pattern = "TOP of Model", x = dis_text)) + 1
end_index = which(grepl(pattern = "BOTTOM of Layer   1", x = dis_text)) - 1
# Trim white space, split the numeric values, and vectorize
ground_surface_elev_values = trimws(dis_text[start_index:end_index])
gse_val = unlist(strsplit(ground_surface_elev_values, split = "  "))
gse = matrix(data = as.numeric(gse_val), nrow=NROW, ncol=NCOL, byrow=TRUE)

# image(t(gse[440:1,])) # Test by mapping: transpose x and y vals and reverse x vals to convert to geographic position
for(i in 1:NLAY){
  h_layer = H_all[,,i,]
  h_avg = apply(X = h_layer, MARGIN = c(1,2), FUN = mean)
  
  dtw = gse - h_avg
  dtw_lt_15 = dtw
  dtw_lt_15[dtw_lt_15 > 15] = NA
  
  image(t(dtw_lt_15[440:1,]))
}
# 

# Evaluate Storage --------------------------------------------------------
if (Evaluate_Storage==T){
  print('Reading MODFLOW Listing File')
  MODFLOW_Budget('SVIHM.lst', c('STORAGE', 'CONSTANT_HEAD', 'WELLS', 'RECHARGE', 'ET_SEGMENTS','STREAM_LEAKAGE', 'DRAINS'))
  
  #Month to Month
  Storage_Diff_Yearly_Monthly = -MODFLOW_Budget_Monthly$STORAGE_net_m3[-1]                      #Monthly change in storage (negative sign due to MODFLOW sign conventions)
  Monthly_Head_Diff_Avg = colMeans(t(apply(mon_well_heads[,-1],1,FUN = diff)))   #Average difference in head at 50 monitoring well locations
  monthly_lin_mod = lm(Storage_Diff_Yearly_Monthly~Monthly_Head_Diff_Avg)
  month_eqn_text = paste0('y = ',round(monthly_lin_mod$coefficients[2]/1E6,digits = 2),'x')
  Monthly_SVH = data.frame(delta_H = Monthly_Head_Diff_Avg, delta_S = Storage_Diff_Yearly_Monthly)
  
  Obs_monthly_head_diff = read.table('Obs_Monthly_Head_Diff.txt')
  Obs_monthly_head_diff = aggregate(V2~V1, Obs_monthly_head_diff, FUN = mean)
  Obs_monthly_head_diff$V1 = Obs_monthly_head_diff$V1-1
  
  Obs_head_v_storage = data.frame(delta_H = Obs_monthly_head_diff$V2, delta_S = Storage_Diff_Yearly_Monthly[Obs_monthly_head_diff$V1])
  obs_monthly_lin_mod = lm(Obs_head_v_storage$delta_S~Obs_head_v_storage$delta_H)
  
  
  pdf(paste0(output_dir,'Monthly_Head_Storage_Regression.pdf'),width = 6, height = 4)
  (Monthly_Head_Storage_Plot = ggplot(data = Monthly_SVH, aes(x = delta_H, y = delta_S/1E6)) + 
    geom_smooth(method = 'lm') +
    geom_smooth(data = Obs_head_v_storage, method = 'lm', color = 'red') +  
    geom_point(color = 'blue') + 
    geom_point(data = Obs_head_v_storage, aes(x = delta_H, y = delta_S/1E6), color = 'red') +
    ggtitle(' Monthly Head vs Storage Change') +
    xlab('Monthly Average Change in Head (m)') +
    ylab(bquote('Monthly Average Change in Storage ('~ Mm^3*')')) +
    scale_x_continuous(limits = c(-2,3), expand = c(0,0)) +
    scale_y_continuous(limits = c(-25,50), expand = c(0,0)) +
    theme(panel.background = element_blank(), panel.border = element_rect(fill = NA),
        legend.position = c(0.5,0.08), legend.direction = 'horizontal',
        legend.title = element_blank(),
        legend.key = element_rect(fill = NA), legend.background = element_rect(fill = NA),
        legend.text = element_text(size = 12, margin = margin(r=5,l=2, unit = 'pt')),
        plot.title = element_text(hjust = 0.5)) +
    annotate('text',x = -1, y = 35,label = month_eqn_text, size = 4) +
    annotate('text',x = -1, y = 30,label = paste0('R^2 = ',round(summary(monthly_lin_mod)$r.squared,digits = 2)), size = 4))
   graphics.off()
   
   #year to year (Jan,Feb,Mar)
   Cumulative_Storage = -cumsum(MODFLOW_Budget_Monthly$STORAGE_net_m3) #Monthly change in storage (negative sign due to MODFLOW sign conventions
   Storage_Diff_Yearly_Jan = Cumulative_Storage[seq(4+12,252,by = 12)] - Cumulative_Storage[seq(4,252-12,by = 12)]
   Storage_Diff_Yearly_Feb = Cumulative_Storage[seq(5+12,252,by = 12)] - Cumulative_Storage[seq(5,252-12,by = 12)]
   Storage_Diff_Yearly_Mar = Cumulative_Storage[seq(6+12,252,by = 12)] - Cumulative_Storage[seq(6,252-12,by = 12)]

   Jan_Head_Diff_Avg = colMeans(t(apply(mon_well_heads[,seq(5,ncol(mon_well_heads),by = 12)],1,FUN = diff)))   #Average Difference in Jan Heads
   Feb_Head_Diff_Avg = colMeans(t(apply(mon_well_heads[,seq(6,ncol(mon_well_heads),by = 12)],1,FUN = diff)))   #Average Difference in Jan Heads
   Mar_Head_Diff_Avg = colMeans(t(apply(mon_well_heads[,seq(7,ncol(mon_well_heads),by = 12)],1,FUN = diff)))   #Average Difference in Jan Heads
   
   Yearly_SVH_JFM = data.frame(delta_H = c(Jan_Head_Diff_Avg,Feb_Head_Diff_Avg,Mar_Head_Diff_Avg),
                               delta_S = c(Storage_Diff_Yearly_Jan,Storage_Diff_Yearly_Feb,Storage_Diff_Yearly_Mar))
   #Convert from metric to imperial
   Yearly_SVH_JFM$delta_H = Yearly_SVH_JFM$delta_H*3.28          # m to ft
   Yearly_SVH_JFM$delta_S = Yearly_SVH_JFM$delta_S*0.000810714   # m^3 to acre-ft
   
   Yearly_SVH_JFM_lin_mod = lm(Yearly_SVH_JFM$delta_S/1E3~Yearly_SVH_JFM$delta_H)
   yearly_eqn_text = paste0('y = ',round(Yearly_SVH_JFM_lin_mod$coefficients[2],digits = 2),'x')

   
   Yearly_JFM_Head_Storage_Plot = ggplot(data = Yearly_SVH_JFM, aes(x = delta_H, y = delta_S/1E3)) + 
       geom_smooth(method = 'lm') +  
       geom_point(color = 'blue') + 
       ggtitle(' Yearly Head vs Storage Change (Jan, Feb, Mar)') +
       xlab('Annual Average Change in Head ft)') +
       ylab('Annual Change in Staorage (TAF)') +
       scale_x_continuous(limits = c(-6,8), expand = c(0,0), breaks = seq(-6,8,by = 2)) +
       scale_y_continuous(limits = c(-30,40), expand = c(0,0), breaks = seq(-30,40,by = 10)) +
       theme(panel.background = element_blank(), panel.border = element_rect(fill = NA),
             legend.position = c(0.5,0.08), legend.direction = 'horizontal',
             legend.title = element_blank(),
             legend.key = element_rect(fill = NA), legend.background = element_rect(fill = NA),
             legend.text = element_text(size = 12, margin = margin(r=5,l=2, unit = 'pt')),
             plot.title = element_text(hjust = 0.5)) +
       annotate('text',x = -4, y = 30,label = month_eqn_text, size = 4) +
       annotate('text',x = -4, y = 25,label = paste0('R^2 = ',round(summary(monthly_lin_mod)$r.squared,digits = 2)), size = 4)
   
   pdf(paste0(output_dir,'Yearly_JFM_Head_Storage_Regression.pdf'),width = 6, height = 4)
   Yearly_JFM_Head_Storage_Plot
   graphics.off()
   
}

closeAllConnections()
end_time <- Sys.time()
print(paste0('Total Run Time was ', round(end_time - start_time, digits = 1), ' seconds'))
