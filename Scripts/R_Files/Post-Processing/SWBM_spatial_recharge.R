SWBM_spatial_recharge = function(recharge_vol_filename, SWBM_log_filename){
  library(MASS)
  recharge_vol = read.table(recharge_vol_filename,skip = 2) # import total monthly linear recharge (m)
  field_area_MF = read.table(SWBM_log_filename,skip = 7, nrows = 2119, header = T)[,c(1,7)] # import total monthly linear recharge (m)
  field_area_MF$MF_Area = field_area_MF$MF_Area*0.000247105 # convert m^2 to acres
  recharge_vol$V1 = rep(seq(1991,2011),each = 12)   # change stress period to water year
  recharge_vol_annual =  aggregate(.~V1,recharge_vol, FUN = sum)
  recharge_vol_annual =  t(recharge_vol_annual[,-1]*0.000810714) # convert m^3 to acre-ft
  Recharge_stats = data.frame(Field_ID = seq(1,2119),
                        Vol_Avg_AF = apply(recharge_vol_annual, 1, FUN = mean),
                        Vol_STD_AF = apply(recharge_vol_annual, 1, FUN = sd),
                        Vol_Med_AF = apply(recharge_vol_annual, 1, FUN = median),
                        Vol_Max_AF = apply(recharge_vol_annual, 1, FUN = max),
                        Vol_Min_AF = apply(recharge_vol_annual, 1, FUN = min))
  Recharge_stats$Avg_Rate = Recharge_stats$Vol_Avg_AF/field_area_MF$MF_Area*12    #Rate in in/yr
  Recharge_stats$STD_Rate = Recharge_stats$Vol_STD_AF/field_area_MF$MF_Area*12    #Rate in in/yr
  Recharge_stats$Med_Rate = Recharge_stats$Vol_Med_AF/field_area_MF$MF_Area*12    #Rate in in/yr
  Recharge_stats$Max_Rate = Recharge_stats$Vol_Max_AF/field_area_MF$MF_Area*12    #Rate in in/yr
  Recharge_stats$Min_Rate = Recharge_stats$Vol_Min_AF/field_area_MF$MF_Area*12    #Rate in in/yr
  
  Recharge_stats[is.na(Recharge_stats)] = 0
  write.matrix(x = Recharge_stats, file = 'SWBM_Annual_Recharge_Stats.txt', sep = ',')
  return(Recharge_stats)
  }
