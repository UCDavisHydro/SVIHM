SWBM_Landuse = function(budget_terms, landuse_cats, header_names){
  filename = paste0('monthly_',budget_terms,'_by_luse.dat')
  landuse_area_m2 = read.table('landuse_area_m2.dat',header = T) # import monthly landuse areas
  names(landuse_area_m2) = c('Month',landuse_cats, 'Water' )
  landuse_area_acres = landuse_area_m2
  landuse_area_acres[,-1] = landuse_area_acres[,-1]*0.000247105
  
  #Import SWBM Variables from input files
  RD_Mult=  read.table('general_inputs.txt',nrows = 1)[6]              #Read in root depth multiplier
  Kc_Mults = read.table('crop_coeff_mult.txt',nrows = 1)[c(1,2,3,4)]   #Read crop coefficient multipliers
  AG_IE = read.table('irr_eff.txt',nrows = 1, skip = 1)[c(1,2)]        #Read in alfalfa/grain irrigation efficiencies
  P_IE = read.table('irr_eff.txt', skip = 2)[c(1,2)]                   #Read in pasture irrigation efficiencies
  
  Root_Text_standard = paste('RootDepth: Alfalfa/Grain/Native Veg =', RD_Mult*8, 'ft | Pasture =  4 ft')
  Root_Text_metric = paste('RootDepth: Alfalfa/Grain/Native Veg =', RD_Mult*8*0.3049, 'm | Pasture =  1.22 m')
  Kc_A_Text = paste('Alfalfa Kc:', 0.9*Kc_Mults[1])
  Kc_P_Text = paste('Pasture Kc:', 0.9*Kc_Mults[3])
  AG_IE_Text = paste('Alfalfa/Grain IrrEff: WL =',AG_IE[1] ,'| CP =',AG_IE[2])
  P_IE_Text = paste('Pasture IrrEff: WL =',P_IE[1] ,'| CP =',P_IE[2])
  
  TEXT_FOR_TABLE_HEADER_standard = c(Root_Text_standard,Kc_A_Text,
                            'Grain Kc: variable',Kc_P_Text,'Flood IrrEff: 0.7',AG_IE_Text,P_IE_Text)
  TEXT_FOR_TABLE_HEADER_metric = c(Root_Text_metric,Kc_A_Text,
                                     'Grain Kc: variable',Kc_P_Text,'Flood IrrEff: 0.7',AG_IE_Text,P_IE_Text)
  
  print_units = c('m/yr', 'inch/yr', 'Mm3/yr', 'TAF/yr')
  text_units = substr(print_units,1,nchar(print_units)-3)
  
  for (i in 1:length(budget_terms)){
   monthly_m3 = read.table(filename[i], header = T)         #import monthly volumes by landuse type (m^3)
   names(monthly_m3) = c('SP',landuse_cats)
   monthly_total_m3 = as.data.frame(rowSums(monthly_m3[-1]))
   monthly_m3$WY = rep(seq(1991,2011),each = 12)
   annual_m3 = aggregate(.~WY,monthly_m3[c('WY',landuse_cats)],FUN=sum)
   mean_21yr_Mm3 = round(colMeans(annual_m3[landuse_cats])/1E6,2)
   mean_21yr_TAF = round(mean_21yr_Mm3 * 0.810714,2)
   monthly_m = monthly_m3[landuse_cats] / subset(landuse_area_m2,select = landuse_cats)
   monthly_m$WY = rep(seq(1991,2011),each = 12)
   annual_m = aggregate(.~WY,monthly_m,FUN = sum)
   mean_21yr_m = round(colMeans(annual_m[landuse_cats]),2)
   mean_21yr_inch = round(colMeans(annual_m[landuse_cats]) * 39.3701,2)
  
   WY2001_Dry_Mm3 = round(subset(annual_m3,WY==2001, select = landuse_cats)/1E6,2)
   WY2010_Avg_Mm3 = round(subset(annual_m3,WY==2010, select = landuse_cats)/1E6,2)
   WY2006_Wet_Mm3 = round(subset(annual_m3,WY==2006, select = landuse_cats)/1E6,2)
   WY2001_Dry_m = round(subset(annual_m,WY==2001, select = landuse_cats),2)
   WY2010_Avg_m = round(subset(annual_m,WY==2010, select = landuse_cats),2)
   WY2006_Wet_m = round(subset(annual_m,WY==2006, select = landuse_cats),2)
   
   WY2001_Dry_TAF = round(WY2001_Dry_Mm3 * 0.810714,2)
   WY2010_Avg_TAF = round(WY2010_Avg_Mm3 * 0.810714,2)
   WY2006_Wet_TAF = round(WY2006_Wet_Mm3 * 0.810714,2)
   WY2001_Dry_inch = round(WY2001_Dry_m * 39.3701,2)
   WY2010_Avg_inch = round(WY2010_Avg_m * 39.3701,2)
   WY2006_Wet_inch = round(WY2006_Wet_m * 39.3701,2)
  
   SWBM_landuse_Mm3 = as.data.frame(rbind(mean_21yr_Mm3,WY2001_Dry_Mm3,WY2010_Avg_Mm3,WY2006_Wet_Mm3))
   SWBM_landuse_m = as.data.frame(rbind(mean_21yr_m,WY2001_Dry_m,WY2010_Avg_m,WY2006_Wet_m))
   SWBM_landuse_TAF = as.data.frame(rbind(mean_21yr_TAF,WY2001_Dry_TAF,WY2010_Avg_TAF,WY2006_Wet_TAF))
   SWBM_landuse_inch = as.data.frame(rbind(mean_21yr_inch,WY2001_Dry_inch,WY2010_Avg_inch,WY2006_Wet_inch))
   rownames(SWBM_landuse_Mm3) = c('21yr_avg', 'Dry_Year_2001', 'Avg_Year_2010', 'Wet_Year_2006')
   rownames(SWBM_landuse_m) = c('21yr_avg', 'Dry_Year_2001', 'Avg_Year_2010', 'Wet_Year_2006')
   rownames(SWBM_landuse_TAF) = c('21yr_avg', 'Dry_Year_2001', 'Avg_Year_2010', 'Wet_Year_2006')
   rownames(SWBM_landuse_inch) = c('21yr_avg', 'Dry_Year_2001', 'Avg_Year_2010', 'Wet_Year_2006')
   
   eval(parse(text = paste0('SWBM_',budget_terms[i],'_Mm3 = SWBM_landuse_Mm3')))
   eval(parse(text = paste0('SWBM_',budget_terms[i],'_m = SWBM_landuse_m')))
   eval(parse(text = paste0('SWBM_',budget_terms[i],'_TAF = SWBM_landuse_TAF')))
   eval(parse(text = paste0('SWBM_',budget_terms[i],'_inch = SWBM_landuse_inch')))
  }
  for (i in 1:length(rownames(SWBM_landuse_Mm3))){
    eval(parse(text = paste0("SWBM_LU_",rownames(SWBM_landuse_Mm3)[i],"_Mm3 = data.frame(pET = t(subset(SWBM_pET_Mm3,rownames(SWBM_pET_Mm3)== rownames(SWBM_pET_Mm3)[",i,"])),
                                                               aET = t(subset(SWBM_aET_Mm3,rownames(SWBM_pET_Mm3)== rownames(SWBM_pET_Mm3)[",i,"])),
                                                               Irr = t(subset(SWBM_irrigation_Mm3,rownames(SWBM_irrigation_Mm3)== rownames(SWBM_irrigation_Mm3)[",i,"])),
                                                               SW = t(subset(SWBM_surfacewater_Mm3,rownames(SWBM_surfacewater_Mm3)== rownames(SWBM_surfacewater_Mm3)[",i,"])),
                                                               GW = t(subset(SWBM_groundwater_Mm3,rownames(SWBM_groundwater_Mm3)== rownames(SWBM_groundwater_Mm3)[",i,"])),
                                                               recharge = t(subset(SWBM_recharge_Mm3,rownames(SWBM_recharge_Mm3)== rownames(SWBM_recharge_Mm3)[",i,"])),
                                                               deficiency = t(subset(SWBM_deficiency_Mm3,rownames(SWBM_deficiency_Mm3)== rownames(SWBM_deficiency_Mm3)[",i,"])),
                                                               area = round(colMeans(subset(landuse_area_m2,select = landuse_cats))/1E6,2))")))
    eval(parse(text = paste0("colnames(SWBM_LU_",rownames(SWBM_landuse_Mm3)[i],"_Mm3) = c(header_names,'Area (Mm^2)*')")))
    eval(parse(text = paste0("SWBM_LU_",rownames(SWBM_landuse_Mm3)[i],"_Mm3 <<- SWBM_LU_",rownames(SWBM_landuse_Mm3)[i],"_Mm3")))
    
    eval(parse(text = paste0("SWBM_LU_",rownames(SWBM_landuse_m)[i],"_m = data.frame(pET = t(subset(SWBM_pET_m,rownames(SWBM_pET_m)== rownames(SWBM_pET_m)[",i,"])),
                                                               aET = t(subset(SWBM_aET_m,rownames(SWBM_pET_m)== rownames(SWBM_pET_m)[",i,"])),
                             Irr = t(subset(SWBM_irrigation_m,rownames(SWBM_irrigation_m)== rownames(SWBM_irrigation_m)[",i,"])),
                             SW = t(subset(SWBM_surfacewater_m,rownames(SWBM_surfacewater_m)== rownames(SWBM_surfacewater_m)[",i,"])),
                             GW = t(subset(SWBM_groundwater_m,rownames(SWBM_groundwater_m)== rownames(SWBM_groundwater_m)[",i,"])),
                             recharge = t(subset(SWBM_recharge_m,rownames(SWBM_recharge_m)== rownames(SWBM_recharge_m)[",i,"])),
                             deficiency = t(subset(SWBM_deficiency_m,rownames(SWBM_deficiency_m)== rownames(SWBM_deficiency_m)[",i,"])),
                             area = round(colMeans(subset(landuse_area_m2,select = landuse_cats))/1E6,2))")))
    eval(parse(text = paste0("colnames(SWBM_LU_",rownames(SWBM_landuse_m)[i],"_m) = c(header_names,'Area (Mm^2)*')")))
    eval(parse(text = paste0("SWBM_LU_",rownames(SWBM_landuse_m)[i],"_m <<- SWBM_LU_",rownames(SWBM_landuse_m)[i],"_m")))
    
    eval(parse(text = paste0("SWBM_LU_",rownames(SWBM_landuse_TAF)[i],"_TAF = data.frame(pET = t(subset(SWBM_pET_TAF,rownames(SWBM_pET_TAF)== rownames(SWBM_pET_TAF)[",i,"])),
                                                               aET = t(subset(SWBM_aET_TAF,rownames(SWBM_pET_TAF)== rownames(SWBM_pET_TAF)[",i,"])),
                             Irr = t(subset(SWBM_irrigation_TAF,rownames(SWBM_irrigation_TAF)== rownames(SWBM_irrigation_TAF)[",i,"])),
                             SW = t(subset(SWBM_surfacewater_TAF,rownames(SWBM_surfacewater_TAF)== rownames(SWBM_surfacewater_TAF)[",i,"])),
                             GW = t(subset(SWBM_groundwater_TAF,rownames(SWBM_groundwater_TAF)== rownames(SWBM_groundwater_TAF)[",i,"])),
                             recharge = t(subset(SWBM_recharge_TAF,rownames(SWBM_recharge_TAF)== rownames(SWBM_recharge_TAF)[",i,"])),
                             deficiency = t(subset(SWBM_deficiency_TAF,rownames(SWBM_deficiency_TAF)== rownames(SWBM_deficiency_TAF)[",i,"])),
                             area = round(colMeans(subset(landuse_area_acres,select = landuse_cats))/1E3,2))")))
    eval(parse(text = paste0("colnames(SWBM_LU_",rownames(SWBM_landuse_TAF)[i],"_TAF) = c(header_names,'Area (thousand acres)*')")))
    eval(parse(text = paste0("SWBM_LU_",rownames(SWBM_landuse_TAF)[i],"_TAF <<- SWBM_LU_",rownames(SWBM_landuse_TAF)[i],"_TAF")))
    
    eval(parse(text = paste0("SWBM_LU_",rownames(SWBM_landuse_inch)[i],"_inch = data.frame(pET = t(subset(SWBM_pET_inch,rownames(SWBM_pET_inch)== rownames(SWBM_pET_inch)[",i,"])),
                                                               aET = t(subset(SWBM_aET_inch,rownames(SWBM_pET_inch)== rownames(SWBM_pET_inch)[",i,"])),
                             Irr = t(subset(SWBM_irrigation_inch,rownames(SWBM_irrigation_inch)== rownames(SWBM_irrigation_inch)[",i,"])),
                             SW = t(subset(SWBM_surfacewater_inch,rownames(SWBM_surfacewater_inch)== rownames(SWBM_surfacewater_inch)[",i,"])),
                             GW = t(subset(SWBM_groundwater_inch,rownames(SWBM_groundwater_inch)== rownames(SWBM_groundwater_inch)[",i,"])),
                             recharge = t(subset(SWBM_recharge_inch,rownames(SWBM_recharge_inch)== rownames(SWBM_recharge_inch)[",i,"])),
                             deficiency = t(subset(SWBM_deficiency_inch,rownames(SWBM_deficiency_inch)== rownames(SWBM_deficiency_inch)[",i,"])),
                             area = round(colMeans(subset(landuse_area_acres,select = landuse_cats))/1E3,2))")))
    eval(parse(text = paste0("colnames(SWBM_LU_",rownames(SWBM_landuse_inch)[i],"_inch) = c(header_names,'Area (thousand acres)*')")))
    eval(parse(text = paste0("SWBM_LU_",rownames(SWBM_landuse_inch)[i],"_inch <<- SWBM_LU_",rownames(SWBM_landuse_inch)[i],"_inch")))
  }
  for (i in 1:length(print_units)){
    if (print_units[i]=='m/yr' | print_units[i]=='Mm3/yr'){
      TEXT_FOR_TABLE_HEADER = TEXT_FOR_TABLE_HEADER_metric
   }else {
      TEXT_FOR_TABLE_HEADER = TEXT_FOR_TABLE_HEADER_standard
   }

    table_file = paste0(out_dir,'/Water_Budget_by_Landuse_',text_units[i],'.txt')
    
    eval(parse(text = paste0("write(TEXT_FOR_TABLE_HEADER,sep = '\n', file=table_file, append = F)")))                                  # Header with parameters
    eval(parse(text = paste0("write('', file=table_file, append = T)")))
    eval(parse(text = paste0("write(paste0('21 Year Average - ',print_units[i]), file=table_file, append = T)")))
    eval(parse(text = paste0("cat(capture.output(SWBM_LU_21yr_avg_",text_units[i],"), file = table_file, sep = '\n', append = T)")))              # Basecase Water Budget (in/yr)
    eval(parse(text = paste0("write('', file=table_file, append = T)")))
    eval(parse(text = paste0("write('--------------------------------------------------------------------------------------------------------------', file=table_file, append = T)")))
    eval(parse(text = paste0("write('', file=table_file, append = T)")))
    eval(parse(text = paste0("write(paste0('Dry Year (2001) - ',print_units[i]), file=table_file, append = T)")))
    eval(parse(text = paste0("cat(capture.output(SWBM_LU_Dry_Year_2001_",text_units[i],"), file = table_file, sep = '\n', append = T)")))              # Basecase Water Budget (in/yr)
    eval(parse(text = paste0("write('', file=table_file, append = T)")))
    eval(parse(text = paste0("write('--------------------------------------------------------------------------------------------------------------', file=table_file, append = T)")))
    eval(parse(text = paste0("write('', file=table_file, append = T)")))
    eval(parse(text = paste0("write(paste0('Average Year (2010) - ',print_units[i]), file=table_file, append = T)")))
    eval(parse(text = paste0("cat(capture.output(SWBM_LU_Avg_Year_2010_",text_units[i],"), file = table_file, sep = '\n', append = T)")))                  # Basecase Water Budget (AF/yr)
    eval(parse(text = paste0("write('', file=table_file, append = T)")))
    eval(parse(text = paste0("write('--------------------------------------------------------------------------------------------------------------', file=table_file, append = T)")))
    eval(parse(text = paste0("write('', file=table_file, append = T)")))
    eval(parse(text = paste0("write(paste0('Wet Year (2006) - ',print_units[i]), file=table_file, append = T)")))
    eval(parse(text = paste0("cat(capture.output(SWBM_LU_Wet_Year_2006_",text_units[i],"), file = table_file, sep = '\n', append = T)")))                  # Basecase Water Budget (AF/yr)
    eval(parse(text = paste0("write('', file=table_file, append = T)")))
    eval(parse(text = paste0("write('* 21 year average for alfalfa and grain, values change following rotation', file=table_file, append = T)")))
    
    
    
    
    
    
    
    
    
    
    # write(TEXT_FOR_TABLE_HEADER,sep = '\n', file=table_file, append = F)                                  # Header with parameters
    # write('', file=table_file, append = T)
    # write(paste0('21 Year Average - ',text_units[i]), file=table_file, append = T)
    # cat(capture.output(paste0('SWBM_LU_21yr_avg_',text_units[i])), file = table_file, sep = '\n', append = T)              # Basecase Water Budget (in/yr)
    # write('', file=table_file, append = T)
    # write('-----------------------------------------------------------------------------------------------', file=table_file, append = T)
    # write('', file=table_file, append = T)
    # write(paste0('Dry Year (2001) - ',text_units[i]), file=table_file, append = T)
    # cat(capture.output(paste0('Dry_Year_2001',text_units[i])), file = table_file, sep = '\n', append = T)              # Basecase Water Budget (in/yr)
    # write('', file=table_file, append = T)
    # write('-----------------------------------------------------------------------------------------------', file=table_file, append = T)
    # write('', file=table_file, append = T)
    # write(paste0('Average Year (2010) - ',text_units[i]), file=table_file, append = T)
    # cat(capture.output(paste0('Avg_Year_2010',text_units[i])), file = table_file, sep = '\n', append = T)                  # Basecase Water Budget (AF/yr)
    # write('', file=table_file, append = T)
    # write('-----------------------------------------------------------------------------------------------', file=table_file, append = T)
    # write('', file=table_file, append = T)
    # write(paste0('Wet Year (2006) - ',text_units[i]), file=table_file, append = T)
    # cat(capture.output(paste0('Wet_Year_2006',text_units[i])), file = table_file, sep = '\n', append = T)                  # Basecase Water Budget (AF/yr)
    # write('', file=table_file, append = T)
    # write('* 21 year average for alfalfa and grain, values change following rotation', file=table_file, append = T)
    
    
    
    
    
    # #-------------------------------------------------------------------------------------------------------------
    # #-------------------------------------------------------------------------------------------------------------
    # write(TEXT_FOR_TABLE_HEADER_metric,sep = '\n', file=Sim_Avg_Table_File_metric, append = F)                                  # Header with parameters
    # write('', file=Sim_Avg_Table_File_metric, append = T)
    # write('21 Year Average Water Budget - in/yr', file=Sim_Avg_Table_File_metric, append = T)
    # cat(capture.output(SWBM_LU_21yr_avg_inch), file = Sim_Avg_Table_File_metric, sep = '\n', append = T)              # Basecase Water Budget (in/yr)
    # write('', file=Sim_Avg_Table_File_metric, append = T)
    # write('-----------------------------------------------------------------------------------------------', file=Sim_Avg_Table_File_metric, append = T)
    # write('', file=Sim_Avg_Table_File_metric, append = T)
    # write('21 Year Average Water Budget - AF/yr', file=Sim_Avg_Table_File_metric, append = T)
    # cat(capture.output(SWBM_LU_21yr_avg_TAF), file = Sim_Avg_Table_File_metric, sep = '\n', append = T)                  # Basecase Water Budget (AF/yr)
    # write('', file=Sim_Avg_Table_File_metric, append = T)
    # write('* 21 year average for alfalfa and grain, values change following rotation', file=Sim_Avg_Table_File_metric, append = T)
    # 
  }
}
