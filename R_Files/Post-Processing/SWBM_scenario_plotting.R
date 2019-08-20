# Plot Scenario Comparisons

# Input: two monthly_water_budget.dat type files, (commonly, one historical and one a scenario)
# Output: One pdf with:
## Overview of historical
## Overview of scenario
## Historical-scenario comparisons for each water budget component
## Overall water budget term difference table
## Overall water budget term difference table by month

library(ggplot2)
library(plotly)

rm(list = ls())

#Define directories
proj_dir = dirname(dirname(dirname(getActiveDocumentContext()$path ))) #Outer SVIHM folder
scenario_dir = file.path(proj_dir,"SWBM","up2018_b")
output_dir = file.path(proj_dir,"SVIHM_Input_Files","Scenario_Development")
pdf_dir = output_dir
# pdf_dir = file.path(scenario_dir,"")#,"comparison_pdfs")


# Functions ---------------------------------------------------------------

make_legend_symbol_table = function(){
  label = c("Precipitation","Surface water irrigation", "Groundwater irrigation", "Evapotranspiration","Recharge","Change in storage")
  color = c("lightblue", "deepskyblue3", "deepskyblue4", "goldenrod", "green4", "black")
  components = data.frame(label,color)
  components$color = as.character(components$color)
  components$abbrev = c("Precip", "SW_Irr", "GW_Irr", "ET", "Recharge", "Storage")
  
  return(components)
}

plot_water_budget_overview = function(mwb, scenario_name){
  #Label budget components
  P = mwb$Precip
  SW = mwb$SW_Irr
  GW = mwb$GW_Irr
  ET = mwb$ET
  R = mwb$Recharge
  S = mwb$Storage
  
  #Plot parameters
  vol_lim = c(min(mwb[2:7]), max(mwb[2:7]))
  x_lim = c(1, dim(mwb)[1])
  #Symbols and legend labels
  components = make_legend_symbol_table()

  #Pdf setup
  setwd(pdf_dir)
  pdfname = paste0(scenario_name, "_overview.pdf")
  pdf(pdfname, 7.5,9)
  par(mfrow=c(2,1))
  
  #Initialize Panel 1
  plot(x = NA, y = NA, xlim = x_lim,  ylim = vol_lim,
       axes = FALSE, xlab = "Year", ylab = "", #Suppress y-axis label text
       main = paste(scenario_name, "monthly water budget"),
       sub = "Negative values indicate water leaving modeled soil volume", cex = 0.8)
  
  #Manual stylized grid lines (plotted first to not obscure data)
  abline(v = seq(from = 4, to = 336, by=12),
         lty = 3, lwd = 0.5, col = "lightgray")
  abline(h = pretty(vol_lim), v = seq(from = 52, to = 336, by=60), # Jan of 95, 00, 05, 10
         lty = 1, col = "darkgray")
  abline(h = 0, col = "black")
  #Manual y-label and axis labels, and a box (plot border)
  mtext(side = 2, expression(paste("Water volume (cubic meters x 10"^"7", ")")), 
        las = 0, line = 2) #manual y-label
  axis(1, at = seq(from = 4, to = 336, by=12), labels = 1991:2018)
  axis(2, at = pretty(vol_lim), pretty(vol_lim)/10^7)
  box()
  
  #Plot water budget components over time. Ordered to optimize visibility
  lines(R, col = components$color[5], lwd = 2) 
  lines(P, col = components$color[1], lwd = 2)
  lines(GW, col = components$color[3], lwd = 2)
  lines(SW, col = components$color[2], lwd = 2)
  lines(ET, col = components$color[4], lwd = 2)
  
  
  #Initialize Panel 2
  plot(x = NA, y = NA, xlim = x_lim,  ylim = vol_lim,
       axes = FALSE, xlab = "Year", ylab = "", #Suppress y-axis text
       main = paste0(scenario_name, " monthly change in soil water storage"),
       sub = "Negative values indicate water leaving modeled soil volume", cex = 0.8)
  #Manual stylized grid lines (plotted first to not obscure data)
  abline(v = seq(from = 4, to = 252, by=12),
         lty = 3, lwd = 0.5, col = "lightgray")
  abline(h = pretty(vol_lim), v = seq(from = 52, to = 252, by=60), # Jan of 95, 00, 05, 10
         lty = 1, col = "darkgray")
  abline(h = 0, col = "black")
  #Manual y-label and axis labels
  mtext(side = 2, expression(paste("Change in storage (cubic meters x 10"^"7", ")")), 
        las = 0, line = 2) #manual y-label
  axis(1, at = seq(from = 4, to = 252, by=12), #Jan of years ending in 5 and 0
       labels = 1991:2011)
  axis(2, at = pretty(vol_lim), pretty(vol_lim)/10^7)
  box()
  
  #Plot change in storage over time
  lines(S, type = "l", col = components$color[6], lwd = 2)
  
  #Add legend to the bottom panel
  legend(x = "topleft", lwd = 2, ncol = 2, bg= "white",
         col = components[,2], legend = components[,1])
  
  dev.off()
}

plot_water_budget_comparison = function(mwb1, mwb2, scenario_names){
  #x-values are stress periods (months; 252 months in model). 
  #Each stress period is a row in the monthly water budget.
  #Assumes mwb1 and mwb2 have the column names of .dat output from the SWBM and have the same dimensions.
  
  #Plot parameters
  num_params = dim(mwb1)[2]-1
  vol_lim = c(min(mwb1[2:(num_params+1)]), max(mwb1[2:(num_params+1)]))
  x_limits = c(1, dim(mwb1)[1]) 
  #Symbols and legend labels
  components = make_legend_symbol_table()
  
  setwd(pdf_dir)
  pdfname = paste0("comparisons_",scenario_names[1], "_", scenario_names[2],".pdf")
  # vol_lim = c(min(R), max(P)) # set above, from historical obs.
  pdf(pdfname, 8.5,11)
  
  par(mfrow = c(2, 1))
  for(i in 1:num_params){
    y_1 = mwb1[,i+1] #first column is "stress period." i+1 avoids it
    y_2 = mwb2[,i+1]
    
    #Set plot y limits
    minmax_y = c(min(c(y_1, y_2)), max(c(y_1, y_2)))
    range_y = diff(minmax_y)
    y_limits = c(minmax_y[1], minmax_y[2] + 0.1 * range_y) #leave room for legend at bottom
    
    param_label = components[i,1]
    line_col=components[i,2]
    
    #Initialize plot
    plot(x=NA, y=NA, xlim = x_limits, ylim = y_limits,
         axes = FALSE, xlab = "Year", ylab = "", #Suppress y-axis label text
         main = paste0("Comparison: ", param_label, ", ", scenario_names[1], " vs ", scenario_names[2]),
         sub = "Negative values indicate water leaving modeled soil volume")
    
    #Manual stylized grid lines (plotted first to avoid obscuring data)
    abline(v = seq(from = 4, to = 252, by=12),
           lty = 3, lwd = 0.5, col = "lightgray")
    abline(h = pretty(y_limits), v = seq(from = 52, to = 252, by=60), # Jan of 95, 00, 05, 10
           lty = 1, col = "darkgray")
    abline(h = 0, col = "black") 
    #Manual y-label, axis labels, and add a box around the plot
    mtext(side = 2, expression(paste("Water volume (cubic meters x 10"^"7", ")")), 
          las = 0, line = 2) #manual y-label
    axis(1, at = seq(from = 4, to = 252, by=12), #Jan of years ending in 5 and 0
         labels = 1991:2011)
    axis(2, at = pretty(y_limits), pretty(y_limits)/10^7)
    box()
    #Plot data
    lines(y_1, type = "l", col = line_col, lwd = 2, lty = 1) 
    lines(y_2, type = "l", col = "black", lwd = 1, lty = 2) #distinguish from baseline
    # Legend
    legend(x = "topleft", ncol = 2, bg= "white", col = c(line_col, "black"), 
           lwd = c(2,1), lty = c(1,2), #y_1 = weight 2, solid; y_2 = weight 1, dashed
           legend = scenario_names)
    
    
  }
  dev.off()
  
}

budget_overall = function(mwbs, scenario_ids){
  #
  components = make_legend_symbol_table()
  n_components = dim(components)[1]
  
  #Initialize table
  summary_table = data.frame(matrix(NA, length(scenario_ids), n_components +1))
  colnames(summary_table) = c("Scenario_id", components$abbrev)
  summary_table$Scenario_id = scenario_ids
  
  for(i in 1:length(mwbs)){
    mwb = mwbs[[i]]
    summary_table[i, 2:(n_components+1)] = colSums(mwb[2:(n_components+1)])
  }
  return(summary_table)
  
}

budget_stat_by_year = function(mwbs, scenario_ids, stat = "mean") {
  #Assumptions regarding monthly water budget dataframes:
  ## assumes monthly stress periods and total number of stress periods divisible by 12
  ## assumes model starts at beginning of water year 1991 (Oct 1990)
  ## assumes all mwb files have same dimensions
  
  components = make_legend_symbol_table()
  n_components = dim(components)[1]
  n_scenarios = length(scenario_ids)
  n_years = dim(mwbs[[i]])[1] / 12 
  
  
  #Initialize table
  summary_table = data.frame(matrix(data = NA, nrow = n_scenarios * n_years, ncol = n_components + 2))
  colnames(summary_table) = c("Scenario_id", "Water_year", components$abbrev)
  summary_table$Scenario_id = sort(rep(scenario_ids, n_years))
  summary_table$Water_year = rep(1991:2011, length(scenario_ids)) 
  
  for(i in 1:length(mwbs)){
    mwb = mwbs[[i]]
    
    mwb$wy = sort(rep(1991:(1991+n_years - 1), 12))
    # mwb$mo = rep(c(10:12, 1:9),n_years)
    # mwb$yr = mwb$wy
    # mwb$yr[mwb$mo > 9] = mwb$wy[mwb$mo > 9] - 1
    
    for(j in 1:n_components){
      comp = components$abbrev[j]
      sum_by_wy = aggregate(mwb[,colnames(mwb) == comp], by = list(mwb$wy), FUN = stat)$x
      summary_table[summary_table$Scenario_id == scenario_ids[i], j + 2] = sum_by_wy
    }
  }
  return(summary_table)
}

budget_stat_by_month = function(mwbs, scenario_ids, stat = "mean"){
  #Assumptions regarding monthly water budget dataframes:
  ## assumes monthly stress periods and total number of stress periods divisible by 12
  ## assumes model starts at beginning of water year 1991 (Oct 1990)
  ## assumes all mwb files have same dimensions
  
  components = make_legend_symbol_table()
  n_components = dim(components)[1]
  n_scenarios = length(scenario_ids)
  
  #Initialize table
  summary_table = data.frame(matrix(data = NA, nrow = n_scenarios * 12, ncol = n_components + 2))
  colnames(summary_table) = c("Scenario_id", "Month", components$abbrev)
  summary_table$Scenario_id = sort(rep(scenario_ids, 12))
  summary_table$Month = rep(1:12, length(scenario_ids)) 
  
  for(i in 1:length(mwbs)){
    mwb = mwbs[[i]]
    
    mwb$wy = sort(rep(1991:(1991+n_years - 1), 12))
    mwb$mo = rep(c(10:12, 1:9),n_years)
    mwb$yr = mwb$wy
    mwb$yr[mwb$mo > 9] = mwb$wy[mwb$mo > 9] - 1
    
    for(j in 1:n_components){
      comp = components$abbrev[j]
      mean_by_mo = aggregate(mwb[,colnames(mwb) == comp], by = list(mwb$mo), FUN = stat)$x
      summary_table[summary_table$Scenario_id == scenario_ids[i], j + 2] = mean_by_mo
    }
  }
  return(summary_table)
}  

barplots_overall = function(scenario_totals){
  components = make_legend_symbol_table()
  n_components = dim(components)[1]
  
  #One barplot for each component
  setwd(pdf_dir)
  pdf("budget_stack_overall.pdf", 5.5,4)
  par(mfrow = c(2,3))
  
  axis_names = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
  y_axis_label= expression(paste("Total modeled volume (km" ^ "3", " x 10" ^ "3",")"))
  # y_axis_label_storage= expression(paste("Total modeled volume (m" ^ "3", " x 10" ^ "9",")"))
  # y_labels = c(rep(y_axis_label, 5), y_axis_label_storage)
  denominator = c(rep(10^9,5),10^9)
  
  #straight barplots
  for(i in 1:n_components){
    if(i %in% c(1, 4)){
      barplot( names = as.factor(scenario_totals$Scenario_id), 
               height = scenario_totals[, i + 1] / denominator[i],
               las = 2,
               ylab = y_axis_label,
               axisnames = axis_names[i],
               col = components$color[i],
               main = components$label[i],
               ylim = c(min(scenario_totals[,2:7]), max(scenario_totals[,2:7])) / denominator[1]
      )
    } else {
      barplot( names = as.factor(scenario_totals$Scenario_id), 
               height = scenario_totals[, i + 1] / denominator[i],
               las = 2,
               ylab = NULL,
               axisnames = axis_names[i],
               col = components$color[i],
               main = components$label[i],
               ylim = c(min(scenario_totals[,2:7]), max(scenario_totals[,2:7])) / denominator[1]
      )
    }
  }
  
  dev.off()
}


# Read in scenario budgets ------------------------------------------------

#Read in SWBM scenario outputs: monthly water budget 
scenario_dir = file.path(proj_dir,"SWBM","up2018_a")
monthly_water_budget_hista = read.table(file.path(scenario_dir,"monthly_water_budget.dat"), header = TRUE)
mwb_daily_et = monthly_water_budget_hista

scenario_dir = file.path(proj_dir,"SWBM","up2018_b")
monthly_water_budget_histb = read.table(file.path(scenario_dir,"monthly_water_budget.dat"), header = TRUE)
mwb_monthly_et = monthly_water_budget_histb


#Read in Scenario As
setwd(paste0(proj_dir,"/sca_output/10_lg_storms"))
monthly_water_budget_sca10 = read.table("monthly_water_budget.dat", header = TRUE)
mwb_sca10 = monthly_water_budget_sca10

setwd(paste0(proj_dir,"/sca_output/5_lg_storms"))
monthly_water_budget_sca5 = read.table("monthly_water_budget.dat", header = TRUE)
mwb_sca5 = monthly_water_budget_sca5

setwd(paste0(proj_dir,"/sca_output/3_lg_storms"))
monthly_water_budget_sca3 = read.table("monthly_water_budget.dat", header = TRUE)
mwb_sca3 = monthly_water_budget_sca3

#Read in Scenario Bs
setwd(paste0(proj_dir,"/scb_output/scb_0.9"))
monthly_water_budget_scb90 = read.table("monthly_water_budget.dat", header = TRUE)
mwb_scb90 = monthly_water_budget_scb90

setwd(paste0(proj_dir,"/scb_output/scb_0.8"))
monthly_water_budget_scb80 = read.table("monthly_water_budget.dat", header = TRUE)
mwb_scb80 = monthly_water_budget_scb80

setwd(paste0(proj_dir,"/scb_output/scb_0.7"))
monthly_water_budget_scb70 = read.table("monthly_water_budget.dat", header = TRUE)
mwb_scb70 = monthly_water_budget_scb70

#Read in Scenario Cs
setwd(paste0(proj_dir,"/scc_output/10_percent_drier"))
monthly_water_budget_scc10 = read.table("monthly_water_budget.dat", header = TRUE)
mwb_scc10 = monthly_water_budget_scc10

setwd(paste0(proj_dir,"/scc_output/20_percent_drier"))
monthly_water_budget_scc20 = read.table("monthly_water_budget.dat", header = TRUE)
mwb_scc20 = monthly_water_budget_scc20

setwd(paste0(proj_dir,"/scc_output/30_percent_drier"))
monthly_water_budget_scc30 = read.table("monthly_water_budget.dat", header = TRUE)
mwb_scc30 = monthly_water_budget_scc30



# Generate scenario plots -------------------------------------------------


# generate  overview plots
plot_water_budget_overview(mwb_hist, "Historical")

plot_water_budget_overview(mwb_sca10, "Scenario A, 10 large storms")
plot_water_budget_overview(mwb_sca5, "Scenario A, 5 large storms")
plot_water_budget_overview(mwb_sca3, "Scenario A, 3 large storms")

plot_water_budget_overview(mwb_scb90, "Scenario B, 90 percent wet season duration")
plot_water_budget_overview(mwb_scb80, "Scenario B, 80 percent wet season duration")
plot_water_budget_overview(mwb_scb70, "Scenario B, 70 percent wet season duration")

plot_water_budget_overview(mwb_scc10, "Scenario C, 10 percent wet season duration")
plot_water_budget_overview(mwb_scc20, "Scenario C, 20 percent wet season duration")
plot_water_budget_overview(mwb_scc30, "Scenario C, 30 percent wet season duration")


#Generate comparison plots - historical vs 3 scenarios
plot_water_budget_comparison(mwb_hist, mwb_sca10, c("Historical", "Scenario A, 10 large storms"))
plot_water_budget_comparison(mwb_hist, mwb_sca5, c("Historical", "Scenario A, 5 large storms"))
plot_water_budget_comparison(mwb_hist, mwb_sca3, c("Historical", "Scenario A, 3 large storms"))

plot_water_budget_comparison(mwb_hist, mwb_scb90, c("Historical", "Scenario B, 90 percent wet season duration"))
plot_water_budget_comparison(mwb_hist, mwb_scb80, c("Historical", "Scenario B, 80 percent wet season duration"))
plot_water_budget_comparison(mwb_hist, mwb_scb70, c("Historical", "Scenario B, 70 percent wet season duration"))

plot_water_budget_comparison(mwb_hist, mwb_scc10, c("Historical", "Scenario C, 10 percent drier dry years"))
plot_water_budget_comparison(mwb_hist, mwb_scc20, c("Historical", "Scenario C, 20 percent drier dry years"))
plot_water_budget_comparison(mwb_hist, mwb_scc30, c("Historical", "Scenario C, 30 percent drier dry years"))

# Generate scenario comparison tables and bar graphs -------------------------------------

#Metrics:
# total recharge
# total groundwater pumped
# total SW extracted
#irrigation onset?

mwbs = list(mwb_daily_et, mwb_monthly_et)
scenario_ids = c("daily_et", "monthly_et")

mwbs = list(mwb_hist, mwb_sca10, mwb_sca5, mwb_sca3, mwb_scb90, mwb_scb80, mwb_scb70, mwb_scc10, mwb_scc20, mwb_scc30)
scenario_ids = c("hist","sca10", "sca05", "sca03", "scb90", "scb80", "scb70", "scc10", "scc20", "scc30")

scenario_totals = budget_overall(mwbs, scenario_ids)
barplots_overall(scenario_totals)

#Tables for latex
#Overall volumes
scenario_totals_km3 = scenario_totals
scenario_totals_km3[,2:7] = round(scenario_totals_km3[,2:7]/(1000^2))
scenario_totals_km3$latex_line_end = "//"
colnames(scenario_totals_km3) = c("ScenarioID", 
                                 "Precipitation", 
                                 "Surface Water Irrigation", 
                                 "Groundwater Irrigation", 
                                 "ET", 
                                 "Recharge", 
                                 "Storage", 
                                 "latex_line_end")

setwd(pdf_dir)
write.table(scenario_totals_km3, "scenario_totals_for_latex.txt",
            col.names = TRUE, row.names = FALSE, sep = " & ", quote = FALSE)

#Changes from historical
change_from_hist = scenario_totals[2:10,]

for(i in 1:dim(change_from_hist)[1]){
  change_from_hist[i,2:7] = round(as.matrix(scenario_totals[i+1,2:7]) / 
                                    as.numeric(scenario_totals[1,2:7]), 2)
}
change_from_hist$latex_line_end = "//"

colnames(change_from_hist) = c("ScenarioID", 
                                  "Precipitation", 
                                  "Surface Water Irrigation", 
                                  "Groundwater Irrigation", 
                                  "ET", 
                                  "Recharge", 
                                  "Storage", 
                                  "latex_line_end")

setwd(pdf_dir)
write.table(change_from_hist, "change_from_hist_for_latex.txt",
            col.names = TRUE, row.names = FALSE, sep = " & ", quote = FALSE)

#Scenario descriptions

scenario_ids = c("hist","sca10, sca05, sca03", "scb90, scb80, scb70", "scc10, scc20, scc30")
scenario_desc = c("Historical", "Scenario A", "Scenario B", "Scenario C")
manipulations = c("--", "Rain arrives in large storms only", 
                  "Reduction in rainy season duration", 
                  "Precipitation reduced in dry years and increased in wet years")
decision_vars = c("--", "Number of large storms", 
              "Fraction of historical rainy season duration", 
              "Reduction in dry year precip")
scenarios_tried = c("--", "10, 5, and 3 large storms",
                    "90, 80, and 70 percent wet season duration",
                    "10, 20, 30 percent drier dry years")

sc_desc = data.frame(scenario_desc, manipulations, decision_vars, scenarios_tried,scenario_ids)
sc_desc$latex_line_end = "backback"

setwd(pdf_dir)
write.table(sc_desc, "scen_desc_for_latex.txt",
            col.names = TRUE, row.names = FALSE, sep = " & ", quote = FALSE)


#Input data source notes

budg = make_legend_symbol_table()
budg$color = NULL
budg$Origin = c("Measured", "Modeled, up to observed maximum monthly flow volume", "Modeled")
budg$uses_crop_demand = c("--","Yes", "Yes", "Yes", )
cor
budg = make_legend_symbol_table()
budg$source[1] = "Four NOAA NCDC rainfall records in Scott Valley"
budg$source[2] = "Four NOAA NCDC rainfall records in Scott Valley"



# Parallel axis plot for scenarios ----------------------------------------


# Scratch work ------------------------------------------------------------


barplot(t(data.matrix(scenario_totals[,2:7])), 
        names = scenario_totals$Scenario_id, 
        col = components$color,
        legend = components$abbrev)

# 

barplot(t(data.matrix(scenario_totals[,2:7])), 
        main=,
        names = scenario_totals$Scenario_id, 
        horiz = TRUE,
        las=2,
        # xlim=c(-30,30),
        col = 'blue',beside = FALSE)


data_for_gg = gather(scenario_totals, "budget_component", -"Scenario_id",
                     value = "total_volume_m3")

ggplot(data = data_for_gg, aes(Scenario_id, budget_component, y=total_volume_m3)) +
  geom_col(aes(fill = budget_component), position = position_stack(reverse = TRUE)) +
  geom_hline(yintercept = 0)

"C:/Users/ckouba/Documents/UCD/_Coursework/2019_Q1_Winter/ECI273_WatResSysEng/project/project_data_files/SV_Map.JPG"

library(convertGraph)
install.phantom("C:/Users/ckouba/Documents/UCD/_Coursework/2019_Q1_Winter/ECI273_WatResSysEng/project/project_data_files")
convertGraph(from = "C:/Users/ckouba/Documents/UCD/_Coursework/2019_Q1_Winter/ECI273_WatResSysEng/project/project_data_files/SV_Map.JPG", 
             to = "C:/Users/ckouba/Documents/UCD/_Coursework/2019_Q1_Winter/ECI273_WatResSysEng/project/project_data_files/SV_Map.pdf",
             path = "C:/Users/ckouba/Documents/UCD/_Coursework/2019_Q1_Winter/ECI273_WatResSysEng/project/project_data_files")
