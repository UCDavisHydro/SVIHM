# Plot Scenario Comparisons

# Input: two monthly_water_budget.dat type files, (commonly, one historical and one a scenario)
# Output: One pdf with:
## Overview of historical
## Overview of scenario
## Historical-scenario comparisons for each water budget component
## Overall water budget term difference table
## Overall water budget term difference table by month

library(ggplot2)
library(data.table) #melt function
library(plotly)
library(rstudioapi)
library(gridExtra)

rm(list = ls())

#Define directories
svihm_dir = dirname(dirname(dirname(getActiveDocumentContext()$path ))) #Outer SVIHM folder
scenario_dir = file.path(svihm_dir,"SWBM")
ref_data_dir = file.path(svihm_dir, "SVIHM_Input_Files", "reference_data")
output_dir = file.path(svihm_dir,"SVIHM_Input_Files","Scenario_Development")
# pdf_dir = "C:/Users/Claire/Documents/UCD/Presentations or Talks or Workshops/2019.09.17 GRA WGC/Figures"
pdf_dir = file.path(scenario_dir,"")#,"comparison_pdfs")


# Functions ---------------------------------------------------------------

# make_legend_symbol_table = function(){
#   label = c("Precipitation","Surface water irrigation", "Groundwater irrigation", "Evapotranspiration","Recharge","Change in storage")
#   color = c("lightblue", "deepskyblue3", "deepskyblue4", "goldenrod", "green4", "black")
#   components = data.frame(label,color)
#   components$color = as.character(components$color)
#   components$abbrev = c("Precip", "SW_Irr", "GW_Irr", "ET", "Recharge", "Storage")
#   
#   return(components)
# }

# #Match Gus' SWBM colors
# make_legend_symbol_table = function(){
#   
#   # SWBM_flux_labels = c('Precipitation','ET','SW Irrigation', 'Recharge',   'GW Irrigation','Storage')
#   # SWBM_colors = c('lightblue1',      'red', 'darkcyan',      'mediumblue', 'darkgreen',    'goldenrod')
#   SWBM_flux_labels = c('Precipitation','Surface water Irrigation','Groundwater Irrigation','ET', 'Recharge','Change in storage')
#   SWBM_colors = c('lightblue1', 'darkcyan', 'darkgreen','mediumblue','goldenrod' )
#   
#   # label = c("Precipitation","Surface water irrigation", "Groundwater irrigation", "Evapotranspiration","Recharge","Change in storage")
#   # color = c("lightblue", "deepskyblue3", "deepskyblue4", "goldenrod", "green4", "black")
#   components = data.frame(label = SWBM_flux_labels,color = SWBM_colors)
#   components$color = as.character(components$color)
#   components$abbrev = c("Precip", "SW_Irr", "GW_Irr", "ET", "Recharge", "Storage")
#   
#   return(components)
# }

#Make more visible for powerpoint
make_legend_symbol_table = function(){

  label = c("Precipitation","Surface water irrigation", "Groundwater irrigation", "Evapotranspiration","Recharge","Change in storage")
  color = c("deepskyblue2", "steelblue4", "black", "orange2", "green4", "darkgray")
  components = data.frame(label,color)
  components$color = as.character(components$color)
  components$abbrev = c("Precip", "SW_Irr", "GW_Irr", "ET", "Recharge", "Storage")

  return(components)
}

# # SWBM plotting colors - coordinate with budget figure
# make_legend_symbol_table = function(){
# 
#   label = c("Precipitation","Surface water irrigation", "Groundwater irrigation", "Evapotranspiration","Recharge","Change in storage")
#   color = c("deepskyblue2", "blue", "midnightblue", "orange2", "green4", "mistyrose")
#   components = data.frame(label,color)
#   components$color = as.character(components$color)
#   components$abbrev = c("Precip", "SW_Irr", "GW_Irr", "ET", "Recharge", "Storage")
# 
#   return(components)
# }


plot_water_budget_overview = function(mwb, scenario_name, output_type = "pdf"){
  #Label budget components
  P = mwb$Precip
  SW = mwb$SW_Irr
  GW = mwb$GW_Irr
  ET = mwb$ET
  R = mwb$Recharge
  S = mwb$Storage
  
  #Plot parameters
  vol_lim = c(min(mwb[2:7]), max(mwb[2:7]))
  x_lim = c(240, dim(mwb)[1])# x_lim = c(1, dim(mwb)[1])
  #Symbols and legend labels
  components = make_legend_symbol_table()

  #OUtput file setup
  setwd(pdf_dir)
  if(output_type == "pdf"){
    pdfname = paste0(scenario_name, "_overview.pdf")
    pdf(pdfname, 7.5,9)
    par(mfrow=c(2,1))
  } else if(output_type =="png"){
    pngname = paste0(scenario_name, "plot1_overview.png")
    png(pngname, width = 8, height = 5.5, units = "in", res = 200) 
  }
  
  
  #Initialize Panel 1
  plot(x = NA, y = NA, xlim = x_lim,  ylim = vol_lim,
       axes = FALSE, xlab = "Year", ylab = "", #Suppress y-axis label text
       main = paste(scenario_name, "monthly water budget"),
       sub = "Negative values indicate water leaving modeled soil volume", cex = 0.8)
  
  #Manual stylized grid lines (plotted first to not obscure data)
  abline(v = seq(from = 4, to = 336, by=12),
         lty = 3, lwd = 0.5, col = "lightgray")
  abline(h = pretty(vol_lim), v = seq(from = 52, to = 336, by=12),#60), # Jan of 95, 00, 05, 10
         lty = 1, col = "darkgray")
  abline(h = 0, col = "black")
  #Manual y-label and axis labels, and a box (plot border)
  mtext(side = 2, expression(paste("Water volume (cubic meters x 10"^"7", ")")), 
        las = 0, line = 2) #manual y-label
  axis(1, at = seq(from = 4, to = 336, by=12), labels = 1991:2018)
  axis(2, at = pretty(vol_lim), pretty(vol_lim)/10^7)
  box()
  
  #Plot water budget components over time. Ordered to optimize visibility
  lines(R, col = components$color[5], lwd = 2.5) 
  lines(P, col = components$color[1], lwd = 2.5)
  lines(GW, col = components$color[3], lwd = 2.5)
  lines(SW, col = components$color[2], lwd = 2.5)
  lines(ET, col = components$color[4], lwd = 2.5)
  
  #New png file if making pngs
  if(output_type =="png"){
    dev.off()
    pngname = paste0(scenario_name, "plot2_overview.png")
    png(pngname, width = 9, height = 5, units = "in", res = 200) #plot for a poster
  }
  
  #Initialize Panel 2
  plot(x = NA, y = NA, xlim = x_lim,  ylim = vol_lim,
       axes = FALSE, xlab = "Year", ylab = "", #Suppress y-axis text
       main = paste0(scenario_name, " monthly change in soil water storage"),
       sub = "Negative values indicate water leaving modeled soil volume", cex = 0.8)
  #Manual stylized grid lines (plotted first to not obscure data)
  abline(v = seq(from = 4, to = 336, by=12),
         lty = 3, lwd = 0.5, col = "lightgray")
  abline(h = pretty(vol_lim), v = seq(from = 52, to = 336, by=60), # Jan of 95, 00, 05, 10
         lty = 1, col = "darkgray")
  abline(h = 0, col = "black")
  #Manual y-label and axis labels
  mtext(side = 2, expression(paste("Change in storage (cubic meters x 10"^"7", ")")), 
        las = 0, line = 2) #manual y-label
  axis(1, at = seq(from = 4, to = 336, by=12), #Jan of years ending in 5 and 0
       labels = 1991:2018)
  axis(2, at = pretty(vol_lim), pretty(vol_lim)/10^7)
  box()
  
  #Plot change in storage over time
  lines(S, type = "l", col = components$color[6], lwd = 2.5)
  
  #Add legend to the bottom panel
  legend(x = "topleft", lwd = 2, ncol = 2, bg= "white",
         col = components[,2], legend = components[,1])
  
  dev.off()
}

plot_water_budget_comparison = function(mwb1, mwb2, scenario_names){
  #x-values are stress periods (months; 336 months in model). 
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
  n_years = dim(mwbs[[1]])[1] / 12 
  scenario_ids = unlist(scenario_ids)
  
  #Initialize table
  summary_table = data.frame(matrix(data = NA, nrow = n_scenarios * n_years, ncol = n_components + 2))
  colnames(summary_table) = c("Scenario_id", "Water_year", components$abbrev)
  summary_table$Scenario_id = sort(rep(scenario_ids, n_years))
  summary_table$Water_year = rep(1991:2018, length(scenario_ids)) 
  
  for(i in 1:length(mwbs)){
    mwb = mwbs[[i]]
    
    mwb$wy = sort(rep(1991:(1991+n_years - 1), 12))

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
  # pdf("budget_stack_overall.pdf", 6,4)
  png("budget_stack_overall.png", 6,3, units = "in", res = 200)
  par(mfrow = c(2,3))
  
  axis_names = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
  # y_axis_label= expression(paste("Total modeled volume (km" ^ "3", " x 10" ^ "3",")"))
  y_axis_label = "Total modeled volume (TAF)"
  # y_axis_label_storage= expression(paste("Total modeled volume (m" ^ "3", " x 10" ^ "9",")"))
  # y_labels = c(rep(y_axis_label, 5), y_axis_label_storage)
  # denominator = c(rep(10^9,5),10^9)
  m3_to_taf_multiplier = 3.28084 * 1/4046.36 * 1/1000 #m to ft * m^2 to acre * AF to TAF
  denominator = 1/rep(m3_to_taf_multiplier, 6)
  
  #straight barplots
  for(i in 1:n_components){
    if(i %in% c(1, 4)){
      barplot( names = as.factor(scenario_totals$Scenario_id), 
               height = scenario_totals[, i + 1] / denominator[i],
               las = 2,
               cex.axis = 0.8,
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
               cex.axis = 0.8,
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

barplots_comparison = function(scenario_totals){
  components = make_legend_symbol_table()
  n_components = dim(components)[1]
  
  #One barplot for each component
  setwd(pdf_dir)
  # pdf("budget_stack_comparison.pdf", 6,4)
  png("budget_stack_comparison.png", 6,4, units = "in", res = 200)
  par(mfrow = c(2,3))
  
  axis_names = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
  # y_axis_label= expression(paste("Total modeled volume (km" ^ "3", " x 10" ^ "3",")"))
  y_axis_label = "Total modeled volume (TAF)"
  # y_axis_label_storage= expression(paste("Total modeled volume (m" ^ "3", " x 10" ^ "9",")"))
  # y_labels = c(rep(y_axis_label, 5), y_axis_label_storage)
  # denominator = c(rep(10^9,5),10^9)
  m3_to_taf_multiplier = 3.28084 * 1/4046.36 * 1/1000 #m to ft * m^2 to acre * AF to TAF
  denominator = 1/rep(m3_to_taf_multiplier, 6)
  
  #straight barplots
  for(i in 1:n_components){
    if(i %in% c(1, 4)){
      barplot( names = as.factor(scenario_totals$Scenario_id), 
               height = scenario_totals[, i + 1] / denominator[i],
               las = 2,
               cex.axis = 0.8,
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
               cex.axis = 0.8,
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

monthly_water_budget_hist = read.table(file.path(scenario_dir,"hist","monthly_water_budget.dat"), header = TRUE)
mwb_hist = monthly_water_budget_hist

#Read in Scenario A (extreme days algorithm; Nov 2019)
monthly_water_budget_sca_95_07 = read.table(file.path(scenario_dir,"pvar_a_extr_95_07","monthly_water_budget.dat"), header = TRUE)
mwb_sca_95_07 = monthly_water_budget_sca_95_07


#Read in Scenario As
monthly_water_budget_sca10 = read.table(file.path(scenario_dir,"pvar_a10","monthly_water_budget.dat"), header = TRUE)
mwb_sca10 = monthly_water_budget_sca10

monthly_water_budget_sca05 = read.table(file.path(scenario_dir,"pvar_a05","monthly_water_budget.dat"), header = TRUE)
mwb_sca05 = monthly_water_budget_sca05

monthly_water_budget_sca03 = read.table(file.path(scenario_dir,"pvar_a03","monthly_water_budget.dat"), header = TRUE)
mwb_sca03 = monthly_water_budget_sca03

#Read in Scenario Bs
monthly_water_budget_scb70 = read.table(file.path(scenario_dir,"pvar_b70","monthly_water_budget.dat"), header = TRUE)
mwb_scb70 = monthly_water_budget_scb70

monthly_water_budget_scb80 = read.table(file.path(scenario_dir,"pvar_b80","monthly_water_budget.dat"), header = TRUE)
mwb_scb80 = monthly_water_budget_scb80

monthly_water_budget_scb90 = read.table(file.path(scenario_dir,"pvar_b90","monthly_water_budget.dat"), header = TRUE)
mwb_scb90 = monthly_water_budget_scb90

#Read in Scenario Cs
monthly_water_budget_scc10 = read.table(file.path(scenario_dir,"pvar_c10","monthly_water_budget.dat"), header = TRUE)
mwb_scc10 = monthly_water_budget_scc10

monthly_water_budget_scc20 = read.table(file.path(scenario_dir,"pvar_c20","monthly_water_budget.dat"), header = TRUE)
mwb_scc20 = monthly_water_budget_scc20

monthly_water_budget_scc30 = read.table(file.path(scenario_dir,"pvar_c30","monthly_water_budget.dat"), header = TRUE)
mwb_scc30 = monthly_water_budget_scc30



# Generate scenario plots -------------------------------------------------


make_legend_symbol_table = function(){
  
  label = c("Precipitation","Surface water irrigation", "Groundwater irrigation", "Evapotranspiration","Recharge","Change in storage")
  color = c("deepskyblue", "blue", "gray36", "orange2", "green4", "darkgray")
  components = data.frame(label,color)
  components$color = as.character(components$color)
  components$abbrev = c("Precip", "SW_Irr", "GW_Irr", "ET", "Recharge", "Storage")
  
  return(components)
}

# generate  overview plots
plot_water_budget_overview(mwb_hist, "Historical", output_type = "png")
# plot_water_budget_overview(mwb_sca_95_07, "Scenario A, 95 pctile, 7 pct increase", output_type = "png")

plot_water_budget_overview(mwb_sca10, "Scenario A, 10 large storms")
plot_water_budget_overview(mwb_sca05, "Scenario A, 5 large storms", output_type = "png")
plot_water_budget_overview(mwb_sca03, "Scenario A, 3 large storms")

plot_water_budget_overview(mwb_scb90, "Scenario B, 90 percent wet season duration")
plot_water_budget_overview(mwb_scb80, "Scenario B, 80 percent wet season duration", output_type = "png")
plot_water_budget_overview(mwb_scb70, "Scenario B, 70 percent wet season duration")

plot_water_budget_overview(mwb_scc10, "Scenario C, 10 percent wet season duration")
plot_water_budget_overview(mwb_scc20, "Scenario C, 20 percent wet season duration", output_type = "png")
plot_water_budget_overview(mwb_scc30, "Scenario C, 30 percent wet season duration")


#Generate comparison plots - historical vs 3 scenarios
plot_water_budget_comparison(mwb_hist, mwb_sca10, c("Historical", "Scenario A, 10 large storms"))
plot_water_budget_comparison(mwb_hist, mwb_sca05, c("Historical", "Scenario A, 5 large storms"))
plot_water_budget_comparison(mwb_hist, mwb_sca03, c("Historical", "Scenario A, 3 large storms"))

plot_water_budget_comparison(mwb_hist, mwb_scb90, c("Historical", "Scenario B, 90 percent wet season duration"))
plot_water_budget_comparison(mwb_hist, mwb_scb80, c("Historical", "Scenario B, 80 percent wet season duration"))
plot_water_budget_comparison(mwb_hist, mwb_scb70, c("Historical", "Scenario B, 70 percent wet season duration"))

plot_water_budget_comparison(mwb_hist, mwb_scc10, c("Historical", "Scenario C, 10 percent drier dry years"))
plot_water_budget_comparison(mwb_hist, mwb_scc20, c("Historical", "Scenario C, 20 percent drier dry years"))
plot_water_budget_comparison(mwb_hist, mwb_scc30, c("Historical", "Scenario C, 30 percent drier dry years"))




# Bar graphs -------------------------------------

#Metrics:
# total recharge
# total groundwater pumped
# total SW extracted
#irrigation onset?

# mwbs = list(mwb_hist, mwb_sca10, mwb_sca05, mwb_sca03, mwb_scb90, mwb_scb80, mwb_scb70, mwb_scc10, mwb_scc20, mwb_scc30)
# scenario_ids = c("hist","sca10", "sca05", "sca03", "scb90", "scb80", "scb70", "scc10", "scc20", "scc30")
# mwbs = list(mwb_hist, mwb_scb70, mwb_scb80, mwb_scb90)
# scenario_ids = c("hist","scb70", "scb80", "scb90")
mwbs = list(mwb_hist, mwb_sca_95_07)
scenario_ids = c("hist","sca_95_07")


scenario_totals = budget_overall(mwbs, scenario_ids)
# barplots_overall(scenario_totals)

#dry, wet, average years
yearly_budgets = budget_stat_by_year(mwbs = mwbs, scenario_ids = scenario_ids, stat = "sum")

wet_year = 2006; dry_year = 2001; avg_year = 2015
#Make subset tables for the barplot graphic. Eliminate water year column
scenario_totals_wet = yearly_budgets[yearly_budgets$Scenario_id %in% scenario_ids &
                                       yearly_budgets$Water_year == wet_year, 
                                     colnames(yearly_budgets)[colnames(yearly_budgets)!="Water_year"]]
scenario_totals_avg = yearly_budgets[yearly_budgets$Scenario_id %in% scenario_ids &
                                       yearly_budgets$Water_year == avg_year, 
                                     colnames(yearly_budgets)[colnames(yearly_budgets)!="Water_year"]]
scenario_totals_dry = yearly_budgets[yearly_budgets$Scenario_id %in% scenario_ids &
                                       yearly_budgets$Water_year == dry_year, 
                                     colnames(yearly_budgets)[colnames(yearly_budgets)!="Water_year"]]

#combine into one df, for barplots (GRA poster 2019)
scenario_totals_all = rbind(scenario_totals_wet, scenario_totals_dry, scenario_totals_avg, scenario_totals)
scta = scenario_totals_all
scta$year_type = rep(c("Wet (2006)", "Dry (2001)", "Avg (2015)", "Overall"), each = 2)

scta_m = melt(scta, id.vars = c("Scenario_id", "year_type"))

#initialize table of change factors
scta_m_chg = data.frame(matrix(data = NA, nrow = 0, ncol = 5)) ; colnames(scta_m_chg) = c(colnames(scta_m), "percent_chg_fm_hist")
for(yrtype in c("Wet (2006)", "Dry (2001)", "Avg (2015)", "Overall")){
  for(component in c("GW_Irr", "Recharge", "SW_Irr")){
    scta_m_subset = scta_m[scta_m$year_type == yrtype & scta_m$variable == component,]
    scta_m_hist = scta_m_subset[scta_m_subset$Scenario_id == "hist",]
    scta_m_scenarios = scta_m_subset[scta_m_subset$Scenario_id != "hist",]
    
    scta_m_scenarios$percent_chg_fm_hist = (scta_m_scenarios$value - scta_m_hist$value)/scta_m_hist$value
    scta_m_chg = rbind(scta_m_chg, scta_m_scenarios)
  }
}

scta_m_chg$year_type = factor(scta_m_chg$year_type, levels=c("Wet (2006)", "Avg (2015)", "Dry (2001)", "Overall"))

#add color coding for each scenario
scid_colors = c(#"darkorchid2", "darkorchid3", "darkorchid4", 
  "khaki2", "khaki3", "khaki4",
  "darkseagreen2","darkseagreen3","darkseagreen4",
                # "darkolivegreen2","darkolivegreen3","darkolivegreen4",
                "lightpink1","lightpink3","lightpink4")

#Attempts to combine barplots: by year type, by component, by scenario ID
scta_m_gw = scta_m_chg[scta_m_chg$variable == "GW_Irr",]
scta_m_rch = scta_m_chg[scta_m_chg$variable == "Recharge",]
scta_m_sw = scta_m_chg[scta_m_chg$variable == "SW_Irr",]

gw = ggplot(scta_m_gw, aes(factor(year_type), percent_chg_fm_hist*100, fill = Scenario_id)) +
  ylim(-10, 10)+#ylim(-20, 55)+
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Change from Historical Groundwater Pumping", y = "Percent change", x = NULL)+
  scale_fill_manual(values=scid_colors)+
  theme_bw()

sw = ggplot(scta_m_sw, aes(factor(year_type), percent_chg_fm_hist*100, fill = Scenario_id)) +
  ylim(-10, 10)+#ylim(-20, 55)+
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Change from Historical Surface Water Irrigation", y = "Percent change", x = NULL)+
  scale_fill_manual(values=scid_colors)+
  theme_bw()

rch = ggplot(scta_m_rch, aes(factor(year_type), percent_chg_fm_hist*100, fill = Scenario_id)) +
  ylim(-10, 10)+#ylim(-20, 55)+
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Change from Historical Recharge", y = "Percent change", x = NULL)+
  scale_fill_manual(values=scid_colors)+
  theme_bw()

png("Scenario Results.png", width = 12, height = 10, units = "in", res = 300)
grid.arrange(rch, gw, sw,  nrow = 3)
dev.off()

grid.arrange(
  p3,
  p3,
  p3,
  nrow = 1,
  top = "Title of the page",
  bottom = textGrob(
    "this footnote is right-justified",
    gp = gpar(fontface = 3, fontsize = 9),
    hjust = 1,
    x = 1
  )
)

# Tables for latex --------------------------------------------------------

mwbs = list(mwb_hist, mwb_sca10, mwb_sca05, mwb_sca03, mwb_scb90, mwb_scb80, mwb_scb70, mwb_scc10, mwb_scc20, mwb_scc30)
scenario_ids = c("hist","sca10", "sca05", "sca03", "scb90", "scb80", "scb70", "scc10", "scc20", "scc30")
# mwbs = list(mwb_hist, mwb_sca05, mwb_scb80, mwb_scc20)
# scenario_ids = c("hist","sca05", "scb80", "scc20")

scenario_totals = budget_overall(mwbs, scenario_ids)

#Tables for latex
#Overall volumes
# scenario_totals_km3 = scenario_totals
m3_to_taf_multiplier = 3.28084 * 1/4046.36 * 1/1000 #m to ft * m^2 to acre * AF to TAF
scenario_totals_taf=scenario_totals
scenario_totals_taf[,2:7] = round(scenario_totals[,2:7] * m3_to_taf_multiplier)

# scenario_totals_km3[,2:7] = round(scenario_totals_km3[,2:7]/(1000^2))
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

#Changes from historical - overall water budget terms
change_from_hist = scenario_totals[2:10,]
for(i in 1:dim(change_from_hist)[1]){
  change_from_hist[i,2:7] = round(as.matrix(scenario_totals[i+1,2:7]) / 
                                        as.numeric(scenario_totals[1,2:7]), 2)
}

# #Changes from historical - dry, avg, and wet years
# change_from_hist_dry = scenario_totals_dry[2:10,]
# change_from_hist_avg = scenario_totals_avg[2:10,]
# change_from_hist_wet = scenario_totals_wet[2:10,]
# 
# for(i in 1:dim(change_from_hist)[1]){
#   change_from_hist_dry[i,2:7] = round(as.matrix(scenario_totals_dry[i+1,2:7]) / 
#                                     as.numeric(scenario_totals_dry[1,2:7]), 2)
#   change_from_hist_avg[i,2:7] = round(as.matrix(scenario_totals_avg[i+1,2:7]) / 
#                                         as.numeric(scenario_totals_avg[1,2:7]), 2)
#   change_from_hist_wet[i,2:7] = round(as.matrix(scenario_totals_wet[i+1,2:7]) / 
#                                         as.numeric(scenario_totals_wet[1,2:7]), 2)
# }
# 
# #Save as csvs for excel formatting
# scenario_order = c('sca10','sca05','sca03',
#                    'scb90','scb80','scb70',
#                    'scc10','scc20','scc30')
# #Make sure they're in order for the poster
# change_from_hist_dry$Scenario_id = factor(change_from_hist_dry$Scenario_id, levels =scenario_order)
# change_from_hist_dry=change_from_hist_dry[order(change_from_hist_dry$Scenario_id),]
# change_from_hist_wet$Scenario_id = factor(change_from_hist_wet$Scenario_id, levels =scenario_order)
# change_from_hist_wet=change_from_hist_wet[order(change_from_hist_wet$Scenario_id),]
# change_from_hist_avg$Scenario_id = factor(change_from_hist_avg$Scenario_id, levels =scenario_order)
# change_from_hist_avg=change_from_hist_avg[order(change_from_hist_avg$Scenario_id),]
# change_from_hist$Scenario_id = factor(change_from_hist$Scenario_id, levels =scenario_order)
# change_from_hist=change_from_hist[order(change_from_hist$Scenario_id),]
# 
# 
# setwd(pdf_dir)
# write.csv(change_from_hist, "change from hist, total model period.csv")
# write.csv(change_from_hist_dry, "change from hist_dry year.csv")
# write.csv(change_from_hist_wet, "change from hist_wet year.csv")
# write.csv(change_from_hist_avg, "change from hist_avg year.csv")

#Format for latex
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


# Plot dry, wet year hist vs altered precip -------------------------------

#To do: make rainfall_analysis_and_scenarios source-able
source('~/GitHub/SVIHM/SVIHM_Input_Files/Rainfall_Analysis_and_Scenarios.R')

#Generate scenario a
P_sca05 = generate_scenario_a(select(ppt_hist, date, precip_m), num_large_storms=5)

#Generate scenario B 
rain_day_threshold = 0 # Any rain counts as a rainy day
rainy_season_bounds = c(0.1, 0.9)
fraction_season_length = 0.8 #try 90, 80, 70
scenario_folder_name = "pvar_b80"

scb_tables = generate_scenario_b(select(ppt_hist, date, precip_m), fraction_season_length)

rain_stats_scb80 = scb_tables[[1]]
P_scb80 = scb_tables[[2]]

rs_hist = rainy_season_table(select(ppt_hist, date, precip_m), rainy_season_bounds)
rs_scb80 = rainy_season_table(select(P_scb80, date, precip_m), rainy_season_bounds)

#Generate Scenario C

rain_day_threshold = 0 # Any rain counts as a rainy day
rainy_season_bounds = c(0.1, 0.9) #fraction of total precip that defines wet season
dry_wet_thresholds = c(1.0, 1.0) # mult. of mean annual precip. dry must <= wet threshold.
percent_drier = 20 #try 10, 20, 30

P = select(ppt_hist, date, precip_m)
S = stm_hist

scc = generate_scenario_c( P, S, percent_drier, dry_wet_thresholds)
P_scc20 = scc[[1]]
S_scc20 = scc[[2]]

plot_altered_rainfall_comp=function(sc1_name, sc2_name, sc_letter, P1, P2, wy){
  x_limits = as.Date(c(paste0(wy-1,"-10-01"), paste0(wy,"-09-30")))
  plot(P1$date, P1$precip_m * 100/2.54, col = NA,
       xlim = x_limits, xlab = paste("Month in WY", wy), ylab = NA, #"Daily Precip (in)",
       # main = paste(sc1_name, "vs", sc2_name)
       main = NULL)

  # color-code plot background (with a very wide line) for wet or dry
  if(wy==dry_year){abline(v=mean(x_limits), lwd = 1000, col = rgb(1,.92,.8,.5))} #blanchedalmond
  if(wy==wet_year){{abline(v=mean(x_limits), lwd = 1000, col = rgb(.68,.85,.9, 0.5))}}#lightblue
  #Plot historical record
  lines(P1$date, P1$precip_m * 100/2.54, col = "deepskyblue2", type = "l", lwd = 2)
  #Plot altered scenario
  altered_precip = colnames(P2)[colnames(P2) %in% c("sca", "scb", "scc")]
  lines(P2$date, P2[,altered_precip] * 100/2.54, col = "black", type = "l", lwd = 1)
  box()
  #Add custom y-axes. If it's a wet year (left column), add the scenario name.
  # if(wy==wet_year){mtext(text = sc2_name, side = 2, line = 3, cex = .8)}
  mtext(text = "Daily Precip (in)", side = 2, line = 2, cex = .7)
  
  
  if(sc_letter == "B"){
    rs1 = rainy_season_table(select(P1, date, precip_m), rainy_season_bounds)
    rs1_duration = rs1$duration_days[rs1$wat_yr == wy]
    p2_cols = c("date", altered_precip)
    rs2 = rainy_season_table(data.frame(date=P2$date, precip_m=P2[,altered_precip]), rainy_season_bounds)
    rs2_duration = rs2$duration_days[rs2$wat_yr == wy]
    
    rs1_dates=as.Date(paste0(wy-1,"-10-01")) + c(rs1$wy_day_start[rs1$wat_yr==wy], rs1$wy_day_end[rs1$wat_yr==wy])
    rs2_dates=as.Date(paste0(wy-1,"-10-01")) + c(rs2$wy_day_start[rs2$wat_yr==wy], rs2$wy_day_end[rs2$wat_yr==wy])
    abline(v=rs1_dates, col = c("deepskyblue2"), lty = 2, lwd = 1)  
    abline(v=rs2_dates, col = c("black"), lty = 2, lwd =  1)  
  # }
  # if(sc_letter == "B" & wy == wet_year){
    legend(x = "topright", cex=1.4,lwd = c(2,2,1, 1), lty = c(1,1,2,2), 
           col = rep(c("deepskyblue2", "black"),2),
           legend = c(paste0("Historical record, water year ", wy),
                      "Altered record",
                      paste0("Historical rainy season (",rs1_duration," days)"), 
                      paste0("Altered rainy season (",rs2_duration," days)")))
  }
    

  
  
}


# plot_altered_rainfall_comp(P1 = ppt_hist, sc1_name = "Historical", 
#                            # P2= P_sca05,   sc2_name = "Scenario A, Only 5 Storms", sc_letter = "A",
#                            P2= P_scb80,  sc2_name = "Scenario B, 80% Rainy Season Duration", sc_letter = "B",
#                            # P2= P_scc20,   sc2_name = "Scenario C, Dry Years 20% Drier",  sc_letter = "C",
#                            wy = 2015)

Dry_Avg_Wet_Yrs = c(2001,2015,2006)

#6 plot figure
# sca, scb, scc extremes. wet and dry years. 
# dry_year = 2001; wet_year = 2006
dry_year = 2015; wet_year = 2016

draft_num = 9
png(file.path(pdf_dir,paste0("altered_rainfall_6plot_",draft_num,".png")), 
    width = 11, height = 7, units = "in", res = 200)
par(mfrow = c(3,2), mar=c(4,4,1,2))
plot_altered_rainfall_comp(P1 = ppt_hist, sc1_name = "Historical", 
                           P2= P_sca05,   sc2_name = "Scenario A, Only 5 Storms", sc_letter = "A",
                           # P2= P_scb80,  sc2_name = "Scenario B, 80% Rainy Season Duration", sc_letter = "B",
                           # P2= P_scc20,   sc2_name = "Scenario C, Dry Years 20% Drier",  sc_letter = "C",
                           wy = wet_year)
plot_altered_rainfall_comp(P1 = ppt_hist, sc1_name = "Historical", 
                           P2= P_sca05,   sc2_name = "Scenario A, Only 5 Storms", sc_letter = "A",
                           # P2= P_scb80,  sc2_name = "Scenario B, 80% Rainy Season Duration", sc_letter = "B",
                           # P2= P_scc20,   sc2_name = "Scenario C, Dry Years 20% Drier",  sc_letter = "C",
                           wy = dry_year)

plot_altered_rainfall_comp(P1 = ppt_hist, sc1_name = "Historical", 
                           # P2= P_sca05,   sc2_name = "Scenario A, Only 5 Storms", sc_letter = "A",
                           P2= P_scb80,  sc2_name = "Scenario B, 80% Rainy Season Duration", sc_letter = "B",
                           # P2= P_scc20,   sc2_name = "Scenario C, Dry Years 20% Drier",  sc_letter = "C",
                           wy = wet_year)
plot_altered_rainfall_comp(P1 = ppt_hist, sc1_name = "Historical", 
                           # P2= P_sca05,   sc2_name = "Scenario A, Only 5 Storms", sc_letter = "A",
                           P2= P_scb80,  sc2_name = "Scenario B, 80% Rainy Season Duration", sc_letter = "B",
                           # P2= P_scc20,   sc2_name = "Scenario C, Dry Years 20% Drier",  sc_letter = "C",
                           wy = dry_year)

plot_altered_rainfall_comp(P1 = ppt_hist, sc1_name = "Historical", 
                           # P2= P_sca05,   sc2_name = "Scenario A, Only 5 Storms", sc_letter = "A",
                           # P2= P_scb80,  sc2_name = "Scenario B, 80% Rainy Season Duration", sc_letter = "B",
                           P2= P_scc20,   sc2_name = "Scenario C, Dry Years 20% Drier",  sc_letter = "C",
                           wy = wet_year)
plot_altered_rainfall_comp(P1 = ppt_hist, sc1_name = "Historical", 
                           # P2= P_sca05,   sc2_name = "Scenario A, Only 5 Storms", sc_letter = "A",
                           # P2= P_scb80,  sc2_name = "Scenario B, 80% Rainy Season Duration", sc_letter = "B",
                           P2= P_scc20,   sc2_name = "Scenario C, Dry Years 20% Drier",  sc_letter = "C",
                           wy = dry_year)

dev.off()


#Whiplash effect
# comp_wbs = c("hist","sca10", "sca05", "sca03", "scb90", "scb80", "scb70", "scc10", "scc20", "scc30")
start_date = as.Date("1990-10-01"); end_date = as.Date("2018-09-30")
comp_mwbs = list(mwb_hist)#,mwb_sca05, mwb_scb80, mwb_scc20)
mwb = mwb_hist

wy_budget = budget_stat_by_year(mwbs=list(mwb_hist, mwb_sca05, mwb_scb80, mwb_scc20), stat="sum",
                                scenario_ids = list("hist","sca05","scb80", "scc20"))

components = make_legend_symbol_table()
scen = "scc20"

for(i in 1:dim(components)[1]){
  component = components$abbrev[i]
  selector = wy_budget$Scenario_id=="hist"
  plot(wy_budget$Water_year[selector], 
       wy_budget[selector, colnames(wy_budget)==component], 
       col = "darkgray", type = "l", lwd = 2, ylim = c(0, 2e8),
       main = paste("Hist,",scen,"comparison"))
  
  selector = wy_budget$Scenario_id==scen
  lines(wy_budget$Water_year[selector], wy_budget$Precip[selector], 
        col = components$color[components$abbrev=="Precip"],
        lwd =2)
}


# MODFLOW plots: streams sim v obs ----------------------------------------

# FJ gauge
.




# Scratch work ------------------------------------------------------------

#which years should we use as examples of dry, wet, and average?

wy_budget = budget_stat_by_year(mwbs=list(mwb_hist),
                                stat="sum",
                                scenario_ids = list("hist"))

#Add water year type
wy_budget$wy_type = wy_type$yr_type[match(wy_budget$Water_year, wy_type$wy)]
wy_budget_sorted = wy_budget[order(wy_budget$Precip),]
wy_budget_sorted$order = 1:dim(wy_budget_sorted)[1]
#wet year: 2006. 2003 is wetter but is a clear outlier. Other wet years include 2005, 1995, and 2017.
# dry year: 2001. 1994 is drier but is an outlier. Other dry years include 2018, 1991, 2012, 2014, and 1992.
# normal year: 2010. Prettyyyy normal. 2015 is another good one. 






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

# Parallel axis plot for scenarios




# Just recharge, SW and GW irrigation change for scb70, 80, 90
