# Plot Scenario Comparisons

# library(ggplot2)
library(plotly)

rm(list = ls())

#Define directories
pdf_dir = "C:/Users/ckouba/Git/SVIHM/SVIHM/SWBM/update2018notes"
proj_dir = "C:/Users/ckouba/Git/SVIHM/SVIHM/SWBM/up2018"



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
  abline(v = seq(from = 4, to = dim(mwb)[1], by=12),
         lty = 3, lwd = 0.5, col = "lightgray")
  abline(h = pretty(vol_lim), v = seq(from = 52, to = dim(mwb)[1], by=60), # Jan of 95, 00, 05, 10
         lty = 1, col = "darkgray")
  abline(h = 0, col = "black")
  #Manual y-label and axis labels, and a box (plot border)
  mtext(side = 2, expression(paste("Water volume (cubic meters x 10"^"7", ")")), 
        las = 0, line = 2) #manual y-label
  axis(1, at = seq(from = 4, to = dim(mwb)[1], by=12), labels = 1991:2018)
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
  abline(v = seq(from = 4, to = dim(mwb)[1], by=12),
         lty = 3, lwd = 0.5, col = "lightgray")
  abline(h = pretty(vol_lim), v = seq(from = 52, to = dim(mwb)[1], by=60), # Jan of 95, 00, 05, 10
         lty = 1, col = "darkgray")
  abline(h = 0, col = "black")
  #Manual y-label and axis labels
  mtext(side = 2, expression(paste("Change in storage (cubic meters x 10"^"7", ")")), 
        las = 0, line = 2) #manual y-label
  axis(1, at = seq(from = 4, to = dim(mwb)[1], by=12), #Jan of years ending in 5 and 0
       labels = 1991:2018)
  axis(2, at = pretty(vol_lim), pretty(vol_lim)/10^7)
  box()
  
  #Plot change in storage over time
  lines(S, type = "l", col = components$color[6], lwd = 2)
  
  #Add legend to the bottom panel
  legend(x = "topleft", lwd = 2, ncol = 2, bg= "white",
         col = components[,2], legend = components[,1])
  
  dev.off()
}



# Plot budgets ------------------------------------------------------------

monthly_water_budget = read.table(file.path(proj_dir,"monthly_water_budget.dat"), header = TRUE)
mwb18 = monthly_water_budget

plot_water_budget_overview(mwb18, "Scott Valley SWBM WY 1991-2018")
dev.off()
