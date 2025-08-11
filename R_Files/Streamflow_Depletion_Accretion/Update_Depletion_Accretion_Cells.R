# General Comments --------------------------------------------------------

# Updates cell locations for streamflow depletion/accretion analysis

# Script Initialization ---------------------------------------------------
rm(list=ls())  #Clear workspace

# User Input --------------------------------------------------------------
Counter = as.numeric(read.table(file = 'Counter.dat', header = F))
Cell_Info = read.table('depletion_accretion_cells.txt', header = F, skip = Counter, nrows = 1)
Dep_Acc_Text = readLines('depletion_accretion_rates_template.txt')
PMP_RCH_RATE = 1000    #Negative rate for pumping (depletion), positive for recharge (accretion)

# Substitute Values -------------------------------------------------------
if (PMP_RCH_RATE<0) {       #If depeltion is active, put well in layer 2
Dep_Acc_Text = gsub(x = Dep_Acc_Text, pattern = 'WELL_LAY', replacement = Cell_Info[1])
} else {                    #If accretion is active, put well in layer 1
  Dep_Acc_Text = gsub(x = Dep_Acc_Text, pattern = 'WELL_LAY', replacement = '1')
}
if (Counter==0){  #Basecase Run
  Dep_Acc_Text = gsub(x = Dep_Acc_Text, pattern = 'WELL_ROW', replacement = '1')
  Dep_Acc_Text = gsub(x = Dep_Acc_Text, pattern = 'WELL_COL', replacement = '1')
  Dep_Acc_Text = gsub(x = Dep_Acc_Text, pattern = 'PMP_RCH_RATE', replacement = '0')
} else {         #Depletion/Accretion Run
  Dep_Acc_Text = gsub(x = Dep_Acc_Text, pattern = 'WELL_ROW', replacement = Cell_Info[2])
  Dep_Acc_Text = gsub(x = Dep_Acc_Text, pattern = 'WELL_COL', replacement = Cell_Info[3])
  Dep_Acc_Text = gsub(x = Dep_Acc_Text, pattern = 'PMP_RCH_RATE', replacement = as.character(PMP_RCH_RATE))
}
write(Dep_Acc_Text, file = 'depletion_accretion_rates.txt')
