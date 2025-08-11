# General Comments --------------------------------------------------------

# Updates cell locations for streamflow depletion/accretion analysis

# Script Initialization ---------------------------------------------------
rm(list=ls())  #Clear workspace

# User Input --------------------------------------------------------------
Counter = as.numeric(read.table(file = 'Counter.dat', header = F))
Cell_Info = read.table('depletion_accretion_cells.txt', header = F, skip = Counter, nrows = 1)
Dep_Acc_Text = readLines('depletion_accretion_rates_template.txt')
Dep_Acc_Value = -1   #Flag for using pumping (depletion, -1) or recharge (accretion, 1)

# Substitute Values -------------------------------------------------------
Dep_Acc_Text = gsub(x = Dep_Acc_Text, pattern = 'DEP_ACC_FLAG', replacement = as.character(Dep_Acc_Value))
Dep_Acc_Text = gsub(x = Dep_Acc_Text, pattern = 'WELL_LAY', replacement = Cell_Info[1])
Dep_Acc_Text = gsub(x = Dep_Acc_Text, pattern = 'WELL_ROW', replacement = Cell_Info[2])
Dep_Acc_Text = gsub(x = Dep_Acc_Text, pattern = 'WELL_COL', replacement = Cell_Info[3])

write(Dep_Acc_Text, file = 'depletion_accretion_rates.txt')
