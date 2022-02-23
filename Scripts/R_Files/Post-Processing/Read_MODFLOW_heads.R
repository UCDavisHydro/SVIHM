# Read MODFLOW Binary Head File
rm(list=ls())
library(ggplot2)
start_time <- Sys.time()

# User Inputs -------------------------------------------------------------
NLAY = 2                 # Number of layers in model
NSP = 252                # Number of stress peridos for which heads are printed
filename = 'SVIHM.hds'   #Name of binary head file
No_FLow_Val = 9999       #Value of no flow cells
output_dir = 'Results/'  #Output directory

# Read Heads --------------------------------------------------------------
fid = file(filename, "rb")
bytes = 0                                     #Bytes counter
p=1

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
}

closeAllConnections()
end_time <- Sys.time()
print(paste0('Total Run Time was ', round(end_time - start_time, digits = 1), ' seconds'))
