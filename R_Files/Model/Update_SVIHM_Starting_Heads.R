#Script for translating binary head file output from MODFLOW
rm(list=ls())  #Clear workspace

## Read Head File
NLAY = 2# Number of layers in model (MODIFY AS NEEDED)
NSP =  252 #Number of Stress Preiods (MODIFY AS NEEDED)
filename = list.files(pattern = 'hds')

Head_Print_Dates = seq(as.Date("1990/11/1"), by = "month", length.out = 252)-1   #Dates for the end of each SP
bytes_all = matrix(data = 0,nrow = 252,ncol = 1)
##############################################################################################
##############                   READ BINARY HEAD DATA              ##########################
##############################################################################################
fid = file(filename, "rb")
bytes=0                                          #Initialize bytes counter (helpful for skipping ahead in file if needed)
#seek(fid,739288*(NSP-1),origin = 'start')
seek(fid,739288,origin = 'start')
{
  KSTP = readBin(fid, integer(), size=4); bytes=bytes+4              #Time step number
  KPER = readBin(fid, integer(), size=4); bytes=bytes+4              #Stress period
  PERTIM = readBin(fid, double(), size=4); bytes=bytes+4             #Time in the current stress period
  TOTIM = readBin(fid, double(), size=4); bytes=bytes+4              #Total elapsed time
  DESC = readChar(fid, nchars = 16, useBytes = TRUE); bytes=bytes+16  #Description of the array
  DESC = trimws(DESC, which = c("both", "left", "right"))             #Delete leading spaces
  NCOL = readBin(fid, integer(), size=4); bytes=bytes+4              #Number of columns in the model
  NROW = readBin(fid, integer(), size=4); bytes=bytes+4              #Number of rows in the model
  ILAY = readBin(fid, integer(), size=4); bytes=bytes+4              #Current Layer Number
  H=array(rep(NaN, NROW*NCOL*NLAY), c(NROW, NCOL, NLAY))                     #Initialize Matrix of NaN Values
  for (ir in 1 : NROW) {                                  #Loop though first layer moving across columns
    for (ic in 1 : NCOL) {
      H[ir,ic,ILAY]=readBin(fid, double(), size=4); bytes=bytes+4
    }
  }
          #    fseek(fid,1478444,'cof') #Skip reading Layer 2
  for (i in 2:NLAY){                                       #Loop though remaining layers
   KSTP = readBin(fid, integer(), size=4); bytes=bytes+4              #Time step number
   KPER = readBin(fid, integer(), size=4); bytes=bytes+4              #Stress period
   PERTIM = readBin(fid, double(), size=4); bytes=bytes+4             #Time in the current stress period
   TOTIM = readBin(fid, double(), size=4); bytes=bytes+4              #Total elapsed time
   DESC = readChar(fid, nchars = 16, useBytes = TRUE); bytes=bytes+16  #Description of the array
   DESC = trimws(DESC, which = c("both", "left", "right"))             #Delete leading spaces
   NCOL = readBin(fid, integer(), size=4); bytes=bytes+4              #Number of columns in the model
   NROW = readBin(fid, integer(), size=4); bytes=bytes+4              #Number of rows in the model
   ILAY = readBin(fid, integer(), size=4); bytes=bytes+4              #Current Layer Number
   for (ir in 1 : NROW) {                                  #Loop though first layer moving across columns
     for (ic in 1 : NCOL) {
       H[ir,ic,ILAY]=readBin(fid, double(), size=4); bytes=bytes+4
     }
   }
  }
}
write.table(H[,,1], file = 'Starting_Heads_L1.txt', row.names = F, col.names=F)
write.table(H[,,2], file = 'Starting_Heads_L2.txt', row.names = F, col.names=F)

# H[which(H==9999)] = NA
# breaks <- seq(800, 940, by=2)
# cols <- colorRampPalette(c("red", "green", "blue"))(length(breaks)-1)
# (Layer1_head_plot = levelplot(t(apply(H[,,1],2,rev)), at=breaks, col.regions=cols))
# (Layer2_plot = levelplot(t(apply(H[,,2],2,rev)), at=breaks, col.regions=cols))
