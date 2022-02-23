rm(list=ls())  #Clear workspace
nstress = 336
StartingMonths = seq(as.Date("1990/10/1"), by = "month", length.out = nstress+1)
num_days = diff(as.numeric(StartingMonths))
print(getwd())
SFR_Name = list.files(pattern = 'sfr')
InputText = readLines(SFR_Name)  #Read in text file
OutputText = InputText # Initialize output

# Read in piped flows
pipe_flow_filename = list.files(pattern = 'pipe_flow')

pipe_flow = read.table(pipe_flow_filename)$V1

# Extract relevant SFR flowlines
if(pipe_flow_filename == "pipe_flow_Etna.txt"){line_finder = "12  1  14  0"}
if(pipe_flow_filename == "pipe_flow_French.txt"){line_finder = "9  1  10  0"}

# etna_line_finder = "12  1  14  0  0.00E+00"
# french_line_finder = "9  1  10  0  0.00E+00" # NSEG ICALC OUTSEG IUPSEG

trib_lines = grep(pattern = line_finder, x = InputText)
if(length(trib_lines) != nstress){print("Condition of 0 flow in all stress periods not found")}

for(i in 1:length(trib_lines)){
  trib_line = trib_lines[i]            # Index for line in text file for that trib for that stress period
  sfr_text_SP = InputText[trib_line]   # Read line
  
  pipe_flow_SP = pipe_flow[i]  # Read pipe flow for that stress period
  
  pipe_flow_text = formatC(x = (pipe_flow_SP), digits = 2, format = "E")  # Format as sci notation

  # It doesn't like replacing "+" so replace in 2 steps
  pft1 = substr(x = pipe_flow_text, start = 1, stop = 4)    
  pft2 = substr(x = pipe_flow_text, start = 7, stop = 8)
  sfr_text_SP_amended = gsub(pattern = "(0.00)",             # Amend the line of text
                             replacement = pft1,
                             x = sfr_text_SP)

  sfr_text_SP_amended = gsub(pattern = "00  ",             # Amend the line of text
                             replacement = paste0(pft2, "  "),
                             x = sfr_text_SP_amended)
  
  # For some reason, won't find-replace the "+"
  
  OutputText[trib_line] = sfr_text_SP_amended                 # replace line of text in Output Text
}


write.table(OutputText, file = 'SVIHM.sfr', row.names = F, quote = F, col.names = F,
            append = F)
