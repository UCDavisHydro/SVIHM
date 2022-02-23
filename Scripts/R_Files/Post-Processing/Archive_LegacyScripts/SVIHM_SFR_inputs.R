SVIHM_SFR_inputs = function(SFR_filename){

  StartingMonths = seq(as.Date("1990/10/1"), by = "month", length.out = 252)
  numDays = as.numeric(seq(as.Date("1990/11/1"), by = "month", length.out = 252) - StartingMonths)
  Inflow_Text = readLines(SFR_filename)
  
  #Extract Inflows for each stress period and convert to CFS
  SR_EF_SF = as.numeric(sapply(strsplit(Inflow_Text[grep('  1  1  3  0  ',Inflow_Text)],'  '),"[[",6))*numDays
  Sugar = as.numeric(sapply(strsplit(Inflow_Text[grep('  2  1  3  0  ',Inflow_Text)],'  '),"[[",6))*numDays
  French1 = as.numeric(sapply(strsplit(Inflow_Text[grep('  6  1  8  0  ',Inflow_Text)],'  '),"[[",6))*numDays
  French2 = as.numeric(sapply(strsplit(Inflow_Text[grep('  7  1  8  0  ',Inflow_Text)],'  '),"[[",6))*numDays
  Etna = as.numeric(sapply(strsplit(Inflow_Text[grep('  11  1  12  0  ',Inflow_Text)],'  '),"[[",6))*numDays
  Johnson = as.numeric(sapply(strsplit(Inflow_Text[grep('  15  1  17  0  ',Inflow_Text)],'  '),"[[",6))*numDays
  Crystal = as.numeric(sapply(strsplit(Inflow_Text[grep('  16  1  17  0  ',Inflow_Text)],'  '),"[[",6))*numDays
  Patterson = as.numeric(sapply(strsplit(Inflow_Text[grep('  18  1  19  0  ',Inflow_Text)],'  '),"[[",6))*numDays
  Kidder = as.numeric(sapply(strsplit(Inflow_Text[grep('  21  1  22  0  ',Inflow_Text)],'  '),"[[",6))*numDays
  Moffett = as.numeric(sapply(strsplit(Inflow_Text[grep('  24  1  25  0  ',Inflow_Text)],'  '),"[[",6))*numDays
  Mill = as.numeric(sapply(strsplit(Inflow_Text[grep('  27  1  29  0  ',Inflow_Text)],'  '),"[[",6))*numDays
  Shackleford = as.numeric(sapply(strsplit(Inflow_Text[grep('  28  1  29  0  ',Inflow_Text)],'  '),"[[",6))*numDays
  Farmers = as.numeric(sapply(strsplit(Inflow_Text[grep('  31  1  0  3  ',Inflow_Text)],'  '),"[[",7))*numDays
  SVID = as.numeric(sapply(strsplit(Inflow_Text[grep('  32  1  0  10  ',Inflow_Text)],'  '),"[[",7))*numDays
  Drain_Inflow = as.numeric(sapply(strsplit(Inflow_Text[grep('  19  1  22  0  ',Inflow_Text)],'  '),"[[",6))*numDays
  
  Monthly_SFR_Vol = data.frame(Date = StartingMonths,
                      Inflows_m3 = SR_EF_SF + Sugar + French1 + French2 + Etna + Johnson + Crystal + 
                        Patterson + Kidder + Moffett + Mill + Shackleford,
                      Drain_Inflow_m3 = Drain_Inflow,
                      Farmers_Div_m3 = -Farmers,
                      SVID_Div_m3 = -SVID)
  
return(Monthly_SFR_Vol)
 }
