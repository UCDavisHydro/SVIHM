#RandPar_UCODE_in.R - R script that generates UCODE input files (*.in) that reference the random parameter files
rm(list=ls())

options(warn=-1)   # suppress warnings (set to 0 to turn warnings on)
UniRandPar = FALSE  #generate UCODE input files using uniform random parameter sets
CovRandPar = TRUE   #generate UCODE input files using uniform random parameter sets (generated using PEST's RANDPAR utility)
UniRandPar_dir = (getwd())
CovRandPar_dir = ('../../PEST/PEST_RANDPAR/')

template_in_file = readLines('SVIHM_RandPar_Template.in')
param_template = read.table('UCODE_param_template.txt', skip = 2, nrows = 61, header = T)

if (UniRandPar == TRUE) {
  for (i in 1:500){
    output = template_in_file
    output = gsub(x = output, pattern = '@Random Parameter File@', replacement = paste0('SVIHM.unirandpar',i))
    write(file = paste0('SVIHM_UniRandPar',i,'.in'),x = output)
  }
}

if (CovRandPar == TRUE) {
  for (j in 1:5){
    for (i in 1:100){
      par_file = paste0('SVIHM_cal_',j,'.covrandpar',i)
      randpars = read.table(paste0(CovRandPar_dir,'/cal_',j,'_par',i,'.par'), skip = 1)
      new_params = param_template
      new_params$StartValue = round(randpars$V2,8)
      write(file = par_file, 'BEGIN  Parameter_Data  Table',append = F)
      write(file = par_file, 'NROW=61  NCOL=8  COLUMNLABELS', append = T)
      write.table(file = par_file, new_params, quote = F, col.names = T, row.names = F, append = T, sep = '  ')
      write(file = par_file, 'END  Parameter_Data', append = T)
      output = template_in_file
      output = gsub(x = output, pattern = '@Random Parameter File@', replacement = paste0('SVIHM_cal_',j,'.covrandpar',i))
      write(file = paste0('SVIHM_cal_',j,'_covrandpar',i,'.in'),x = output)
    }
  }
}