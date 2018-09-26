#UniRandPar.R - R script that generates UCODE input files (*.in) that use the uniform random parameter sets
rm(list=ls())

template_in_file = readLines('SVIHM_UniRandPar_Template.in')

for (i in 1:500){
  output = template_in_file
  output = gsub(x = output, pattern = '@Uniform Random Parameter File@', replacement = paste0('SVIHM.unirandpar',i))
  write(file = paste0('SVIHM_UniRandPar',i,'.in'),x = output)
}