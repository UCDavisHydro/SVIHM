#!/bin/bash -eu

#SBATCH --export=ALL

#if [[ $1 == 1 ]]; then                                                                            #Copy files to head directory if first task
#  cp SVIHM_prediction_4.in /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_4      #UCODE input file
#  sed -i 's/Run_SVIHM/Run_SVIHM_ILR/1I' SVIHM_prediction_4.in                             #replace basecase flag with ILR flag for SWBM 
#  cp Calibration_4/UCODE_Output_Files/* /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_4  #Previous UCODE output files
#  cp Run_SVIHM_ILR.sh /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_4                        #Script to run SVIHM
#  cp ../UCODE/UCODE_Input_Files/SVIHM_Cal_4.corfac /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_4  #copy corfac file to directory
#fi          

if [[ $# == 2 ]]; then                                                                         #If 2 command line variables are provided
  sleep 5                                                                                      #Sleep for 5 seconds to ensure UCODE status file is written
  for j in $(seq $1 $2)                                                                        #loop over subset of parameters (groups of # of cpus per node - 8 for aqua)
  do 
#	  cp Run_SVIHM_ILR.sh '/aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_4/Runner'$j           #copy script to run SVIHM into runner directories
    echo 'Starting Runner'$j
    cd '/aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_4/Runner'$j && ./runner &          #start runner program from within directory
    cd /zeolite/dtolley/pred_uncert/UCODE_Pred_Uncert                                          #return to home directory for copying files at the beginning of the loop
    if [[ $j == $2 ]];then
      while [ $(<'/aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_4/UCODE.status') == 1 ]  #Keep job running while UCODE is still running so that runners don't shut down
      do
      sleep 60
      done
    fi
  done
  
fi

if [[ $# == 1 ]]; then                                                                         #If only one command line variable is provided 
    cd /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_4                                   #cd into directory       
    echo 1 > UCODE.status                                                                      #Create status file for if UCODE is still running. Runner loops will check this so they don't quit prematurely.
    sleep 60                                                                                   #Make sure runners are started before UCODE is executed
    echo 'Starting UCODE Prediction Run'
    ~/ucode_2014_1.004/bin/ucode_2014 SVIHM_prediction_4.in SVIHM_Cal_4               #Run UCODE
    echo 'Running LINEAR_UNCERTAINTY'
    ~/ucode_2014_1.004/bin/linear_uncertainty SVIHM_Cal_4                                      #Run LINEAR_UNCERTAINTY
    echo 'Running CORFAC_PLUS'
    ~/ucode_2014_1.004/bin/corfac_plus SVIHM_Cal_4                                             #Run CORFAC_PLUS
    
    cp ./*._* /zeolite/dtolley/pred_uncert/UCODE_Pred_Uncert/Results/ILR                  #Copy all UCODE Exchange Files
    cp ./*.#* /zeolite/dtolley/pred_uncert/UCODE_Pred_Uncert/Results/ILR                  #Copy all UCODE Output Files
    echo 0 > UCODE.status                                                                      #End Runner loops.
fi