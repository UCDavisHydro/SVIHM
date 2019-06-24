#!/bin/bash -eu

#SBATCH --export=ALL

cp SVIHM_basecase_linearity_adv_4.in /aqua/dtolley/UCODE_Linear_Uncert/Basecase/Calibration_4   #UCODE input file
cp Calibration_4/UCODE_Output_Files/* /aqua/dtolley/UCODE_Linear_Uncert/Basecase/Calibration_4  #Previous UCODE output files
cp Run_SVIHM.sh /aqua/dtolley/UCODE_Linear_Uncert/Basecase/Calibration_4                        #Script to run SVIHM
    
cd /aqua/dtolley/UCODE_Linear_Uncert/Basecase/Calibration_4                                   #cd into directory       
echo 'Starting UCODE in Advanced-Test-Model-Linearity Mode'
~/ucode_2014_1.004/bin/ucode_2014 SVIHM_basecase_linearity_adv_4.in SVIHM_Cal_4            #Run UCODE
echo 'Running MODEL_LINEARITY_ADV'
~/ucode_2014_1.004/bin/model_linearity_adv SVIHM_Cal_4                                     #Run LINEAR_UNCERTAINTY

cp ./*._* /zeolite/dtolley/pred_uncert/UCODE_Pred_Uncert/Calibration_4/UCODE_Output_Files/    #Copy all UCODE Exchange Files
cp ./*.#* /zeolite/dtolley/pred_uncert/UCODE_Pred_Uncert/Calibration_4/UCODE_Output_Files/    #Copy all UCODE Output Files
