#!/bin/bash -eu

#SBATCH --export=ALL


cp SVIHM_basecase_linearity_adv_1.in /aqua/dtolley/UCODE_Linear_Uncert/Basecase/Calibration_1   #UCODE input file
cp Calibration_1/UCODE_Output_Files/* /aqua/dtolley/UCODE_Linear_Uncert/Basecase/Calibration_1  #Previous UCODE output files
cp Run_SVIHM.sh /aqua/dtolley/UCODE_Linear_Uncert/Basecase/Calibration_1                        #Script to run SVIHM

cd /aqua/dtolley/UCODE_Linear_Uncert/Basecase/Calibration_1                                   #cd into directory       
echo 'Starting UCODE in Advanced-Test-Model-Linearity Mode'
~/ucode_2014_1.004/bin/ucode_2014 SVIHM_basecase_linearity_adv_1.in SVIHM_Cal_1            #Run UCODE
echo 'Running MODEL_LINEARITY_ADV'
~/ucode_2014_1.004/bin/model_linearity_adv SVIHM_Cal_1                                     #Run LINEAR_UNCERTAINTY

cp ./*._* /zeolite/dtolley/pred_uncert/UCODE_Pred_Uncert/Calibration_1/UCODE_Output_Files/    #Copy all UCODE Exchange Files
cp ./*.#* /zeolite/dtolley/pred_uncert/UCODE_Pred_Uncert/Calibration_1/UCODE_Output_Files/    #Copy all UCODE Output Files