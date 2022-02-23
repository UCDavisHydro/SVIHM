#!/bin/bash -eu
#SBATCH --job-name=SVIHM_basecase_linearity_5
#SBATCH --ntasks=1
#SBATCH --output=SVIHM_basecase_linearity_5.out
#SBATCH --error=SVIHM_basecase_linearity_5.err
#SBATCH --export=ALL

cp SVIHM_basecase_linearity_5.in /aqua/dtolley/UCODE_Linear_Uncert/Basecase/Calibration_5
cd /aqua/dtolley/UCODE_Linear_Uncert/Basecase/Calibration_5                    #cd into directory

echo 'Starting UCODE in Linearity Mode'
~/ucode_2014_1.004/bin/ucode_2014 SVIHM_basecase_linearity_5.in SVIHM_Cal_5
echo 'Running MODEL_LINEARITY'
~/ucode_2014_1.004/bin/model_linearity SVIHM_Cal_5

cp ./*._* /zeolite/dtolley/pred_uncert/UCODE_Pred_Uncert/Calibration_5/UCODE_Output_Files/   #Copy all UCODE Exchange Files
cp ./*.#* /zeolite/dtolley/pred_uncert/UCODE_Pred_Uncert/Calibration_5/UCODE_Output_Files/   #Copy all UCODE Output Files
