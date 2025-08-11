#!/bin/bash -eu
#SBATCH --job-name=SVIHM_basecase_linearity_4
#SBATCH --ntasks=1
#SBATCH --output=SVIHM_basecase_linearity_4.out
#SBATCH --error=SVIHM_basecase_linearity_4.err
#SBATCH --export=ALL

cp SVIHM_basecase_linearity_4.in /aqua/dtolley/UCODE_Linear_Uncert/Basecase/Calibration_4

cd /aqua/dtolley/UCODE_Linear_Uncert/Basecase/Calibration_4                    #cd into directory

echo 'Starting UCODE in Linearity Mode'
~/ucode_2014_1.004/bin/ucode_2014 SVIHM_basecase_linearity_4.in SVIHM_Cal_4
echo 'Running MODEL_LINEARITY'
~/ucode_2014_1.004/bin/model_linearity SVIHM_Cal_4

cp ./*._* /zeolite/dtolley/pred_uncert/UCODE_Pred_Uncert/Calibration_4/UCODE_Output_Files/   #Copy all UCODE Exchange Files
cp ./*.#* /zeolite/dtolley/pred_uncert/UCODE_Pred_Uncert/Calibration_4/UCODE_Output_Files/   #Copy all UCODE Output Files
