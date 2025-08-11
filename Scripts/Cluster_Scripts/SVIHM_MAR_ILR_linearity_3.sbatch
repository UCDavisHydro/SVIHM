#!/bin/bash -eu
#SBATCH --job-name=SVIHM_MAR_ILR_linearity_3
#SBATCH --ntasks=1
#SBATCH --output=SVIHM_MAR_ILR_linearity_3.out
#SBATCH --error=SVIHM_MAR_ILR_linearity_3.err
#SBATCH --export=ALL

mkdir /aqua/dtolley/UCODE_Linear_Uncert/MAR_ILR/Calibration_3
rsync -av --exclude='UCODE_Output_Files' Calibration_3/* /aqua/dtolley/UCODE_Linear_Uncert/MAR_ILR/Calibration_3                                        #Copy model files to aqua folder (not backed up)
\cp Calibration_3/UCODE_Output_Files/* /aqua/dtolley/UCODE_Linear_Uncert/MAR_ILR/Calibration_3                     #copy UCODE files to aqua folder (not backed up)        

cp Run_SVIHM_Scenario.sh /aqua/dtolley/UCODE_Linear_Uncert/MAR_ILR/Calibration_3   
cp SVIHM_scenario_linearity_3.in /aqua/dtolley/UCODE_Linear_Uncert/MAR_ILR/Calibration_3

cd /aqua/dtolley/UCODE_Linear_Uncert/MAR_ILR/Calibration_3                    #cd into directory
chmod +x *.sh                                                             #give execution permission to all shell scripts
sed -i 's/basecase/MAR_ILR/1I' general_inputs.txt                              #replace basecase flag with MAR_ILR flag for SWBM
sed -i 's/basecase/MAR_ILR/1I' SVIHM_SWBM_General_Inputs.jtf                   #replace basecase flag with ILR flag for SWBM

echo 'Starting UCODE in Linearity Mode'
~/ucode_2014_1.004/bin/ucode_2014 SVIHM_scenario_linearity_3.in SVIHM_Cal_3
echo 'Running MODEL_LINEARITY'
~/ucode_2014_1.004/bin/model_linearity SVIHM_Cal_3

cp ./*._* /zeolite/dtolley/pred_uncert/UCODE_Pred_Uncert/Calibration_3/UCODE_Output_Files/   #Copy all UCODE Exchange Files
cp ./*.#* /zeolite/dtolley/pred_uncert/UCODE_Pred_Uncert/Calibration_3/UCODE_Output_Files/   #Copy all UCODE Output Files
