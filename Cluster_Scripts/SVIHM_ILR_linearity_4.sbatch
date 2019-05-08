#!/bin/bash -eu
#SBATCH --job-name=SVIHM_ILR_linearity_4
#SBATCH --ntasks=1
#SBATCH --output=SVIHM_ILR_linearity_4.out
#SBATCH --error=SVIHM_ILR_linearity_4.err
#SBATCH --export=ALL

mkdir /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_4
rsync -av --exclude='UCODE_Output_Files' Calibration_4/* /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_4                                        #Copy model files to aqua folder (not backed up)
\cp Calibration_4/UCODE_Output_Files/* /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_4                     #copy UCODE files to aqua folder (not backed up) 

cp Run_SVIHM_Scenario.sh /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_4   
cp SVIHM_scenario_linearity_4.in /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_4

cd /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_4                    #cd into directory
chmod +x *.sh                                                             #give execution permission to all shell scripts
sed -i 's/basecase/ILR/1I' general_inputs.txt                              #replace basecase flag with ILR flag for SWBM
sed -i 's/basecase/ILR/1I' SVIHM_SWBM_General_Inputs.jtf                   #replace basecase flag with ILR flag for SWBM

echo 'Starting UCODE in Linearity Mode'
~/ucode_2014_1.004/bin/ucode_2014 SVIHM_scenario_linearity_4.in SVIHM_Cal_4
echo 'Running MODEL_LINEARITY'
~/ucode_2014_1.004/bin/model_linearity SVIHM_Cal_4

cp ./*._* /zeolite/dtolley/pred_uncert/UCODE_Pred_Uncert/Calibration_4/UCODE_Output_Files/   #Copy all UCODE Exchange Files
cp ./*.#* /zeolite/dtolley/pred_uncert/UCODE_Pred_Uncert/Calibration_4/UCODE_Output_Files/   #Copy all UCODE Output Files
