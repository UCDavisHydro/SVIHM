#!/bin/bash -eu
#SBATCH --job-name=Post_Cal_Sensitivity_1
#SBATCH --ntasks=1
#SBATCH --nodes=2
#SBATCH --cpus-per-task=14
#SBATCH --output=Post_Cal_Sensitivity_1.out
#SBATCH --error=Post_Cal_Sensitivity_1.err
#SBATCH --export=ALL


if [[ -d "/aqua/dtolley/UCODE_Linear_Uncert/Basecase/Calibration_1" ]]; then
	echo 'Deleting previous files'
  rm -rf /aqua/dtolley/UCODE_Linear_Uncert/Basecase/Calibration_1       #delete directory if it exists
fi

cp -r Calibration_1 /aqua/dtolley/UCODE_Linear_Uncert/Basecase       #Copy Calibration 1 Directory to aqua folder (not backed up)
cp Run_SVIHM.sh /aqua/dtolley/UCODE_Linear_Uncert/Basecase/Calibration_1   
cp SVIHM_sensitivity_1.in /aqua/dtolley/UCODE_Linear_Uncert/Basecase/Calibration_1 
           
for j in $(seq 1 14)                                        #loop over number of parameters
do
	export b=$j
	mkdir '/aqua/dtolley/UCODE_Linear_Uncert/Basecase/Calibration_1/Runner'$b
	cp Calibration_1/* '/aqua/dtolley/UCODE_Linear_Uncert/Basecase/Calibration_1/Runner'$b      
	cp Run_SVIHM.sh '/aqua/dtolley/UCODE_Linear_Uncert/Basecase/Calibration_1/Runner'$b
done

cd /aqua/dtolley/UCODE_Linear_Uncert/Basecase/Calibration_1                    #cd into directory

#Start runners
for j in $(seq 1 14)                                        #loop over number of parameters
do
	export b=$j
	cd 'Runner'$b
  echo 'Starting Runner'$b
	./runner &                 #start runner program
  cd ..
done

echo 'Starting UCODE'
~/ucode_2014_1.004/bin/ucode_2014 SVIHM_sensitivity_1.in SVIHM_Cal_1
echo 'Running Residual Analysis'
~/ucode_2014_1.004/bin/residual_analysis SVIHM_Cal_1

cp ./*._* /zeolite/dtolley/pred_uncert/UCODE_Pred_Uncert/Calibration_1/UCODE_Output_Files/   #Copy all UCODE Exchange Files
cp ./*.#* /zeolite/dtolley/pred_uncert/UCODE_Pred_Uncert/Calibration_1/UCODE_Output_Files/   #Copy all UCODE Output Files
