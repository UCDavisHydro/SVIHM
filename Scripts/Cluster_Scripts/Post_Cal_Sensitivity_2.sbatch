#!/bin/bash -eu
#SBATCH --job-name=Post_Cal_Sensitivity_2
#SBATCH --ntasks=1
#SBATCH --nodes=2
#SBATCH --cpus-per-task=14
#SBATCH --output=Post_Cal_Sensitivity_2.out
#SBATCH --error=Post_Cal_Sensitivity_2.err
#SBATCH --export=ALL


if [[ -d "/aqua/dtolley/UCODE_Linear_Uncert/Basecase/Calibration_2" ]]; then
	echo 'Deleting previous files'
  rm -rf /aqua/dtolley/UCODE_Linear_Uncert/Basecase/Calibration_2       #delete directory if it exists
fi

cp -r Calibration_2 /aqua/dtolley/UCODE_Linear_Uncert/Basecase       #Copy Calibration 2 Directory to aqua folder (not backed up)
cp Run_SVIHM.sh /aqua/dtolley/UCODE_Linear_Uncert/Basecase/Calibration_2
cp SVIHM_sensitivity_2.in /aqua/dtolley/UCODE_Linear_Uncert/Basecase/Calibration_2    
           
for j in $(seq 1 14)                                        #loop over number of parameters
do
	export b=$j
	mkdir '/aqua/dtolley/UCODE_Linear_Uncert/Basecase/Calibration_2/Runner'$b
	cp Calibration_2/* '/aqua/dtolley/UCODE_Linear_Uncert/Basecase/Calibration_2/Runner'$b      
	cp Run_SVIHM.sh '/aqua/dtolley/UCODE_Linear_Uncert/Basecase/Calibration_2/Runner'$b
done

cd /aqua/dtolley/UCODE_Linear_Uncert/Basecase/Calibration_2                    #cd into directory

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
~/ucode_2014_1.004/bin/ucode_2014 SVIHM_sensitivity_2.in SVIHM_Cal_2
echo 'Running Residual Analysis'
~/ucode_2014_1.004/bin/residual_analysis SVIHM_Cal_2

cp ./*._* /zeolite/dtolley/pred_uncert/UCODE_Pred_Uncert/Calibration_2/UCODE_Output_Files/   #Copy all UCODE Exchange Files
cp ./*.#* /zeolite/dtolley/pred_uncert/UCODE_Pred_Uncert/Calibration_2/UCODE_Output_Files/   #Copy all UCODE Output Files
