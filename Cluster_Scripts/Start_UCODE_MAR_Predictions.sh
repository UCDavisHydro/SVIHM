#!/bin/bash -eu
while true; do
    read -p "This will overwrite MAR prediction files. Continue? (y/n)" yn
    case $yn in
        [Yy]* )
          echo "Deleting Previous Files..." 
          if [ -d "/aqua/dtolley/UCODE_Pred_Uncert/MAR/" ]; then rm -Rf /aqua/dtolley/UCODE_Pred_Uncert/MAR; fi
          mkdir /aqua/dtolley/UCODE_Pred_Uncert/MAR
          echo "Copying Files..."
          for j in $(seq 1 5)                                                                        # loop over 5 calibrations
          do
          	mkdir /aqua/dtolley/UCODE_Pred_Uncert/MAR/Calibration_$j
          	for k in $(seq 1 7)
          		do
          		mkdir /aqua/dtolley/UCODE_Pred_Uncert/MAR/Calibration_$j/Runner$k
          	  cp ./SVIHM/SWBM/input/* /aqua/dtolley/UCODE_Pred_Uncert/MAR/Calibration_$j/Runner$k/                 #copy SWBM input files
          	  cp ./SVIHM/SWBM/bin/SWBM /aqua/dtolley/UCODE_Pred_Uncert/MAR/Calibration_$j/Runner$k/SWBM                   #copy SWBM executable
          	  cp Update_SVIHM_Drain_Inflows.R /aqua/dtolley/UCODE_Pred_Uncert/MAR/Calibration_$j/Runner$k/                #copy R Script for updating drain inflows
          	  cp ./SVIHM/MODFLOW/SVIHM* /aqua/dtolley/UCODE_Pred_Uncert/MAR/Calibration_$j/Runner$k/                      #copy MODFLOW input files
          	  cp "./SVIHM/MODFLOW/Starting_Heads_Cal_"$j"_L1.txt" "/aqua/dtolley/UCODE_Pred_Uncert/MAR/Calibration_"$j"/Runner"$k"/Starting_Heads_L1.txt" #copy initial heads
          	  cp "./SVIHM/MODFLOW/Starting_Heads_Cal_"$j"_L2.txt" "/aqua/dtolley/UCODE_Pred_Uncert/MAR/Calibration_"$j"/Runner"$k"/Starting_Heads_L2.txt"
          	  cp ./SVIHM/MODFLOW/OWHM /aqua/dtolley/UCODE_Pred_Uncert/MAR/Calibration_$j/Runner$k/OWHM                    #copy OWHM executable
          	  cp ./UCODE/Calibration_Exchange_Files/SVIHM_Cal_$j* /aqua/dtolley/UCODE_Pred_Uncert/MAR/Calibration_$j/Runner$k/ # copy exchange files created during model calibration
          	  cp ./UCODE/UCODE_Input_Files/SVIHM.* /aqua/dtolley/UCODE_Pred_Uncert/MAR/Calibration_$j/Runner$k/           #copy UCODE input files
          	  cp "./UCODE/UCODE_Input_Files/SVIHM_corfac.corfac" "/aqua/dtolley/UCODE_Pred_Uncert/MAR/Calibration_"$j"/SVIHM_Cal_$j.corfac" #copy Calibration
              cp ./UCODE/UCODE_Input_Files/SVIHM_postcal.param$j /aqua/dtolley/UCODE_Pred_Uncert/MAR/Calibration_$j/Runner$k/ #copy UCODE parameter file
              cp ./UCODE/UCODE_Instruction_Files/*.jif /aqua/dtolley/UCODE_Pred_Uncert/MAR/Calibration_$j/Runner$k/           #copy UCODE instruction files
              cp ./UCODE/UCODE_Template_Files/*.jtf /aqua/dtolley/UCODE_Pred_Uncert/MAR/Calibration_$j/Runner$k/              #copy UCODE template files
              cp ./runner /aqua/dtolley/UCODE_Pred_Uncert/MAR/Calibration_$j/Runner$k/              #copy runner program
              cp ./SVIHM/Run_SVIHM_prediction_MAR.sh /aqua/dtolley/UCODE_Pred_Uncert/MAR/Calibration_$j/Runner$k/ #copy script for running SVIHM (first basecase then scenario)
            done
            cp /aqua/dtolley/UCODE_Pred_Uncert/MAR/Calibration_$j/Runner$k/* /aqua/dtolley/UCODE_Pred_Uncert/MAR/Calibration_$j/
            cp ./UCODE/SVIHM_prediction_MAR_$j.in /aqua/dtolley/UCODE_Pred_Uncert/MAR/Calibration_$j/ 
         done
          sbatch --job-name='Cal_1_MAR_runners' --output='SVIHM_MAR_1_pred_runners.out' --error='SVIHM_MAR_1_pred_runners.err' --nodelist='aqua-c11' --nodes='1' --ntasks='7' --cpus-per-task='1' SVIHM_prediction_MAR_1.sbatch 1 7
          #sbatch --job-name='9_14' --output='range_test_9_14.out' --error='range_test_9_14.err' --nodes='1' --ntasks='6' --cpus-per-task='1' SVIHM_prediction_MAR_1_test.sbatch 9 14         #intended to use second node for greater parallelization but not currently working
          sbatch --job-name='Cal_1_MAR_UCODE' --output='SVIHM_MAR_1_pred_ucode.out' --error='SVIHM_MAR_1_pred_ucode.err'  --nodelist='aqua-c11' --ntasks='1' --cpus-per-task='1' SVIHM_prediction_MAR_1.sbatch 8
          sbatch --job-name='Cal_2_MAR_runners' --output='SVIHM_MAR_2_pred_runners.out' --error='SVIHM_MAR_2_pred_runners.err' --nodelist='aqua-c12' --nodes='1' --ntasks='7' --cpus-per-task='1' SVIHM_prediction_MAR_2.sbatch 1 7
          sbatch --job-name='Cal_2_MAR_UCODE' --output='SVIHM_MAR_2_pred_ucode.out' --error='SVIHM_MAR_2_pred_ucode.err'  --nodelist='aqua-c12' --ntasks='1' --cpus-per-task='1' SVIHM_prediction_MAR_2.sbatch 8
          sbatch --job-name='Cal_3_MAR_runners' --output='SVIHM_MAR_3_pred_runners.out' --error='SVIHM_MAR_3_pred_runners.err' --nodelist='aqua-c13' --nodes='1' --ntasks='7' --cpus-per-task='1' SVIHM_prediction_MAR_3.sbatch 1 7
          sbatch --job-name='Cal_3_MAR_UCODE' --output='SVIHM_MAR_3_pred_ucode.out' --error='SVIHM_MAR_3_pred_ucode.err'  --nodelist='aqua-c13' --ntasks='1' --cpus-per-task='1' SVIHM_prediction_MAR_3.sbatch 8
          sbatch --job-name='Cal_4_MAR_runners' --output='SVIHM_MAR_4_pred_runners.out' --error='SVIHM_MAR_4_pred_runners.err' --nodelist='aqua-c14' --nodes='1' --ntasks='7' --cpus-per-task='1' SVIHM_prediction_MAR_4.sbatch 1 7
          sbatch --job-name='Cal_4_MAR_UCODE' --output='SVIHM_MAR_4_pred_ucode.out' --error='SVIHM_MAR_4_pred_ucode.err'  --nodelist='aqua-c14' --ntasks='1' --cpus-per-task='1' SVIHM_prediction_MAR_4.sbatch 8
          sbatch --job-name='Cal_5_MAR_runners' --output='SVIHM_MAR_5_pred_runners.out' --error='SVIHM_MAR_5_pred_runners.err' --nodelist='aqua-c15' --nodes='1' --ntasks='7' --cpus-per-task='1' SVIHM_prediction_MAR_5.sbatch 1 7
          sbatch --job-name='Cal_5_MAR_UCODE' --output='SVIHM_MAR_5_pred_ucode.out' --error='SVIHM_MAR_5_pred_ucode.err'  --nodelist='aqua-c15' --ntasks='1' --cpus-per-task='1' SVIHM_prediction_MAR_5.sbatch 8
          false;;
        [Nn]* ) exit;;
        * ) echo "Please answer y/n";;
    esac
done