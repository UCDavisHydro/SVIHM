#!/bin/bash -eu
while true; do
    read -p "This will overwrite MAR prediciton files. Continue? (y/n)" yn
    case $yn in
        [Yy]* ) 
          sbatch --job-name='Cal_1_MAR_runners' --output='SVIHM_MAR_1_pred_runners.out' --error='SVIHM_MAR_1_pred_runners.err' --nodelist='aqua-c33' --nodes='1' --ntasks='7' --cpus-per-task='1' SVIHM_MAR_prediction_1.sbatch 1 7
          #sbatch --job-name='9_14' --output='range_test_9_14.out' --error='range_test_9_14.err' --nodes='1' --ntasks='6' --cpus-per-task='1' SVIHM_MAR_prediction_1_test.sbatch 9 14         #intended to use second node for greater parallelization but not currently working
          sbatch --job-name='Cal_1_MAR_UCODE' --output='SVIHM_MAR_1_pred_ucode.out' --error='SVIHM_MAR_1_pred_ucode.err'  --nodelist='aqua-c33' --ntasks='1' --cpus-per-task='1' SVIHM_MAR_prediction_1.sbatch 8
          #sbatch --job-name='Cal_2_MAR_runners' --output='SVIHM_MAR_2_pred_runners.out' --error='SVIHM_MAR_2_pred_runners.err' --nodelist='aqua-c29' --nodes='1' --ntasks='7' --cpus-per-task='1' SVIHM_MAR_prediction_2.sbatch 1 7
          #sbatch --job-name='Cal_2_MAR_UCODE' --output='SVIHM_MAR_2_pred_ucode.out' --error='SVIHM_MAR_2_pred_ucode.err'  --nodelist='aqua-c29' --ntasks='1' --cpus-per-task='1' SVIHM_MAR_prediction_2.sbatch 8
          #sbatch --job-name='Cal_3_MAR_runners' --output='SVIHM_MAR_3_pred_runners.out' --error='SVIHM_MAR_3_pred_runners.err' --nodelist='aqua-c30' --nodes='1' --ntasks='7' --cpus-per-task='1' SVIHM_MAR_prediction_3.sbatch 1 7
          #sbatch --job-name='Cal_3_MAR_UCODE' --output='SVIHM_MAR_3_pred_ucode.out' --error='SVIHM_MAR_3_pred_ucode.err'  --nodelist='aqua-c30' --ntasks='1' --cpus-per-task='1' SVIHM_MAR_prediction_3.sbatch 8
          #sbatch --job-name='Cal_4_MAR_runners' --output='SVIHM_MAR_4_pred_runners.out' --error='SVIHM_MAR_4_pred_runners.err' --nodelist='aqua-c31' --nodes='1' --ntasks='7' --cpus-per-task='1' SVIHM_MAR_prediction_4.sbatch 1 7
          #sbatch --job-name='Cal_4_MAR_UCODE' --output='SVIHM_MAR_4_pred_ucode.out' --error='SVIHM_MAR_4_pred_ucode.err'  --nodelist='aqua-c31' --ntasks='1' --cpus-per-task='1' SVIHM_MAR_prediction_4.sbatch 8
          #sbatch --job-name='Cal_5_MAR_runners' --output='SVIHM_MAR_5_pred_runners.out' --error='SVIHM_MAR_5_pred_runners.err' --nodelist='aqua-c32' --nodes='1' --ntasks='7' --cpus-per-task='1' SVIHM_MAR_prediction_5.sbatch 1 7
          #sbatch --job-name='Cal_5_MAR_UCODE' --output='SVIHM_MAR_5_pred_ucode.out' --error='SVIHM_MAR_5_pred_ucode.err'  --nodelist='aqua-c32' --ntasks='1' --cpus-per-task='1' SVIHM_MAR_prediction_5.sbatch 8
          false;;
        [Nn]* ) exit;;
        * ) echo "Please answer y/n";;
    esac
done