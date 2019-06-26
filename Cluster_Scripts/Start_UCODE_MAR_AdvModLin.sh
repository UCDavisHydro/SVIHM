#!/bin/bash -eu
while true; do
    read -p "This will overwrite MAR advanced model linearity files. Continue? (y/n)" yn
    case $yn in
        [Yy]* )
          echo "Copying Files..."
          for j in $(seq 1 5)                                                                        # loop over 5 calibrations
          do
            cp ./UCODE/SVIHM_AdvModLin_MAR_$j.in /aqua/dtolley/UCODE_Pred_Uncert/MAR/Calibration_$j/ 
         done
          sbatch --job-name='Cal_1_MAR_runners' --output='SVIHM_MAR_1_advmodlin_runners.out' --error='SVIHM_MAR_1_advmodlin_runners.err' --nodelist='aqua-c21' --nodes='1' --ntasks='7' --cpus-per-task='1' SVIHM_AdvModLin_MAR_1.sbatch 1 7
          #sbatch --job-name='9_14' --output='range_test_9_14.out' --error='range_test_9_14.err' --nodes='1' --ntasks='6' --cpus-per-task='1' SVIHM_AdvModLin_MAR_1_test.sbatch 9 14         #intended to use second node for greater parallelization but not currently working
          sbatch --job-name='Cal_1_MAR_UCODE' --output='SVIHM_MAR_1_advmodlin_ucode.out' --error='SVIHM_MAR_1_advmodlin_ucode.err'  --nodelist='aqua-c21' --ntasks='1' --cpus-per-task='1' SVIHM_AdvModLin_MAR_1.sbatch 8
          sbatch --job-name='Cal_2_MAR_runners' --output='SVIHM_MAR_2_advmodlin_runners.out' --error='SVIHM_MAR_2_advmodlin_runners.err' --nodelist='aqua-c22' --nodes='1' --ntasks='7' --cpus-per-task='1' SVIHM_AdvModLin_MAR_2.sbatch 1 7
          sbatch --job-name='Cal_2_MAR_UCODE' --output='SVIHM_MAR_2_advmodlin_ucode.out' --error='SVIHM_MAR_2_advmodlin_ucode.err'  --nodelist='aqua-c22' --ntasks='1' --cpus-per-task='1' SVIHM_AdvModLin_MAR_2.sbatch 8
          sbatch --job-name='Cal_3_MAR_runners' --output='SVIHM_MAR_3_advmodlin_runners.out' --error='SVIHM_MAR_3_advmodlin_runners.err' --nodelist='aqua-c23' --nodes='1' --ntasks='7' --cpus-per-task='1' SVIHM_AdvModLin_MAR_3.sbatch 1 7
          sbatch --job-name='Cal_3_MAR_UCODE' --output='SVIHM_MAR_3_advmodlin_ucode.out' --error='SVIHM_MAR_3_advmodlin_ucode.err'  --nodelist='aqua-c23' --ntasks='1' --cpus-per-task='1' SVIHM_AdvModLin_MAR_3.sbatch 8
          sbatch --job-name='Cal_4_MAR_runners' --output='SVIHM_MAR_4_advmodlin_runners.out' --error='SVIHM_MAR_4_advmodlin_runners.err' --nodelist='aqua-c24' --nodes='1' --ntasks='7' --cpus-per-task='1' SVIHM_AdvModLin_MAR_4.sbatch 1 7
          sbatch --job-name='Cal_4_MAR_UCODE' --output='SVIHM_MAR_4_advmodlin_ucode.out' --error='SVIHM_MAR_4_advmodlin_ucode.err'  --nodelist='aqua-c24' --ntasks='1' --cpus-per-task='1' SVIHM_AdvModLin_MAR_4.sbatch 8
          sbatch --job-name='Cal_5_MAR_runners' --output='SVIHM_MAR_5_advmodlin_runners.out' --error='SVIHM_MAR_5_advmodlin_runners.err' --nodelist='aqua-c25' --nodes='1' --ntasks='7' --cpus-per-task='1' SVIHM_AdvModLin_MAR_5.sbatch 1 7
          sbatch --job-name='Cal_5_MAR_UCODE' --output='SVIHM_MAR_5_advmodlin_ucode.out' --error='SVIHM_MAR_5_advmodlin_ucode.err'  --nodelist='aqua-c25' --ntasks='1' --cpus-per-task='1' SVIHM_AdvModLin_MAR_5.sbatch 8
          false;;
        [Nn]* ) exit;;
        * ) echo "Please answer y/n";;
    esac
done