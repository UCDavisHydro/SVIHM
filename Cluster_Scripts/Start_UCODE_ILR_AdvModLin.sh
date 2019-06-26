#!/bin/bash -eu
while true; do
    read -p "This will overwrite ILR advanced model linearity files. Continue? (y/n)" yn
    case $yn in
        [Yy]* )
          echo "Copying Files..."
          for j in $(seq 1 5)                                                                        # loop over 5 calibrations
          do
            cp ./UCODE/SVIHM_AdvModLin_ILR_$j.in /aqua/dtolley/UCODE_Pred_Uncert/ILR/Calibration_$j/ 
         done
          sbatch --job-name='Cal_1_ILR_runners' --output='SVIHM_ILR_1_advmodlin_runners.out' --error='SVIHM_ILR_1_advmodlin_runners.err' --nodelist='aqua-c21' --nodes='1' --ntasks='7' --cpus-per-task='1' SVIHM_AdvModLin_ILR_1.sbatch 1 7
          #sbatch --job-name='9_14' --output='range_test_9_14.out' --error='range_test_9_14.err' --nodes='1' --ntasks='6' --cpus-per-task='1' SVIHM_AdvModLin_ILR_1_test.sbatch 9 14         #intended to use second node for greater parallelization but not currently working
          sbatch --job-name='Cal_1_ILR_UCODE' --output='SVIHM_ILR_1_advmodlin_ucode.out' --error='SVIHM_ILR_1_advmodlin_ucode.err'  --nodelist='aqua-c21' --ntasks='1' --cpus-per-task='1' SVIHM_AdvModLin_ILR_1.sbatch 8
          sbatch --job-name='Cal_2_ILR_runners' --output='SVIHM_ILR_2_advmodlin_runners.out' --error='SVIHM_ILR_2_advmodlin_runners.err' --nodelist='aqua-c22' --nodes='1' --ntasks='7' --cpus-per-task='1' SVIHM_AdvModLin_ILR_2.sbatch 1 7
          sbatch --job-name='Cal_2_ILR_UCODE' --output='SVIHM_ILR_2_advmodlin_ucode.out' --error='SVIHM_ILR_2_advmodlin_ucode.err'  --nodelist='aqua-c22' --ntasks='1' --cpus-per-task='1' SVIHM_AdvModLin_ILR_2.sbatch 8
          sbatch --job-name='Cal_3_ILR_runners' --output='SVIHM_ILR_3_advmodlin_runners.out' --error='SVIHM_ILR_3_advmodlin_runners.err' --nodelist='aqua-c23' --nodes='1' --ntasks='7' --cpus-per-task='1' SVIHM_AdvModLin_ILR_3.sbatch 1 7
          sbatch --job-name='Cal_3_ILR_UCODE' --output='SVIHM_ILR_3_advmodlin_ucode.out' --error='SVIHM_ILR_3_advmodlin_ucode.err'  --nodelist='aqua-c23' --ntasks='1' --cpus-per-task='1' SVIHM_AdvModLin_ILR_3.sbatch 8
          sbatch --job-name='Cal_4_ILR_runners' --output='SVIHM_ILR_4_advmodlin_runners.out' --error='SVIHM_ILR_4_advmodlin_runners.err' --nodelist='aqua-c24' --nodes='1' --ntasks='7' --cpus-per-task='1' SVIHM_AdvModLin_ILR_4.sbatch 1 7
          sbatch --job-name='Cal_4_ILR_UCODE' --output='SVIHM_ILR_4_advmodlin_ucode.out' --error='SVIHM_ILR_4_advmodlin_ucode.err'  --nodelist='aqua-c24' --ntasks='1' --cpus-per-task='1' SVIHM_AdvModLin_ILR_4.sbatch 8
          sbatch --job-name='Cal_5_ILR_runners' --output='SVIHM_ILR_5_advmodlin_runners.out' --error='SVIHM_ILR_5_advmodlin_runners.err' --nodelist='aqua-c25' --nodes='1' --ntasks='7' --cpus-per-task='1' SVIHM_AdvModLin_ILR_5.sbatch 1 7
          sbatch --job-name='Cal_5_ILR_UCODE' --output='SVIHM_ILR_5_advmodlin_ucode.out' --error='SVIHM_ILR_5_advmodlin_ucode.err'  --nodelist='aqua-c25' --ntasks='1' --cpus-per-task='1' SVIHM_AdvModLin_ILR_5.sbatch 8
          false;;
        [Nn]* ) exit;;
        * ) echo "Please answer y/n";;
    esac
done