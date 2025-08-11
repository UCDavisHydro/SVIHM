#!/bin/bash -eu
while true; do
    read -p "This will overwrite basecase model linearity advanced files. Continue? (y/n)" yn
    case $yn in
        [Yy]* ) 
          sbatch --job-name='Cal_1_BC_UCODE' --output='SVIHM_basecase_1_modlinadv.out' --error='SVIHM_basecase_1_modlinadv.err'  --ntasks='1' --cpus-per-task='1' SVIHM_basecase_linearity_adv_1.sbatch 
          sbatch --job-name='Cal_2_BC_UCODE' --output='SVIHM_basecase_2_modlinadv.out' --error='SVIHM_basecase_2_modlinadv.err'  --ntasks='1' --cpus-per-task='1' SVIHM_basecase_linearity_adv_2.sbatch 
          sbatch --job-name='Cal_3_BC_UCODE' --output='SVIHM_basecase_3_modlinadv.out' --error='SVIHM_basecase_3_modlinadv.err'  --ntasks='1' --cpus-per-task='1' SVIHM_basecase_linearity_adv_3.sbatch 
          sbatch --job-name='Cal_4_BC_UCODE' --output='SVIHM_basecase_4_modlinadv.out' --error='SVIHM_basecase_4_modlinadv.err'  --ntasks='1' --cpus-per-task='1' SVIHM_basecase_linearity_adv_4.sbatch 
          sbatch --job-name='Cal_5_BC_UCODE' --output='SVIHM_basecase_5_modlinadv.out' --error='SVIHM_basecase_5_modlinadv.err'  --ntasks='1' --cpus-per-task='1' SVIHM_basecase_linearity_adv_5.sbatch 
          false;;
        [Nn]* ) exit;;
        * ) echo "Please answer y/n";;
    esac
done