#!/bin/bash -eu
while true; do
    read -p "This will overwrite basecase linearity files. Continue? (y/n)" yn
    case $yn in
        [Yy]* ) sbatch SVIHM_basecase_linearity_1.sbatch
                sbatch SVIHM_basecase_linearity_2.sbatch
                sbatch SVIHM_basecase_linearity_3.sbatch
                sbatch SVIHM_basecase_linearity_4.sbatch
                sbatch SVIHM_basecase_linearity_5.sbatch
                false;;
        [Nn]* ) exit;;
        * ) echo "Please answer y/n";;
    esac
done
