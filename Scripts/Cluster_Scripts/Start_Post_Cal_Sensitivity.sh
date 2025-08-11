#!/bin/bash -eu

while true; do
    read -p "This will overwrite all files. Continue? (y/n)" yn
    case $yn in
        [Yy]* ) sbatch Post_Cal_Sensitivity_1.sbatch
                sbatch Post_Cal_Sensitivity_2.sbatch
                sbatch Post_Cal_Sensitivity_3.sbatch
                sbatch Post_Cal_Sensitivity_4.sbatch
                sbatch Post_Cal_Sensitivity_5.sbatch
                false;;
        [Nn]* ) exit;;
        * ) echo "Please answer y/n";;
    esac
done