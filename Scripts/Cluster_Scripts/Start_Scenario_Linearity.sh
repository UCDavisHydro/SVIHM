#!/bin/bash -eu

while true; do
    read -p "This will overwrite scenario linearity files. Continue? (y/n)" yn
    case $yn in
        [Yy]* ) 
        				if [[ -d "/aqua/dtolley/UCODE_Linear_Uncert/MAR" ]]; then
	        				echo 'Deleting MAR run directory'
  								rm -rf /aqua/dtolley/UCODE_Linear_Uncert/MAR     #delete directory if it exists
								fi
								if [[ -d "/aqua/dtolley/UCODE_Linear_Uncert/ILR" ]]; then
	        				echo 'Deleting ILR run directory'
  								rm -rf /aqua/dtolley/UCODE_Linear_Uncert/ILR     #delete directory if it exists
								fi
								if [[ -d "/aqua/dtolley/UCODE_Linear_Uncert/MAR_ILR" ]]; then
	        				echo 'Deleting MAR_ILR run directory'
  								rm -rf /aqua/dtolley/UCODE_Linear_Uncert/MAR_ILR     #delete directory if it exists
								fi
							
                mkdir /aqua/dtolley/UCODE_Linear_Uncert/MAR
                mkdir /aqua/dtolley/UCODE_Linear_Uncert/ILR
                mkdir /aqua/dtolley/UCODE_Linear_Uncert/MAR_ILR
                
                sbatch SVIHM_MAR_linearity_1.sbatch
                sbatch SVIHM_MAR_linearity_2.sbatch
                sbatch SVIHM_MAR_linearity_3.sbatch
                sbatch SVIHM_MAR_linearity_4.sbatch
                sbatch SVIHM_MAR_linearity_5.sbatch
                sbatch SVIHM_ILR_linearity_1.sbatch
                sbatch SVIHM_ILR_linearity_2.sbatch
                sbatch SVIHM_ILR_linearity_3.sbatch
                sbatch SVIHM_ILR_linearity_4.sbatch
                sbatch SVIHM_ILR_linearity_5.sbatch
                sbatch SVIHM_MAR_ILR_linearity_1.sbatch
                sbatch SVIHM_MAR_ILR_linearity_2.sbatch
                sbatch SVIHM_MAR_ILR_linearity_3.sbatch
                sbatch SVIHM_MAR_ILR_linearity_4.sbatch
                sbatch SVIHM_MAR_ILR_linearity_5.sbatch && false;;
        [Nn]* ) exit;;
        * ) echo "Please answer y/n";;
    esac
done
