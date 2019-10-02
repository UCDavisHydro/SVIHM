#!/bin/bash -eu

#SBATCH --job-name=Stream_Depletion
#SBATCH -o ./SLURM_out/Run_%A_%a.out
#SBATCH -e ./SLURM_err/Run_%A_%a.err
#SBATCH --array=0-712%200   
#SBATCH --ntasks=1

#if [[ $SLURM_ARRAY_TASK_ID == 0 ]]; then   # Basecase Run
# export bc_dir='/zeolite/dtolley/Streamflow_Depletion_Accretion/Basecase_out/Cal_4'
# rm -rf $bc_dir
# mkdir $bc_dir
# cp SVIHM/SWBM_depletion_accretion/input/* $bc_dir/
# cp SVIHM/SWBM_depletion_accretion/bin/* $bc_dir/
# cp SVIHM/MODFLOW/* $bc_dir/
# cp 'SVIHM/Starting_Heads_Cal_4_L1.txt' $bc_dir/Starting_Heads_L1.txt
# cp 'SVIHM/Starting_Heads_Cal_4_L2.txt' $bc_dir/Starting_Heads_L2.txt
# cp Update_SVIHM_Drain_Inflows.R $bc_dir/Update_SVIHM_Drain_Inflows.R
# cp depletion_accretion_rates_basecase.txt $bc_dir/depletion_accretion_rates.txt
# 
# cd $bc_dir
# echo 0 > SVIHM.status                           #Create status file 
# 
# #Run SVIHM
# cp Drains_initial_m3day.txt Drains_m3day.txt    # Initialize drain flows to zero
# ./SWBM_depletion_accretion                      # 1st run of SWBM
# ./OWHM SVIHM.nam                                # 1st run of MODFLOW
# Rscript Update_SVIHM_Drain_Inflows.R SVIHM.lst  # Update drain inflow from discharge zone  
# ./SWBM_depletion_accretion                      # 2nd run of SWBM
# ./OWHM SVIHM.nam                                # 2nd run of MODFLOW
# 
# echo 1 > SVIHM.status                           #Update status file
# 
# #cp SVIHM.cbb '/zeolite/dtolley/Streamflow_Depletion_Accretion/Model_output/Depletion/CBB_Files/SVIHM_'$SLURM_ARRAY_TASK_ID'.cbb'
# cp SVIHM.lst /zeolite/dtolley/Streamflow_Depletion_Accretion/Model_output/Basecase/SVIHM.lst
# cp Streamflow_Global.dat /zeolite/dtolley/Streamflow_Depletion_Accretion/Model_output/Basecase/Streamflow_Global.dat
# cp Streamflow_FJ_SVIHM.dat /zeolite/dtolley/Streamflow_Depletion_Accretion/Model_output/Basecase/Streamflow_FJ_SVIHM.dat
# cp Streamflow_AS_SVIHM.dat /zeolite/dtolley/Streamflow_Depletion_Accretion/Model_output/Basecase/Streamflow_AS_SVIHM.dat
# cp Streamflow_BY_SVIHM.dat /zeolite/dtolley/Streamflow_Depletion_Accretion/Model_output/Basecase/Streamflow_BY_SVIHM.dat
# cp Streamflow_LS_SVIHM.dat /zeolite/dtolley/Streamflow_Depletion_Accretion/Model_output/Basecase/Streamflow_LS_SVIHM.dat
# cp Streamflow_Pred_Loc_2.dat /zeolite/dtolley/Streamflow_Depletion_Accretion/Model_output/Basecase/Streamflow_Pred_Loc_2.dat
# cp Streamflow_Pred_Loc_3.dat /zeolite/dtolley/Streamflow_Depletion_Accretion/Model_output/Basecase/Streamflow_Pred_Loc_3.dat
# cp Streamflow_Pred_Loc_4.dat /zeolite/dtolley/Streamflow_Depletion_Accretion/Model_output/Basecase/Streamflow_Pred_Loc_4.dat
# cp Streamflow_Pred_Loc_5.dat /zeolite/dtolley/Streamflow_Depletion_Accretion/Model_output/Basecase/Streamflow_Pred_Loc_5.dat
# 
#  
#  else  
#  export run_dir='/aqua/dtolley/Streamflow_Depletion_Accretion/Depletion/Run_'$SLURM_ARRAY_TASK_ID
#  rm -rf $run_dir
#  mkdir $run_dir
#  cp SVIHM/SWBM_depletion_accretion/input/* $run_dir/
#  cp SVIHM/SWBM_depletion_accretion/bin/* $run_dir/
#  cp SVIHM/MODFLOW/* $run_dir/
#  cp SVIHM/Starting_Heads_Cal_4_L1.txt $run_dir/Starting_Heads_L1.txt
#  cp SVIHM/Starting_Heads_Cal_4_L2.txt $run_dir/Starting_Heads_L2.txt
#  cp Update_SVIHM_Drain_Inflows.R $run_dir/Update_SVIHM_Drain_Inflows.R
#  cp Update_Depletion_Accretion_Cells.R $run_dir/Update_Depletion_Accretion_Cells.R
#  
#  cd $run_dir
#  echo $SLURM_ARRAY_TASK_ID > Counter.dat
#  Rscript Update_Depletion_Accretion_Cells.R      # Assign pumping/recharge to cell
#  
#  #Run SVIHM
#  cp Drains_initial_m3day.txt Drains_m3day.txt    # Initialize drain flows to zero
#  ./SWBM_depletion_accretion                      # 1st run of SWBM
#  ./OWHM SVIHM.nam                                # 1st run of MODFLOW
#  Rscript Update_SVIHM_Drain_Inflows.R SVIHM.lst  # Update drain inflow from discharge zone  
#  ./SWBM_depletion_accretion                      # 2nd run of SWBM
#  ./OWHM SVIHM.nam                                # 2nd run of MODFLOW
#  
#  #cp SVIHM.cbb '/zeolite/dtolley/Streamflow_Depletion_Accretion/Model_output/Depletion/CBB_Files/SVIHM_'$SLURM_ARRAY_TASK_ID'.cbb'
#  cp SVIHM.lst '/zeolite/dtolley/Streamflow_Depletion_Accretion/Model_output/Depletion/LST_Files/SVIHM_'$SLURM_ARRAY_TASK_ID'.lst' 
#  cp Streamflow_Global.dat '/zeolite/dtolley/Streamflow_Depletion_Accretion/Model_output/Depletion/Global_Streamflow/Streamflow_Global_'$SLURM_ARRAY_TASK_ID'.dat'
#  cp Streamflow_FJ_SVIHM.dat '/zeolite/dtolley/Streamflow_Depletion_Accretion/Model_output/Depletion/FJ_Streamflow/Streamflow_FJ_SVIHM_'$SLURM_ARRAY_TASK_ID'.dat'
#  cp Streamflow_AS_SVIHM.dat '/zeolite/dtolley/Streamflow_Depletion_Accretion/Model_output/Depletion/AS_Streamflow/Streamflow_AS_SVIHM_'$SLURM_ARRAY_TASK_ID'.dat'
#  cp Streamflow_BY_SVIHM.dat '/zeolite/dtolley/Streamflow_Depletion_Accretion/Model_output/Depletion/BY_Streamflow/Streamflow_BY_SVIHM_'$SLURM_ARRAY_TASK_ID'.dat'
#  cp Streamflow_LS_SVIHM.dat '/zeolite/dtolley/Streamflow_Depletion_Accretion/Model_output/Depletion/LS_Streamflow/Streamflow_LS_SVIHM_'$SLURM_ARRAY_TASK_ID'.dat'
#  cp Streamflow_Pred_Loc_2.dat '/zeolite/dtolley/Streamflow_Depletion_Accretion/Model_output/Depletion/Pred_Loc_2_Streamflow/Streamflow_Pred_Loc_2_'$SLURM_ARRAY_TASK_ID'.dat'
#  cp Streamflow_Pred_Loc_3.dat '/zeolite/dtolley/Streamflow_Depletion_Accretion/Model_output/Depletion/Pred_Loc_3_Streamflow/Streamflow_Pred_Loc_3_'$SLURM_ARRAY_TASK_ID'.dat'
#  cp Streamflow_Pred_Loc_4.dat '/zeolite/dtolley/Streamflow_Depletion_Accretion/Model_output/Depletion/Pred_Loc_4_Streamflow/Streamflow_Pred_Loc_4_'$SLURM_ARRAY_TASK_ID'.dat'
#  cp Streamflow_Pred_Loc_5.dat '/zeolite/dtolley/Streamflow_Depletion_Accretion/Model_output/Depletion/Pred_Loc_5_Streamflow/Streamflow_Pred_Loc_5_'$SLURM_ARRAY_TASK_ID'.dat'
#fi
#
#
#cd  /zeolite/dtolley/Streamflow_Depletion_Accretion/     #move back to head directory

#Ensure basecase run has finished before starting calculations
BC_status=$(cat './Basecase_out/Cal_4/SVIHM.status')
while [ "$BC_status" -eq 0 ]; do
	sleep 1m
	BC_status=$(cat './Basecase_out/Cal_4/SVIHM.status')
done     

if [[ $SLURM_ARRAY_TASK_ID -ne 0 ]]; then
  Rscript Streamflow_Depletion_Accretion.R $SLURM_ARRAY_TASK_ID 1837 -2725.5
fi








