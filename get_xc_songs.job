#!/bin/bash
#SBATCH --time=1:00:00
#SBATCH --nodes=1 
#SBATCH --ntasks-per-node=1 
#SBATCH --gpus-per-node=0
#SBATCH --job-name=xc
#SBATCH --account=PAA0202
#SBATCH --partition=serial
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=provost.27@osu.edu

#slurm starts job in working DIR
cd $SLURM_SUBMIT_DIR

#software licenses syntax
#use: SBATCH --licenses={software flag}@osc:N
#use: SBATCH --licenses=abaqus@osc:5

#set up software environment
# module load intel
#Load cuda module for Nvidia libraries
# module load cuda

module load gnu/9.1.0
module load openmpi/1.10.7
module load mkl/2019.0.5
module load R/4.0.2
module load miniconda3
module load java

Rscript "~/bioacoustics/download_xc.R"

## sbatch "/users/PYS1065/kprovost/bioacoustics/get_xc.job"
## squeue -u kprovost

