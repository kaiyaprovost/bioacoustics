#!/bin/bash
#SBATCH --time=1:00:00
#SBATCH --nodes=1 
#SBATCH --ntasks-per-node=1 
#SBATCH --gpus-per-node=0
#SBATCH --job-name=vak
#SBATCH --account=PAA0202
#SBATCH --partition=serial
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=provost.27@osu.edu
#SBATCH --mem=16G

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

source activate vak-env

cd "/users/PYS1065/kprovost/bioacoustics/Sounds_and_Annotations/Aves/"

## convert mp3 to wav files 
Rscript "/users/PYS1065/kprovost/bioacoustics/mp3towav.R"

## rename the mp3s
python3 "/users/PYS1065/kprovost/bioacoustics/rename_xenocanto.py"

## run vak 
vak prep "/users/PYS1065/kprovost/bioacoustics/TOMLS/Aves_slurm.toml"
vak predict "/users/PYS1065/kprovost/bioacoustics/TOMLS/Aves_slurm.toml"

## convert results 
for i in /users/PYS1065/kprovost/bioacoustics/Sounds_and_Annotations/Aves/*/*/*/*annot.csv; do 
python3 "/users/PYS1065/kprovost/bioacoustics/tweetynet_output_to_raven.py" $i 10000 500 0; gzip $i; 
done;

for i in /users/PYS1065/kprovost/bioacoustics/Sounds_and_Annotations/Aves/*/*/*/*/*annot.csv; do 
python3 "/users/PYS1065/kprovost/bioacoustics/tweetynet_output_to_raven.py" $i 10000 500 0; gzip $i; 
done;

## sbatch "/users/PYS1065/kprovost/bioacoustics/slurm_wav_processing.job"
## squeue -u kprovost