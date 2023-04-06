## Kaiya L Provost, 1 Dec 2022 
## take toml file and add a folder to it 

import os
import sys

# cd "/users/PYS1065/kprovost/bioacoustics/WAVS/"
# for i in */; do 
# echo $i;
# python /users/PYS1065/kprovost/bioacoustics/make_slurm_file.py /users/PYS1065/kprovost/bioacoustics/slurm_tweetynet_predict_TEMPLATE.job ${i%/};
# sbatch /users/PYS1065/kprovost/bioacoustics/slurm_tweetynet_predict_${i%/}.job ;
# done

try:
    input_job = str(sys.argv[1])
    print("\t input_job is: ",input_job)
except:
    print("input_job not given, quitting")
    exit()
    # input_job = "/users/PYS1065/kprovost/bioacoustics/slurm_tweetynet_predict_TEMPLATE.job"

try:
    folder_to_add = str(sys.argv[2])
    print("\t folder_to_add is: ",folder_to_add)
except:
    print("folder_to_add not given, quitting")
    exit()
    #folder_to_add = "/users/PYS1065/kprovost/bioacoustics/WAVS/Oreothraupis.arremonops.unknown"

try:
    folder_replacement = str(sys.argv[3])
    print("\t folder_replacement is: ",folder_replacement)
except:
    print("folder_replacement not given, defaulting to XXXFOLDERXXX")
    folder_replacement = "XXXFOLDERXXX"

with open(input_job,"r") as infile:
    lines = infile.read()

lines2=lines.replace(folder_replacement,folder_to_add)
folder_base = os.path.basename(folder_to_add)
if folder_base == "":
    folder_base = folder_to_add.split("/")[-2]

output_job = input_job.replace("TEMPLATE",folder_base)

with open(output_job,"w") as outfile:
    outfile.write(lines2)