## Kaiya L Provost, 17 Nov 2022 
## take toml file and add a folder to it 

import os
import sys

## python /users/PYS1065/kprovost/bioacoustics/make_toml_file.py /users/PYS1065/kprovost/bioacoustics/Passerellidae_26Oct2022_predict_9SppB_OSC_TEMPLATE.toml $i;

try:
    input_toml = str(sys.argv[1])
    print("\t input_toml is: ",input_toml)
except:
    print("input_toml not given, quitting")
    exit()
    # input_toml = "/Users/kprovost/Documents/GitHub/bioacoustics/tweetynet/AVES/Passerellidae_26Oct2022_predict_9SppB_OSC_TEMPLATE.toml"

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

with open(input_toml,"r") as infile:
    lines = infile.read()

lines2=lines.replace(folder_replacement,folder_to_add)
folder_base = os.path.basename(folder_to_add)
if folder_base == "":
    folder_base = folder_to_add.split("/")[-2]

output_toml = input_toml.replace("TEMPLATE",folder_base)

with open(output_toml,"w") as outfile:
    outfile.write(lines2)