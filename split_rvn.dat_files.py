import os

myfile="/Users/kprovost/Documents/Postdoc_Working/MMRR/WAVS/Wave/0STATSDONE/rvn.dat_trimmed_spectro_fcts_collapsed_COMBINED_7Mar2023.txt.temp"

## header = '"Selection" "Begin.File.1" "View" "Channel" "Begin.Time..s." "End.Time..s." "Low.Freq..Hz." "High.Freq..Hz." "Begin.File" "type" "selec.file" "difference" "gapprev" "gapnext" "paths" "raw_countour" "slopes" "mean_slope" "inflections" "ffreq" "duration" "meanfreq" "sd" "freq.median" "freq.Q25" "freq.Q75" "freq.IQR" "time.median" "time.Q25" "time.Q75" "time.IQR" "skew" "kurt" "sp.ent" "time.ent" "entropy" "sfm" "meandom" "mindom" "maxdom" "dfrange" "modindx" "startdom" "enddom" "dfslope" "meanpeakf"\n'

print("readlines")
with open(myfile,"r") as infile:
    lines = infile.readlines()

print("iterate lines")
header = lines[0]

for i in range(len(lines)):
    if i % 100 == 0:
        print(i)
    if i>0:
        line_i = lines[i]
        split_i = line_i.split()
        id = split_i[2]
        split_id = id.split(".")
        genus = split_id[0]
        genus = genus.replace('''"''',"")
        myoutfile = myfile.replace("rvn.dat",genus+"_rvn.dat")
        if os.path.exists(myoutfile):
            with open(myoutfile,"a") as outfile:
                _ = outfile.write(line_i)
        else:
            with open(myoutfile,"a") as outfile:
                _ = outfile.write(header)
                _ = outfile.write(line_i)
            
        
    


