## 8 January 2026
## Kaiya Provost, and also Dave Kelly with the assist

## note: this only works right now on the raw birdnet files

import glob

## allow this to accept arguments at some point

## make the set to check agaist 
my_set = set()

## get a folder
infile_folder = "/Users/kprovost/Documents/Research/Caro_Provost_Transit_Noise/"
infile_pattern = "BirdNET.selection.table.txt"
my_infiles = glob.glob(infile_folder+"**/*"+infile_pattern,recursive=True)
print(my_infiles)

exit

# infile_path = "/Users/kprovost/Documents/Research/Caro_Provost_Transit_Noise/February_2025_Deployment/Adelphi/UC/KProvost_05/20250225/20250225_104000.BirdNET.selection.table.txt"
outfile_path = "/Users/kprovost/Documents/Research/Caro_Provost_Transit_Noise/big_temp_file.txt"

header_printed = False 
my_infiles_length = len(my_infiles)

for infile_i in range(my_infiles_length):
    infile_path = my_infiles[infile_i]
    print(str(infile_i)+"/"+str(my_infiles_length))
    with open(infile_path,"r") as infile:
        ## the first line is the header and the second on lines are going to be not headers
        ## this assumes that the header is identical every time 
        header = infile.readline() ## header
        ## for these files we need to make a combo of
        ## Begin Time (s), Common Name, Begin Path
        ## and Begin Path needs to be split up later on
        split_header = header.split("\t")
        index_time = split_header.index("Begin Time (s)")
        index_name = split_header.index("Common Name")
        index_path = split_header.index("Begin Path")
        index_conf = split_header.index("Confidence")
        new_header = "Begin Path\tBegin Time (s)\tCommon Name\tConfidence\tLocality\tSublocality\tRecorder\tDate\tDateTime\tTime\n"

        ## write the header to the outfile 
        if header_printed == False:
            with open(outfile_path,"w") as outfile:
               outfile.write(new_header)
            header_printed = True

        # print(header)
        for line in infile:
            split_line = line.split("\t")
            time = split_line[index_time]
            name = split_line[index_name]
            conf = split_line[index_conf]
            path = split_line[index_path]
            ## now you need to split the path up so that the path is NOT machine dependent
            split_path = path.replace(".WAV","").split("/")
            ## time is the last one
            datetime = split_path[-1]
            time = datetime.split("_")[-1].replace(".WAV","")
            
            new_path = "\t".join(split_path[-5:])+"\t"+str(time) 
            # print(new_path)
            my_set_item = (new_path,time,name) ## this is a tuple
            # print(my_set_item)
            ## check if it is in the set
            if my_set_item not in my_set:
                ## add it to the set
                my_set.add(my_set_item)
                ## generate the new line
                # "Begin Path\tBegin Time (s)\tCommon Name\tConfidence\tLocality\tSublocality\tRecorder\tDate\tDateTime\n"
                my_new_line = path+"\t"+time+"\t"+name+"\t"+conf+"\t"+new_path+"\n"
                ## and write it to my dataframe file 
                with open(outfile_path,"a") as outfile:
                    outfile.write(my_new_line)
            else:
                pass
