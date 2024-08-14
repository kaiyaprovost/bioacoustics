#!/usr/bin/env python3
## Made by Kaiya L Provost
## Last updated 7 January 2021

## TODO: audacity, chipper, raven all seem to have different time units 

## run in terminal: take the "#" off of the beginning first and change the folders 
<<<<<<< Updated upstream
# for i in /Users/kprovost/Documents/Postdoc_Working/JY_project/2023_Zonotrichia_Model/*.Table.1.selections.txt; do 
=======
# for i in /Users/kprovost/Documents/Research/Tyrannidae/S*.Table.1.selections.txt; do 
>>>>>>> Stashed changes
# python3 /Users/kprovost/Documents/GitHub/bioacoustics/formatting/raven2xml.py "${i}" 48; 
# done;
# for i in /Users/kprovost/Documents/Postdoc_Working/Halkin/*.Table.1.selections.txt; do 
# python3 /Users/kprovost/Documents/GitHub/bioacoustics/formatting/raven2xml.py "${i}" 44.1; 
# done;


## below here is python code
import sys
import glob
import os 
import pandas as pd

try:
    infile = str(sys.argv[1])
    print("\tFile is: ",infile)
except:
    print("Filename not given, quitting")
    exit()
    #infile="/Users/kprovost/OneDrive - The Ohio State University/BLB_Data/BLB11543.Table.1.selections.txt"
    
try:
    samples_per_ms = float(sys.argv[2])
    print("\tSamples per ms is:",samples_per_ms)
except:
    print("\tSamples per ms not given or invalid. Defaulting to 48 (or 48000 / second)")
    samples_per_ms=48
    
output_filepath = str(infile)+".xml"
if os.path.exists(output_filepath):
    print("File exists: skipping")
else:

    add_labels = True


    with open(infile,"r") as input:
        lines = input.readlines()

    num_sequences = 1
    output_string = "<Sequences><NumSequence>"+str(num_sequences)+"</NumSequence>"
    ## the filename needs to be extracted from the infile though
    wavfilename= infile.split("/")[-1].replace(".Table.1.selections.txt",".wav")
    output_string += "\n<Sequence><WaveFileName>"+str(wavfilename)+"</WaveFileName>"

    ## each line should have three things separated by tabs: onset, offset, labels
    #Selection    View            Channel    Begin Time (s)    End Time (s)    Low Freq (Hz)    High Freq (Hz)
    #1            Spectrogram 1    1        0.575812500        0.849843750        3508.8            4421.1
    ## all these seconds need to be multiplied by 1000 and then by samples_per_ms and then rounded to integers 

    ## to get the longest_length need to process lines[0] and lines[-1]

    first_onset = lines[1].strip().split("\t")[3]
    last_offset = lines[-1].strip().split("\t")[4]
    longest_length = float(last_offset)-float(first_onset)
    output_string += "<Position>"+str(int(round(float(first_onset)*1000*samples_per_ms)))+"</Position>"
    output_string += "<Length>"+str(int(round(float(longest_length)*1000*samples_per_ms)))+"</Length>"
    number_notes = len(lines)
    output_string += "<NumNote>"+str(number_notes)+"</NumNote>"

    for i in range(len(lines)):
        if i > 0:
            split = lines[i].strip().split("\t")
            ## need to subtract the first_onset and then convert to samples not seconds
            onset = int(round((float(split[3]) - float(first_onset))*1000*samples_per_ms))
            offset = int(round((float(split[4]) - float(first_onset))*1000*samples_per_ms))
            #print("len split"+str(len(split)))
            if len(split) > 7 and add_labels == True:
                label = split[-1] ## this means an annotation was manually set
            else:
                ##label = split[0]
                label = 1
            ## onset is the same as position
            length = int(offset)-int(onset)
            #output_string += "\n\t<Note><Position>"+str(onset)+"</Position><Length>"+str(length)+"</Length></Note>"
            output_string += "\n\t<Note><Position>"+str(onset)+"</Position><Length>"+str(length)+"</Length><Label>"+str(label)+"</Label></Note>"
        
    


    output_string += "\n</Sequence>"
    output_string += "\n</Sequences>"

    with open(output_filepath,"w") as output:
        _ = output.write(output_string)
        
    ##
    '''
    <Sequences><NumSequence>571</NumSequence>
    <Sequence><WaveFileName>0.wav</WaveFileName><Position>32000</Position><Length>43168</Length>
    <NumNote>8</NumNote>
            <Note><Position>2240</Position><Length>2688</Length></Note>
            <Note><Position>8256</Position><Length>2784</Length></Note>
            <Note><Position>14944</Position><Length>2816</Length></Note>
            <Note><Position>21088</Position><Length>2336</Length></Note>
            <Note><Position>26048</Position><Length>2048</Length></Note>
            <Note><Position>30176</Position><Length>2400</Length></Note>
            <Note><Position>34688</Position><Length>2688</Length></Note>
            <Note><Position>39456</Position><Length>2592</Length></Note></Sequence>
    </Sequences>
    '''