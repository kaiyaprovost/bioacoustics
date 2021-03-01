#!/usr/bin/env python3
## Made by Kaiya L Provost
## Last updated 7 January 2021


## TODO: Chipper and Audacity export in different units of time -- for now change int to float, but need to figure out and correct


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
	infile="/Users/kprovost/OneDrive - The Ohio State University/BLB_Data/blb24711_label_test.txt"
	
output_filepath = str(infile)+".audacity.xml"

with open(infile,"r") as input:
	lines = input.readlines()

num_sequences = 1
output_string = "<Sequences><NumSequence>"+str(num_sequences)+"</NumSequence>"
## the filename needs to be extracted from the infile though
wavfilename= infile.split("/")[-1].split("_")[0].upper()+".wav"
output_string += "<Sequence><WaveFileName>"+str(wavfilename)+"</WaveFileName>"

## each line should have three things separated by tabs: onset, offset, labels
## to get the longest_length need to process lines[0] and lines[-1]

first_onset = lines[0].strip().split("\t")[0]
last_offset = lines[-1].strip().split("\t")[1]
longest_length = float(last_offset)-float(first_onset)
output_string += "<Position>"+str(first_onset)+"</Position>"
output_string += "<Length>"+str(longest_length)+"</Length>"
number_notes = len(lines)
output_string += "<NumNote>"+str(number_notes)+"</NumNote>"

for i in range(len(lines)):
	onset,offset,label = lines[i].strip().split("\t")
	## onset is the same as position
	length = float(offset)-float(onset)
	output_string += "<Note><Position>"+str(onset)+"</Position><Length>"+str(length)+"</Length><Label>"+str(label)+"</Label></Note>"
		
output_string += "</Sequence>"
output_string += "</Sequences>"

with open(output_filepath,"w") as output:
	_ = output.write(output_string)
		
##
'''
<Sequences><NumSequence>571</NumSequence>
<Sequence><WaveFileName>0.wav</WaveFileName><Position>32000</Position><Length>43168</Length>
<NumNote>8</NumNote>
		<Note><Position>2240</Position><Length>2688</Length><Label>0</Label></Note>
		<Note><Position>8256</Position><Length>2784</Length><Label>0</Label></Note>
		<Note><Position>14944</Position><Length>2816</Length><Label>0</Label></Note>
		<Note><Position>21088</Position><Length>2336</Length><Label>1</Label></Note>
		<Note><Position>26048</Position><Length>2048</Length><Label>0</Label></Note>
		<Note><Position>30176</Position><Length>2400</Length><Label>0</Label></Note>
		<Note><Position>34688</Position><Length>2688</Length><Label>1</Label></Note>
		<Note><Position>39456</Position><Length>2592</Length><Label>0</Label></Note></Sequence>
</Sequences>
'''