#!/usr/bin/env python3
## Made by Kaiya L Provost
## Last updated 6 January 2021

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
	infile="/Users/kprovost/OneDrive - The Ohio State University/BLB_Data/TESTING/SegSyllsOutput_20210106_T130214/segmentedSyllables_syllables_all.txt"
	
try:
	syllfile = str(sys.argv[2])
	print("\tFile is: ",syllfile)
except:
	print("Syllable file not given, quitting")
	exit()
	syllfile="/Users/kprovost/OneDrive - The Ohio State University/BLB_Data/TESTING/SegSyllsOutput_20210106_T130214/AnalysisOutput_20210106_T142154_songsylls.txt"
	

output_filepath = str(infile)+str(syllfile.split("/")[-1])+".xml"

with open(syllfile,"r") as sylls:
	syllines = sylls.readlines()

sylldict = {}
for j in range(len(syllines)):
	syldata = syllines[j].strip().split("\t")
	if j == 0:
		names_location = syldata.index("FileName")
		syllables_location = syldata.index("syllable_pattern")
	else:
		gzipfilename = syldata[names_location]
		wavfilename = gzipfilename.split("/")[-1].split("_")[-1].replace(".gzip",".wav")
		syllable_pattern = syldata[syllables_location]
		sylldict[wavfilename]=syllable_pattern

with open(infile,"r") as input:
	lines = input.readlines()

num_sequences = len(lines)-1
output_string = "<Sequences><NumSequence>"+str(num_sequences)+"</NumSequence>"
## each line should have three things separated by tabs: filename, onsets, offsets
for i in range(len(lines)):
	if i > 0:
		wavfilename,onset,offset = lines[i].strip().split("\t")
		output_string += "<Sequence><WaveFileName>"+str(wavfilename)+"</WaveFileName>"
		onset = onset.replace("[","").replace("]","").replace(" ","").split(",")
		## onset is the same as position
		offset = offset.replace("[","").replace("]","").replace(" ","").split(",")
		## these are not numeric
		output_string += "<Position>"+str(onset[0])+"</Position>"
		longest_length = int(offset[-1])-int(onset[0])
		output_string += "<Length>"+str(longest_length)+"</Length>"
		number_notes = len(onset)
		output_string += "<NumNote>"+str(number_notes)+"</NumNote>"
		lengths = [int(offset[i]) - int(onset[i]) for i in range(len(onset))]
		syllable_pattern = sylldict.get(wavfilename,list(range(number_notes)))
		if type(syllable_pattern) is str:	
			syllable_pattern = syllable_pattern.replace("[","").replace("]","").replace(" ","").split(",")
		for k in range(number_notes):
			output_string += "<Note><Position>"+str(onset[k])+"</Position><Length>"+str(lengths[k])+"</Length><Label>"+str(syllable_pattern[k])+"</Label></Note>"
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