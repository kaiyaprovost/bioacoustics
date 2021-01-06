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

def chipper2xml():
	[]




with open(infile,"r") as input:
	lines = input.readlines()
	## each line should have three things separated by tabs: filename, onsets, offsets
	for i in range(len(lines)):
		namefile,onset,offset = lines[i].strip().split("\t")
		onset = onset.replace("[","").replace("]","").replace(" ","").split(",")
		offset = offset.replace("[","").replace("]","").replace(" ","").split(",")
		## these are not numeric
		number_notes = len(onset)
		length = [int(offset[i]) - int(onset[i]) for i in range(len(onset))]
		
		
		
##
'''
<Sequences><NumSequence>571</NumSequence>
<Sequence><WaveFileName>0.wav</WaveFileName><Position>32000</Position><Length>43168</Length><NumNote>8</NumNote>
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