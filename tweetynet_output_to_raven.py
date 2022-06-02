#!/usr/bin/env python3
## Made by Kaiya L Provost
## Last updated 13 January 2021

import sys
import glob
import os 
import re
import numpy as np
import pandas as pd

## TODO: this code does not work if onset and offset_Hz are onset and offset_Ind instead

## to run this code: copy the line below and paste into terminal 

# python3 "/Users/kprovost/Documents/GitHub/bioacoustics/tweetynet_output_to_raven.py" "/Users/kprovost/Documents/TweetyNet/testing_wcs/Melozone_fusca_onlytest/Wave_prep_210920_140148.annot.csv"

# for i in /Users/kprovost/Documents/Postdoc_Working/Sounds_and_Annotations/Aves/*annot.csv; do python3 "/Users/kprovost/Documents/GitHub/bioacoustics/tweetynet_output_to_raven.py" $i 10000 500 0; gzip $i; done;

try:
	csvfile = sys.argv[1]
	print("CSV file given: ",csvfile)
except:
	print("CSV file not given, quitting.")
	exit()
	#csvfile = "/Users/kprovost/Documents/TweetyNet/testing_wcs/test/Wave_prep_210406_101324.annot.csv"
try:
	upper_hz = sys.argv[2]
	print("Upper HZ cutoff given: ",upper_hz)
except:
	print("Upper HZ cutoff not given, defaulting to 10000.")
	upper_hz=10000
try:
	lower_hz = sys.argv[3]
	print("Lower HZ cutoff given: ",lower_hz)
except:
	print("Lower HZ cutoff not given, defaulting to 500.")
	lower_hz=500

try:
	keep_suffix=sys.argv[4]
	print("Keeping suffix value:",keep_suffix)
	if(keep_suffix=="1"):
		keep_suffix=True
		print("Keeping suffix")
	else:
		keep_suffix=False
		print("Removing suffix")
except:
	print("Defaulting to not keeping suffix")
	keep_suffix=False

annot_base = os.path.basename(csvfile)
annot_path = os.path.dirname(csvfile)

## header is: label,onset_s,offset_s,onset_Hz,offset_Hz,audio_path,annot_path,sequence,annotation
## header needs to be: Selection	View	Channel	Begin Time (s)	End Time (s)	Low Freq (Hz)	High Freq (Hz)	Begin File	type

## view = "Spectrogram"
## channel = 1
## begin/end time = onset/offset_s
## low/high freq = onset/offset_Hz if not none
## audiopath and annotpath determines file name
## annotation = which file it is 
## not sure what sequence is 
## label = type
df = pd.read_csv(csvfile)

df["View"] = "Spectrogram"
df["Channel"] = "1"

df.loc[df["onset_Hz"] == "None","onset_Hz"] = lower_hz
df.loc[df["offset_Hz"] == "None","offset_Hz"] = upper_hz


df = df.rename(columns={"label": "type", "onset_s": "Begin Time (s)", "offset_s": "End Time (s)",
	"onset_Hz": "Low Freq (Hz)", "offset_Hz": "High Freq (Hz)","audio_path":"Begin File"})

## header needs to be: Selection	View	Channel	Begin Time (s)	End Time (s)	Low Freq (Hz)	High Freq (Hz)	Begin File	type
df = df[["View","Channel","Begin Time (s)","End Time (s)","Low Freq (Hz)","High Freq (Hz)","type","Begin File","annot_path"]]

## need to iterate through each subset by the audiopath
## selection = i

for audio_path in df["Begin File"].unique():
	#print(audio_path)
	subset = df.loc[df["Begin File"] == audio_path]
	subset.insert(2, "Selection", list(range(1,len(subset)+1)), True)
	if(keep_suffix==True):
		outfile=annot_path+"/"+audio_path[:-4]+"."+annot_base+".selections.txt"
	else:
		outfile = annot_path+"/"+audio_path[:-4]+".selections.txt"
	subset = subset[["Selection","View","Channel","Begin Time (s)","End Time (s)","Low Freq (Hz)","High Freq (Hz)","Begin File","type"]]
	subset.to_csv(outfile,sep="\t",index=False)
	
	
	
	
	
	
	
	
	
	
	
	