#!/usr/bin/env python3
## Made by Kaiya L Provost
## Last updated 13 January 2021
## TODO: 
# sudo python3 /Users/kprovost/Documents/GitHub/bioacoustics/formatting/combine_xmls.py /Users/kprovost/Documents/Postdoc_Working/Halkin/Wave/; 
import sys
import glob
import os 
import re
try:
    directory = sys.argv[1]
    print("Directory given: ",directory)
except:
    print("Directory not given, quitting.")
    exit()
    
os.chdir(directory)
lines = []
listfiles = glob.glob("*.xml",recursive=False)
#listfiles = glob.glob("/**/*.xml",recursive=True)
numfiles = len(listfiles)
for xmlfile in listfiles:
    print(xmlfile)
    with open(xmlfile,"r") as infile:
        full = infile.read()
    full = full.replace("\n","")
    with open(xmlfile,"w") as outfile:
        outfile.write(full)
for xmlfile in listfiles:
    print(xmlfile)
    with open(xmlfile,"r") as infile:
        lines += infile.readlines()
## each line of lines should be one file
## need to extract the "<Sequences><NumSequence>*</NumSequence>" and remove it, and then remove "</Sequences>"
total_sequences = 0
for i in range(len(lines)):
    try:
        total_sequences += int(lines[i].replace(">","<").split("<")[4]) ## gets the number of the sequences in here. should be one but might not be
    except:
        print("ERROR")
        print(i)
        print(lines[i])
        print("###")
        break
    ## remove the <Sequences><NumSequence>*</NumSequence>
    lines[i]=re.sub(r"<Sequences><NumSequence>\d+</NumSequence>","",lines[i])
    lines[i]=re.sub(r"</Sequences>","",lines[i])
## now need to output all of these wtih a new header and new tail
print("Appending to Annotation.xml")
with open("Annotation.xml","a") as outfile:
    _ = outfile.write("<Sequences><NumSequence>"+str(total_sequences)+"</NumSequence>")
    _ = outfile.writelines(lines)
    _ = outfile.write("</Sequences>")
    
'''
<Sequences><NumSequence>1</NumSequence>
<Sequence><WaveFileName>0.wav</WaveFileName><Position>32000</Position><Length>43168</Length>
<NumNote>8</NumNote>
    
    <Note><Position>2240</Position><Length>2688</Length><Label>1</Label></Note>
    
    <Note><Position>8256</Position><Length>2784</Length><Label>1</Label></Note>
    
    <Note><Position>14944</Position><Length>2816</Length><Label>1</Label></Note>
    
    <Note><Position>21088</Position><Length>2336</Length><Label>1</Label></Note>
    
    <Note><Position>26048</Position><Length>2048</Length><Label>1</Label></Note>
    
    <Note><Position>30176</Position><Length>2400</Length><Label>1</Label></Note>
    
    <Note><Position>34688</Position><Length>2688</Length><Label>1</Label></Note>
    
    <Note><Position>39456</Position><Length>2592</Length><Label>1</Label></Note></Sequence>
</Sequences>
<Sequences><NumSequence>1</NumSequence>
<Sequence><WaveFileName>0.wav</WaveFileName><Position>32000</Position><Length>43168</Length>
<NumNote>8</NumNote>
    
    <Note><Position>2240</Position><Length>2688</Length><Label>1</Label></Note>
    
    <Note><Position>8256</Position><Length>2784</Length><Label>1</Label></Note>
    
    <Note><Position>14944</Position><Length>2816</Length><Label>1</Label></Note>
    
    <Note><Position>21088</Position><Length>2336</Length><Label>1</Label></Note>
    
    <Note><Position>26048</Position><Length>2048</Length><Label>1</Label></Note>
    
    <Note><Position>30176</Position><Length>2400</Length><Label>1</Label></Note>
    
    <Note><Position>34688</Position><Length>2688</Length><Label>1</Label></Note>
    
    <Note><Position>39456</Position><Length>2592</Length><Label>1</Label></Note></Sequence>
</Sequences>
'''