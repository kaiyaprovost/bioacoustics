#!/usr/bin/env python3
## Made by Kaiya L Provost
## Last updated 25 May 2022

import os
import re
import glob

pathname = os.getcwd()

files = glob.glob(pathname+"/"+"**/**mp3.wav", recursive=True)

for filename in files:
    print(filename)
    split = filename.replace(".mp3","").split("-")
    if len(split) >= 4:
        newfilename = ".".join(split[:-1])+".XC."+split[-1]
    else:
        newfilename = ".".join(split[:-1])+".unknown.XC."+split[-1]
    try:
        os.rename(filename,newfilename)
    except:
        print("FAILED NAME CHANGE")

files2 = glob.glob(pathname+"/"+"**/**mp3.selections.txt", recursive=True)

for filename2 in files2:
    print(filename2)
    split2 = filename2.replace(".mp3","").split("-")
    if len(split2) >= 5:
        newfilename2 = ".".join(split2[:-1])+".XC."+split2[-1]
    else:
        newfilename2 = ".".join(split2[:-1])+".unknown.XC."+split2[-1]
    try:
        os.rename(filename2,newfilename2)
    except:
        print("FAILED NAME CHANGE")

## process BLB if it does not contain unknown 

files3 = glob.glob(pathname+"/"+"**/**BLB**", recursive=True)

for filename3 in files3:
    #print(filename3)
    if "unknown" in filename3:
        #print("GOOD")
        pass
    else:
        print(filename3)
        #print("BAD")
        split3 = filename3.split("BLB")
        newfilename3 = "unknown.BLB".join(split3)
        try:
            os.rename(filename3,newfilename3)
        except:
            print("FAILED NAME CHANGE")
            

files4 = glob.glob(pathname+"/"+"**/**XC**", recursive=True)

for filename4 in files4:
    if "unknown" in filename4:
        pass
    else:
        print(filename4)
        split4 = filename4.split("XC")
        newfilename4 = "unknown.XC".join(split4)
        try:
            os.rename(filename4,newfilename4)
        except:
            print("FAILED NAME CHANGE")