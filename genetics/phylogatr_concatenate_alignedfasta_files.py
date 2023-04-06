#!/usr/bin/env python3
## Made by Kaiya L Provost
## Last updated 17 Feb 2021

import sys
import glob
import os

try:
    directory = str(sys.argv[1])
    print("\Directory is: ",infile)
except:
    print("Directory not given, quitting")
    exit()
    directory="/Users/kprovost/OneDrive - The Ohio State University/Phylogatr_Data/Birds-phylogatr-results_7dec2020/ALLONEFILE/"

os.chdir(directory)

files = glob.glob("**/*.afa",recursive=True)

## need to separate out by gene 

genes = [i.split("-")[-1].split(".")[0] for i in files]
unique_genes = list(set(genes))
unique_genes.sort()

## to the afa files, need to add same information directly after ">" (underscore sep)
## outputs: one file for each gene for the afas

for i in range(len(files)):
    file = files[i]
    gene = genes[i]
    if (i % 100 == 0):
        print(i,gene,file)
    order,family,species,filename=file.split("/")
    with open(file,"r") as infile:
        data=infile.read()
    ## go through and replace ">" with ">+order/family/species/"
    outfilename=gene+".merged.fasta"
    with open(outfilename,"a") as outfile:
        ## the "_" before this is to stop it printing to the terminal when it outputs
        _ = outfile.write(data.replace(">",">"+order+"/"+family+"/"+species+"/"))

## to occurrences, need to add the information from the order, family, species name, etc (tab sep) to each line
## outputs: one master occurrence record. 

occurrencefiles =  glob.glob("**/occurrences.txt",recursive=True)

for i in range(len(occurrencefiles)):
    file = occurrencefiles[i]
    if (i % 100 == 0):
        print(i,file)
    order,family,species,filename=file.split("/")
    with open(file,"r") as infile:
        lines=infile.readlines()
    newlines = [ line.strip()+"\t"+order+"\t"+family+"\t"+species+"\n" for line in lines[1:] ]
    with open("master_occurrence.txt","a") as outfile:
        if i == 0:
            header=lines[0].strip()+"\torder\tfamily\tspecies\n"
            _ = outfile.write(header)
        _ = outfile.writelines(newlines)
