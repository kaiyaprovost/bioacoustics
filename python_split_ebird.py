#!/usr/bin/env python3## Made by Kaiya L Provost## Last updated 17 March 2022 (Happy St Patricks Day)import osimport globimport gzipimport shutil#import pandas## get filesos.chdir("C:\\Users\\kaiya\\Documents\\Work");os.getcwd();files = glob.glob("*201799069?")print(len(files))for filename in files: count = 0; ## NOTE: doing this with pandas is actually slower than doing line by line when the files are already split up # ## import data # pd = pandas.read_csv(filename,sep="\t",low_memory=False,header=None) # ## extract species # spp = list(pd[pd.columns[5]].unique()) # ## iterate over species  # for i in range(len(spp)):     # species = spp[i]     # print(str(filename)+":"+str(species)+":"+str(i)+"/"+str(len(spp)))     # subset = pd.loc[pd[pd.columns[5]] == species]     # outfilename="ebd_relMay-2017.txt"+"_"+species+".txt";     # pd.to_csv(outfilename, mode='a', index=False, header=None,sep="\t") with open(filename,"r", encoding='utf-8') as infile:     lines = infile.readlines() for line in lines:     if count % 10000 == 0:         print(str(filename)+":"+str(count));     if count == 0:         header = line;     else:         try:             splits = line.split("\t");             ## 6th one is the correct one for species             species = splits[5].replace(" ","-").replace("/","x");             #outfilename=filename+"_"+species+".txt";             outfilename = "ebd_relMay-2017.txt"+"_"+species+".txt";             if os.path.exists(outfilename):                 with open(outfilename,"a") as outfile:                     _=outfile.write(line);             else:                 with open(outfilename,"a") as outfile:                     _=outfile.write(header); ## need to include the "_" or it writes extra stuff                     _=outfile.write(line);         except:             #print("ISSUE:"+str(count));             pass     count += 1; with open(filename, 'rb') as f_in:     with gzip.open(filename+".txt.gz", 'wb') as f_out:         shutil.copyfileobj(f_in, f_out) os.unlink(filename)## 401,000,000 = 140gb## 40,100,000 = 14 gb ## 13,035,591