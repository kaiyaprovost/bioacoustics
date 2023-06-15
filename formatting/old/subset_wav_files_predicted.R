## Written by KP and JY on 9 Feb 2022

## goal of this script:
## Take TweetyNet annotations and subset the wav files
## so that we have smaller wavs to work with 

## steps 
## import selections files
## import wav files
## match selections and wav files to each other 
## cut the wav files based on the selection files

## import our libraries and set our working directory 
library(tuneR)
setwd("~/Documents/")

## import selections files 
## set mypath to be where the selections files are
mypath="/Users/kprovost/Documents/Postdoc_Working/JY_project/ANNOTATED/SELECTIONS/"
metafiles = list.files(path=mypath,
                       pattern=".selections.txt$",recursive = F,full.names = T)
#print(metafiles)

## import wav files 
wavpath="/Users/kprovost/Documents/Postdoc_Working/JY_project/ANNOTATED/WAVS/"
wavfiles = list.files(path=wavpath,
                      pattern=".wav$",recursive = F,full.names = T)
wavfiles = wavfiles[!grepl("wav_")]
#print(wavfiles)

## match selections and wav files

## extract "basenames" of the files -- takes off the file path
metafiles_basenames = basename(metafiles)
wavfiles_basenames = basename(wavfiles)

## remove all same text from the wavfiles
wavfiles_basenames_clean = sub(".wav","",wavfiles_basenames)
#print(wavfiles_basenames_clean) ## removes only the middle ".wav" from the subsetted files 

## remove the text from the metafiles
print(metafiles_basenames)
## if they all have the same ending text, use "sub"
## if they don't, need to use a different method like "gsub"
## for now they all have the same ending text 
metafiles_basenames_clean = sub(".selections.txt","",metafiles_basenames)
#print(metafiles_basenames_clean)

## check if they match
matches=intersect(wavfiles_basenames_clean,metafiles_basenames_clean)
#print(matches)

## figure out which of the originals are those matches
## give me the numbers of list A that are the same as list B
keep_wavs = which(wavfiles_basenames_clean %in% matches) ## just the indexes 
keep_meta = which(metafiles_basenames_clean %in% matches) ## just the indexes 

## only subset the wavfiles and metafiles that are kept
wavfiles_matched = wavfiles[keep_wavs]
metafiles_matched = metafiles[keep_meta]
#print(wavfiles_matched)
#print(metafiles_matched)
## explicitly check lengths match
#length(wavfiles_matched) == length(metafiles_matched)

## iterate over the matched metafiles 

for(meta_index in rev(1:length(metafiles_matched))){
  meta = metafiles_matched[meta_index] 
  print(meta)
  
  ## look up the wavfile in the same position as the metafile
  ## note: this assumes the wavfiles and metafiles are in the same order 
  ## note: ours are, for now 
  wavfile = wavfiles_matched[meta_index]
  print(wavfile)
  
  ## check to see if they match each other before moving on 
  ## extract basenames, do the sub functions, and see if they are identical
  do_they_match = sub(".selections.txt","",basename(meta)) == sub(".wav","",basename(wavfile))
  #print(do_they_match)
  
  if(do_they_match == TRUE) {
    
    ## read the wav file
    wav = readWave(wavfile,units = "samples",from=0) ## if you change this to seconds it can cut off bits
    ## get the sample rate, i.e., 48000hz 
    samples_per_ms=wav@samp.rate
    ## length of the wavfile in samples, which can be converted to seconds
    full_duration=length(wav)
    ## setting parameters for the splitting
    seconds_separated = 1 ## how much the selections need to be separated by to be split into two different wav files
    buffer = 1 ## how many seconds should be on either side of the new wavfile 
    buffer_samp = buffer*samples_per_ms ## converting buffer seconds to samples 
    
    ## read in the selection table 
    df = read.table(meta,header=T,sep="\t",check.names = F)
    ## sort selection table by time 
    df=df[order(df[,4]),] ## begin time is col 4
    ## renumber the selections if needed 
    df$Selection = 1:nrow(df)
    
    ## get separations of more than seconds_separated seconds
    ## apply functions: run a loop for you with less code and return a list for you
    differences = sapply(2:nrow(df), ## starting at the second row of the metafile, until the last row of the metafile
                         FUN=function(x){
      ## x is what we are looping over: number of row of the metafile
      row1 = df[x,4] ## begin time is col 4, get the start time of this row
      row2 = df[(x-1),5] ## end time is col 5, get the end time of the previous row
      dif = row1-row2
      return(dif)
    })
    
    ## find which of the differences are too long 
    need_splitting = which(differences >= seconds_separated)
    ## find when all of the sounds start and stop that need to be split up 
    lasts = df[c(need_splitting,nrow(df)),5] ## end time is col 5
    firsts = df[c(1,need_splitting+1),4] ## begin time is col 4
    soundlengths = lasts-firsts
    
    ## starting to generate the new start and end times for our new wavfiles 
    new_firsts = firsts-buffer ## calculates the start times, plus the buffer 
    new_lasts = lasts+buffer ## calculates the end times, plus the buffer 
    
    ## convert the start and end times to samples from seconds 
    boundaries = cbind(floor(new_firsts*samples_per_ms),ceiling(new_lasts*samples_per_ms))
    
    ## now we split the wav files at the boundaries we just calculated 
    for(rownum in 1:nrow(boundaries)) {
      ## buffer length calculated 
      this_buffer=buffer_samp
      row = boundaries[rownum,]
      ## convert to seconds as well 
      seconds = row/samples_per_ms
      
      ## if the value is less than zero, it must be zero instead 
      ## row[1] is the start time defined by boundaries
      if(row[1]<0){
        row[1]=0
        seconds[1]=0
        this_buffer=this_buffer-abs(boundaries[rownum,1])
      }
      
      ## if the value is longer than the entire song, it must be the length of the song instead 
      ## row[2] is the end time defined by boundaries 
      if(row[2]>full_duration){
        row[2]=full_duration
        seconds[2]=full_duration/samples_per_ms
      }
      
      
      this_buffer/samples_per_ms ## this is the amount of time between start of recording and start of first annotation
      
      ## generate the new files -- wavfile 
      newfile = paste(wavfile,"_",row[1],"-",row[2],".wav",sep="")
      writeWave(wav[row[1]:row[2]],newfile,extensible = F)
      
      ## THIS SUBSET IS NOT WORKING AND I DON'T KNOW WHY -- TODO figure out if this is still true 9 feb 2022
      ## subset the metafile to match the wavfile
      
      ## df is the original metafile, aka the selection table
      ## subset_df is made from df by only taking the selections that match the clipped wavfile
      subset_df = df[df[,4]>=seconds[1] & df[,5]<=seconds[2],] ## begin time is col 4, end time is col 5
      subset_df = subset_df[,1:7]
      
      seconds[2]-seconds[1]
      
      subset_df[,4] = subset_df[,4]-(seconds[1])
      subset_df[,5] = subset_df[,5]-(seconds[1])
      
      ## generate the new metafile 
      newmeta = paste(wavfile,"_",row[1],"-",row[2],".selections.txt",sep="")
      write.table(subset_df,newmeta,quote=F,sep="\t",row.names = F)
      
      
      
    }
  }
  try(R.utils::gzip(wavfile,overwrite=T))
}


## NOTES 
## 319 Z.l.n. songs and 398 Z.l.p songs
## most of the clipped sounds are less than 0.7 mb 