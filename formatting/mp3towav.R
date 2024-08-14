library(tuneR)

## TODO: update so that formatted correctly

<<<<<<< Updated upstream
## set our working directory that has the mp3 files in it
setwd("/Users/kprovost/Documents/Chondestes_grammacus/")
=======
setwd("/Users/kprovost/Documents/Research/")
mp3files = list.files(path=getwd(),
                      pattern="\\.mp3$",full.names = T,recursive=T)
## get rid of anything that is "weird" or in the "WEIRD" folder
mp3files=mp3files[!(grepl("WEIRD",mp3files))]
mp3files=mp3files[!(grepl("weird",mp3files))]
>>>>>>> Stashed changes

## get all the mp3 files in my folder
## \\. means it's a real dot, not a wildcard
## $ in the pattern means the end of the file
mp3files = list.files(path=getwd(),
                      pattern="\\.mp3$",full.names = TRUE,recursive=TRUE)

## reverse the order of mp3files
mp3files = mp3files[length(mp3files):1]

## randomize the order of mp3files
mp3files = sample(mp3files)

## for loop to convert all the mp3s to wavs
## if this fails, try scrambling the order
## if that still fails, audacity
for(mp3 in mp3files) {
  wavfile = gsub("mp3","wav",mp3)
  
  ## ! turns TRUE into FALSE and FALSE into TRUE
  ## if the wavfile does not exist already
  if(!(file.exists(wavfile))) {
    print(mp3)
    
    ## try means try it out, and only do something if there is no error
    
    try({
      r <- readMP3(mp3) ## read in the MP3 file in working directory
      })  
    
    ## if the mp3 file is in stereo, convert it to mono
    if(r@stereo) {
      ## convert the mp3 to mono using an average of both left and right channels
      try({r = mono(r, which = c("both"))}) 
    }
    
    ## output the wavfile to the path in "wavfile"
    try({writeWave(r,wavfile,extensible=FALSE)})
  } else {print("skip")}
  
  ## try to compress the mp3 file to be smaller as a gzip
  ## mp3.gz
  try({R.utils::gzip(mp3,overwrite=T)})
}

## go get the list of the wav files
wavfiles = list.files(path=getwd(),
                      pattern="\\.wav$",full.names = TRUE,recursive = TRUE)

<<<<<<< Updated upstream
=======
## code to check the sample rate
#y=sapply(wavfiles,FUN=function(wav){
#  return(sound::rate(wav))
#})
# tells you the frequency of each sample rate in your wavfiles
#sort(table(y))

## the sample rate you are going to convert to after checking the frequencies
preferred_sample_rate = 48000

## convert and rename the wavfiles to preferred sampling rate 
>>>>>>> Stashed changes
for(wav in (sort(wavfiles))) {
  print(wav)
  try({
    ## try reading the file in 
    r <- tuneR::readWave(wav,from=0,units="seconds")
    ## convert to mono if stereo
    if(r@stereo) {
      try({r = mono(r, which = c("both"))}) 
    }
    ## 48000 is the most common (mode) sampling rate
    ## convert the wav files into 48000 if needed
    if(sound::rate(wav) != 48000) {
      ## if it is not 48000, resample it to 48000
      r2=seewave::resamp(r,g=48000,output="Wave")
      
      ## write out the converted Wave, changing the name
      try({writeWave(r2,sub(".wav",".resample.48000.wav",wav),extensible=FALSE)})
      
      ## gzip the original wav file 
      try({R.utils::gzip(wav,overwrite=T)})
      
    } else {
      ## if it is ALREADY 48000
      ## just going to output the file again, changing the name
      try({writeWave(r,sub(".wav",".resample.48000.wav",wav),extensible=FALSE)})
      
      ## gzip the original wav file
      try({R.utils::gzip(wav,overwrite=T)})
    }
  })
}

## secifically for zipped
wavgzfiles = list.files(path=getwd(),
                        pattern="\\.wav\\.gz$",full.names = T,recursive = T)
wavgzfiles=wavgzfiles[!(grepl("resample",wavgzfiles))]
wavgzfiles=wavgzfiles[!(grepl("DONE",wavgzfiles))]
#outfile="/Users/kprovost/Documents/BLB.sampling.rates.13may2022.txt"
for(wavgz in rev(sort(wavgzfiles))) {
  wav = gsub("\\.gz","",wavgz)
  print(wav)
  outfile=sub(".wav",".resample.48000.wav",wav)
  outfile2=sub(".wav",".resample.48000.wav",wavgz)
  if(file.exists(outfile) | file.exists(outfile2)) {
    print("skipping")
    } else {
      R.utils::gunzip(wavgz)
    try({
      r <- tuneR::readWave(wav,from=0,units="seconds")
      if(r@stereo) {
        try({r = mono(r, which = c("both"))}) ## this is making things bad?
      }
      #if(r@samp.rate != 48000) {
      if(sound::rate(wav) != 48000) {
        
        #r = sound::loadSample(wav)
        #r <- tuneR::readWave(wav,from=0,units="seconds")
        #print(r)
        #r2=sound::setRate(r,48000) ## this causes too much distortion
        #print(r2)
        r2=seewave::resamp(r,g=48000,output="Wave")
        #print(wav)
        #write(paste(wav,r@samp.rate,sep="\t"),outfile,append=T)
        #sound::saveSample(r2,sub(".wav",".resample.48000.wav",wav),overwrite = T)
        try({writeWave(r2,outfile,extensible=FALSE)})
        try({R.utils::gzip(wav,overwrite=T)})
        try({R.utils::gzip(outfile,overwrite=T)})
      } else {
        try({writeWave(r,outfile,extensible=FALSE)})
        try({R.utils::gzip(wav,overwrite=T)})
        try({R.utils::gzip(outfile,overwrite=T)})
      }
    })
  }
}



