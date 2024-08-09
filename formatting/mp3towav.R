library(tuneR)

## TODO: update so that formatted correctly

setwd("/Users/kprovost/Documents/Tyrannidae/Contopus-cooperi")
mp3files = list.files(path=getwd(),
                      pattern="\\.mp3$",full.names = T,recursive=T)
## get rid of anything that is "weird" or in the "WEIRD" folder
mp3files=mp3files[!(grepl("WEIRD",mp3files))]
mp3files=mp3files[!(grepl("weird",mp3files))]

## sort your files based on file size 
#x <- file.info(mp3files)
#mp3files=mp3files[order(x$size)]
for(mp3 in (mp3files)[1:length(mp3files)]) {
  ## check if the wav file we are trying to make already exists
  if(!(file.exists(paste(mp3,".wav",sep="")))) {
    ## wav file does not exist
    print(mp3) ## prints the name of the mp3 file to convert
    try({r <- readMP3(mp3)})  ## read in the MP3 file in working directory
    ## convert from stereo to mono, but it is not working well for mp3s for some reason
    #if(r@stereo) {
    #  try({r = mono(r, which = c("both"))}) ## this is making things bad
    #}
    ## write the mp3 file as a wav file with format *mp3.wav
    try({writeWave(r,paste(mp3,".wav",sep=""),extensible=FALSE)})
  } else {print("skip")} # the wav file already exists, so we skip doing anything
  ## gzips the mp3 file to compress it, renaming it to *mp3.gz
  try({R.utils::gzip(mp3,overwrite=T)})
}

## if R breaks we need to do the ones that make it break in audacity

#setwd("/Users/kprovost/Documents/BLB_data/")
## finding the wav files and making sure they are all the same sampling rate
wavfiles = list.files(path=getwd(),
                      pattern="\\.wav$",full.names = T,recursive = T)
## getting rid of anything that has been processed already
wavfiles=wavfiles[!(grepl("resample",wavfiles))]
## getting rid of anything that is in a folder that we don't want to do again
wavfiles=wavfiles[!(grepl("copy",wavfiles))]
wavfiles=wavfiles[!(grepl("BAD",wavfiles))]
wavfiles=wavfiles[!(grepl("DONE",wavfiles))]
wavfiles=wavfiles[!(grepl("tweetynet-ed",wavfiles))]
#wavfiles=wavfiles[grepl("255595",wavfiles)]

## code to check the sample rate
#y=sapply(wavfiles[1:5],FUN=function(wav){
#  return(sound::rate(wav))
#})
## tells you the frequency of each sample rate in your wavfiles
#sort(table(y))

## the sample rate you are going to convert to after checking the frequencies
preferred_sample_rate = 48000

## convert and rename the wavfiles to preferred sampling rate 
for(wav in (sort(wavfiles))) {
  print(wav)
  try({
    r <- tuneR::readWave(wav,from=0,units="seconds")
    ## if the wavfile is in stereo, convert it to mono
    if(r@stereo) {
      try({r = mono(r, which = c("both"))}) ## this is making things bad?
    }
    
    if(sound::rate(wav) != preferred_sample_rate) {
      ## if it is not the preferred sampling rate, convert it to the preferred and output
      r2=seewave::resamp(r,g=preferred_sample_rate,output="Wave")
      try({writeWave(r2,sub(".wav",paste(".resample.",preferred_sample_rate,".wav",sep=""),wav),extensible=FALSE)})
      ## zip the unconverted file
      try({R.utils::gzip(wav,overwrite=T)})
    } else {
      ## if it is the preferred rate, just reoutput it and zip
      try({writeWave(r,sub(".wav",paste(".resample.",preferred_sample_rate,".wav",sep=""),wav),extensible=FALSE)})
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
  outfile=sub(".wav",paste(".resample.",preferred_sample_rate,".wav",sep=""),wav)
  outfile2=sub(".wav",paste(".resample.",preferred_sample_rate,".wav",sep=""),wavgz)
  if(file.exists(outfile) | file.exists(outfile2)) {
    print("skipping")
    } else {
      R.utils::gunzip(wavgz)
    try({
      r <- tuneR::readWave(wav,from=0,units="seconds")
      if(r@stereo) {
        try({r = mono(r, which = c("both"))}) ## this is making things bad?
      }
      #if(r@samp.rate != preferred_sample_rate) {
      if(sound::rate(wav) != preferred_sample_rate) {
        
        #r = sound::loadSample(wav)
        #r <- tuneR::readWave(wav,from=0,units="seconds")
        #print(r)
        #r2=sound::setRate(r,preferred_sample_rate) ## this causes too much distortion
        #print(r2)
        r2=seewave::resamp(r,g=preferred_sample_rate,output="Wave")
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



# setwd("/Users/kprovost/Documents/XenoCanto/")
# wavfiles = list.files(path="/Users/kprovost/Documents/XenoCanto/0_DONE/Trochilidae/Calypte/",
#                       pattern="\\.wav$",full.names = T,recursive = T)
# wavfiles=wavfiles[!(grepl("copy",wavfiles))]
# wavfiles=wavfiles[!(grepl("BAD",wavfiles))]
# 
# outfile="sampling.rates.txt"
# for(wav in rev(wavfiles)) {
#   r <- readWave(wav,from=0,units="seconds")
#   if(r@samp.rate != 48000) {
#     print(wav)
#     write(paste(wav,r@samp.rate,sep="\t"),outfile,append=T)
#   }
# }
# 
# for(wav in (wavfiles)) {
#   
#   try({r <- readWave(wav,from=0,units="seconds")  ## MP3 file in working directory
#   })
#   if(r@stereo) {
#     print(wav)
#     try({r = mono(r, which = c("both"))}) ## this is making things bad
#     try({writeWave(r,paste(wav,".copy",sep=""),extensible=FALSE)})
#   }
#   #},error={print("oop"); next})
# }


