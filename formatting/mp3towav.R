library(tuneR)

## TODO: update so that formatted correctly

#setwd("/users/PYS1065/kprovost/bioacoustics/Sounds_and_Annotations/Aves/")
setwd("/Users/kprovost/Documents/Postdoc_Working/JY_Project/")
mp3files = list.files(path=getwd(),
                      pattern="\\.mp3$",full.names = T,recursive=T)
mp3files=mp3files[!(grepl("WEIRD",mp3files))]
mp3files=mp3files[!(grepl("weird",mp3files))]
#mp3files=mp3files[!(grepl("Cardinalidae",mp3files))]

#x <- file.info(mp3files)
#mp3files=mp3files[order(x$size)]
for(mp3 in (mp3files)[length(mp3files):1]) {
  if(!(file.exists(paste(mp3,".wav",sep="")))) {
    print(mp3)
    try({r <- readMP3(mp3)})  ## MP3 file in working directory
    if(r@stereo) {
      try({r = mono(r, which = c("both"))}) ## this is making things bad
    }
    try({writeWave(r,paste(mp3,".wav",sep=""),extensible=FALSE)})
  } else {print("skip")}
  try({R.utils::gzip(mp3,overwrite=T)})
}

#setwd("/Users/kprovost/Documents/BLB_data/")
wavfiles = list.files(path=getwd(),
                      pattern="\\.wav$",full.names = T,recursive = T)
wavfiles=wavfiles[!(grepl("copy",wavfiles))]
wavfiles=wavfiles[!(grepl("BAD",wavfiles))]
wavfiles=wavfiles[!(grepl("resample",wavfiles))]
wavfiles=wavfiles[!(grepl("DONE",wavfiles))]
wavfiles=wavfiles[!(grepl("tweetynet-ed",wavfiles))]
#wavfiles=wavfiles[grepl("255595",wavfiles)]

#outfile="/Users/kprovost/Documents/BLB.sampling.rates.13may2022.txt"
for(wav in (sort(wavfiles))) {
  print(wav)
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
      try({writeWave(r2,sub(".wav",".resample.48000.wav",wav),extensible=FALSE)})
      try({R.utils::gzip(wav,overwrite=T)})
    } else {
      try({writeWave(r,sub(".wav",".resample.48000.wav",wav),extensible=FALSE)})
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


