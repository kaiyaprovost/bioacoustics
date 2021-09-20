library(tuneR)

setwd("/Users/kprovost/OneDrive - The Ohio State University/Song/XenoCanto/Passeriformes/Oscines/Passerellidae/Zonotrichia")
mp3files = list.files(path=getwd(),
                      pattern="\\.mp3$",full.names = T,recursive=T)
mp3files=mp3files[!(grepl("WEIRD",mp3files))]
mp3files=mp3files[!(grepl("weird",mp3files))]
#mp3files=mp3files[!(grepl("Cardinalidae",mp3files))]

#x <- file.info(mp3files)
#mp3files=mp3files[order(x$size)]
for(mp3 in sample(mp3files)) {
  if(!(file.exists(paste(mp3,".wav",sep="")))) {
    print(mp3)
    r <- readMP3(mp3)  ## MP3 file in working directory
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
wavfiles=wavfiles[!(grepl("weird",wavfiles))]
wavfiles=wavfiles[!(grepl("tweetynet-ed",wavfiles))]

outfile="/Users/kprovost/Documents/BLB.sampling.rates.20sep2021.txt"
for(wav in sample(wavfiles)) {
  #print(wav)
  try({
    r <- readWave(wav,from=0,units="seconds")
    if(r@samp.rate != 48000) {
      print(wav)
      write(paste(wav,r@samp.rate,sep="\t"),outfile,append=T)
      try({R.utils::gzip(wav,overwrite=T)})
    }
    })
}

setwd("/Users/kprovost/Documents/XenoCanto/")
wavfiles = list.files(path="/Users/kprovost/Documents/XenoCanto/0_DONE/Trochilidae/Calypte/",
                      pattern="\\.wav$",full.names = T,recursive = T)
wavfiles=wavfiles[!(grepl("copy",wavfiles))]
wavfiles=wavfiles[!(grepl("BAD",wavfiles))]

outfile="sampling.rates.txt"
for(wav in rev(wavfiles)) {
  r <- readWave(wav,from=0,units="seconds")
  if(r@samp.rate != 48000) {
    print(wav)
    write(paste(wav,r@samp.rate,sep="\t"),outfile,append=T)
  }
}

for(wav in (wavfiles)) {
  
  try({r <- readWave(wav,from=0,units="seconds")  ## MP3 file in working directory
  })
  if(r@stereo) {
    print(wav)
    try({r = mono(r, which = c("both"))}) ## this is making things bad
    try({writeWave(r,paste(wav,".copy",sep=""),extensible=FALSE)})
  }
  #},error={print("oop"); next})
}


