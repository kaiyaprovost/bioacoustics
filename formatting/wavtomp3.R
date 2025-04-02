library(tuneR)

## TODO: update so that formatted correctly

setwd("/Users/kprovost/Documents/Research/Elkin_Songs/")
wavfiles = list.files(path=getwd(),
                      pattern="\\.wav$",full.names = T,recursive = F)[1:10]

for(wav in (sort(wavfiles))) {
  print(wav)
  try({
    r <- tuneR::readWave(wav,from=0,units="seconds")
    ## if the wavfile is in stereo, convert it to mono
    if(r@stereo) {
      try({r = mono(r, which = c("both"))}) ## this is making things bad?
    }
      ## if it is the preferred rate, just reoutput it and zip
      try({writeWave(r,sub(".wav",paste(".resample.wav",sep=""),wav),extensible=FALSE)})
  })
}
