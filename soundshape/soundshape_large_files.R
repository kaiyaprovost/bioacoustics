library(SoundShape)

## align the wavfiles 
path="/Users/kprovost/Documents/Postdoc_Working/Sounds_and_Annotations/Poecile.carolinensis/Wave/"
setwd(path)

# Create temporary folder to store ".wav" files
wav.at <- file.path(base::tempdir(), "original_wave")
if(!dir.exists(wav.at)) dir.create(wav.at)
# Create temporary folder to store results
store.at <- file.path(base::tempdir(), "output")
if(!dir.exists(store.at)) dir.create(store.at)

align.wave(wav.at=wav.at, wav.to="Aligned",time.length=10,f=48000,wl=512)