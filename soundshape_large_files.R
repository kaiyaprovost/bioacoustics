library(SoundShape)

## align the wavfiles 
path="~/Documents/Postdoc_Working/JY_Project/Soundshape/"
setwd(path)

# Create temporary folder to store ".wav" files
wav.at <- file.path(base::tempdir(), "original_wave")
if(!dir.exists(wav.at)) dir.create(wav.at)
# Create temporary folder to store results
store.at <- file.path(base::tempdir(), "output")
if(!dir.exists(store.at)) dir.create(store.at)

align.wave(wav.at=wav.at, wav.to="Aligned",time.length=10,f=48000,wl=512)