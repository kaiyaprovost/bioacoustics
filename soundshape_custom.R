library(SoundShape)
install.packages("SoundShape")

path = "~/songselections"

wav.at = file.path(path, "ClippedWavs")
dir.create(wav.at)

store_dir = file.path(wav.at, "Stored")
dir.create(store_dir)

## cut our wavfiles
## read in our wavfiles one at a time
## first we get the wavfiles
files = list.files(path = path,
                   pattern = "wav$",
                   full.names = T)
for (my_file in files) {
  print(my_file)
  wav = tuneR::readWave(my_file)
  ## look up the data with this file
  my_data_file = sub(pattern = "wav", replacement = "Table.1.selections.txt", my_file)
  
  if (file.exists(my_data_file) == TRUE) {
    wav_data = read.delim(my_data_file)
    ## use the start and end time of the data to tell R where to clip the wavfile
    for (row_number in 1:nrow(wav_data)) {
      print(row_number)
      start_time = wav_data$Begin.Time..s.[row_number]
      stop_time  = wav_data$End.Time..s.[row_number]
      cut_wav = seewave::cutw(
        wave = wav,
        from = start_time,
        to = stop_time,
        output = "Wave"
      )
      tuneR::writeWave(cut_wav, filename = file.path(wav.at, paste(
        basename(my_file), row_number, "wav", sep = "."
      )))
    }
  }
}

## align our wavfiles!

## we need that a custom function to do this so that soundshape does not break

align.wave.custom <-
  function(wav.at = NULL,
           wav.to = "Aligned",
           time.length = 1,
           time.perc = 0.0,
           dBlevel = 25,
           f = 44100,
           wl = 512,
           ovlp = 70,
           overwrite = F,
           verbose = T,
           alignCheckpoint = 1)  {
    if (is.null(wav.at)) {
      stop("Use 'wav.at' to specify folder path where '.wav' files are stored")
    }
    
    # Create folder to store aligned calls
    if (!dir.exists(file.path(wav.at, wav.to)))
      dir.create(file.path(wav.at, wav.to))
    
    # Replace sounds for each ".wav" file in a folder
    filestoalign = list.files(wav.at, pattern = ".wav")
    numalignfiles = length(filestoalign)
    for (j in alignCheckpoint:numalignfiles) {
      file = filestoalign[j]
      if (verbose == T) {
        print(paste(j, file, numalignfiles))
      }
      if (overwrite == T ||
          !(file.exists(file.path(wav.at, wav.to, file)))) {
        orig.wav0 <- tuneR::readWave(paste(wav.at, "/", file, sep = ""))
        
        # Add silence to fill sound window and prevent error
        orig.wav <-
          seewave::addsilw(
            orig.wav0,
            f = f,
            at = "end",
            d = (time.length * 10),
            output = "Wave"
          )
        
        # create spectro object
        orig.spec <-
          seewave::spectro(
            orig.wav,
            f = f,
            wl = wl,
            ovlp = ovlp,
            osc = F,
            grid = F,
            plot = F
          )
        
        # Acquire contours
        cont.spec <-
          grDevices::contourLines(
            x = orig.spec$time,
            y = orig.spec$freq,
            z = t(orig.spec$amp),
            levels = seq(-dBlevel,-dBlevel, 1)
          )
        
        # vectors to store minimum and maximum time values
        min.spec <- numeric(length(cont.spec))
        max.spec <- numeric(length(cont.spec))
        
        # minimum and maximum time values among contours detected
        for (i in 1:length(min.spec)) {
          min.spec[i] <- min(cont.spec[[i]]$x)
        }
        for (i in 1:length(max.spec)) {
          max.spec[i] <- max(cont.spec[[i]]$x)
        }
        
        # minimum and maximum time values
        t.min <- min(min.spec)
        t.max <- max(max.spec)
        
        if ((t.min - (time.perc * time.length)) < 0)
          stop("Time percentage is too large. Consider a smaller value of 'time.perc'")
        
        # cut Wave file using minimum and maximum time values
        short.wav0 <- seewave::deletew(
          orig.wav,
          f = f,
          output = "Wave",
          from = (t.max + (time.perc * time.length)),
          to = max(orig.spec$time)
        )
        
        short.wav <-
          seewave::deletew(
            short.wav0,
            f = f,
            output = "Wave",
            from = 0,
            to = (t.min - (time.perc * time.length))
          )
        
        
        # Add silence to fill sound window
        final.wav <-
          seewave::addsilw(
            short.wav,
            f = f,
            at = "end",
            d = time.length,
            output = "Wave"
          )
        
        tuneR::writeWave(final.wav, file.path(wav.at, wav.to, file), extensible = F)
      } else {
        if (verbose == T) {
          print("SKIPPING")
        }
      }
    } #end loop
    
  } #end function

align.wave.custom(
  wav.at = wav.at,
  wav.to = "Aligned",
  time.length = 1.0,
  f = 48000,
  time.perc = 0,
  alignCheckpoint = 583,
  overwrite = F
)
## time.length is the percent of the sound
## try playing around with time.length if things are not working

## generate the eigensample
eig.sample <- eigensound(
  analysis.type = "threeDshape",
  f = 48000,
  log.scale = TRUE,
  wav.at = file.path(wav.at, "Aligned"),
  store.at = store_dir
)
pca.eig.sample <- stats::prcomp(
  geomorph::two.d.array(
    eig.sample
  )
)
summary(pca.eig.sample)
rotation = pca.eig.sample$rotation
importance = summary(pca.eig.sample)$importance
data = pca.eig.sample$x
data = as.data.frame(data)
plot(data$PC1,data$PC2)

write.table(data,"~/pca_data.txt")
write.table(rotation,"~/pca_rotation.txt")
write.table(importance,"~/pca_importance.txt")
