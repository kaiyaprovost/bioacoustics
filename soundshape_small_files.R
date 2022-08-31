library(SoundShape)
verbose=T
doplot=F
date=format(Sys.time(), "%d%b%Y")

## SOMETHING IN HERE IS CHANGING THE ACTUAL SOUNDS
## it is at the alignment step that the actual sounds change

## get the wavfiles 
path="/Users/kprovost/Documents/Postdoc_Working/Sounds_and_Annotations/Myiarchus.tuberculifer/Wave"
wav.at = "/Users/kprovost/Documents/Postdoc_Working/SoundShape"
setwd(path)
wavfiles = list.files(path,".wav$",full.names = T)
wavfiles = wavfiles[grepl("wav_",wavfiles)]

selectionfiles = list.files(path,"selections.txt$",full.names = T)
selectionfiles = selectionfiles[grepl("wav_",selectionfiles)]

maxdiflist = 0
  
## custom functions
align.wave.custom <- function(wav.at=NULL, wav.to="Aligned", time.length=1, time.perc=0.0, dBlevel=25, f=48000, wl=512, ovlp=70,
                              overwrite=F,verbose=T,alignCheckpoint=1)  {
  
  if(is.null(wav.at)) {stop("Use 'wav.at' to specify folder path where '.wav' files are stored")}
  
  # Create folder to store aligned calls
  if(!dir.exists(file.path(wav.at, wav.to))) dir.create(file.path(wav.at, wav.to))
  
  # Replace sounds for each ".wav" file in a folder
  filestoalign = list.files(wav.at, pattern = ".wav")
  numalignfiles=length(filestoalign)
  for(j in alignCheckpoint:numalignfiles){
    file = filestoalign[j]
    if(verbose==T) {print(paste(j,file,numalignfiles))}
    if(overwrite==T || !(file.exists(file.path(wav.at, wav.to, file)))){
      
      orig.wav0 <- tuneR::readWave(paste(wav.at,"/", file, sep=""))
      
      # Add silence to fill sound window and prevent error
      orig.wav <- seewave::addsilw(orig.wav0, f=f, at="end", d=(time.length*10), output = "Wave")
      
      # create spectro object
      orig.spec <- seewave::spectro(orig.wav, f=f, wl=wl, ovlp=ovlp, osc=F, grid=F, plot=F)
      
      # Acquire contours
      cont.spec <- grDevices::contourLines(x=orig.spec$time, y=orig.spec$freq, z=t(orig.spec$amp),
                                           levels=seq(-dBlevel,-dBlevel,1))
      
      # vectors to store minimum and maximum time values
      min.spec <- numeric(length(cont.spec))
      max.spec <- numeric(length(cont.spec))
      
      # minimum and maximum time values among contours detected
      for(i in 1:length(min.spec)){min.spec[i] <- min(cont.spec[[i]]$x)}
      for(i in 1:length(max.spec)){max.spec[i] <- max(cont.spec[[i]]$x)}
      
      # minimum and maximum time values
      t.min <- min(min.spec)
      t.max <- max(max.spec)
      
      if((t.min-(time.perc*time.length))<0)
        stop("Time percentage is too large. Consider a smaller value of 'time.perc'")
      
      # cut Wave file using minimum and maximum time values
      short.wav0 <- seewave::deletew(orig.wav, f=f, output = "Wave",
                                     from = (t.max+(time.perc*time.length)), to = max(orig.spec$time))
      
      short.wav <- seewave::deletew(short.wav0, f=f, output = "Wave",
                                    from = 0, to = (t.min-(time.perc*time.length)))
      
      
      # Add silence to fill sound window
      final.wav <- seewave::addsilw(short.wav, f=f, at="end", d=time.length, output = "Wave")
      
      tuneR::writeWave(final.wav, file.path(wav.at, wav.to, file), extensible = F)
    } else {
      if(verbose==T) {print("SKIPPING")}
    }
  } #end loop
  
} #end function

eigensound.custom <- function (analysis.type = "threeDshape", wav.at = NULL, store.at = wav.at, 
                               dBlevel = 25, flim = c(0, 10), tlim = c(0, 1), trel = tlim, 
                               x.length = 80, y.length = 60, log.scale = TRUE, back.amp = 35, 
                               add.points = FALSE, add.contour = TRUE, lwd = 1, EQ = c(0.05,0.15, 0.3, 0.5, 0.7, 0.85, 0.95), mag.time = 1, f = 48000, 
                               wl = 512, ovlp = 70, plot.exp = TRUE, plot.as = "jpeg", plot.type = "surface", 
                               rotate.Xaxis = 60, rotate.Yaxis = 40, TPS.file = NULL,doIndTPS=T,shuffle=F,eigcheckpoint=1,verbose=T,rev=F) {
  if (is.null(wav.at)) {
    stop("Use 'wav.at' to specify folder path where '.wav' files are stored")
  }
  if (is.null(analysis.type)) {
    stop("Method undefined in 'analysis.type'")
  }
  if (!is.null(analysis.type)) {
    if (analysis.type != "threeDshape") 
      stop("Invalid analysis specified by 'analysis.type'.")
  }
  if (plot.exp == TRUE) {
    if (plot.as != "jpeg" && plot.as != "tif" && plot.as != 
        "tiff") 
      stop("Invalid image format specified by 'plot.as'. Choose between 'jpeg', 'tif' or 'tiff'")
  }
  if (plot.exp == TRUE && analysis.type == "threeDshape") {
    if (plot.type != "surface" && plot.type != "points") 
      stop("Invalid 3D sound shape specified by 'plot.type'. Choose between 'surface' or 'points'")
  }
  if (!is.null(TPS.file)) {
    TPS.path <- paste(store.at, "/", TPS.file, ".tps", sep = "")
    file.create(TPS.path, showWarnings = TRUE)
  }
  if (analysis.type == "threeDshape") {
    files=list.files(wav.at, pattern = ".wav$")
    files=files[eigcheckpoint:length(files)]
    if(verbose==T){print(paste("CHECKPOINTED AT:",eigcheckpoint))}
    if(rev==T){
      files=rev(files)
      if(verbose==T){print("REVERSED")}
    }
    if(shuffle==T){
      files=sample(files)
      if(verbose==T){print("SHUFFLED")}
    }
    
    lenfiles=length(files)
    for (file_index in 1:length(files)) {
      file=files[file_index]
      if(verbose==T){print(paste(file,file_index,lenfiles))}
      
      indtps = paste(file,TPS.file,sep=".")
      indtps.Path <- paste(store.at, "/", indtps, ".tps", sep = "")
      
      if(doIndTPS==T & (file.exists(indtps.Path))){
        if(verbose==T){print("skipping")}
      } else {
        
        threeD <- tuneR::readWave(paste(wav.at, "/", file, 
                                        sep = ""))
        e = NULL
        try({e <- seewave::spectro(threeD, f = f, wl = wl, ovlp = ovlp, 
                                   flim = flim, tlim = tlim, plot = F)
        })
        if(is.null(e)){
          try({e <- seewave::spectro(threeD, f = f, wl = wl, ovlp = ovlp, 
                                     flim = flim, plot = F)
          })
        }
        freq.seq <- seq(1, length(e$freq), length = y.length)
        ifelse(isTRUE(log.scale), time.seq <- 10^(seq(log10(1), 
                                                      log10(length(e$time)), length.out = x.length)), 
               time.seq <- seq(1, length(e$time), length.out = x.length))
        time.sub <- e$time[time.seq]
        freq.sub <- e$freq[freq.seq]
        amp.sub <- e$amp[freq.seq, time.seq]
        for (i in 1:length(amp.sub)) {
          if (amp.sub[i] == -Inf | amp.sub[i] <= -dBlevel) {
            amp.sub[i] <- -dBlevel
          }
        }
        colnames(amp.sub) <- time.sub
        rownames(amp.sub) <- freq.sub
        ind.3D <- as.matrix(stats::setNames(reshape2::melt(t(amp.sub)), 
                                            c("time", "freq", "amp")))
        
        ifelse(!exists("coord"), coord <- array(data = ind.3D, 
                                                dim = c(dim(ind.3D), 1)), coord <- abind::abind(coord, 
                                                                                                ind.3D, along = 3))
        if (plot.exp == TRUE) {
          if (plot.as == "jpeg") {
            grDevices::jpeg(width = 5000, height = 3500, 
                            units = "px", res = 500, filename = paste(store.at, 
                                                                      "/", sub(".wav", "", file), ".jpg", sep = ""))
          }
          if (plot.as == "tiff" | plot.as == "tif") {
            grDevices::tiff(width = 5000, height = 3500, 
                            units = "px", res = 500, filename = paste(store.at, 
                                                                      "/", sub(".wav", "", file), ".tif", sep = ""))
          }
          if (plot.type == "surface") {
            plot3D::persp3D(x = time.sub, y = freq.sub, 
                            z = t(amp.sub), border = "black", lwd = 0.1, 
                            theta = rotate.Xaxis, phi = rotate.Yaxis, 
                            resfac = 1, r = 3, expand = 0.5, cex.axis = 0.7, 
                            scale = T, axes = T, col = seewave::spectro.colors(n = 100), 
                            ticktype = "detailed", nticks = 4, xlab = "Time (s)", 
                            ylab = "Frequency (kHz)", zlab = "Amplitude (dB)", 
                            main = sub(".wav", "", file), clab = expression("Amplitude dB"))
          }
          if (plot.type == "points") {
            plot3D::scatter3D(x = ind.3D[, 1], y = ind.3D[, 
                                                          2], z = ind.3D[, 3], pch = 21, cex = 0.5, 
                              theta = rotate.Xaxis, phi = rotate.Yaxis, 
                              resfac = 1, r = 3, expand = 0.5, cex.axis = 0.7, 
                              scale = T, axes = T, col = seewave::spectro.colors(n = 100), 
                              ticktype = "detailed", nticks = 4, xlab = "Time (s)", 
                              ylab = "Frequency (kHz)", zlab = "Amplitude (dB)", 
                              main = sub(".wav", "", file), clab = expression("Amplitude dB"))
          }
          grDevices::dev.off()
        }
        if (!is.null(TPS.file)) {
          lmline <- paste("LM=", dim(ind.3D)[1], sep = "")
          idline <- paste("ID=", sub(".wav", "", file), 
                          sep = "")
          if(doIndTPS==T){
            file.create(indtps.Path, showWarnings = TRUE)
            write(lmline, indtps.Path, append = TRUE)
            utils::write.table(ind.3D, indtps.Path, col.names = FALSE, 
                               row.names = FALSE, append = TRUE)
            write(idline, indtps.Path, append = TRUE)
            write("", indtps.Path, append = TRUE)
          } else {
            write(lmline, TPS.path, append = TRUE)
            utils::write.table(ind.3D, TPS.path, col.names = FALSE, 
                               row.names = FALSE, append = TRUE)
            write(idline, TPS.path, append = TRUE)
            write("", TPS.path, append = TRUE)
            
          }
          
          rm(lmline, idline)
        }
      }
      
    }
    dimnames(coord)[[3]] <- sub(".wav", "", list.files(wav.at, 
                                                       pattern = ".wav"))
  }
  else (coord <- NULL)
  if (analysis.type == "threeDshape") {
    coord <- coord
    
  }
  results <- coord
}
readmulti.tps.custom <- function(filelist, ... ){
  tps.list <- filelist
  readland.args <- list(...)
  if(is.null(readland.args$specID)) readland.args$specID <- "None"
  
  file.ext <- substr(tps.list, nchar(tps.list)-3, nchar(tps.list))
  if(!all(file.ext%in%c(".tps", ".TPS"))) 
    stop("File list includes files in a format other than tps, please ammend")
  
  dt.dims <- sapply(1:length(tps.list), function(x){
    print(paste(x,length(tps.list)))
    dim(readland.tps(tps.list[x], ...))
  }, simplify = T)
  p1 <- dt.dims[1, 1]; k1 <- dt.dims[2, 1]; n1 <- dt.dims[3, 1]
  
  if(any(dt.dims[1,]!=p1)) stop("Input tps files include different numbers of landmarks, please correct")
  
  if(any(dt.dims[2,]!=k1)) stop("Input tps files include landmarks in different dimensions (2D and 3D), please correct")
  
  all.lms <- NULL
  for(f in 1:length(tps.list)){   
    print(paste("SECOND:",f,length(tps.list)))
    lms <- two.d.array(readland.tps(tps.list[f], ...))
    all.lms <- rbind(all.lms, lms)
  }
  all.lms <- arrayspecs(all.lms, p1, k1)
  
  if(any(table(dimnames(all.lms)[3])!=1)) {
    if(readland.args$specID != "imageID") {
      dimnames(all.lms)[[3]] <- 1:dim(all.lms)[3]
    } else {
      warning("Input files seem to include repeated specimen names")
    }
  }
  
  return(all.lms)
}




## cut up the files
for(j in 1:length(wavfiles)){
  print(j)
  wavfile = wavfiles[j]
  prefix=substr(wavfile,1,nchar(wavfile)-4)
  prefix=basename(prefix)
  selection = selectionfiles[grepl(prefix,selectionfiles)]
  
  if(length(selection)==1){
    ## do the stuff
    wav=tuneR::readWave(wavfile)
    wavdf = read.table(selection,header=T,sep="\t")
    if(is.null(wavdf$Delta.Time..s.)){
      wavdf$Delta.Time..s. = (wavdf$End.Time..s. - wavdf$Begin.Time..s.)
    }
    maxdif = max(wavdf$Delta.Time..s.,na.rm=T)
    maxdiflist = max(c(maxdiflist,maxdif),na.rm=T)
    
    for(i in 1:nrow(wavdf)){
      if(wavdf$End.Time..s.[i]-wavdf$Begin.Time..s.[i]<=0) {
        if(verbose==T){print(paste("SKIPPING ROW WITH NEGATIVE SIZE",i))}
      } else {
        
        cutwav=NULL
        try({cutwav = seewave::cutw(wav,from=wavdf$Begin.Time..s.[i],to=wavdf$End.Time..s.[i],output="Wave")})
        if(is.null(cutwav)){
          print("redo")
          try({cutwav = seewave::cutw(wav,from=wavdf$Begin.Time..s.[i],to=wavdf$End.Time..s.[i],output="Wave")})
        }
        cutwav_f=NULL
        try({cutwav_f=seewave::ffilter(cutwav, from = 0, to = 500, bandpass = FALSE,wl = 512, ovlp = 70, output="Wave",rescale=T)})
        
        if(!(is.null(cutwav_f))){
          
          try({tuneR::writeWave(cutwav_f, filename = file.path(wav.at, paste(prefix,i,"wav",sep=".")), extensible = FALSE)})
        }
        
        
      }
      
    }
    
  } else {
    print("ERROR")
  }
  
}

## align the files
align.wave.custom(wav.at=wav.at, wav.to="Aligned", time.length = maxdiflist,time.perc=0)

## calculate the eigenvectors -- lengthy
eig.sample <- eigensound.custom(analysis.type="threeDshape",
                         wav.at=file.path(wav.at, "Aligned"),
                         store.at=file.path(wav.at, "Stored"),
                         TPS.file = "tpsfile",
                         eigcheckpoint = 1,
                         log.scale = T
                         )
geomorph::writeland.tps(eig.sample, file=paste(wav.at,"/fulltps.",date,".tps",sep=""), scale = NULL, specID = TRUE)



pca.eig.sample <- stats::prcomp(geomorph::two.d.array(eig.sample))
summary(pca.eig.sample)

samples = dimnames(eig.sample)[[3]]
samp_split=lapply(samples,FUN=function(x){
  y=strsplit(x,"resample.48000")[[1]]
  individual=y[1]
  subind=y[2]
  z=strsplit(subind,"\\.")[[1]]
  indrange=z[1]
  indsyll=z[2]
  q=cbind(individual,indrange,indsyll)
  })
sample_split_df = do.call(rbind,samp_split)
sample_split_df = as.data.frame(sample_split_df)
sample_split_df$samples=samples

plot(pca.eig.sample$x[,1],
     pca.eig.sample$x[,2],
     col=as.numeric(as.factor(sample_split_df[,3])))

min_pc1 = min(pca.eig.sample$x[,1],na.rm=T)
min_pc2 = min(pca.eig.sample$x[,2],na.rm=T)
max_pc1 = max(pca.eig.sample$x[,1],na.rm=T)
max_pc2 = max(pca.eig.sample$x[,2],na.rm=T)
sample_split_df[which(pca.eig.sample$x[,1] == min_pc1),]
sample_split_df[which(pca.eig.sample$x[,1] == max_pc1),]
sample_split_df[which(pca.eig.sample$x[,2] == min_pc2),]
sample_split_df[which(pca.eig.sample$x[,2] == max_pc2),]

sample_split_df$PC1rank=as.numeric(dplyr::percent_rank(pca.eig.sample$x[,1]))
sample_split_df$PC2rank=as.numeric(dplyr::percent_rank(pca.eig.sample$x[,2]))
plot(sample_split_df$PC1rank,pca.eig.sample$x[,1])
plot(sample_split_df$PC2rank,pca.eig.sample$x[,2])
sample_split_df$PC1group=as.numeric(dplyr::ntile(pca.eig.sample$x[,1],10))
sample_split_df$PC2group=as.numeric(dplyr::ntile(pca.eig.sample$x[,2],10))

plot(sample_split_df$PC1group,pca.eig.sample$x[,1])
plot(sample_split_df$PC2group,pca.eig.sample$x[,2])

png("eig.sample.pca.groups_PC2.png")
par(mfrow=c(2,5))
for(i in 1:10){
  hypo.surf(eig.sample[,,sample_split_df$samples[sample_split_df$PC2group==i]],  PC="mean", flim = c(0, 15), tlim = c(0, 1),
            wl = 512, ovlp = 70,x.length = 80, y.length = 60, rotate.Xaxis = 60, rotate.Yaxis = 40, 
            log.scale = TRUE,f=48000)
}
dev.off()
png("eig.sample.pca.groups_PC1.png")
par(mfrow=c(2,5))
for(i in 1:10){
  hypo.surf(eig.sample[,,sample_split_df$samples[sample_split_df$PC1group==i]],  PC="mean", flim = c(0, 15), tlim = c(0, 1),
            wl = 512, ovlp = 70,x.length = 80, y.length = 60, rotate.Xaxis = 60, rotate.Yaxis = 40, 
            log.scale = TRUE,f=48000)
}
dev.off()

hypo.surf(eig.sample,  PC="mean", flim = c(0, 15), tlim = c(0, 1),
          wl = 512, ovlp = 70,x.length = 80, y.length = 60, rotate.Xaxis = 60, rotate.Yaxis = 40, 
          log.scale = TRUE,f=48000)

