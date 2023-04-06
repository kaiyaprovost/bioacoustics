library(warbleR)
library(Rraven)
library(soundgen)
library(SoundShape)
library(hypervolume)
library(Rmpi)# for mpi.* 
library(snow) # for clusterExport, clusterApply

## learn to make parallel for slurm
#A function representing the core 'work' for our task: sum elements of a large vector
#We'll want to run this proc 100x
myProc <- function(size=500){
  # Load a large vector 
  vec <- rnorm(size) 
  # Now sum the vec values 
  return(sum(vec)) }
max_loop <- 100 
## version 5: use snow backed by Rmpi 
workers=2
#workers <- as.numeric(Sys.getenv(c("SLURM_NTASKS")))-1 
cl <- makeCluster(workers, type="MPI") # MPI tasks to use 
clusterExport(cl, list('myProc')) 
tick <- proc.time()
result <- clusterApply(cl, 1:max_loop, function(i) myProc()) 
write.table(result, file = "foo.csv", sep = ",") 
tock <- proc.time() - tick 
cat("\nsnow w/ Rmpi test times using", workers, "MPI workers: \n")
tock 
# stopCluster(cl)  ## this hangs for some reason
#mpi.quit()



mainpath = "/Users/kprovost/OneDrive - The Ohio State University/BLB_Data/Cardinalidae/Cardinalis/Cardinalis_sinuatus/"
setwd(mainpath)

date=format(Sys.time(), "%d%b%Y")
#date="19may2021"

align.wave.custom <- function(wav.at=NULL, wav.to="Aligned", time.length=1, time.perc=0.0, dBlevel=25, f=44100, wl=512, ovlp=70,
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

eigensound.custom = function (analysis.type = threeDshape, wav.at = NULL, store.at = wav.at, 
                              dBlevel = 25, flim = c(0, 10), tlim = c(0, 1), trel = tlim, 
                              x.length = 80, y.length = 60, log.scale = TRUE, back.amp = 35, 
                              add.points = FALSE, add.contour = TRUE, lwd = 1, EQ = c(0.05,0.15, 0.3, 0.5, 0.7, 0.85, 0.95), mag.time = 1, f = 44100, 
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
    files=list.files(wav.at, pattern = ".wav")
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
        e <- seewave::spectro(threeD, f = f, wl = wl, ovlp = ovlp, 
                              flim = flim, tlim = tlim, plot = F)
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
} #end function


## for i in table1files
path=c("/Users/kprovost/Documents","/Users/kprovost/OneDrive - The Ohio State University/BLB_Data", "/Users/kprovost/OneDrive - The Ohio State University/XenoCanto")
pattern=c(".Table.1.selections.txt",".selections.txt")
listfiles=c()
for(pa in path){
  print(pa)
  for(patt in pattern){
    print(patt)
    listfiles1=list.files(path=pa,pattern=patt,recursive=T,full.names = T)
    print(length(listfiles1))
    listfiles=c(listfiles,listfiles1)
  }
}
listfiles = listfiles[grepl("Zonotrichia",basename(listfiles))]
listfiles = listfiles[!(grepl("xml",listfiles))]
listfiles = listfiles[!(grepl("wav_",listfiles))]
listfiles=unique(listfiles)

run_soundshape = function(path,pattern,doplot=F,verbose=F,outpath=NULL,listfiles=NULL,tpsname="eig.sample",clusterind=F,overwrite=T,
                          checkpointNumber=1,pcascalemin=10,pcascale=T,redo_eig=T,alignCheckpoint=1,eigcheckpoint=1,manualcombinetps=F,shuffle=F,rev=F){
  print(Sys.time())
  
  if(is.null(outpath)){
    outpath = base::tempdir()
  }
  
  if(is.null(listfiles)) {
    if(verbose==T){print("generating listfiles")}
    if(length(path)==1) {
      listfiles=c()
      for(patt in pattern){
        listfiles = c(listfiles,list.files(path=path,pattern=patt,recursive = T,full.names = T))
      }
      
    } else {
      for(patt in pattern){
        listfiles1 = sapply(path,FUN=function(x){listfiles = list.files(path=x,pattern=patt,recursive = T,full.names = T)})
        listfiles1 = as.vector(unlist(listfiles))
        listfiles=c(listfiles,listfiles1)
      }
      #path=path[1]
    }
    
    listfiles = listfiles[!(grepl("xml",listfiles))]
    listfiles = listfiles[!(grepl("wav_",listfiles))]
  }
  
  if(verbose==T){print("creating smallwav folders")}
  wav.at <- file.path(outpath,"original_wave_bandpass")
  if(!dir.exists(wav.at)) dir.create(wav.at)
  store.at <- file.path(outpath,"output_temp")
  if(!dir.exists(store.at)) dir.create(store.at)
  
  listnotes = c()
  listmaxdif = c()
  
  if(verbose==T){print("looping over list of files")}
  
  ## this is probably what can be paralellized  
  for (file in listfiles[checkpointNumber:length(listfiles)]) {
    if(verbose==T){print(paste(which(listfiles==file),basename(file)))}
    wavfile=file
    for(patt in pattern){
      wavfile = sub(patt,".wav",wavfile)
    }
    
    if(file.exists(wavfile)) {
      ## generate cutwav
      wav=NULL
      try({wav=tuneR::readWave(wavfile)})
      if(is.null(wav)){
        print("WAV COULD NOT BE READ")
      } else {
        wavdf = read.table(file,header=T,sep="\t")
        listnotes = c(listnotes,nrow(wavdf))
        
        if(is.null(wavdf$Delta.Time..s.)){
          wavdf$Delta.Time..s. = (wavdf$End.Time..s. - wavdf$Begin.Time..s.)
        }
        
        maxdif = max(wavdf$Delta.Time..s.,na.rm=T)
        
        prefix = sub(".wav","",basename(wavfile))
        
        for(i in 1:nrow(wavdf)){
          #if(verbose==T){print(i)}
          
          if(!(file.exists(file.path(wav.at, paste(prefix,i,"wav",sep="."))))) {
            
            if(wavdf$End.Time..s.[i]-wavdf$Begin.Time..s.[i]<=0) {
              if(verbose==T){print(paste("SKIPPING ROW WITH NEGATIVE SIZE",i))}
            } else {
              
              cutwav=NULL
              try({cutwav = seewave::cutw(wav,from=wavdf$Begin.Time..s.[i]-maxdif*0.01,to=wavdf$Begin.Time..s.[i]+maxdif,output="Wave")})
              if(is.null(cutwav)){
                print("redo")
                try({cutwav = seewave::cutw(wav,from=wavdf$Begin.Time..s.[i],to=wavdf$End.Time..s.[i],output="Wave")})
              }
              cutwav_f=NULL
              try({cutwav_f=seewave::ffilter(cutwav, from = 0, to = 500, bandpass = FALSE,wl = 512, ovlp = 70, output="Wave",rescale=T)})
              
              if(!(is.null(cutwav_f))){
                if(doplot==T){
                  
                  
                  par(mfrow=c(1,2), mar=c(0,2,1,0))
                  threeDspectro(cutwav, flim=c(0.5, 10), 
                                colkey=list(plot=FALSE), cex.axis=0.4, cex.lab=0.8, resfac=3)
                  threeDspectro(cutwav, flim=c(0.5, 10), plot.type="points",
                                samp.grid=TRUE, x.length=70, y.length=50, main="Semilandmarks 3D",
                                colkey=list(plot=FALSE), cex.axis=0.4, cex.lab=0.8)
                  
                  
                  par(mfrow=c(1,2), mar=c(4,4,2,1)) # view side by side
                  seewave::oscillo(cutwav, title="Oscillogram")
                  seewave::spectro(cutwav, flim=c(0.5, 10), grid=FALSE, scale=FALSE, main="Spectrogram")
                  
                  
                  par(mfrow=c(1,2), mar=c(4,4,1,1))
                  
                  # Set background at -25 dB and remove -Inf values from spectrogram data 
                  spec_cut <- seewave::spectro(cutwav_f, flim=c(0, 10),  
                                               f=48000,wl=512, ovlp=70,contlevels = seq(-25, -25, 1),
                                               collevels = seq(-25, 0, 0.1),fastdisp=T,
                                               plot=doplot)
                  for(i in 1:length(spec_cut$amp)){if(spec_cut$amp[i] == -Inf |spec_cut$amp[i] <= -25)
                  {spec_cut$amp[i] <- -25}}
                  
                  # 3D spectrogram (with a lower dBlevel for illustrative purpuses)
                  threeDspectro(cutwav_f, dBlevel=25, flim=c(0, 10), main="",
                                colkey=list(plot=FALSE), cex.axis=0.4, cex.lab=0.8, resfac=2) 
                  
                  plot3D::contour3D(x=spec_cut$time, y=spec_cut$freq, colvar=t(spec_cut$amp), z=-25,
                                    plot=T, add=T, addbox=F, col="black", lwd=1.9, nlevels=2, dDepth=0.25)
                  
                  
                  # Add curve of relative amplitude
                  
                  threeDspectro(cutwav_f, samp.grid=TRUE, x.length=70, y.length=47, plot.type="surface", 
                                dBlevel=25, flim=c(0, 10),  f=48000, wl=512, ovlp=70, main="As 'surface'",
                                colkey=list(plot=FALSE), cex.axis=0.4, cex.lab=0.8)
                  
                  # As "points"
                  threeDspectro(cutwav_f, samp.grid=TRUE, x.length=70, y.length=47, plot.type="points", 
                                dBlevel=25, flim=c(0, 10), f=48000, wl=512, ovlp=70, main="As 'points'",
                                colkey=list(plot=FALSE), cex.axis=0.4, cex.lab=0.8)
                }
                
                try({tuneR::writeWave(cutwav_f, filename = file.path(wav.at, paste(prefix,i,"wav",sep=".")), extensible = FALSE)})
              }
              
              
            }
          }
        }
        listmaxdif=c(listmaxdif,maxdif)
      }} else {
        print(paste("can't find wav:",wavfile))
        listnotes = c(listnotes,NA)
        listmaxdif = c(listmaxdif,NA)
      }
  }
  
  if(verbose==T){print("aligning wavs (lengthy)"); print(Sys.time())}
  align.wave.custom(wav.at=wav.at, wav.to="Aligned",
                    f=48000,time.perc=0,overwrite=overwrite,alignCheckpoint = alignCheckpoint)
  
  if(doplot==T) {
    SoundShape::eigensound(analysis.type = "twoDshape", wav.at = file.path(wav.at, "Aligned"),
                           store.at=store.at, plot.exp=TRUE, flim=c(0, 10), tlim=c(0, 0.8),
                           fastdisp=T)
  }
  
  
  if(verbose==T){print("calculating eigensound (lengthy)");print(Sys.time())}
  if(redo_eig==T || !(file.exists(file.path(store.at,paste(tpsname,".tps",sep=""))))){
    eig.sample <- eigensound.custom(analysis.type="threeDshape", dBlevel=25, 
                                    f=48000, wl=512, ovlp=70, flim=c(0, 10),tlim = c(0,0.4),
                                    log.scale=T, plot.exp=doplot, plot.type="points",
                                    wav.at=file.path(wav.at, "Aligned"), store.at=store.at,
                                    TPS.file=tpsname,eigcheckpoint=eigcheckpoint,shuffle=shuffle,rev=rev)
  } else {
    if(manualcombinetps==T){
      eig.sample = geomorph::readmulti.tps(list.files(path=file.path(store.at),pattern=paste(".wav.",tpsname,".tps",sep=""),full.names = T),specID = "ID",negNA=F)
      
    } else {
      eig.sample = geomorph::readland.tps(file.path(store.at,paste(tpsname,".tps",sep="")),specID = "ID",negNA=F)
      
    }
    if(dim(eig.sample)[3]!=length(list.files(wav.at))){
      eig.sample <- SoundShape::eigensound(analysis.type="threeDshape", dBlevel=25, 
                                           f=48000, wl=512, ovlp=70, flim=c(0, 10),tlim = c(0,0.4),
                                           log.scale=T, plot.exp=doplot, plot.type="points",
                                           wav.at=file.path(wav.at, "Aligned"), store.at=store.at,
                                           TPS.file=tpsname,eigcheckpoint=eigcheckpoint)
    }
  }

  
  # Verify names of acoustic units from sample 
  sample.gr=dimnames(eig.sample)[[3]]
  #sample.gr = rownames(pca.eig.sample$x)
  
  sample.gr = sapply(sample.gr,FUN=function(y){
    z=strsplit(as.character(y),"-")[[1]]
    if(length(z)>1){
      z=paste(z[1:2],collapse="-")
    }
    x=strsplit(as.character(z),"\\.")[[1]]
    if(clusterind==F){
      x = x[1]
    } else {
      x = paste(x[1:2],collapse=".")
    }
    return(x)
  })
  sample.gr = as.factor(sample.gr)
  samples_per_cluster=table(sample.gr)
  samples_per_cluster_scaled = samples_per_cluster
  samples_per_cluster_scaled[samples_per_cluster_scaled>pcascalemin] = pcascalemin
  sample.gr.scaled = unlist(sapply(1:length(samples_per_cluster_scaled),
                                   FUN=function(i){
                                     name=names(samples_per_cluster_scaled)[i]
                                     val=samples_per_cluster_scaled[i]
                                     newgroup = rep(name,val)
                                     return(newgroup)
                                   },simplify = T))
  
  if(verbose==T){print("calculating pca");print(Sys.time())}
  # PCA using three-dimensional semilandmark coordinates embeeded in eig.sample
  
  ## CAN WE CALCULATE A WEIGHTED PCA???
  ## subsample the data and then project the data 
  
  eig.2D = geomorph::two.d.array(eig.sample)
  
  if(pcascale==F){
    pca.eig.sample <- stats::prcomp(eig.2D)
    write.table(pca.eig.sample$x,file.path(outpath,paste("pca_soundshape_SCALE",pcascale,".temp",sep="")))
    write.table(pca.eig.sample$rotation,file.path(outpath,paste("pca_soundshape_rotation_SCALE",pcascale,".temp",sep="")))
    write.table(unclass(summary(pca.eig.sample))$importance,file.path(outpath,paste("pca_soundshape_importance_SCALE",pcascale,".temp",sep="")))
    
    pca.eig.data = pca.eig.sample$x
    
  } else {
    print("SCALING")
    eid.2D.sample = NULL
    
    for(group in names(samples_per_cluster)) {
      subset = eig.2D[sample.gr==group,]
      toadd = subset[sample(1:nrow(subset),size = min(pcascalemin,nrow(subset)),replace = F),]
      if(is.null(eid.2D.sample)){
        eid.2D.sample=toadd
      } else {
        eid.2D.sample = rbind(eid.2D.sample,toadd)
      }
    }
    pca.eig.subset <- stats::prcomp(eid.2D.sample)
    pca.eig.sample = stats::predict(pca.eig.subset,newdata=eig.2D)
    
    write.table(pca.eig.subset$x,file.path(outpath,paste("pca_soundshape_SUBSET.temp",sep="")))
    write.table(pca.eig.sample,file.path(outpath,paste("pca_soundshape_SCALE",pcascale,".temp",sep="")))
    write.table(pca.eig.subset$rotation,file.path(outpath,paste("pca_soundshape_rotation_SUBSET.temp",sep="")))
    write.table(unclass(summary(pca.eig.subset))$importance,file.path(outpath,paste("pca_soundshape_importance_SUBSET.temp",sep="")))
    
    pca.eig.data = pca.eig.sample
  }
  
   
  
  if(doplot==T) {
    # Create hypothetical sound surfaces using hypo.surf
    
    # Mean shape configuration (consensus)
    hypo.surf(eig.sample,  PC="mean", flim=c(0, 10), tlim=c(0, 0.4), x.length=70, y.length=47,
              cex.lab=0.7, cex.axis=0.5, cex.main=1)
    
    # Minimum and maximum deformations - Principal Component 1
    hypo.surf(eig.sample, PC=1, flim=c(0, 10), tlim=c(0, 0.4), x.length=70, y.length=47,
              cex.lab=0.7, cex.axis=0.5, cex.main=1)
  }
  
  # Based on those names, create factor to use as groups in subsequent ordination plot
  listnotes[is.na(listnotes)]=0
  
  if(verbose==T){print("plotting pca")}
  # Ordination plot
  
  ## NEEDS TO BE FIXED FOR NEW DATATYPE
  if(pcascale==F){
  png(file.path(outpath,"pca_soundshape_pca.png"))
  SoundShape::pca.plot(pca.eig.sample, groups=sample.gr, conv.hulls=sample.gr,leg=F)
  dev.off()
  } else {
    png(file.path(outpath,"pca_soundshape_pca_SCALE.png"))
    plot(pca.eig.subset$x[,1],pca.eig.subset$x[,2],cex=0.5,pch=4,col=as.numeric(as.factor(sample.gr.scaled)))
    points(pca.eig.data[,1],pca.eig.data[,2],col=as.numeric(as.factor(sample.gr)))
    dev.off()
  }
  
  print(Sys.time())
} #end function

run_soundshape(path=path,pattern=pattern,doplot=F,verbose=T,listfiles=listfiles,outpath="~/Documents/",
               overwrite=F,checkpointNumber=1,redo_eig=T,alignCheckpoint=1,eigcheckpoint=1,shuffle=F,rev=T)



