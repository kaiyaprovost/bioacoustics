library(SoundShape)
verbose=T
doplot=F
clusterN=2
date=format(Sys.time(), "%d%b%Y")

## SOMETHING IN HERE IS CHANGING THE ACTUAL SOUNDS
## it is at the alignment step that the actual sounds change

## get the wavfiles 
path="/Users/kprovost/Documents/Research/Ruscitelli_Cowbird/Selections & WAV files/"
wav.at = "/Users/kprovost/Documents/Research/Ruscitelli_Cowbird/Selections & WAV files/SoundShape/"
setwd(path)
wavfiles = list.files(path,".wav$",full.names = T,recursive=F)
#wavfiles = wavfiles[grepl("wav_",wavfiles)]

selectionfiles = list.files(path,"selections.txt$",full.names = T)
#selectionfiles = selectionfiles[grepl("wav_",selectionfiles)]

maxdiflist = 0

## custom functions
align.wave.custom <- function(wav.at=NULL, wav.to="Aligned", time.length=1, time.perc=0.0, dBlevel=25, f=48000, wl=512, ovlp=70,
                              overwrite=F,verbose=T,alignCheckpoint=1,zipme=T)  {
  
  if(is.null(wav.at)) {stop("Use 'wav.at' to specify folder path where '.wav' files are stored")}
  
  # Create folder to store aligned calls
  if(!dir.exists(file.path(wav.at, wav.to))) dir.create(file.path(wav.at, wav.to))
  
  # Replace sounds for each ".wav" file in a folder
  filestoalign = list.files(wav.at, pattern = ".wav$",full.names = T,recursive=F)
  numalignfiles=length(filestoalign)
  for(j in alignCheckpoint:numalignfiles){
    file = filestoalign[j]
    if(verbose==T) {print(paste(j,file,numalignfiles))}
    if(overwrite==T || !(file.exists(file.path(wav.at, wav.to, basename(file))))){
      
      orig.wav0 <- tuneR::readWave(file)
      #orig.wav0 <- tuneR::readWave(paste(wav.at,"/", file, sep=""))
      
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
      
      tuneR::writeWave(final.wav, file.path(wav.at, wav.to, basename(file)), extensible = F)
      #R.utils::gzip()
    } else {
      if(verbose==T) {print("SKIPPING")}
    }
    if(zipme==T){
      if(verbose==T){print("ZIPPING")}
      try({R.utils::gzip(file)})
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
    dim(geomorph::readland.tps(tps.list[x], ...))
  }, simplify = T)
  p1 <- dt.dims[1, 1]; k1 <- dt.dims[2, 1]; n1 <- dt.dims[3, 1]
  
  if(any(dt.dims[1,]!=p1)) stop("Input tps files include different numbers of landmarks, please correct")
  
  if(any(dt.dims[2,]!=k1)) stop("Input tps files include landmarks in different dimensions (2D and 3D), please correct")
  
  all.lms <- NULL
  for(f in 1:length(tps.list)){   
    print(paste("SECOND:",f,length(tps.list)))
    lms <- geomorph::two.d.array(geomorph::readland.tps(tps.list[f], ...))
    all.lms <- rbind(all.lms, lms)
  }
  all.lms <- geomorph::arrayspecs(all.lms, p1, k1)
  
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
  print(paste(j,length(wavfiles)))
  wavfile = wavfiles[j]
  prefix=substr(wavfile,1,nchar(wavfile)-4)
  prefix=basename(prefix)
  selection = selectionfiles[grepl(paste(prefix,"\\.",sep=""),selectionfiles)]
  
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
      writefile=file.path(wav.at, paste(prefix,i,"wav",sep="."))
      if(wavdf$End.Time..s.[i]-wavdf$Begin.Time..s.[i]<=0) {
        if(verbose==T){print(paste("SKIPPING ROW WITH NEGATIVE SIZE",i))}
      } else {
        if(file.exists(writefile)) {
          print("DONE")
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
            
            try({tuneR::writeWave(cutwav_f, filename = writefile, extensible = FALSE)})
          }
          
        }
      }
      
    }
    
  } else {
    print("ERROR")
  }
  try({R.utils::gzip(wavfile)})
  try({R.utils::gzip(selection)})
}

## align the files
align.wave.custom(wav.at=wav.at, wav.to="Aligned", time.length = maxdiflist,time.perc=0,alignCheckpoint = 1)
## align.checkpoint is where it starts

#dBlevel=25, f=48000, wl=512, ovlp=70,
#overwrite=F,verbose=T,alignCheckpoint=1,zip=T

## calculate the eigenvectors -- lengthy
store_dir=file.path(wav.at, "Stored")
dir.create(store_dir)
tps_file=paste(wav.at,"/fulltps.",date,".tps",sep="")
eig.sample <- eigensound.custom(analysis.type="threeDshape",
                                wav.at=file.path(wav.at, "Aligned"),
                                store.at=store_dir,
                                TPS.file = "tpsfile",
                                eigcheckpoint = 1,
                                log.scale = T,
                                doIndTPS = T,
                                shuffle=F
)
#geomorph::writeland.tps(eig.sample, file=tps_file, scale = NULL, specID = TRUE)
#eig.sample=geomorph::readland.tps(tps_file)


## pca iterations

iter_start=1 ## set to 1
num_to_keep_per_species = 2279
num_iterations=1
pcascalemin=2279
pcascale=T
verbose=T
pca_to_keep = 5
dynamic_pca_cutoff=0.5
tps_path=file.path(wav.at, "TPS_PCA")
dir.create(tps_path)
store_dir=file.path(wav.at, "Stored")
dir.create(store_dir)

tps.list = list.files(path=store_dir,
                      pattern=".tps",
                      full.names = F)
tps.list=tps.list[tps.list!="tpsfile.tps"]

#tps.species=sapply(tps.list,FUN=function(y){
#  x=strsplit(as.character(y),"\\.")[[1]]
#  x =  paste(x[1:clusterN],collapse=".")
#  return(x)
#})
tps.species = "Molothrus_ater"

tps.df = cbind(tps.list,tps.species)
tps.df=as.data.frame(tps.df)

soundshape_pca_only=function(eig.sample,outpath,samples_per_cluster,sample.gr,date,pcascalemin,
                             verbose=T,pcascale=T,outfilename="pca_soundshape") {
  if(verbose==T){print("calculating pca");print(Sys.time())}
  # PCA using three-dimensional semilandmark coordinates embeeded in eig.sample
  
  ## CAN WE CALCULATE A WEIGHTED PCA???
  ## subsample the data and then project the data 
  
  eig.2D = geomorph::two.d.array(eig.sample)
  
  if(pcascale==F){
    pca.eig.sample <- stats::prcomp(eig.2D)
    write.table(pca.eig.sample$x,file.path(outpath,paste(outfilename,"_SCALE",pcascale,".",date,".temp",sep="")))
    write.table(pca.eig.sample$rotation,file.path(outpath,paste(outfilename,"_rotation_SCALE",pcascale,".",date,".temp",sep="")))
    write.table(unclass(summary(pca.eig.sample))$importance,file.path(outpath,paste(outfilename,"_importance_SCALE",pcascale,".",date,".temp",sep="")))
    
    pca.eig.data = pca.eig.sample$x
    
  } else {
    print("SCALING")
    eid.2D.sample = NULL
    
    for(group in names(samples_per_cluster)) {
      print(group)
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
    
    write.table(pca.eig.subset$x,file.path(outpath,paste(outfilename,"_SUBSET",".",date,".temp",sep="")))
    write.table(pca.eig.sample,file.path(outpath,paste(outfilename,"_SCALE",pcascale,".",date,".temp",sep="")))
    write.table(pca.eig.subset$rotation,file.path(outpath,paste(outfilename,"_rotation_SUBSET",".",date,".temp",sep="")))
    write.table(unclass(summary(pca.eig.subset))$importance,file.path(outpath,paste(outfilename,"_importance_SUBSET",".",date,".temp",sep="")))
    
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
  #listnotes[is.na(listnotes)]=0
  
  if(verbose==T){print("plotting pca")}
  # Ordination plot
  
  ## NEEDS TO BE FIXED FOR NEW DATATYPE
  if(pcascale==F){
    png(paste(outpath,"/",outfilename,"_pca",".",date,".png",sep=""))
    SoundShape::pca.plot(pca.eig.sample, groups=sample.gr, conv.hulls=sample.gr,leg=F)
    dev.off()
  } else {
    png(paste(outpath,"/",outfilename,"_pca_SCALE",".",date,".png",sep=""))
    plot(pca.eig.subset$x[,1],pca.eig.subset$x[,2],cex=0.5,pch=4,col=as.numeric(as.factor(sample.gr.scaled)))
    points(pca.eig.data[,1],pca.eig.data[,2],col=as.numeric(as.factor(sample.gr)))
    dev.off()
  }
  
  print(Sys.time())
}

for(this_iteration in sort(iter_start:num_iterations)){
  print(paste(this_iteration,num_iterations))
  
  outfilename=paste("pca_soundshape",".",this_iteration,sep="")
  outtps=paste(tps_path,"/","tpspca.",date,".",this_iteration,".tps",sep="")
  pngfile=(paste(tps_path,"/",outfilename,"_pca_SCALE",".",date,".png",sep=""))
  
  if(!file.exists(pngfile)){
    
    if(!file.exists(outtps)){
      print(paste("creating iteration file",this_iteration))
      kept=c()
      for(spp in sort(unique(tps.df$tps.species))){
        print(spp)
        
        temp = tps.df[tps.df$tps.species==spp,]
        if(nrow(temp)<num_to_keep_per_species){
          tokeep=sample(1:nrow(temp),num_to_keep_per_species,replace=T)
        } else {
          tokeep=sample(1:nrow(temp),num_to_keep_per_species,replace=F)
        }
        kept  = c(kept,temp$tps.list[tokeep])
      }
      
      # tps_path
      print(paste("reading in kept",this_iteration))
      eig.sample = readmulti.tps.custom(file.path(store_dir,kept),specID = "ID",negNA=F,warnmsg=F)
      print(paste("writing out kept as iteration",this_iteration,"/",num_iterations))
      geomorph::writeland.tps(eig.sample, file=outtps, scale = NULL, specID = TRUE)
    } else {
      print(paste("reading existing iteration file",this_iteration))
      eig.sample = geomorph::readland.tps(outtps,specID = "ID",negNA=F,warnmsg=F)
    }
    
    sample.gr=rep(sort(unique(tps.df$tps.species)),num_to_keep_per_species)
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
    
    
    temp=soundshape_pca_only(eig.sample=eig.sample,outpath=tps_path,samples_per_cluster=samples_per_cluster,
                             sample.gr=sample.gr,date=date,pcascalemin=pcascalemin,
                             verbose=verbose,pcascale=pcascale,outfilename=outfilename)
  } else {
    print(paste("skipping iteration",this_iteration,"file exists"))
  }
  
}

## check pca cutoffs
importance_files = list.files(path=tps_path,pattern="importance",full.names = T)
pca_importances = sapply(importance_files,FUN=function(f){
  df = read.table(f)
  ## cumulative is 3rd row
  to_keep = min(which(df[3,]>=dynamic_pca_cutoff),na.rm=T)
  to_keep = max(c(pca_to_keep,to_keep),na.rm=T)
  return(to_keep)
})
pca_importances=as.numeric(pca_importances)


## upload the importance and add broken stick
broken_stick = function(P) {
  sequence = 1:P
  divided = 1/sequence
  seq_sums = sapply(sequence,FUN=function(x){
    subset = divided[x:P]
    subset_sum = sum(subset)
    i = subset_sum/P
    return(i)
  })
  return(seq_sums)
}

my_shp_pca_imp = read.table("/Users/kprovost/Documents/Research/Ruscitelli_Cowbird/Selections & WAV files/SoundShape/TPS_PCA/old/pca_soundshape.1_importance_SUBSET.21Aug2024.temp",
                            header=T,sep=" ")

broken = broken_stick(2279)



iter_files = list.files(path=tps_path,pattern="SCALETRUE")
outtable=paste(tps_path,"test.vols.txt",sep="/")
for(f in iter_files){
  pcaxF = as.data.frame(data.table::fread(file.path(tps_path,f),sep=" "))
  rownames(pcaxF) = pcaxF$V1
  pcaxF=pcaxF[,-1]
  for(i in 6:7){
    hull3=geometry::convhulln(pcaxF[,(1:i)],output.options="FA")
    vol=hull3$vol
    centroid_sd=as.data.frame(rbind(matrixStats::colSds(as.matrix(pcaxF[,(1:i)]),na.rm=T)))
    pseudovol = prod(centroid_sd)
    x=data.frame(f=f,i=i,vol=vol,pseudovol=pseudovol)
    write.table(x,outtable,append=T,row.names = F,quote = F)
  }
}

## colume of a spheroid
spheroid_vol = function(A,B,C) {
  volume=(4/3) * (pi) * (A * B * C)
  return(volume)
}

## can do product off of the centroid instead 

do_hull = F
species_iteration_volumes=lapply(1:length(iter_files),FUN=function(i){ ## 
  print(i)
  f=iter_files[i]
  imp=pca_importances[i]
  pcaxF = as.data.frame(data.table::fread(file.path(tps_path,f),sep=" "))
  rownames(pcaxF) = pcaxF$V1
  pcaxF=pcaxF[,-1]
  #rownames(pcaxF)
  rownames(pcaxF)=gsub("-",".",rownames(pcaxF))
  rownames(pcaxF)=gsub("_",".",rownames(pcaxF))
  rownames(pcaxF)=gsub(".mp3","",rownames(pcaxF))
  #rownames(pcaxF)=gsub(".gambellii","",rownames(pcaxF))
  #rownames(pcaxF)=gsub(".oriantha","",rownames(pcaxF))
  ## need to split things by resample.48000
  
  
  pcaxF_list=lapply(rownames(pcaxF),FUN=function(x){
    splitX=strsplit(x,"\\.resample\\.48000\\.")[[1]][1]
    splitY=strsplit(x,"\\.resample\\.48000\\.")[[1]][2]
    splits=strsplit(splitX,"\\.")[[1]]
    genus=splits[1]
    species=splits[2]
    if(length(splits)==4){
      if(splits[3] == "XC" | splits[3] == "BLB") {
        subspecies="unknown"
        id = paste(splits[3],splits[4],sep=".")
      } else {
        subspecies = splits[3]
        id=paste("XC",splits[4],sep=".")
      }
    } else {
      subspecies = splits[3]
      id = paste(splits[4],splits[5],sep=".")
    }
    
    splits2 = strsplit(splitY,"\\.")[[1]]
    id = paste(id,splits2[1],splits2[2],sep=".")
    syllable=splits2[3]
    toreturn=cbind(genus,species,subspecies,id,syllable)
    return(toreturn)
  })
  pcaxF_df=do.call(rbind,pcaxF_list)
  pcaxF=cbind(pcaxF,pcaxF_df)
  pcaxF_small = cbind(pcaxF[,(1:imp)],pcaxF[,c("genus","species","subspecies","id","syllable")])
  
  if(do_hull==T) {
    hull3=geometry::convhulln(pcaxF_small[,(1:imp)],output.options="FA")
    fullvol=hull3$vol
    voldf=cbind("ALL","ALL",fullvol,nrow(pcaxF_small))
    for(spp in sort(unique(pcaxF_small$species))){
      print(spp)
      temp = pcaxF_small[pcaxF_small$species==spp,]
      hulltemp=geometry::convhulln(temp[,(1:imp)],output.options="FA")
      tempvol=hulltemp$vol
      toadd2=cbind(spp,"ALL",tempvol,nrow(temp))
      voldf = rbind(voldf,toadd2)
      
    }
    colnames(voldf) = c("species","id","vol","nrow")
    voldf = as.data.frame(voldf)
    
    voldf$relative = as.numeric(voldf$vol)  / max(as.numeric(voldf$vol),na.rm=T)
    outputvol = voldf[voldf$species!="ALL",c("species","relative")]
    relative = outputvol$relative; names(relative) = outputvol$species
  } else {
    ## calculate a centroid point? 
    centroid = as.data.frame(rbind(colMeans(pcaxF_small[,(1:imp)],na.rm=T)))
    centroid_sd=as.data.frame(rbind(matrixStats::colSds(as.matrix(pcaxF_small[,(1:imp)]),na.rm=T)))
    colnames(centroid_sd) = paste(colnames(centroid),"SD",sep=".")
    centroid=cbind(centroid,centroid_sd)
    centroid$species = "ALL"
    for(spp in sort(unique(pcaxF_small$species))){
      print(spp)
      temp = pcaxF_small[pcaxF_small$species==spp,]
      temp_mean = as.data.frame(rbind(colMeans(temp[,(1:imp)],na.rm=T)))
      temp_sd=as.data.frame(rbind(matrixStats::colSds(as.matrix(temp[,(1:imp)]),na.rm=T)))
      temp_cent=cbind(temp_mean,temp_sd)
      temp_cent$species = spp
      colnames(temp_cent) = colnames(centroid)
      centroid = rbind(centroid,temp_cent)
    }
    
    row_volumes = matrixStats::rowProds(as.matrix(centroid[,(imp+1):(2*imp)]))
    
    voldf=cbind(centroid,row_volumes)
    voldf$relative = voldf$row_volumes/max(voldf$row_volumes,na.rm=T)
    voldf = as.data.frame(voldf)
    
    outputvol = voldf[voldf$species!="ALL",c("species","relative")]
    relative = outputvol$relative; names(relative) = outputvol$species
  }
  
  return(relative)
  
})
spp_iter_vol_df  = do.call(rbind,species_iteration_volumes)
rownames(spp_iter_vol_df) = iter_files
spp_iter_vol_df=as.data.frame(spp_iter_vol_df)
write.table(spp_iter_vol_df,paste(tps_path,"/iterations_of_vol_standardized_",date,"_iters",num_iterations,"_persppnum",num_to_keep_per_species,"_pcs",pca_to_keep,"_doHull",do_hull,".txt",
                                  sep=""))


## get some stats in there
boxplot(spp_iter_vol_df,las=2)
colMeans(spp_iter_vol_df,na.rm=T)
matrixStats::colSds(as.matrix(spp_iter_vol_df),na.rm=T)

## lengthy: pca
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


