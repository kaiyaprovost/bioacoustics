library(warbleR)

path="/Users/kprovost/Dropbox (AMNH)/Postdoc_Backup/Cardinalis_cardinalis/Full Annotated Files/"
setwd(path)
outfile=paste("rvn.dat.ravenversion_",date,".txt",sep="")
rvn.dat=read.table(outfile,header=T)

X=rvn.dat[1,c("selec","start","end","bottom.freq","top.freq","sound.files")]
type="fundamental"
wl=512
length.out = 28
wn="hanning"
ovlp=50
bp=c(0.5,10)
threshold=50
img=F
parallel=1
img.suffix="frequency.ts"
pb=F
clip.edges=T
leglab="frequency.ts"
track.harm = FALSE
raw.contour = FALSE
adjust.wl = TRUE
ff.method = "tuneR"
entropy.range = c(2, 10)

freq_ts_custom <- function(X, type = "dominant", wl = 512, length.out = 20, wn = "hanning", ovlp = 70, bp = c(0, 22), threshold = 15, img = TRUE, 
                           parallel = 1, path = NULL, img.suffix = "frequency.ts", pb = TRUE, clip.edges = FALSE, leglab = "frequency.ts",
                           track.harm = FALSE, raw.contour = FALSE, adjust.wl = TRUE, ff.method = "seewave", entropy.range = c(2, 10),...){     
  
  # set pb options 
  on.exit(pbapply::pboptions(type = .Options$pboptions$type), add = TRUE)
  
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(freq_ts)
  
  # get warbleR options
  opt.argms <- if(!is.null(getOption("warbleR"))) getOption("warbleR") else SILLYNAME <- 0
  
  # rename path for sound files
  names(opt.argms)[names(opt.argms) == "wav.path"] <- "path"
  
  # remove options not as default in call and not in function arguments
  opt.argms <- opt.argms[!sapply(opt.argms, is.null) & names(opt.argms) %in% argms]
  
  # get arguments set in the call
  call.argms <- as.list(base::match.call())[-1]
  
  # remove arguments in options that are in call
  opt.argms <- opt.argms[!names(opt.argms) %in% names(call.argms)]
  
  # set options left
  if (length(opt.argms) > 0) {
    for (q in 1:length(opt.argms)) {
      assign(names(opt.argms)[q], opt.argms[[q]])
    }
  }
  
  #check path to working directory
  if (is.null(path)) {path <- getwd()} else {
    if (!dir.exists(path)) {stop("'path' provided does not exist")} else {
      path <- normalizePath(path)
    }
  }
  
  #if X is not a data frame
  if (!any(is.data.frame(X), is_selection_table(X), is_extended_selection_table(X))) {
    stop("X is not of a class 'data.frame', 'selection_table' or 'extended_selection_table'")
  }
  
  if (!all(c("sound.files", "selec", 
             "start", "end") %in% colnames(X))) {
    stop(paste(paste(c("sound.files", "selec", "start", "end")[!(c("sound.files", "selec", 
                                                                   "start", "end") %in% colnames(X))], collapse=", "), "column(s) not found in data frame"))
  }
  #if there are NAs in start or end stop
  if (any(is.na(c(X$end, X$start)))) {stop("NAs found in start and/or end")  }
  
  #if end or start are not numeric stop
  if (any(!is(X$end, "numeric"), !is(X$start, "numeric"))) {stop("'start' and 'end' must be numeric")}
  
  #if any start higher than end stop
  if (any(X$end - X$start <= 0)) {stop(paste("Start is higher than or equal to end in", length(which(X$end - X$start <= 0)), "case(s)")) } 
  
  #if any selections longer than 20 secs stop
  if (any(X$end - X$start>20)) {stop(paste(length(which(X$end - X$start>20)), "selection(s) longer than 20 sec"))  }
  options( show.error.messages = TRUE)
  
  #if bp is not vector or length!=2 stop
  if (!is.null(bp)) {
    if (!is.vector(bp)) {
      stop("'bp' must be a numeric vector of length 2")} 
    else{
      if (!length(bp) == 2) 
      {stop("'bp' must be a numeric vector of length 2")}
    }
  }
  
  
  # if type argument  
  if (!any(type == "dominant", type == "fundamental", type == "entropy")) {stop(paste("type", type, "is not recognized"))}    
  
  # if ff.method argument  
  if (!any(ff.method == "seewave", ff.method == "tuneR")) {stop(paste("ff.method", ff.method, "is not recognized"))}  
  
  #return warning if not all sound files were found
  if (!is_extended_selection_table(X))
  {
    recs.wd <- list.files(path = path, pattern = "\\.wav$", ignore.case = TRUE)
    if (length(unique(X$sound.files[(X$sound.files %in% recs.wd)])) != length(unique(X$sound.files)) & pb){ 
      cat(paste(length(unique(X$sound.files))-length(unique(X$sound.files[(X$sound.files %in% recs.wd)])), 
                ".wav file(s) not found"))
    }
    #count number of sound files in working directory and if 0 stop
    d <- which(X$sound.files %in% recs.wd) 
    if (length(d) == 0){
      stop("The .wav files are not in the working directory")
    }  else {X <- X[d, , drop = FALSE]}
  }
  
  #if parallel is not numeric
  if (!is.numeric(parallel)) {stop("'parallel' must be a numeric vector of length 1")} 
  if (any(!(parallel %% 1 == 0),parallel < 1)) {stop("'parallel' should be a positive integer")}
  
  if (pb) {if (img) {cat("Creating spectrograms overlaid with fundamental frequency measurements:")} else
  {cat("Measuring fundamental frequency:")}}
  
  # if (type == "dominant") {
  #   contour_FUN <- function(X, i, bp, wl, threshold, entropy.range, raw.contour, track.harm, adjust.wl){
  #     
  #     # Read sound files to get sample rate and length
  #     r <- warbleR::read_wave(X = X, path = path, index = i, header = TRUE)
  #     f <- r$sample.rate
  #     
  #     # if bp is frange
  #     if (bp[1] == "frange") bp <- c(X$bottom.freq[i], X$top.freq[i])
  #     
  #     #in case bp its higher than can be due to sampling rate
  #     b <- bp 
  #     if (!is.null(b)) {
  #       if (b[2] > ceiling(f/2000) - 1) b[2] <- ceiling(f/2000) - 1 {
  #         b <- b * 1000
  #       }
  #     }
  #     
  #     r <- warbleR::read_wave(X = X, path = path, index = i)
  #     
  #     # calculate dominant frequency at each time point     
  #     dfrq1 <- track_harmonic(wave = r, f = f, wl = wl, plot = FALSE, ovlp = ovlp, bandpass = b, fftw = TRUE,
  #                             threshold = threshold, dfrq = !track.harm, adjust.wl = adjust.wl)
  #     
  #     dfrq <- dfrq1[!is.na(dfrq1[,2]), , drop = FALSE]
  #     
  #     #make NA's the ones outside banpass freqs
  #     dfrq[dfrq[,2] < b[1]/1000, ] <- NA
  #     
  #     if (any(is.na(dfrq[1, ]))) {
  #       dfrq <- dfrq[!is.na(dfrq[ , 1]), , drop = FALSE]
  #     }
  #     
  #     # make a matrix containing results and name/order columns
  #     dfrq <- data.frame(dfrq, X$start[i] + dfrq[, 1])
  #     if (!is.data.frame(dfrq)) dfrq <- as.data.frame(t(dfrq))
  #     colnames(dfrq) <- c("relative.time", "frequency", "absolute.time")
  #     if (!is.data.frame(dfrq)) dfrq <- as.data.frame(t(dfrq))
  #     dfrq <- dfrq[, c(3, 1, 2), drop = FALSE]
  #     
  #     # remove NAs on edges only if more than 
  #     if (clip.edges & nrow(dfrq) > 2)
  #     {
  #       dfrq <- dfrq[which(as.numeric(is.na(dfrq$frequency)) == 0)[1]:nrow(dfrq), , drop = FALSE]
  #       
  #       # clip end edges
  #       dfrq <- dfrq[1:max(which(as.numeric(is.na(dfrq$frequency)) == 0)), , drop = FALSE]
  #     }
  #     
  #     # interpolate if no raw.contour
  #     if (!raw.contour)
  #     {
  #       #if more than one detection extrapolate else repeat value
  #       if (nrow(dfrq) > 1 | all(is.na(dfrq[, 3])))
  #       {
  #         apdom <- try(approx(x = dfrq$relative.time[!is.na(dfrq$frequency)], y =  dfrq$frequency[!is.na(dfrq$frequency)], 
  #                             xout = seq(from = min(dfrq$relative.time, na.rm = TRUE),  to = max(dfrq$relative.time, na.rm = TRUE), 
  #                                        length.out = length.out), method = "linear"), silent = TRUE)
  #         
  #         if (is(apdom, "try-error")) apdom <- list(x =  seq(from = 0,  to = X$end[i] - X$start[i], 
  #                                                            length.out = length.out), y = rep(NA, length.out))
  #         
  #       } else
  #         # repeat same value length.out times if only 1 detection
  #       { 
  #         apdom <- dfrq[rep(1, length.out), , drop = FALSE]
  #         apdom[, 1] <- seq(from = X$start[i],  to = X$end[i],
  #                           length.out = length.out)
  #         
  #         apdom[, 2] <- apdom[, 1] - X$start[i] 
  #         colnames(apdom)[3] <- "y"
  #       }                          
  #       
  #       if (clip.edges & !raw.contour) 
  #       {
  #         
  #         #fix for ploting with trackfreqs
  #         dfrq1[,2][is.na(dfrq1[,2])] <- 0
  #         
  #         #calculate time at start and end with no amplitude detected (duration of clipped edges)
  #         durend1 <- suppressWarnings(diff(range(dfrq1[,1][rev(cumsum(rev(dfrq1[,2])) == 0)])))
  #         durend <- durend1
  #         if (is.infinite(durend) | is.na(durend)) durend <- 0
  #         
  #         durst1 <- suppressWarnings(diff(range(dfrq1[,1][cumsum(dfrq1[,2]) == 0])))   
  #         durst <- durst1
  #         if (is.infinite(durst) | is.na(durst)) durst <- 0
  #         
  #         by.dur <- mean(diff(apdom$x))
  #         clipst <- length(seq(from = 0, to = durst, by = by.dur))
  #         clipend <- length(seq(from = 0, to = durend, by = by.dur))
  #         
  #         apdom1 <- apdom
  #         apdom1 <- list(x = apdom$x, y = apdom$y)
  #         apdom1$y <- c(rep(NA, clipst) ,apdom, rep(NA, clipend))
  #         
  #         if (is.infinite(durst1) | is.na(durst1)) apdom1$y <- apdom1$y[-1]
  #         if (is.infinite(durend1) | is.na(durend1)) apdom1$y <- apdom1$y[-length(apdom1$y)]
  #         
  #         cstm.cntr <- data.frame(sound.files = X$sound.files[i], selec = X$selec[i], t(apdom1$y))
  #       }
  #       
  #       if (!raw.contour)
  #         cstm.cntr <- data.frame(sound.files = X$sound.files[i], selec = X$selec[i], t(apdom$y)) else
  #           cstm.cntr <- dfrq
  #     }        
  #     
  #     if (img)  
  #     {
  #       warbleR::track_freq_contour(X = X[i, , drop = FALSE], wl = wl, osci = FALSE, leglab = leglab, pb = FALSE, wn = wn, bp = bp, 
  #                                   parallel = 1, path = path, img.suffix = img.suffix, ovlp = ovlp,
  #                                   custom.contour = cstm.cntr, frange.detec = FALSE, ...)
  #     } 
  #     if (!raw.contour) return(t(apdom$y))  else return(dfrq)  
  #   } 
  # }
  # 
  if (type == "fundamental"){
    contour_FUN <- function(X, i, bp, wl, threshold, entropy.range, raw.contour, track.harm, adjust.wl){
      
      # Read sound files to get sample rate and length
      r <- warbleR::read_wave(X = X, path = path, index = i, header = TRUE)
      f <- r$sample.rate
      
      b<- bp 
      if (!is.null(b)) {if (b[2] > ceiling(f/2000) - 1) b[2] <- ceiling(f/2000) - 1 
      b <- b * 1000}
      
      r <- warbleR::read_wave(X = X, path = path, index = i)
      
      # calculate fundamental frequency at each time point     
      if (ff.method == "seewave") {
        ffreq1 <- seewave::fund(r, fmax= b[2], f = f, ovlp = ovlp, threshold = threshold, plot = FALSE)
      } else
      {
        if (any(slotNames(r) == "stereo")) if (r@stereo) r <- mono(r, which = "both")
        suppressWarnings(ff1 <- tuneR::FF(tuneR::periodogram(r, width = wl, overlap = wl*ovlp / 100), peakheight = (100 - threshold) / 100)/1000)
        ff2 <- seq(0, X$end[i] - X$start[i], length.out = length(ff1))
        
        ffreq1 <- cbind(ff2, ff1)
      }
      
      ffreq <- matrix(ffreq1[!is.na(ffreq1[,2]),], ncol = 2)  
      ffreq <- matrix(ffreq[ffreq[,2] > b[1]/1000,], ncol = 2)
      
      if (nrow(ffreq) < 2) {
        apfund <- list()
        apfund$x <- ffreq1[, 1]
        apfund$y <- rep(NA, length.out)
        apfund1 <- apfund
      } else {
        if (!clip.edges)  {
          
          # clip start edges
          ffreq <- ffreq[which(as.numeric(is.na(ffreq[ , 2])) == 0)[1]:nrow(ffreq), ]
          
          # clip end edges
          ffreq <- ffreq[1:max(which(as.numeric(is.na(ffreq[ , 2])) == 0)), ]
          
          # interpolate
          apfund <- approx(ffreq[,1], ffreq[,2], xout = seq(from = ffreq1[1, 1],
                                                            to = ffreq1[nrow(ffreq1), 1], length.out = length.out), 
                           method = "linear") 
          apfund1 <- apfund
        } else {
          if (!raw.contour)
            apfund <- approx(ffreq[,1], ffreq[,2], 
                             xout = seq(from = ffreq[1, 1],  to = ffreq[nrow(ffreq), 1], 
                                        length.out = length.out), method = "linear") else
                                          
                                          apfund <- ffreq
                                        #fix for ploting with trackfreqs
                                        #calculate time at start and end with no amplitude detected (duration of clipped edges)
                                        durend1 <- suppressWarnings(diff(range(ffreq1[,1][rev(cumsum(rev(ffreq1[,2])) == 0)])))
                                        durend <- durend1
                                        if (is.infinite(durend) | is.na(durend)) durend <- 0
                                        
                                        durst1 <- suppressWarnings(diff(range(ffreq1[,1][cumsum(ffreq1[,2]) == 0])))   
                                        durst <- durst1
                                        if (is.infinite(durst) | is.na(durst)) durst <- 0
                                        
                                        by.dur <- mean(diff(apfund$x))
                                        clipst <- length(seq(from = 0, to = durst, by = by.dur))
                                        clipend <- length(seq(from = 0, to = durend, by = by.dur))
                                        
                                        apfund1 <- apfund
                                        apfund1$y <- c(rep(NA, clipst) ,apfund$y, rep(NA, clipend))
                                        
                                        if (is.infinite(durst1) | is.na(durst1)) apfund1$y <- apfund1$y[-1]
                                        if (is.infinite(durend1) | is.na(durend1)) apfund1$y <- apfund1$y[-length(apfund1$y)]
        }
      }
      
      if (img) 
        warbleR::track_freq_contour(X[i, , drop = FALSE], wl = wl, osci = FALSE, leglab = leglab, pb = FALSE, wn = wn,
                                    parallel = 1, path = path, img.suffix =  img.suffix, ovlp = ovlp,
                                    custom.contour = data.frame(sound.files = X$sound.files[i], selec = X$selec[i], t(apfund$y)), ...)
      
      return(apfund$y)  
    } 
  }
  
  if (type == "entropy"){
    contour_FUN <- function(X, i, bp, wl, threshold, entropy.range, raw.contour, track.harm, adjust.wl){
      
      # Read sound files to get sample rate and length
      r <- warbleR::read_wave(X = X, path = path, index = i, header = TRUE)
      f <- r$sample.rate
      
      # if bp is frange
      if (!is.null(bp))
        if (bp[1] == "frange") bp <- c(X$bottom.freq[i], X$top.freq[i])
      
      #in case bp its higher than can be due to sampling rate
      b<- bp 
      if (!is.null(b)) {if (b[2] > ceiling(f/2000) - 1) b[2] <- ceiling(f/2000) - 1 
      b <- b * 1000}
      
      r <- warbleR::read_wave(X = X, path = path, index = i)
      
      #filter if this was needed
      if (!is.null(bp)) r <- ffilter(wave = r, from = b[1], to = b[2]) 
      
      # measure espectral entropy
      sp.en <- csh(wave = r, f = f, wl = wl, ovlp = ovlp, wn = wn, 
                   threshold = threshold, plot = F)
      
      if (clip.edges) 
      {    #remove initial values with 0
        sp.en1 <- sp.en[cumsum(sp.en[,2]) != 0, ]
        
        #remove end values with 0
        sp.en1 <- sp.en1[rev(cumsum(rev(sp.en1[,2])) != 0),]
        
      } else sp.en1 <- sp.en
      
      apen <- approx(sp.en1[,1], sp.en1[,2], xout = seq(from = sp.en1[1, 1],
                                                        to = sp.en1[nrow(sp.en1), 1], length.out = length.out),
                     method = "linear")  
      
      #fix for ploting with trackfreqs
      if (clip.edges) 
      { apen1 <- approx(sp.en[,1], sp.en[,2], xout = seq(from = sp.en[1, 1],
                                                         to = sp.en[nrow(sp.en), 1], length.out = length.out),
                        method = "linear")
      
      #make 0s at start and end NAs so they are plot at the bottom by trackfreqs
      apen1$y[cumsum(apen1$y) == 0] <- NA
      apen1$y[rev(cumsum(rev(apen1$y))) == 0] <- NA
      }  else apen1 <- apen
      
      correc.apen <- entropy.range[1] + (entropy.range[2] - entropy.range[1]) * apen1$y 
      
      if (img) 
        warbleR::track_freq_contour(X[i, , drop = FALSE], wl = wl, osci = FALSE, leglab = leglab, pb = FALSE, wn = wn,
                                    parallel = 1, path = path, img.suffix =  img.suffix, ovlp = ovlp,
                                    custom.contour = data.frame(sound.files = X$sound.files[i], selec = X$selec[i], t(correc.apen)), ...)
      
      
      return(apen$y)  
    } 
  }
  
  
  # set pb options 
  pbapply::pboptions(type = ifelse(pb, "timer", "none"))
  
  # set clusters for windows OS
  if (Sys.info()[1] == "Windows" & parallel > 1) {
    cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel))
  } else {
    cl <- parallel
  }
  
  # run loop apply function
  lst <- pbapply::pblapply(X = 1:nrow(X), cl = cl, FUN = function(i) { 
    contour_FUN(X, i, bp, wl, threshold, entropy.range, raw.contour, track.harm, adjust.wl)
  })
  
  df <- data.frame(sound.files = X$sound.files, selec = X$selec, as.data.frame(matrix(unlist(lst),nrow = length(X$sound.files), byrow = TRUE)))
  colnames(df)[3:ncol(df)]<-paste("ffreq",1:(ncol(df)-2),sep = "-")
  df[ ,3:ncol(df)] <- round(df[ ,3:ncol(df)], 3)
  
  return(df)
  
}
