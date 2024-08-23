#rm(list = ls())

## really simple r stuff

## TODO: something is going wrong with the fcts code. its outputting 
## the same individuals over and over but with non-duplicate data

## extract simple metrics from known annotated syllables 
## using the pre-annotated ones already has this information
## for now just REAL simple doing length and bandwidth of the boxes
## and since boxes might be off this shouldn't be trusted
## start with C. cardinalis

#outpath = "/users/PYS1065/kprovost/bioacoustics/Sounds_and_Annotations/Aves/Piciformes/Ramphastidae/"
#path="/users/PYS1065/kprovost/bioacoustics/Sounds_and_Annotations/Aves/Accipitriformes/Accipitridae/"
#path=outpath
outpath = "/Users/kprovost/Documents/Research/Tyrannidae/predicted_annotations/"
path="/Users/kprovost/Documents/Research/Tyrannidae/predicted_annotations/"
spp_substitute = ""
parts_of_name_to_keep = 1:3
setwd(path)
#date=format(Sys.time(), "%d%b%Y")
date="15Aug2024"

runSumstat=T ## calcualte summary statistics
runBigpc=F ## calculate a principal components analysis
runCentroids=F ## calculate centroid locations among individuals
runSyllables=F ## calculate distances between syllanbes 
generatePlots=F ## make all the plots if files exist

outersect <- function(x, y) {
  sort(c(setdiff(x, y),
         setdiff(y, x)))
}

seconds_to_samples = function(seconds,samplerate){
  ## samples = 25176
  ## samples / samplerate = seconds
  ## seconds * samplerate = samples
  samples = seconds * samplerate
  return(samples)
}

samples_to_seconds = function(samples,samplerate){
  ## samples = 25176
  ## samples / samplerate = seconds
  ## seconds * samplerate = samples
  seconds = samples / samplerate
  return(seconds)
}

sample_rate_calculator = function(samples,seconds){
  ## samples / samplerate = seconds
  ## seconds * samplerate = samples
  samplerate = samples / seconds
  return(samplerate)
}

## ROBUST MEASUREMENTS:
##Center Freq (Hz)
##Freq 5% (Hz)
##Freq 95% (Hz)
##PFC Max Freq (Hz)
##PFC Min Freq (Hz)
##Time 5% (s)
##Time 95% (s)
## 

## can measure from warbleR? 
##getwd()
##dom.freq.ts <- freq_ts(X = lbh_selec_table, path = tempdir()) -- this is a note to kaiya and should be ignored

columns_to_keep = c("Selection","Begin.Time..s.","End.Time..s.",
                    "Low.Freq..Hz.","High.Freq..Hz.",
                    "Center.Freq..Hz.","Freq.5...Hz.","Freq.95...Hz.",
                    "PFC.Max.Freq..Hz.","PFC.Min.Freq..Hz.",
                    "Time.5...s.","Time.95...s.")

#path="/Users/kprovost/Dropbox (AMNH)/Postdoc_Backup/Zonotrichia_leucophrys/Subsets"
## get the list of files to import
metafiles = list.files(path,pattern=".selections.txt$",full.names = T,recursive=T) ## .Table.1
metafiles=metafiles[1:length(metafiles)]
metafiles=sub("//","/",metafiles)
shortnames = sub(".selections.txt","",basename(metafiles))
shortnames = sub("wav_","",shortnames)

## CLEANING UP A PREVIOUS MISTAKE
## DO NOT RUN THIS UNLESS YOU MADE THAT MISTAKE
# for(meta in metafiles){
#   print(meta)
#   df = read.table(meta,header=T,fill=T)
#   df = df[df$Low.Freq..Hz.>500,]
#   df = df[,colSums(is.na(df))!=nrow(df)]
#   write.table(df,meta,sep="\t",quote=T,row.names = F)
# }

#shortnames = sub("Cardinalis_cardinalis.BLB","",shortnames)
shortnames = sub(spp_substitute,"",shortnames)
inds = sapply(shortnames,FUN=function(x){paste(strsplit(x,"\\.")[[1]][parts_of_name_to_keep],sep=".",collapse = ".")})
#inds = sapply(shortnames,FUN=function(x){strsplit(x,"\\.")[[1]][1]})
inds = make.unique(inds)

calculate_freq_slope = function(x){
  diffs=diff(as.numeric(x))
  meanslope=mean(diffs,na.rm=T)
}


if(runSumstat==T){
  print("RUNNING SUMSTATS")
  library(Rraven)
  library(warbleR)
  final_bigtable_file = paste(path,"rvn.dat_trimmed_spectro_fcts_collapsed_",date,".txt",sep="")
  temp_bigtable_file = paste(path,"rvn.dat_trimmed_spectro_fcts_collapsed_",date,".txt.temp",sep="")
  generate_sumstats = function(filenames,suffix="selections.txt$",import_raven=T,
                               date=format(Sys.time(), "%d%b%Y"),make_merged=F,
                               check_correlations=F,dolength=F,loop=F,verbose=F){
    
    ## works whether warbler format T or F
    
    
    ## if the file doesn't have begin file then need to make it have that
    
    paths=unique(dirname(filenames))
    if(verbose==T){
      print("PATHS:")
      print(paths)
      print("~~~~~")
    }
    for(path in rev(paths[1:length(paths)])) {
      setwd(path)
      if(verbose==T){print(path)}
      
      filenames2 = list.files(path = path, pattern = suffix,full.names = T,recursive=F)
      filenames2 = sub("//","/",filenames2)
      filenames2 = filenames2[filenames2 %in% filenames]
      paths2=unique(dirname(filenames2))
      
      
      ## this is slower and prone to errors?
      if (import_raven ==T) {
        if(verbose==T){print("import raven")}
        
        outfile=paste("rvn.dat.ravenversion_",date,".txt",sep="")
        
        if(file.exists(outfile)) {
          rvn.dat=read.table(outfile,header=T)
        } else {
          
          dat_list = lapply(paths2,FUN=function(path2){
            print(path2)
            dat=NULL
            try({dat <- imp_raven(sound.file.col="Begin File",
                                  all.data = T,name.from.file = T,ext.case = "lower",
                                  path=path2,
                                  only.spectro.view = F,warbler.format = T,unread=F,recursive = F,files=NULL)})
            if(is.null(dat)){
              try({dat <- imp_raven(sound.file.col="Begin File",
                                    all.data = T,name.from.file = F,ext.case = "lower",
                                    path=path2,
                                    only.spectro.view = F,warbler.format = T,unread=F,recursive = F,files=NULL)})
            }
            return(dat)
          })
          
          rvn.dat=do.call(plyr::rbind.fill,dat_list)
          write.table(rvn.dat,outfile,row.names = F)
          ##rvn.dat.freq 
        }
        colnames(rvn.dat)[1:7] = c("selec","View","Channel","start","end","bottom.freq","top.freq")
        if(!("sound.files" %in% colnames(rvn.dat))) {
          rvn.dat$sound.files = rvn.dat$`Begin File`
        }
        
      } else {
        if(verbose==T){print("import not raven")}
        
        outfile=paste("rvn.dat_",date,".txt",sep="")
        
        if(file.exists(outfile)) {
          rvn.dat=read.table(outfile,header=T)
        } else {
          dat_list = lapply(1:length(filenames),FUN=function(i){
            print(paste(i,"/",length(filenames)))
            df = read.table(filenames[i],header=T,sep="\t")
            df$selec.file = filenames[i]
            return(df)
          })
          
          rvn.dat=do.call(plyr::rbind.fill,dat_list)
          write.table(rvn.dat,outfile,row.names = F)
        }
        colnames(rvn.dat)[1:7] = c("selec","View","Channel","start","end","bottom.freq","top.freq")
        if(!("sound.files" %in% colnames(rvn.dat))) {
          rvn.dat$sound.files = rvn.dat$Begin.File
        }
        
        
      }
      
      if(verbose==T){print("trim")}
      trimfile=paste("rvn.dat_trimmed_",date,".txt",sep="")
      if(file.exists(trimfile)) {
        rvn.dat=read.table(trimfile,header=T)
      } else {
        rvn.dat = rvn.dat[order(rvn.dat$selec.file, rvn.dat$start),] 
        rvn.dat$difference = rvn.dat$end-rvn.dat$start
        rvn.dat$gapprev = NA
        rvn.dat$gapnext = NA
        
        if(make_merged==T) {
          if(verbose==T){print("merge")}
          for(i in 1:nrow(rvn.dat)){
            if(i != 1) {
              if(rvn.dat$selec.file[i] == rvn.dat$selec.file[i-1]) {rvn.dat$gapprev[i] = rvn.dat$start[i]-rvn.dat$end[i-1]}
            } 
            if (i != nrow(rvn.dat)) {
              if(rvn.dat$selec.file[i] == rvn.dat$selec.file[i+1]) {rvn.dat$gapnext[i] = rvn.dat$start[i+1]-rvn.dat$end[i]}
            }
          }
          hist(rvn.dat$gapprev)
          
          ## combine overlapping sounds or sounds with not a lot of differences? 
          ## anything 0.5 or higher is definitely a different sound
          ## i think anything 0.01 or lower is the same sound
          ## cutoff: 0.05 
          
          rvn.dat.merged = rvn.dat[,c("start","end","bottom.freq","top.freq","sound.files","type","selec.file","difference","gapnext","gapprev")]
          rvn.dat.merged = rvn.dat.merged[order(rvn.dat.merged$selec.file, rvn.dat.merged$start),] 
          
          
          tomerge = c()
          
          for(i in 2:nrow(rvn.dat.merged)){
            print(i)
            if(!(is.na(rvn.dat.merged$gapprev[i])) & rvn.dat.merged$gapprev[i] <= 0.05 & rvn.dat.merged$selec.file[i]==rvn.dat.merged$selec.file[i-1]) {
              tomerge = c(tomerge,i-1,i)
            } else {
              if(length(tomerge)>0) {
                tomerge=sort(unique(tomerge))
                print(tomerge)
                rvn.dat.merged[tomerge,"start"] = min(rvn.dat.merged[tomerge,"start"],na.rm=T)
                rvn.dat.merged[tomerge,"end"] = max(rvn.dat.merged[tomerge,"end"],na.rm=T)
                
                rvn.dat.merged[tomerge,"bottom.freq"] = min(rvn.dat.merged[tomerge,"bottom.freq"],na.rm=T)
                rvn.dat.merged[tomerge,"top.freq"] = max(rvn.dat.merged[tomerge,"top.freq"],na.rm=T)
                
                rvn.dat.merged$difference[tomerge] = rvn.dat.merged$end[tomerge]-rvn.dat.merged$start[tomerge]
                
                rvn.dat.merged[tomerge,"gapnext"] = NA
                rvn.dat.merged[tomerge,"gapprev"] = NA
                
              }
              tomerge=c()
            }
          }
          rvn.dat.merged = unique(rvn.dat.merged)
          for(i in 1:nrow(rvn.dat.merged)){
            if(i != 1) {
              if(rvn.dat.merged$selec.file[i] == rvn.dat.merged$selec.file[i-1]) {rvn.dat.merged$gapprev[i] = rvn.dat.merged$start[i]-rvn.dat.merged$end[i-1]}
            } 
            if (i != nrow(rvn.dat.merged)) {
              if(rvn.dat.merged$selec.file[i] == rvn.dat.merged$selec.file[i+1]) {rvn.dat.merged$gapnext[i] = rvn.dat.merged$start[i+1]-rvn.dat.merged$end[i]}
            }
          }
          
          ## type==1 means was tweetynet'd 
          ## type==NA means was manually done
          rvn.dat.merged = rvn.dat.merged[order(rvn.dat.merged$sound.files, rvn.dat.merged$start),] 
          write.table(rvn.dat.merged,paste(trimfile,".merged.txt",sep=""),row.names = F)
          ## note to self: add a write-statement 
        }
        
        ## remove sounds that are too short?
        rvn.dat=rvn.dat[rvn.dat$difference>=0.05,]
        
        ## TODO: update this code 
        if (dolength==T){
          if(verbose==T){print("length")}
          ## cound the ";" in the peakfreqcontour to generate the function determining how many length.out to get
          #rvn.dat$`Peak Freq Contour (Hz)`
          
          ## THIS LINE MIGHT NOT WORK WITH THESE DATA RIGHT NOW
          try({length.out=as.numeric(sapply(rvn.dat$Peak.Freq.Contour..Hz.,FUN=function(i){as.numeric(stringr::str_count(i,";"))},simplify = T))})
          try({length.out=as.numeric(sapply(rvn.dat$`Peak Freq Contour (Hz)`,FUN=function(i){as.numeric(stringr::str_count(i,";"))},simplify = T))})
          rvn.dat$length.out=length.out+1
          
          mod=lm(rvn.dat$length.out~rvn.dat$difference)
          predicted=predict(mod,rvn.dat[,c("difference","length.out")])
          
          rvn.dat$length.out[is.na(rvn.dat$length.out)] = round(predicted[is.na(rvn.dat$length.out)])
        }
        write.table(rvn.dat,trimfile,row.names = F)
      }
      
      if(verbose==T){print("spectro")}
      specfile=paste("rvn.dat_trimmed_spectro_",date,".txt",sep="")
      if(file.exists(specfile)) { 
        rvn.dat.sp=read.table(specfile,header=T)  
      } else {
        
        sp_list = lapply(paths2,FUN=function(pathx){
          print(pathx)
          
          these_files = list.files(pathx,pattern="wav$")
          raven_files = rvn.dat$sound.files
          not_here = outersect(these_files,raven_files)
          print(paste("not here:",not_here))
          if(length(intersect(these_files,raven_files))>0){
          
          ## works on sin but not on card?
          rvn.dat.this = rvn.dat[!(rvn.dat$sound.files %in% not_here),]
          rvn.dat.this = rvn.dat.this[,1:11]
          rvn.dat.this = rvn.dat.this[complete.cases(rvn.dat.this),]
          
          sp <- spectro_analysis(X = rvn.dat.this,harmonicity = F,
                                 path=pathx,
                                 bp=c(0,22),fast=T) ## fast = 35 sec 28 cols, not fast = 3 min 15 sec
          ## harmonicity = T irrespective of fast causes a lot of errors and is estimated to take ~20 min
          return(sp)
          }
        })
        
        sp = do.call(plyr::rbind.fill,sp_list)
        
        ## merge sumstats
        
        rvn.dat.sp = merge(rvn.dat,sp,all=T)
        #rvn.dat.sp = cbind(rvn.dat,sp[,3:ncol(sp)])
        write.table(rvn.dat.sp,specfile,row.names = F)
      }
      
      if (check_correlations==T) {
        if(verbose==T){print("corrs")}
        correlation=cor(rvn.dat.sp[,sapply(rvn.dat.sp,class) %in% c("integer","numeric")],use="pairwise.complete.obs")
        
        correlation=correlation[colSums(!(is.na(correlation)))!=0,rowSums(!(is.na(correlation)))!=0]
        correlation=correlation[rownames(correlation)!="selec",colnames(correlation)!="selec"]
        
        corr_small = correlation[!(rownames(correlation) %in% colnames(sp)),colnames(correlation) %in% colnames(sp)]
        
        ## only corrs above 0.7
        evensmaller=corr_small[rowSums(corr_small>=0.7)>0,colSums(corr_small>=0.7)>0]
        
        png("corrplot.png",height=900,width=1200)
        corrplot::corrplot(t(evensmaller),method="number",diag=T,is.corr=F)
        dev.off()
        
        "high correlations sp:raven between duration, frequency, and time metrics"
      }
      
      ## this throws errors when the auto-generated ones are done? is it because they overlap? 47:74
      ## is it because they are too short? works when end-start >= 0.05 seconds but not >= 0.01
      # x=track_freq_contour(rvn.dat[rvn.dat$end-rvn.dat$start>=0.2,c("selec","start","end","sound.files")],bp=c(0,22))
      
      
      
      
      #rvn.dat$paths = dirname(rvn.dat$selec.file)
      rvn.dat$paths = path
      
      #rvn.dat=rvn.dat[rvn.dat$paths!="/Users/kprovost/Documents/BLB_Data",]
      
      ## this is gonna take a looooong time
      ## with the full dataset this takes ~30 sec 
      ## seems to run faster with raw.countor == T?
      ## fundamental and dominant pretty much are identical
      ## can also do rvn.dat$length.out[i] instead of length.out=100
      
      ## this only works when in the same working directory 
      
      if(verbose==T){print("fcts")}
      fctsfile=paste("rvn.dat_trimmed_fcts_",date,".txt",sep="")
      if(file.exists(fctsfile)) { 
        rvn.dat.fcts=read.table(fctsfile,header=T)
      } else {
        ## need to remove any selections over 20 seconds long or FCTs will break
        rvn.dat = rvn.dat[rvn.dat$difference<20,]
        ## also remove anything that isn't in the directory
        these_files = list.files(path,pattern="wav$")
        raven_files = rvn.dat$sound.files
        not_here = outersect(these_files,raven_files)
        print(paste("not here:",not_here))
        rvn.dat=rvn.dat[!(rvn.dat$sound.files %in% not_here),]
        
        if(loop==T) {
          y=lapply(1:nrow(rvn.dat),
                   FUN=function(i){
                     print(paste(i,"/",nrow(rvn.dat)))
                     x=freq_ts(rvn.dat[i,c("selec","start","end","bottom.freq","top.freq","sound.files")],
                               length.out = 100,img=F,ff.method = "tuneR",raw.contour = F,type="fundamental",
                               bp=c(0.5,10),clip.edges = F,threshold = 50,ovlp=50)
                   })
          fcts_dataframe = do.call(plyr::rbind.fill,y)
          
        } else {
          fcts_dataframe=freq_ts(rvn.dat[,c("selec","start","end","bottom.freq","top.freq","sound.files")],
                                 length.out = 100,img=F,ff.method = "tuneR",raw.contour = F,type="fundamental",pb=T,
                                 bp=c(0.5,10),clip.edges = F,threshold = 50,ovlp=50)
        }
        
        fcts_inflect = inflections(X = fcts_dataframe)
        fcts_dataframe=merge(fcts_inflect,fcts_dataframe, all=T)
        
        ## experiment with changing length
        ## 1017 not working use that to test 
        
        z=lapply(1:nrow(rvn.dat),
                 FUN=function(i){
                   print(paste(i,"/",nrow(rvn.dat)))
                   dur_sec=rvn.dat$end[i]-rvn.dat$start[i]
                   dur_samp = seconds_to_samples(seconds=dur_sec,samplerate=48000)
                   length_out =  round(dur_samp / (512 * (50/100)),0)-1
                   x=NULL
                   try({
                     x=freq_ts(rvn.dat[i,c("selec","start","end","bottom.freq","top.freq","sound.files")],img=F,
                               ff.method = "tuneR",
                               raw.contour = T,
                               type="fundamental",## issue
                               pb=F,
                               length.out = length_out,
                               bp=c(0.5,10),
                               clip.edges = F,
                               threshold = 50,
                               wl=512,
                               ovlp=50
                     )
                     ## calculate slope
                     slopes = diff(as.numeric(x[,-c(1:2)])*1000)
                     mean_slope = mean(slopes,na.rm=T)
                     slopes = paste(as.numeric(slopes),collapse = ";")
                     slopes=as.data.frame(slopes)
                     
                     raw_countour = paste(as.numeric(x[,-c(1:2)])*1000,collapse = ";")
                     raw_countour=as.data.frame(raw_countour)
                     
                     x=cbind(x[,1:2],raw_countour,slopes,mean_slope)
                   })
                   return(x)
                 })
        fcts_dataframe_raw = do.call(plyr::rbind.fill,z)
        fcts_dataframe=merge(fcts_dataframe_raw,fcts_dataframe, all=T)
        
        rvn.dat.fcts=merge(rvn.dat,fcts_dataframe,all=T)
        write.table(rvn.dat.fcts,fctsfile,row.names = F)
        
      }
      
      if(verbose==T){print("merge spectro fcts")}
      rvn.dat.sp.fcts = merge(rvn.dat.fcts,rvn.dat.sp,all=T)
      mergefile = paste("rvn.dat_trimmed_spectro_fcts_",date,".txt",sep="")
      write.table(rvn.dat.sp.fcts,mergefile)
      
      
      
      
    }
  }
  
  if(!(file.exists(final_bigtable_file))) {
  if(!(file.exists(temp_bigtable_file))){
  
  generate_sumstats(filenames=metafiles,verbose=T,date=date)
  print("DONE WITH MAKING STATS")
  
  ## to get slope of PFCs, subtract the i from i-1 value, multiply by 0.1875 -- i don't know why its this number
  
  ## convert big rvn.dat to selection table by modifying file 
  
  big_files = list.files(path=path,pattern="rvn.dat_trimmed_spectro_fcts_",
                         full.names = T,recursive = T)
  big_files = big_files[!(grepl("collapsed",big_files))]
  if(!(is.null(big_files))){
    if(length(big_files)==1){
      big_table = data.table::fread(big_files,data.table=F)
      big_table = unique(big_table)
    } else {
      big_table_list = lapply(big_files,FUN=function(x){data.table::fread(x,data.table=F)})
      big_table=do.call(gtools::smartbind,big_table_list)
      big_table = unique(big_table)
    }
    
  }
  
  big_table = big_table[ , !( colnames( big_table ) %in% c("V1") ) ]
  big_table = as.data.frame(big_table)
  colnames(big_table)=gsub('\\"','',colnames(big_table))
  
  ##  convert the ffreq cols to one col
  to_paste=colnames(big_table)[grepl("ffreq-",colnames(big_table))]
  big_table$ffreq = apply( big_table[ , to_paste ] , 1 , paste , collapse = ";" )
  big_table = big_table[ , !( colnames( big_table ) %in% to_paste ) ]
  colnames(big_table)[colnames(big_table) %in% c("selec","sound.files","start","end","bottom.freq","top.freq")] = c("Selection","Begin File","Begin Time (s)","End Time (s)", "Low Freq (Hz)", "High Freq (Hz)")
  write.table(big_table,temp_bigtable_file)
  } else {
    big_table = read.table(temp_bigtable_file,header=T,check.names = F)
  }
  
  
  big_table$`Low Freq (Hz)`[big_table$`Low Freq (Hz)` <= 10] = (big_table$`Low Freq (Hz)` * 1000)
  big_table$`High Freq (Hz)`[big_table$`High Freq (Hz)` <= 10] = (big_table$`High Freq (Hz)` * 1000)
  
  newcols_raw=lapply(big_table$`Begin File`,FUN=function(x){
    x = sub(".wav","",x)
    y = strsplit(x,"\\.")[[1]]
    if(length(y)==4) {
      z=rbind(genus=y[1],species=y[2],subspecies="unknown",database=y[3],catalog=y[4])
    } else if (length(y)==5) {
      z=rbind(genus=y[1],species=y[2],subspecies=y[3],database=y[4],catalog=y[5])
    }
    return(z)
  })
  newcols = do.call(cbind,newcols_raw)
  newcols = t(newcols)
  big_table = cbind(big_table,newcols)
  
  
  write.table(big_table,final_bigtable_file)
  } else {
  big_table=read.table(final_bigtable_file)
}
  ## now go over the old files and update the data
  ## TODO: ACOUNT FOR THE DIFFERENCE IN FREQUENCY 
  
  ## file mismatch:
  ## 1. change all of the filenames that are inside the selections.txt files
  ## 2. when you import the filename, change the filename accordingly 
  
  for(selec.file in sort(unique(big_table$selec.file))){
    print(selec.file)
    to_read = list.files(path=path,pattern=selec.file,full.names = T,recursive=T)
    to_read = to_read[!(grepl(".temp$",to_read))] ## removes files with .temp in them
    to_read=sub("//","/",to_read)
    
    if(length(to_read)>0){
      #print(to_read)
      for(i in 1:length(to_read)){
        print(i)
        ## DO NOT check if file exists, will break
        to_read_i = to_read[i]
        tempfile=paste(to_read_i,".temp",sep="")
        df_selec = read.table(to_read_i,header=T,fill=T,sep="\t",check.names =F)
        
        ## use the sub() function to change the filename 
        ## "Begin File" 
        df_selec$`Begin File` = gsub(pattern="_",replacement=".",x=df_selec$`Begin File`)
        df_selec$`Begin File` = gsub(pattern="-",replacement=".",x=df_selec$`Begin File`)
        df_selec$`Begin File` = gsub(pattern="BLB",replacement="BLB.",x=df_selec$`Begin File`)
        df_selec$`Begin File` = gsub(pattern="\\.\\.",replacement=".",x=df_selec$`Begin File`)
        
        write.table(df_selec,to_read_i,sep="\t",row.names = F)
        
        #print("subset")
        big_subset = big_table[big_table$selec.file==selec.file,]
        #print("merge")
        merged = merge(df_selec,big_subset,all=T) ## combine based on shared column names
        ## will look for exact matches between data
        ## if the data are converted wrong, they will not match 
        
        #print("write")
        write.table(merged,tempfile,sep="\t",quote=F,row.names = F,append=F)
      }
    }
  }
  
}

if(runBigpc==T){
  print("RUN BIG PC")
  bigdffile=paste(path,"bigdf_for_pca_",date,".txt",sep="")
  if(file.exists(bigdffile)){
    bigdf=data.table::fread(bigdffile,data.table = F)
  } else {
    
    
    print("GATHERING FILES")
    
    metafiles = list.files(path,pattern=".selections.txt.temp$",full.names = T,recursive = T) ## edited the code so that can find the files with the sumstats we generated
    
    print("GENERATING SUMSTATS")
    
    dflist = lapply(metafiles,FUN=function(meta){
      print(meta)
      name=sub(".Table.1.selections.txt","",basename(meta))
      split = strsplit(name,".wav_")[[1]]
      ind = split[1]
      segment = split[2]
      
      #df = NULL
      #try({df = read.table(meta,sep="\t",header=T)})
      df = read.table(meta,sep="\t",header=T)
      #if(!(is.null(df))){
      #print(head(df)) ## worked
      df=df[order(df[,"Begin.Time..s."]),]
      df$Selection=1:nrow(df)
      try({df$Bandwidth = as.numeric(df$Freq.75...Hz.-df$Freq.25...Hz.)},silent=T)
      if(is.null(df$Bandwidth)){
        try({df$Bandwidth = as.numeric(df$freq.Q75-df$freq.Q25)},silent=T)
        if(is.null(df$Bandwidth)){
          #df$Bandwidth = df$High.Freq..Hz.-df$Low.Freq..Hz.
          df$Bandwidth=NA
        }
      }
      #print(head(df))
      try({df$Time = as.numeric(df$Time.75...s.-df$Time.25...s.)},silent=T)
      if(is.null(df$Time)){
        try({df$Time = as.numeric(df$time.Q75-df$time.Q25)},silent=T)
        if(is.null(df$Time)){
          #df$Time = df$End.Time..s.-df$Begin.Time..s.
          df$Time=NA
        }
      }
      try({df$Center = as.numeric(df$Center.Freq..Hz.)},silent=T)
      if(is.null(df$Center)){
        try({df$Center = as.numeric(df$freq.median)},silent=T)
        if(is.null(df$Center)){
          df$Center=NA
        }
      }
      try({df$Inflection = as.numeric(df$PFC.Num.Inf.Pts)},silent=T)
      if(is.null(df$Inflection)){
        try({df$Inflection = as.numeric(df$inflections)},silent=T)
        if(is.null(df$Inflection)){
          df$Inflection=NA
        }   
      }    
      try({df$Slope = as.numeric(df$PFC.Avg.Slope..Hz.ms.)},silent=T)
      if(is.null(df$Slope)){
        try({df$Slope = as.numeric(df$mean_slope)},silent=T)
        if(is.null(df$Slope)){
          df$Slope=NA
        }  
      }      
      df = df[,c("Selection","Bandwidth","Time","Center","Inflection","Slope")]
      df$Individual = ind
      df$Segment = segment
      
      if(sum(complete.cases(df[,c("Selection","Bandwidth","Time","Center","Inflection","Slope")]))>1){
        return(df)
      }
      #} else {return(NULL)}
    })
    names(dflist)=basename(metafiles)
    
    species_dflist = sapply(names(dflist),FUN=function(x){
      y=strsplit(x,"\\.")[[1]][1:2]
      z=paste(y,sep=".",collapse = ".")
    },simplify = T)
    
    bigdf = do.call(rbind,dflist)
    write.table(bigdf,bigdffile)
  }
  #plot(bigdf[,c("Bandwidth","Time","Center","Inflection","Slope")],col=rgb(0,0,0,0.3))
  
  ## quick pca on bigdf 
  
  big_for_pca=bigdf[complete.cases(bigdf[,c("Selection","Bandwidth","Time","Center","Inflection","Slope")]),]
  
  big_pca = prcomp(big_for_pca[,c("Bandwidth","Time","Center","Inflection","Slope")],
                   center = T,scale. = T)
  
  big_for_pca_combo = cbind(big_for_pca,big_pca$x)
  big_for_pca_combo = merge(big_for_pca,big_for_pca_combo,all=T)
  
  newcols_raw=lapply(big_for_pca_combo$Individual,FUN=function(x){
    #print(x)
    x = sub(".selections.txt.temp","",x)
    x = sub(".temp","",x)
    y = strsplit(x,"\\.")[[1]]
    if(length(y)==4) {
      z=rbind(genus=y[1],species=y[2],subspecies="unknown",database=y[3],catalog=y[4])
    } else if (length(y)>=5) {
      z=rbind(genus=y[1],species=y[2],subspecies=y[3],database=y[4],catalog=y[5])
    }
    return(z)
  })
  newcols = do.call(cbind,newcols_raw)
  newcols = t(newcols)
  big_for_pca_combo = cbind(big_for_pca_combo,newcols)
  #big_for_pca_combo = big_for_pca
  pcacombofile=paste(path,"combined_table_for_pca_",date,".txt",sep="")
  write.table(big_for_pca_combo,pcacombofile,sep="\t",row.names = F)
  
  #plot(as.data.frame(big_pca$x[,1:2]),col=as.numeric(as.factor(big_for_pca$Individual)))
  
  #pdf("~/test.pdf")
  #par(mfrow=c(3,3))
  #for(ind in unique(big_for_pca$Individual)){
  #plot(as.data.frame(big_pca$x[,1:2]),col="grey",main=ind)
  #points(big_pca$x[which(big_for_pca$Individual==ind),1:2]) 
  #}
  #dev.off()
  
  write.table(big_pca$rotation,paste("rotation_table_for_pca_",date,".txt",sep=""),sep="\t",row.names = F)
  write.table(summary(big_pca)$importance[2,] * t(big_pca$rotation),paste("importance-rotation_table_for_pca_",date,".txt",sep=""),sep="\t",row.names = F)
  write.table(summary(big_pca)$importance,paste("importance_table_for_pca_",date,".txt",sep=""),sep="\t",row.names = F)
  
  print(big_pca$rotation)
  print(summary(big_pca)$importance[2,] * t(big_pca$rotation))
  #corrplot::corrplot(summary(big_pca)$importance[2,] * t(big_pca$rotation),
  #                   method="color",is.corr=F)
  
  
  ## generate a centroid
  agg = aggregate(cbind(big_for_pca_combo$Bandwidth,big_for_pca_combo$Time,big_for_pca_combo$Center,big_for_pca_combo$Inflection,
                        big_for_pca_combo$Slope,big_for_pca_combo$PC1,big_for_pca_combo$PC2,big_for_pca_combo$PC3,big_for_pca_combo$PC4,big_for_pca_combo$PC5)
                  ~big_for_pca_combo$genus+big_for_pca_combo$species+big_for_pca_combo$subspecies+big_for_pca_combo$database+big_for_pca_combo$catalog,
                  FUN=function(x){mean(x,na.rm=T)})
  colnames(agg) = c("genus","species","subspecies","database","catalog",
                    "Bandwidth","Time","Center","Inflection","Slope",
                    "PC1","PC2","PC3","PC4","PC5")
  bigcentfile=paste(path,"centroids_per_individual_bigpca_",date,".txt",sep="")
  write.table(agg,bigcentfile,row.names = F,sep="\t",quote=F)
  
  aggspp = aggregate(cbind(big_for_pca_combo$Bandwidth,big_for_pca_combo$Time,big_for_pca_combo$Center,big_for_pca_combo$Inflection,
                        big_for_pca_combo$Slope,big_for_pca_combo$PC1,big_for_pca_combo$PC2,big_for_pca_combo$PC3,big_for_pca_combo$PC4,big_for_pca_combo$PC5)
                  ~big_for_pca_combo$genus+big_for_pca_combo$species+big_for_pca_combo$subspecies,
                  FUN=function(x){mean(x,na.rm=T)})
  colnames(aggspp) = c("genus","species","subspecies",
                    "Bandwidth","Time","Center","Inflection","Slope",
                    "PC1","PC2","PC3","PC4","PC5")
  bigcentfile=paste(path,"centroids_per_subspecies_bigpca_",date,".txt",sep="")
  write.table(aggspp,bigcentfile,row.names = F,sep="\t",quote=F)
  
  aggspp2 = aggregate(cbind(big_for_pca_combo$Bandwidth,big_for_pca_combo$Time,big_for_pca_combo$Center,big_for_pca_combo$Inflection,
                           big_for_pca_combo$Slope,big_for_pca_combo$PC1,big_for_pca_combo$PC2,big_for_pca_combo$PC3,big_for_pca_combo$PC4,big_for_pca_combo$PC5)
                     ~big_for_pca_combo$genus+big_for_pca_combo$species,
                     FUN=function(x){mean(x,na.rm=T)})
  colnames(aggspp2) = c("genus","species",
                    "Bandwidth","Time","Center","Inflection","Slope",
                    "PC1","PC2","PC3","PC4","PC5")
  bigcentfile=paste(path,"centroids_per_species_bigpca_",date,".txt",sep="")
  write.table(aggspp2,bigcentfile,row.names = F,sep="\t",quote=F)

  agggen = aggregate(cbind(big_for_pca_combo$Bandwidth,big_for_pca_combo$Time,big_for_pca_combo$Center,big_for_pca_combo$Inflection,
                            big_for_pca_combo$Slope,big_for_pca_combo$PC1,big_for_pca_combo$PC2,big_for_pca_combo$PC3,big_for_pca_combo$PC4,big_for_pca_combo$PC5)
                      ~big_for_pca_combo$genus,
                      FUN=function(x){mean(x,na.rm=T)})
  colnames(agggen) = c("genus",
                        "Bandwidth","Time","Center","Inflection","Slope",
                        "PC1","PC2","PC3","PC4","PC5")
  bigcentfile=paste(path,"centroids_per_genus_bigpca_",date,".txt",sep="")
  write.table(agggen,bigcentfile,row.names = F,sep="\t",quote=F)
}

if(runCentroids==T){
  print("RUN CENTROIDS")
  
  
  
  
  
  
  if(!(exists("dflist"))){
    print("GATHERING FILES")
    
    metafiles = list.files(path,pattern=".selections.txt.temp$",full.names = T,recursive = T) ## edited the code so that can find the files with the sumstats we generated
    
    print("GENERATING SUMSTATS")
    
    dflist = lapply(metafiles,FUN=function(meta){
      print(meta)
      name=sub(".Table.1.selections.txt","",basename(meta))
      split = strsplit(name,".wav_")[[1]]
      ind = split[1]
      segment = split[2]
      
      df = read.table(meta,sep="\t",header=T)
      #print(head(df)) ## worked
      df=df[order(df[,"Begin.Time..s."]),]
      df$Selection=1:nrow(df)
      try({df$Bandwidth = as.numeric(df$Freq.75...Hz.-df$Freq.25...Hz.)},silent=T)
      if(is.null(df$Bandwidth)){
        try({df$Bandwidth = as.numeric(df$freq.Q75-df$freq.Q25)},silent=T)
        if(is.null(df$Bandwidth)){
          #df$Bandwidth = df$High.Freq..Hz.-df$Low.Freq..Hz.
          df$Bandwidth=NA
        }
      }
      #print(head(df))
      try({df$Time = as.numeric(df$Time.75...s.-df$Time.25...s.)},silent=T)
      if(is.null(df$Time)){
        try({df$Time = as.numeric(df$time.Q75-df$time.Q25)},silent=T)
        if(is.null(df$Time)){
          #df$Time = df$End.Time..s.-df$Begin.Time..s.
          df$Time=NA
        }
      }
      try({df$Center = as.numeric(df$Center.Freq..Hz.)},silent=T)
      if(is.null(df$Center)){
        try({df$Center = as.numeric(df$freq.median)},silent=T)
        if(is.null(df$Center)){
          df$Center=NA
        }
      }
      try({df$Inflection = as.numeric(df$PFC.Num.Inf.Pts)},silent=T)
      if(is.null(df$Inflection)){
        try({df$Inflection = as.numeric(df$inflections)},silent=T)
        if(is.null(df$Inflection)){
          df$Inflection=NA
        }   
      }    
      try({df$Slope = as.numeric(df$PFC.Avg.Slope..Hz.ms.)},silent=T)
      if(is.null(df$Slope)){
        try({df$Slope = as.numeric(df$mean_slope)},silent=T)
        if(is.null(df$Slope)){
          df$Slope=NA
        }  
      }      
      df = df[,c("Selection","Bandwidth","Time","Center","Inflection","Slope")]
      df$Individual = ind
      df$Segment = segment
      
      if(sum(complete.cases(df[,c("Selection","Bandwidth","Time","Center","Inflection","Slope")]))>1){
        return(df)
      }
    })
    names(dflist)=basename(metafiles)
    
    species_dflist = sapply(names(dflist),FUN=function(x){
      y=strsplit(x,"\\.")[[1]][1:2]
      z=paste(y,sep=".",collapse = ".")
    },simplify = T)
  }
  
  this_length = length(dflist)
  ind_mean_matrix = matrix(nrow=this_length,ncol=this_length)
  centroiddf=data.frame(matrix(ncol=3,nrow=0))
  
  ## compare centroids 
  for(ind_i in 1:this_length) {
    print(paste(ind_i,"/",this_length))
    df_i = dflist[[ind_i]]
    if(!(is.null(df_i))){
      
      centroid_i = colMeans(df_i[,c("Bandwidth","Time","Center","Inflection","Slope")],na.rm=T)
      centroiddf=rbind(centroiddf,centroid_i)
      
      for(ind_j in ind_i:this_length){
        #print(paste(ind_i,"-",ind_j))
        df_j = dflist[[ind_j]]
        if(!(is.null(df_j))){
          centroid_j = colMeans(df_j[,c("Bandwidth","Time","Center","Inflection","Slope")],na.rm=T)
          cent_ij = dist(rbind(centroid_i,centroid_j))
          ind_mean_matrix[ind_i,ind_j] = cent_ij
          ind_mean_matrix[ind_j,ind_i] = cent_ij
        } else {
          ind_mean_matrix[ind_i,ind_j] = NA
          ind_mean_matrix[ind_j,ind_i] = NA
        }
      }
    } else {
      ind_mean_matrix[ind_i,] = NA
      ind_mean_matrix[,ind_i] = NA
      centroiddf=rbind(centroiddf,c(NA,NA,NA,NA,NA))
    }
  }
  colnames(centroiddf) = c("Bandwidth","Time","Center","Inflection","Slope")
  centroiddf$Ind = basename(metafiles)
  
  ## for centroid data, calculate the species, genus, database, number, etc
  newcols_raw=lapply(centroiddf$Ind,FUN=function(x){
    x = sub(".selections.txt.temp","",x)
    y = strsplit(x,"\\.")[[1]]
    x = sub(".temp","",x)
    if(length(y)==4) {
      z=rbind(genus=y[1],species=y[2],subspecies="unknown",database=y[3],catalog=y[4])
    } else if (length(y)>=5) {
      z=rbind(genus=y[1],species=y[2],subspecies=y[3],database=y[4],catalog=y[5])
    }
    return(z)
  })
  newcols = do.call(cbind,newcols_raw)
  newcols = t(newcols)
  centroiddf = cbind(centroiddf,newcols)
  
  colnames(ind_mean_matrix) = centroiddf$Ind ## this is failing on inds so changed to Ind
  rownames(ind_mean_matrix) = centroiddf$Ind
  #corrplot::corrplot(ind_mean_matrix,is.corr=F,method="color")
  #plot(ape::nj(ind_mean_matrix))
  
  write.table(centroiddf,
              paste(outpath,"/centroid_locations_per_individual_",date,".txt",sep=""),
              sep="\t",quote=F,row.names = F)
  write.table(ind_mean_matrix,
              paste(outpath,"/mean_centroid_distances_individuals_",date,".txt",sep=""),
              sep="\t",quote=F,row.names = T)
  
  #plot(centroiddf)
  
  ## separate out centroid distances by species
  centroidfilename=paste(outpath,"/mean_centroid_distances_individuals_",date,".txt",sep="")
  centroidfilename=sub("//","/",centroidfilename)
  ind_mean_matrix = data.table::fread(file=centroidfilename,data.table = F)
  rownames(ind_mean_matrix) = ind_mean_matrix$V1
  ind_mean_matrix=ind_mean_matrix[,-which(colnames(ind_mean_matrix)=="V1")]
  
  genera = sapply(colnames(ind_mean_matrix),function(x){strsplit(x,"\\.")[[1]][1]})
  scientific = sapply(colnames(ind_mean_matrix),function(x){
    y=strsplit(x,"\\.")[[1]][1:2]
    y=paste(y,sep=".",collapse=".")
    return(y)
  })
  withsubspecies = sapply(colnames(ind_mean_matrix),function(x){
    y=strsplit(x,"\\.")[[1]][1:3]
    if(y[3]!="unknown" & y[3] != "XC") {
      y=paste(y,sep=".",collapse=".")
      return(y)
    } else {
      return(NA)
    }
  })
  
  ## split out by genera, species, and subspcecies
  for(genus in sort(unique(genera))) {
    print(genus)
    ind_mean_matrix_genus = ind_mean_matrix[which(genera == genus),which(genera == genus)]
    write.table(ind_mean_matrix_genus,paste(outpath,"/mean_centroid_distances_individuals_",date,"_genus_",genus,".txt",sep=""))
  }
  for(sci in sort(unique(scientific))) {
    print(sci)
    ind_mean_matrix_sci = ind_mean_matrix[which(scientific == sci),which(scientific == sci)]
    write.table(ind_mean_matrix_sci,paste(outpath,"/mean_centroid_distances_individuals_",date,"_scientific_",sci,".txt",sep=""))
  }
  for(subspp in sort(unique(withsubspecies))) {
    print(subspp)
    ind_mean_matrix_subspp = ind_mean_matrix[which(withsubspecies == subspp),which(withsubspecies == subspp)]
    write.table(ind_mean_matrix_subspp,paste(outpath,"/mean_centroid_distances_individuals_",date,"_withsubspecies_",subspp,".txt",sep=""))
  }
  
  ## quick pca on centroids 
  
  cent_df = read.table(paste(outpath,"/centroid_locations_per_individual_",date,".txt",sep=""),
                       sep="\t",header=T)
  cent_for_pca=cent_df[complete.cases(cent_df),]
  cent_pca = prcomp(cent_for_pca[,c("Bandwidth","Time","Center","Inflection","Slope")],
                    center = T,scale. = T)
  
  cent_df_pca = cbind(cent_for_pca,cent_pca$x)
  cent_df_pca = merge(cent_df,cent_df_pca,all=T)
  
  write.table(cent_df_pca,
              paste(outpath,"/centroid_locations_per_individual_PCA_",date,".txt",sep=""),
              sep="\t",quote=F,row.names = T)
  
  #plot(as.data.frame(cent_pca$x))
  ## pc1 = -time, +center, -inf, +band, +slope
  ## pc2 = +slope, +inf, +band, +center, +time
  ## pc3 = -slope*, +band, +inf, +time, +cent
  ## pc4 = -band*, +cent*, +inf, -time, -slope
  ## pc5 = -time*, +inf*, -cent, +band, -slope
  cent_pca$rotation
  summary(cent_pca)$importance[2,] * t(cent_pca$rotation)
  #corrplot::corrplot(summary(cent_pca)$importance[2,] * t(cent_pca$rotation),
  #                   method="color",is.corr=F)
  
  
  write.table(cent_pca$rotation,paste(outpath,"rotation_table_for_centroid_pca_",date,".txt",sep=""),sep="\t",row.names = F)
  write.table(summary(cent_pca)$importance[2,] * t(cent_pca$rotation),paste(outpath,"importance-rotation_table_for_centroid_pca_",date,".txt",sep=""),sep="\t",row.names = F)
  write.table(summary(cent_pca)$importance,paste(outpath,"importance_table_for_centroid_pca_",date,".txt",sep=""),sep="\t",row.names = F)
  
}

## compare pairwise syllable-syllable between ind a, ind b 

if(runSyllables==T){
  print("RUN SYLLABLES")
  
  if(!(exists(dflist))){
    print("GATHERING FILES")
    
    metafiles = list.files(path,pattern=".selections.txt.temp$",full.names = T,recursive = T) ## edited the code so that can find the files with the sumstats we generated
    
    print("GENERATING SUMSTATS")
    
    dflist = lapply(metafiles,FUN=function(meta){
      print(meta)
      name=sub(".Table.1.selections.txt","",basename(meta))
      split = strsplit(name,".wav_")[[1]]
      ind = split[1]
      segment = split[2]
      
      df = read.table(meta,sep="\t",header=T)
      #print(head(df)) ## worked
      df=df[order(df[,"Begin.Time..s."]),]
      df$Selection=1:nrow(df)
      try({df$Bandwidth = as.numeric(df$Freq.75...Hz.-df$Freq.25...Hz.)},silent=T)
      if(is.null(df$Bandwidth)){
        try({df$Bandwidth = as.numeric(df$freq.Q75-df$freq.Q25)},silent=T)
        if(is.null(df$Bandwidth)){
          #df$Bandwidth = df$High.Freq..Hz.-df$Low.Freq..Hz.
          df$Bandwidth=NA
        }
      }
      #print(head(df))
      try({df$Time = as.numeric(df$Time.75...s.-df$Time.25...s.)},silent=T)
      if(is.null(df$Time)){
        try({df$Time = as.numeric(df$time.Q75-df$time.Q25)},silent=T)
        if(is.null(df$Time)){
          #df$Time = df$End.Time..s.-df$Begin.Time..s.
          df$Time=NA
        }
      }
      try({df$Center = as.numeric(df$Center.Freq..Hz.)},silent=T)
      if(is.null(df$Center)){
        try({df$Center = as.numeric(df$freq.median)},silent=T)
        if(is.null(df$Center)){
          df$Center=NA
        }
      }
      try({df$Inflection = as.numeric(df$PFC.Num.Inf.Pts)},silent=T)
      if(is.null(df$Inflection)){
        try({df$Inflection = as.numeric(df$inflections)},silent=T)
        if(is.null(df$Inflection)){
          df$Inflection=NA
        }   
      }    
      try({df$Slope = as.numeric(df$PFC.Avg.Slope..Hz.ms.)},silent=T)
      if(is.null(df$Slope)){
        try({df$Slope = as.numeric(df$mean_slope)},silent=T)
        if(is.null(df$Slope)){
          df$Slope=NA
        }  
      }      
      df = df[,c("Selection","Bandwidth","Time","Center","Inflection","Slope")]
      df$Individual = ind
      df$Segment = segment
      
      if(sum(complete.cases(df[,c("Selection","Bandwidth","Time","Center","Inflection","Slope")]))>1){
        return(df)
      }
    })
    names(dflist)=basename(metafiles)
    
    species_dflist = sapply(names(dflist),FUN=function(x){
      y=strsplit(x,"\\.")[[1]][1:2]
      z=paste(y,sep=".",collapse = ".")
    },simplify = T)
  }
  
  
  for(spp in sort(unique(species_dflist))){
    print(spp)
    dflist_subset = dflist[grepl(spp,names(dflist))]
    dflist_subset=dflist_subset[!sapply(dflist_subset,is.null)]
    
    this_length = length(dflist_subset)
    
    ind_dist_matrix = list()
    nameslist = c()
    
    for(ind_i in 1:this_length) {
      print(paste(ind_i,"/",this_length))
      df_i = dflist_subset[[ind_i]]
      
      if(!(is.null(df_i))){
        
        for(ind_j in ind_i:this_length){
          #print(paste(ind_i,"-",ind_j))
          df_j = dflist_subset[[ind_j]]
          
          if(!(is.null(df_j))){
            
            # plot(rbind(df_i[,c("Bandwidth","Time")],df_j[,c("Bandwidth","Time")]),
            #      type="n")
            # points(df_i[,c("Bandwidth","Time")],col="red",pch=1)
            # points(df_j[,c("Bandwidth","Time")],col="black",pch=17)
            
            nameslist=append(nameslist,paste(ind_i,ind_j,sep="-"))
            
            matrix_ij = matrix(nrow=nrow(df_i),ncol=nrow(df_j))
            
            ## loop over the syllables
            
            for(row_i in 1:nrow(df_i)){
              for(row_j in 1:nrow(df_j)){
                
                syll_i = df_i[row_i,c("Bandwidth","Time","Center","Inflection","Slope")]
                syll_j = df_j[row_j,c("Bandwidth","Time","Center","Inflection","Slope")]
                
                matrix_ij[row_i,row_j] = dist(rbind(syll_i,syll_j))
                
              }
            }
            
            newlist = list(matrix_ij)
            ind_dist_matrix = append(ind_dist_matrix,newlist)
            
          } else {
            ind_dist_matrix = append(ind_dist_matrix,list(NULL))
          }
        }
        
      } else {
        ind_dist_matrix = append(ind_dist_matrix,list(NULL))
      }
    }
    names(ind_dist_matrix) = nameslist
    
    mean_dists = lapply(ind_dist_matrix,FUN=mean); names(mean_dists) = nameslist
    
    mean_dist_inds_df=as.data.frame(cbind(nameslist,mean_dists,do.call(rbind,sapply(nameslist,FUN=function(x){strsplit(x,"-")}))))
    colnames(mean_dist_inds_df)=c("nameslist","mean_dists","ind1","ind2")
    mean_dist_inds_df$ind1=as.character(shortnames[as.numeric(mean_dist_inds_df$ind1)])
    mean_dist_inds_df$ind2=as.character(shortnames[as.numeric(mean_dist_inds_df$ind2)])
    mean_dist_inds_df$nameslist=as.character(mean_dist_inds_df$nameslist)
    mean_dist_inds_df$mean_dists = as.numeric(mean_dist_inds_df$mean_dists)
    
    x=mean_dist_inds_df[,c(1,2,3)]; colnames(x) = c("mean_dists","ind1","ind2")
    y=mean_dist_inds_df[,c(1,3,2)]; colnames(y) = c("mean_dists","ind1","ind2")
    mean_dist_inds_df = unique(mean_dist_inds_df)
    
    mean_dist_inds_df = mean_dist_inds_df[order(mean_dist_inds_df$ind1,mean_dist_inds_df$ind2),]
    
    ## the above is not quite right. resulting data is not symmetrical
    write.table(mean_dist_inds_df,paste(outpath,"/mean_syll_distance_inds_",spp,"_",date,".txt",sep=""),
                sep="\t",row.names = F,quote=F)
    
    mean_dist_inds_df=read.table(paste(outpath,"/mean_syll_distance_inds_",spp,"_",date,".txt",sep=""),
                                 sep="\t",header=T)
    
    x=mean_dist_inds_df[,c(1,2,3)]; colnames(x) = c("mean_dists","ind1","ind2")
    y=mean_dist_inds_df[,c(1,3,2)]; colnames(y) = c("mean_dists","ind1","ind2")
    
    mean_dist_inds_df = rbind(x,y)
    mean_dist_inds_df = unique(mean_dist_inds_df)
    mean_dist_inds_df = mean_dist_inds_df[order(mean_dist_inds_df$ind1,mean_dist_inds_df$ind2),]
    
    
    mean_dist_mat=matrix(unlist(mean_dist_inds_df$mean_dists),nrow=length(unique(mean_dist_inds_df$ind1)),ncol=length(unique(mean_dist_inds_df$ind2)))
    colnames(mean_dist_mat) = unique(mean_dist_inds_df$ind2)
    rownames(mean_dist_mat) = unique(mean_dist_inds_df$ind1)
    
    #corrplot::corrplot(mean_dist_mat,is.corr=F,method="color")
    
    write.table(mean_dist_mat,paste(outpath,"/mean_syll_distance_inds_square_",spp,"_",date,".txt",sep=""),
                sep="\t",row.names = T,quote=F)
    
    #plot(c(ind_mean_matrix),c(mean_dist_mat)) ## basically the same it looks like.
    
    
    #colnames(mean_dist_mat) = inds
    #rownames(mean_dist_mat) = inds
    
    #par(mfrow=c(1,2))
    #pdf("~/test.pdf",width=15,height=15)
    #plot(ape::nj(mean_dist_mat),type="cladogram",cex=0.5)
    #dev.off()
    #phytools::multiRF(c(ape::nj(mean_dist_mat),ape::nj(ind_mean_matrix)))
    
    ## calculate some sort of euclidean distances 
  }
}

if(generatePlots==T){
  print("GENERATE PLOTS")
  ## check for each section if files exist
  
  ## generate distance trees for each small matrix
  
  matrix_files_genus = list.files(path,pattern="_genus_",recursive=F,full.names = T)
  matrix_files_genus = matrix_files_genus[!(grepl("tree",matrix_files_genus))]
  for(matrix_file in rev(matrix_files_genus)){
    print(matrix_file)
    matrix_dist = read.table(matrix_file,header=T)
    ## remove na rows
    if(sum(is.na(matrix_dist))>0){
      allna=colSums(is.na(matrix_dist))
      bad=names(which(allna==max(allna,na.rm=T)))
      matrix_dist = matrix_dist[-which(colnames(matrix_dist) %in% bad),-which(colnames(matrix_dist) %in% bad)]
    }
    try({
      njtree=ape::njs(as.dist(matrix_dist))
      njtree$tip.label = sub(".selections.txt.temp","",njtree$tip.label)
      #plot(njtree,type="radial")
      ape::write.tree(njtree,paste(matrix_file,".tree",sep=""))
    })
  }
  
  matrix_files_sci = list.files(path,pattern="_scientific_",recursive=T,full.names = T)
  matrix_files_sci = matrix_files_sci[!(grepl("tree",matrix_files_sci))]
  for(matrix_file in matrix_files_sci){
    print(matrix_file)
    matrix_dist = read.table(matrix_file,header=T)
    ## remove na rows
    if(sum(is.na(matrix_dist))>0){
      allna=colSums(is.na(matrix_dist))
      bad=names(which(allna==max(allna,na.rm=T)))
      matrix_dist = matrix_dist[-which(colnames(matrix_dist) %in% bad),-which(colnames(matrix_dist) %in% bad)]
    }
    try({
      njtree=ape::njs(as.dist(matrix_dist))
      njtree$tip.label = sub(".selections.txt.temp","",njtree$tip.label)
      #plot(njtree,type="radial")
      ape::write.tree(njtree,paste(matrix_file,".tree",sep=""))
    })
  }
  
  matrix_files_sub = list.files(path,pattern="_withsubspecies_",recursive=T,full.names = T)
  matrix_files_sub = matrix_files_sub[!(grepl("tree",matrix_files_sub))]
  for(matrix_file in matrix_files_sub){
    print(matrix_file)
    matrix_dist = read.table(matrix_file,header=T)
    ## remove na rows
    if(sum(is.na(matrix_dist))>0){
      allna=colSums(is.na(matrix_dist))
      bad=names(which(allna==max(allna,na.rm=T)))
      matrix_dist = matrix_dist[-which(colnames(matrix_dist) %in% bad),-which(colnames(matrix_dist) %in% bad)]
    }
    try({
      njtree=ape::njs(as.dist(matrix_dist))
      njtree$tip.label = sub(".selections.txt.temp","",njtree$tip.label)
      #plot(njtree,type="radial")
      ape::write.tree(njtree,paste(matrix_file,".tree",sep=""))
    })
  }
  
  centroid_pca_file = paste("centroid_locations_per_individual_PCA_",date,".txt",sep="")
  
  if(file.exists(centroid_pca_file)){
    
    centroid_pca = read.table(centroid_pca_file,header=T)
    centroid_pca$scientific = paste(centroid_pca$genus,centroid_pca$species)
    
    par(mfrow=c(2,3))
    boxplot(centroid_pca$Bandwidth~centroid_pca$genus,las=2,main="Bandwidth")
    boxplot(centroid_pca$Time~centroid_pca$genus,las=2,main="Time")
    boxplot(centroid_pca$Center~centroid_pca$genus,las=2,main="Center")
    boxplot(centroid_pca$Inflection~centroid_pca$genus,las=2,main="Inflection")
    boxplot(centroid_pca$Slope~centroid_pca$genus,las=2,min="Slope")
    plot(centroid_pca$Bandwidth,centroid_pca$Time,col=as.numeric(as.factor(centroid_pca$genus)),main="Bandwidth:Time")
    
    par(mfrow=c(2,3))
    boxplot(centroid_pca$Bandwidth~centroid_pca$genus,las=2,main="PC1")
    boxplot(centroid_pca$Time~centroid_pca$genus,las=2,main="PC2")
    boxplot(centroid_pca$Center~centroid_pca$genus,las=2,main="PC3")
    boxplot(centroid_pca$Inflection~centroid_pca$genus,las=2,main="PC4")
    boxplot(centroid_pca$Slope~centroid_pca$genus,las=2,min="PC5")
    plot(centroid_pca$PC1,centroid_pca$PC2,col=as.numeric(as.factor(centroid_pca$genus)),main="PC1:2")
    
    par(mfrow=c(2,3))
    boxplot(centroid_pca$Bandwidth~centroid_pca$scientific,las=2,main="Bandwidth")
    boxplot(centroid_pca$Time~centroid_pca$scientific,las=2,main="Time")
    boxplot(centroid_pca$Center~centroid_pca$scientific,las=2,main="Center")
    boxplot(centroid_pca$Inflection~centroid_pca$scientific,las=2,main="Inflection")
    boxplot(centroid_pca$Slope~centroid_pca$scientific,las=2,min="Slope")
    plot(centroid_pca$Bandwidth,centroid_pca$Time,col=as.numeric(as.factor(centroid_pca$scientific)),main="Bandwidth:Time")
    
    par(mfrow=c(2,3))
    boxplot(centroid_pca$Bandwidth~centroid_pca$scientific,las=2,main="PC1")
    boxplot(centroid_pca$Time~centroid_pca$scientific,las=2,main="PC2")
    boxplot(centroid_pca$Center~centroid_pca$scientific,las=2,main="PC3")
    boxplot(centroid_pca$Inflection~centroid_pca$scientific,las=2,main="PC4")
    boxplot(centroid_pca$Slope~centroid_pca$scientific,las=2,min="PC5")
    plot(centroid_pca$PC1,centroid_pca$PC2,col=as.numeric(as.factor(centroid_pca$scientific)),main="PC1:2")
    
    par(mfrow=c(3,3))
    plot(centroid_pca$PC1,centroid_pca$PC3,col=as.numeric(as.factor(centroid_pca$genus)),main="PC1:3")
    plot(centroid_pca$PC1,centroid_pca$PC4,col=as.numeric(as.factor(centroid_pca$genus)),main="PC1:4")
    plot(centroid_pca$PC1,centroid_pca$PC5,col=as.numeric(as.factor(centroid_pca$genus)),main="PC1:5")
    plot(centroid_pca$PC2,centroid_pca$PC3,col=as.numeric(as.factor(centroid_pca$genus)),main="PC2:3")
    plot(centroid_pca$PC2,centroid_pca$PC4,col=as.numeric(as.factor(centroid_pca$genus)),main="PC2:4")
    plot(centroid_pca$PC2,centroid_pca$PC5,col=as.numeric(as.factor(centroid_pca$genus)),main="PC2:5")
    plot(centroid_pca$PC3,centroid_pca$PC4,col=as.numeric(as.factor(centroid_pca$genus)),main="PC3:4")
    plot(centroid_pca$PC3,centroid_pca$PC5,col=as.numeric(as.factor(centroid_pca$genus)),main="PC3:5")
    plot(centroid_pca$PC4,centroid_pca$PC5,col=as.numeric(as.factor(centroid_pca$genus)),main="PC4:5")
    
    ## let's do some stats
    
    
    
  }
  
  centroid_dist_file = paste("mean_centroid_distances_individuals_",date,".txt",sep="")
  
  if(file.exists(centroid_dist_file)){
    
    centroid_dist = read.table(centroid_dist_file,header=T)
    
    ## remove na rows
    if(sum(is.na(centroid_dist))>0){
      allna=colSums(is.na(centroid_dist))
      bad=names(which(allna==max(allna,na.rm=T)))
      centroid_dist = centroid_dist[-which(colnames(centroid_dist) %in% bad),-which(colnames(centroid_dist) %in% bad)]
      
    }
    
    
    njtree=ape::njs(as.dist(centroid_dist))
    ape::write.tree(njtree,paste("mean_centroid_distances_individuals_tree_",date,".tree",sep=""))
    
  }
  
  big_pca_file = paste("combined_table_for_pca_",date,".txt",sep="")
  
  if(file.exists(big_pca_file)){
    
    big_pca = read.table(big_pca_file,header=T)
    big_pca$scientific = paste(big_pca$genus,big_pca$species)
    
    modelspp=aov(big_pca$PC1~big_pca$scientific)
    summary(modelspp) ## sig
    perspp_sig = TukeyHSD(modelspp)$`big_pca$scientific`[,4]
    perspp_sig = TukeyHSD(modelspp)$`big_pca$scientific`[,4]
    perspp_sig[perspp_sig==0] = min(perspp_sig[perspp_sig!=0],na.rm=T)/10
    
    barplot(log10(perspp_sig)*-1,las=2)
    abline(h=log10(c(0.1,0.05,0.01,0.001))*-1,col=c("lightgrey","lightblue","blue","darkblue"))
    
    ## turn this into a heatmap
    perspp_sig_df=as.data.frame(cbind(perspp_sig))
    perspp_sig_df$comparison = rownames(perspp_sig_df)
    perspp_sig_df$spp1= sapply(perspp_sig_df$comparison,function(x){
      splits=strsplit(x,"-")[[1]][1]
    })
    perspp_sig_df$spp2= sapply(perspp_sig_df$comparison,function(x){
      splits=strsplit(x,"-")[[1]][2]
    })
    perspp_sig_df$log = log10(perspp_sig_df$perspp_sig)*-1
    sigdist=as.dist(xtabs(perspp_sig_df$perspp_sig ~ perspp_sig_df$spp1 + perspp_sig_df$spp2))
    siglogdist=as.dist(xtabs(perspp_sig_df$log ~ perspp_sig_df$spp1 + perspp_sig_df$spp2))
    corrplot::corrplot(as.matrix(siglogdist),is.corr=F,method="color"#,
                       #col=colorRampPalette(c("blue","yellow","red"))(200)
    )
    corrplot::corrplot(as.matrix(sigdist),is.corr=F,method="color")
    
    
    
    modelgen=aov(big_pca$PC1~big_pca$genus)
    summary(modelgen) ## sig
    pergen_sig = TukeyHSD(modelgen)$`big_pca$genus`[,4]
    pergen_sig[pergen_sig==0] = min(pergen_sig[pergen_sig!=0],na.rm=T)/10
    
    barplot(log10(pergen_sig)*-1,las=2)
    abline(h=log10(c(0.1,0.05,0.01,0.001))*-1,col=c("lightgrey","lightblue","blue","darkblue"))
    
    ## turn this into a heatmap
    pergen_sig_df=as.data.frame(cbind(pergen_sig))
    pergen_sig_df$comparison = rownames(pergen_sig_df)
    pergen_sig_df$spp1= sapply(pergen_sig_df$comparison,function(x){
      splits=strsplit(x,"-")[[1]][1]
    })
    pergen_sig_df$spp2= sapply(pergen_sig_df$comparison,function(x){
      splits=strsplit(x,"-")[[1]][2]
    })
    pergen_sig_df$log = log10(pergen_sig_df$pergen_sig)*-1
    sigdist=as.dist(xtabs(pergen_sig_df$pergen_sig ~ pergen_sig_df$spp1 + pergen_sig_df$spp2))
    siglogdist=as.dist(xtabs(pergen_sig_df$log ~ pergen_sig_df$spp1 + pergen_sig_df$spp2))
    corrplot::corrplot(as.matrix(siglogdist),is.corr=F,method="number",
                       col=colorRampPalette(c("blue","yellow","red"))(200))
    corrplot::corrplot(as.matrix(sigdist),is.corr=F,method="number")
  }
  raw_stats_file = paste("rvn.dat_trimmed_spectro_fcts_collapsed_",date,".txt",sep="")
  
  
}

## do we need to do discriminant function analysis? 
## http://www.sthda.com/english/articles/36-classification-methods-essentials/146-discriminant-analysis-essentials-in-r/



## warbler stuff with specan function
## params <- specan(Phae.hisnrt, bp = c(2, 10), threshold = 15)


## clustering: cluster within individuuals and then cluster between individuals? 

