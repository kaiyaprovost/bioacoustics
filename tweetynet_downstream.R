library(warbleR)
library(Rraven)
library(soundgen)
library(SoundShape)
library(hypervolume)

do_sumstats = F
do_soundshape = T

mainpath = "/Users/kprovost/OneDrive - The Ohio State University/BLB_Data/Cardinalidae/Cardinalis/Cardinalis_sinuatus/"
setwd(mainpath)

date=format(Sys.time(), "%d%b%Y")
#date="19may2021"

if(do_sumstats==T){
  
  filenames = list.files(path = getwd(), 
                         pattern = "Table.1.selections.txt$",full.names = T,recursive=T)
  filenames = filenames[!(grepl("wav_",filenames))]
  
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
    for(path in (paths[1:length(paths)])) {
      setwd(path)
      if(verbose==T){print(path)}
      
      filenames2 = list.files(path = path, pattern = suffix,full.names = T,recursive=T)
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
                                  all.data = T,name.from.file = F,ext.case = "lower",
                                  path=path2,
                                  only.spectro.view = F,warbler.format = T,unread=F,recursive = F,files=NULL)})
            if(is.null(dat)){
              try({dat <- imp_raven(sound.file.col="Begin File",
                                    all.data = T,name.from.file = T,ext.case = "lower",
                                    path=path2,
                                    only.spectro.view = F,warbler.format = T,unread=F,recursive = F,files=NULL)})
            }
            return(dat)
          })
          
          rvn.dat=do.call(plyr::rbind.fill,dat_list)
          write.table(rvn.dat,outfile)
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
          write.table(rvn.dat,outfile)
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
        }
        
        ## remove sounds that are too short?
        rvn.dat=rvn.dat[rvn.dat$difference>=0.05,]
        
        
        if (dolength==T){
          if(verbose==T){print("length")}
          ## cound the ";" in the peakfreqcontour to generate the function determining how many length.out to get
          #rvn.dat$`Peak Freq Contour (Hz)`
          try({length.out=as.numeric(sapply(rvn.dat$Peak.Freq.Contour..Hz.,FUN=function(i){as.numeric(stringr::str_count(i,";"))},simplify = T))})
          try({length.out=as.numeric(sapply(rvn.dat$`Peak Freq Contour (Hz)`,FUN=function(i){as.numeric(stringr::str_count(i,";"))},simplify = T))})
          rvn.dat$length.out=length.out+1
          
          mod=lm(rvn.dat$length.out~rvn.dat$difference)
          predicted=predict(mod,rvn.dat[,c("difference","length.out")])
          
          rvn.dat$length.out[is.na(rvn.dat$length.out)] = round(predicted[is.na(rvn.dat$length.out)])
        }
        write.table(rvn.dat,trimfile)
      }
      
      
      #plot(rvn.dat$difference,rvn.dat$length.out)
      #points(rvn.dat$difference[is.na(rvn.dat$length.out)],ceiling(predicted[is.na(rvn.dat$length.out)]),col="red")
      
      #rvn.dat.st <- selection_table(rvn.dat, path="/Users/kprovost/Documents/BLB_Data/Cardinalis/Cardinalis_sinuatus",
      #                              whole.recs = T)
      
      ## generate some sumstats
      
      ## not sure why not working, worked before
      ##  Error in if (any(spec < 0)) stop("Data do not have to be in dB") : missing value where TRUE/FALSE needed 
      ## fixed by manually setting bp -- in hz if raven format, in khz if warbler format
      
      ## need to do this directory-wise
      
      if(verbose==T){print("spectro")}
      specfile=paste("rvn.dat_trimmed_spectro_",date,".txt",sep="")
      if(file.exists(specfile)) { 
        rvn.dat.sp=read.table(specfile,header=T)  
      } else {
        
        sp_list = lapply(paths2,FUN=function(pathx){
          print(pathx)
          ## works on sin but not on card?
          sp <- spectro_analysis(X = rvn.dat,harmonicity = F,
                                 path=pathx,
                                 bp=c(0,22),fast=T) ## fast = 35 sec 28 cols, not fast = 3 min 15 sec
          ## harmonicity = T irrespective of fast causes a lot of errors and is estimated to take ~20 min
          return(sp)
        })
        
        sp = do.call(plyr::rbind.fill,sp_list)
        
        ## merge sumstats
        
        rvn.dat.sp = merge(rvn.dat,sp,all=T)
        #rvn.dat.sp = cbind(rvn.dat,sp[,3:ncol(sp)])
        write.table(rvn.dat.sp,specfile)
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
        if(loop==T) {
          y=lapply(1:nrow(rvn.dat),FUN=function(i){
            print(paste(i,"/",nrow(rvn.dat)))
            x=freq_ts(rvn.dat[i,c("selec","start","end","bottom.freq","top.freq","sound.files")],
                      length.out = 100,img=F,ff.method = "tuneR",raw.contour = F,type="dominant",
                      bp=c(0.5,10),clip.edges = T,threshold = 50)
          })
          fcts_dataframe = do.call(plyr::rbind.fill,y)
        } else {
          fcts_dataframe=freq_ts(rvn.dat[,c("selec","start","end","bottom.freq","top.freq","sound.files")],
                                 length.out = 100,img=F,ff.method = "tuneR",raw.contour = F,type="dominant",pb=T,
                                 bp=c(0.5,10),clip.edges = T,threshold = 50)
        }
        
        
        #fcts_dataframe[is.na(fcts_dataframe)] = 0
        
        #temp = rvn.dat[rvn.dat$sound.files=="BLB10213.wav" & rvn.dat$selec==1,]
        #a1 = fcts_dataframe[fcts_dataframe$sound.files=="BLB10213.wav" & fcts_dataframe$selec==1,]
        #a2 = fcts_unequal[fcts_unequal$sound.files=="BLB10213.wav" & fcts_unequal$selec==1 & fcts_unequal$PFC..1!=0 & !(is.na(fcts_unequal$PFC..1)),]
        #a3 = fcts_dataframe2[fcts_dataframe2$sound.files=="BLB10213.wav" & fcts_dataframe2$selec==1,]
        #a4 = fcts_dataframe3[fcts_dataframe3$sound.files=="BLB10213.wav" & fcts_dataframe3$selec==1,]
        #plot(as.numeric(a1[2,3:ncol(a1)]),type="l",ylim=c(0,6),xlim=c(0,60)) ## fund
        #points(as.numeric(a2[1,3:ncol(a2)])/1000,type="l",col="cyan",ylim=c(0,6),xlim=c(0,60)) ## raven
        
        ## the fundamental frequency matches raven although it is not perfect
        
        #fcts_dataframe_zeros = fcts_dataframe
        #fcts_dataframe_zeros[is.na(fcts_dataframe_zeros)] = 0
        #lot(fcts_dataframe$selec,rvn.dat.sp$selec)
        
        #rvn.dat.sp.fcts = cbind(rvn.dat.sp,fcts_dataframe[,3:ncol(fcts_dataframe)])
        
        rvn.dat.fcts=merge(rvn.dat,fcts_dataframe,all=T)
        #rvn.dat.fcts = cbind(rvn.dat,fcts_dataframe[,3:ncol(fcts_dataframe)])
        write.table(rvn.dat.fcts,fctsfile)
        
        ## create dissimilarity measures between frequency ranges, scaled and unscaled
        ## probably not helpful?
        # dissim=freq_DTW(ts.df = fcts_dataframe_zeros[1:10,],pb=T,scale=F,open.end = T,open.begin=T,img=F,clip.edges=T,
        #                 img.suffix=".image",X=rvn.dat,type="fundamental") ## 100 is about 10 seconds, exponentially grows
        # dissim_scale=freq_DTW(ts.df = fcts_dataframe_zeros[1:10,],pb=T,scale=T,open.end = T,open.begin=T,img=F,clip.edges=T,
        #                 img.suffix=".image",X=rvn.dat,type="fundamental") ## 100 is about 10 seconds, exponentially grows 
        # corrplot::corrplot(dissim,is.corr=F,method="color")
        
        #x=freq_ts(rvn.dat[1:46,],length.out = 100) ## just one took about 8 seconds with length.out=20, about the same with length.out = 100
        #plot(as.numeric(x[1,3:ncol(x)]))
        
        ## extract the data from rvn.dat if it is present
        # fcts_unequal <- extract_ts(X = rvn.dat, ts.column = "Peak Freq Contour (Hz)")
        # min(fcts_unequal[,3:ncol(fcts_unequal)],na.rm=T)
        # fcts_unequal[is.na(fcts_unequal)] = 0
        # 
        # fcts = extract_ts(X = rvn.dat, ts.column = "Peak Freq Contour (Hz)",equal.length = T,length.out = ncol(fcts_unequal)-2)
        # fcts=fcts[complete.cases(fcts),]
        # 
        # dissim=freq_DTW(ts.df = fcts_unequal[1:28,],pb=T,scale=F,open.end = T,open.begin=T,img=T,clip.edges=T,img.suffix=".image",X=rvn.dat) ## 100 is about 10 seconds
        # corrplot::corrplot(dissim,is.corr=F,method="color",order="hclust")
        # 
        # dissim2=freq_DTW(ts.df = fcts[1:28,],pb=T,scale=F,open.end = T,open.begin=T,img=T,clip.edges=T,img.suffix=".image",X=rvn.dat) ## 100 is about 10 seconds
        # corrplot::corrplot(dissim2,is.corr=F,method="color",order="hclust")
      }
      
      if(verbose==T){print("merge spectro fcts")}
      rvn.dat.sp.fcts = merge(rvn.dat.fcts,rvn.dat.sp,all=T)
      mergefile = paste("rvn.dat_trimmed_spectro_fcts_",date,".txt",sep="")
      write.table(rvn.dat.sp.fcts,mergefile)
      
      
      
      
    }
  }
  
  generate_sumstats(filenames,verbose=T)
  
  setwd(mainpath)
  
  combine_finished_substats = function(finished_files,date=format(Sys.time(), "%d%b%Y"),workingdirectory="~/",verbose=F){
    setwd(workingdirectory)
    ## get all of them combined 
    
    finishedfile=paste("rvn.dat.compiled_",date,".txt",sep="")
    if(file.exists(finishedfile)) {
      finished = read.table(finishedfile,header=T,fill=T,sep="\t")
      
    } else {
      if(verbose==T){print("LOOP")}
      fin_list = lapply(finished_files,FUN=function(x){
        fin = read.table(x,header=T,sep=" ",fill=T)
      })
      if(verbose==T){print("RBIND")}
      finished = do.call(plyr::rbind.fill,fin_list)
      if(verbose==T){print("UNIQ")}
      finished=unique(finished)
      if(verbose==T){print("WRITE")}
      write.table(finished,finishedfile,sep="\t",quote=F,row.names = F)
    }
    
    
    finishedtrimmedfile = paste("rvn.dat.compiled_trimmed_",date,".txt",sep="")
    if(file.exists(finishedtrimmedfile)) {
      finished = read.table(finishedtrimmedfile,header=T,sep="\t")
    } else {
      if(verbose==T){print("removing blanks and too shorts")}
      finished=finished[,colSums(is.na(finished))!=nrow(finished)]
      finished = finished[,colSums(is.na(finished))<= (0.5 * nrow(finished))]
      
      if(verbose==T){print("sorting")}
      sort(unique(rowSums(is.na(finished))))
      sort(unique(colSums(is.na(finished))))
      if(verbose==T){print("dedup")}
      finished=finished[which(rowSums(is.na(finished))<=1),]
      species=sapply(finished$selec.file,FUN=function(x){
        y=strsplit(x,"\\.")[[1]][1]
        y=strsplit(y,"-")[[1]]
        if(length(y)>1){
          y=paste(y[1:2],sep="_",collapse="_")
        }
        return(y)
      })
      finished$species = species
      
      genus = sapply(finished$species,FUN=function(x){
        return(strsplit(x,"_")[[1]][1])
      })
      finished$genus = genus
      finished$genus[finished$genus=="Zonotricha"] = "Zonotrichia"
      finished$species[finished$species=="Zonotricha_albicollis"] = "Zonotrichia_albicollis"
      
      family = sapply(finished$paths,FUN=function(x){
        if(grepl("XenoCanto",x)==T) {
          y = strsplit(x,"XenoCanto/")[[1]][2]
          y = strsplit(y,"/")[[1]]
          return(y[length(y)-1])
        } else {
          return(NA)
        }
      })
      
      finished$family = family
      finished$family[finished$genus=="Cardinalis"] = "Cardinalidae"
      finished$family[finished$genus=="Zonotrichia"] = "Passerellidae"
      
      
      write.table(finished,finishedtrimmedfile,sep="\t",quote=F,row.names = F)
      
    }
    
    
    pcafile = paste("rvn.dat.compiled_trimmed_PCA_",date,".txt",sep="")
    if(file.exists(pcafile)) {
      pca_data = read.table(pcafile,sep="\t",header=T)
    } else {
      
      
      
      metadata_cols = c("species","sound.files","selec","View","Channel","type","selec.file",
                        "paths","start","end","bottom.freq","top.freq","difference","genus","family")
      
      classes=sapply(finished,class)
      non_number_cols = colnames(finished)[!(classes=="numeric" | classes=="integer")]
      numvals = sapply(finished,FUN=function(x){length(unique(x))})
      nounique_cols = colnames(finished)[numvals<=1]
      missings = colSums(is.na(finished))!=0
      missing_cols = colnames(finished)[missings]
      
      metadata_cols = unique(c(metadata_cols,non_number_cols,nounique_cols,missing_cols))
      
      finished_pca = finished[,which(!(colnames(finished) %in% metadata_cols))]
      ## ONLY USE THE TABLE.1 FILES FOR THE PCA??? 
      pca_cor = cor(finished_pca,use="pairwise.complete.obs")
      png("testcorrplotpca.png",height=10,width=10,units="in",res=300)
      corrplot::corrplot(pca_cor,method="ellipse")
      dev.off()
      
      pca = prcomp(finished_pca,scale. = T,center=T)
      
      
      pca_data = cbind(finished[,which((colnames(finished) %in% metadata_cols))],finished_pca,pca$x[,1:10])
      
      write.table(pca_data,pcafile,sep="\t")
    }
    
    for(family in sort(unique(finished$family))) {
      print(family)
      pngfile=paste("temppca",family,".png",sep="")
      if(!(file.exists(pngfile))){
        png(pngfile)
        plot(pca_data$PC1,pca_data$PC2)
        points(pca_data$PC1[finished$family==family],pca_data$PC2[finished$family==family],col="red")
        dev.off()
      }
    }
    
    
    #unique(finished$species)
    if(!(file.exists("temppca2.png"))){
      png("temppca2.png")
      plot(pca_data$PC1,pca_data$PC2,
           bg=as.numeric(as.factor(pca_data$sound.files)),
           pch=as.numeric(pca_data$type)+21,cex=abs(as.numeric(pca_data$type)-2)/2)
      dev.off()
    }
  }
  
  finished_files = list.files(path=getwd(),pattern=paste("rvn.dat_trimmed_spectro_fcts_",#date,".txt",
                                                         sep=""),
                              full.names = T,recursive = T)
  
  combine_finished_substats(finished_files,verbose=T,workingdirectory=mainpath,date=date)
  
  # filenames2 = list.files(path = "/Users/kprovost/Documents", pattern = "annot.csv.txt$",full.names = T,recursive=T)
  # 
  # tochange=sapply(filenames2,FUN = function(x){
  #   y=strsplit((x),".wav-")[[1]][1]
  #   z=paste(y,".selections.txt",sep="")
  #   return(z)
  # })
  # 
  # file.rename(filenames2,tochange)
  
  
  
  
  #####
  ## do individual differences or locality differences and stuff
  
  setwd("/Users/kprovost/OneDrive - The Ohio State University/BLB_Data/")
  #df = read.table("/Users/kprovost/OneDrive - The Ohio State University/BLB_Data/rvn.dat.compiled_trimmed_PCA_19may2021.txt",header=T,fill=T,sep="\t")
  df2 = df[df$sound.files=="BLB5541.wav",]
  df2=df2[grepl("Table.1",df2$selec.file),]
  df2=df2[order(df2$selec),] 
  df2 = unique(df2)
  
  plot(df2$PC1,df2$PC2)
  plot(df2$time.IQR,df2$freq.IQR,type="n")
  text(df2$time.IQR,df2$freq.IQR,df2$selec)
  
  plot(df2$selec,df2$time.IQR)
  
  ## freq_dtw?
  # scale_dif = freq_DTW(ts.df=df2[,c("sound.files","selec","start","end",paste("ffreq",seq(1,100),sep="."))],
  #          scale = T)
  # raw_dif = freq_DTW(ts.df=df2[,c("sound.files","selec","start","end",paste("ffreq",seq(1,100),sep="."))],
  #                      scale = F)
  # 
  # ## can we calculate distance relative to where they are in the song?
  # 
  # x=lapply(1:nrow(scale_dif),FUN=function(i){
  #   row = scale_dif[i,]
  #   difference=c()
  #   distance=c()
  #   for(j in i:length(row)) {
  #     difference = c(difference,row[j])
  #     distance = c(distance,i-j)
  #   }
  #   return(cbind(difference,distance))
  # })
  # y=do.call(rbind,x)
  # plot(abs(y[,2]),y[,1])
  # mod=lm(y[,1]~y[,2])
  # abline(mod,col="red")
  # 
  # summary(y[y[,2]!=0,1])
  # 
  # plot(as.numeric(scale_dif),as.numeric(raw_dif),xlab="shape difference",ylab="shape+freq difference")
  
  plot(as.numeric(df2[1,paste("ffreq",seq(1,100),sep=".")]),type="l",col="black")
  points(as.numeric(df2[2,paste("ffreq",seq(1,100),sep=".")]),type="l",col="red")
  points(as.numeric(df2[6,paste("ffreq",seq(1,100),sep=".")]),type="l",col="cyan")
}
## try out soundshape

if(do_soundshape==T){

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
}


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
}

run_soundshape(path=path,pattern=pattern,doplot=F,verbose=T,listfiles=listfiles,outpath="~/Documents/",
               overwrite=F,checkpointNumber=6316,redo_eig=T,alignCheckpoint=513037,eigcheckpoint=46226,shuffle=F,rev=T)
}

pca_to_keep = 3
pcaxF = as.data.frame(data.table::fread("/Users/kprovost/Documents/Zonotrichia_pca_soundshape_SCALEFALSE.txt",sep=" "))
rownames(pcaxF) = pcaxF$V1
pcaxF=pcaxF[,-1]

strsplits <- function(x, splits, ...) {
  for (split in splits)
  {
    x <- unlist(strsplit(x, split, ...))
  }
  return(x[!x == ""]) # Remove empty values
}

spplist = do.call(rbind,lapply(rownames(pcaxF),FUN=function(x){
  y=strsplits(x,c("\\."))
  if(length(y)==2){
    z=cbind("",y[1])
  } else if(length(y)>=3){
    z=cbind(y[1],y[2])
  }
  return(z)
}))
colnames(spplist) = c("Scientific","COLLECTING_UNIT_ID")
pcaxF = cbind(pcaxF,spplist)

pcax = pcaxF[,c("Scientific","COLLECTING_UNIT_ID",paste("PC",1:pca_to_keep,sep=""))]
pcax$Scientific = sub("\\s","_",pcax$Scientific)

for (i in 1:nrow(pcax)) {
  if(pcax$COLLECTING_UNIT_ID[i]=="mp3") {
    y=strsplits(pcax$Scientific[i],"-")
    pcax$COLLECTING_UNIT_ID[i] = paste("XC",y[3],sep="")
    pcax$Scientific[i] = paste(y[1:2],collapse="_")
  }
}



metadata = read.table("/Users/kprovost/OneDrive - The Ohio State University/BLB_Data/Spreadsheets/BLB_klp copy.csv",
                      sep=",",
                      header=T, fill=T)
#metadata = metadata[metadata$GENUS_NAME=="Cardinalis",]
metadata$Scientific = sub("\\s","_",metadata$SCIENTIFIC_NAME)
metadata = metadata[,c("COLLECTING_UNIT_ID","Scientific","SUBSPECIES_EPITHET","YEAR_IDENTIFIED","STATE_PROVINCE","LONGITUDE","LATITUDE")]
colnames(metadata) = c("COLLECTING_UNIT_ID","Scientific","Subspecies","YEAR_IDENTIFIED","STATE_PROVINCE","LONGITUDE","LATITUDE")

metadataxc = as.data.frame(data.table::fread("/Users/kprovost/OneDrive - The Ohio State University/XenoCanto/xenocanto_fullrecords_upto_628187.txt",
                                             sep="\t",fill=T))
metadataxc$Scientific = paste(metadataxc$Genus,metadataxc$Specific_epithet,sep="_")
metadataxc = metadataxc[,c("Recording_ID","Scientific","Subspecies","Date","Locality","Latitude","Longitude")]
metadataxc$year=sapply(metadataxc$Date,FUN=function(x){strsplit(x,"-")[[1]][1]})
metadataxc$state=sapply(metadataxc$Locality,FUN=function(x){
  y=strsplit(x,",")[[1]]
  z = trimws(y[length(y)])
  return(z)
})
metadataxc$year = as.numeric(metadataxc$year)
metadataxc$state = as.character(metadataxc$state)


full = merge(pcax,metadata,by="COLLECTING_UNIT_ID",all.x=T)

metadataxc = metadataxc[,c("Recording_ID","Scientific","Subspecies","year","state","Latitude","Longitude")]
colnames(metadataxc) = c("COLLECTING_UNIT_ID","Scientific","Subspecies","YEAR_IDENTIFIED","STATE_PROVINCE","LATITUDE","LONGITUDE")
metadataxc$COLLECTING_UNIT_ID = paste("XC",metadataxc$COLLECTING_UNIT_ID,sep="")

full=merge(full,metadataxc,by="COLLECTING_UNIT_ID",all.x=T)

full$Scientific.x[full$Scientific.x=="" | is.na(full$Scientific.x)] = full$Scientific.y[full$Scientific.x=="" | is.na(full$Scientific.x)]
full$Scientific.x[full$Scientific.x=="" | is.na(full$Scientific.x)] = full$Scientific[full$Scientific.x=="" | is.na(full$Scientific.x)]
full$Scientific.x = sub("\\s","_",full$Scientific.x)

full$Subspecies.x[full$Subspecies.x=="" | is.na(full$Subspecies.x)] = full$Subspecies.y[full$Subspecies.x=="" | is.na(full$Subspecies.x)]
full$Subspecies.x = sub("\\s","_",full$Subspecies.x)

full$YEAR_IDENTIFIED.x[full$YEAR_IDENTIFIED.x=="" | is.na(full$YEAR_IDENTIFIED.x)] = full$YEAR_IDENTIFIED.y[full$YEAR_IDENTIFIED.x=="" | is.na(full$YEAR_IDENTIFIED.x)]
full$YEAR_IDENTIFIED.x = sub("\\s","_",full$YEAR_IDENTIFIED.x)

full$STATE_PROVINCE.x[full$STATE_PROVINCE.x=="" | is.na(full$STATE_PROVINCE.x)] = full$STATE_PROVINCE.y[full$STATE_PROVINCE.x=="" | is.na(full$STATE_PROVINCE.x)]
full$STATE_PROVINCE.x = sub("\\s","_",full$STATE_PROVINCE.x)

full$LONGITUDE.x[full$LONGITUDE.x=="" | is.na(full$LONGITUDE.x)] = full$LONGITUDE.y[full$LONGITUDE.x=="" | is.na(full$LONGITUDE.x)]
full$LONGITUDE.x = sub("\\s","_",full$LONGITUDE.x)
full$LATITUDE.x[full$LATITUDE.x=="" | is.na(full$LATITUDE.x)] = full$LATITUDE.y[full$LATITUDE.x=="" | is.na(full$LATITUDE.x)]
full$LATITUDE.x = sub("\\s","_",full$LATITUDE.x)

head(full)

full = full[,c("COLLECTING_UNIT_ID","Scientific.x","Subspecies.x","YEAR_IDENTIFIED.x","STATE_PROVINCE.x","LATITUDE.x","LONGITUDE.x",paste("PC",1:pca_to_keep,sep=""))]
sppx = unique(full$Scientific.x)
sppx=sppx[stringr::str_count(sppx,"_")>=2]
sppx=sppx[complete.cases(sppx)]
sppy=sapply(sppx,FUN=function(x){
  y=strsplit(x,"_")[[1]]
  z=paste(y[1:2],collapse="_")
  return(z)
})

for(i in 1:length(sppx)){
  full$Scientific.x[full$Scientific.x==sppx[i] & !(is.na(full$Scientific.x))] = sppy[i]
}

## convert state names to state abbreviations etc
for(i in 1:length(datasets::state.abb)) {
  abb = datasets::state.abb[i]
  #print(abb)
  state = datasets::state.name[i]
  #print(state)
  if (length(full$STATE[full$STATE==abb & !(is.na(full$STATE))])>0){
    full$STATE[full$STATE==abb & !(is.na(full$STATE))] = state
  }
}
full$STATE = sub("\\s","",full$STATE)
full$STATE = sub("_","",full$STATE)
full$STATE[full$STATE=="BC" & !(is.na(full$STATE))] = "BritishColumbia"
full$STATE[full$STATE=="SanDiegoCounty" & !(is.na(full$STATE))] = "California"

colnames(full) = c("ID","SCI","SUBSPP","YEAR","STATE","LAT","LON",paste("PC",1:pca_to_keep,sep=""))
full=full[,c("ID","SCI","SUBSPP","YEAR","STATE","LAT","LON",paste("PC",1:pca_to_keep,sep=""))]
write.table(full,"~/Zonotrichia_full_pcs_meta.txt")

full=read.table("~/Zonotrichia_full_pcs_meta.txt")

plot(full$PC1,full$PC2,col="grey",cex=0.1)
for(i in 1:length(unique(full$SUBSPP))){
  subspp=unique(full$SUBSPP)[i]
  print(i)
  print(palette()[i])
  print(subspp)
  subset=full[full$SUBSPP==subspp,c("PC1","PC2")]
  subset=subset[complete.cases(subset),]
  hpts <- chull(subset)
  hpts <- c(hpts, hpts[1])
  lines(subset[hpts, ],col=i)
}

plot(full$PC1,full$PC3,col="grey",cex=0.1)
for(i in 1:length(unique(full$SUBSPP))){
  subspp=unique(full$SUBSPP)[i]
  subset=full[full$SUBSPP==subspp,c("PC1","PC3")]
  subset=subset[complete.cases(subset),]
  hpts <- chull(subset)
  hpts <- c(hpts, hpts[1])
  lines(subset[hpts, ],col=i)
}

plot(full$PC2,full$PC3,col="grey",cex=0.1)
for(i in 1:length(unique(full$SUBSPP))){
  subspp=unique(full$SUBSPP)[i]
  subset=full[full$SUBSPP==subspp,c("PC2","PC3")]
  subset=subset[complete.cases(subset),]
  hpts <- chull(subset)
  hpts <- c(hpts, hpts[1])
  lines(subset[hpts, ],col=i)
}

## try hypervolume

#hv=hypervolume(full[,(1:pca_to_keep)+7])
## warning: some dimensions have much higher stdevs than others, consider rescaling axes before analysis 
#summary(hv)

#boxplot(full$PC1~full$SCI,las=2,col=2:6)
#boxplot(full$PC2~full$SCI,las=2,col=2:6)

hull3=geometry::convhulln(full[,(1:pca_to_keep)+7],output.options="FA")
fullarea=hull3$area
fullvol=hull3$vol

areadf=cbind("ALL","ALL",fullarea,1,1,nrow(full[,(1:pca_to_keep)+7]),1,1)
voldf=cbind("ALL","ALL",fullvol,1,1,nrow(full[,(1:pca_to_keep)+7]),1,1)

for(i in 1:length(unique(full$SCI))){
  sci = unique(full$SCI)[i]
  print(sci)
  data1 = full[full$SCI==sci,]
  data2 = full[full$SCI==sci,(1:pca_to_keep)+7]
  data2 = data2[complete.cases(data2),]
  
  if(nrow(data2)>4){
    ## 4 because need 4 to calculate the qhull
    hull3=geometry::convhulln(data2,output.options="FA")
    area2=hull3$area
    toadd2=cbind(sci,"ALL",area2,area2/fullarea,1,nrow(data2),nrow(data2)/nrow(full[,(1:pca_to_keep)+7]),1)
    areadf = rbind(areadf,toadd2)
    vol2=hull3$vol
    toadd2=cbind(sci,"ALL",vol2,vol2/fullvol,1,nrow(data2),nrow(data2)/nrow(full[,(1:pca_to_keep)+7]),1)
    voldf = rbind(voldf,toadd2)
  }
  
  for(j in 1:length(unique(data1$STATE))) {
    state = unique(data1$STATE)[j]
    print(state)
    if(is.na(state)) {
      data = data1[is.na(data1$STATE),(1:pca_to_keep)+7]
    } else {
      data = data1[data1$STATE==state & !(is.na(data1$STATE)),(1:pca_to_keep)+7]
    }
    
    data = data[complete.cases(data),]
    if(nrow(data)>4){
      hull3=geometry::convhulln(data,output.options="FA")
      area=hull3$area
      toadd=cbind(sci,state,area,area/fullarea,area/area2,nrow(data),nrow(data)/nrow(full[,(1:pca_to_keep)+7]),nrow(data)/nrow(data2))
      areadf = rbind(areadf,toadd)
      vol=hull3$vol
      toadd=cbind(sci,state,vol,vol/fullvol,vol/vol2,nrow(data),nrow(data)/nrow(full[,(1:pca_to_keep)+7]),nrow(data)/nrow(data2))
      voldf = rbind(voldf,toadd)
    }
  }
  
  for(k in 1:length(unique(data1$SUBSPP))) {
    subspp = unique(data1$SUBSPP)[k]
    print(subspp)
    if(is.na(subspp)) {
      data = data1[is.na(data1$SUBSPP),(1:pca_to_keep)+7]
    } else {
      data = data1[data1$SUBSPP==subspp & !(is.na(data1$SUBSPP)),(1:pca_to_keep)+7]
    }
    
    data = data[complete.cases(data),]
    if(nrow(data)>4){
      hull3=geometry::convhulln(data,output.options="FA")
      area=hull3$area
      toadd=cbind(sci,subspp,area,area/fullarea,area/area2,nrow(data),nrow(data)/nrow(full[,(1:pca_to_keep)+7]),nrow(data)/nrow(data2))
      areadf = rbind(areadf,toadd)
      vol=hull3$vol
      toadd=cbind(sci,subspp,vol,vol/fullvol,vol/vol2,nrow(data),nrow(data)/nrow(full[,(1:pca_to_keep)+7]),nrow(data)/nrow(data2))
      voldf = rbind(voldf,toadd)
    }
  }
  
}

areadf=as.data.frame(areadf)
voldf = as.data.frame(voldf)
colnames(areadf) = c("species","subset","area","relativearea","relativespparea","N","relativeN","relativesppN")
colnames(voldf) = c("species","subset","volume","relativevolume","relativesppvolume","N","relativeN","relativesppN")

alldf = merge(areadf,voldf,all=T)
for(i in 3:11){
  alldf[,i] = as.numeric(alldf[,i])
}

write.table(alldf,"~/Zonotrichia_pca_soundshape_3dconvexhull_per_species.txt")

## the area is proportional to N, so, should probably do a power analysis with bootstraps
## what is null expectation of area for a given species for a given N
permute_area = function(dataframe,proportion=NULL,to_sample=NULL,columns){
  df = dataframe[,columns]
  df = df[complete.cases(df),]
  if(is.null(to_sample) & is.null(proportion)){
    stop("ERROR: must specify only one of to_sample or proportion")
  }
  if(!(is.null(to_sample)) & !(is.null(proportion))){
    stop("ERROR: must specify only one of to_sample or proportion")
  }
  if(is.null(to_sample)) {
    to_sample = round(proportion*nrow(df))
  }
  if(is.null(proportion)) {
    proportion = nrow(df) / to_sample
  }
  newdf = df[sample(1:nrow(df),to_sample,replace=F),] ### 
  hull3=geometry::convhulln(newdf,output.options="FA")
  areavol = cbind(hull3$area,hull3$vol)
  colnames(areavol) = c("area","vol")
  return(areavol)
}

pdf("~/expected_permutation.pdf")
for(spp in unique(full$SCI)){
  if(!(is.na(spp))) {
    print(spp)
    small = full[full$SCI==spp,(1:pca_to_keep)+7]
    small = small[complete.cases(small),]
    hull3=geometry::convhulln(small,output.options="FA")
    fullarea=hull3$area
    fullvol=hull3$vol
    par(mfrow=c(2,1))
    permuted=lapply(1:10000,FUN=function(x){permute_area(dataframe=full,to_sample=nrow(small),columns=paste("PC",1:pca_to_keep,sep=""))}) ## about 6 seconds for 1000
    permuted2 = as.data.frame(do.call(rbind,permuted))
    hist(permuted2$area,xlim=c(min(permuted2$area,fullarea,na.rm=T),max(permuted2$area,fullarea,na.rm=T)),main=spp)
    abline(v=fullarea,col="red")
    hist(permuted2$vol,xlim=c(min(permuted2$vol,fullvol,na.rm=T),max(permuted2$vol,fullvol,na.rm=T)),main="")
    abline(v=fullvol,col="red")
  }
}
dev.off()


## check permutation for the zono subspp 
pdf("~/Zono_subspp_expected_permutation.pdf")
for (number in c(1240,1001,963,900,800,409)){
  permuted=lapply(1:1000,FUN=function(x){permute_area(dataframe=full,to_sample=number,columns=paste("PC",1:pca_to_keep,sep=""))}) ## about 6 seconds for 1000
  permuted2 = as.data.frame(do.call(rbind,permuted))
  pval_small = sum(permuted2$vol>=fullvol)/1000
  pval_big = sum(permuted2$vol<=fullvol)/1000
  hist(permuted2$vol,xlim=c(min(permuted2$vol,fullvol,na.rm=T),max(permuted2$vol,fullvol,na.rm=T)),main=paste("N:",number,"P(small):",pval_small,"P(big):",pval_big))
  abline(v=fullvol,col="red")
}
dev.off()

## zonotrichia raster stuff
library(raster)

## import a raster
bothbias=raster::raster("/Users/kprovost/OneDrive - The Ohio State University/XenoCanto/Sum-BLB-and-XC_songs.asc")
full$LAT=as.numeric(full$LAT)
full$LON=as.numeric(full$LON)
cells=raster::cellFromXY(bothbias,full[,c("LON","LAT")])
full$cells = cells

celldf=cbind("ALL",fullvol,nrow(full))

for(cell in unique(cells)){
  if(!(is.na(cell))) {
  subset = full[full$cells==cell,]
  subset = subset[,(1:pca_to_keep)+7]
  subset = subset[complete.cases(subset),]
  ## calculate the volume of this subset
  hullsub=geometry::convhulln(subset,output.options="FA")
  subsetvol=hullsub$vol
  celldf = rbind(celldf,cbind(cell,subsetvol,nrow(subset)))
  }
}

celldf = as.data.frame(celldf)
colnames(celldf) = c("cell","fullvol","N")
celldf$relvol = as.numeric(celldf$fullvol)/(as.numeric(celldf$fullvol[celldf$cell=="ALL"]))
relsongvol = bothbias
values(relsongvol) = 0
values(relsongvol)[is.na(values(bothbias))] = NA
blank=bothbias
values(blank)[!(is.na(values(blank)))] = 0
values(bothbias)[values(bothbias)==0] = NA
relsongvol[as.numeric(celldf$cell[celldf$cell!="ALL"])] = as.numeric(celldf$relvol[celldf$cell!="ALL"])
relsongvol[relsongvol==0] = NA
plot(blank,ylim=c(0,90),xlim=c(-180,0),col="grey",legend=F)
plot(relsongvol,ylim=c(0,90),xlim=c(-180,0),col=plasma(10),add=T)


nsongs = bothbias
values(nsongs) = 0
values(nsongs)[is.na(values(bothbias))] = NA
nsongs[as.numeric(celldf$cell[celldf$cell!="ALL"])] = as.numeric(celldf$N[celldf$cell!="ALL"])
plot(nsongs,ylim=c(0,90),xlim=c(-180,0))


pdf("~/Zono_cells_expected_permutation.pdf")
for (number in as.numeric(unique(celldf$N))){
  permuted=lapply(1:1000,FUN=function(x){permute_area(dataframe=full,to_sample=number,columns=paste("PC",1:pca_to_keep,sep=""))}) ## about 6 seconds for 1000
  permuted2 = as.data.frame(do.call(rbind,permuted))
  pval_small = sum(permuted2$vol>=fullvol)/1000
  pval_big = sum(permuted2$vol<=fullvol)/1000
  hist(permuted2$vol,xlim=c(min(permuted2$vol,fullvol,na.rm=T),max(permuted2$vol,fullvol,na.rm=T)),main=paste("N:",number,"P(small):",pval_small,"P(big):",pval_big))
  abline(v=fullvol,col="red")
}
dev.off()

meantemp=raster("/Users/kprovost/OneDrive - The Ohio State University/WorldClim2.1_jan2020/bio_2-5m_bil/bio1_40.asc")
meanprec=raster("/Users/kprovost/OneDrive - The Ohio State University/WorldClim2.1_jan2020/bio_2-5m_bil/bio12_40.asc")

songenv = cbind(values(relsongvol),values(meantemp),values(meanprec),values(nsongs))
songenv = as.data.frame(songenv)
colnames(songenv) = c("relsongvol","meantemp","meanprec","N")
songenv = songenv[complete.cases(songenv),]
songenv = songenv[songenv$relsongvol!=0,]
cor(songenv)

par(mfrow=c(1,2))
plot(songenv$meantemp,songenv$relsongvol)
summary(lm(songenv$relsongvol~songenv$meantemp))
abline(lm(songenv$relsongvol~songenv$meantemp),col="red")
summary(lm(songenv$relsongvol~songenv$meantemp+songenv$N))

plot(songenv$meanprec,songenv$relsongvol)
summary(lm(songenv$relsongvol~songenv$meanprec))
summary(lm(songenv$relsongvol~songenv$meanprec+songenv$N))
abline(lm(songenv$relsongvol~songenv$meanprec),col="red")


## look at diversity in pca space 
plot(full$PC1,full$PC2,#col=as.numeric(as.factor(full$SUBSPP)),
     col="white")
for(i in 1:length(unique(full$SUBSPP))){
  subspp=unique(full$SUBSPP)[i]
  subset=full[full$SUBSPP==subspp,c("PC1","PC2")]
  subset=subset[complete.cases(subset),]
hpts <- chull(subset)
hpts <- c(hpts, hpts[1])
lines(subset[hpts, ],col=i)
}

## and compare to nucdiv
leu=raster("/Users/kprovost/OneDrive - The Ohio State University/Phylogatr_Data/Birds-phylogatr-results_7dec2020/just_zono/Zonotrichia-leucophrys/Zonotrichia-leucophrys-COI.afa_nucdiv.asc")
songgene = cbind(values(relsongvol),values(leu))
songgene = as.data.frame(songgene)
songgene = songgene[complete.cases(songgene),]

## diversity in time? 
full$decade = round(as.numeric(full$YEAR),-1)
full$YEAR=as.numeric(full$YEAR)
plot(full$YEAR,full$PC1)
summary(lm(full$PC1~full$YEAR))
summary(lm(full$PC2~full$YEAR))
summary(lm(full$PC3~full$YEAR))



plot(full$YEAR,full$PC2)
plot(full$YEAR,full$PC3)
cor(full[,c("YEAR","PC1","PC2","PC3")],use="pairwise.complete.obs")



plot(full$PC1,full$PC2,col=as.numeric(as.factor(full$decade)),)
plot(full$PC1,full$PC3,col=as.numeric(as.factor(full$decade)),)
plot(full$PC2,full$PC3,col=as.numeric(as.factor(full$decade)),)

yeardf=cbind("ALL",fullvol,nrow(full))
for(year in sort(unique(full$YEAR))){
  if(!(is.na(year))) {
    print(year)
    subset = full[full$YEAR==year,]
    subset = subset[,(1:pca_to_keep)+7]
    subset = subset[complete.cases(subset),]
    ## calculate the volume of this subset
    hullsub=geometry::convhulln(subset,output.options="FA")
    subsetvol=hullsub$vol
    yeardf = rbind(yeardf,cbind(year,subsetvol,nrow(subset)))
  }
}
yeardf = as.data.frame(yeardf)
colnames(yeardf) = c("year","volume","N")
yeardf$relvol = as.numeric(yeardf$volume)/(as.numeric(yeardf$volume[yeardf$year=="ALL"]))
plot(as.numeric(yeardf$year),as.numeric(yeardf$relvol))
summary(lm(as.numeric(yeardf$relvol)~as.numeric(yeardf$N)))

plot(as.numeric(yeardf$year),as.numeric(yeardf$N))


## permutation select closest N points?

hull3=geometry::convhulln(full[,(1:pca_to_keep)+7],output.options="FA")
fullarea=hull3$area
fullvol=hull3$vol
pdf("~/expected_permutation_proportion.pdf")
for(prop in seq(0.05,0.95,0.05)){
  print(prop)
  par(mfrow=c(2,1))
  
  
  
  permuted=lapply(1:1000,FUN=function(x){permute_area(dataframe=full,proportion=prop,columns=paste("PC",1:pca_to_keep,sep=""))}) ## about 6 seconds for 1000
  permuted2 = as.data.frame(do.call(rbind,permuted))
  hist(permuted2$area,xlim=c(0,fullarea),main=prop)
  
  
  
  
  
  
  abline(v=fullarea,col="red")
  hist(permuted2$vol,xlim=c(0,fullvol),main="")
  abline(v=fullvol,col="red")
}
dev.off()

gplot(full$PC1,full$PC2,col=as.numeric(as.factor(full$STATE)),
      pch=as.character(as.factor(full$STATE)))

par(mfrow=c(2,1))
plot(full$PC1,full$PC2,col=as.numeric(as.factor(full$SCI)),
     pch=as.character(as.factor(full$SCI)))

plot(full$PC1,full$PC2,type="n")

for(i in 1:length(unique(full$SCI))){
  sci = unique(full$SCI)[i]
  data = full[full$SCI==sci,c("PC1","PC2")]
  data=data[complete.cases(data),]
  ch=chull(data)
  hpts <- c(ch, ch[1])
  lines(data[hpts, ],col=i)
}
legend("topright",legend=unique(full$SCI),
       fill=1:length(unique(full$SCI)),bty="n")





