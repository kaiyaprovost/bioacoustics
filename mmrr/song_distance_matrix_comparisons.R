## TODO: functionize this 
library(raster)
library(usmap)
library(ggplot2)
library(tseries)
library(maps)
library(mapdata)
library(sp)
## distances vs ibd as a test framework for mmrr 
## this is for NOCA

## DO NOT USE CENTROIDS FOR FINAL VALUES -- ONLY TEMP VALUES
eachFolder=F
doCentroid=F
doMMRR=F
nummmrr=1000
doGene=T
doSong=T
doCorr=F
path="/Users/kprovost/Documents/Postdoc_Working/"
genefolder = "~/Documents/Postdoc_Working/Genetics/Birds-phylogatr-results_7dec2020/Aves/Passeriformes/Passerellidae/"
songfolder = "/Users/kprovost/Documents/Postdoc_Working/MMRR/DISTANCES/FINAL/"
setwd(path)

# MMRR performs Multiple Matrix Regression with Randomization analysis
# Y is a dependent distance matrix
# X is a list of independent distance matrices (with optional names)
MMRR<-function(Y,X,nperm=999,center=T,scale=T){
  #compute regression coefficients and test statistics
  nrowsY<-nrow(Y)
  y<-unfold(Y,center=center,scale=scale)
  if(is.null(names(X)))names(X)<-paste("X",1:length(X),sep="")
  Xmats<-sapply(X,FUN=function(x){unfold(x,center=center,scale=scale)})
  fit<-lm(y~Xmats)
  coeffs<-fit$coefficients
  summ<-summary(fit)
  r.squared<-summ$r.squared
  confint = confint(fit)
  
  tstat<-summ$coefficients[,"t value"]
  Fstat<-summ$fstatistic[1]
  tprob<-rep(1,length(tstat))
  Fprob<-1
  
  #perform permutations
  for(i in 1:nperm){
    rand<-sample(1:nrowsY)
    Yperm<-Y[rand,rand]
    yperm<-unfold(Yperm)
    fit<-lm(yperm~Xmats)
    summ<-summary(fit)
    Fprob<-Fprob+as.numeric(summ$fstatistic[1]>=Fstat)
    tprob<-tprob+as.numeric(abs(summ$coefficients[,"t value"])>=abs(tstat))
  }
  
  #return values
  tp<-tprob/(nperm+1)
  Fp<-Fprob/(nperm+1)
  names(r.squared)<-"r.squared"
  names(coeffs)<-c("Intercept",names(X))
  names(tstat)<-paste(c("Intercept",names(X)),"(t)",sep="")
  names(tp)<-paste(c("Intercept",names(X)),"(p)",sep="")
  names(Fstat)<-"F-statistic"
  names(Fp)<-"F p-value"
  #names(confint)<-"Confidence interval"
  
  return(list(r.squared=r.squared,
              coefficients=coeffs,
              tstatistic=tstat,
              tpvalue=tp,
              Fstatistic=Fstat,
              Fpvalue=Fp,
              Conf=confint))
}
# unfold converts the lower diagonal elements of a matrix into a vector
# unfold is called by MMRR
unfold<-function(X,center=T,scale=T){
  x<-vector()
  for(i in 2:nrow(X)) x<-c(x,X[i,1:i-1])
  x<-scale(x, center=center, scale=scale)  # Comment this line out if you wish to perform the analysis without standardizing the distance matrices! 
  return(x)
}
## output mmrr function
outputMMRR = function(genMat,Xmats,nummmrr,thislength,name_gene,scale=T,center=T){
  mmrr=NULL
  try({mmrr=MMRR(Y=as.matrix(genMat),X=Xmats,nperm=nummmrr,scale=scale,center=center)})
  if(is.null(mmrr)){
    mmrr_row=cbind(name_gene,thislength,NA,NA,NA,NA,NA)
  } else {
    mmrr_row=cbind(name_gene,thislength,
                   mmrr$r.squared,
                   mmrr$coefficients[2],
                   mmrr$coefficients[3],
                   mmrr$tpvalue[2],
                   mmrr$tpvalue[3],
                   mmrr$Fpvalue)
  }
  colnames(mmrr_row) = c("spp-gene","N","rsq",
                         paste(names(Xmats)[1],"_coef",sep=""),
                         paste(names(Xmats)[2],"_coef",sep=""),
                         paste(names(Xmats)[1],"_p",sep=""),
                         paste(names(Xmats)[2],"_p",sep=""),
                         "overall_p")
  return(mmrr_row)
}

## brief moment to get genetic distances 
if(doGene==T) {
  big_difference_df = data.frame()
  gene_outfile=paste("gene_distances_mmrr_",nummmrr,"_30Jan2023.txt",sep="")
  
  gene_dist_list = list.files(genefolder,pattern="DISTANCES.txt$",recursive=T,full.names = T)
  gene_dist_list = gene_dist_list[grepl("master.passerellidae",gene_dist_list)]
  
  dist_folder = "/Users/kprovost/Documents/Postdoc_Working/MMRR/DISTANCES/FINAL/"
  env_dist_list = list.files(dist_folder,pattern="GENETICS",recursive=T,full.names=T)
  ## GEOGRAPHY.adjusted
  geo_dist_list = env_dist_list[grepl("GEOGRAPHY.adjusted",env_dist_list)]
  ## HABITAT.PC1234
  hab_dist_list = env_dist_list[grepl("HABITAT.PC1234",env_dist_list)]
  ## WC.PC123
  clim_dist_list = env_dist_list[grepl("WC.PC123",env_dist_list)]
  ## anthromes.class.1940
  urb40_dist_list = env_dist_list[grepl("anthromes.class.1940",env_dist_list)]
  ## anthromes.class.2017
  urb17_dist_list = env_dist_list[grepl("anthromes.class.2017",env_dist_list)]
  urb17_dist_list = urb17_dist_list[!grepl("1940",urb17_dist_list)]
  ## anthromes.class.2017.1940.dif
  urbdif_dist_list = env_dist_list[grepl("anthromes.class.2017.1940.dif",env_dist_list)]
  ## TOPOGRAPHY.PC123
  topo_dist_list = env_dist_list[grepl("TOPOGRAPHY.PC123",env_dist_list)]
  
  
  ## loop over genes
  
  for(i in 1:length(gene_dist_list)) {
    gene = gene_dist_list[i]
    print(paste(i,basename(gene)))
    name_gene = basename(gene)
    gene_i = read.table(gene,header=T)
    taxon=strsplit(basename(gene),"\\.")[[1]]
    if(length(taxon)==7) {
      taxon=c("ALL","ALL") ## genus, species
    } else if (length(taxon)==9){
      taxon=c(taxon[7],"ALL") ## genus, species
    } else {
      taxon=c(taxon[7],taxon[9]) ## genus, species
    }
    
    ## the gene ones match based on the first part, "_
    gene_i=gene_i[order(rownames(gene_i)) , order(colnames(gene_i))]
    
    clim_i_list = clim_dist_list[grepl(taxon[1],clim_dist_list) & grepl(taxon[2],clim_dist_list)]
    hab_i_list = hab_dist_list[grepl(taxon[1],hab_dist_list) & grepl(taxon[2],hab_dist_list)]
    geo_i_list = geo_dist_list[grepl(taxon[1],geo_dist_list) & grepl(taxon[2],geo_dist_list)]
    urb40_i_list = urb40_dist_list[grepl(taxon[1],urb40_dist_list) & grepl(taxon[2],urb40_dist_list)]
    urb17_i_list = urb17_dist_list[grepl(taxon[1],urb17_dist_list) & grepl(taxon[2],urb17_dist_list)]
    urbdif_i_list = urbdif_dist_list[grepl(taxon[1],urbdif_dist_list) & grepl(taxon[2],urbdif_dist_list)]
    topo_i_list = topo_dist_list[grepl(taxon[1],topo_dist_list) & grepl(taxon[2],topo_dist_list)]
    
    ## check if there are more than one
    if(length(clim_i_list)!=1 | length(hab_i_list)!=1 | length(geo_i_list)!=1 |
       length(urb40_i_list)!=1 | length(urb17_i_list)!=1 | length(urbdif_i_list)!=1 |
       length(topo_i_list)!= 1){
      print("BAD BAD NOT GOOD")
    } else {
      print("good")
      
      print("read tables")
      clim_i = read.table(clim_i_list,header=T)
      hab_i = read.table(hab_i_list,header=T)
      geo_i = read.table(geo_i_list,header=T)
      urb17_i = read.table(urb17_i_list,header=T)
      urb40_i = read.table(urb40_i_list,header=T)
      urbdif_i = read.table(urbdif_i_list,header=T)
      topo_i = read.table(topo_i_list,header=T)
      
      print("sapply ids")
      clim_ids=sapply(colnames(clim_i),FUN=function(x){strsplit(x,"\\.")[[1]][5]})
      hab_ids=sapply(colnames(hab_i),FUN=function(x){strsplit(x,"\\.")[[1]][5]})
      geo_ids=sapply(colnames(geo_i),FUN=function(x){strsplit(x,"\\.")[[1]][5]})
      urb17_ids=sapply(colnames(urb17_i),FUN=function(x){strsplit(x,"\\.")[[1]][5]})
      urb40_ids=sapply(colnames(urb40_i),FUN=function(x){strsplit(x,"\\.")[[1]][5]})
      urbdif_ids=sapply(colnames(urbdif_i),FUN=function(x){strsplit(x,"\\.")[[1]][5]})
      topo_ids=sapply(colnames(topo_i),FUN=function(x){strsplit(x,"\\.")[[1]][5]})
      
      print("rowcols")
      rownames(clim_i) = clim_ids
      colnames(clim_i) = clim_ids
      rownames(hab_i) = hab_ids
      colnames(hab_i) = hab_ids
      rownames(geo_i) = geo_ids
      colnames(geo_i) = geo_ids
      rownames(urb17_i) = urb17_ids
      rownames(urb40_i) = urb40_ids
      rownames(urbdif_i) = urbdif_ids
      rownames(topo_i) = topo_ids
      colnames(urb17_i) = urb17_ids
      colnames(urb40_i) = urb40_ids
      colnames(urbdif_i) = urbdif_ids
      colnames(topo_i) = topo_ids
      
      print("matches")
      matches = intersect(rownames(clim_i),rownames(gene_i))
      matches = intersect(rownames(hab_i),matches)
      matches = intersect(rownames(geo_i),matches)
      matches = intersect(rownames(urb17_i),matches)
      matches = intersect(rownames(urb40_i),matches)
      matches = intersect(rownames(urbdif_i),matches)
      matches = intersect(rownames(topo_i),matches)
      
      print("makemats")
      ecoMat = clim_i[which(matches %in% clim_ids),which(matches %in% clim_ids)]
      habMat = hab_i[which(matches %in% hab_ids),which(matches %in% hab_ids)]
      geoMat = geo_i[which(matches %in% geo_ids),which(matches %in% geo_ids)]
      genMat = gene_i[which(matches %in% rownames(gene_i)),which(matches %in% colnames(gene_i))]
      urb17Mat = urb17_i[which(matches %in% urb17_ids),which(matches %in% urb17_ids)]
      urb40Mat = urb40_i[which(matches %in% urb40_ids),which(matches %in% urb40_ids)]
      urbdifMat = urbdif_i[which(matches %in% urbdif_ids),which(matches %in% urbdif_ids)]
      topoMat = topo_i[which(matches %in% topo_ids),which(matches %in% topo_ids)]
      
      print("setmats")
      #Xmats <- list(geography=as.matrix(geoMat),ecology=as.matrix(ecoMat),habitat=as.matrix(habMat))
      Xmats_GE <- list(geography=as.matrix(geoMat),ecology=as.matrix(ecoMat))
      Xmats_GH <- list(geography=as.matrix(geoMat),habitat=as.matrix(habMat))
      Xmats_G17 <- list(geography=as.matrix(geoMat),urb2017=as.matrix(urb17Mat))
      Xmats_G40 <- list(geography=as.matrix(geoMat),urb1940=as.matrix(urb40Mat))
      Xmats_GD <- list(geography=as.matrix(geoMat),urbdif=as.matrix(urbdifMat))
      Xmats_GT <- list(geography=as.matrix(geoMat),topography=as.matrix(topoMat))
      #Xmats_EH <- list(ecology=as.matrix(ecoMat),habitat=as.matrix(habMat))
      #Xmats_G <- list(geography=as.matrix(geoMat))
      #Xmats_E <- list(ecology=as.matrix(ecoMat))
      #Xmats_H <- list(habitat=as.matrix(habMat))
      
      newrow=cbind("X","Y")
      if(sum(ecoMat!=0,na.rm=T)>0){
        print("outputs 1")
        output_1=outputMMRR(genMat=genMat,Xmats=Xmats_GE,nummmrr=nummmrr,
                            thislength=length(Xmats_GE),name_gene=name_gene,scale=T,center=T)
        newrow=gtools::smartbind(newrow,output_1)
      }
      if(sum(habMat!=0,na.rm=T)>0){
        print("outputs 2")
        output_2=outputMMRR(genMat=genMat,Xmats=Xmats_GH,nummmrr=nummmrr,
                            thislength=length(Xmats_GH),name_gene=name_gene,scale=T,center=T)
        newrow=gtools::smartbind(newrow,output_2)
      }
      
      if(sum(urb17Mat!=0,na.rm=T)>0){
        print("outputs 3")
        output_3=outputMMRR(genMat=genMat,Xmats=Xmats_G17,nummmrr=nummmrr,
                            thislength=length(Xmats_G17),name_gene=name_gene,scale=T,center=T)    
        newrow=gtools::smartbind(newrow,output_3)
      }
      if(sum(urb40Mat!=0,na.rm=T)>0){
        print("outputs 4")
        output_4=outputMMRR(genMat=genMat,Xmats=Xmats_G40,nummmrr=nummmrr,
                            thislength=length(Xmats_G40),name_gene=name_gene,scale=T,center=T)    
        newrow=gtools::smartbind(newrow,output_4)
      }
      if(sum(urbdifMat!=0,na.rm=T)>0){
        print("outputs 5")
        output_5=outputMMRR(genMat=genMat,Xmats=Xmats_GD,nummmrr=nummmrr,
                            thislength=length(Xmats_GD),name_gene=name_gene,scale=T,center=T)  
        newrow=gtools::smartbind(newrow,output_5)
      }
      if(sum(topoMat!=0,na.rm=T)>0){
        print("outputs 6")
        output_6=outputMMRR(genMat=genMat,Xmats=Xmats_GT,nummmrr=nummmrr,
                            thislength=length(Xmats_GT),name_gene=name_gene,scale=T,center=T) 
        newrow=gtools::smartbind(newrow,output_6)
      }
      
      print("write")
      write.table(newrow,gene_outfile,row.names = F,quote = F,append=T)
      big_difference_df = gtools::smartbind(big_difference_df,newrow)
      
    }
    
  }
  
  setwd(genefolder)
  write.table(big_difference_df,gene_outfile,row.names = F,quote = F,append=T)
  
}

## do the same thing for the song distances
if(doSong==T) {
  big_difference_df = data.frame()
  song_outfile=paste("song_distances_mmrr_",nummmrr,"_30Jan2023.txt",sep="")
  
  song_dist_list = list.files(songfolder,pattern="DISTANCES.txt$",recursive=T,full.names = T)
  song_dist_list = song_dist_list[grepl("master.passerellidae",song_dist_list)]
  
  dist_folder = "/Users/kprovost/Documents/Postdoc_Working/MMRR/DISTANCES/FINAL/"
  env_dist_list = list.files(dist_folder,pattern="SONG",recursive=T,full.names=T)
  ## GEOGRAPHY.adjusted
  geo_dist_list = env_dist_list[grepl("GEOGRAPHY.adjusted",env_dist_list)]
  ## HABITAT.PC1234
  hab_dist_list = env_dist_list[grepl("HABITAT.PC1234",env_dist_list)]
  ## WC.PC123
  clim_dist_list = env_dist_list[grepl("WC.PC123",env_dist_list)]
  ## anthromes.class.1940
  urb40_dist_list = env_dist_list[grepl("anthromes.class.1940",env_dist_list)]
  ## anthromes.class.2017
  urb17_dist_list = env_dist_list[grepl("anthromes.class.2017",env_dist_list)]
  urb17_dist_list = urb17_dist_list[!grepl("1940",urb17_dist_list)]
  ## anthromes.class.2017.1940.dif
  urbdif_dist_list = env_dist_list[grepl("anthromes.class.2017.1940.dif",env_dist_list)]
  ## TOPOGRAPHY.PC123
  topo_dist_list = env_dist_list[grepl("TOPOGRAPHY.PC123",env_dist_list)]
  
  
  ## loop over songs
  
  for(i in 1:length(song_dist_list)) {
    song = song_dist_list[i]
    print(paste(i,basename(song)))
    name_song = basename(song)
    song_i = read.table(song,header=T)
    taxon=strsplit(basename(song),"\\.")[[1]]
    if(length(taxon)==7) {
      taxon=c("ALL","ALL") ## genus, species
    } else if (length(taxon)==9){
      taxon=c(taxon[7],"ALL") ## genus, species
    } else {
      taxon=c(taxon[7],taxon[9]) ## genus, species
    }
    
    ## the song ones match based on the first part, "_
    song_i=song_i[order(rownames(song_i)) , order(colnames(song_i))]
    
    clim_i_list = clim_dist_list[grepl(taxon[1],clim_dist_list) & grepl(taxon[2],clim_dist_list)]
    hab_i_list = hab_dist_list[grepl(taxon[1],hab_dist_list) & grepl(taxon[2],hab_dist_list)]
    geo_i_list = geo_dist_list[grepl(taxon[1],geo_dist_list) & grepl(taxon[2],geo_dist_list)]
    urb40_i_list = urb40_dist_list[grepl(taxon[1],urb40_dist_list) & grepl(taxon[2],urb40_dist_list)]
    urb17_i_list = urb17_dist_list[grepl(taxon[1],urb17_dist_list) & grepl(taxon[2],urb17_dist_list)]
    urbdif_i_list = urbdif_dist_list[grepl(taxon[1],urbdif_dist_list) & grepl(taxon[2],urbdif_dist_list)]
    topo_i_list = topo_dist_list[grepl(taxon[1],topo_dist_list) & grepl(taxon[2],topo_dist_list)]
    
    ## check if there are more than one
    if(length(clim_i_list)!=1 | length(hab_i_list)!=1 | length(geo_i_list)!=1 |
       length(urb40_i_list)!=1 | length(urb17_i_list)!=1 | length(urbdif_i_list)!=1 |
       length(topo_i_list)!= 1){
      print("BAD BAD NOT GOOD")
    } else {
      print("good")
      
      print("read tables")
      clim_i = read.table(clim_i_list,header=T)
      hab_i = read.table(hab_i_list,header=T)
      geo_i = read.table(geo_i_list,header=T)
      urb17_i = read.table(urb17_i_list,header=T)
      urb40_i = read.table(urb40_i_list,header=T)
      urbdif_i = read.table(urbdif_i_list,header=T)
      topo_i = read.table(topo_i_list,header=T)
      
      print("sapply ids")
      clim_ids=sapply(colnames(clim_i),FUN=function(x){strsplit(x,"\\.")[[1]][5]})
      hab_ids=sapply(colnames(hab_i),FUN=function(x){strsplit(x,"\\.")[[1]][5]})
      geo_ids=sapply(colnames(geo_i),FUN=function(x){strsplit(x,"\\.")[[1]][5]})
      urb17_ids=sapply(colnames(urb17_i),FUN=function(x){strsplit(x,"\\.")[[1]][5]})
      urb40_ids=sapply(colnames(urb40_i),FUN=function(x){strsplit(x,"\\.")[[1]][5]})
      urbdif_ids=sapply(colnames(urbdif_i),FUN=function(x){strsplit(x,"\\.")[[1]][5]})
      topo_ids=sapply(colnames(topo_i),FUN=function(x){strsplit(x,"\\.")[[1]][5]})
      
      print("rowcols")
      rownames(clim_i) = clim_ids
      colnames(clim_i) = clim_ids
      rownames(hab_i) = hab_ids
      colnames(hab_i) = hab_ids
      rownames(geo_i) = geo_ids
      colnames(geo_i) = geo_ids
      rownames(urb17_i) = urb17_ids
      rownames(urb40_i) = urb40_ids
      rownames(urbdif_i) = urbdif_ids
      rownames(topo_i) = topo_ids
      colnames(urb17_i) = urb17_ids
      colnames(urb40_i) = urb40_ids
      colnames(urbdif_i) = urbdif_ids
      colnames(topo_i) = topo_ids
      
      print("matches")
      matches = intersect(rownames(clim_i),rownames(song_i))
      matches = intersect(rownames(hab_i),matches)
      matches = intersect(rownames(geo_i),matches)
      matches = intersect(rownames(urb17_i),matches)
      matches = intersect(rownames(urb40_i),matches)
      matches = intersect(rownames(urbdif_i),matches)
      matches = intersect(rownames(topo_i),matches)
      
      print("makemats")
      ecoMat = clim_i[which(matches %in% clim_ids),which(matches %in% clim_ids)]
      habMat = hab_i[which(matches %in% hab_ids),which(matches %in% hab_ids)]
      geoMat = geo_i[which(matches %in% geo_ids),which(matches %in% geo_ids)]
      genMat = song_i[which(matches %in% rownames(song_i)),which(matches %in% colnames(song_i))]
      urb17Mat = urb17_i[which(matches %in% urb17_ids),which(matches %in% urb17_ids)]
      urb40Mat = urb40_i[which(matches %in% urb40_ids),which(matches %in% urb40_ids)]
      urbdifMat = urbdif_i[which(matches %in% urbdif_ids),which(matches %in% urbdif_ids)]
      topoMat = topo_i[which(matches %in% topo_ids),which(matches %in% topo_ids)]
      
      print("setmats")
      #Xmats <- list(geography=as.matrix(geoMat),ecology=as.matrix(ecoMat),habitat=as.matrix(habMat))
      Xmats_GE <- list(geography=as.matrix(geoMat),ecology=as.matrix(ecoMat))
      Xmats_GH <- list(geography=as.matrix(geoMat),habitat=as.matrix(habMat))
      Xmats_G17 <- list(geography=as.matrix(geoMat),urb2017=as.matrix(urb17Mat))
      Xmats_G40 <- list(geography=as.matrix(geoMat),urb1940=as.matrix(urb40Mat))
      Xmats_GD <- list(geography=as.matrix(geoMat),urbdif=as.matrix(urbdifMat))
      Xmats_GT <- list(geography=as.matrix(geoMat),topography=as.matrix(topoMat))
      #Xmats_EH <- list(ecology=as.matrix(ecoMat),habitat=as.matrix(habMat))
      #Xmats_G <- list(geography=as.matrix(geoMat))
      #Xmats_E <- list(ecology=as.matrix(ecoMat))
      #Xmats_H <- list(habitat=as.matrix(habMat))
      
      newrow=cbind("X","Y")
      if(sum(ecoMat!=0,na.rm=T)>0){
        print("outputs 1")
        output_1=outputMMRR(genMat=genMat,Xmats=Xmats_GE,nummmrr=nummmrr,
                            thislength=length(Xmats_GE),name_song=name_song,scale=T,center=T)
        newrow=gtools::smartbind(newrow,output_1)
      }
      if(sum(habMat!=0,na.rm=T)>0){
        print("outputs 2")
        output_2=outputMMRR(genMat=genMat,Xmats=Xmats_GH,nummmrr=nummmrr,
                            thislength=length(Xmats_GH),name_song=name_song,scale=T,center=T)
        newrow=gtools::smartbind(newrow,output_2)
      }
      
      if(sum(urb17Mat!=0,na.rm=T)>0){
        print("outputs 3")
        output_3=outputMMRR(genMat=genMat,Xmats=Xmats_G17,nummmrr=nummmrr,
                            thislength=length(Xmats_G17),name_song=name_song,scale=T,center=T)    
        newrow=gtools::smartbind(newrow,output_3)
      }
      if(sum(urb40Mat!=0,na.rm=T)>0){
        print("outputs 4")
        output_4=outputMMRR(genMat=genMat,Xmats=Xmats_G40,nummmrr=nummmrr,
                            thislength=length(Xmats_G40),name_song=name_song,scale=T,center=T)    
        newrow=gtools::smartbind(newrow,output_4)
      }
      if(sum(urbdifMat!=0,na.rm=T)>0){
        print("outputs 5")
        output_5=outputMMRR(genMat=genMat,Xmats=Xmats_GD,nummmrr=nummmrr,
                            thislength=length(Xmats_GD),name_song=name_song,scale=T,center=T)  
        newrow=gtools::smartbind(newrow,output_5)
      }
      if(sum(topoMat!=0,na.rm=T)>0){
        print("outputs 6")
        output_6=outputMMRR(genMat=genMat,Xmats=Xmats_GT,nummmrr=nummmrr,
                            thislength=length(Xmats_GT),name_song=name_song,scale=T,center=T) 
        newrow=gtools::smartbind(newrow,output_6)
      }
      
      print("write")
      write.table(newrow,song_outfile,row.names = F,quote = F,append=T)
      big_difference_df = gtools::smartbind(big_difference_df,newrow)
      
    }
    
  }
  
  setwd(songfolder)
  write.table(big_difference_df,song_outfile,row.names = F,quote = F,append=T)
  
}










if(eachFolder==F){
  
  if(doCentroid==T){
    distdf = read.table("mean_centroid_distances_individuals.txt",
                        header=T,sep="\t",check.names = F,row.names = 1)
  } else {
    distdf = read.table("mean_syll_distance_inds_square.txt",
                        header=T,sep="\t",check.names = F,row.names = 1)
  }
  distdf = as.matrix(distdf)
  
  
  #metadf$ID2 = paste(metadf$COLLECTION,metadf$ID,sep=".") 
  ## may need to change this to match the filenames 
  row.names(distdf)
  metadf = metadf[metadf$ID2 %in% (row.names(distdf)),]
  metasmall = metadf[,c("ID2","LATITUDE","LONGITUDE")]
  colnames(metasmall) = c("ID","LATITUDE","LONGITUDE")
  metasmall=metasmall[complete.cases(metasmall),]
  metasmall=unique(metasmall)
  rownames(metasmall) = metasmall$ID
  metasmall = metasmall[order(as.character(metasmall$ID)),]
  
  if(!(file.exists("ibd_distances_per_individual.txt"))){
    IBD=as.matrix(dist(metasmall[,2:3],diag=T,upper=T))
    #write.table(as.matrix(IBD),"/Users/kprovost/Dropbox (AMNH)/Postdoc_Backup/Cardinalis_cardinalis/Subsets/ibd_distances_per_individual.txt",
    #            sep="\t",quote=F,row.names = T)
    write.table(as.matrix(IBD),"ibd_distances_per_individual.txt",
                sep="\t",quote=F,row.names = T)
  } else {
    IBD=read.table("ibd_distances_per_individual.txt",sep="\t",header=T)
  }
  
  if(!(file.exists("climate_distances_per_individual_allWC.txt"))){
    r <- getData("worldclim",var="bio",res=10)
    
    r2 = crop(r,extent(c(-130,-100,30,60)))
    r2pca=RStoolbox::rasterPCA(r2,spca = T)
    plot(r2pca$map[[1:3]])
    points(metasmall$LONGITUDE,metasmall$LATITUDE)
    
    
    #r <- r[[c(1,12)]]
    #names(r) <- c("Temp","Prec")
    points = extract(r,metasmall[,c("LONGITUDE","LATITUDE")])
    rownames(points) = metasmall$ID
    write.table(points,"~/raw_climate.txt")
    
    points2 = points[complete.cases(points),]
    pca = prcomp(points2,scale. = T,center = T)
    points2 = cbind(points2,pca$x)
    write.table(points2,"~/raw_climate_pca.txt")
    for (i in 8:45){
      print(colnames(df)[i])
      print(t.test(df[df$outlier==1,i],df[df$outlier==0,i])$p.value)
    }
    
    climDist = dist(points,diag=T,upper = T)
    write.table(as.matrix(climDist),"climate_distances_per_individual_allWC.txt",
                sep="\t",quote=F,row.names = T)
  } else {
    climDist = read.table("climate_distances_per_individual_allWC.txt",sep="\t",
                          header=T)
  }
  
  if(doCorr==T){
    par(mfrow=c(3,1))
    corrplot::corrplot(as.matrix(IBD),is.corr = F,method="color",
                       order="alphabet",main="geo")
    corrplot::corrplot(as.matrix(distdf),is.corr = F,method="color",
                       order="alphabet",main="song")
    corrplot::corrplot(as.matrix(climDist),is.corr = F,method="color",
                       order="alphabet",main="eco")
    
    par(mfrow=c(1,1))
    data=cbind(log10(c(IBD)),c(IBD),c(distdf),unlist(c(climDist)))
    data[is.infinite(data)]=-2
    colnames(data) = c("logIBD","IBD","SongDist","ClimDist")
    data=as.data.frame(data)
    
    plot(data$logIBD,data$SongDist)
    mod=lm(data$SongDist~data$logIBD)
    abline(mod,col="red")
    summary(mod)
    mod2=lm(data$SongDist[data$logIBD>-2]~data$logIBD[data$logIBD>-2])
    abline(mod2,col="blue")
    summary(mod2)
    
    plot(data$IBD,data$SongDist)
    mod=lm(data$SongDist~data$IBD)
    abline(mod,col="red")
    summary(mod)
    mod2=lm(data$SongDist[data$IBD>0]~data$IBD[data$IBD>0])
    abline(mod2,col="blue")
    summary(mod2)
    
    plot(data$ClimDist,data$SongDist)
    mod=lm(data$SongDist~data$ClimDist)
    abline(mod,col="red")
    summary(mod)
    mod2=lm(data$SongDist[data$ClimDist>0]~data$ClimDist[data$ClimDist>0])
    abline(mod2,col="blue")
    summary(mod2)
    
    plot(data$IBD,data$ClimDist)
    mod=lm(data$ClimDist~data$IBD)
    abline(mod,col="red")
    summary(mod)
    mod2=lm(data$ClimDist[data$IBD>0]~data$IBD[data$IBD>0])
    abline(mod2,col="blue")
    summary(mod2)
    
    plot(data$ClimDist,data$IBD)
    mod=lm(data$IBD~data$ClimDist)
    abline(mod,col="red")
    summary(mod)
    mod2=lm(data$IBD[data$IBD>0]~data$ClimDist[data$IBD>0])
    abline(mod2,col="blue")
    summary(mod2)
  }
  
  ## MMRR from Wang 2013
  if(doMMRR==T){
    
    # Tutorial for data files gendist.txt, geodist.txt, and ecodist.txt
    
    # Read the matrices from files.
    # The read.matrix function requires {tseries} package to be installed and loaded.
    # If the files have a row as a header (e.g. column names), then specify 'header=TRUE', default is 'header=FALSE'.
    library(tseries)
    #genMat <- read.matrix("/Users/kprovost/Downloads/MMRRtutorial/gendist.txt")
    #geoMat <- read.matrix("/Users/kprovost/Downloads/MMRRtutorial/geodist.txt")
    #ecoMat <- read.matrix("/Users/kprovost/Downloads/MMRRtutorial/ecodist.txt")
    
    #genMat=read.matrix("/Users/kprovost/Dropbox (AMNH)/Postdoc_Backup/Zonotrichia_leucophrys/Subsets/mean_syll_distance_inds_square.txt",header=F,sep="\t",skip=1)
    #ecoMat=read.matrix("/Users/kprovost/Dropbox (AMNH)/Postdoc_Backup/Zonotrichia_leucophrys/Subsets/climate_distances_per_individual_allWC.txt",header=F,sep="\t",skip=1)
    #geoMat=read.matrix("/Users/kprovost/Dropbox (AMNH)/Postdoc_Backup/Zonotrichia_leucophrys/Subsets/ibd_distances_per_individual.txt",header=F,sep="\t",skip=1)
    if(doCentroid==T){
      genMat=read.table("mean_centroid_distances_individuals.txt",header=T,sep="\t",skip=0,check.names = F)
    } else {
      genMat=read.table("mean_syll_distance_inds_square.txt",header=T,sep="\t",skip=0,check.names = F)
      
    }
    ecoMat=read.table("climate_distances_per_individual_allWC.txt",header=T,sep="\t",skip=0,check.names = F)
    geoMat=read.table("ibd_distances_per_individual.txt",header=T,sep="\t",skip=0,check.names = F)
    
    ## shared ones
    keptcols=Reduce(intersect, list(rownames(ecoMat),rownames(genMat),rownames(geoMat),
                                    colnames(ecoMat),colnames(genMat),colnames(geoMat)))
    
    genMat = genMat[which(colnames(genMat) %in% keptcols),which(rownames(genMat) %in% keptcols)]
    ecoMat = ecoMat[which(colnames(ecoMat) %in% keptcols),which(rownames(ecoMat) %in% keptcols)]
    geoMat = geoMat[which(colnames(geoMat) %in% keptcols),which(rownames(geoMat) %in% keptcols)]
    
    if(nrow(genMat)!=ncol(genMat)){
      genMat = genMat[,-1]
    }
    if(nrow(ecoMat)!=ncol(ecoMat)){
      ecoMat = ecoMat[,-1]
    }
    if(nrow(geoMat)!=ncol(geoMat)){
      geoMat = geoMat[,-1]
    }
    
    ## remove rows/cols that are entirely NA 
    
    # Make a list of the explanatory (X) matrices.
    # Names are optional.  Order doesn't matter.
    # Can include more than two matrices, if desired.
    Xmats <- list(geography=as.matrix(geoMat),ecology=as.matrix(ecoMat))
    
    # Run MMRR function using genMat as the response variable and Xmats as the explanatory variables.
    # nperm does not need to be specified, default is nperm=999)
    mmrr=MMRR(as.matrix(genMat),Xmats,nperm=999)
    print(mmrr)
    
    ## for my NOCA results: rsq = 0.14. coeffs geo = 0.52, eco = -0.22. pvals geo = 0.009, eco = 0.11. overall p 0.011
    ## for TEMPORARY zono results: rsq = 0.0019. coeffs geo = 0.000839 eco = 0.043. pvls geo = 0.991 eco = 0.274. overall p 0.43
    ## for TEMPORARY noca results: rsq = 0.086. coeffs geo = 0.43, eco = -0.23. pvals geo = 0.021, eco = 0.010. overall p 0.038.
    ## for TEMPORARY sin results: rsq = 0.03. coeffs geo = 0.173, eco = -2.27. pvals geo = 0.39, eco - 0.34. overall p 0.557. 
    ## for TEMPORARY calypte results: rsq = 0.003. coeffs geo = 0.011, eco = 0.0059. pvals geo = 0.89, eco - 0.57. overall p 0.80. 
    ## for TEMPORARY empid results: rsq = 0.07. coeffs geo = -0.14, eco = -0.17. pvals geo = 0.27, eco - 0.27. overall p 0.12. 
    ## for TEMPORARY melozone results: rsq = 0.08. coeffs geo = 0.91, eco = -0.71. pvals geo = 0.25, eco - 0.46. overall p 0.49. 
  }
  
  
  
  #   
  # map("world",c("Canada","USA","Mexico"),
  #     xlim=c(-180,0),ylim=c(0,90))
  # points(metasmall[,3:2],col="red")
  # points(occdf$longitude,occdf$latitude,col="blue")
  # 
  # par(mfrow=c(2,2))
  # plot(ape::nj(as.matrix(distances)),type="phylogram",align.tip.label=T)
  # corrplot::corrplot(as.matrix(distances),is.corr=F,method="color",order="alphabet")
  # 
  # h <- pegas::haplotype(data,strict=F)
  # plot(h)
  # occdf$haplo1 = 0
  # ind.hap<-with(
  #   utils::stack(setNames(attr(h, "index"), rownames(h))),
  #   table(hap=ind, pop=rownames(data)[values])
  # )
  # #net <- pegas::haploNet(h,getProb = F)
  # #plot(net, size=attr(net, "freq"), scale.ratio=0.2, pie=ind.hap)
  # #legend(-8, 0, colnames(ind.hap), col=rainbow(ncol(ind.hap)), pch=19, ncol=2)
  # haps=as.matrix(t(ind.hap))
  # haps=as.data.frame(haps)
  # haps=haps[haps$Freq!=0,]
  # occdf = merge(occdf,haps[,1:2])
  # map("world",c("Canada","USA","Mexico"),
  #     xlim=c(-180,0),ylim=c(0,90))
  # points(occdf$longitude,occdf$latitude,col=as.numeric(as.factor(occdf$hap)),
  #      pch=as.numeric(as.factor(occdf$hap)))
  # 
  # data = ape::read.dna(gene2,format="fasta")
  # fasta_accessions = labels(data)
  # zono = occdf[occdf$pop %in% fasta_accessions,c("accession","latitude","longitude","gbif_id","pop","cells")]
  # total_nuc_div = pegas::nuc.div(data,variance=F,pairwise.deletion=T)
  # distances=ape::dist.gene(data,method="pairwise",pairwise.deletion = T,variance = T)
  # plot(ape::nj(as.matrix(distances)),type="phylogram",align.tip.label=T)
  # corrplot::corrplot(as.matrix(distances),is.corr=F,method="color",order="alphabet")
  
}

## ideas for how to match individuals and songs
## interpolate genetic distances over space somehow
## assign individual songs to their closest individual sequence 
} else {
  matrix_list = list.files(path=path,pattern="mean_centroid_distances_",full.names = T,recursive = F)
  matrix_list = matrix_list[!(grepl("tree",matrix_list))]
  x <- file.info(matrix_list)
  matrix_list=rownames(x[order(x$size),])
  #matrix_list = matrix_list[(grepl("genus",matrix_list))] ## genus, withsubspecies, scientific
  for(matrix_file in matrix_list[1:length(matrix_list)]){
    print(matrix_file)
    thispath = dirname(matrix_file)
    setwd(thispath)
    taxa = strsplit(basename(matrix_file),"_")[[1]]
    taxa = taxa[length(taxa)]
    taxa=sub(".txt","",taxa)
    print(taxa)
    try({
      print("LOAD SONG DIST")
      distdf = as.matrix(read.table(matrix_file))
      rownames(distdf) = sub(".selections.txt.temp","",rownames(distdf))
      colnames(distdf) = sub(".selections.txt.temp","",colnames(distdf))
      metasmall = metadf[metadf$ID2 %in% (row.names(distdf)),]
      metasmall = metasmall[,c("ID2","LATITUDE","LONGITUDE")]
      colnames(metasmall) = c("ID","LATITUDE","LONGITUDE")
      rownames(metasmall) = metasmall$ID
      metasmall = metasmall[order(as.character(metasmall$ID)),]
      
      print("LOAD IBD")
      IBDfilename=paste("ibd_distances_per_individual_",taxa,".txt",sep="")
      if(!(file.exists(IBDfilename))){
        IBD=as.matrix(dist(metasmall[,2:3],diag=T,upper=T))
        write.table(as.matrix(IBD),IBDfilename,
                    sep="\t",quote=F,row.names = T)
      } else {
        IBD=read.table(IBDfilename,sep="\t",header=T)
      }
      
      print("LOAD CLIM")
      climfilename=paste("climate_distances_per_individual_allWC_",taxa,".txt",sep="")
      if(!(file.exists(climfilename))){
        
        rasterfile = "~/Documents/Postdoc_Working/worldclim_bio_res10_stack.tif"
        if(file.exists(rasterfile)){
          r = raster::stack(rasterfile)
        } else {
          r <- raster::getData("worldclim",var="bio",res=10)
          raster::writeRaster(r,rasterfile,format="GTiff")
        }
        points = raster::extract(r,metasmall[,c("LONGITUDE","LATITUDE")])
        rownames(points) = metasmall$ID
        
        climDist = dist(points,diag=T,upper = T)
        write.table(as.matrix(climDist),climfilename,
                    sep="\t",quote=F,row.names = T)
      } else {
        climDist = read.table(climfilename,sep="\t",
                              header=T)
      }
      
      
      # Tutorial for data files gendist.txt, geodist.txt, and ecodist.txt
      
      # Read the matrices from files.
      # The read.matrix function requires {tseries} package to be installed and loaded.
      # If the files have a row as a header (e.g. column names), then specify 'header=TRUE', default is 'header=FALSE'.
      
      
      
      print("DO MMRR")
      genMat=as.matrix(distdf)
      ecoMat=as.matrix(climDist)
      geoMat=as.matrix(IBD)
      
      ## shared ones
      keptcols=Reduce(intersect, list(rownames(ecoMat),rownames(genMat),rownames(geoMat),
                                      colnames(ecoMat),colnames(genMat),colnames(geoMat)))
      
      genMat = genMat[which(colnames(genMat) %in% keptcols),which(rownames(genMat) %in% keptcols)]
      ecoMat = ecoMat[which(colnames(ecoMat) %in% keptcols),which(rownames(ecoMat) %in% keptcols)]
      geoMat = geoMat[which(colnames(geoMat) %in% keptcols),which(rownames(geoMat) %in% keptcols)]
      
      if(nrow(genMat)!=ncol(genMat)){
        genMat = genMat[,-1]
      }
      if(nrow(ecoMat)!=ncol(ecoMat)){
        ecoMat = ecoMat[,-1]
      }
      if(nrow(geoMat)!=ncol(geoMat)){
        geoMat = geoMat[,-1]
      }
      
      ## remove rows/cols that are entirely NA 
      
      # Make a list of the explanatory (X) matrices.
      # Names are optional.  Order doesn't matter.
      # Can include more than two matrices, if desired.
      Xmats <- list(geography=as.matrix(geoMat),ecology=as.matrix(ecoMat))
      
      
      # Run MMRR function using genMat as the response variable and Xmats as the explanatory variables.
      # nperm does not need to be specified, default is nperm=999)
      mmrr=MMRR(as.matrix(genMat),Xmats,nperm=999)
      #print(mmrr)
      
      thisrow = cbind(r.squared=mmrr$r.squared,
                      coefficientsIntercept=mmrr$coefficients[1],
                      coefficientsgeography=mmrr$coefficients[2],
                      coefficientsecology=mmrr$coefficients[3],
                      tstatisticIntercept=mmrr$tstatistic[1],
                      tstatisticgeography=mmrr$tstatistic[2],
                      tstatisticecology=mmrr$tstatistic[3],
                      tpvalueIntercept=mmrr$tpvalue[1],
                      tpvaluegeography=mmrr$tpvalue[2],
                      tpvalueecology=mmrr$tpvalue[3],
                      Fstatistic=mmrr$Fstatistic,
                      Fpvalue=mmrr$Fpvalue,
                      ConfIntercept2.5=mmrr$Conf[1,1],
                      ConfIntercept97.5=mmrr$Conf[1,2],
                      Confgeography2.5=mmrr$Conf[2,1],
                      Confgeography97.5=mmrr$Conf[2,2],
                      Confecology2.5=mmrr$Conf[3,1],
                      Confecology97.5=mmrr$Conf[3,2],
                      file=basename(matrix_file))
      rownames(thisrow) = basename(matrix_file)
      
      mmrr_output_filename = "~/Documents/Postdoc_Working/mmrr_output.txt"
      write.table(thisrow,mmrr_output_filename,quote=F,append=T,sep="\t",row.names=T,col.names=T)
    })
    ## for my NOCA results: rsq = 0.14. coeffs geo = 0.52, eco = -0.22. pvals geo = 0.009, eco = 0.11. overall p 0.011
    ## for TEMPORARY zono results: rsq = 0.0019. coeffs geo = 0.000839 eco = 0.043. pvls geo = 0.991 eco = 0.274. overall p 0.43
    ## for TEMPORARY noca results: rsq = 0.086. coeffs geo = 0.43, eco = -0.23. pvals geo = 0.021, eco = 0.010. overall p 0.038.
    ## for TEMPORARY sin results: rsq = 0.03. coeffs geo = 0.173, eco = -2.27. pvals geo = 0.39, eco - 0.34. overall p 0.557. 
    ## for TEMPORARY calypte results: rsq = 0.003. coeffs geo = 0.011, eco = 0.0059. pvals geo = 0.89, eco - 0.57. overall p 0.80. 
    ## for TEMPORARY empid results: rsq = 0.07. coeffs geo = -0.14, eco = -0.17. pvals geo = 0.27, eco - 0.27. overall p 0.12. 
    ## for TEMPORARY melozone results: rsq = 0.08. coeffs geo = 0.91, eco = -0.71. pvals geo = 0.25, eco - 0.46. overall p 0.49. 
  }
  
  
}








# metadf = read.table("/Users/kprovost/Downloads/song_metadata_edited_zl.txt",
#                     sep="\t",header=T)
# metadf$SUBSPECIES[metadf$SUBSPECIES==""] = "unknown"
# metadf$ORIGINALSUBSPECIES[metadf$ORIGINALSUBSPECIES==""] = "unknown"
# metadf$ID2 = paste(metadf$GENUS,metadf$SPECIES,metadf$SUBSPECIES,metadf$COLLECTION,metadf$ID,sep=".") 
# metadf$ID2 = paste(metadf$GENUS,metadf$SPECIES,#metadf$SUBSPECIES,
#                    metadf$COLLECTION,metadf$ID,sep=".") 
# 
# plot(metadf$LONGITUDE,metadf$LATITUDE,col=as.numeric(as.factor(metadf$SUBSPECIES)))
# shp1 = shapefile("/Users/kprovost/Downloads/cb_2016_us_state_500k/cb_2016_us_state_500k.shp")
# summer = metadf[metadf$MONTH %in% c(5:8),]
# winter = metadf[metadf$MONTH %in% c(1:4,9:12),]
# plot(summer$LONGITUDE,summer$LATITUDE,col="red")
# points(winter$LONGITUDE,winter$LATITUDE,col="blue",pch=16)
# palette(c("black","red","orange","goldenrod","green","cyan","blue"))
# misalign=(metadf[metadf$SUBSPECIES!=metadf$ORIGINALSUBSPECIES,])
# plot(misalign$LONGITUDE,misalign$LATITUDE,col=as.numeric(as.factor(misalign$SUBSPECIES)),
#      pch=as.numeric(as.factor(misalign$SUBSPECIES)))
# pdf("~/subspecies_assignments.pdf",height=5,width=10)
# for(subspp in sort(unique(metadf$SUBSPECIES))){
#   par(mfrow=c(1,3))
#   metadf_subspp1 = metadf[metadf$SUBSPECIES==subspp,]
#   metadf_subspp2 = metadf[metadf$ORIGINALSUBSPECIES==subspp,]
#   plot(metadf_subspp1$LONGITUDE,metadf_subspp1$LATITUDE,col=as.numeric(as.factor(metadf_subspp1$ORIGINALSUBSPECIES)),pch=as.numeric(as.factor(metadf_subspp1$ORIGINALSUBSPECIES)),
#        main=paste(subspp,"ASSIGNED"))
#   plot(metadf_subspp2$LONGITUDE,metadf_subspp2$LATITUDE,col=as.numeric(as.factor(metadf_subspp2$SUBSPECIES)),pch=as.numeric(as.factor(metadf_subspp1$SUBSPECIES)),
#        main=paste(subspp,"ORIGINAL"))
#   metadf_subspp3 = unique(rbind(metadf_subspp1,metadf_subspp2))
#   plot(metadf_subspp3$LONGITUDE,metadf_subspp3$LATITUDE,col=ifelse(metadf_subspp3$ORIGINALSUBSPECIES==metadf_subspp3$SUBSPECIES,
#                                                                    "black","red"),
#        pch=ifelse(metadf_subspp3$ORIGINALSUBSPECIES==metadf_subspp3$SUBSPECIES,1,2))
# }
# dev.off()
# metadf = metadf[metadf$GENUS=="Zonotrichia",]
# metadf = metadf[metadf$SPECIES=="leucophrys",]
# metadf = metadf[metadf$SUBSPECIES %in% c("nuttalli","pugetensis"),]
## Admixed pops between -123.702 and -123.85. Full subspp assignments at -123.105 and -124.358
## Admixed pops between 38.981 and 39.832. Full subspp assignments at 38.419 and 40.544
# abline(v=-123.702,col="grey")
# abline(v=-123.85,col="grey")
# abline(v=-123.105,col="green")
# abline(v=-124.358,col="green")
# abline(h=38.981,col="grey")
# abline(h=39.832,col="grey")
# abline(h=38.419,col="green")
# abline(h=40.544,col="green")
