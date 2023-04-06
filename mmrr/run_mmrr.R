rm(list = ls())

## set up functions
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

unfold<-function(X,center=T,scale=T){
  # unfold converts the lower diagonal elements of a matrix into a vector
  # unfold is called by MMRR
  x<-vector()
  for(i in 2:nrow(X)) x<-c(x,X[i,1:i-1])
  x<-scale(x, center=center, scale=scale)  # Comment this line out if you wish to perform the analysis without standardizing the distance matrices! 
  return(x)
}

outputMMRR = function(genMat,Xmats,nummmrr,thislength,name_gene,scale=T,center=T){
  samples = nrow(genMat)
  mmrr=NULL
  try({mmrr=MMRR(Y=as.matrix(genMat),X=Xmats,nperm=nummmrr,scale=scale,center=center)})
  if(is.null(mmrr)){
    mmrr_row=cbind(name_gene,thislength,samples,nummmrr,NA)
  } else {
    mmrr_row=cbind(name_gene,thislength,samples,nummmrr,
                   mmrr$r.squared,
                   ifelse(nrow(mmrr$Conf)>=2,mmrr$coefficients[2],NA),
                   ifelse(nrow(mmrr$Conf)>=3,mmrr$coefficients[3],NA),
                   ifelse(nrow(mmrr$Conf)>=4,mmrr$coefficients[4],NA),
                   ifelse(nrow(mmrr$Conf)>=2,mmrr$tpvalue[2],NA),
                   ifelse(nrow(mmrr$Conf)>=3,mmrr$tpvalue[3],NA),
                   ifelse(nrow(mmrr$Conf)>=4,mmrr$tpvalue[4],NA),
                   mmrr$Fpvalue,
                   mmrr$Fstatistic,
                   ifelse(nrow(mmrr$Conf)>=2,mmrr$tstatistic[2],NA),
                   ifelse(nrow(mmrr$Conf)>=3,mmrr$tstatistic[3],NA),
                   ifelse(nrow(mmrr$Conf)>=4,mmrr$tstatistic[4],NA),
                   ifelse(nrow(mmrr$Conf)>=2,mmrr$Conf[2,1],NA),
                   ifelse(nrow(mmrr$Conf)>=3,mmrr$Conf[3,1],NA),
                   ifelse(nrow(mmrr$Conf)>=4,mmrr$Conf[4,1],NA),
                   ifelse(nrow(mmrr$Conf)>=2,mmrr$Conf[2,2],NA),
                   ifelse(nrow(mmrr$Conf)>=3,mmrr$Conf[3,2],NA),
                   ifelse(nrow(mmrr$Conf)>=4,mmrr$Conf[4,2],NA)
    )
  }
  colnames(mmrr_row) = c("spp-gene","N","samples","nummmrr","rsq",
                         ifelse(nrow(mmrr$Conf)>=2,paste(names(Xmats)[1],"_coef",sep=""),NA),
                         ifelse(nrow(mmrr$Conf)>=3,paste(names(Xmats)[2],"_coef",sep=""),NA),
                         ifelse(nrow(mmrr$Conf)>=4,paste(names(Xmats)[3],"_coef",sep=""),NA),
                         ifelse(nrow(mmrr$Conf)>=2,paste(names(Xmats)[1],"_p",sep=""),NA),
                         ifelse(nrow(mmrr$Conf)>=3,paste(names(Xmats)[2],"_p",sep=""),NA),
                         ifelse(nrow(mmrr$Conf)>=4,paste(names(Xmats)[3],"_p",sep=""),NA),
                         "overall_p","F",
                         ifelse(nrow(mmrr$Conf)>=2,paste(names(Xmats)[1],"_t",sep=""),NA),
                         ifelse(nrow(mmrr$Conf)>=3,paste(names(Xmats)[2],"_t",sep=""),NA),
                         ifelse(nrow(mmrr$Conf)>=4,paste(names(Xmats)[3],"_t",sep=""),NA),
                         ifelse(nrow(mmrr$Conf)>=2,paste(names(Xmats)[1],"_2.5",sep=""),NA),
                         ifelse(nrow(mmrr$Conf)>=3,paste(names(Xmats)[2],"_2.5",sep=""),NA),
                         ifelse(nrow(mmrr$Conf)>=4,paste(names(Xmats)[3],"_2.5",sep=""),NA),
                         ifelse(nrow(mmrr$Conf)>=2,paste(names(Xmats)[1],"_97.5",sep=""),NA),
                         ifelse(nrow(mmrr$Conf)>=3,paste(names(Xmats)[2],"_97.5",sep=""),NA),
                         ifelse(nrow(mmrr$Conf)>=4,paste(names(Xmats)[3],"_97.5",sep=""),NA)
  )
  return(mmrr_row)
}

## get folders
distance_folder = "/Users/kprovost/Documents/Postdoc_Working/MMRR/DISTANCES/FINAL/"
song_folder = "/Users/kprovost/Documents/Postdoc_Working/MMRR/WAVS/Wave/0STATSDONE/"
genetics_folder = "/Users/kprovost/Documents/Postdoc_Working/Genetics/Birds-phylogatr-results_7dec2020/Aves/Passeriformes/Passerellidae/"

## get species
centroid_file = "/Users/kprovost/Documents/Postdoc_Working/MMRR/WAVS/Wave/0STATSDONE/centroid_locations_per_individual_08Mar2023.txt"
df_pca = data.table::fread(paste(centroid_file,"_PCA-DATA.txt",sep=""),header=T,data.table=F)
spp_df = df_pca[,c("genus","species","subspecies")]
spp_df = unique(spp_df)

## merge centroid file with previous env data file
env_file = "/Users/kprovost/Documents/Postdoc_Working/MMRR/Passerellidae_genetic_song_combined_occurrences_ENVS_9Nov2022_CLUSTER_trimmed.txt"
env = data.table::fread(env_file,header=T,data.table=F)
colnames(env)[colnames(env)=="GENUS"] = "genus"
colnames(env)[colnames(env)=="SPECIES"] = "species"
colnames(env)[colnames(env)=="COLLECTION"] = "database"
colnames(env)[colnames(env)=="ID"] = "catalog"
colnames(env)[colnames(env)=="SUBSPECIES"] = "subspecies"
colnames(env)[colnames(env)=="FULLID"] = "Ind"

df_pca_env = merge(df_pca,env,all=T)
latlong_only = df_pca_env[,c("Ind","genus","species","subspecies","database","catalog","LATITUDE","LONGITUDE","ADJUSTED_LONGITUDE")]
latlong_only = unique(latlong_only) 
## use adjusted longitude


## extract files
distance_files = list.files(path=distance_folder,pattern="txt$",full.names = T,recursive=T)
song_files = list.files(path=song_folder,pattern="DIST.*txt$",full.names = T,recursive=T)
genetics_files = list.files(path=genetics_folder,pattern="master.*txt$",full.names = T,recursive=T)

genMat_file = ""
u40Mat_file = ""
u17Mat_file = ""
uDFMat_file = ""
geoMat_file = ""
timMat_file = ""
cliMat_file = ""
sonMat_file = ""
habMat_file = ""
topMat_file = ""



genus="Aimophila"
species="ruficeps"
subspecies="unknown"

distance_genus = distance_files[grepl(genus,distance_files)]
distance_species = distance_genus[grepl(species,distance_genus)]
#distance_subspecies = distance_species[grepl(subspecies,distance_species)]
song_genus = song_files[grepl(genus,song_files)]
song_species = song_genus[grepl(species,song_genus)]
song_subspecies  = song_species[grepl(subspecies,song_species)]
genetics_genus = genetics_files[grepl(genus,genetics_files)]
genetics_species = genetics_genus[grepl(species,genetics_genus)]
genetics_subspecies = genetics_species[grepl(subspecies,genetics_species)]



## loop over the song files as sonMat files

## loop over the genetics files as genMat files




for(genus in sort(unique(spp_df$genus))){
  print(genus)
}



print("matching")
genMat = as.matrix(songdist)
geoMat = as.matrix(ibddist)
ecoMat = as.matrix(climdist)
timMat = as.matrix(yeardist)

genMat = genMat[order(rownames(genMat)),order(colnames(genMat))]
geoMat = geoMat[order(rownames(geoMat)),order(colnames(geoMat))]
ecoMat = ecoMat[order(rownames(ecoMat)),order(colnames(ecoMat))]
timMat = timMat[order(rownames(timMat)),order(colnames(timMat))]

matches = intersect(rownames(genMat),rownames(geoMat))
matches = intersect(rownames(ecoMat),matches)
matches = intersect(rownames(timMat),matches)

genMat = genMat[which(matches %in% rownames(genMat)),which(matches %in% colnames(genMat))]
geoMat = geoMat[which(matches %in% rownames(geoMat)),which(matches %in% colnames(geoMat))]
ecoMat = ecoMat[which(matches %in% rownames(ecoMat)),which(matches %in% colnames(ecoMat))]
timMat = timMat[which(matches %in% rownames(timMat)),which(matches %in% colnames(timMat))]

print("listing")
Xmats_all = list(geography=as.matrix(geoMat),ecology=as.matrix(ecoMat),time=as.matrix(timMat))


