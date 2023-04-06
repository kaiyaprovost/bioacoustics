rm(list = ls())

outpath = "/Users/kprovost/Documents/Postdoc_Working/MMRR/WAVS/Wave/0STATSDONE/"
path="/Users/kprovost/Documents/Postdoc_Working/MMRR/WAVS/Wave/0STATSDONE/"

setwd(path)
date=format(Sys.time(), "%d%b%Y")
centroid_pattern="_COMBINED_7Mar2023.txt.temp$"

print("GATHERING FILES")

metafiles = list.files(path,pattern=centroid_pattern,full.names = T,recursive = T) ## edited the code so that can find the files with the sumstats we generated
metafiles = metafiles[1]

print("GENERATING SUMSTATS")

concatdf = lapply(metafiles,FUN=function(meta){
  print(meta)
  
  df = read.table(meta,sep=" ",header=T)
  df=df[order(df[,"Begin.Time..s."]),]
  df$ffreq=NA
  df$raw_countour=NA
  df$slopes=NA
  rownames(df) = NULL
  
  df$Bandwidth = df$freq.IQR
  df$Time = df$duration
  df$Center = df$freq.median
  df$Inflection = df$inflections
  df$Slope = df$mean_slope
  df$Individual = df$Begin.File.1
  df$Segment = 1
  
  df$Bandwidth[is.na(df$Bandwidth)] = df$freq.Q75[is.na(df$Bandwidth)]-df$freq.Q25[is.na(df$Bandwidth)]
  df$Time[is.na(df$Time)] = df$time.Q75[is.na(df$Time)]-df$time.Q25[is.na(df$Time)]
  
  df = df[,c("Selection","Individual","Bandwidth","Time","Center","Inflection","Slope")]
  
  if(sum(complete.cases(df[,c("Selection","Individual","Bandwidth","Time","Center","Inflection","Slope")]))>1){
    return(df)
  }
})
bigdf = do.call(rbind,concatdf)

## dflist needs to be one dataframe for each individual
individuals = sort(unique(bigdf$Individual))
dflist = lapply(individuals,FUN=function(ind){
  df = bigdf[bigdf$Individual==ind,]
  df = unique(df)
  df = df[complete.cases(df),]
  if(nrow(df)>=1){
    return(df)
  }
})
names(dflist)=individuals

species_dflist = sapply(names(dflist),FUN=function(x){
  y=strsplit(x,"\\.")[[1]][1:2]
  z=paste(y,sep=".",collapse = ".")
},simplify = T)
names(species_dflist) = NULL


this_length = length(dflist)
centroiddf=data.frame(matrix(ncol=3,nrow=0))

## calculate centroids 
for(ind_i in 1:this_length) {
  print(paste(ind_i,"/",this_length))
  df_i = dflist[[ind_i]]
  if(!(is.null(df_i))){
    centroid_i = colMeans(df_i[,c("Bandwidth","Time","Center","Inflection","Slope")],na.rm=T)
    centroiddf=rbind(centroiddf,centroid_i)
  } else {
    centroiddf=rbind(centroiddf,c(NA,NA,NA,NA,NA))
  }
}
colnames(centroiddf) = c("Bandwidth","Time","Center","Inflection","Slope")
centroiddf$Ind = individuals

## for centroid data, calculate the species, genus, database, number, etc
newcols_raw=lapply(centroiddf$Ind,FUN=function(x){
  x = sub(".resample.48000.wav","",x)
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
centroiddf = centroiddf[complete.cases(centroiddf),]

write.table(centroiddf,
            paste(outpath,"/centroid_locations_per_individual_",date,".txt",sep=""),
            sep="\t",quote=F,row.names = F,
            append=T,col.names = T)
lapply(metafiles,FUN=function(x){
  print(x)
  R.utils::gzip(x)
})

