## calculating vocal differentiation from pca scores
## need proper metadata file: number, species, lat long, year month day, sex, elevation
library(raster)
library(viridis)
library(RColorBrewer)

cols_sym = brewer.pal(11,"RdBu")
cols_asym = brewer.pal(9,"Blues")


pca_file = "/Users/kprovost/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Song/SoundShape/Cardinalis_sinuatus_pca_soundshape.txt"
imp_file = "/Users/kprovost/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Song/SoundShape/Cardinalis_sinuatus_pca_soundshape_importance.txt"
meta_file = "/Users/kprovost/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Song/song_metadata.txt"
meta_data = read.table(meta_file,sep="\t",header=T)
imp_data = (read.table(imp_file,sep=" ",header=T))
imp_data$value = rownames(imp_data)
imp_data = as.data.frame(t(imp_data))
imp_data$PC = rownames(imp_data)
plot(imp_data$`Cumulative Proportion`)
thresh=min(which(imp_data$`Cumulative Proportion`>=0.5))

pca_data_big = read.table(pca_file,sep=" ",header=T)
print(dim(pca_data_big))
pca_data = pca_data_big[,1:thresh]
print(colnames(pca_data))

pca_data$samples = rownames(pca_data)
pca_data$samples = sub("-",".",sub("_",".",pca_data$samples))
pca_data$samples = sub("BLB","BLB.",pca_data$samples)
pca_data$samples = sub("\\.\\.",".",pca_data$samples)
pca_temp=sapply(pca_data$samples,FUN=function(x){strsplit(x,split = "\\.")})
pca_metadata = as.data.frame(do.call(rbind,pca_temp))
colnames(pca_metadata) = paste("metadata",1:ncol(pca_metadata),sep=".")
pca_metadata$samples = rownames(pca_metadata)

## fix the col names after looking
colnames(pca_metadata) = c("GENUS","SPECIES","COLLECTION","ID","SYLLABLE","samples")

pca_combo = merge(pca_data,pca_metadata)

pca_full = merge(pca_combo,meta_data,by=c("COLLECTION","ID","GENUS","SPECIES"),
                 all.x=T,all.y=F,sort=T)

## turn metadata into a raster

plot(pca_full$PC1,pca_full$PC2)

bothbias_big=raster("/Users/kprovost/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Song/XenoCanto/DATASETS/combined/Sum-BLB-and-XC_songs.asc")

maxlat=max(pca_full$LATITUDE,na.rm=T)
maxlon=max(pca_full$LONGITUDE,na.rm=T)
minlat=min(pca_full$LATITUDE,na.rm=T)
minlon=min(pca_full$LONGITUDE,na.rm=T)

bothbias = crop(bothbias_big,extent(c(minlon-1,maxlon+1,minlat-1,maxlat+1)))
plot(bothbias)
points(pca_full$LONGITUDE,pca_full$LATITUDE)

pca_full$LATITUDE=as.numeric(pca_full$LATITUDE)
pca_full$LONGITUDE=as.numeric(pca_full$LONGITUDE)
cells=raster::cellFromXY(bothbias,pca_full[,c("LONGITUDE","LATITUDE")])
pca_full$cells = cells

if(thresh > 10){
  numpc = 3
} else {
  numpc = thresh
}

pcas = paste("PC",1:numpc,sep="")

## write a function to do the subsetting by groups

volume_by_group = function(data,groupcol,datacols){
  # data = pca_full
  # groupcol = cells
  # datacols = pcas
  fullvol=geometry::convhulln(data[,datacols],output.options="FA")$vol
  voldf=cbind("ALL",fullvol,nrow(data))
  
  groups = data[,groupcol]
  for(group in sort(unique(groups))){
    if(!(is.na(group))){
      subset = data[data[,groupcol]==group,datacols]
      subset = subset[complete.cases(subset),]
      ## calculate the volume of this subset
      subsetvol=geometry::convhulln(subset,output.options="FA")$vol
      voldf = rbind(voldf,cbind(group,subsetvol,nrow(subset)))
    }
  }
  voldf = as.data.frame(voldf)
  colnames(voldf) = c("group","fullvol","N")
  voldf$relvol = as.numeric(voldf$fullvol)/(as.numeric(voldf$fullvol[voldf$group=="ALL"]))
  return(voldf)
  
}

centroid_by_group = function(data,groupcol,datacols){
  
  ## overall centroid
  fullcentroid=unlist(colMeans(data[,datacols]))
  centdf=rbind(c("ALL",nrow(data),fullcentroid))
  
  groups = data[,groupcol]
  for(group in sort(unique(groups))){
    if(!(is.na(group))){
      subset = data[data[,groupcol]==group,datacols]
      subset = subset[complete.cases(subset),]
      ## calculate the centroid of this subset
      subsetcentroid=unlist(colMeans(subset[,datacols]))
      centdf = rbind(centdf,(rbind(c(group,nrow(subset),subsetcentroid))))
    }
  }
  centdf = as.data.frame(centdf)
  colnames(centdf) = c("group","N",paste("cent",datacols,sep="."))
  return(centdf)
}

sd_by_group = function(data,groupcol,datacols){
  
  ## overall centroid
  fullsds=unlist(matrixStats::colSds(as.matrix(data[,datacols])))
  sdsdf=rbind(c("ALL",nrow(data),fullsds))
  
  groups = data[,groupcol]
  for(group in sort(unique(groups))){
    if(!(is.na(group))){
      subset = data[data[,groupcol]==group,datacols]
      subset = subset[complete.cases(subset),]
      ## calculate the centroid of this subset
      subsetsds=unlist(matrixStats::colSds(as.matrix((subset[,datacols]))))
      sdsdf = rbind(sdsdf,(rbind(c(group,nrow(subset),subsetsds))))
    }
  }
  sdsdf = as.data.frame(sdsdf)
  colnames(sdsdf) = c("group","N",paste("sds",datacols,sep="."))
  return(sdsdf)
}

celldf=volume_by_group(data=pca_full,groupcol="cells",datacols=pcas)
IDdf=volume_by_group(data=pca_full,groupcol="ID",datacols=pcas)

cellcent=centroid_by_group(data=pca_full,groupcol="cells",datacols=pcas)
IDcent=centroid_by_group(data=pca_full,groupcol="ID",datacols=pcas)

cellsds=sd_by_group(data=pca_full,groupcol="cells",datacols=pcas)
IDsds=sd_by_group(data=pca_full,groupcol="ID",datacols=pcas)

palette(brewer.pal(8,"Dark2"))

plot(pca_full$PC1,pca_full$PC2,col=as.numeric(as.factor(pca_full$ID))+1,
     pch=as.numeric(as.factor(pca_full$ID)))
points(IDcent$cent.PC1,IDcent$cent.PC2,col="black",pch=as.numeric(as.factor(IDcent$group)),
       cex=2,lwd=2)

## plot centroid cell values on a map with sds values

cellblank = bothbias; values(cellblank) = NA

cellpc1 = cellblank; cellpc2 = cellblank; cellpc3 = cellblank;
values(cellpc1)[as.numeric(cellcent$group[cellcent$group!="ALL"])] = as.numeric(cellcent$cent.PC1[cellcent$group!="ALL"])
values(cellpc2)[as.numeric(cellcent$group[cellcent$group!="ALL"])] = as.numeric(cellcent$cent.PC2[cellcent$group!="ALL"])
values(cellpc3)[as.numeric(cellcent$group[cellcent$group!="ALL"])] = as.numeric(cellcent$cent.PC3[cellcent$group!="ALL"])
cellpc1sd = cellblank; cellpc2sd = cellblank; cellpc3sd = cellblank;
values(cellpc1sd)[as.numeric(cellsds$group[cellsds$group!="ALL"])] = as.numeric(cellsds$sds.PC1[cellsds$group!="ALL"])
values(cellpc2sd)[as.numeric(cellsds$group[cellsds$group!="ALL"])] = as.numeric(cellsds$sds.PC2[cellsds$group!="ALL"])
values(cellpc3sd)[as.numeric(cellsds$group[cellsds$group!="ALL"])] = as.numeric(cellsds$sds.PC3[cellsds$group!="ALL"])

par(mfrow=c(2,3))
plot(cellpc1,colNA="grey",col=cols_asym)
plot(cellpc2,colNA="grey",col=cols_asym)
plot(cellpc3,colNA="grey",col=cols_asym)
plot(cellpc1sd,colNA="grey",col=cols_asym)
plot(cellpc2sd,colNA="grey",col=cols_asym)
plot(cellpc3sd,colNA="grey",col=cols_asym)

## calculate distance between individuals
IDdist=as.matrix(dist(IDcent[,3:ncol(IDcent)],upper=T,diag=T,method = "euclidean"))
colnames(IDdist) = IDcent$group
rownames(IDdist) = IDcent$group
IDdist=IDdist[colnames(IDdist)!="ALL",colnames(IDdist)!="ALL"]
plot(ape::nj(IDdist),"unrooted")

heatmap(IDdist)

## also need the geog dist between individuals 
pca_inds = unique(pca_full[c("COLLECTION","ID","LATITUDE","LONGITUDE")])
IDgeogdist = as.matrix(dist(pca_inds[,c("LATITUDE","LONGITUDE")]))
rownames(IDgeogdist) = pca_inds$ID
colnames(IDgeogdist) = pca_inds$ID

## account for geogroaphic distance with individual distance
ras = stack(c(raster(IDdist),raster(IDgeogdist)))
ras_stats=layerStats(ras,"pearson",na.rm=T,as.sample=F)

ID_mod=lm(values(raster(IDdist))~values(raster(IDgeogdist)))
ID_res=ID_mod$residuals
ID_res_ras = raster(IDdist)
values(ID_res_ras) = ID_res

plot(values(raster(IDgeogdist)),values(raster(IDdist)))
abline(ID_mod,col="red")
summary(ID_mod)

par(mfrow=c(3,1))
plot(raster(IDdist))
plot(raster(IDgeogdist))
plot(ID_res_ras)

## calculate distance between cells
celldist=as.matrix(dist(cellcent[,3:ncol(cellcent)],upper=T,diag=T,method = "euclidean"))
colnames(celldist) = cellcent$group
rownames(celldist) = cellcent$group
celldist=celldist[colnames(celldist)!="ALL",colnames(celldist)!="ALL"]
plot(ape::nj(celldist),"unrooted")

heatmap(celldist)

## calculate distance between syllables
sampledist = as.matrix(dist(pca_full[,pcas],upper=T,diag=T,method="euclidean"))
colnames(sampledist) = pca_full$samples
rownames(sampledist) = pca_full$samples
sampledist=sampledist[colnames(sampledist)!="ALL",colnames(sampledist)!="ALL"]
plot(ape::nj(sampledist),"unrooted",show.tip.label=F)

heatmap(sampledist)











plot(bothbias)

relsongvol = bothbias
values(relsongvol) = 0
values(relsongvol)[is.na(values(bothbias))] = NA
blank=bothbias
values(blank)[!(is.na(values(blank)))] = 0
values(bothbias)[values(bothbias)==0] = NA
relsongvol[as.numeric(celldf$cell[celldf$cell!="ALL"])] = as.numeric(celldf$relvol[celldf$cell!="ALL"])

plot(log10(values(bothbias)),values(relsongvol))

relsongvol[relsongvol==0] = NA
#plot(blank,ylim=c(0,90),xlim=c(-180,0),col="grey",legend=F)
#plot(relsongvol,ylim=c(0,90),xlim=c(-180,0),col=plasma(10),add=T)

## now do this accounting for bias
## need a better method, ideally one using lm

mod=lm(values(relsongvol)~log10(values(bothbias)))
bias_resid = residuals(mod)

divbias = relsongvol
divbias[as.numeric(names(bias_resid))] = as.numeric(bias_resid)
#plot(blank,ylim=c(0,90),xlim=c(-180,0),col="grey",legend=F)
#plot(divbias,ylim=c(0,90),xlim=c(-180,0),col=plasma(10),add=T)



par(mfrow=c(3,1))
plot(log10(bothbias),col=cols_asym,main="LOG BIAS")
plot(blank,ylim=c(0,90),xlim=c(-180,0),col="grey",legend=F,main="REL VOL")
plot(relsongvol,ylim=c(0,90),xlim=c(-180,0),col=cols_asym,add=T)
plot(blank,ylim=c(0,90),xlim=c(-180,0),col="grey",legend=F,main="REL VOL / BIAS")
endpt = max(abs(values(divbias)),na.rm=T)
plot(divbias,ylim=c(0,90),xlim=c(-180,0),col=cols_sym,add=T,zlim=c(-endpt,endpt))

#dev.off()








### ideas

## monmonier's algorithm? 

pca_mmn = unique(pca_full[,c("ID","LONGITUDE","LATITUDE")])
pca_mmn$LATITUDE=jitter(pca_mmn$LATITUDE)
pca_mmn$LONGITUDE=jitter(pca_mmn$LONGITUDE)
rownames(pca_mmn) = pca_mmn$ID

cn=adegenet::chooseCN(xy=as.matrix(pca_mmn[,c("LONGITUDE","LATITUDE")]),
                      type=1)

mmn1=adegenet::monmonier(xy=as.matrix(pca_mmn[,c("LONGITUDE","LATITUDE")]),
                         dist=as.dist(IDdist),
                         cn=cn,threshold = 15)
mmn2=adegenet::monmonier(xy=as.matrix(pca_mmn[,c("LONGITUDE","LATITUDE")]),
                         dist=as.dist(IDgeogdist),
                         cn=cn,threshold=8)

plot(mmn1,col="blue",add.arr=F,method="greylevel")
plot(mmn2,col="green",add.arr=F,method="greylevel")

mmn=adegenet::optimize.monmonier(xy=as.matrix(pca_mmn[,c("LONGITUDE","LATITUDE")]),
                                 dist=as.dist(IDdist),return.best = T,
                                 cn=cn,threshold = 20) ## added threshold to get to run
plot(mmn,col="red",add.arr=F,method="greylevel")
