df = read.table("/Users/kprovost/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Song/song_metadata_PASSERELLIDAE.txt",header=T,
                sep="\t")
latlong = df[,c("LONGITUDE","LATITUDE")]
df=df[which(complete.cases(latlong)),]
latlong = df[,c("LONGITUDE","LATITUDE")]

r <- getData("worldclim",var="bio",res=10)
bg=r[[1]]
bg[!(is.na(bg))] = 0
#bg=raster::aggregate(bg,40)
bg=raster::aggregate(bg,10)

minlat=min(df$LATITUDE,na.rm=T)
maxlat=max(df$LATITUDE,na.rm=T)
minlon=min(df$LONGITUDE,na.rm=T)
maxlon=max(df$LONGITUDE,na.rm=T)
bgc = crop(bg,extent(minlon,-40,minlat,maxlat))

latlongcoor = SpatialPoints(coords = latlong)
#bg2 = aggregate(bg,fact=20)
rasnorm = rasterize(latlong, bgc,fun='count',background=10e-1,update=F,na.rm=T)
rasnorm[is.na(bg)] = NA
ras=rasnorm
values(ras) = log10(values(rasnorm))
ras[is.na(bg)] = NA
ras[rasnorm<0] = -0.1
cuts=c(-0.1,0,0.5,1,1.5,2,2.5,3,3.5,4,4.5)
plot(ras,breaks=cuts,col=c("#BBBBBBFF",viridis::plasma(9)))



for(dfile in (distfiles)){
  print(dfile)
  ecoMat=read.table("climate_distances_per_individual_allWC.txt",header=T,sep="\t",skip=0,check.names = F)
  geoMat=read.table("ibd_distances_per_individual.txt",header=T,sep="\t",skip=0,check.names = F)
  genMat=read.table(dfile,header=T,sep="\t",skip=0,check.names = F)
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
  try({mmrr=MMRR(as.matrix(genMat),Xmats,nperm=999)
  
  newrow=cbind(basename(dfile),mmrr$r.squared,mmrr$coefficients[names(mmrr$coefficients)=="geography"],
               mmrr$coefficients[names(mmrr$coefficients)=="ecology"],mmrr$tpvalue[names(mmrr$tpvalue)=="geography(p)"],
               mmrr$tpvalue[names(mmrr$tpvalue)=="ecology(p)"],mmrr$Fpvalue)
  colnames(newrow) = c("spp-gene","rsq","geography_coef","ecology_coef","geography_p","ecology_p","overall_p")
  
  print(newrow)
  })
  
}
