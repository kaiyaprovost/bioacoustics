n_individuals = 10
mean_syllables = 80
sd_syllables = 20
n_type_syllables = 20
mean_type_syllables = 10
sd_type_syllables = 3

pc1_syllable_means = runif(n_type_syllables,min=-1,max=1)
pc2_syllable_means = runif(n_type_syllables,min=-1,max=1)
pc3_syllable_means = runif(n_type_syllables,min=-1,max=1)

pc1_syllable_sds = runif(n_type_syllables,min=0,max=0.2)
pc2_syllable_sds = runif(n_type_syllables,min=0,max=0.2)
pc3_syllable_sds = runif(n_type_syllables,min=0,max=0.2)

## for each individual
syldf = NULL
for(i in 1:n_individuals){
  print(paste("ind:",i))
  
  ycoord=runif(1)
  xcoord=runif(1)
  
  ## number of syllables to generate
  ind_syll_n = round(rnorm(n=1,mean=mean_syllables,sd=sd_syllables),0)
  ind_syll_type_n = round(rnorm(n=1,mean=mean_type_syllables,sd=sd_type_syllables),0)
  
  ## type of syllables to generate
  ind_actual_syll = sort(sample(1:n_type_syllables,ind_syll_type_n,replace=F))
  
  ## the syllables themselves
  ind_sylls = table((sample(ind_actual_syll,size=ind_syll_n,replace=T)))
  
  for(j in 1:ind_syll_n){
    #print(paste("syll:",j,"/",ind_syll_n))
    this_syll = sample(ind_actual_syll,size=1,replace=T)
    pc1 = rnorm(1,mean=pc1_syllable_means[this_syll],sd=pc1_syllable_sds[this_syll])
    pc2 = rnorm(1,mean=pc2_syllable_means[this_syll],sd=pc2_syllable_sds[this_syll])
    pc3 = rnorm(1,mean=pc3_syllable_means[this_syll],sd=pc3_syllable_sds[this_syll])
    
    if(is.null(syldf)){
      syldf = cbind(i,ycoord,xcoord,j,this_syll,pc1,pc2,pc3)
    } else {
      syldf = rbind(syldf,cbind(i,ycoord,xcoord,j,this_syll,pc1,pc2,pc3))
    }
    
  }
  
}
syldf=as.data.frame(syldf)
colnames(syldf) = c("IND","Y","X","SYL","SYLTYPE","PC1","PC2","PC3")

plot(syldf$PC1,syldf$PC2,pch=syldf$SYLTYPE,col=syldf$SYLTYPE)
plot(syldf$PC1,syldf$PC3,pch=syldf$SYLTYPE,col=syldf$SYLTYPE)

plot(syldf$PC1,syldf$PC2,pch=syldf$IND,col=syldf$IND)
plot(syldf$PC1,syldf$PC3,pch=syldf$IND,col=syldf$IND)

plot(unique(syldf[,c("X","Y")]))

ras = raster(nrows=10,ncols=10,xmn=0,xmx=1,ymn=0,ymx=1)

syldf$Y=as.numeric(syldf$Y)
syldf$X=as.numeric(syldf$X)
cells=raster::cellFromXY(ras,syldf[,c("X","Y")])
syldf$cells = cells

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

pcas=c("PC1","PC2","PC3")

cellvol=volume_by_group(data=syldf,groupcol="cells",datacols=pcas)
INDvol=volume_by_group(data=syldf,groupcol="IND",datacols=pcas)
syllvol=volume_by_group(data=syldf,groupcol="SYLTYPE",datacols=pcas)

cellcent=centroid_by_group(data=syldf,groupcol="cells",datacols=pcas)
INDcent=centroid_by_group(data=syldf,groupcol="IND",datacols=pcas)
sylcent=centroid_by_group(data=syldf,groupcol="SYLTYPE",datacols=pcas)

cellsds=sd_by_group(data=syldf,groupcol="cells",datacols=pcas)
INDsds=sd_by_group(data=syldf,groupcol="IND",datacols=pcas)
sylsds=sd_by_group(data=syldf,groupcol="SYLTYPE",datacols=pcas)

## plot centroid cell values on a map with sds values

cellpc1 = ras; cellpc2 = ras; cellpc3 = ras;
values(cellpc1)[as.numeric(cellcent$group[cellcent$group!="ALL"])] = as.numeric(cellcent$cent.PC1[cellcent$group!="ALL"])
values(cellpc2)[as.numeric(cellcent$group[cellcent$group!="ALL"])] = as.numeric(cellcent$cent.PC2[cellcent$group!="ALL"])
values(cellpc3)[as.numeric(cellcent$group[cellcent$group!="ALL"])] = as.numeric(cellcent$cent.PC3[cellcent$group!="ALL"])
cellpc1sd = ras; cellpc2sd = ras; cellpc3sd = ras;
values(cellpc1sd)[as.numeric(cellsds$group[cellsds$group!="ALL"])] = as.numeric(cellsds$sds.PC1[cellsds$group!="ALL"])
values(cellpc2sd)[as.numeric(cellsds$group[cellsds$group!="ALL"])] = as.numeric(cellsds$sds.PC2[cellsds$group!="ALL"])
values(cellpc3sd)[as.numeric(cellsds$group[cellsds$group!="ALL"])] = as.numeric(cellsds$sds.PC3[cellsds$group!="ALL"])

cols_asym = RColorBrewer::brewer.pal(9,"Blues")

par(mfrow=c(2,3))
plot(cellpc1,colNA="grey",col=cols_asym)
plot(cellpc2,colNA="grey",col=cols_asym)
plot(cellpc3,colNA="grey",col=cols_asym)
plot(cellpc1sd,colNA="grey",col=cols_asym)
plot(cellpc2sd,colNA="grey",col=cols_asym)
plot(cellpc3sd,colNA="grey",col=cols_asym)

## euclidean distances

INDdist=as.matrix(dist(INDcent[,paste("cent",pcas,sep=".")],upper=T,diag=T,method = "euclidean"))
colnames(INDdist) = INDcent$group
rownames(INDdist) = INDcent$group
INDdist=INDdist[colnames(INDdist)!="ALL",colnames(INDdist)!="ALL"]
plot(ape::nj(INDdist),"unrooted")

syldf_mmn = unique(syldf[,c("IND","X","Y")])
rownames(syldf_mmn) = syldf_mmn$IND

cn=adegenet::chooseCN(xy=as.matrix(syldf_mmn[,c("X","Y")]),
                      type=5,ask=F,d1=0,d2="dmax")

mmn1=adegenet::monmonier(xy=as.matrix(syldf_mmn[,c("X","Y")]),
                    dist=as.dist(INDdist),
                    cn=cn,scanthres = T)
plot(mmn1,col="blue",add.arr=F,method="greylevel")

mmn=adegenet::optimize.monmonier(xy=as.matrix(syldf_mmn[,c("X","Y")]),
                    dist=as.dist(INDdist),return.best = T,
                    cn=cn,threshold = 0.4) ## added threshold to get to run
plot(mmn,col="red",add.arr=F,method="greylevel")

par(mfrow=c(2,1))
plot(mmn1)
plot(mmn)
