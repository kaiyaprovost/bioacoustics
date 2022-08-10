## TODO: functionize this 

## distances vs ibd as a test framework for mmrr 
## this is for NOCA

## DO NOT USE CENTROIDS FOR FINAL VALUES -- ONLY TEMP VALUES
eachFolder=T
doCentroid=T
doMMRR=T
doGene=F
doCorr=F
path="/Users/kprovost/Documents/Postdoc_Working/Sounds_and_Annotations/"
setwd(path)
metadf = read.table("/Users/kprovost/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Song/song_metadata.txt",
                    sep="\t",header=T)
metadf$SUBSPECIES[metadf$SUBSPECIES==""] = "unknown"
metadf$ID2 = paste(metadf$GENUS,metadf$SPECIES,metadf$SUBSPECIES,metadf$COLLECTION,metadf$ID,sep=".") 
metadf$ID2 = paste(metadf$GENUS,metadf$SPECIES,#metadf$SUBSPECIES,
                   metadf$COLLECTION,metadf$ID,sep=".") 

# MMRR performs Multiple Matrix Regression with Randomization analysis
# Y is a dependent distance matrix
# X is a list of independent distance matrices (with optional names)

MMRR<-function(Y,X,nperm=999){
  #compute regression coefficients and test statistics
  nrowsY<-nrow(Y)
  y<-unfold(Y)
  if(is.null(names(X)))names(X)<-paste("X",1:length(X),sep="")
  Xmats<-sapply(X,unfold)
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

unfold<-function(X){
  x<-vector()
  for(i in 2:nrow(X)) x<-c(x,X[i,1:i-1])
  x<-scale(x, center=TRUE, scale=TRUE)  # Comment this line out if you wish to perform the analysis without standardizing the distance matrices! 
  return(x)
}

library(tseries)
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
  
  library(maps)
  library(mapdata)
  
  # par(mfrow=c(1,2))
  # map("world",c("Canada","USA","Mexico"),
  #     xlim=c(-180,0),ylim=c(0,90))
  # points(metasmall[,3:2],col="red")
  # plot(ape::nj(as.matrix(distdf)),"phylogram",cex=0.5)
  
  ## gonna get a REAL basic ecological distance matrix 
  library(raster)
  library(sp)
  
  if(!(file.exists("climate_distances_per_individual_allWC.txt"))){
    r <- getData("worldclim",var="bio",res=10)
    #r <- r[[c(1,12)]]
    #names(r) <- c("Temp","Prec")
    points = extract(r,metasmall[,c("LONGITUDE","LATITUDE")])
    rownames(points) = metasmall$ID
    
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
  
  ## brief moment to get genetic distances 
  if(doGene==T) {
    library(raster)
    raster = "/Users/kprovost/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Environment/WorldClim2.1_jan2020/blank_worldclim.asc"
    bg = raster(raster)
    gene_folder = "/Users/kprovost/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Genetics/Birds-phylogatr-results_7dec2020/Aves/"
    occ_list = list.files(gene_folder,pattern="occurrences.txt",recursive=T,full.names = T)
    
    big_difference_df = data.frame()
    r <- getData("worldclim",var="bio",res=10)
    
    for(i in 1:length(occ_list)){
      print(paste("OCC",i))
      occ = occ_list[i]
      occ_directory=dirname(occ)
      occdf=NULL
      try({occdf = read.table(occ,header=T,sep="\t")}) ## failing
      if(!(is.null(occdf))){
        occdf$pop = paste(occdf$accession,occdf$gbif_id,sep="_")
        cells=raster::cellFromXY(bg,occdf[,c("longitude","latitude")])
        occdf$cells = cells
        gene_list = list.files(occ_directory,pattern=".afa$",full.names = T,recursive = F)
        
        
        points = extract(r,occdf[,c("longitude","latitude")])
        rownames(points) = occdf$pop
        climDist = dist(points,diag=T,upper = T)
        IBD=as.matrix(dist(occdf[,c("longitude","latitude")],diag=T,upper=T))
        rownames(IBD) = occdf$pop
        colnames(IBD) = occdf$pop
        
        for(gene in gene_list){
          print(paste("GENE",basename(gene)))
          data = ape::read.dna(gene,format="fasta")
          fasta_accessions = labels(data)
          zono = occdf[occdf$pop %in% fasta_accessions,c("accession","latitude","longitude","gbif_id","pop","cells")]
          total_nuc_div = pegas::nuc.div(data,variance=F,pairwise.deletion=T)
          distances=ape::dist.gene(data,method="pairwise",pairwise.deletion = T,variance = T)
          
          ecoMat = as.matrix(climDist)
          genMat = as.matrix(distances)
          geoMat = as.matrix(IBD)
          
          keptcols=Reduce(intersect, list(rownames(ecoMat),rownames(genMat),rownames(geoMat),
                                          colnames(ecoMat),colnames(genMat),colnames(geoMat)))
          
          genMat = genMat[which(colnames(genMat) %in% keptcols),which(rownames(genMat) %in% keptcols)]
          ecoMat = ecoMat[which(colnames(ecoMat) %in% keptcols),which(rownames(ecoMat) %in% keptcols)]
          geoMat = geoMat[which(colnames(geoMat) %in% keptcols),which(rownames(geoMat) %in% keptcols)]
          
          Xmats <- list(geography=as.matrix(geoMat),ecology=as.matrix(ecoMat))
          name_gene = basename(gene)
          # Run MMRR function using genMat as the response variable and Xmats as the explanatory variables.
          # nperm does not need to be specified, default is nperm=999)
          mmrr=NULL
          try({mmrr=MMRR(as.matrix(genMat),Xmats,nperm=999)})
          if(is.null(mmrr)){
            
            newrow=cbind(name_gene,NA,NA,NA,NA,NA,NA)
            
          } else {
            #print(mmrr)
            
            
            newrow=cbind(name_gene,mmrr$r.squared,mmrr$coefficients[names(mmrr$coefficients)=="geography"],
                         mmrr$coefficients[names(mmrr$coefficients)=="ecology"],mmrr$tpvalue[names(mmrr$tpvalue)=="geography(p)"],
                         mmrr$tpvalue[names(mmrr$tpvalue)=="ecology(p)"],mmrr$Fpvalue)
          }
          colnames(newrow) = c("spp-gene","rsq","geography_coef","ecology_coef","geography_p","ecology_p","overall_p")
          
          big_difference_df = rbind(big_difference_df,newrow)
        }
        
      }
    }
    
    setwd(gene_folder)
    write.table(big_difference_df,"gene_distances_mmrr.txt",row.names = F,quote = F)
    
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



