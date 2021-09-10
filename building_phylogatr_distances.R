## TO DO: fix color scheme on mean/stdv plots. output mean/stdev plots 

library(pegas)
library(raster)
library(tidyr)
library(gstat)

interpolate=F
haplotype=F
njtree=F
outputraster=F
diversity=F
relative_diversity=T
relative_diversity_positive=T

occ = read.table("/Users/kprovost/OneDrive - The Ohio State University/Phylogatr_Data/Birds-phylogatr-results_7dec2020/ALLONEFILE/master_occurrence.txt",
                 header=T,sep="\t")
occ$pop = paste(occ$accession,occ$gbif_id,sep="_")


cols=colorRampPalette(RColorBrewer::brewer.pal(9,"Blues"))




background = raster::raster("/Users/kprovost/OneDrive - The Ohio State University/raw_bbs_data/bio1/bio1.bil")
bg = background 
bg[!(is.na(bg))] = 0
writeRaster(bg,"/Users/kprovost/OneDrive - The Ohio State University/blank_worldclim_.asc",format="ascii")

#bg=raster::aggregate(bg,40)
#writeRaster(bg,"/Users/kprovost/OneDrive - The Ohio State University/blank_worldclim_agg40.asc",format="ascii")

bg=raster::aggregate(bg,100)
writeRaster(bg,"/Users/kprovost/OneDrive - The Ohio State University/blank_worldclim_agg100.asc",format="ascii")
bg = raster("/Users/kprovost/OneDrive - The Ohio State University/blank_worldclim_agg100.asc")

# plot(bg)

bg_all = bg
cells=raster::cellFromXY(bg,occ[,c("longitude","latitude")])
occ$cells = cells
celltab=table(cells)
celltab_log = log10(celltab)
bg_all[as.numeric(names(celltab))] = as.numeric(celltab)
#raster::writeRaster(bg_all,"/Users/kprovost/OneDrive - The Ohio State University/Phylogatr_Data/sampling_across_world.asc",
#                    format="ascii",overwrite=T)
# plot(bg,colNA="grey")
#png("/Users/kprovost/OneDrive - The Ohio State University/Phylogatr_Data/sampling_across_NorthAmerica.png")
#plot(bg_all,colNA="grey",col=c("pink",cols(100)),
#     #ylim=c(0,90),xlim=c(-180,0)
#)
#dev.off()


bg_log = bg
bg_log[!(is.na(bg_log))] = -0.1
bg_log[as.numeric(names(celltab_log))] = as.numeric(celltab_log)
#png("/Users/kprovost/OneDrive - The Ohio State University/Phylogatr_Data/sampling_across_NorthAmerica_LOG.png")
#plot(bg_log,colNA="grey",breaks=c(-0.5,0,0.5,1,1.5,2,2.5,3),
#     col=c("pink",cols(7))#,ylim=c(0,90),xlim=c(-180,0)
#)
#dev.off()

folder = "/Users/kprovost/OneDrive - The Ohio State University/Phylogatr_Data/Birds-phylogatr-results_7dec2020/Aves/"
fasta_files = list.files(path=folder,pattern=".afa$",full.names = T,recursive = T)
## 2266

#total_nuc_div_outfile="/Users/kprovost/OneDrive - The Ohio State University/Phylogatr_Data/total_nucleotide_diversity.temp"
total_haplo_outfile="/Users/kprovost/OneDrive - The Ohio State University/Phylogatr_Data/total_haplotypes.temp"

all_nuc_div = read.table("/Users/kprovost/OneDrive - The Ohio State University/Phylogatr_Data/total_nucleotide_diversity.txt",sep="\t",
                         header=T)
all_haplo = read.table("/Users/kprovost/OneDrive - The Ohio State University/Phylogatr_Data/total_haplotypes.txt",sep="\t",
                       header=T)

for(fasta in fasta_files[1:length(fasta_files)]) {
  print(fasta)
  
  data = ape::read.dna(fasta,format="fasta")
  fasta_accessions = labels(data)
  zono = occ[occ$pop %in% fasta_accessions,c("accession","latitude","longitude","gbif_id","pop","cells")]
  total_nuc_div = pegas::nuc.div(data,variance=F,pairwise.deletion=T)
  
  #write(paste(fasta,total_nuc_div,sep="\t"),total_nuc_div_outfile,append=T)
  
  if(njtree==T) {
    # plot(zono$longitude,zono$latitude,col="red")
    
    distances=ape::dist.gene(data,method="percentage",pairwise.deletion = T,variance = T)
    plot(ape::nj(distances),type="phylogram",align.tip.label=T)
    corrplot::corrplot(as.matrix(distances),is.corr=F,method="color",order="hclust")
  }
  
  zono_haps = NULL
  
  h <- pegas::haplotype(data,strict=F)
  h <- sort(h, what = "label")
  
  number_haplotypes = length(summary(h))
  #print(number_haplotypes)
  
  write(paste(fasta,number_haplotypes,sep="\t"),total_haplo_outfile,append=T)
  
  if(haplotype==T) {
    if(number_haplotypes > 1) {
      
      #(net <- pegas::haploNet(h,getProb = F))
      ind.hap<-with(
        stack(setNames(attr(h, "index"), rownames(h))),
        table(hap=ind, pop=rownames(data)[values])
      )
      #plot(net, size=attr(net, "freq"), scale.ratio=0.2, pie=ind.hap)
      #legend(-8, 0, colnames(ind.hap), col=rainbow(ncol(ind.hap)), pch=19, ncol=2)
      haps=as.matrix(t(ind.hap))
      zono_haps=merge(haps,zono)
      zono_haps = zono_haps[zono_haps$Freq!=0,]
      
      
      
      haplos_per_cell=colSums(table(unique(zono_haps[,c("hap","cells")])))
      normalized_haplos = haplos_per_cell/number_haplotypes
      haploraster = bg
      haploraster[!(is.na(haploraster))] = NA
      
      for(i in 1:length(normalized_haplos)){
        name = names(normalized_haplos)[i]
        cell = as.numeric(name)
        haploraster[cell] = as.numeric(normalized_haplos[i])
      }
      
      if(outputraster==T) {
        raster::writeRaster(haploraster,paste(fasta,"_norm_numhaplo.asc",sep=""),
                            format="ascii",overwrite=T)
      }
      
    } 
  }
  
  if(!(is.null(zono_haps))){
    zono = zono_haps
  }
  
  if(diversity==T) {
    cells=zono$cells
    
    if(sum(!is.na(cells))>0) {
      
      celltab=table(cells)
      bg[as.numeric(names(celltab))] = as.numeric(celltab)
      
      # plot(bg)
      
      # palette(c("black","red","cyan","goldenrod","green"))
      # plot(bg,add=F,ylim=c(0,90),xlim=c(-180,0),col=cols(7),colNA="grey")
      # points(zono$longitude,zono$latitude,pch=1)
      # points(jitter(zono_haps$longitude,250),jitter(zono_haps$latitude,250),col=as.numeric(as.factor(zono_haps$hap)),
      #        pch=16)
      
      divraster = bg
      divraster[!(is.na(divraster))] = -0.001
      
      diversity=(zono[,c("latitude","longitude","cells")])
      diversity = aggregate(cbind(diversity$latitude,diversity$longitude)~diversity$cells,FUN=function(x){mean(x,na.rm=T)})
      colnames(diversity) = c("cells","latitude","longitude")
      diversity$newdiv = NA
      diversity$N = 0
      
      for(cell in sort(unique(cells))) {
        accs = zono$pop[zono$cells==cell]
        accs = accs[complete.cases(accs)]
        diversity$N[diversity$cells==cell] = length(accs)
        if(length(accs)>1){
          newdiv = nuc.div(data[accs,],variance=F,pairwise.deletion = T)
        } else {
          newdiv = NA
        }
        divraster[cell] = newdiv
        diversity$newdiv[diversity$cells==cell] = newdiv
        
      }
      if (max(diversity$N,na.rm=T) > 1) {
        
        if(outputraster==T) {
          divraster[divraster==-0.001] = NA
          writeRaster(divraster,paste(fasta,"_nucdiv.asc",sep=""),
                      format="ascii",overwrite=T)
        }
        
        # plot(divraster,ylim=c(0,90),xlim=c(-180,0),colNA="black",
        #     breaks=c(-0.001,-0.000001,seq(0,0.003,length.out=5)),
        #     col=c("pink",cols(7)))
        
        
        
        
        
        if(interpolate==T){
          
          ## set up the raster stuff for interpolating 
          ras=bg
          values(ras)[!(is.na(values(ras)))] = 1
          
          x_range <- as.numeric(c(extent(ras)[1], extent(ras)[2]))  # min/max longitude of the interpolation area
          y_range <- as.numeric(c(extent(ras)[3], extent(ras)[4]))  # min/max latitude of the interpolation area
          # create an empty grid of values ranging from the xmin-xmax, ymin-ymax
          ## convert the grd by an order of mag
          grd <- expand.grid(x = seq(from = x_range[1],
                                     to = x_range[2], 
                                     by = res(ras)[1]*10),
                             y = seq(from = y_range[1],                                           
                                     to = y_range[2], 
                                     by = res(ras)[2]*10))  # expand points to grid
          # class(grd)
          # Convert grd object to a matrix and then turn into a spatial
          # points object
          coordinates(grd) <- ~x + y
          # turn into a spatial pixels object
          gridded(grd) <- TRUE
          
          
          temp = diversity[,c("longitude","latitude","newdiv")]
          colnames(temp) = c("LON","LAT","NUM")
          temp=temp[complete.cases(temp),]
          
          if(nrow(temp) > 0){
            
            coordinates(temp) <- ~LON + LAT
            idw_pow1 <- idw(formula = NUM ~ 1,
                            locations = temp,
                            newdata = grd,
                            idp = 5)
            idw = (raster(idw_pow1, layer=1, values=TRUE))
            
            if(sd(values(idw),na.rm=T)>0) {
              
              # plot(idw)
              idw2 = resample(idw,ras)
              # plot(idw2)
              extent(idw2)
              extent(ras)
              cropped = idw2*ras
              
              png(paste("/Users/kprovost/OneDrive - The Ohio State University/Phylogatr_Data/interpolated/",basename(fasta),".interpolated_nucdiv.png",sep=""))
              plot(cropped,#ylim=c(0,90),xlim=c(-180,0),
                   col=cols(100),colNA="grey")
              points(zono$longitude,zono$latitude,pch=1,col="red")
              dev.off()
            }
          }
        }
        
        
        
      }
    }
  }
  
  if(relative_diversity==T) {
    
    splitfasta = (strsplit(basename(fasta),"-")[[1]])
    species=paste(splitfasta[1:2],collapse="-")
    gene=paste(splitfasta[3:length(splitfasta)],sep="",collapse="")
    gene=strsplit(gene,"\\.")[[1]][1]
    
    full_nucdiv = all_nuc_div$NUCLEOTIDE_DIVERSITY[all_nuc_div$SCIENTIFIC==species & all_nuc_div$GENE==gene]
    cells=zono$cells
    #difference_raster = bg
    #difference_raster[!(is.na(difference_raster))] = NA
    #relative_raster = bg
    #relative_raster[!(is.na(relative_raster))] = NA
    proportion_raster = bg
    proportion_raster[!(is.na(proportion_raster))] = NA
    for(cell in sort(unique(cells))) {
      #print(cell)
      ## include and exclude that cell
      #accs_exc = zono$pop[zono$cells!=cell]
      accs_inc = zono$pop[zono$cells==cell]
      #accs_exc = accs_exc[complete.cases(accs_exc)]
      accs_inc = accs_inc[complete.cases(accs_inc)]
      #newdiv_exc = nuc.div(data[accs_exc,],variance=F,pairwise.deletion = T)
      newdiv_inc = nuc.div(data[accs_inc,],variance=F,pairwise.deletion = T)
      #difference = newdiv_exc-full_nucdiv
      #relativedifference = difference/(newdiv_exc+full_nucdiv)
      #difference_raster[cell] = difference
      #relative_raster[cell] = relativedifference
      
      proportion = newdiv_inc/full_nucdiv
      proportion_raster[cell] = proportion
    }
    
    #raster::writeRaster(difference_raster,paste(fasta,"_nucdiv_difference.asc",sep=""),
    #                    format="ascii",overwrite=T)
    #raster::writeRaster(relative_raster,paste(fasta,"_nucdiv_relative_difference.asc",sep=""),
    #                    format="ascii",overwrite=T)
    raster::writeRaster(proportion_raster,paste(fasta,"_nucdiv_proportion.asc",sep=""),
                        format="ascii",overwrite=T)
    
    
    
    
  }
  

  
}

## get the ascii files in a stack
genes = c("AK1", "ATP6", "ATP8", "BRM", "CHD","CHDW","CHDZ", "COII", "COIII", 
          "DRD4", "EEF2", "ENO1", "FGB", "G3PDH", "GAPDH", "GH1", "GRIN1", "HBA", 
          "HBA1", "HBA2", "HBB", "HBBA", "HBD", "LDHA", "LMNA", "MC1R",
          "MUSK", 
          "MX", "MY2", "MYC", "ND1",
          "ND3", "ND4", "ND5", "ND6", "OD",
          "ODC", "ODC1", "PCK1", "PEPCK", "RHO", "RI2", "SERT", "SLC30A5", 
          "TGFB2", "VLDL9R","ND2","CYTB","COI")
filetype = "proportion" ## can be nucdiv, nucdiv_difference, numhaplo, nucdiv_relative_difference, proportion
ascii_files = list.files(path=folder,pattern=paste(filetype,".asc$",sep=""),full.names = T,recursive = T)

## for NUCDIV only
for(gene in genes) {
  print(gene)
  ascii_subset = ascii_files[grepl(gene,ascii_files)]
  print(length(ascii_subset))
  
  if(length(ascii_subset)>0){
    stack = raster::stack(ascii_subset)
    raster::writeRaster(stack,filename=paste("/Users/kprovost/OneDrive - The Ohio State University/Phylogatr_Data/",gene,"_",filetype,"_rasters.tif",sep=""),
                        format="GTiff",overwrite=T)
    
    print("means")
    means = raster::calc(stack,fun=function(x){mean(x,na.rm=T)})
    raster::writeRaster(means,filename=paste("/Users/kprovost/OneDrive - The Ohio State University/Phylogatr_Data/",gene,"_",filetype,"_MEAN.asc",sep=""),
                        format="ascii",overwrite=T)
    print("sds")
    sds = raster::calc(stack,fun=function(x){sd(x,na.rm=T)})
    raster::writeRaster(sds,filename=paste("/Users/kprovost/OneDrive - The Ohio State University/Phylogatr_Data/",gene,"_",filetype,"_STDEV.asc",sep=""),
                        format="ascii",overwrite=T)
    
    pdf(paste("/Users/kprovost/OneDrive - The Ohio State University/Phylogatr_Data/",gene,"_",filetype,"_rasters.pdf",sep=""))
    plot(bg,col="grey",colNA="black",main=paste("means, N =",length(ascii_subset)),legend=F)
    plot(means,col=cols(100)[25:100],add=T,zlim=c(0,1))
    plot(bg,col="black",colNA="pink",main=paste("sdvs, N =",length(ascii_subset)),legend=F)
    plot(sds,col=cols(100)[25:100],add=T)
    dev.off()
  } 
}

## TRY TO CONTRAST COI NUCDIV WITH COI NUMHAPLO

sampling="/Users/kprovost/OneDrive - The Ohio State University/Phylogatr_Data/sampling_across_world.asc"
sm = raster::raster(sampling)
smv = raster::values(sm)

## for NUMHAPLO only
for ( gene in genes[40:length(genes)] ) {
  
  print(gene)
  
  numhaplo=paste("/Users/kprovost/OneDrive - The Ohio State University/Phylogatr_Data/numhaplo_rasters/",gene,"_numhaplo_MEAN.asc",sep="")
  nucdiv=paste("/Users/kprovost/OneDrive - The Ohio State University/Phylogatr_Data/nucdiv_rasters/",gene,"_nucdiv_MEAN.asc",sep="")
  
  nh = raster::raster(numhaplo)
  nd = raster::raster(nucdiv)
  
  nhv = raster::values(nh)
  ndv = raster::values(nd)
  
  #cor(nhv,ndv,use="pairwise.complete.obs")
  #cor(smv,ndv,use="pairwise.complete.obs")
  #cor(smv,nhv,use="pairwise.complete.obs")
  
  #mod=lm(ndv~nhv)
  #print(summary(mod)$coefficients[,4]<0.05) # p = 0.21 adjr = 0.0007
  
  #mod=lm(ndv~smv)
  #print(summary(mod)$coefficients[,4]<0.05) # p = 0.62 adjr = -0.001
  
  #mod=lm(nhv~smv)
  #print(summary(mod)$coefficients[,4]<0.05) # p = 1.27e-9 adjr = 0.02
  
  #mod=lm(nhv~smv+ndv)
  #print(summary(mod)$coefficients[,4]<0.05) # smv p=0.0005 ndv p=0.18 adjr = 0.016
  
  mod=lm(ndv~smv+nhv)
  print(summary(mod)$coefficients[,4]<0.05) # smv p=0.0005 ndv p=0.18 adjr = 0.016
  
  
}

png("test.png")
plot(smv,nhv)
dev.off()


## looking at change in pi when remove a value? 
## this works for both the difference and relative difference
## will also work for proportion?
for(gene in c("COI")) {
  print(gene)
  ascii_subset = ascii_files[grepl(gene,ascii_files)]
  print(length(ascii_subset))
  
  if(length(ascii_subset)>0){
    
    if(file.exists(paste("/Users/kprovost/OneDrive - The Ohio State University/Phylogatr_Data/",gene,"_",filetype,"_rasters.tif",sep=""))) {
      stack = raster(paste("/Users/kprovost/OneDrive - The Ohio State University/Phylogatr_Data/",gene,"_",filetype,"_rasters.tif",sep=""))
    } else {
      stack = raster::stack(ascii_subset)
      raster::writeRaster(stack,filename=paste("/Users/kprovost/OneDrive - The Ohio State University/Phylogatr_Data/",gene,"_",filetype,"_rasters.tif",sep=""),
                          format="GTiff",overwrite=T)
    }

    print("means")
    if(file.exists(paste("/Users/kprovost/OneDrive - The Ohio State University/Phylogatr_Data/",gene,"_",filetype,"_MEAN.asc",sep=""))) {
      means=raster(paste("/Users/kprovost/OneDrive - The Ohio State University/Phylogatr_Data/",gene,"_",filetype,"_MEAN.asc",sep=""))
    } else {
      means = raster::calc(stack,fun=function(x){mean(x,na.rm=T)})
      raster::writeRaster(means,filename=paste("/Users/kprovost/OneDrive - The Ohio State University/Phylogatr_Data/",gene,"_",filetype,"_MEAN.asc",sep=""),
                          format="ascii",overwrite=T)
    }
    print("sds")
    if(file.exists(paste("/Users/kprovost/OneDrive - The Ohio State University/Phylogatr_Data/",gene,"_",filetype,"_STDEV.asc",sep=""))) {
      sds=raster(paste("/Users/kprovost/OneDrive - The Ohio State University/Phylogatr_Data/",gene,"_",filetype,"_STDEV.asc",sep=""))
    } else {
      sds = raster::calc(stack,fun=function(x){sd(x,na.rm=T)})
      raster::writeRaster(sds,filename=paste("/Users/kprovost/OneDrive - The Ohio State University/Phylogatr_Data/",gene,"_",filetype,"_STDEV.asc",sep=""),
                          format="ascii",overwrite=T)
    }
    
    pdf(paste("/Users/kprovost/OneDrive - The Ohio State University/Phylogatr_Data/",gene,"_",filetype,"_rasters.pdf",sep=""))
    plot(bg,col="pink",colNA="grey",main=paste("means, N =",length(ascii_subset)),legend=F)
    plot(means,col=cols(100),add=T,zlim=c(0,1))
    plot(bg,col="pink",colNA="grey",main=paste("sdvs, N =",length(ascii_subset)),legend=F)
    plot(sds,col=cols(100),add=T)
    dev.off()
  } 
}

