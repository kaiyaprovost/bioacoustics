rm(list = ls())

outpath = "/Users/kprovost/Documents/Postdoc_Working/MMRR/WAVS/Wave/0STATSDONE/"
path="/Users/kprovost/Documents/Postdoc_Working/MMRR/WAVS/Wave/0STATSDONE/"

setwd(path)
date=format(Sys.time(), "%d%b%Y")

doPCA=F
doFull=F
centroid_file = "/Users/kprovost/Documents/Postdoc_Working/MMRR/WAVS/Wave/0STATSDONE/centroid_locations_per_individual_08Mar2023.txt"

if(doPCA==T){
  df = data.table::fread(centroid_file,header=T,data.table=F)
  
  ## make a pca
  pca = prcomp(df[,1:5],center=T,scale.=T)
  pca_rot=pca$rotation
  pca_dat=pca$x
  pca_imp=summary(pca)$importance
  
  df_pca = cbind(df,pca_dat)
  write.table(df_pca,paste(centroid_file,"_PCA-DATA.txt",sep=""),sep="\t",quote=F,row.names = F)
  write.table(pca_rot,paste(centroid_file,"_PCA-ROTATION.txt",sep=""),sep="\t",quote=F,row.names = T)
  write.table(pca_imp,paste(centroid_file,"_PCA-IMPORTANCE.txt",sep=""),sep="\t",quote=F,row.names = T)
  
  ## one raw differences and one pca differences of these metrics
} else {
  df_pca = data.table::fread(paste(centroid_file,"_PCA-DATA.txt",sep=""),header=T,data.table=F)
}

rownames(df_pca) = df_pca$Ind

if(doFull==T){
  ## try dist on raw
  raw_dist = dist(df_pca[,1:5])
  pca_dist = dist(df_pca[,12:16])
  
  raw_dist = (as.matrix(raw_dist))
  raw_dist = (as.data.frame(raw_dist))
  data.table::fwrite(raw_dist,paste(centroid_file,"_DIST-RAW.txt",sep=""),sep="\t",quote=F,row.names = T)
  
  pca_dist = (as.matrix(pca_dist))
  pca_dist = (as.data.frame(pca_dist))
  data.table::fwrite(pca_dist,paste(centroid_file,"_DIST-PCA.txt",sep=""),sep="\t",quote=F,row.names = T)
  
} else {
  for(genus in sort(unique(df_pca$genus))){
    print(genus)
    df_pca_genus = df_pca[df_pca$genus==genus,]
    raw_dist_genus = dist(df_pca_genus[,1:5])
    pca_dist_genus = dist(df_pca_genus[,12:16])
    
    raw_dist_genus = (as.matrix(raw_dist_genus))
    raw_dist_genus = (as.data.frame(raw_dist_genus))
    data.table::fwrite(raw_dist_genus,paste(centroid_file,"_DIST-RAW_",genus,".txt",sep=""),sep="\t",quote=F,row.names = T)
    
    pca_dist_genus = (as.matrix(pca_dist_genus))
    pca_dist_genus = (as.data.frame(pca_dist_genus))
    data.table::fwrite(pca_dist_genus,paste(centroid_file,"_DIST-PCA_",genus,".txt",sep=""),sep="\t",quote=F,row.names = T)
    
    for(species in sort(unique(df_pca_genus$species))){
      print(species)
      df_pca_genus_species = df_pca_genus[df_pca_genus$species==species,]
      raw_dist_genus_species = dist(df_pca_genus_species[,1:5])
      pca_dist_genus_species = dist(df_pca_genus_species[,12:16])
      
      raw_dist_genus_species = (as.matrix(raw_dist_genus_species))
      raw_dist_genus_species = (as.data.frame(raw_dist_genus_species))
      data.table::fwrite(raw_dist_genus_species,paste(centroid_file,"_DIST-RAW_",genus,".",species,".txt",sep=""),sep="\t",quote=F,row.names = T)
      
      pca_dist_genus_species = (as.matrix(pca_dist_genus_species))
      pca_dist_genus_species = (as.data.frame(pca_dist_genus_species))
      data.table::fwrite(pca_dist_genus_species,paste(centroid_file,"_DIST-PCA_",genus,".",species,".txt",sep=""),sep="\t",quote=F,row.names = T)
      
      
      for(subspecies in sort(unique(df_pca_genus_species$subspecies))){
        print(subspecies)
        df_pca_genus_species_subspecies = df_pca_genus_species[df_pca_genus_species$subspecies==subspecies,]
        raw_dist_genus_species_subspecies = dist(df_pca_genus_species_subspecies[,1:5])
        pca_dist_genus_species = dist(df_pca_genus_species_subspecies[,12:16])
        
        raw_dist_genus_species = (as.matrix(raw_dist_genus_species))
        raw_dist_genus_species = (as.data.frame(raw_dist_genus_species))
        data.table::fwrite(raw_dist_genus_species,paste(centroid_file,"_DIST-RAW_",genus,".",species,".",subspecies,".txt",sep=""),sep="\t",quote=F,row.names = T)
        
        pca_dist_genus_species = (as.matrix(pca_dist_genus_species))
        pca_dist_genus_species = (as.data.frame(pca_dist_genus_species))
        data.table::fwrite(pca_dist_genus_species,paste(centroid_file,"_DIST-PCA_",genus,".",species,".",subspecies,".txt",sep=""),sep="\t",quote=F,row.names = T)
        
      }
      
    }
    
  }
  
}

