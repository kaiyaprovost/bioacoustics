centdf
for(genus in unique(centdf$genus)){
  print(genus)
  gendf = centdf[centdf$genus==genus,]
  ##genus
  gendist = as.matrix(dist(gendf[,c(6:10)]))
  colnames(gendist) = gendf$Ind
  rownames(gendist) = gendf$Ind
  write.table(gendist,paste("~/mean_centoid_distances_individuals_MASTER_genus_",genus,".txt",sep=""),sep="\t",row.names = T,quote=F)
  
  for(species in unique(gendf$species)){
    print(paste(genus,species))
    ## scientific
    thisspp = which(gendf$species==species)
    sppdist=gendist[thisspp,thisspp]
    write.table(sppdist,paste("~/mean_centoid_distances_individuals_MASTER_scientific_",genus,".",species,".txt",sep=""),sep="\t",row.names = T,quote=F)
    
    if(length(unique(gendf$subspecies))>1){
      for(subspecies in unique(gendf$subspecies)){
        print(paste(genus,species,subspecies))
        thissub = which(gendf$subspecies==subspecies & gendf$species==species)
        if(length(thissub)>0){
          subdist=gendist[thissub,thissub]
          ##withsubspecies
          write.table(sppdist,paste("~/mean_centoid_distances_individuals_MASTER_withsubspecies_",genus,".",species,".",subspecies,".txt",sep=""),sep="\t",row.names = T,quote=F)
        }
      }
    }
  }
}





cents_qual = read.table("/Users/kprovost/Documents/Postdoc_Working/Sounds_and_Annotations/Aves/Passeriformes/Oscines/Passerellidae/Zonotrichia/leucophrys/centroid_locations_per_individual_QUALITY.txt",header=T)
ALL_dist = as.matrix(dist(cents_qual[,1:5],diag=T,upper=T))
ab_dist = as.matrix(dist(cents_qual[cents_qual$quality %in% c("A","B"),1:5],diag=T,upper=T))
abc_dist = as.matrix(dist(cents_qual[cents_qual$quality %in% c("A","B","C"),1:5],diag=T,upper=T))
abcd_dist = as.matrix(dist(cents_qual[cents_qual$quality %in% c("A","B","C","D"),1:5],diag=T,upper=T))
abcde_dist = as.matrix(dist(cents_qual[cents_qual$quality %in% c("A","B","C","D","E"),1:5],diag=T,upper=T))
colnames(ALL_dist) = cents_qual$Ind
colnames(ab_dist) = cents_qual$Ind[cents_qual$quality %in% c("A","B")]
colnames(abc_dist) = cents_qual$Ind[cents_qual$quality %in% c("A","B","C")]
colnames(abcd_dist) = cents_qual$Ind[cents_qual$quality %in% c("A","B","C","D")]
colnames(abcde_dist) = cents_qual$Ind[cents_qual$quality %in% c("A","B","C","D","E")]
rownames(ALL_dist) = colnames(ALL_dist)
rownames(ab_dist) = colnames(ab_dist)
rownames(abc_dist) = colnames(abc_dist)
rownames(abcd_dist) = colnames(abcd_dist)
rownames(abcde_dist) = colnames(abcde_dist)
write.table(ALL_dist,"/Users/kprovost/Documents/Postdoc_Working/Sounds_and_Annotations/Aves/Passeriformes/Oscines/Passerellidae/Zonotrichia/leucophrys/mean_centroid_distances_individuals_qABCDEUNK.txt")
write.table(ab_dist,"/Users/kprovost/Documents/Postdoc_Working/Sounds_and_Annotations/Aves/Passeriformes/Oscines/Passerellidae/Zonotrichia/leucophrys/mean_centroid_distances_individuals_qAB.txt")
write.table(abc_dist,"/Users/kprovost/Documents/Postdoc_Working/Sounds_and_Annotations/Aves/Passeriformes/Oscines/Passerellidae/Zonotrichia/leucophrys/mean_centroid_distances_individuals_qABC.txt")
write.table(abcd_dist,"/Users/kprovost/Documents/Postdoc_Working/Sounds_and_Annotations/Aves/Passeriformes/Oscines/Passerellidae/Zonotrichia/leucophrys/mean_centroid_distances_individuals_qABCD.txt")
write.table(abcde_dist,"/Users/kprovost/Documents/Postdoc_Working/Sounds_and_Annotations/Aves/Passeriformes/Oscines/Passerellidae/Zonotrichia/leucophrys/mean_centroid_distances_individuals_qABCDE.txt")
