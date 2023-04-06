## updated 1 dec 2022
## k l provost


master_coi = ("/Users/kprovost/Documents/Postdoc_Working/Genetics/Birds-phylogatr-results_7dec2020/Aves/Passeriformes/Passerellidae/master_passerellidae_COI_1Dec2022.afa")
master_cytb = "/Users/kprovost/Documents/Postdoc_Working/Genetics/Birds-phylogatr-results_7dec2020/Aves/Passeriformes/Passerellidae/master_passerellidae_CYTB_1Dec2022.afa"
master_occ_df = read.table("/Users/kprovost/Documents/Postdoc_Working/Genetics/Birds-phylogatr-results_7dec2020/Aves/Passeriformes/Passerellidae/master_passerellidae_occurrences_1Dec2022.txt",sep="\t",header=T)
full_data = "/Users/kprovost/Documents/Postdoc_Working/MMRR/Passerellidae_genetic_song_combined_occurrences.txt"
full_df = read.table(full_data,sep="\t",header=T)
full_df = full_df[full_df$COLLECTION=="genbank",]
master_occ_df = master_occ_df[,c("accession","latitude","longitude","gbif_id")]
master_occ_df=unique(master_occ_df)
colnames(master_occ_df) = c("ID","LATITUDE","LONGITUDE","gbif_id")
master_occ_df$LATITUDE=round(master_occ_df$LATITUDE,digits=6)
master_occ_df$LONGITUDE=round(master_occ_df$LONGITUDE,digits=6)
full_df$LATITUDE=round(full_df$LATITUDE,digits=6)
full_df$LONGITUDE=round(full_df$LONGITUDE,digits=6)

merged_full_master = merge(x=full_df,y=master_occ_df,all.x=T,all.y=F,by=c("ID","LATITUDE","LONGITUDE"))

coi = ape::read.dna(master_coi,format="fasta")
cytb = ape::read.dna(master_cytb,format="fasta")

full_coi = full_df[full_df$ID %in% labels(coi),]
full_cytb = full_df[full_df$ID %in% labels(cytb),]

coi_distances=ape::dist.gene(coi,method="pairwise",pairwise.deletion = T,variance = T)
cytb_distances=ape::dist.gene(cytb,method="pairwise",pairwise.deletion = T,variance = T)

coi_outfile=paste(master_coi,"_DISTANCES.txt",sep="")
coi_distances=as.data.frame(as.matrix(coi_distances))
write.table(coi_distances,coi_outfile,quote=F,row.names = T,sep="\t")

cytb_outfile=paste(master_cytb,"_DISTANCES.txt",sep="")
cytb_distances=as.data.frame(as.matrix(cytb_distances))
write.table(cytb_distances,cytb_outfile,quote=F,row.names = T,sep="\t")



ape::write.tree((ape::nj(as.dist(coi_distances))), file=paste(master_coi,"_DISTANCES.tree",sep="")) 
ape::write.tree((ape::nj(as.dist(cytb_distances))), file=paste(master_cytb,"_DISTANCES.tree",sep="")) 

## loop over species

for(genus in rev(sort(unique(full_df$GENUS)))){
  print(genus)
  full_g = full_df[full_df$GENUS==genus,]
  this_genus = full_g$ID
  try({coi_genus = coi_distances[rownames(coi_distances) %in% this_genus,colnames(coi_distances) %in% this_genus]
  write.table(coi_genus,paste(master_coi,"GENUS",genus,"DISTANCES.txt",sep="_"),quote=F,row.names = T,sep="\t")})
  try({cytb_genus = cytb_distances[rownames(cytb_distances) %in% this_genus,colnames(cytb_distances) %in% this_genus]
  write.table(cytb_genus,paste(master_cytb,"GENUS",genus,"DISTANCES.txt",sep="_"),quote=F,row.names = T,sep="\t")})
  
  for(species in rev(sort(unique(full_g$SPECIES)))){
    print(species)
    full_s = full_g[full_g$SPECIES==species,]
    this_species = full_s$ID
    try({coi_species = coi_genus[rownames(coi_genus) %in% this_species,colnames(coi_genus) %in% this_species]
    write.table(coi_species,paste(master_coi,"GENUS",genus,"SPECIES",species,"DISTANCES.txt",sep="_"),quote=F,row.names = T,sep="\t")})
    try({cytb_species = cytb_genus[rownames(cytb_genus) %in% this_species,colnames(cytb_genus) %in% this_species]
    write.table(cytb_species,paste(master_cytb,"GENUS",genus,"SPECIES",species,"DISTANCES.txt",sep="_"),quote=F,row.names = T,sep="\t")})
    
  }
}
# 
# 
# 
# 
# write.table(merged_full_master,"~/test.txt")
# 
# gene_folder = "~/Documents/Postdoc_Working/Genetics/Birds-phylogatr-results_7dec2020/Aves/Passeriformes/Passerellidae/"
# occ_list = list.files(gene_folder,pattern="occurrences.txt",recursive=T,full.names = T)
# occ_list = occ_list[grepl("Zonotrichia-leucophrys",occ_list)]
# 
# 
# big_difference_df = data.frame()
# 
# for(i in 1:length(occ_list)){
#   print(paste("OCC",i))
#   occ = occ_list[i]
#   occ_directory=dirname(occ)
#   occdf=NULL
#   try({occdf = read.table(occ,header=T,sep="\t")}) ## failing
#   if(!(is.null(occdf))){
#     occdf$pop = paste(occdf$accession,occdf$gbif_id,sep="_")
#     gene_list = list.files(occ_directory,pattern=".afa$",full.names = T,recursive = F)
#     smaller_full = full_df[full_df$ID %in% occdf$accession,c("ID","LATITUDE","LONGITUDE","COLLECTION")]
#     colnames(smaller_full) = c("accession","latitude","longitude","COLLECTION")
#     
#     for(gene in gene_list){
#       print(paste("GENE",basename(gene)))
#       data = ape::read.dna(gene,format="fasta")
#       fasta_accessions = labels(data)
#       zono = occdf[occdf$pop %in% fasta_accessions,c("accession","latitude","longitude","gbif_id","pop")]
#       zono = merge(zono,smaller_full)
#       
#       #total_nuc_div = pegas::nuc.div(data,variance=F,pairwise.deletion=T)
#       distances=ape::dist.gene(data,method="pairwise",pairwise.deletion = T,variance = T)
#       ## WRITE THESE OUT!!!
#       outfile=paste(gene,"_DISTANCES.txt",sep="")
#       distances=as.data.frame(as.matrix(distances))
#       distances = distances[which(rownames(distances) %in% zono$pop),which(colnames(distances) %in% zono$pop)]
#       write.table(distances,outfile,quote=F,row.names = T,sep="\t")
#       
#     }
#   }
# }
