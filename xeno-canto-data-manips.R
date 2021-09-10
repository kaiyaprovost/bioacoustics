df = read.table("/Users/kprovost/OneDrive - The Ohio State University/Combined_Datasets/xc-northamerica-songs.txt",header=T)


agg = aggregate(df$num ~df$all_spp,FUN=sum)
colnames(agg) = c("spp","num")
write.table(agg,file="/Users/kprovost/OneDrive - The Ohio State University/Combined_Datasets/xc-northamerica-songs_SUMBYSPECIES.txt",row.names = F,sep="\t")

df = read.table("/Users/kprovost/OneDrive - The Ohio State University/Phylogatr_Data/Birds-phylogatr-results_7dec2020/genes.txt",header=T,sep="\t")
agg = aggregate(df$num_seqs_aligned ~df$species,FUN=sum)
colnames(agg) = c("spp","num")
write.table(agg,file="/Users/kprovost/OneDrive - The Ohio State University/Phylogatr_Data/Birds-phylogatr-results_7dec2020/genes_SUMBYSPECIES.txt",row.names = F,sep="\t")

df1 = read.table("/Users/kprovost/OneDrive - The Ohio State University/Combined_Datasets/xc-northamerica-songs_SUMBYSPECIES.txt",header=T,sep="\t")
df2 = read.table("/Users/kprovost/OneDrive - The Ohio State University/Phylogatr_Data/Birds-phylogatr-results_7dec2020/genes_SUMBYSPECIES.txt",header=T,sep="\t")

colnames(df1) = c("spp","songnum")
colnames(df2) = c("spp","genenum")

merged=merge(df1,df2,all = T)
write.table(merged,file="/Users/kprovost/OneDrive - The Ohio State University/Combined_Datasets/xc-northamerica-songs_genes_MERGED.txt",row.names = F,sep="\t")

df = read.table("/Users/kprovost/OneDrive - The Ohio State University/Combined_Datasets/xc-northamerica-songs_genes_MERGED.txt",header=T,sep="\t")
df$genus = unlist(sapply(df$spp,FUN=function(x){strsplit(x," ")[[1]][1]}))

agg = aggregate(cbind(df$songnum,df$genenum)~df$genus,FUN=sum)
colnames(agg) = c("genus","songnum","genenum")
write.table(agg,file="/Users/kprovost/OneDrive - The Ohio State University/Combined_Datasets/xc-northamerica-songs_genes_MERGED_GENUS.txt",row.names = F,sep="\t")
