timefile = "/Users/kprovost/Documents/Postdoc_Working/MMRR/song_metadata_edited_11july2022_Passerellidae.txt"
time = read.table(timefile,header=T,sep="\t")
time = time[,c("COLLECTION","ID","GENUS","SPECIES","SUBSPECIES","YEAR")]
time$YEAR = as.numeric(time$YEAR)
time$SUBSPECIES[time$SUBSPECIES==""] = "unknown"
#hist(time$YEAR)
#plot(time$ID,time$YEAR,col=as.numeric(as.factor(time$COLLECTION)))
time$name = paste(time$GENUS,time$SPECIES,time$SUBSPECIES,time$COLLECTION,time$ID,sep=".")
#timedist = dist(time$YEAR,diag=T,upper=T)

for(genus in sort(unique(time$GENUS))) {
  print(genus)
  g_df = time[time$GENUS==genus,]
  g_df_dist = as.matrix(dist(g_df$YEAR))
  colnames(g_df_dist) = g_df$name
  rownames(g_df_dist) = g_df$name
  write.table(g_df_dist,paste(timefile,".",genus,".TIME.dist.txt",sep=""),quote = F,sep="\t",row.names = F)
  
  for(species in sort(unique(g_df$SPECIES))){
    print(paste(genus,species))
    sp_df = g_df[g_df$SPECIES==species,]
    sp_df_dist = as.matrix(dist(sp_df$YEAR))
    colnames(sp_df_dist) = sp_df$name
    rownames(sp_df_dist) = sp_df$name
    write.table(sp_df_dist,paste(timefile,".",genus,".",species,".TIME.dist.txt",sep=""),quote = F,sep="\t",row.names = F)
    
    allsub = sort(unique(sp_df$SUBSPECIES))
    allsub=allsub[allsub!="unknown"]
    if(length(allsub)>0){
      for(subspp in allsub){
        print(paste(genus,species,subspp))
        sub_df = sp_df[sp_df$SUBSPECIES==subspp,]
        sub_df_dist = as.matrix(dist(sub_df$YEAR))
        colnames(sub_df_dist) = sub_df$name
        rownames(sub_df_dist) = sub_df$name
        write.table(sub_df_dist,paste(timefile,".",genus,".",species,".",subspp,".TIME.dist.txt",sep=""),quote = F,sep="\t",row.names = F)
        
      }
    } 
  }
}

