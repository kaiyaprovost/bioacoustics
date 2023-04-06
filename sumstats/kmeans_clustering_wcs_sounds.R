## AMPLITUDE MATTERS FOR THE PCAS


filenames = list.files(path="/Users/kprovost/OneDrive - The Ohio State University/BLB_Data/",
                       pattern="*Table.1.selections.txt$",full.names = T,recursive=T)

# for(file in filenames) {
#   ## if file does not exist make it?
#   newfile = paste(file,"_short.txt",sep="")
#   csv = data.table::fread(file,sep="\t",header=T,fill=T)
#   csv = csv[,1:7]
#   write.table(csv,newfile,sep="\t",row.names = F,quote = F)
# }

NUM_K=2

df_list = lapply(filenames,FUN=function(x){
  csv = data.table::fread(file=x,sep="\t",header=T,fill=T)
  return(csv)
})
merged = plyr::rbind.fill(df_list)
merged = unique(merged)

merged=merged[,(which(colnames(merged)!="type"))]

## remove any columns which are entirely na
merged = merged[,which(colSums(is.na(merged)) != nrow(merged))]
merged = unique(merged)

anno = (merged[complete.cases(merged),])

anno_kmeans = kmeans(anno[,c(4:14,16:17,19:27,29,31:37,43:49,51:56,58:60,62:70)],centers=NUM_K)

#anno=read.table("/Users/kprovost/OneDrive - The Ohio State University/BLB_Data/batch3/annotated/BLB7412.Table.1.selections.txt",sep="\t",header=T)
summary(anno)

numeric_for_pca=c()


for(i in 1:length(colnames(anno))) {
  print(i)
  if(length(unique(as.numeric(anno[,i])))>1) {
    #hist(as.numeric(anno[,i]),main=colnames(anno)[i])
    numeric_for_pca = c(numeric_for_pca,i)
  }
  
}

## drop all but one of the "begin times", "end times", "selection", "time.#", "file.offset","MAXTIME","PEAKTIME", etc
#numeric_for_pca=numeric_for_pca[!(numeric_for_pca %in% which(grepl("time",colnames(anno),ignore.case = T)))]
numeric_for_pca=numeric_for_pca[!(numeric_for_pca %in% which(grepl("beg",colnames(anno),ignore.case = T)))]
numeric_for_pca=numeric_for_pca[!(numeric_for_pca %in% which(grepl("file",colnames(anno),ignore.case = T)))]
numeric_for_pca=numeric_for_pca[!(numeric_for_pca %in% which(grepl("end",colnames(anno),ignore.case = T)))]
numeric_for_pca=numeric_for_pca[!(numeric_for_pca %in% which(grepl("sample",colnames(anno),ignore.case = T)))]

## highly correlated
cols_to_drop = c("Dur 50% (s)","Length (frames)","SNR NIST Quick (dB FS)","Selection",
                 "Low Freq (Hz)","High Freq (Hz)","Max Freq (Hz)","Freq 95% (Hz)",
                 "Peak Power Density (dB FS)","Center Freq (Hz)","Avg Power Density (dB FS)","Freq 5% (Hz)",
                 "Energy","Freq 75% (Hz)","Peak Freq (Hz)","Agg Entropy (bits)","BW 90% (Hz)",
                 "Time 5% (s)","Time 25% (s)","Center Time (s)","Peak Time (s)","File Offset (s)",
                 "Max Time (s)","Time 75% (s)","Dur 90% (s)","Center Time Rel.","Time 75% Rel.")


## needt to drop: dur 90%, length frames, 

for_pca = anno[,numeric_for_pca]
for_pca = for_pca[,!(colnames(for_pca) %in% cols_to_drop)]







for(wav in sort(unique(anno$`Begin File`))){
  
  palette(c("black",RColorBrewer::brewer.pal(12,"Paired")))
  
  print(wav)
  subset = anno[anno$`Begin File`==wav,]
  subset2 = subset[,numeric_for_pca]
  subset2 = subset2[,!(colnames(subset2) %in% cols_to_drop)]
  subset_pca = prcomp(subset2,center=T,scale.=T)
  subset_pca_data = subset_pca$x
  
  if(wav=="BLB11529.wav") {
    subset$type=1
  }

  plot(subset_pca_data[,1:2],#col=subset$type,pch=as.character(subset$type)
       pch=as.character(subset$Selection)
       )
  # x=read.table("/Users/kprovost/OneDrive - The Ohio State University/BLB_Data/batch3/annotated/BLB11529.Table.1.selections.txt_short.txt",header=T,sep="\t")
  plot(subset_pca_data[,1:2],col=as.numeric(as.factor(x$type)),pch=as.character(x$type))
  #x$type = subset$type
  
  
  kmeans(subset_pca_data,centers=NUM_K)
  
  subset_fcts <- Rraven::extract_ts(X = subset, ts.column = "Peak Freq Contour (Hz)",  equal.length = T)
  
  
  plot(as.numeric(subset_fcts[1,]))
  
}



pdf("~/test.pdf",height=20,width=20)
corrplot::corrplot(abs(cor(for_pca)),method="number",order="hclust",is.corr = F,
                   diag=F)
dev.off()
sort(colSums(abs(cor(for_pca))))

pca = prcomp(for_pca,center = T,scale. = T)
pca_data = pca$x

pca_data_kmeans = kmeans(pca_data,centers=NUM_K,iter.max=100)
plot(pca_data[,"PC1"],pca_data[,"PC2"],col=as.numeric(as.factor(pca_data_kmeans$cluster)))
plot(pca_data[,"PC1"],pca_data[,"PC2"],col=as.numeric(as.factor(anno_kmeans$cluster)))


plot(anno$`Delta Time (s)`,anno$`Delta Freq (Hz)`)


library("warbleR")
fcts <- Rraven::extract_ts(X = anno, ts.column = "Peak Freq Contour (Hz)",  equal.length = T)

# fcts <- Rraven::extract_ts(X = anno, ts.column = "PFC Slope (Hz/ms)",  equal.length = TRUE)
## PFC Slope (Hz/ms), Freq Contour 95% (Hz), Freq Contour 75% (Hz), Freq Contour 50% (Hz),  Freq Contour 25% (Hz)
## Freq Contour  5% (Hz)

fcts_dataonly= fcts[,3:ncol(fcts)]
fcts_kmeans = kmeans(fcts_dataonly,centers=5)
fcts_kmeans$centers




png("~/TEST.png")
plot(1,ylim=c(1000,17000),xlim=c(0,ncol(fcts_dataonly)),type="n")
#plot(1,ylim=c(-1500,3000),xlim=c(0,31),type="n")
for(i in 1:nrow(fcts_dataonly)) {
  points(as.numeric(fcts_dataonly[i,]),col=rgb(0,0,0,0.1),ylim=c(1000,17000),type="l")
  #points(as.numeric(fcts_dataonly[i,]),col=rgb(0,0,0,0.1),ylim=c(-1500,3000),type="l")
}
dev.off()

par(mfrow=c(4,5),mar=c(0,0,0,0))
for(k in 1:nrow(fcts_kmeans$centers)) {
  plot(fcts_kmeans$centers[k,],col=k,pch=k,ylim=c(1000,17000))
  #plot(fcts_kmeans$centers[k,],col=k,pch=k,ylim=c(-1500,3000))
}


par(mfrow=c(1,1),mar=c(4,4,0,0))
pca_fcts = prcomp(fcts[,3:32],center=T,scale.=T)
pca_fcts_data = pca_fcts$x
#plot(pca_fcts_data[,1:2],col=as.numeric(as.factor(fcts$sound.files)))
#dist(pca_fcts_data)


pca_kmeans = kmeans(pca_fcts_data,centers=NUM_K)
plot(pca_fcts_data[,1:2],col=as.numeric(fcts_kmeans$cluster))
plot(pca_fcts_data[,1:2],col=as.numeric(pca_kmeans$cluster))

table(fcts_kmeans$cluster,pca_kmeans$cluster)
uniqs=paste(fcts_kmeans$cluster,pca_kmeans$cluster)
plot(pca_fcts_data[,1:2],col=as.numeric(as.factor((uniqs))),pch=as.numeric(as.factor((uniqs))) %% 26)
agg = aggregate(as.matrix(fcts_dataonly) ~ uniqs, FUN=function(x){mean(x,na.rm=T)})
plot(fcts_dataonly[,1:2],col="grey")
points(agg[,2:3])

pdf("TESTTEST.pdf")
par(mfrow=c(2,5),mar=c(0,0,2,0))
for(k in 1:41) {
  print(k)
  
  subset = fcts_dataonly[uniqs==(agg[k,1]),]
  
  plot(as.numeric(agg[k,-1]),ylim=c(1000,17000),col="white",main=(agg[k,1]))
  for(i in 1:nrow(subset)) {
    #points(as.numeric(fcts_dataonly[i,]),col=rgb(0,0,0,0.1),ylim=c(1000,17000),type="l")
    points(as.numeric(subset[i,]),col=rgb(0,0,0,0.3),ylim=c(-1500,3000))
  }
  
  points(as.numeric(agg[k,-1]),ylim=c(1000,17000),col="red",pch=16)
  #plot(fcts_kmeans$centers[k,],col=k,pch=k,ylim=c(-1500,3000))
}
dev.off()

distances=dist(as.matrix(fcts_dataonly),diag=F)
fit <- hclust(distances, method="ward.D") 
pdf("test.pdf",width=100)
plot(fit,cex=0.1,lwd=0.1,hang=-0.1)
groups <- cutree(fit, k=26)
rect.hclust(fit, k=26, border="red") 
dev.off()

groups <- cutree(fit, k=2)
plot(pca_fcts_data[,1:2],col=as.numeric(groups))


## PCA kmeans pca_data_kmeans
## PCA fcts kmeans pca_kmeans
## raw fcts kmeans fcts_kmeans
## raw kmeans anno_kmeans

assignments=cbind(pca_data_kmeans$cluster,pca_kmeans$cluster,fcts_kmeans$cluster,anno_kmeans$cluster)
assignments2=paste(pca_data_kmeans$cluster,pca_kmeans$cluster,fcts_kmeans$cluster,anno_kmeans$cluster)
assignments2 = as.numeric(as.factor(assignments2))

plot(pca_fcts_data[,1:2],col=as.numeric(as.factor(assignments2)),pch=as.numeric(as.factor(assignments2)) %% 26)

anno_short = anno[,1:7]
anno_short$label = assignments2

fcts_varlen <- Rraven::extract_ts(X = anno, ts.column = "Peak Freq Contour (Hz)",  equal.length = F)
fcts_varlen_dataonly= fcts_varlen[,3:ncol(fcts_varlen)]


clusters=unique(sort(unique(colSums(!(is.na(fcts_varlen_dataonly))))))
pdf("making_cluster.tests.pdf")
par(mfrow=c(1,2))
for(clust in clusters) {
  nclust = floor(log10(clust))*10
  if(nclust>1) {
    cluster_subset = fcts_varlen_dataonly[,(colSums(!(is.na(fcts_varlen_dataonly)))) >= clust]
    cluster_subset = cluster_subset[complete.cases(cluster_subset),]
    
    if(nrow(cluster_subset)>nclust) {
      
      cluster_kmeans = kmeans(cluster_subset,centers=nclust,iter.max = 100)
      pca_subset = pca_fcts_data[rownames(pca_fcts_data) %in% rownames(cluster_subset),]
      plot(pca_subset[,1:2],col=as.numeric(cluster_kmeans$cluster),pch=(as.numeric(cluster_kmeans$cluster) %% 26),main=clust)
      plot(as.numeric(cluster_kmeans$centers[1,]),col=1,pch=1,ylim=c(1000,17000),type="n")
      for(i in 1:nclust) {
        points(as.numeric(cluster_kmeans$centers[i,]),col=(i %% 26),pch=(i %% 26),type="l",ylim=c(1000,17000))
      }
    }
  }
}
dev.off()


label_df=matrix(data=-1,nrow=nrow(fcts_dataonly),ncol=length(clusters))
colnames(label_df) = clusters
rownames(label_df) = rownames(fcts_dataonly)
label_df = as.data.frame(label_df)

pdf("making_cluster.tests2.pdf")
par(mfrow=c(1,2))
for(clust in clusters) {
  nclust = 10
  if(nclust>1) {
    
    cluster_subset = fcts_varlen_dataonly[,(colSums(!(is.na(fcts_varlen_dataonly)))) >= clust]
    cluster_subset = cluster_subset[complete.cases(cluster_subset),]
    
    rows = rownames(cluster_subset)
    cluster_subset = fcts_dataonly[rownames(fcts_dataonly) %in% rows,]
    
    if(nrow(cluster_subset)>nclust) {
      
      cluster_kmeans = kmeans(cluster_subset,centers=nclust,iter.max = 100)
      pca_subset = pca_fcts_data[rownames(pca_fcts_data) %in% rownames(cluster_subset),]
      plot(pca_subset[,1:2],col=as.numeric(cluster_kmeans$cluster),pch=(as.numeric(cluster_kmeans$cluster) %% 26),main=clust)
      plot(as.numeric(cluster_kmeans$centers[1,]),col=1,pch=1,ylim=c(1000,17000),type="n")
      for(i in 1:nclust) {
        points(as.numeric(cluster_kmeans$centers[i,]),col=(i %% 26),pch=(i %% 26),type="l",ylim=c(1000,17000))
      }
      
      label_df[rownames(label_df) %in% rows,colnames(label_df)==clust] = cluster_kmeans$cluster
      
    }
  }
}
dev.off()

label_df[label_df==10] = 0
label_df[label_df==-1] = NA

write.table(label_df,file="clusters.txt",sep="\t",row.names = T)


labels=c("a","b","c","d","e","f","g","h","i","j")
small_df = label_df[,-c(1:10)]
for(i in 1:ncol(small_df)) {
  print(i)
  if (i == 1) {
    for(j in 0:9) {
      small_df[small_df[,i]==j & !(is.na(small_df[,i])),i] = labels[j+1]
    }
    
    
  } else {
    
    to_check = small_df[,c(i-1,i)]
    
    for(label in labels){
    has_label = to_check[to_check[,1]==label & !(is.na(to_check[,1])),]
    print(has_label)
    labels_to_replace = names(rev(sort(table(has_label[,2]))))
    labels_to_replace = as.numeric(labels_to_replace)
    labels_to_replace=labels_to_replace[complete.cases(labels_to_replace)]
    small_df[small_df[,i] == labels_to_replace[1] & !(is.na(small_df[,i])),i] = label
    }
    
    to_check = small_df[,c(i-1,i)]
    
    ## check which ones are still numeric an which labels are not present
    still_numeric = unique((as.numeric(to_check[!(is.na(to_check[,2])),2])))
    still_numeric = still_numeric[complete.cases(still_numeric)]
    
    labeled = unique(to_check[!(to_check[,2] %in% still_numeric) & !(is.na(to_check[,2])),2])
    missing_labels = labels[!(labels %in% labeled)]
    
    if(length(still_numeric) == length(missing_labels) & length(still_numeric) > 0) {
      for(k in 1:length(still_numeric)) {
        small_df[small_df[,i] == still_numeric[k] & !(is.na(small_df[,i])),i] = missing_labels[k]
      }
    }
    
  }
}


for(label in labels) {
  small_df[small_df==label & !(is.na(small_df))] = as.numeric(which(labels==label)-1)
}

write.table(small_df,file="clusters2.txt",sep="\t",row.names = T)

small_df=data.matrix(small_df)


total_labels=matrix(data=0,nrow=nrow(small_df),ncol=10)
colnames(total_labels) = 1:10
rownames(total_labels) = rownames(small_df)
for(row in 1:nrow(small_df)) {
  x=table(small_df[row,])
  for(i in 1:length(x)) {
    column = names(x[i])
    total_labels[row,column] = as.numeric(x[i])
  }
}

total_labels = t(total_labels)
label_distances = dist(total_labels)
fit <- hclust(label_distances, method="ward.D") 
pdf("label_distances.pdf",width=100)
plot(fit,cex=0.1,lwd=0.1,hang=-0.1)
groups <- cutree(fit, k=36)
rect.hclust(fit, k=36, border="red") 
dev.off()

plot(pca_fcts_data[,1:2],col=as.numeric(as.factor(groups)),pch=as.numeric(as.factor(groups)))

fcts[,1:2]

filenames = list.files(path="/Users/kprovost/OneDrive - The Ohio State University/BLB_Data/",pattern="Table.1.selections.txt_short.txt$",full.names = T,recursive=T)
groups[groups==10] = 0
for(i in 11:36) {
  groups[groups==i] = letters[i-10]
}


for(file in filenames) {
  csv = data.table::fread(file,sep="\t",header=T,fill=T)
  csv = csv[,1:7]
  csv=unique(csv)
  wavfile = paste(strsplit(basename(file),"\\.")[[1]][1],".wav",sep="")
  wavsubset = unique(fcts[fcts$sound.files==wavfile,1:2])
  colnames(wavsubset) = c("sound.files","Selection")
  wavsubset = wavsubset[wavsubset$Selection %in% csv$Selection,]
  groupssubset = groups[names(groups) %in% rownames(wavsubset)]
  wavsubset$type = groupssubset
  csv = merge(csv,wavsubset[,2:3],all=T)
  write.table(csv,file,sep="\t",row.names = F,quote = F)
}


