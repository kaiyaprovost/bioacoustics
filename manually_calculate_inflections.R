df1 = "/Users/kprovost/Documents/Research/Student_Projects/LASP/rvn.dat_trimmed_spectro_fcts_18Jun2025_TABLE1.txt"
df2 = "/Users/kprovost/Documents/Research/Student_Projects/LASP/rvn.dat_trimmed_spectro_fcts_18Jun2025_TWEETYNET.txt"

df1 = read.table(df1,sep=" ",header=T)
df2 = read.table(df2,sep=" ",header=T)

df = gtools::smartbind(df1,df2)
df3 = merge(df1,df2,all=T,by=c("selec","sound.files"))

write.table(df,"/Users/kprovost/Documents/Research/Student_Projects/LASP/rvn.dat_trimmed_spectro_fcts_18Jun2025_TABLE1TWEETYNET.txt",sep="\t",quote=F,row.names = F)

dfe <- read.table("/Users/kprovost/Documents/Research/Student_Projects/LASP/rvn.dat_trimmed_spectro_fcts_18Jun2025_TABLE1TWEETYNET_edited.txt",
                  header=T,sep="\t")
## 26-125 are the ffqs
## to calculate slope you need to get the differences between each 

myslopes <- lapply(1:nrow(dfe),FUN=function(i){
  slopes <- diff(unlist(dfe[i,26:125]))
})
myslopes_df <- do.call(rbind,myslopes)
colnames(myslopes_df) = gsub("ffreq","ffreqslope",colnames(myslopes_df))
mean_slopes <- rowMeans(myslopes_df)
dfe$mean_slope <- mean_slopes

## inflections is the number of times it changes from positive to negative 
## stole the code from warbleR::inflections()
inflections <- sapply(1:nrow(myslopes_df),FUN=function(i){
  infls <- length(which(c(FALSE, diff(myslopes_df[i,] > 0) != 0)))
})

dfe$inflections <- inflections

write.table(dfe,"/Users/kprovost/Documents/Research/Student_Projects/LASP/rvn.dat_trimmed_spectro_fcts_18Jun2025_TABLE1TWEETYNET_edited_inflections.txt",
            sep="\t",row.names = F,quote=F)

dfe <- read.table("/Users/kprovost/Documents/Research/Student_Projects/LASP/rvn.dat_trimmed_spectro_fcts_18Jun2025_TABLE1TWEETYNET_edited_inflections.txt",
                  sep="\t",header=T)

mycorr <- cor(dfe[,c(1,5:22,24:25,126:130,132,134:140)],use="pairwise.complete.obs")
corrplot::corrplot((mycorr),method="color",order="hclust")
write.table(abs(mycorr),"/Users/kprovost/Documents/Research/Student_Projects/LASP/rvn.dat_trimmed_spectro_fcts_18Jun2025_TABLE1TWEETYNET_edited_inflections_CORR.txt",sep="\t",quote=F)

to_keep = c("dfrange", "dfslope", "enddom", "entropy", "freq.IQR", "freq.median", "freq.Q75", "inflections", "kurt", "mean_slope", "meanpeakf", "mindom", "modindx", "sfm", "startdom", "time.ent", "time.IQR")
meta_keep = c("selec", "sound.files", "View", "Channel", "start", "end", "bottom.freq", "top.freq")

dfe_pca <- dfe[,c(meta_keep,to_keep)]
write.table(dfe_pca,"/Users/kprovost/Documents/Research/Student_Projects/LASP/rvn.dat_trimmed_spectro_fcts_18Jun2025_TABLE1TWEETYNET_edited_inflections_uncorrelated.txt",sep="\t",quote=F)

dfe_pca <- read.table("/Users/kprovost/Documents/Research/Student_Projects/LASP/rvn.dat_trimmed_spectro_fcts_18Jun2025_TABLE1TWEETYNET_edited_inflections_uncorrelated.txt",sep="\t",header=T)

pca <- prcomp(dfe_pca[complete.cases(dfe_pca),to_keep],center=T,scale.=T)
summary(pca)
summary(pca)$importance
pca$sdev
pca$rotation
pca$center
pca$scale
pca$x

plot(broken_stick(17),summary(pca)$importance[2,])
abline(0,1)

dfe_pca2 <- cbind(dfe_pca[complete.cases(dfe_pca),],pca$x[,1:5])
write.table(dfe_pca2,"/Users/kprovost/Documents/Research/Student_Projects/LASP/rvn.dat_trimmed_spectro_fcts_18Jun2025_TABLE1TWEETYNET_edited_inflections_uncorrelated_PC12345.txt",sep="\t",quote=F)


test <- read.table("/Users/kprovost/Documents/Research/Student_Projects/LASP/LASP_metadata_18June2025.txt",sep="\t",header=T)

test$sound.files <- paste(test$Genus,"-",test$Specific_epithet,"-",test$Recording_ID,".resample.48000.wav",sep="")

merged <- merge(dfe_pca2,test,all=T,by="sound.files")
write.table(merged,"/Users/kprovost/Documents/Research/Student_Projects/LASP/rvn.dat_trimmed_spectro_fcts_18Jun2025_TABLE1TWEETYNET_edited_inflections_uncorrelated_PC12345_metadata.txt",sep="\t",quote=F)

