## 16 July 2024
## after first revision

## pca of the data
path = "/Users/kprovost/AMNH Dropbox/Kaiya Provost/Postdoc_Backup/Molothrus ater/Selections & WAV files/"
pattern = ".Table.1.selections.txt$"
myfiles = list.files(path=path,pattern=pattern,full.names = T,recursive = T)
df_list = lapply(myfiles,FUN=function(x){
  df = read.table(x,header=T,sep="\t")
  return(df)
})
df = do.call(gtools::smartbind,df_list)
df = unique(df)
df$type[df$type==""] = NA
df$Type[df$Type==""] = NA
df$type[is.na(df$type)] = df$Type[is.na(df$type)]
df$Type[is.na(df$Type)] = df$type[is.na(df$Type)]
#write.table(df,"/Users/kprovost/AMNH Dropbox/Kaiya Provost/Postdoc_Backup/Molothrus ater/combined_molothrus_ater.Table.1.selections.txt",
#            sep="\t",row.names = F,quote = F)

big_df = df
colnames(big_df)

good_cols = c("Selection","Begin.File",
  "Agg.Entropy..bits.",
"Avg.Power.Density..dB.FS.Hz.",
"BW.50...Hz.",
"BW.90...Hz.",
"Center.Time..s.",
"Center.Time.Rel.",
"Delta.Freq..Hz.",
"Max.Entropy..bits.",
"Min.Entropy..bits.",
"Peak.Time.Relative",
"PFC.Avg.Slope..Hz.ms.",
"PFC.Max.Freq..Hz.",
"PFC.Min.Freq..Hz.",
"PFC.Min.Slope..Hz.ms.",
"PFC.Num.Inf.Pts",
"SNR.NIST.Quick..dB.",
"Time.5..Rel.",
"Time.25..Rel.",
"Time.75..Rel.",
"Time.95..Rel.")

big_df = big_df[,colnames(big_df) %in% good_cols]

#blanks = colSums(is.na(big_df)) == nrow(big_df)
#badcols = names(blanks[blanks==T])
#big_df = big_df[,!(colnames(big_df) %in% badcols)]

#for(i in 4:ncol(big_df)){
#  print(i)
#  big_df[,i] = as.numeric(big_df[,i])
#}

#blanks = colSums(is.na(big_df)) == nrow(big_df)
#badcols = names(blanks[blanks==T])
#big_df = big_df[,!(colnames(big_df) %in% badcols)]
#for(i in 4:ncol(big_df)){
#
#  if (sd(big_df[,i],na.rm=T) == 0) {
#    #print(colnames(big_df)[i])
#    big_df[,i] = NA
#  }
#}
#blanks = colSums(is.na(big_df)) == nrow(big_df)
#badcols = names(blanks[blanks==T])
#big_df = big_df[,!(colnames(big_df) %in% badcols)]

## count the number of NA in big_df

goodcols_nometa = good_cols[!(good_cols %in% c("Selection","Begin.File"))]

## calculate correlation 
corr = cor(big_df[,goodcols_nometa],use = "pairwise.complete.obs")
corrplot::corrplot(corr,method="color")
#write.table(corr,"/Users/kprovost/AMNH Dropbox/Kaiya Provost/Postdoc_Backup/Molothrus ater/Selections & WAV files/corrplot_reduced_20Aug2024.csv",
#            sep=",",row.names = T,quote = F)

## write out
#write.table(big_df,"/Users/kprovost/AMNH Dropbox/Kaiya Provost/Postdoc_Backup/Molothrus ater/Selections & WAV files/big_data_frame_for_PCA.txt",
#            sep="\t",row.names = F,quote=F)

## calculate the PCA 21 Aug, 2024
## but remove missing values
big_df_nomiss = big_df[complete.cases(big_df),]
pca = prcomp(big_df_nomiss[,goodcols_nometa],scale. = T,center=T)

eigens = pca$sdev
rotation = pca$rotation
importance = summary(pca)$importance
data = pca$x
pca_data = merge(df,cbind(data,big_df_nomiss),all=T)

dim(data); dim(big_df_nomiss); dim(df)

broken_stick = function(P) {
  sequence = 1:P
  divided = 1/sequence
  seq_sums = sapply(sequence,FUN=function(x){
    subset = divided[x:P]
    subset_sum = sum(subset)
    i = subset_sum/P
    return(i)
  })
  return(seq_sums)
}
broken_stick_pca = broken_stick(ncol(pca$x))
importance=rbind(importance,broken_stick_pca)
plot(importance[2,],importance[4,]); abline(a=0,b=1)

broken = broken_stick(2279)

#write.table(pca_data,"/Users/kprovost/AMNH Dropbox/Kaiya Provost/Postdoc_Backup/Molothrus ater/Selections & WAV files/big_data_PCA.txt",
#            sep="\t",row.names = F,quote=F)
#write.table(importance,"/Users/kprovost/AMNH Dropbox/Kaiya Provost/Postdoc_Backup/Molothrus ater/Selections & WAV files/big_data_PCA_importance.txt",
#            sep="\t",row.names = F,quote=F)
#write.table(eigens,"/Users/kprovost/AMNH Dropbox/Kaiya Provost/Postdoc_Backup/Molothrus ater/Selections & WAV files/big_data_PCA_eigenvalues.txt",
#            sep="\t",row.names = F,quote=F)
#write.table(rotation,"/Users/kprovost/AMNH Dropbox/Kaiya Provost/Postdoc_Backup/Molothrus ater/Selections & WAV files/big_data_PCA_rotation.txt",
#            sep="\t",row.names = F,quote=F)

## aggregate by mean and sd for PC1
mean_pc1 = aggregate(pca_data$PC1~pca_data$Begin.File,FUN=function(x){mean(x,na.rm=T)})
sd_pc1 = aggregate(pca_data$PC1~pca_data$Begin.File,FUN=function(x){sd(x,na.rm=T)})
colnames(mean_pc1) = c("Begin.File","meanPC1")
colnames(sd_pc1) = c("Begin.File","sdPC1")
mean_sd_pc1 = merge(mean_pc1,sd_pc1,all=T,by="Begin.File")
## make a numeric ID column
mean_sd_pc1$ID = sub("\\.wav","",mean_sd_pc1$Begin.File)
mean_sd_pc1$ID = sub("BLB","",mean_sd_pc1$ID)
mean_sd_pc1$ID = as.numeric(mean_sd_pc1$ID)

## combine that with the pca data
pca_data_mean = merge(pca_data,mean_sd_pc1,all=T,by="Begin.File")
#write.table(pca_data_mean,"/Users/kprovost/AMNH Dropbox/Kaiya Provost/Postdoc_Backup/Molothrus ater/Selections & WAV files/big_data_PCA_mean.txt",
#            sep="\t",row.names = F,quote=F)

#write.table(mean_sd_pc1,"/Users/kprovost/AMNH Dropbox/Kaiya Provost/Postdoc_Backup/Molothrus ater/Selections & WAV files/big_data_PCA_meanONLY.txt",
#            sep="\t",row.names = F,quote=F)

##then combine that with the metadata we already have
meta <- read.delim("/Users/kprovost/AMNH Dropbox/Kaiya Provost/Postdoc_Backup/Molothrus ater/Selections & WAV files/0syllable_type_added/BLB_klp_Molothrus.ater.csv",
                   sep=",",header=T)
meta_df = unique(meta)
#meta_df$Begin.File = paste("BLB",meta_df$ID,".wav",sep="")

mean_sd_pc1_meta = merge(meta_df,mean_sd_pc1,all=T,by="ID")
dim(mean_sd_pc1)
dim(meta_df)
dim(mean_sd_pc1_meta)
#write.table(mean_sd_pc1_meta,"/Users/kprovost/AMNH Dropbox/Kaiya Provost/Postdoc_Backup/Molothrus ater/Selections & WAV files/big_data_PCA_meanONLY_metadata.txt",
#            sep="\t",row.names = F,quote=F)
plot(mean_sd_pc1_meta$YEAR_COLLECTED,mean_sd_pc1_meta$meanPC1)

pca_data_mean_meta = merge(meta_df,pca_data_mean,all=T,by="ID")
#write.table(pca_data_mean_meta,"/Users/kprovost/AMNH Dropbox/Kaiya Provost/Postdoc_Backup/Molothrus ater/Selections & WAV files/big_data_PCA_mean_metadata.txt",
#            sep="\t",row.names = F,quote=F)

table(pca_data_mean_meta$type)
boxplot(pca_data_mean_meta$PC1~pca_data_mean_meta$type)
plot(pca_data_mean_meta$YEAR_COLLECTED,pca_data_mean_meta$PC1,
     col=as.numeric(as.factor(pca_data_mean_meta$type)))
## now we need to extract us some data

df_edit = read.table("/Users/kprovost/AMNH Dropbox/Kaiya Provost/Postdoc_Backup/Molothrus ater/Selections & WAV files/big_data_PCA_mean_metadata_urban_EDITED.txt",
                     header=T,sep="\t")

## need to add that to the shp.pcs 
shp_pc_importance_file = "/Users/kprovost/AMNH Dropbox/Kaiya Provost/Postdoc_Backup/Molothrus ater/Selections & WAV files/SoundShape/TPS_PCA/pca_soundshape.1_importance_SUBSET.23Aug2024.temp"
shp_pc_importance = read.table(shp_pc_importance_file,header=T,sep=" ")
expected_brokenstick = broken_stick(ncol(shp_pc_importance))
actual_brokenstick = shp_pc_importance[2,]
which(actual_brokenstick>expected_brokenstick)

shp_pcs = read.table("/Users/kprovost/AMNH Dropbox/Kaiya Provost/Postdoc_Backup/Molothrus ater/Selections & WAV files/SoundShape/TPS_PCA/pca_soundshape.1_SCALETRUE.23Aug2024.temp",
                     header=T,sep=" ")
shp_pcs = shp_pcs[,1:5]
shp_pcs$syllid = rownames(shp_pcs)
id_sel =  do.call(rbind,strsplit(shp_pcs$syllid,"\\."))
colnames(id_sel) = c("ID","Selection")
shp_pcs = cbind(shp_pcs,id_sel)
shp_pcs$ID = sub("BLB","",shp_pcs$ID)
shp_pcs$ID = as.numeric(shp_pcs$ID)

## need to add mean and sd each ID
shp_mean_agg = aggregate(cbind(Shp.PC1,Shp.PC2,Shp.PC3,Shp.PC4,Shp.PC5)~ID,data=shp_pcs,FUN=function(x){mean(x,na.rm=T)})
shp_mean_agg$ID = as.numeric(shp_mean_agg$ID)
shp_sd_agg = aggregate(cbind(Shp.PC1,Shp.PC2,Shp.PC3,Shp.PC4,Shp.PC5)~ID,data=shp_pcs,FUN=function(x){sd(x,na.rm=T)})
shp_sd_agg$ID = as.numeric(shp_sd_agg$ID)
colnames(shp_mean_agg) = c("ID","meanShp.PC1","meanShp.PC2","meanShp.PC3","meanShp.PC4","meanShp.PC5")
colnames(shp_sd_agg) = c("ID","sdShp.PC1","sdShp.PC2","sdShp.PC3","sdShp.PC4","sdShp.PC5")
shp_pcs = merge(shp_pcs,shp_sd_agg,all=T,by="ID")
shp_pcs = merge(shp_pcs,shp_mean_agg,all=T,by="ID")

#write.table(shp_pcs,"/Users/kprovost/AMNH Dropbox/Kaiya Provost/Postdoc_Backup/Molothrus ater/Selections & WAV files/shp_pcs.txt",sep="\t",row.names = F,quote = F)

df_merge = merge(df_edit,shp_pcs,all=T,by=c("ID","Selection"))
  
## we might need to aggregate by individual and by syllable type actually
boxplot(df_merge$Prop.PC1~df_merge$type)
boxplot(df_merge$Shp.PC1~df_merge$type)
boxplot(df_merge$Shp.PC2~df_merge$type)
boxplot(df_merge$Shp.PC3~df_merge$type)
boxplot(df_merge$Shp.PC4~df_merge$type)
boxplot(df_merge$Shp.PC5~df_merge$type)

plot(df_merge$Prop.PC1,df_merge$Shp.PC1,col=as.numeric(as.factor(df_merge$type)),
     pch=as.numeric(as.factor(df_merge$type)))

#write.table(df_merge,"/Users/kprovost/AMNH Dropbox/Kaiya Provost/Postdoc_Backup/Molothrus ater/final_dataset_23Aug2024.txt",
#            sep="\t",quote=F,row.names =F)


df_merge = read.table("/Users/kprovost/AMNH Dropbox/Kaiya Provost/Postdoc_Backup/Molothrus ater/final_dataset_23Aug2024.txt",
                      sep="\t",header=T)

mean_agg_type_ind = aggregate(cbind(Prop.PC1,Shp.PC1,Shp.PC2,Shp.PC3,Shp.PC4,Shp.PC5)~ID+type,data=df_merge,
                              FUN=function(x){mean(x,na.rm=T)})
colnames(mean_agg_type_ind)[3:8] = paste("mean",colnames(mean_agg_type_ind)[3:8],sep="")
sd_agg_type_ind = aggregate(cbind(Prop.PC1,Shp.PC1,Shp.PC2,Shp.PC3,Shp.PC4,Shp.PC5)~ID+type,data=df_merge,
                              FUN=function(x){sd(x,na.rm=T)})
colnames(sd_agg_type_ind)[3:8] = paste("sd",colnames(sd_agg_type_ind)[3:8],sep="")

## combine those 
meansd_agg_type_df = merge(mean_agg_type_ind,sd_agg_type_ind,all=T,by=c("ID","type"))
#write.table(meansd_agg_type_df,"/Users/kprovost/AMNH Dropbox/Kaiya Provost/Postdoc_Backup/Molothrus ater/final_dataset_23Aug2024_meanSdIndType.txt",
#            sep="\t",quote=F,row.names =F)
plot(meansd_agg_type_df$meanProp.PC1,meansd_agg_type_df$meanShp.PC1,col=as.numeric(as.factor(meansd_agg_type_df$type)))

## and now finally we can run out dang models and make our dang plots

#remove.packages("Matrix")
#utils::install.packages("lme4", type = "source")
library(lme4)
library(lmerTest)
library(Matrix)
df_merge_uniques = df_merge[,c("COUNTY","ID","LATITUDE","LONGITUDE","YEAR",
                               "cropland_year","grazing_year","popc_year","uopp_year",
                               "meanProp.PC1","meanShp.PC1","meanShp.PC2","meanShp.PC3","meanShp.PC4","meanShp.PC5",
                               "sdProp.PC1","sdShp.PC1","sdShp.PC2","sdShp.PC3","sdShp.PC4","sdShp.PC5")]
df_merge_uniques = unique(df_merge_uniques)

df_merge_typeuniques = df_merge[,c("COUNTY","ID","LATITUDE","LONGITUDE","YEAR","type",
                                   "cropland_year","grazing_year","popc_year","uopp_year")]
df_merge_typeuniques = unique(df_merge_typeuniques)
df_merge_typeuniques = merge(df_merge_typeuniques,meansd_agg_type_df,all=T,by=c("ID","type"))
df_merge_typeuniques = df_merge_typeuniques[df_merge_typeuniques!="other",]
#write.table(df_merge_typeuniques,"/Users/kprovost/AMNH Dropbox/Kaiya Provost/Postdoc_Backup/Molothrus ater/final_dataset_23Aug2024_meanSdIndType_meta.txt",
#            sep="\t",quote=F,row.names =F)

plot(df_merge_typeuniques$YEAR,df_merge_typeuniques$meanProp.PC1,
     col=as.numeric(as.factor(df_merge_typeuniques$type)))

df_merge = df_merge[df_merge$type!="other",]

summary(aov(Prop.PC1~type,data=df_merge)) ## sig
summary(aov(Shp.PC1~type,data=df_merge)) ## sig
summary(aov(Shp.PC2~type,data=df_merge)) ## not sig
summary(aov(Shp.PC3~type,data=df_merge)) ## sig
summary(aov(Shp.PC4~type,data=df_merge)) ## not sig
summary(aov(Shp.PC5~type,data=df_merge)) ## sig

all_prop_pc1_glm = lmer(meanProp.PC1~cropland_year+grazing_year+uopp_year+(1|COUNTY)+YEAR+type,data=df_merge)
all_shp_pc1_glm = lmer(meanShp.PC1~cropland_year+grazing_year+uopp_year+(1|COUNTY)+YEAR+type,data=df_merge)
all_shp_pc2_glm = lmer(meanShp.PC2~cropland_year+grazing_year+uopp_year+(1|COUNTY)+YEAR+type,data=df_merge)
all_shp_pc3_glm = lmer(meanShp.PC3~cropland_year+grazing_year+uopp_year+(1|COUNTY)+YEAR+type,data=df_merge)
all_shp_pc4_glm = lmer(meanShp.PC4~cropland_year+grazing_year+uopp_year+(1|COUNTY)+YEAR+type,data=df_merge)
all_shp_pc5_glm = lmer(meanShp.PC5~cropland_year+grazing_year+uopp_year+(1|COUNTY)+YEAR+type,data=df_merge)

summary(all_prop_pc1_glm) ## sig
summary(all_shp_pc1_glm) ## sig
summary(all_shp_pc2_glm) ## sig
summary(all_shp_pc3_glm) ## sig
summary(all_shp_pc4_glm) ## sig
summary(all_shp_pc5_glm) ## sig

mean_prop_pc1_glm = lmer(meanProp.PC1~cropland_year+grazing_year+uopp_year+(1|COUNTY)+YEAR,data=df_merge_uniques)
prop_pc1_glm_whistle = lmer(meanProp.PC1~cropland_year+grazing_year+uopp_year+(1|COUNTY)+YEAR,data=df_merge_typeuniques[df_merge_typeuniques$type=="w",])
prop_pc1_glm_song = lmer(meanProp.PC1~cropland_year+grazing_year+uopp_year+(1|COUNTY)+YEAR,data=df_merge_typeuniques[df_merge_typeuniques$type=="s",])
prop_pc1_glm_call = lmer(meanProp.PC1~cropland_year+grazing_year+uopp_year+(1|COUNTY)+YEAR,data=df_merge_typeuniques[df_merge_typeuniques$type=="c",])
summary(mean_prop_pc1_glm) ## not sig
summary(prop_pc1_glm_whistle) ## sig
summary(prop_pc1_glm_song) ## sig
summary(prop_pc1_glm_call) ## sig 

all_Shp_pc1_glm = lmer(meanShp.PC1~cropland_year+grazing_year+uopp_year+(1|COUNTY)+YEAR+type,data=df_merge)
summary(all_Shp_pc1_glm) ## sig

mean_Shp_pc1_glm = lmer(meanShp.PC1~cropland_year+grazing_year+uopp_year+(1|COUNTY)+YEAR,data=df_merge_uniques)
Shp_pc1_glm_whistle = lmer(meanShp.PC1~cropland_year+grazing_year+uopp_year+(1|COUNTY)+YEAR,data=df_merge_typeuniques[df_merge_typeuniques$type=="w",])
Shp_pc1_glm_song = lmer(meanShp.PC1~cropland_year+grazing_year+uopp_year+(1|COUNTY)+YEAR,data=df_merge_typeuniques[df_merge_typeuniques$type=="s",])
Shp_pc1_glm_call = lmer(meanShp.PC1~cropland_year+grazing_year+uopp_year+(1|COUNTY)+YEAR,data=df_merge_typeuniques[df_merge_typeuniques$type=="c",])
summary(mean_Shp_pc1_glm) ## not sig
summary(Shp_pc1_glm_whistle) ## not sig
summary(Shp_pc1_glm_song) ## not sig, singular
summary(Shp_pc1_glm_call) ## not sig, singular


all_Shp_PC2_glm = lmer(meanShp.PC2~cropland_year+grazing_year+uopp_year+(1|COUNTY)+YEAR+type,data=df_merge)
summary(all_Shp_PC2_glm) ## sig

mean_Shp_PC2_glm = lmer(meanShp.PC2~cropland_year+grazing_year+uopp_year+(1|COUNTY)+YEAR,data=df_merge_uniques)
Shp_PC2_glm_whistle = lmer(meanShp.PC2~cropland_year+grazing_year+uopp_year+(1|COUNTY)+YEAR,data=df_merge_typeuniques[df_merge_typeuniques$type=="w",])
Shp_PC2_glm_song = lmer(meanShp.PC2~cropland_year+grazing_year+uopp_year+(1|COUNTY)+YEAR,data=df_merge_typeuniques[df_merge_typeuniques$type=="s",])
Shp_PC2_glm_call = lmer(meanShp.PC2~cropland_year+grazing_year+uopp_year+(1|COUNTY)+YEAR,data=df_merge_typeuniques[df_merge_typeuniques$type=="c",])
summary(mean_Shp_PC2_glm) ## sig, singular
summary(Shp_PC2_glm_whistle) ## sig
summary(Shp_PC2_glm_song) ## sig, singular
summary(Shp_PC2_glm_call) ## sig, singular



palette(c("blue","grey","red","goldenrod"))
plot(df_merge$YEAR,df_merge$Prop.PC1,
     col=as.numeric(as.factor(df_merge$type)),
     pch=as.numeric(as.factor(df_merge$type)))
legend("topright",legend=c("Calls","Other","Songs","Whistles"),
       col=1:4,pch=1:4)
abline(lm(df_merge$Prop.PC1~df_merge$YEAR),
       col="black",lwd=3)
abline(lm(df_merge$Prop.PC1[df_merge$type=="s"]~df_merge$YEAR[df_merge$type=="s"]),
       col="blue",lwd=2)


prop_pc2_glm_A = lmer(Prop.PC2~cropland_year+uopp_year+(1|COUNTY)+YEAR,data=df_edit) ## singular fit?
summary(prop_pc2_glm_A)

shp_pc1_glm = lmer(Shp.PC1~cropland_year+grazing_year+uopp_year+(1|COUNTY)+YEAR,data=df_edit)
shp_pc2_glm = lmer(Shp.PC2~cropland_year+grazing_year+uopp_year+(1|COUNTY)+YEAR,data=df_edit) 
shp_pc3_glm = lmer(Shp.PC3~cropland_year+grazing_year+uopp_year+(1|COUNTY)+YEAR,data=df_edit) 
shp_pc4_glm = lmer(Shp.PC4~cropland_year+grazing_year+uopp_year+(1|COUNTY)+YEAR,data=df_edit)
shp_pc5_glm = lmer(Shp.PC5~cropland_year+grazing_year+uopp_year+(1|COUNTY)+YEAR,data=df_edit) 
summary(shp_pc1_glm)
summary(shp_pc2_glm)
summary(shp_pc3_glm)
summary(shp_pc4_glm)
summary(shp_pc5_glm)











urban_meta = read.table("/Users/kprovost/AMNH Dropbox/Kaiya Provost/Postdoc_Backup/Molothrus ater/Molothrus.ater.combined.PCA.merged_nocorrelations_metadata_16July2024_EDITED.csv",
                        sep=",",header=T)
urban_keep = c("Lat","Long","YEAR","Anthromes_Year","cropland_year","cropland.stack_1940AD.tif",
               "cropland.stack_1950AD.tif","cropland.stack_1960AD.tif","cropland.stack_1970AD.tif",
               "cropland.stack_1980AD.tif","cropland.stack_1990AD.tif","cropland.stack_2000AD.tif",
               "grazing_year","grazing.stack_1940AD.tif","grazing.stack_1950AD.tif",
               "grazing.stack_1960AD.tif","grazing.stack_1970AD.tif","grazing.stack_1980AD.tif",
               "grazing.stack_1990AD.tif","grazing.stack_2000AD.tif","popc_year",
               "popc.stack_1940AD.tif","popc.stack_1950AD.tif","popc.stack_1960AD.tif",
               "popc.stack_1970AD.tif","popc.stack_1980AD.tif","popc.stack_1990AD.tif",
               "popc.stack_2000AD.tif","uopp_year","uopp.stack_1940AD.tif","uopp.stack_1950AD.tif",
               "uopp.stack_1960AD.tif","uopp.stack_1970AD.tif","uopp.stack_1980AD.tif",
               "uopp.stack_1990AD.tif","uopp.stack_2000AD.tif")

urban_meta = urban_meta[,urban_keep]
urban_meta = unique(urban_meta)
colnames(urban_meta)[1:3] = c("LATITUDE","LONGITUDE","YEAR_COLLECTED")

pca_data_mean_meta_urban = merge(x=urban_meta,y=pca_data_mean_meta,all.y=T,all.x=F,by=c("LATITUDE","LONGITUDE","YEAR_COLLECTED"))
dim(pca_data_mean_meta_urban)
View(pca_data_mean_meta_urban)
#write.table(pca_data_mean_meta_urban,"/Users/kprovost/AMNH Dropbox/Kaiya Provost/Postdoc_Backup/Molothrus ater/Selections & WAV files/big_data_PCA_mean_metadata_urban.txt",
#            sep="\t",row.names = F,quote=F)
plot(pca_data_mean_meta_urban$PC1,pca_data_mean_meta_urban$cropland_year)

df_edit = read.table("/Users/kprovost/AMNH Dropbox/Kaiya Provost/Postdoc_Backup/Molothrus ater/Selections & WAV files/big_data_PCA_mean_metadata_urban_EDITED.txt",
                    sep="\t",header=T)





## we only want the actual metadata 
#meta_keep_cols = c("Begin.File","COLLECTOR","COUNTRY","COUNTY","cropland.dif","cropland.new","cropland.old","cropland.stack_1940AD.tif","cropland.stack_1950AD.tif","cropland.stack_1960AD.tif","cropland.stack_1970AD.tif","cropland.stack_1980AD.tif","cropland.stack_1990AD.tif","cropland.stack_2000AD.tif","DAY","ELEVATION","FAMILY_NAME","GENUS","grazing.dif","grazing.new","grazing.old","grazing.stack_1940AD.tif","grazing.stack_1950AD.tif","grazing.stack_1960AD.tif","grazing.stack_1970AD.tif","grazing.stack_1980AD.tif","grazing.stack_1990AD.tif","grazing.stack_2000AD.tif","ID","INDIVIDUAL","ir_rice.stack_1940AD.tif","ir_rice.stack_1950AD.tif","ir_rice.stack_1960AD.tif","ir_rice.stack_1970AD.tif","ir_rice.stack_1980AD.tif","ir_rice.stack_1990AD.tif","ir_rice.stack_2000AD.tif","JULIAN_DAY","Lat","LOCALITY_NAME","Long","MONTH","ORDER_NAME","popc.dif","popc.new","popc.old","popc.stack_1940AD.tif","popc.stack_1950AD.tif","popc.stack_1960AD.tif","popc.stack_1970AD.tif","popc.stack_1980AD.tif","popc.stack_1990AD.tif","popc.stack_2000AD.tif","SEX","SPECIES","STATE_PROVINCE","tot_irri.stack_1940AD.tif","tot_irri.stack_1950AD.tif","tot_irri.stack_1960AD.tif","tot_irri.stack_1970AD.tif","tot_irri.stack_1980AD.tif","tot_irri.stack_1990AD.tif","tot_irri.stack_2000AD.tif","uopp.dif","uopp.new","uopp.old","uopp.stack_1940AD.tif","uopp.stack_1950AD.tif","uopp.stack_1960AD.tif","uopp.stack_1970AD.tif","uopp.stack_1980AD.tif","uopp.stack_1990AD.tif","uopp.stack_2000AD.tif" ,"YEAR")
#meta_df = meta_df[,c(meta_keep_cols)]
#meta_df = meta_df[!(is.na(meta_df$Begin.File)),]
#meta_df = meta_df[!(is.na(meta_df$Selection)),]
#meta_df = meta_df[complete.cases(meta_df),]
#meta_df = unique(meta_df)
##write.table(meta_df,"/Users/kprovost/AMNH Dropbox/Kaiya Provost/Postdoc_Backup/Molothrus ater/Selections & WAV files/metadata_only.txt",
#            sep="\t",row.names = F,quote=F)
#
#meta_good = meta[,c(good_cols,"Begin.Time..s.","End.Time..s.","Low.Freq..Hz.","High.Freq..Hz.")]
#meta_good = meta_good[!(is.na(meta_good$Selection)),]
#meta_good = meta_good[complete.cases(meta_good),]
### glue this to the other data?
#pca_data_mean_meta_good = merge(x=meta_good,y=pca_data_mean,all.y=T,all.x=F#,
#                                #by=c("Selection","Begin.File","Begin.Time..s.","End.Time..s.","Low.Freq..Hz.","High.Freq..Hz.")
#                                )
##write.table(pca_data_mean_meta_good,"/Users/kprovost/AMNH Dropbox/Kaiya Provost/Postdoc_Backup/Molothrus ater/Selections & WAV files/pca_data_mean_meta_good.txt",
#            sep="\t",row.names = F,quote=F)
#
#pca_data_mean_meta = merge(meta_df,pca_data_mean,all=T)

#
## plot year
#plot(pca_data_mean_meta$YEAR,pca_data_mean_meta$PC1)




## import the data and metadata we already have
library(raster)
df <- read.delim("~/Work/OSU/Molothrus ater/Data/Molothrus.ater.combined.PCA.merged_nocorrelations_metadata_28Nov2023.txt")
raw_anthromes_folder = "~/Work/OSU/Molothrus ater/Data/anthromes/"
setwd(raw_anthromes_folder)
raw_anthromes_files = list.files(path=raw_anthromes_folder,
                 pattern="AD.tif",full.names = T)
raw_anthromes_files = raw_anthromes_files[!grepl("stack",raw_anthromes_files)]
years = c("1940AD","1950AD","1960AD","1970AD","1980AD","1990AD","2000AD")

for(year in years) {
  print(year)
  this_year = raw_anthromes_files[grepl(year,raw_anthromes_files)]
  this_year_stack = raster::stack(this_year)
  name_this_year = paste("stack_",year,".tif",sep="")
  try({
    raster::writeRaster(this_year_stack,name_this_year,overwrite=T,format="GTiff")
  })
}

## import the data for each year between 1940 and 2000 from the unique lat-longs
df_latlong = df[,c("Long","Lat")]
df_latlong = unique(df_latlong)
df_latlong = df_latlong[complete.cases(df_latlong),]


stack_anthromes_files = list.files(path=raw_anthromes_folder,
                                   pattern="stack",full.names=T)

for(stack_file in stack_anthromes_files) {
  this_stack = stack(stack_file)
  names(this_stack) = c("cropland","grazing","ir_rice","popc","tot_irri","uopp")
  names(this_stack) = paste(names(this_stack),basename(stack_file),sep=".")
  cells = cellFromXY(this_stack,df_latlong)
  new_data = extract(this_stack,cells)
  df_latlong = cbind(df_latlong,new_data)
}

df_merged = merge(df,df_latlong,all=T)

#write.table(df_merged,"~/Work/OSU/Molothrus ater/Data/Molothrus.ater.combined.PCA.merged_nocorrelations_metadata_16July2024.txt",
#            sep="\t",row.names = F)

## use a general linear mixed model for each principal component that includes 
## time, urbanization, and their interaction as fixed effects and location as a random effect,
## This should allow you to assess whether songs change across time and with cropland cover, 
## while controlling for location. 

## read in the edited version 
df_edit = read.table("~/Work/OSU/Molothrus ater/Data/Molothrus.ater.combined.PCA.merged_nocorrelations_metadata_16July2024_EDITED.csv",
                     sep=",",header=T)
keep_colnames = c("BLB_ID","COUNTY","Lat","Long","YEAR","Shp.PC1","Shp.PC2","Shp.PC3","Shp.PC4",
                  "Shp.PC5","Prop.PC1","Prop.PC2","Prop.PC3","Prop.PC4","Prop.PC5",
                  "cropland_year","grazing_year","popc_year","uopp_year")

df_edit = df_edit[,keep_colnames]


## plotting
my_data = "/Users/kprovost/Documents/Research/Cowbird/Molothrus.ater.combined.PCA.merged_nocorrelations_metadata_16July2024_EDITED.csv"
df = read.table(my_data,header=T,sep=",")

prop1_crop = lm(df$Prop.PC1~df$cropland_year)
prop1_uopp = lm(df$Prop.PC1~df$uopp_year)
prop1_graz = lm(df$Prop.PC1~df$grazing_year)
prop1_year = lm(df$Prop.PC1~df$YEAR)
shp1_crop = lm(df$Shp.PC1~df$cropland_year)
shp1_uopp = lm(df$Shp.PC1~df$uopp_year)
shp1_graz = lm(df$Shp.PC1~df$grazing_year)
shp1_year = lm(df$Shp.PC1~df$YEAR)

par(mfrow=c(2,4))
plot(df$YEAR,df$Prop.PC1,ylab="Prop.PC1",xlab="Year %")
abline(prop1_year,col="red",lwd=3,lty=1)
plot(df$cropland_year,df$Prop.PC1,ylab="Prop.PC1",xlab="Cropland %")
abline(prop1_crop,col="red",lwd=3,lty=1)
plot(df$uopp_year,df$Prop.PC1,ylab="Prop.PC1",xlab="Urban Occupancy %")
abline(prop1_uopp,col="red",lwd=3,lty=1)
plot(df$grazing_year,df$Prop.PC1,ylab="Prop.PC1",xlab="Grazing %")
abline(prop1_graz,col="red",lwd=1,lty=3)

plot(df$YEAR,df$Shp.PC1,ylab="Shp.PC1",xlab="Year %")
abline(shp1_year,col="red",lwd=3,lty=1)
plot(df$cropland_year,df$Shp.PC1,ylab="Shp.PC1",xlab="Cropland %")
abline(shp1_crop,col="red",lwd=3,lty=1)
plot(df$uopp_year,df$Shp.PC1,ylab="Shp.PC1",xlab="Urban Occupancy %")
abline(shp1_uopp,col="red",lwd=3,lty=1)
plot(df$grazing_year,df$Shp.PC1,ylab="Shp.PC1",xlab="Grazing %")
abline(shp1_graz,col="red",lwd=3,lty=1)



library(AICcmodavg)
AICc(prop_pc1_glm)

corx=cor(df_edit[,c("Lat","Long","YEAR","Shp.PC1","Shp.PC2","Shp.PC3","Shp.PC4",
               "Shp.PC5","Prop.PC1","Prop.PC2","Prop.PC3","Prop.PC4","Prop.PC5",
               "cropland_year","grazing_year","uopp_year")],
    use ="everything")
corrplot::corrplot(corx,method = "number")



## below is from first submission
## TODO: clean this up when finished

## broken stick function
brokenstick = function(n) {
  to_sum = 1/(1:n)
  sums = sapply(1:n,FUN=function(i){
    sum(to_sum[i:n])
  })
  broken = sums/n
  return(broken)
}
x = brokenstick(1040)

## generate PCA for biostats

{
BLB26 <-
  read.delim(
    "/Users/kprovost/Documents/Postdoc_Working/Molothrus.ater.selections/BLB26.Table.1.selections.txt"
  )
colnames(BLB26) ## this prints the column names of this table

BLB26$Begin.Time..s.
plot(BLB26$Begin.Time..s.)
summary(BLB26)

## run a pca on single data file
prcomp(BLB26) ## this doesn't work -- not numerical

## get only numerical data and complete data
colSums(BLB26[, 4:5])
is.na(BLB26)
colSums(is.na(BLB26))
nrow(BLB26)
colSums(is.na(BLB26)) == nrow(BLB26)
badcolumns = which(colSums(is.na(BLB26)) == nrow(BLB26))

## which function
values = c("A", "B", "C", "B")
which(values == "B")

## make a copy of the data
BLB26.full <- BLB26
BLB26.full <- BLB26.full[,-badcolumns]

## check if numeric
for (item in colnames(BLB26.full)) {
  ## do something
  print(item)
  check = is.numeric(BLB26.full[, item])
  
  if (check == TRUE) {
    ## do something
    ## keep this column
    ## do nothing
    print("keep!")
  } else {
    ## do something else
    ## get rid of this column
    badcolumn = which(colnames(BLB26.full) == item) ## get the column number
    BLB26.full <- BLB26.full[,-badcolumn]
  }
}

## run our pca
pca = prcomp(BLB26.full, center = TRUE, scale. = FALSE)
## we need to scale this
str(pca)
summary(pca)
rotation = pca$rotation
importance = summary(pca)$importance
data = pca$x

sapply(BLB26.full, dplyr::n_distinct)

badcolumns2 <- which(sapply(BLB26.full, dplyr::n_distinct) == 1)
BLB26.full <- BLB26.full[,-badcolumns2]

## run our pca
pca2 = prcomp(BLB26.full, center = TRUE, scale. = TRUE)
str(pca2)
summary(pca2)
rotation2 = pca2$rotation
importance2 = summary(pca2)$importance
data2 = pca2$x

## oops these need to be dataframes
data = as.data.frame(data)
data2 = as.data.frame(data2)
plot(data$PC1, data2$PC1)
}
{
## 2 November 2022

folder = "/Users/kprovost/Documents/Postdoc_Working/Molothrus.ater.selections"
files = list.files(path = folder,
                   pattern = "selections.txt",
                   full.names = T)

blb_data = lapply(
  files,
  FUN = function(f) {
    ## with each file called "f" in files
    ## read the file f
    return(read.delim(f))
  }
)

blb_data[[1]][, 1:3]

install.packages("gtools")

big_blb_data = do.call(what = gtools::smartbind, args = blb_data)
hist(big_blb_data$Low.Freq..Hz.)

new_file = "/Users/kprovost/Documents/Postdoc_Working/Molothrus.ater.selections/Molothrus.ater.combined.Table.1.selections.txt"
#write.table(
#  big_blb_data,
#  new_file,
#  quote = F,
#  sep = "\t",
#  row.names = F
#)
new_file = "~/Work/Molothrus.ater.combined.Table.1.selections.txt"

big_blb_data = read.table(new_file,sep="\t",header=T)

big_blb_data$Begin.Time..s.
plot(big_blb_data$Begin.Time..s.)
summary(big_blb_data)

## get only numerical data and complete data
colSums(big_blb_data[, 4:5])
is.na(big_blb_data)
colSums(is.na(big_blb_data))
nrow(big_blb_data)
colSums(is.na(big_blb_data)) == nrow(big_blb_data)
badcolumns = which(colSums(is.na(big_blb_data)) == nrow(big_blb_data))

## make a copy of the data
big_blb_data.full <- big_blb_data
big_blb_data.full <- big_blb_data.full[,-badcolumns]

## check if numeric
for (item in colnames(big_blb_data.full)) {
  ## do something
  print(item)
  check = is.numeric(big_blb_data.full[, item])
  
  if (check == TRUE) {
    ## do something
    ## keep this column
    ## do nothing
    print("keep!")
  } else {
    ## do something else
    ## get rid of this column
    badcolumn = which(colnames(big_blb_data.full) == item) ## get the column number
    big_blb_data.full <- big_blb_data.full[,-badcolumn]
  }
}

complete.cases(big_blb_data.full) ## remove stuff with only some missing data
big_blb_data.full <-
  big_blb_data.full[complete.cases(big_blb_data.full),]

sapply(big_blb_data.full, dplyr::n_distinct)

badcolumns2 <-
  which(sapply(big_blb_data.full, dplyr::n_distinct) == 1)
big_blb_data.full <- big_blb_data.full[,-badcolumns2]

## check for highly correlated variables
big_blb_data.full = big_blb_data.full[,!(colnames(big_blb_data.full) %in% cols_to_remove)]
r = cor(big_blb_data.full)
r2 = r^2
corrplot::corrplot(r2,"ellipse",order="hclust")
diag(r2) = 0
r2_thresh = r2
r2_thresh[r2_thresh<0.75] =0
#corrplot::corrplot(r2_thresh,"ellipse",order="hclust",diag=F)

colsum=colSums(r2)
x=lapply(1:ncol(r2_thresh),FUN=function(i){
  coln = r2_thresh[,i]
  maxn=round(max(coln,na.rm=T),2)
  if(maxn!=0){
    bad = c(colnames(r2_thresh)[i],maxn,":",names(which(coln == maxn)))
    print(bad)
    return(bad)
  }
})
which(r2_thresh == max(r2_thresh,na.rm=T))

## list of ones to remove
cols_to_remove = c("Begin.Clock.Time",
                   "Begin.Date.Time",
                   "File.Offset..s.",
                   "Beg.File.Samp..samples.",
                   "Begin.Time..s.",
                   "Begin.Sample..samples.",
                   "Time.5...s.",
                   "Time.25...s.",
                   "Center.Time..s.",
                   "Max.Time..s.",
                   "Peak.Time..s.",
                   "Time.75...s.",
                   "Time.95...s.",
                   "End.Sample..samples.",
                   "End.File.Samp..samples.",
                   "End.Time..s.",
                   "End.Clock.Time",
                   "Length..frames.",
                   "Sample.Length..samples.",
                   "Delta.Time..s.",
                   "Dur.90...s.",
                   "Dur.50...s.",
                   "PFC.Min.Slope..Hz.ms.",
                   "High.Freq..Hz.",
                   "Low.Freq..Hz.",
                   "Inband.Power..dB.FS.",
                   "Peak.Power.Density..dB.FS.Hz.",
                   "Energy..dB.FS.",
                   "Freq.25...Hz.",
                   "Freq.75...Hz.",
                   "Center.Freq..Hz.",
                   "Max.Freq..Hz."
                   )

## run our pca
pca2 = prcomp(big_blb_data.full, center = TRUE, scale. = TRUE)
str(pca2)
summary(pca2)
rotation2 = pca2$rotation
importance2 = summary(pca2)$importance
data2 = pca2$x

#write.table(rotation2,"~/Work/cowbird_rotation_pca_fix_28Nov2023.txt",sep="\t")
#write.table(importance2,"~/Work/cowbird_importance_pca_fix_28Nov2023.txt",sep="\t")
#write.table(pca2$sdev,"~/Work/cowbird_sdev_pca_fix_28Nov2023.txt",sep="\t")
#write.table(pca2$center,"~/Work/cowbird_center_pca_fix_28Nov2023.txt",sep="\t")
#write.table(pca2$scale,"~/Work/cowbird_scale_pca_fix_28Nov2023.txt",sep="\t")

## oops these need to be dataframes
data2 = as.data.frame(data2)
plot(data2$PC1, data2$PC2)

pca_plus_big_data = cbind(big_blb_data.full, data2)


## note to self: add the full raw data to this as well
## merge
big_blb_data = read.table(new_file,sep="\t",header=T)
merged_data = merge(big_blb_data, pca_plus_big_data, all = T)
meta = read.delim("~/Work/cowbird data metadata urbanization - Sheet1.tsv")
merged_data = merge(meta,merged_data, all = T)

new_file_pca = "~/Work/Molothrus.ater.combined.PCA.merged_nocorrelations_metadata_28Nov2023.txt"
#write.table(
#  merged_data,
#  new_file_pca,
#  quote = F,
#  sep = "\t",
#  row.names = F
#)
}

## look at correlations and stuff
{
## import some data
new_file_pca = "/Users/kprovost/Documents/Postdoc_Working/Molothrus.ater.selections/Molothrus.ater.combined.PCA.merged.txt"
corr_data_original = read.delim(new_file_pca, header = T, sep = "\t")
colnames(corr_data_original)
corr_data = corr_data_original[, c(86:140)] ## plot all PCs and how correlated they are
corr = cor(corr_data, use = "pairwise.complete.obs")
install.packages("corrplot")
library(corrplot)
corrplot::corrplot(corr, method = "color")

corr_data_2 = corr_data_original[, c(2:40)] ## plot all the numerical raw data I can plot right now
corr_data_2 = corr_data_original[, c("Freq.25...Hz.", "Freq.95...Hz.")]
corr_2 = cor(corr_data_2, use = "pairwise.complete.obs")
corrplot::corrplot(corr_2, method = "ellipse", order = "hclust")
}

## soundshape
{
library(SoundShape)
install.packages("SoundShape")

path = "/Users/kprovost/Documents/Postdoc_Working/Molothrus.ater.selections"

wav.at = file.path(path, "ClippedWavs")
dir.create(wav.at)

store_dir = file.path(wav.at, "Stored")
dir.create(store_dir)

## cut our wavfiles
## read in our wavfiles one at a time
## first we get the wavfiles
files = list.files(path = path,
                   pattern = "wav$",
                   full.names = T)
for (my_file in files) {
  print(my_file)
  #my_file = files[30]
  wav = tuneR::readWave(my_file)
  ## look up the data with this file
  my_data_file = sub(pattern = "wav", replacement = "Table.1.selections.txt", my_file)
  
  if (file.exists(my_data_file) == TRUE) {
    wav_data = read.delim(my_data_file)
    ## use the start and end time of the data to tell R where to clip the wavfile
    #row_number = 1
    for (row_number in 1:nrow(wav_data)) {
      print(row_number)
      start_time = wav_data$Begin.Time..s.[row_number]
      stop_time  = wav_data$End.Time..s.[row_number]
      cut_wav = seewave::cutw(
        wave = wav,
        from = start_time,
        to = stop_time,
        output = "Wave"
      )
      tuneR::writeWave(cut_wav, filename = file.path(wav.at, paste(
        basename(my_file), row_number, "wav", sep = "."
      )))
    }
  }
}

## align our wavfiles!

## we need that custom function Kaiya wrote to do this because soundshape sucks sometimes

align.wave.custom <-
  function(wav.at = NULL,
           wav.to = "Aligned",
           time.length = 1,
           time.perc = 0.0,
           dBlevel = 25,
           f = 44100,
           wl = 512,
           ovlp = 70,
           overwrite = F,
           verbose = T,
           alignCheckpoint = 1)  {
    if (is.null(wav.at)) {
      stop("Use 'wav.at' to specify folder path where '.wav' files are stored")
    }
    
    # Create folder to store aligned calls
    if (!dir.exists(file.path(wav.at, wav.to)))
      dir.create(file.path(wav.at, wav.to))
    
    # Replace sounds for each ".wav" file in a folder
    filestoalign = list.files(wav.at, pattern = ".wav")
    numalignfiles = length(filestoalign)
    for (j in alignCheckpoint:numalignfiles) {
      file = filestoalign[j]
      if (verbose == T) {
        print(paste(j, file, numalignfiles))
      }
      if (overwrite == T ||
          !(file.exists(file.path(wav.at, wav.to, file)))) {
        orig.wav0 <- tuneR::readWave(paste(wav.at, "/", file, sep = ""))
        
        # Add silence to fill sound window and prevent error
        orig.wav <-
          seewave::addsilw(
            orig.wav0,
            f = f,
            at = "end",
            d = (time.length * 10),
            output = "Wave"
          )
        
        # create spectro object
        orig.spec <-
          seewave::spectro(
            orig.wav,
            f = f,
            wl = wl,
            ovlp = ovlp,
            osc = F,
            grid = F,
            plot = F
          )
        
        # Acquire contours
        cont.spec <-
          grDevices::contourLines(
            x = orig.spec$time,
            y = orig.spec$freq,
            z = t(orig.spec$amp),
            levels = seq(-dBlevel,-dBlevel, 1)
          )
        
        # vectors to store minimum and maximum time values
        min.spec <- numeric(length(cont.spec))
        max.spec <- numeric(length(cont.spec))
        
        # minimum and maximum time values among contours detected
        for (i in 1:length(min.spec)) {
          min.spec[i] <- min(cont.spec[[i]]$x)
        }
        for (i in 1:length(max.spec)) {
          max.spec[i] <- max(cont.spec[[i]]$x)
        }
        
        # minimum and maximum time values
        t.min <- min(min.spec)
        t.max <- max(max.spec)
        
        if ((t.min - (time.perc * time.length)) < 0)
          stop("Time percentage is too large. Consider a smaller value of 'time.perc'")
        
        # cut Wave file using minimum and maximum time values
        short.wav0 <- seewave::deletew(
          orig.wav,
          f = f,
          output = "Wave",
          from = (t.max + (time.perc * time.length)),
          to = max(orig.spec$time)
        )
        
        short.wav <-
          seewave::deletew(
            short.wav0,
            f = f,
            output = "Wave",
            from = 0,
            to = (t.min - (time.perc * time.length))
          )
        
        
        # Add silence to fill sound window
        final.wav <-
          seewave::addsilw(
            short.wav,
            f = f,
            at = "end",
            d = time.length,
            output = "Wave"
          )
        
        tuneR::writeWave(final.wav, file.path(wav.at, wav.to, file), extensible = F)
      } else {
        if (verbose == T) {
          print("SKIPPING")
        }
      }
    } #end loop
    
  } #end function

align.wave.custom(
  wav.at = wav.at,
  wav.to = "Aligned",
  time.length = 1.0,
  f = 48000,
  time.perc = 0,
  alignCheckpoint = 583,
  overwrite = F
)
## time.length is the percent of the sound
## try playing around with time.length if things are not working

## generate the eigensample
eig.sample <- eigensound(
  analysis.type = "threeDshape",
  f = 48000,
  log.scale = TRUE,
  wav.at = file.path(wav.at, "Aligned"),
  store.at = store_dir
)
pca.eig.sample <- stats::prcomp(
  geomorph::two.d.array(
    eig.sample
  )
)
summary(pca.eig.sample)
rotation = pca.eig.sample$rotation
importance = summary(pca.eig.sample)$importance
data = pca.eig.sample$x
rotation[1:5,1:10]
importance[,1:10]
data[1:5,1:10]
data = as.data.frame(data)
plot(data$PC1,data$PC2)

#write.table(data,"~/cowbird_pca_data.txt")
#write.table(rotation,"~/cowbird_pca_rotation.txt")
#write.table(importance,"~/cowbird_pca_importance.txt")
}

## TODO: urbanization layers

install.packages("raster")
library(raster)

wcdata = getData("worldclim",download=T,res=10,var="bio")
wcdata[[1]]
plot(wcdata[[1]])
##
crop_extent = extent(-85,-80,35,45)
wcdata_oh = crop(wcdata,crop_extent)
plot(wcdata_oh[[1]])

cowbird_data = read.csv("/Users/kprovost/Documents/Postdoc_Working/Molothrus.ater.selections/BLB_klp_Molothrus.ater.csv")
plot(wcdata_oh[[1]])
points(cowbird_data$LONGITUDE,cowbird_data$LATITUDE)

cells = cellFromXY(wcdata_oh,cowbird_data[,c("LONGITUDE","LATITUDE")])

cell_data = extract(wcdata_oh,cells)

## put together the env data and the cowbird data
cowbird_env = cbind(cowbird_data,cell_data)

old_file = "/Users/kprovost/Downloads/dataverse_files/Anthromes-12k-DGG/raw-data/HYDE/stack_1940AD.tif"
new_file = "/Users/kprovost/Downloads/dataverse_files/Anthromes-12k-DGG/raw-data/HYDE/stack_2017AD.tif"

new_urban = stack(new_file)
plot(new_urban)

layer_names = c("cropland","grazing","ir_rice","popc","tot_irri","uopp")
names(new_urban) = layer_names

crop_extent = extent(-85,-80,35,45)
new_urban_oh = crop(new_urban,crop_extent)
names(new_urban_oh) = layer_names
plot(new_urban_oh)

cells_new = cellFromXY(new_urban_oh,cowbird_data[,c("LONGITUDE","LATITUDE")])
cells_new_data = extract(new_urban_oh,cells_new)

cowbird_new_urban = cbind(cowbird_data,cells_new_data)


old_urban = stack(old_file)
plot(old_urban)

layer_names = c("cropland","grazing","ir_rice","popc","tot_irri","uopp")
names(old_urban) = layer_names

crop_extent = extent(-85,-80,35,45)
old_urban_oh = crop(old_urban,crop_extent)
names(old_urban_oh) = layer_names
plot(old_urban_oh)

cells_old = cellFromXY(old_urban_oh,cowbird_data[,c("LONGITUDE","LATITUDE")])
cells_old_data = extract(old_urban_oh,cells_old)

cowbird_old_urban = cbind(cowbird_data,cells_old_data)

## compare new data and old data
cowbird_old_urban
cowbird_new_urban
## find which columns to compare
colnames(cowbird_new_urban) ## cols 26-31 are our new data in both

cowbird_dif_urban = cowbird_new_urban[,26:31] - cowbird_old_urban[,26:31]

dif_urban_oh = new_urban_oh - old_urban_oh
plot(dif_urban_oh)

cowbird_dif_urban = cbind(cowbird_data,cowbird_dif_urban)

dev.off()
boxplot(cowbird_dif_urban$cropland,xlab="cropland",ylab="difference")
boxplot(cowbird_dif_urban$popc,xlab="population",ylab="difference")

boxplot(cowbird_dif_urban$grazing~cowbird_dif_urban$COUNTY,xlab="county",ylab="difference in grazing")
boxplot(cowbird_dif_urban$grazing~cowbird_dif_urban$SEX,xlab="sex",ylab="difference in grazing")

plot(cowbird_dif_urban$grazing,cowbird_dif_urban$LATITUDE)
plot(cowbird_dif_urban$grazing,cowbird_dif_urban$YEAR_COLLECTED)

#write.table(cowbird_dif_urban,"~/cowbird_dif_urban.txt")
#write.table(cowbird_new_urban,"~/cowbird_new_urban.txt")
#write.table(cowbird_old_urban,"~/cowbird_old_urban.txt")
writeRaster(dif_urban_oh,"~/dif_urban_oh.tif",format="GTiff")









## read.table(blah blah blah)
cowbird=read.table("/Users/kprovost/cowbird_dif_urban.txt")
cowbird$SEX
table(cowbird$SEX)
cowbird$cropland
mean(cowbird$cropland)

plot(cowbird$cropland)
plot(cowbird$ID,cowbird$cropland)
plot(cowbird$YEAR_COLLECTED,cowbird$cropland)

boxplot(cowbird$cropland)
boxplot(cowbird$cropland~cowbird$SEX)
boxplot(cowbird$cropland~cowbird$YEAR_COLLECTED)

hist(cowbird$cropland)

barplot(cowbird$cropland)
barplot(table(cowbird$SEX))
barplot(table(cowbird$YEAR_COLLECTED))

cowbird_urban=read.table("/Users/kprovost/cowbird_dif_urban.txt")
cowbird_song =read.table("/Users/kprovost/Documents/Postdoc_Working/Molothrus.ater.selections/combined_cowbird_metadata_shape_properties.txt",
                         header=T)

cowbird_urban_song = merge(cowbird_urban,cowbird_song,all=T)
head(cowbird_urban_song)

plot(cowbird_urban_song$cropland,cowbird_urban_song$Prop.PC1,col="red") ## note about what i found
plot(cowbird_urban_song$cropland,cowbird_urban_song$Prop.PC1,col=as.numeric(as.factor(cowbird_urban_song$SEX))) ## note about what i found

mod = lm(cowbird_urban_song$Prop.PC2~cowbird_urban_song$cropland)
plot(cowbird_urban_song$cropland,cowbird_urban_song$Prop.PC2,col="red") 
abline(mod)
summary(mod)

boxplot(cowbird_urban_song$Prop.PC1~cowbird_urban_song$COUNTY)
mod2 = aov(cowbird_urban_song$Prop.PC1~cowbird_urban_song$COUNTY)
summary(mod2)
TukeyHSD(mod2)

t.test(cowbird_urban_song$cropland[cowbird_urban_song$COUNTY=="Franklin"],
       cowbird_urban_song$cropland[cowbird_urban_song$COUNTY=="Ottawa"])

boxplot(cowbird_urban_song$Prop.PC1~cowbird_urban_song$MONTH_COLLECTED)


## MAPS
library(raster)
shp=raster::shapefile("/Users/kprovost/Downloads/cb_2016_us_state_500k/cb_2016_us_state_500k.shp")
plot(shp,xlim=c(-85,-80),ylim=c(38,42))
cowbird_song =read.table("/Users/kprovost/Downloads/combined_cowbird_metadata_shape_properties.txt",
                         header=T)
points(cowbird_song$Long,cowbird_song$Lat)

wcdata = getData("worldclim",download=T,res=10,var="bio")



png(filename="/Users/kprovost/Downloads/climate_and_cowbird_ohio_map.png",
    width=600,height=600)
plot(wcdata[[1]],xlim=c(-85,-80),ylim=c(38,42)) 
## we can change this to the urbanization data if we want
plot(shp,xlim=c(-85,-80),ylim=c(38,42),add=T,col="grey") 
points(cowbird_song$Long,cowbird_song$Lat,col="red")
dev.off()

#stack_1940 = raster::raster("~/Desktop/stack_1940AD.tif")
stack_1940 = raster::stack("~/Desktop/stack_1940AD.tif")
stack_2017 = raster::stack("~/Desktop/stack_2017AD.tif")
plot(stack_1940[[1]],xlim=c(-85,-80),ylim=c(38,42)) 

## panels
par(mfrow=c(2,2))
plot(stack_1940[[1]],xlim=c(-85,-80),ylim=c(38,42),main="cropland, 1940AD") 
plot(stack_1940[[2]],xlim=c(-85,-80),ylim=c(38,42),main="grazing, 1940AD") 
#plot(stack_1940[[3]],xlim=c(-85,-80),ylim=c(38,42),main="rice irrigation, 1940AD") 
plot(stack_1940[[4]],xlim=c(-85,-80),ylim=c(38,42),main="census population, 1940AD") 
#plot(stack_1940[[5]],xlim=c(-85,-80),ylim=c(38,42),main="total irrigation, 1940AD") 
plot(stack_1940[[6]],xlim=c(-85,-80),ylim=c(38,42),main="urban occupancy, 1940AD") 



## going to redo some analyses 
cowbird_data <- read.delim("~/Work/OSU/Molothrus ater/cowbird pca and anthromes data 5Dec2023.txt")
## lms and aovs
## make sure correct for latitude


m1 = lm(cowbird_data$PC2~cowbird_data$cropland.new*cowbird_data$Lat)
summary(m1)
par(mfrow=c(1,2))
plot(cowbird_data$Lat,cowbird_data$PC2)
abline(lm(cowbird_data$PC2~cowbird_data$Lat),col="red")
plot(cowbird_data$cropland.new,cowbird_data$PC2)
abline(lm(cowbird_data$PC2~cowbird_data$cropland.new),col="red")

par(mfrow=c(1,1))
plot(cowbird_data$Lat,cowbird_data$cropland.dif)

par(mfrow=c(1,2))
boxplot(cowbird_data$PC2~cowbird_data$COUNTY)
boxplot(cowbird_data$PC2~cowbird_data$MONTH)



sink(file="~/Work/OSU/Molothrus ater/test_aovs.txt")
for(i in c(1:4,9:18,21:41)) {
  print(i)
  print(colnames(cowbird_data)[i])
  as1=aov(cowbird_data$Shp.PC1~cowbird_data[,i])
  as2=aov(cowbird_data$Shp.PC2~cowbird_data[,i])
  as3=aov(cowbird_data$Shp.PC3~cowbird_data[,i])
  as4=aov(cowbird_data$Shp.PC4~cowbird_data[,i])
  as5=aov(cowbird_data$Shp.PC5~cowbird_data[,i])
  ap1=aov(cowbird_data$PC1~cowbird_data[,i])
  ap2=aov(cowbird_data$PC2~cowbird_data[,i])
  ap3=aov(cowbird_data$PC3~cowbird_data[,i])
  print(summary(as1))
  print(summary(as2))
  print(summary(as3))
  print(summary(as4))
  print(summary(as5))
  print(summary(ap1))
  print(summary(ap2))
  print(summary(ap3))
}
sink(file=NULL)
sink(file="~/Work/OSU/Molothrus ater/test_lms.txt")
for(i in c(1:4,9:18,21:41)) {
  print(i)
  print(colnames(cowbird_data)[i])
  as1=lm(cowbird_data$Shp.PC1~cowbird_data[,i])
  as2=lm(cowbird_data$Shp.PC2~cowbird_data[,i])
  as3=lm(cowbird_data$Shp.PC3~cowbird_data[,i])
  as4=lm(cowbird_data$Shp.PC4~cowbird_data[,i])
  as5=lm(cowbird_data$Shp.PC5~cowbird_data[,i])
  ap1=lm(cowbird_data$PC1~cowbird_data[,i])
  ap2=lm(cowbird_data$PC2~cowbird_data[,i])
  ap3=lm(cowbird_data$PC3~cowbird_data[,i])
  print(summary(as1))
  print(summary(as2))
  print(summary(as3))
  print(summary(as4))
  print(summary(as5))
  print(summary(ap1))
  print(summary(ap2))
  print(summary(ap3))
}
sink(file=NULL)
sink(file="~/Work/OSU/Molothrus ater/test_aovs_latitude.txt")
for(i in c(1:4,9:18,21:41)) {
  print(i)
  print(colnames(cowbird_data)[i])
  as1=aov(cowbird_data$Shp.PC1~cowbird_data[,i]+cowbird_data$Lat)
  as2=aov(cowbird_data$Shp.PC2~cowbird_data[,i]+cowbird_data$Lat)
  as3=aov(cowbird_data$Shp.PC3~cowbird_data[,i]+cowbird_data$Lat)
  as4=aov(cowbird_data$Shp.PC4~cowbird_data[,i]+cowbird_data$Lat)
  as5=aov(cowbird_data$Shp.PC5~cowbird_data[,i]+cowbird_data$Lat)
  ap1=aov(cowbird_data$PC1~cowbird_data[,i]+cowbird_data$Lat)
  ap2=aov(cowbird_data$PC2~cowbird_data[,i]+cowbird_data$Lat)
  ap3=aov(cowbird_data$PC3~cowbird_data[,i]+cowbird_data$Lat)
  print(summary(as1))
  print(summary(as2))
  print(summary(as3))
  print(summary(as4))
  print(summary(as5))
  print(summary(ap1))
  print(summary(ap2))
  print(summary(ap3))
}
sink(file=NULL)
sink(file="~/Work/OSU/Molothrus ater/test_lms_latitude.txt")
for(i in c(1:4,9:18,21:41)) {
  print(i)
  print(colnames(cowbird_data)[i])
  as1=lm(cowbird_data$Shp.PC1~cowbird_data[,i]+cowbird_data$Lat)
  as2=lm(cowbird_data$Shp.PC2~cowbird_data[,i]+cowbird_data$Lat)
  as3=lm(cowbird_data$Shp.PC3~cowbird_data[,i]+cowbird_data$Lat)
  as4=lm(cowbird_data$Shp.PC4~cowbird_data[,i]+cowbird_data$Lat)
  as5=lm(cowbird_data$Shp.PC5~cowbird_data[,i]+cowbird_data$Lat)
  ap1=lm(cowbird_data$PC1~cowbird_data[,i]+cowbird_data$Lat)
  ap2=lm(cowbird_data$PC2~cowbird_data[,i]+cowbird_data$Lat)
  ap3=lm(cowbird_data$PC3~cowbird_data[,i]+cowbird_data$Lat)
  print(summary(as1))
  print(summary(as2))
  print(summary(as3))
  print(summary(as4))
  print(summary(as5))
  print(summary(ap1))
  print(summary(ap2))
  print(summary(ap3))
}
sink(file=NULL)

## kaiya doing some stuff that Kristen does not have to do 
{
## 25 shape PCAs to get over 50%
shape_pca = read.table("/Users/kprovost/Documents/Postdoc_Working/Molothrus.ater.selections/cowbird_pca_data.txt")
shape_pca$ID = rownames(shape_pca)
shape_pca_small = shape_pca[,c("ID","PC1","PC2","PC3","PC4","PC5")]
colnames(shape_pca_small) = c("ID","Shp.PC1","Shp.PC2","Shp.PC3","Shp.PC4","Shp.PC5")
shape_pca_small$Genus = "Molothrus"
shape_pca_small$Species = "ater"
shape_pca_small$ID=sub("Molothrus.ater.BLB.","",shape_pca_small$ID)
shape_pca_small$ID=sub(".wav","",shape_pca_small$ID)
## split hte ID into ID and syllable
syllable=sapply(shape_pca_small$ID,FUN=function(x){
  strsplit(x,"\\.")[[1]][2]
})
IDs=sapply(shape_pca_small$ID,FUN=function(x){
  strsplit(x,"\\.")[[1]][1]
})
shape_pca_small$Selection = as.numeric(syllable)
shape_pca_small$ID = as.numeric(IDs)

properties_pca = read.table("/Users/kprovost/Documents/Postdoc_Working/Molothrus.ater.selections/Molothrus.ater.combined.PCA.merged.txt",sep="\t",header=T)
properties_pca_small = properties_pca[,c("Begin.File","PC1","PC2","PC3","PC4","PC5","Selection")]
colnames(properties_pca_small) = c("ID","Prop.PC1","Prop.PC2","Prop.PC3","Prop.PC4","Prop.PC5","Selection")
properties_pca_small$Genus = "Molothrus"
properties_pca_small$Species = "ater"
properties_pca_small$ID=sub("BLB","",properties_pca_small$ID)
properties_pca_small$ID=sub(".wav","",properties_pca_small$ID)

head(shape_pca_small)
head(properties_pca_small)
shape_prop_pca_small = merge(shape_pca_small,properties_pca_small,all=T)
plot(shape_prop_pca_small$Prop.PC1,shape_prop_pca_small$Shp.PC1)


metadata = read.csv("/Users/kprovost/Documents/Postdoc_Working/Molothrus.ater.selections/BLB_klp_Molothrus.ater.csv")
metadata_small = metadata[,c("ID","GENUS_NAME","SPECIES_EPITHET","YEAR_COLLECTED","LONGITUDE","LATITUDE")]
colnames(metadata_small) = c("ID","Genus","Species","Year","Long","Lat")

shape_prop_pca_small$Year = 0
shape_prop_pca_small$Long = 0
shape_prop_pca_small$Lat = 0

for(i in 1:nrow(metadata_small)){
  print(i)
  meta_df = metadata_small[i,]
  shape_prop_pca_small$Year[shape_prop_pca_small$ID == meta_df$ID] = meta_df$Year
  shape_prop_pca_small$Long[shape_prop_pca_small$ID == meta_df$ID] = meta_df$Long
  shape_prop_pca_small$Lat[shape_prop_pca_small$ID == meta_df$ID] = meta_df$Lat
}
#write.table(shape_prop_pca_small,"~/combined_cowbird_metadata_shape_properties.txt")
shape_prop_pca_small = read.table("/Users/kprovost/Documents/Postdoc_Working/combined_cowbird_metadata_shape_properties.txt",header=T)
plot(shape_prop_pca_small$Prop.PC1,shape_prop_pca_small$Shp.PC1)
boxplot(shape_prop_pca_small$Shp.PC1~shape_prop_pca_small$Year)

corrplot::corrplot(cor(shape_prop_pca_small[,5:14],use="pairwise.complete.obs"),method="ellipse")

## trying out a clustering algorithm
id=1322
small = shape_prop_pca_small[shape_prop_pca_small$ID==id,]
plot(small$Shp.PC1,small$Shp.PC2)
}


### code from 2 July 2024
df <- read.delim("~/Work/OSU/Molothrus ater/Data/combined_cowbird_metadata_shape_properties.txt")

## need to extract the env data

## run a generalized linear mixed model for each pc
## time*urb + 1|lat + 1|lon + 1|recording

library(raster)
ras_files = list.files(path="~/Work/OSU/Molothrus ater/Data/anthromes/raw-data/HYDE",
                       pattern="*.tif$",full.names = T)

for(file in ras_files[6]) {
  ## iterate over each raster in the folder
  print(file)
  r = stack(file)
  r_names = sub("X","",names(r))
  
  ## iterate over each layer
  for(i in 1:dim(r)[3]){
    cat(i)
    ri = r[[i]]
   
    ## generate a filename
    ri_filename = sub("tif",paste(r_names[i],".tif",sep=""),file)
    
    ## write out the single layer as its own raster
   writeRaster(ri,ri_filename,format="GTiff",overwrite=T)
  }
}
## gzip anything with BC in the name?
#R.utils::gzip()
