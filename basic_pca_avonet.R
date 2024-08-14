#df = read.csv("/Users/kprovost/Documents/AVONET Supplementary dataset 1.csv",header=T)
df = read.csv("/Users/kprovost/Documents/Chondestes_grammacus/LASP_combined_annotations_18July2024.Table.1.selections.txt",sep="\t",header=T)
View(df)

## to do a PCA 
## you need numeric data only 
## make a numeric only dataset 

## for song data remove all columns that have "samples" in the name
sample_cols <- grepl(pattern="samples",x=colnames(df))
## remove those columns
df = df[,!sample_cols]
#df = df[complete.cases(df),]

#df_num = df[,9:19]
df_num = dplyr::select_if(df,is.numeric)
df_num = df_num[complete.cases(df_num),]
## check and see if all of them are identical 
col_stdevs = matrixStats::colSds(as.matrix(df_num))
bad_columns = which(col_stdevs == 0)
df_num = df_num[,-bad_columns]
## df_num = df_num[,col_stdevs != 0]

my_cor = cor(df_num,use="pairwise.complete.obs")
my_cor >= 0.75
my_cor <= -0.75

corrplot::corrplot(my_cor,method="color",order="hclust",diag=FALSE)
corrplot::corrplot(my_cor >= 0.75,method="color",order="hclust",diag=FALSE)
corrplot::corrplot(my_cor <= -0.75,method="color",order="hclust",diag=FALSE)


## anything that is 100% correlated, just pick one and take it out of the data
## before we do a PCA on it 

write.table(x=my_cor,
            file="/Users/kprovost/Documents/Chondestes_grammacus/correlations_to_remove.csv",
            sep=",")

## these are the columns i kept after removing correlations of 0.75 and above
good_columns = c("Agg.Entropy..bits.","Avg.Entropy..bits.","Avg.Power.Density..dB.FS.Hz.","BW.50...Hz.","BW.90...Hz.","Center.Time.Rel.","Delta.Freq..Hz.","Delta.Time..s.","Dur.50...s.","Dur.90...s.","Energy..dB.FS.","Freq.95...Hz.","Inband.Power..dB.FS.","Length..frames.","Max.Entropy..bits.","Min.Entropy..bits.","Peak.Power.Density..dB.FS.Hz.","Peak.Time.Relative","PFC.Avg.Slope..Hz.ms.","PFC.Max.Freq..Hz.","PFC.Max.Slope..Hz.ms.","PFC.Min.Freq..Hz.","PFC.Min.Slope..Hz.ms.","PFC.Num.Inf.Pts","Selection","Time.25..Rel.","Time.5..Rel.","Time.75..Rel.","Time.95..Rel.")

## subset df_num to be only the good columns

df_num = df_num[,good_columns]

## this does the pca
pca = prcomp(df_num,center=TRUE,scale. = TRUE)
## when scaling, you can only give columns where the column does not have 
## all identical values, or a stdev of 0
summary(pca)
sort(pca$rotation[,"PC2"])

## extract the pca data itself
pca$x
## combine the original and pca data together
df_pca = cbind(df[rownames(pca$x),],pca$x)



## subset to passeriformes so that we have less to look at 
#df_pca_pass = df_pca[df_pca$Order3=="Passeriformes",]

## still too big, subset to just those families
#df_pca_pass = df_pca_pass[df_pca_pass$Family3 %in% c("Passerellidae","Passeridae","Tyrannidae","Tityridae"),]

## plot the pca with each family being its own color and shape
plot(df_pca_pass$PC2,df_pca_pass$PC3,
    col=as.numeric(as.factor(df_pca_pass$Family3)),
    pch=as.numeric(as.factor(df_pca_pass$Family3)))

boxplot(df_pca_pass$PC1~df_pca_pass$Family3)
boxplot(df_pca_pass$PC2~df_pca_pass$Family3)

mod = aov(df_pca_pass$PC1~df_pca_pass$Family3)
summary(mod)

plot(df_pca_pass$Max.Latitude,df_pca_pass$PC1,
     col=as.numeric(as.factor(df_pca_pass$Family3)),
     pch=as.numeric(as.factor(df_pca_pass$Family3)))

# broken stick
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

## dim(pca$x), second number goes in the broken stick
expected = broken_stick(19) 
## you don't need to do this after you subset the PCs
actual = summary(pca)$importance[2,] ## proportion of variance only 
plot(expected,actual)
abline(a=0,b=1)
actual > expected
## keep 6 pcs because actual > expected 
## for avonet data we keep 1 pc
## for shannon's data we are retaining top 2
## for sofia's data we are retaining top XXX

## combine the original and pca data together, but only with the PCs that pass broken stick
df_pca = cbind(df[rownames(pca$x),],pca$x[,c("PC1","PC2")])

#dim(final_df)
dim(pca$x)

## now we can do any analyses on the PCs instead of the raw data
## export these data, including the PCA statistics that we need
write.table(x=df_pca,file="/Users/kprovost/Documents/AVONET.Supplementary.dataset.1_PCA_18July2024.csv",
            sep=",",row.names = FALSE)
## next export the rotations
rotations = pca$rotation
write.table(x=rotations,file="/Users/kprovost/Documents/AVONET.Supplementary.dataset.1_PCA_Rotations_18July2024.csv",
            sep=",",row.names = TRUE)
## then export the importance
importance = summary(pca)$importance
write.table(x=importance,file="/Users/kprovost/Documents/AVONET.Supplementary.dataset.1_PCA_Importance_18July2024.csv",
            sep=",",row.names = TRUE)
## export the standard deviations
stdevs = pca$sdev
write.table(x=stdevs,file="/Users/kprovost/Documents/AVONET.Supplementary.dataset.1_PCA_StDevs_18July2024.csv",
            sep=",",row.names = FALSE)

boxplot(df_pca$PC2~df_pca$Channel)
