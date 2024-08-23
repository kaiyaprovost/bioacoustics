#df = read.csv("/Users/kprovost/Documents/AVONET Supplementary dataset 1.csv",header=T)
df = read.csv("/Users/kprovost/Documents/Research/Tyrannidae/predicted_annotations/rvn.dat_trimmed_spectro_fcts_COMBINED_15Aug2024.txt",sep="\t",header=T)

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
            file="/Users/kprovost/Documents/Research/Tyrannidae/predicted_annotations/correlations_to_remove.csv",
            sep=",")

## these are the columns i kept after removing correlations of 0.75 and above
good_columns = c("selec","mean_slope","inflections","sd","freq.median","time.IQR","skew","kurt","time.ent","mindom","maxdom","modindx","startdom","enddom","dfslope","meanpeakf")

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
## if they have the same number of rows
df_pca = cbind(df[rownames(pca$x),],pca$x) 

## if they don't 
## cbind together the x data (has the removed NAs) and the pca$x data
x_pca = cbind(x,pca$x)

intersect(colnames(df),colnames(x_pca))
df_pca = merge(x=df,y=x_pca,all=T)
dim(df)
dim(x_pca)
dim(df_pca) ## if this looks like nrow(df) + nrow(x_pca), something went wrong

palette(c("black","red","blue","cyan","pink","green","darkgreen"))
legend("bottomright",
       legend=c(unique(as.factor(df_pca$species))),
       col=c(unique(as.numeric(as.factor(df_pca$species)))),
       pch=16,
       title="Specific epithet"
       )
print(levels(as.factor(df_pca$species))) ## the order here should match the order in the palette

mod = aov(pc1~species)
summary(mod)
TukeyHSD(mod)

mean_pc1 = aggregate(df$pc1~df$individual+df$species,FUN=function(x){mean(x,na.rm=T)})





df_pca_pass = df_pca
## subset to passeriformes so that we have less to look at 
#df_pca_pass = df_pca[df_pca$Order3=="Passeriformes",]

## still too big, subset to just those families
#df_pca_pass = df_pca_pass[df_pca_pass$Family3 %in% c("Passerellidae","Passeridae","Tyrannidae","Tityridae"),]

## plot the pca with each family being its own color and shape
plot(df_pca_pass$PC2,df_pca_pass$PC3)

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
expected = broken_stick(16) 
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
write.table(x=df_pca,file="/Users/kprovost/Documents/Research/Tyrannidae/predicted_annotations/rvn.dat_trimmed_spectro_fcts_COMBINED_15Aug2024_PCA.csv",
            sep=",",row.names = FALSE)
## next export the rotations
rotations = pca$rotation
write.table(x=rotations,file="/Users/kprovost/Documents/Research/Tyrannidae/predicted_annotations/rvn.dat_trimmed_spectro_fcts_COMBINED_15Aug2024_PCA_Rotations.csv",
            sep=",",row.names = TRUE)
## then export the importance
importance = summary(pca)$importance
brokenstick=expected
importance = rbind(importance,brokenstick)
write.table(x=importance,file="/Users/kprovost/Documents/Research/Tyrannidae/predicted_annotations/rvn.dat_trimmed_spectro_fcts_COMBINED_15Aug2024_PCA_Importance.csv",
            sep=",",row.names = TRUE)
## export the standard deviations
stdevs = pca$sdev
write.table(x=stdevs,file="/Users/kprovost/Documents/Research/Tyrannidae/predicted_annotations/rvn.dat_trimmed_spectro_fcts_COMBINED_15Aug2024_PCA_StDevs.csv",
            sep=",",row.names = FALSE)

boxplot(df_pca$PC2~df_pca$Channel)

p
