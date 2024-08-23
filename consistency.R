##### 
sigmas = aggregate(df$Prop.PC1~df$BLB_ID,
          FUN=function(x){sd(x,na.rm=T)})

mus = aggregate(df$Prop.PC1~df$BLB_ID,
                FUN=function(x){mean(x,na.rm=T)})

zscore = function(x,mu,sigma) {
  Z = (x-mu)/sigma
  return(Z)
}
zscore(0,1,0.5)
#####
id = df$BLB_ID[1]

ind_zscores = sapply(unique(df$BLB_ID),FUN=function(id){
  print(id)
  ## subset the data df by the ID id
  subset_df = df[df$BLB_ID == id,]
  
  ## take the mean (mu) and sd (sigma) of all points in ID
  mu = mean(subset_df$Prop.PC1,na.rm=T)
  sigma = sd(subset_df$Prop.PC1,na.rm=T)
  
  ## calculate a zscore for each 
  Z = zscore(x=subset_df$Prop.PC1,
             mu=mu,
             sigma=sigma)
  
  ## take the absolute value average zscore for the individual
  absZ = abs(Z)
  mean_absZ = mean(absZ,na.rm=T)
  ## print
  names(mean_absZ) = id
  return(mean_absZ)
})
#names(ind_zscores) = unique(df$BLB_ID)

## convert the zscores into a dataframe 
ind_zscores_df = as.data.frame(ind_zscores)
#ind_zscores_df = do.call(rbind,ind_zscores)
hist(as.numeric(ind_zscores_df$ind_zscores))

par(mfrow=c(2,2))
hist(sigmas[,2])
hist(as.numeric(ind_zscores_df$ind_zscores))

## set up the dataframes for ind_zscores_df and sigmas so we can merge with metadata
metadata_df = df[,c(1,2,4:17)]
metadata_df = unique(metadata_df)

colnames(sigmas) = c("BLB_ID","Prop.PC1_sigma")
colnames(ind_zscores_df) = "zscore"
ind_zscores_df = as.data.frame(ind_zscores_df)
rownames(ind_zscores_df) = unique(df$BLB_ID)
ind_zscores_df$BLB_ID = rownames(ind_zscores_df)

## merge these together
## start by merging sigmas and ind_zscores_df
merged_df = merge(sigmas,ind_zscores_df)
plot(merged_df$Prop.PC1_sigma,merged_df$zscore)
merged_df = merge(merged_df,metadata_df)
head(merged_df)

## get rid of individual syllables before plotting light pollution and averages
## made a df that is only the data you want to plot
to_plot_df = merged_df[,c("Prop.PC1_sigma","zscore","YEAR")]
to_plot_df = unique(to_plot_df)
plot(to_plot_df$YEAR,to_plot_df$Prop.PC1_sigma)

for(id in unique(df$BLB_ID)) {
  print(id)
  ## subset the data df by the ID id
  subset_df = df[df$BLB_ID == id,]
  
  ## take the mean (mu) and sd (sigma) of all points in ID
  mu = mean(subset_df$Prop.PC1,na.rm=T)
  sigma = sd(subset_df$Prop.PC1,na.rm=T)
  
  ## calculate a zscore for each 
  Z = zscore(x=subset_df$Prop.PC1,
         mu=mu,
         sigma=sigma)
  
  ## take the absolute value average zscore for the individual
  absZ = abs(Z)
  mean_absZ = mean(absZ,na.rm=T)
  ## print
  print(mean_absZ)
}





