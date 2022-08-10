## read data 

df = read.table("/Users/kprovost/Documents/Postdoc_Working/Sounds_and_Annotations/centroids_per_individual_bigpca_MASTER_withmetadata.txt",sep="\t",header=T)
df$scientific = paste(df$GENUS,df$SPECIES,sep="_")
df=df[df$FAMILY=="Passerellidae",]
df$LATITUDE=as.numeric(df$LATITUDE)
df$LONGITUDE=as.numeric(df$LONGITUDE)

## phylogeny
phylo = ape::read.tree("/Users/kprovost/Documents/Passerellidae_species_timetreedotorg_5July2022.nwk")
plot(phylo)
phylo_dist=cophenetic.phylo(phylo)
max_phylo_dist  = max(phylo_dist,na.rm=T)

## calculate overall diversity -- not accounting for sampling or anything
spptab = table(df$scientific)
overall_shannon = vegan::diversity(spptab,index="shannon")
num_uniq_spp = length(unique(df$scientific))

hypervolume=geometry::convhulln(df[,c("BANDWIDTH","CENTER","INFLECTION","SLOPE","TIME")],output.options="FA")
volume = hypervolume$vol

hypervolume_pc=geometry::convhulln(df[,c("PC1","PC2","PC3","PC4","PC5")],output.options="FA")
volume_pc = hypervolume_pc$vol

ras = raster(res=c(10,10),ext=extent(-170,-30,-60,80))
values(ras) = NA
plot(ras)

## can we calculate the distances per community? what is a community? 
df$cells = raster::cellFromXY(ras,df[,c("LONGITUDE","LATITUDE")])
celltab = table(df$cells)
values(ras)[as.numeric(names(celltab))] = as.numeric(celltab)
plot(ras,colNA="black")

ras_num_uniq_spp = ras; values(ras_num_uniq_spp) = NA
ras_overall_shannon = ras; values(ras_overall_shannon) = NA
ras_hyper_vol = ras; values(ras_hyper_vol) = NA
ras_hyper_vol_pca = ras; values(ras_hyper_vol_pca) = NA
ras_dist = ras; values(ras_dist) = NA
ras_dist_pca = ras; values(ras_dist_pca) = NA
ras_phylo = ras; values(ras_phylo) = NA





## first we want to extract all individuals from one cell
for(cellnum in sort(unique(df$cells))){
  df_c = unique(df[df$cells==cellnum,])
  df_c = df_c[complete.cases(df_c$ID),]
  spptab_c = table(df_c$scientific)
  overall_shannon_c = vegan::diversity(spptab_c,index="shannon")
  ras_overall_shannon[cellnum] = overall_shannon_c
  uniq_spp_c = unique(df_c$scientific)
  num_uniq_spp_c = length(uniq_spp_c)
  ras_num_uniq_spp[cellnum] = num_uniq_spp_c
  
  df_c_small = df_c[,c("BANDWIDTH","CENTER","INFLECTION","SLOPE","TIME","PC1","PC2","PC3","PC4","PC5")]
  df_c_small = df_c_small[complete.cases(df_c_small),]
  
  overall_dist = dist(df_c_small[,c("BANDWIDTH","CENTER","INFLECTION","SLOPE","TIME")],diag = F,upper=F)
  overall_dist_pc = dist(df_c_small[,c("PC1","PC2","PC3","PC4","PC5")],diag = F,upper=F)
  max_dist = max(overall_dist,na.rm=F)
  max_dist_pc = max(overall_dist_pc,na.rm=F)
  if(is.infinite(max_dist)){max_dist=NA}
  if(is.infinite(max_dist_pc)){max_dist_pc=NA}
  ## these don't correlate? 
  
  ras_dist[cellnum] = max_dist
  ras_dist_pca[cellnum] = max_dist_pc
  
  try({
    phylo_dist_c = phylo_dist[rownames(phylo_dist) %in% uniq_spp_c,colnames(phylo_dist) %in% uniq_spp_c]
    max_phylo_dist_c  = max(phylo_dist_c,na.rm=T)
    if(is.infinite(max_phylo_dist_c)){max_phylo_dist_c=NA}
    ras_phylo[cellnum] = max_phylo_dist_c
  })
  
  
  if(nrow(df_c_small)>=6){
    hypervolume_c=geometry::convhulln(df_c_small[,c("BANDWIDTH","CENTER","INFLECTION","SLOPE","TIME")],output.options="FA")
    volume_c = hypervolume_c$vol
    ras_hyper_vol[cellnum] = volume_c
    
    hypervolume_pc_c=geometry::convhulln(df_c_small[,c("PC1","PC2","PC3","PC4","PC5")],output.options="FA")
    volume_pc_c = hypervolume_pc_c$vol
    ras_hyper_vol_pca[cellnum] = volume_pc_c
  }
  
  
}

png("~/community_metrics_through_space_song.png")
par(mfrow=c(2,4))
plot(ras,colNA="grey",main="sampling",col=viridis::plasma(100))
plot(ras_overall_shannon,colNA="grey",main="shannon",col=viridis::plasma(100))
plot(ras_num_uniq_spp,colNA="grey",main="numspp",col=viridis::plasma(100))
plot(ras_hyper_vol,colNA="grey",main="hypervol",col=viridis::plasma(100))
plot(ras_hyper_vol_pca,colNA="grey",main="pc hypervol",col=viridis::plasma(100))
plot(ras_dist,colNA="grey",main="max dist",col=viridis::plasma(100))
plot(ras_dist_pca,colNA="grey",main="max dist pca",col=viridis::plasma(100))
plot(ras_phylo,colNA="grey",main="phylo",col=viridis::plasma(100))
dev.off()

ras_stack = stack(c(ras,ras_overall_shannon,ras_num_uniq_spp,
                    ras_hyper_vol,ras_hyper_vol_pca,ras_dist,
                    ras_dist_pca,ras_phylo))
names(ras_stack) = c("Sampling",
                     "Shannon",
                     "NUniqSpecies",
                     "Hypervolume",
                     "HypervolumePCA",
                     "MaxDistance",
                     "MaxDistancePCA",
                     "PhyloDist")
writeRaster(ras_stack,"~/raster_community_song_metrics_stack.tiff",format="GTiff",overwrite=T)

## generate function to permute
permute_fun = function(df,colnames){
  df_temp = df
  sample_order = sample(1:nrow(df_temp))
  df_temp[,colnames] = df_temp[sample_order,colnames]
  return(df_temp)
}


## randomize within community
df_list = lapply(1:1000,FUN=function(x){permute_fun(df,"scientific")})

## do it again 
df_output = lapply(1:length(df_list),FUN=function(i){
  print(i)
  df_i = df_list[[i]]
  
  newdf = data.frame()
  
  for(cellnum in sort(unique(df_i$cells))){
    #print(paste(i,cellnum))
    df_c = unique(df_i[df_i$cells==cellnum,])
    df_c = df_c[complete.cases(df_c$ID),]
    spptab_c = table(df_c$scientific)
    overall_shannon_c = vegan::diversity(spptab_c,index="shannon")
    uniq_spp_c = unique(df_c$scientific)
    num_uniq_spp_c = length(uniq_spp_c)
    
    df_c_small = df_c[,c("BANDWIDTH","CENTER","INFLECTION","SLOPE","TIME","PC1","PC2","PC3","PC4","PC5")]
    df_c_small = df_c_small[complete.cases(df_c_small),]
    
    overall_dist = dist(df_c_small[,c("BANDWIDTH","CENTER","INFLECTION","SLOPE","TIME")],diag = F,upper=F)
    overall_dist_pc = dist(df_c_small[,c("PC1","PC2","PC3","PC4","PC5")],diag = F,upper=F)
    ## these don't correlate? 
    max_dist = max(overall_dist,na.rm=F)
    max_dist_pc = max(overall_dist_pc,na.rm=F)
    if(is.infinite(max_dist)){max_dist=NA}
    if(is.infinite(max_dist_pc)){max_dist_pc=NA}
    
    max_phylo_dist_c=NA
    try({
      phylo_dist_c = phylo_dist[rownames(phylo_dist) %in% uniq_spp_c,colnames(phylo_dist) %in% uniq_spp_c]
      max_phylo_dist_c  = max(phylo_dist_c,na.rm=T)
      if(is.infinite(max_phylo_dist_c)){max_phylo_dist_c=NA}
    })
    
    volume_c=NA
    volume_pc_c=NA
    if(nrow(df_c_small)>=6){
      hypervolume_c=geometry::convhulln(df_c_small[,c("BANDWIDTH","CENTER","INFLECTION","SLOPE","TIME")],output.options="FA")
      volume_c = hypervolume_c$vol
      
      hypervolume_pc_c=geometry::convhulln(df_c_small[,c("PC1","PC2","PC3","PC4","PC5")],output.options="FA")
      volume_pc_c = hypervolume_pc_c$vol
    }
    
    new_row = cbind(i,cellnum,overall_shannon_c,num_uniq_spp_c,max_dist,
                    max_dist_pc,max_phylo_dist_c,volume_c,volume_pc_c)
    newdf = rbind(newdf,new_row)
  }
  
  return(newdf)
})

df_output_fin = do.call(rbind,df_output)
df_output_fin = unique(df_output_fin)
write.table(df_output_fin,"~/permutations_for_song_across_space.txt",append=T)

## now need to do the pseudo pval 
## EDIT THIS SO THAT IT DOES LESS, EQUAL, GREATER
pseudo_pval_fun = function(df_output_fin){
  pval_per_cell_df = data.frame()
  for(cellnum in sort(unique(df_output_fin$cellnum))){
    ## just do it with one value for now -- max_dist? 
    small_output = df_output_fin[df_output_fin$cellnum==cellnum,]
    true_value_shannon = ras_overall_shannon[cellnum]
    p_shannon_less = sum(small_output$overall_shannon_c < true_value_shannon,na.rm=T) / sum(complete.cases(small_output$overall_shannon_c),na.rm=T)
    p_shannon_equal = sum(small_output$overall_shannon_c == true_value_shannon,na.rm=T) / sum(complete.cases(small_output$overall_shannon_c),na.rm=T)
    p_shannon_greater = sum(small_output$overall_shannon_c > true_value_shannon,na.rm=T) / sum(complete.cases(small_output$overall_shannon_c),na.rm=T)
    
    true_value_nspp = ras_num_uniq_spp[cellnum]
    p_nspp_less = sum(small_output$num_uniq_spp_c < true_value_nspp,na.rm=T) / sum(complete.cases(small_output$num_uniq_spp_c),na.rm=T)
    p_nspp_equal = sum(small_output$num_uniq_spp_c == true_value_nspp,na.rm=T) / sum(complete.cases(small_output$num_uniq_spp_c),na.rm=T)
    p_nspp_greater = sum(small_output$num_uniq_spp_c > true_value_nspp,na.rm=T) / sum(complete.cases(small_output$num_uniq_spp_c),na.rm=T)
    
    true_value_max_dist = ras_dist[cellnum]
    p_max_dist_less = sum(small_output$max_dist < true_value_max_dist,na.rm=T) / sum(complete.cases(small_output$max_dist),na.rm=T)
    p_max_dist_equal = sum(small_output$max_dist == true_value_max_dist,na.rm=T) / sum(complete.cases(small_output$max_dist),na.rm=T)
    p_max_dist_greater = sum(small_output$max_dist > true_value_max_dist,na.rm=T) / sum(complete.cases(small_output$max_dist),na.rm=T)
    
    true_value_max_dist_pc = ras_dist_pca[cellnum]
    p_max_dist_pc_less = sum(small_output$max_dist_pc < true_value_max_dist_pc,na.rm=T) / sum(complete.cases(small_output$max_dist_pc),na.rm=T)
    p_max_dist_pc_equal = sum(small_output$max_dist_pc == true_value_max_dist_pc,na.rm=T) / sum(complete.cases(small_output$max_dist_pc),na.rm=T)
    p_max_dist_pc_greater = sum(small_output$max_dist_pc > true_value_max_dist_pc,na.rm=T) / sum(complete.cases(small_output$max_dist_pc),na.rm=T)
    
    true_value_phylo = ras_phylo[cellnum]
    p_phylo_less = sum(small_output$max_phylo_dist_c < true_value_phylo,na.rm=T) / sum(complete.cases(small_output$max_phylo_dist_c),na.rm=T)
    p_phylo_equal = sum(small_output$max_phylo_dist_c == true_value_phylo,na.rm=T) / sum(complete.cases(small_output$max_phylo_dist_c),na.rm=T)
    p_phylo_greater = sum(small_output$max_phylo_dist_c > true_value_phylo,na.rm=T) / sum(complete.cases(small_output$max_phylo_dist_c),na.rm=T)
    
    true_value_vol = ras_hyper_vol[cellnum]
    p_vol_less = sum(small_output$volume_c < true_value_vol,na.rm=T) / sum(complete.cases(small_output$volume_c),na.rm=T)
    p_vol_equal = sum(small_output$volume_c == true_value_vol,na.rm=T) / sum(complete.cases(small_output$volume_c),na.rm=T)
    p_vol_greater = sum(small_output$volume_c > true_value_vol,na.rm=T) / sum(complete.cases(small_output$volume_c),na.rm=T)
    
    true_value_vol_pc = ras_hyper_vol_pca[cellnum]
    p_vol_pc_less = sum(small_output$volume_pc_c < true_value_vol_pc,na.rm=T) / sum(complete.cases(small_output$volume_pc_c),na.rm=T)
    p_vol_pc_equal = sum(small_output$volume_pc_c == true_value_vol_pc,na.rm=T) / sum(complete.cases(small_output$volume_pc_c),na.rm=T)
    p_vol_pc_greater = sum(small_output$volume_pc_c > true_value_vol_pc,na.rm=T) / sum(complete.cases(small_output$volume_pc_c),na.rm=T)
    
    new_row = cbind(cellnum,p_shannon_less,p_shannon_equal,p_shannon_greater,
                    p_nspp_less, p_nspp_equal, p_nspp_greater,
                    p_max_dist_less,p_max_dist_equal,p_max_dist_greater,
                    p_max_dist_pc_less,p_max_dist_pc_equal,p_max_dist_pc_greater,
                    p_phylo_less, p_phylo_equal, p_phylo_greater,
                    p_vol_less,p_vol_equal,p_vol_greater,
                    p_vol_pc_less,p_vol_pc_equal,p_vol_pc_greater)
    pval_per_cell_df = rbind(pval_per_cell_df,new_row)
  }
  return(pval_per_cell_df)
}

pseudo_zscore_fun = function(df_output_fin){
  pval_per_cell_df = data.frame()
  ## z_scores <- (data-mean(data))/sd(data)
  for(cellnum in sort(unique(df_output_fin$cellnum))){
    ## just do it with one value for now -- max_dist? 
    small_output = df_output_fin[df_output_fin$cellnum==cellnum,]
    true_value_shannon = ras_overall_shannon[cellnum]
    true_value_nspp = ras_num_uniq_spp[cellnum]
    true_value_max_dist = ras_dist[cellnum]
    true_value_max_dist_pc = ras_dist_pca[cellnum]
    true_value_phylo = ras_phylo[cellnum]
    true_value_vol = ras_hyper_vol[cellnum]
    true_value_vol_pc = ras_hyper_vol_pca[cellnum]
    
    z_shannon = (true_value_shannon - mean(small_output$overall_shannon_c,na.rm=T))/sd(small_output$overall_shannon_c,na.rm=T)
    z_nspp = (true_value_nspp - mean(small_output$num_uniq_spp_c,na.rm=T))/sd(small_output$num_uniq_spp_c,na.rm=T)
    z_max_dist = (true_value_max_dist - mean(small_output$max_dist,na.rm=T))/sd(small_output$max_dist,na.rm=T)
    z_max_dist_pc = (true_value_max_dist_pc - mean(small_output$max_dist_pc,na.rm=T))/sd(small_output$max_dist_pc,na.rm=T)
    z_phylo = (true_value_phylo - mean(small_output$max_phylo_dist_c,na.rm=T))/sd(small_output$max_phylo_dist_c,na.rm=T)
    z_vol = (true_value_vol - mean(small_output$volume_c,na.rm=T))/sd(small_output$volume_c,na.rm=T)
    z_vol_pc = (true_value_vol_pc - mean(small_output$volume_pc_c,na.rm=T))/sd(small_output$volume_pc_c,na.rm=T)
    
    new_row = cbind(cellnum,
                    z_shannon,
                    z_nspp,
                    z_max_dist,
                    z_max_dist_pc,
                    z_phylo,
                    z_vol,
                    z_vol_pc)
    pval_per_cell_df = rbind(pval_per_cell_df,new_row)
  }
  return(pval_per_cell_df)
}

pval_per_cell_df = pseudo_pval_fun(df_output_fin)
z_per_cell_df = pseudo_zscore_fun(df_output_fin)

make_raster_stacks_fun = function(pval_per_cell_df,lesscol,equalcol,greatercol){
  ras_LESS = ras; values(ras_LESS)[pval_per_cell_df$cellnum] = pval_per_cell_df[,lesscol]
  ras_EQUAL = ras; values(ras_EQUAL)[pval_per_cell_df$cellnum] = pval_per_cell_df[,equalcol]
  ras_GREATER = ras; values(ras_GREATER)[pval_per_cell_df$cellnum] = pval_per_cell_df[,greatercol]
  ras_COMBO = stack(c(ras_LESS,ras_EQUAL,ras_GREATER))
  return(ras_COMBO)
}

ras_COMBO_shan = make_raster_stacks_fun(pval_per_cell_df,"p_shannon_less","p_shannon_equal","p_shannon_greater")
ras_COMBO_nspp = make_raster_stacks_fun(pval_per_cell_df,"p_nspp_less","p_nspp_equal","p_nspp_greater")
ras_COMBO_dist = make_raster_stacks_fun(pval_per_cell_df,"p_max_dist_less","p_max_dist_equal","p_max_dist_greater")
ras_COMBO_distpc = make_raster_stacks_fun(pval_per_cell_df,"p_max_dist_pc_less","p_max_dist_pc_equal","p_max_dist_pc_greater")
ras_COMBO_phyl = make_raster_stacks_fun(pval_per_cell_df,"p_phylo_less","p_phylo_equal","p_phylo_greater")
ras_COMBO_volu = make_raster_stacks_fun(pval_per_cell_df,"p_vol_less","p_vol_equal","p_vol_greater")
ras_COMBO_volupc = make_raster_stacks_fun(pval_per_cell_df,"p_vol_pc_less","p_vol_pc_equal","p_vol_pc_greater")

par(mfrow=c(2,4))
plotRGB(ras_COMBO_shan,r=1,g=3,b=2,scale=1,colNA="grey")
plotRGB(ras_COMBO_nspp,r=1,g=3,b=2,scale=1,colNA="grey")
plotRGB(ras_COMBO_dist,r=1,g=3,b=2,scale=1,colNA="grey")
plotRGB(ras_COMBO_distpc,r=1,g=3,b=2,scale=1,colNA="grey")
plotRGB(ras_COMBO_phyl,r=1,g=3,b=2,scale=1,colNA="grey")
plotRGB(ras_COMBO_volu,r=1,g=3,b=2,scale=1,colNA="grey")
plotRGB(ras_COMBO_volupc,r=1,g=3,b=2,scale=1,colNA="grey")

write.table(pval_per_cell_df,"~/pval_for_each_metric_table.txt")

png("~/pvalue_histogram_song.png")
par(mfrow=c(2,4))
hist(pval_per_cell_df$p_shannon,main="shannon",breaks=seq(0,1,0.05))
hist(pval_per_cell_df$p_nspp,main="nspp",breaks=seq(0,1,0.05))
hist(pval_per_cell_df$p_max_dist,main="max dist",breaks=seq(0,1,0.05))
hist(pval_per_cell_df$p_max_dist_pc,main="max dist pca",breaks=seq(0,1,0.05))
hist(pval_per_cell_df$p_phylo,main="phylo dist",breaks=seq(0,1,0.05))
hist(pval_per_cell_df$p_vol,main="hypervol",breaks=seq(0,1,0.05))
hist(pval_per_cell_df$p_vol_pc,main="hypervol pca",breaks=seq(0,1,0.05))
dev.off()

ras_p_shan = ras; values(ras_p_shan)[pval_per_cell_df$cellnum] = pval_per_cell_df$p_shan
ras_p_nspp = ras; values(ras_p_nspp)[pval_per_cell_df$cellnum] = pval_per_cell_df$p_nspp
ras_p_max_dist = ras; values(ras_p_max_dist)[pval_per_cell_df$cellnum] = pval_per_cell_df$p_max_dist
ras_p_max_dist_pc = ras; values(ras_p_max_dist_pc)[pval_per_cell_df$cellnum] = pval_per_cell_df$p_max_dist_pc
ras_p_phylo = ras; values(ras_p_phylo)[pval_per_cell_df$cellnum] = pval_per_cell_df$p_phylo
ras_p_vol = ras; values(ras_p_vol)[pval_per_cell_df$cellnum] = pval_per_cell_df$p_vol
ras_p_vol_pc = ras; values(ras_p_vol_pc)[pval_per_cell_df$cellnum] = pval_per_cell_df$p_vol_pc

png("~/pvalue_spatial_song.png")
par(mfrow=c(2,4))
plot(ras_p_shan,colNA="grey",main="p shannon",col=viridis::plasma(3),breaks=c(0,0.05,0.95,1))
plot(ras_p_nspp,colNA="grey",main="p nspp",col=viridis::plasma(3),breaks=c(0,0.05,0.95,1))
plot(ras_p_max_dist,colNA="grey",main="p max dist",col=viridis::plasma(3),breaks=c(0,0.05,0.95,1))
plot(ras_p_max_dist_pc,colNA="grey",main="p max dist pc",col=viridis::plasma(3),breaks=c(0,0.05,0.95,1))
plot(ras_p_phylo,colNA="grey",main="p phylo",col=viridis::plasma(3),breaks=c(0,0.05,0.95,1))
plot(ras_p_vol,colNA="grey",main="p hypervol",col=viridis::plasma(3),breaks=c(0,0.05,0.95,1))
plot(ras_p_vol_pc,colNA="grey",main="p hypervol pc",col=viridis::plasma(3),breaks=c(0,0.05,0.95,1))
dev.off()

ras_stack_p = stack(c(ras_p_shan,ras_p_nspp,
                      ras_p_max_dist,ras_p_max_dist_pc,ras_p_phylo,
                      ras_p_vol,ras_p_vol_pc))
names(ras_stack_p) = c("P_Shannon",
                     "P_NUniqSpecies",
                     "P_MaxDistance",
                     "P_MaxDistancePCA",
                     "P_PhyloDist",
                     "P_Hypervolume",
                     "P_HypervolumePCA")
writeRaster(ras_stack_p,"~/raster_community_pval_song_stack.tiff",format="GTiff",overwrite=T)

big_stack = stack(c(
  ras,ras_overall_shannon,ras_num_uniq_spp,
  ras_hyper_vol,ras_hyper_vol_pca,ras_dist,
  ras_dist_pca,ras_phylo,
  ras_p_shan,ras_p_nspp,
  ras_p_max_dist,ras_p_max_dist_pc,ras_p_phylo,
  ras_p_vol,ras_p_vol_pc
))
names(big_stack) = 
  c("Sampling",   "Shannon",   "NUniqSpecies",  "Hypervolume",  "HypervolumePCA", "MaxDistance",  "MaxDistancePCA",
    "PhyloDist","P_Shannon", "P_NUniqSpecies",
    "P_MaxDistance","P_MaxDistancePCA",
    "P_PhyloDist","P_Hypervolume","P_HypervolumePCA")
layerStats(big_stack, 'pearson', na.rm=T)$`pearson correlation coefficient`

stack_matrix = as.matrix(big_stack)
correlation=cor(stack_matrix,use="pairwise.complete.obs")
plot(stack_matrix[,"Sampling"],stack_matrix[,"P_MaxDistance"])
corrplot::corrplot(correlation,method="ellipse",diag=F)


## randomize the lat longs
df_list_latlong = lapply(1:1000,FUN=function(x){permute_fun(df,c("LATITUDE","LONGITUDE"))})

## do it again 
df_output_latlong = lapply(1:length(df_list_latlong),FUN=function(i){
  print(i)
  df_i = df_list_latlong[[i]]
  
  newdf = data.frame()
  
  for(cellnum in sort(unique(df_i$cells))){
    #print(paste(i,cellnum))
    df_c = unique(df_i[df_i$cells==cellnum,])
    df_c = df_c[complete.cases(df_c$ID),]
    spptab_c = table(df_c$scientific)
    overall_shannon_c = vegan::diversity(spptab_c,index="shannon")
    uniq_spp_c = unique(df_c$scientific)
    num_uniq_spp_c = length(uniq_spp_c)
    
    df_c_small = df_c[,c("BANDWIDTH","CENTER","INFLECTION","SLOPE","TIME","PC1","PC2","PC3","PC4","PC5")]
    df_c_small = df_c_small[complete.cases(df_c_small),]
    
    overall_dist = dist(df_c_small[,c("BANDWIDTH","CENTER","INFLECTION","SLOPE","TIME")],diag = F,upper=F)
    overall_dist_pc = dist(df_c_small[,c("PC1","PC2","PC3","PC4","PC5")],diag = F,upper=F)
    ## these don't correlate? 
    max_dist = max(overall_dist,na.rm=F)
    max_dist_pc = max(overall_dist_pc,na.rm=F)
    if(is.infinite(max_dist)){max_dist=NA}
    if(is.infinite(max_dist_pc)){max_dist_pc=NA}
    
    max_phylo_dist_c=NA
    try({
      phylo_dist_c = phylo_dist[rownames(phylo_dist) %in% uniq_spp_c,colnames(phylo_dist) %in% uniq_spp_c]
      max_phylo_dist_c  = max(phylo_dist_c,na.rm=T)
      if(is.infinite(max_phylo_dist_c)){max_phylo_dist_c=NA}
    })
    
    volume_c=NA
    volume_pc_c=NA
    if(nrow(df_c_small)>=6){
      hypervolume_c=geometry::convhulln(df_c_small[,c("BANDWIDTH","CENTER","INFLECTION","SLOPE","TIME")],output.options="FA")
      volume_c = hypervolume_c$vol
      
      hypervolume_pc_c=geometry::convhulln(df_c_small[,c("PC1","PC2","PC3","PC4","PC5")],output.options="FA")
      volume_pc_c = hypervolume_pc_c$vol
    }
    
    new_row = cbind(i,cellnum,overall_shannon_c,num_uniq_spp_c,max_dist,
                    max_dist_pc,max_phylo_dist_c,volume_c,volume_pc_c)
    newdf = rbind(newdf,new_row)
  }
  
  return(newdf)
})

df_latlong_fin = do.call(rbind,df_output_latlong)
df_latlong_fin = unique(df_latlong_fin)
write.table(df_latlong_fin,"~/permutations_for_song_across_space_latlong.txt",append=T)

pval_per_cell_latlong = pseudo_pval_fun(df_latlong_fin)
z_per_cell_latlong = pseudo_zscore_fun(df_latlong_fin)

write.table(pval_per_cell_latlong,"~/pval_for_each_metric_table_latlong.txt")


ras_COMBO_shan = make_raster_stacks_fun(pval_per_cell_latlong,"p_shannon_less","p_shannon_equal","p_shannon_greater")
ras_COMBO_nspp = make_raster_stacks_fun(pval_per_cell_latlong,"p_nspp_less","p_nspp_equal","p_nspp_greater")
ras_COMBO_dist = make_raster_stacks_fun(pval_per_cell_latlong,"p_max_dist_less","p_max_dist_equal","p_max_dist_greater")
ras_COMBO_distpc = make_raster_stacks_fun(pval_per_cell_latlong,"p_max_dist_pc_less","p_max_dist_pc_equal","p_max_dist_pc_greater")
ras_COMBO_phyl = make_raster_stacks_fun(pval_per_cell_latlong,"p_phylo_less","p_phylo_equal","p_phylo_greater")
ras_COMBO_volu = make_raster_stacks_fun(pval_per_cell_latlong,"p_vol_less","p_vol_equal","p_vol_greater")
ras_COMBO_volupc = make_raster_stacks_fun(pval_per_cell_latlong,"p_vol_pc_less","p_vol_pc_equal","p_vol_pc_greater")

par(mfrow=c(2,4))
plotRGB(ras_COMBO_shan,r=1,g=3,b=2,scale=1,colNA="grey")
plotRGB(ras_COMBO_nspp,r=1,g=3,b=2,scale=1,colNA="grey")
plotRGB(ras_COMBO_dist,r=1,g=3,b=2,scale=1,colNA="grey")
plotRGB(ras_COMBO_distpc,r=1,g=3,b=2,scale=1,colNA="grey")
plotRGB(ras_COMBO_phyl,r=1,g=3,b=2,scale=1,colNA="grey")
plotRGB(ras_COMBO_volu,r=1,g=3,b=2,scale=1,colNA="grey")
plotRGB(ras_COMBO_volupc,r=1,g=3,b=2,scale=1,colNA="grey")



png("~/pvalue_histogram_song_LATLONG.png")
par(mfrow=c(2,4))
hist(pval_per_cell_latlong$p_shannon,main="shannon",breaks=seq(0,1,0.05))
hist(pval_per_cell_latlong$p_nspp,main="nspp",breaks=seq(0,1,0.05))
hist(pval_per_cell_latlong$p_max_dist,main="max dist",breaks=seq(0,1,0.05))
hist(pval_per_cell_latlong$p_max_dist_pc,main="max dist pca",breaks=seq(0,1,0.05))
hist(pval_per_cell_latlong$p_phylo,main="phylo dist",breaks=seq(0,1,0.05))
hist(pval_per_cell_latlong$p_vol,main="hypervol",breaks=seq(0,1,0.05))
hist(pval_per_cell_latlong$p_vol_pc,main="hypervol pca",breaks=seq(0,1,0.05))
dev.off()

ras_p_shan_ll = ras; values(ras_p_shan_ll)[pval_per_cell_latlong$cellnum] = pval_per_cell_latlong$p_shan
ras_p_nspp_ll = ras; values(ras_p_nspp_ll)[pval_per_cell_latlong$cellnum] = pval_per_cell_latlong$p_nspp
ras_p_max_dist_ll = ras; values(ras_p_max_dist_ll)[pval_per_cell_latlong$cellnum] = pval_per_cell_latlong$p_max_dist
ras_p_max_dist_pc_ll = ras; values(ras_p_max_dist_pc_ll)[pval_per_cell_latlong$cellnum] = pval_per_cell_latlong$p_max_dist_pc
ras_p_phylo_ll = ras; values(ras_p_phylo_ll)[pval_per_cell_latlong$cellnum] = pval_per_cell_latlong$p_phylo
ras_p_vol_ll = ras; values(ras_p_vol_ll)[pval_per_cell_latlong$cellnum] = pval_per_cell_latlong$p_vol
ras_p_vol_pc_ll = ras; values(ras_p_vol_pc_ll)[pval_per_cell_latlong$cellnum] = pval_per_cell_latlong$p_vol_pc

png("~/pvalue_spatial_song_LATLONG.png")
par(mfrow=c(2,4))
plot(ras_p_shan_ll,colNA="grey",main="p shannon",col=viridis::plasma(3),breaks=c(0,0.05,0.95,1))
plot(ras_p_nspp_ll,colNA="grey",main="p nspp",col=viridis::plasma(3),breaks=c(0,0.05,0.95,1))
plot(ras_p_max_dist_ll,colNA="grey",main="p max dist",col=viridis::plasma(3),breaks=c(0,0.05,0.95,1))
plot(ras_p_max_dist_pc_ll,colNA="grey",main="p max dist pc",col=viridis::plasma(3),breaks=c(0,0.05,0.95,1))
plot(ras_p_phylo_ll,colNA="grey",main="p phylo",col=viridis::plasma(3),breaks=c(0,0.05,0.95,1))
plot(ras_p_vol_ll,colNA="grey",main="p hypervol",col=viridis::plasma(3),breaks=c(0,0.05,0.95,1))
plot(ras_p_vol_pc_ll,colNA="grey",main="p hypervol pc",col=viridis::plasma(3),breaks=c(0,0.05,0.95,1))
dev.off()

ras_stack_p_ll = stack(c(ras_p_shan_ll,ras_p_nspp_ll,
                         ras_p_max_dist_ll,ras_p_max_dist_pc_ll,ras_p_phylo_ll,
                         ras_p_vol_ll,ras_p_vol_pc_ll))
names(ras_stack_p_ll) = c("P_LL_Shannon",
                       "P_LL_NUniqSpecies",
                       "P_LL_MaxDistance",
                       "P_LL_MaxDistancePCA",
                       "P_LL_PhyloDist",
                       "P_LL_Hypervolume",
                       "P_LL_HypervolumePCA")
writeRaster(ras_stack_p,"~/raster_community_pval_song_stack_LATLONG.tiff",format="GTiff",overwrite=T)


## now that I have thought about this...actually it would make more sense to permute WITHIN species, so change the lat longs within species instead. 

permute_within_group_fun = function(df,colnames,groupcol) {
  df_temp = df
  groups = sort(unique(df_temp[,groupcol]))
  for(group in groups){
    #print(group)
    groupnums = which(df_temp[,groupcol] == group)
    sample_order = sample(groupnums)
    if(length(groupnums)>1){
    df_temp[groupnums,colnames] = df_temp[sample_order,colnames]
    }
  }
  return(df_temp)
}

df_list_group = lapply(1:1000,FUN=function(x){permute_within_group_fun(df,c("LATITUDE","LONGITUDE"),"scientific")})

## do it again 
df_output_group = lapply(1:length(df_list_group),FUN=function(i){
  print(i)
  df_i = df_list_group[[i]]
  
  newdf = data.frame()
  
  for(cellnum in sort(unique(df_i$cells))){
    #print(paste(i,cellnum))
    df_c = unique(df_i[df_i$cells==cellnum,])
    df_c = df_c[complete.cases(df_c$ID),]
    spptab_c = table(df_c$scientific)
    overall_shannon_c = vegan::diversity(spptab_c,index="shannon")
    uniq_spp_c = unique(df_c$scientific)
    num_uniq_spp_c = length(uniq_spp_c)
    
    df_c_small = df_c[,c("BANDWIDTH","CENTER","INFLECTION","SLOPE","TIME","PC1","PC2","PC3","PC4","PC5")]
    df_c_small = df_c_small[complete.cases(df_c_small),]
    
    overall_dist = dist(df_c_small[,c("BANDWIDTH","CENTER","INFLECTION","SLOPE","TIME")],diag = F,upper=F)
    overall_dist_pc = dist(df_c_small[,c("PC1","PC2","PC3","PC4","PC5")],diag = F,upper=F)
    ## these don't correlate? 
    max_dist = max(overall_dist,na.rm=F)
    max_dist_pc = max(overall_dist_pc,na.rm=F)
    if(is.infinite(max_dist)){max_dist=NA}
    if(is.infinite(max_dist_pc)){max_dist_pc=NA}
    
    max_phylo_dist_c=NA
    try({
      phylo_dist_c = phylo_dist[rownames(phylo_dist) %in% uniq_spp_c,colnames(phylo_dist) %in% uniq_spp_c]
      max_phylo_dist_c  = max(phylo_dist_c,na.rm=T)
      if(is.infinite(max_phylo_dist_c)){max_phylo_dist_c=NA}
    })
    
    volume_c=NA
    volume_pc_c=NA
    if(nrow(df_c_small)>=6){
      hypervolume_c=geometry::convhulln(df_c_small[,c("BANDWIDTH","CENTER","INFLECTION","SLOPE","TIME")],output.options="FA")
      volume_c = hypervolume_c$vol
      
      hypervolume_pc_c=geometry::convhulln(df_c_small[,c("PC1","PC2","PC3","PC4","PC5")],output.options="FA")
      volume_pc_c = hypervolume_pc_c$vol
    }
    
    new_row = cbind(i,cellnum,overall_shannon_c,num_uniq_spp_c,max_dist,
                    max_dist_pc,max_phylo_dist_c,volume_c,volume_pc_c)
    newdf = rbind(newdf,new_row)
  }
  
  return(newdf)
})

df_group_fin = do.call(rbind,df_output_group)
df_group_fin = unique(df_group_fin)
write.table(df_group_fin,"~/permutations_for_song_across_space_WITHINSPP.txt",append=T)

pval_per_cell_group = pseudo_pval_fun(df_group_fin)
z_per_cell_group = pseudo_zscore_fun(df_group_fin)

write.table(pval_per_cell_group,"~/pval_for_each_metric_table_WITHINSPP.txt")



ras_COMBO_shan = make_raster_stacks_fun(pval_per_cell_group,"p_shannon_less","p_shannon_equal","p_shannon_greater")
ras_COMBO_nspp = make_raster_stacks_fun(pval_per_cell_group,"p_nspp_less","p_nspp_equal","p_nspp_greater")
ras_COMBO_dist = make_raster_stacks_fun(pval_per_cell_group,"p_max_dist_less","p_max_dist_equal","p_max_dist_greater")
ras_COMBO_distpc = make_raster_stacks_fun(pval_per_cell_group,"p_max_dist_pc_less","p_max_dist_pc_equal","p_max_dist_pc_greater")
ras_COMBO_phyl = make_raster_stacks_fun(pval_per_cell_group,"p_phylo_less","p_phylo_equal","p_phylo_greater")
ras_COMBO_volu = make_raster_stacks_fun(pval_per_cell_group,"p_vol_less","p_vol_equal","p_vol_greater")
ras_COMBO_volupc = make_raster_stacks_fun(pval_per_cell_group,"p_vol_pc_less","p_vol_pc_equal","p_vol_pc_greater")

par(mfrow=c(2,4))
plotRGB(ras_COMBO_shan,r=1,g=3,b=2,scale=1,colNA="grey")
plotRGB(ras_COMBO_nspp,r=1,g=3,b=2,scale=1,colNA="grey")
plotRGB(ras_COMBO_dist,r=1,g=3,b=2,scale=1,colNA="grey")
plotRGB(ras_COMBO_distpc,r=1,g=3,b=2,scale=1,colNA="grey")
plotRGB(ras_COMBO_phyl,r=1,g=3,b=2,scale=1,colNA="grey")
plotRGB(ras_COMBO_volu,r=1,g=3,b=2,scale=1,colNA="grey")
plotRGB(ras_COMBO_volupc,r=1,g=3,b=2,scale=1,colNA="grey")


png("~/pvalue_histogram_song_WITHINSPP.png")
par(mfrow=c(2,4))
hist(pval_per_cell_group$p_shannon,main="shannon",breaks=seq(0,1,0.05))
hist(pval_per_cell_group$p_nspp,main="nspp",breaks=seq(0,1,0.05))
hist(pval_per_cell_group$p_max_dist,main="max dist",breaks=seq(0,1,0.05))
hist(pval_per_cell_group$p_max_dist_pc,main="max dist pca",breaks=seq(0,1,0.05))
hist(pval_per_cell_group$p_phylo,main="phylo dist",breaks=seq(0,1,0.05))
hist(pval_per_cell_group$p_vol,main="hypervol",breaks=seq(0,1,0.05))
hist(pval_per_cell_group$p_vol_pc,main="hypervol pca",breaks=seq(0,1,0.05))
dev.off()

ras_p_shan_group = ras; values(ras_p_shan_group)[pval_per_cell_group$cellnum] = pval_per_cell_group$p_shan
ras_p_nspp_group = ras; values(ras_p_nspp_group)[pval_per_cell_group$cellnum] = pval_per_cell_group$p_nspp
ras_p_max_dist_group = ras; values(ras_p_max_dist_group)[pval_per_cell_group$cellnum] = pval_per_cell_group$p_max_dist
ras_p_max_dist_pc_group = ras; values(ras_p_max_dist_pc_group)[pval_per_cell_group$cellnum] = pval_per_cell_group$p_max_dist_pc
ras_p_phylo_group = ras; values(ras_p_phylo_group)[pval_per_cell_group$cellnum] = pval_per_cell_group$p_phylo
ras_p_vol_group = ras; values(ras_p_vol_group)[pval_per_cell_group$cellnum] = pval_per_cell_group$p_vol
ras_p_vol_pc_group = ras; values(ras_p_vol_pc_group)[pval_per_cell_group$cellnum] = pval_per_cell_group$p_vol_pc

png("~/pvalue_spatial_song_WITHINSPP.png")
par(mfrow=c(2,4))
plot(ras_p_shan_group,colNA="grey",main="p shannon",col=viridis::plasma(3),breaks=c(0,0.05,0.95,1))
plot(ras_p_nspp_group,colNA="grey",main="p nspp",col=viridis::plasma(3),breaks=c(0,0.05,0.95,1))
plot(ras_p_max_dist_group,colNA="grey",main="p max dist",col=viridis::plasma(3),breaks=c(0,0.05,0.95,1))
plot(ras_p_max_dist_pc_group,colNA="grey",main="p max dist pc",col=viridis::plasma(3),breaks=c(0,0.05,0.95,1))
plot(ras_p_phylo_group,colNA="grey",main="p phylo",col=viridis::plasma(3),breaks=c(0,0.05,0.95,1))
plot(ras_p_vol_group,colNA="grey",main="p hypervol",col=viridis::plasma(3),breaks=c(0,0.05,0.95,1))
plot(ras_p_vol_pc_group,colNA="grey",main="p hypervol pc",col=viridis::plasma(3),breaks=c(0,0.05,0.95,1))
dev.off()

ras_stack_p_group = stack(c(ras_p_shan_group,ras_p_nspp_group,
                         ras_p_max_dist_group,ras_p_max_dist_pc_group,ras_p_phylo_group,
                         ras_p_vol_group,ras_p_vol_pc_group))
names(ras_stack_p_group) = c("P_group_Shannon",
                          "P_group_NUniqSpecies",
                          "P_group_MaxDistance",
                          "P_group_MaxDistancePCA",
                          "P_group_PhyloDist",
                          "P_group_Hypervolume",
                          "P_group_HypervolumePCA")
writeRaster(ras_stack_p,"~/raster_community_pval_song_stack_WITHINSPP.tiff",format="GTiff",overwrite=T)
