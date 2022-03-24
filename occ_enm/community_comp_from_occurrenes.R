## community composition -- test with ebird data
library(raster)

## ebird ap- = "f49839r87f7g"

recalcData  = T
smallFiles=F
data_folder = "/Users/kprovost/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Occurences/Dissertation/EBIRD/by_species/"

ras = raster("/Users/kprovost/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Environment/WorldClim2.1_jan2020/blank_worldclim_agg40.asc")
ras = raster::crop(ras,extent(-75,-70,40,50))
ras_big = raster("/Users/kprovost/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Environment/WorldClim2.1_jan2020/blank_worldclim.asc")
ras_big = raster::crop(ras_big,extent(-75,-70,40,50))

## Jaccard similarity 
jaccard <- function(a, b) {
  intersection = length(intersect(a, b))
  union = length(a) + length(b) - intersection
  return (intersection/union)
}
sorensen = function(a,b) {
  z = length(intersect(a, b))
  y = length(intersect(a, a))
  x = length(intersect(b, b))
  
  SS = (2*z)/(2*z + y + x)
  ##z = number of species common to both quadrats,
  ##y = number of species unique to the first quadrat, and
  ##x = number of species unique to the second quadrat
  return(SS)
}

if(recalcData==T){
  if(smallFiles==T){
    data_files = list.files(path=data_folder,pattern="txt$",full.names = T,recursive = F)
    df_list = lapply(data_files,FUN=function(x){
      df=read.table(x,header=T,sep=" ")
      return(df)
    })
    df_full = do.call(rbind,df_list)
  } else  {
    print("BIG DATA")
    df_full = read.table("/Users/kprovost/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Occurences/Dissertation/EBIRD/full_data/ebd_trim_spplatlong_unique.txt",
                         header=T,sep="\t")
  }
  bad_names =  sort(unique(c(unique(df_full$name)[grepl("/",unique(df_full$name))],
                             unique(df_full$name)[grepl("sp\\.",unique(df_full$name))],
                             unique(df_full$name)[grepl("\\(",unique(df_full$name))],
                             unique(df_full$name)[grepl(" x ",unique(df_full$name))])))
  df_full = df_full[!(df_full$name %in% bad_names),]
  df_full = unique(df_full)
  
  ## now convert this to a raster file, need to import a raster
  
  cells=raster::cellFromXY(ras,df_full[,c("longitude","latitude")])
  df_full$cells = cells
  df_cells = df_full[,c("name","cells")]
  df_cells = unique(df_cells)
  cell_count = table(df_cells$cells)
  cell_bool = cell_count
  cell_bool[cell_bool>0]=1
  df_table=t(table(df_cells))
  write.table(df_table,"~/presence_table_test_code_small.txt")
  
  ## now we need to generate the communities that are there
  
  df_sim = as.data.frame(matrix(nrow=nrow(df_table),ncol=nrow(df_table)))
  colnames(df_sim) = as.character(rownames(df_table))
  rownames(df_sim) = colnames(df_sim)
  df_sor = df_sim
  for(i in 1:nrow(df_table)){
    for(j in i:nrow(df_table)){
      print(paste(i,j))
      row_a = df_table[i,]
      spp_a = names(row_a[row_a>0])
      row_b = df_table[j,]
      spp_b = names(row_b[row_b>0])
      
      jac = jaccard(spp_a,spp_b)
      df_sim[i,j] = jac
      df_sim[j,i] = jac
      
      sor = sorensen(spp_a,spp_b)
      df_sor[i,j] = sor
      df_sor[j,i] = sor
    }
  }
  write.table(df_sim,"~/jacard_similarity_test_code_small.txt")
  write.table(df_sor,"~/sorensen_similarity_test_code_small.txt")
}
df_sim = read.table("~/jacard_similarity_test_code_small.txt",check.names = F)
df_sor = read.table("~/sorensen_similarity_test_code_small.txt",check.names = F)
corrplot::corrplot(as.matrix(df_sim),method="color")
corrplot::corrplot(as.matrix(df_sor),method="color")
njtree=ape::nj(as.dist(1-df_sim))
plot(njtree,"unrooted")

njtree2=ape::nj(as.dist(0.5-df_sor))
plot(njtree2,"unrooted")
#njtree2_root=ape::root(njtree2,outgroup="7")
#plot(njtree2_root)

ras_cells = ras
values(ras_cells)[as.numeric(names(cell_bool))]=cell_bool
plot(ras_cells)

## also might want to do sorensons distance
##SS = 2a/(2a + b + c), where
##Sørensen similarity coefficient,
##a = number of species common to both quadrats,
##b = number of species unique to the first quadrat, and
##c = number of species unique to the second quadrat

## jacard ranges 0:1, sor ranges 0:0.5

## Jaccard calculates the unique (unshared) species as a proportion of the total 
## number of species recorded in the two communities, while Sørensen gives 
## double weight to the shared species (Table 3). The Sørensen dissimilarity 
## is thus closely related to Jaccard, and always has a lower value than Jaccard.

## how do you evaluate if two communities are significant differnt? 
## can you assume adjacent communities are different? 
## multi dimensional scaling? 

## anosim from vegan
nmds=vegan::metaMDS(df_sim, distance = "jaccard")
plot(nmds)
#extract NMDS scores (x and y coordinates)
data.scores = as.data.frame(vegan::scores(nmds))
data.scores$cell = colnames(df_sim)
plot(data.scores[,1:2],pch=as.numeric(data.scores$cell)-7)

vegan::anosim(df_sim,grouping=colnames(df_sim))

##  i think a  permutation test would work
## if assume two localities have the same total species, 
## then subset  the n species each locality is shown to have with sample,
## then calculate jacards and see 

df_table = read.table("~/presence_table_test_code_small.txt")

perm_num = 100
perm_df = as.data.frame(matrix(nrow=nrow(df_table),ncol=nrow(df_table)))
for(row_i in 1:nrow(df_table)){
  comm_a = df_table[row_i,]
  present_a = which(comm_a>0)
  spp_a = colnames(comm_a)[present_a]
  num_a = length(spp_a)
  for(row_j in row_i:nrow(df_table)) {
    comm_b = df_table[row_j,]
    present_b = which(comm_b>0)
    spp_b = colnames(comm_b)[present_b]
    num_b = length(spp_b)
    comm_ab = sort(unique(c(spp_a,spp_b)))
    true_jac = 1-(jaccard(spp_a,spp_b))
    perm_jac = c()
    cat(row_i,row_j,"\n")
    if(row_i==row_j){
      sim_p_val=1
    } else {
      for(perm in 1:perm_num){
        test_spp_a = sample(comm_ab,num_a,replace=F)
        test_spp_b = sample(comm_ab,num_b,replace=F)
        test_jac = 1-(jaccard(test_spp_a,test_spp_b))
        perm_jac = append(perm_jac,test_jac)
      }
      ## calculate how many the true_jac is greater than 
      sim_p_val=sum(true_jac>perm_jac)/perm_num
    }
    perm_df[row_i,row_j] = sim_p_val
    perm_df[row_j,row_i] = sim_p_val
    
  }
}
colnames(perm_df) = rownames(df_table)
rownames(perm_df) = colnames(perm_df)
write.table(perm_df,"~/permutations_data_frame_jaccard_small.txt")
alpha_sig = 0.05
sig_df=perm_df<alpha_sig
sigtree=ape::nj(as.dist(sig_df))
plot(sigtree,"unrooted")

corrplot::corrplot(as.matrix(sig_df),method="color",order="hclust")
cluster=hclust(as.dist(sig_df), method = "complete", members = NULL)
memb <- cutree(cluster, k = 3)
