library(raster)

## for i in /Users/kprovost/Documents/Postdoc_Working/MMRR/Environment/g*tif; do echo $i; Rscript /Users/kprovost/Documents/GitHub/bioacoustics/mmrr_environment.R $i; done;

extent_1 = extent(c(-180,-20,-90,90))
extent_2 = extent(c(160,180,-90,90))
resolution = c(0.1,0.1)

args = commandArgs(trailingOnly=TRUE)
print(length(args))

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

if(length(args)>=1){
  
  ## given a file 
  myfile = args[1]
  print(myfile)
  
  outfile = paste(myfile,"_CROPPED.WRAPPED.asc",sep="")
  #outfile = paste(myfile,"_CROPPED.WRAPPED.tif",sep="")
  if(!(file.exists(outfile))){
    
    #myfile = "/Users/kprovost/Documents/Postdoc_Working/MMRR/Environment//NASA_TOPOGRAPHY_SRTM_RAMP2_TOPO_2000-02-11_gs_3600x1800.asc"
    ras = raster(myfile)
    
    print("crop1")
    ras_A = crop(ras,extent_1)
    print("crop2")
    ras_B = crop(ras,extent_2)
    print("shift")
    ras_B = shift(ras_B,dx=-360)
    print("merge")
    ras_fix = merge(ras_A,ras_B)
    
    print("write")
    writeRaster(ras_fix,outfile,format="ascii")
    #writeRaster(ras_fix,outfile,format="GTiff")
    print("zip")
    R.utils::gzip(myfile)
  } else {
    (print("Not Overwriting"))
    print("zip")
    R.utils::gzip(myfile)
    aggfile = paste(outfile,"_AGGREGATED_0.1-0.1.asc",sep="")
    fact = resolution/res(ras_fix)
    print("aggregate")
    ras_agg = aggregate(ras_fix,fact=fact)
    print("write 2")
    writeRaster(ras_agg,outfile,format="ascii")
    #writeRaster(ras_agg,outfile,format="GTiff")
    print("zip 2")
    R.utils::gzip(outfile)
  }
  directory=dirname(myfile)
  ascii_files = list.files(path=directory,pattern="chunk",full.names = T)
  ascii_files = ascii_files[grepl("gb",ascii_files)]
  for(outfile in sort(ascii_files)){
    print(outfile)
    ras_fix = raster(outfile)
    aggfile = paste(outfile,"_AGGREGATED_0.1-0.1.asc",sep="")
    fact = resolution/res(ras_fix)
    if(length(unique(fact))!=1){
      print("aggregate")
      ras_agg = aggregate(ras_fix,fact=fact,FUN=function(x){getmode(x)})
    } else if ( unique(fact)!=1 ) {
      print("aggregate")
      ras_agg = aggregate(ras_fix,fact=fact,FUN=function(x){getmode(x)})
    } else {
      ras_agg = ras_fix
    }
    print("write 2")
    writeRaster(ras_agg,aggfile,format="ascii")
    #writeRaster(ras_agg,aggfile,format="GTiff")
    print("zip 2")
    R.utils::gzip(outfile)
  }
}



# hydroF = "/Users/kprovost/Downloads/hydrography_l_rivers_v2_wgs84binary.asc"
# hydro = raster(hydroF)
# ## NORTH AMERICAN ONLY
# 
# landcoverF = "/Users/kprovost/Downloads/NA_LandCover_2005V2_25haMMU_wgs84.asc"
# landcover = raster(landcoverF)
# ## NORTH AMERICAN ONLY
# 
# slopeF = "/Users/kprovost/Downloads/elevation_and_derivatives/NASA_slope.tif"
# slope = raster(slopeF)
# ## world
# 
# elevF = "/Users/kprovost/Documents/Postdoc_Working/MMRR/Environment//NASA_TOPOGRAPHY_SRTM_RAMP2_TOPO_2000-02-11_gs_3600x1800.asc"
# elev = raster(elevF)
# 
# elev_A = crop(elev,extent_1)
# elev_B = crop(elev,extent_2)
# elev_B = shift(elev_B,dx=-360)
# elev_fix = merge(elev_A,elev_B)
# 
# writeRaster(elev_fix,paste(elevF,"_CROPPED.WRAPPED.asc",sep=""),format="ascii")
# 
# ## -54.91 to 71.29
# ## -167 to -20
# 
# 
# 
# ## Otherwise it's reasonably straightforward to crop() to separate longitude ranges, shift() one of them in "x", and merge() them back together. 
# 
# 
# naturalF = "/Users/kprovost/Documents/Postdoc_Working/MMRR/Environment/Anthromes_dataverse_files/WCMC_natural_modified_habitat_screening_layer/natural_modified_habitat_screening_layer.tif"
# natural = raster(naturalF)
# 
# 
# ## land cover pca? crop? 
# lcF="/Users/kprovost/Documents/Postdoc_Working/MMRR/Environment/Copernicus_LandCover/PROBAV_LC100_global_v3.0.1_2015-base_Snow-CoverFraction-layer_EPSG-4326.tif"
# lc = raster(lcF)
# fact=resolution/res(lc)
# 
# lc_A = crop(lc,extent_1)
# lc_B = crop(lc,extent_2)
# lc_B = shift(lc_B,dx=-360)
# lc_fix = merge(lc_A,lc_B)
# 
# bsize<-blockSize(lc,minrows=1000)
# i=1
# lc_sm=lc[,bsize$row[i]:(bsize$row[i+1]-1),drop=F]
# lc_sm_agg =aggregate(lc_sm,fact)




## can we actually just extract the data from these rasters and then do all the gobledy gook
df=read.table("/Users/kprovost/Documents/Postdoc_Working/MMRR/Passerellidae_genetic_song_combined_occurrences_ENVS_3Nov2022_CLUSTER.txt",sep="\t",header=T)
df_ll = df[,c("LONGITUDE","LATITUDE")]




directory="/Users/kprovost/Documents/Postdoc_Working/MMRR/Environment"
envfiles = list.files(path=directory,
                      pattern=".tif$",full.names = T,recursive = T)
#envfiles = c(envfiles,list.files(path=directory,
#                      pattern=".asc.gz$",full.names = T,recursive = T))
for(env in envfiles){
  print(env)
  #env_s = gsub(".gz","",env)
  #print("gunzip")
  #R.utils::gunzip(env)
  print("load raster")
  #ras = raster(env_s)
  ras = raster(env)
  envb = basename(env)
  print("get cells")
  cells = cellFromXY(ras,df_ll)
  print("extract")
  envvalue = extract(ras,cells)
  print("add to df")
  df[,envb] = envvalue
  print("write")
  write.table(df,"/Users/kprovost/Documents/Postdoc_Working/MMRR/Passerellidae_genetic_song_combined_occurrences_ENVS_3Nov2022_CLUSTER.txt",
              sep="\t",quote=F,row.names = F)
  print("gzip")
  #R.utils::gzip(env_s)
  R.utils::gzip(env)
}

## for raster stacks
df=read.table("/Users/kprovost/Documents/Postdoc_Working/MMRR/Passerellidae_genetic_song_combined_occurrences_ENVS_stackstemp.txt",sep="\t",header=T)
df_ll = df[,c("LONGITUDE","LATITUDE")]
directory="/Users/kprovost/Documents/Postdoc_Working/MMRR/Environment/raw"
envfiles = list.files(path=directory,
                      pattern=".tif.gz$",full.names = T,recursive = T)
for(env in envfiles[2:3]){
  print(env)
  env_s = gsub(".gz","",env)
  print("gunzip")
  R.utils::gunzip(env)
  print("load raster")
  ras = stack(env_s)
  #ras = raster(env)
  envb = basename(env)
  print("get cells")
  cells = cellFromXY(ras,df_ll)
  print("extract")
  envvaluedf = extract(ras,cells)
  print("add to df")
  df = cbind(df,envvaluedf)
  print("write")
  write.table(df,"/Users/kprovost/Documents/Postdoc_Working/MMRR/Passerellidae_genetic_song_combined_occurrences_ENVS_stackstemp.txt",
              sep="\t",quote=F,row.names = F)
  print("gzip")
  R.utils::gzip(env_s)
  #R.utils::gzip(env)
}


## dont forget to add the worldclim data

df = read.table("/Users/kprovost/Documents/Postdoc_Working/MMRR/Passerellidae_genetic_song_combined_occurrences_ENVS_3Nov2022_CLUSTER.txt",
                header=T,sep = "\t")
wc <- getData("worldclim",var="bio",res=10)
df_ll = df[,c("LONGITUDE","LATITUDE")]
print("get cells")
cells = cellFromXY(wc,df_ll)
print("extract")
envvaluedf = extract(wc,cells)
print("add to df")
df = cbind(df,envvaluedf)
print("write")
write.table(df,"/Users/kprovost/Documents/Postdoc_Working/MMRR/Passerellidae_genetic_song_combined_occurrences_ENVS_3Nov2022_CLUSTER.txt",
            sep="\t",quote=F,row.names = F)
