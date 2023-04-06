rm(list = ls())
doCombine=F
library(raster)

if(doCombine==T){
  ## get species
  centroid_file = "/Users/kprovost/Documents/Postdoc_Working/MMRR/WAVS/Wave/0STATSDONE/centroid_locations_per_individual_08Mar2023.txt_PCA-DATA-METADATA.txt"
  df_pca = data.table::fread(centroid_file,header=T,data.table=F)
  spp_df = df_pca[,c("GENUS","SPECIES","SUBSPECIES")]
  spp_df = unique(spp_df)
  
  ## merge centroid file with previous env data file
  env_file = "/Users/kprovost/Documents/Postdoc_Working/MMRR/Passerellidae_genetic_song_combined_occurrences_ENVS_9Nov2022_CLUSTER_trimmed.txt"
  env = data.table::fread(env_file,header=T,data.table=F)
  #colnames(env)[colnames(env)=="FULLID"] = "Ind"
  
  df_pca_env = merge(df_pca,env,all=T)
  df_pca_env=unique(df_pca_env)
  write.table(df_pca_env,"/Users/kprovost/Documents/Postdoc_Working/MMRR/centroid_locations_08Mar2023_song_genetics_env.txt",quote=F,row.names = F,sep="\t")
  
} else {
  df_pca_env = read.table("/Users/kprovost/Documents/Postdoc_Working/MMRR/occurrences_metadata/centroid_locations_08Mar2023_song_genetics_env.txt",header=T,sep="\t")
}
df_pca_env = unique(df_pca_env)
latlong_only = df_pca_env[,c("LATITUDE","LONGITUDE")]
latlong_only = unique(latlong_only) 
minlat = min(df_pca_env$LATITUDE,na.rm=T)
maxlat = max(df_pca_env$LATITUDE,na.rm=T)
minlon = min(df_pca_env$LONGITUDE,na.rm=T)
maxlon = max(df_pca_env$LONGITUDE,na.rm=T)
## use adjusted longitude for geography

## get the env layers to extract from 

## extract urbanization data
##stacks from 1940 to 2017 by year (2017 as 2020)

if(!(file.exists("/Users/kprovost/Documents/Postdoc_Working/urbandata_latlong.txt"))) {
  if(!(file.exists("/Users/kprovost/Documents/Postdoc_Working/urbandata.txt"))) {
    
    if(!(file.exists("/Users/kprovost/Documents/Postdoc_Working/0EnvTemp/Anthromes_dataverse_files/Anthromes-12k-DGG/raw-data/HYDE/stack_1940-2017-decade.tif"))) {
      u40=stack("/Users/kprovost/Documents/Postdoc_Working/0EnvTemp/Anthromes_dataverse_files/Anthromes-12k-DGG/raw-data/HYDE/stack_1940AD.tif")
      u50=stack("/Users/kprovost/Documents/Postdoc_Working/0EnvTemp/Anthromes_dataverse_files/Anthromes-12k-DGG/raw-data/HYDE/stack_1950AD.tif")
      u60=stack("/Users/kprovost/Documents/Postdoc_Working/0EnvTemp/Anthromes_dataverse_files/Anthromes-12k-DGG/raw-data/HYDE/stack_1960AD.tif")
      u70=stack("/Users/kprovost/Documents/Postdoc_Working/0EnvTemp/Anthromes_dataverse_files/Anthromes-12k-DGG/raw-data/HYDE/stack_1970AD.tif")
      u80=stack("/Users/kprovost/Documents/Postdoc_Working/0EnvTemp/Anthromes_dataverse_files/Anthromes-12k-DGG/raw-data/HYDE/stack_1980AD.tif")
      u90=stack("/Users/kprovost/Documents/Postdoc_Working/0EnvTemp/Anthromes_dataverse_files/Anthromes-12k-DGG/raw-data/HYDE/stack_1990AD.tif")
      u00=stack("/Users/kprovost/Documents/Postdoc_Working/0EnvTemp/Anthromes_dataverse_files/Anthromes-12k-DGG/raw-data/HYDE/stack_2000AD.tif")
      u10=stack("/Users/kprovost/Documents/Postdoc_Working/0EnvTemp/Anthromes_dataverse_files/Anthromes-12k-DGG/raw-data/HYDE/stack_2010AD.tif")
      u17=stack("/Users/kprovost/Documents/Postdoc_Working/0EnvTemp/Anthromes_dataverse_files/Anthromes-12k-DGG/raw-data/HYDE/stack_2017AD.tif")
      
      layers = c("cropland","grazing","irr_ice","popc","tot_irri","uopp")
      names(u40) = paste(layers,".1940",sep="")
      names(u50) = paste(layers,".1950",sep="")
      names(u60) = paste(layers,".1960",sep="")
      names(u70) = paste(layers,".1970",sep="")
      names(u80) = paste(layers,".1980",sep="")
      names(u90) = paste(layers,".1990",sep="")
      names(u00) = paste(layers,".2000",sep="")
      names(u10) = paste(layers,".2010",sep="")
      names(u17) = paste(layers,".2017",sep="")
      
      urbstack = stack(u40,u50,u60,u70,
                       u80,u90,u00,u10,u17)
      
      writeRaster(urbstack,"/Users/kprovost/Documents/Postdoc_Working/0EnvTemp/Anthromes_dataverse_files/Anthromes-12k-DGG/raw-data/HYDE/stack_1940-2017-decade.tif",
                  format="GTiff")
    } else {
      urbstack = stack("/Users/kprovost/Documents/Postdoc_Working/0EnvTemp/Anthromes_dataverse_files/Anthromes-12k-DGG/raw-data/HYDE/stack_1940-2017-decade.tif")
    }
    
    ## crop urbstack to data
    urbcells=cellFromXY(urbstack,latlong_only[,c("LONGITUDE","LATITUDE")])
    latlong_only$urbcells = urbcells
    urbandata=extract(urbstack,unique(urbcells))
    urbandata = as.data.frame(urbandata)
    urbandata$urbcells = unique(urbcells)
    data.table::fwrite(urbandata,"/Users/kprovost/Documents/Postdoc_Working/urbandata.txt")
  } else {
    urbandata = data.table::fread("/Users/kprovost/Documents/Postdoc_Working/urbandata.txt",data.table=F) 
  }
  
  latlong_only = merge(latlong_only,urbandata,all=T)
  data.table::fwrite(latlong_only,"/Users/kprovost/Documents/Postdoc_Working/urbandata_latlong.txt")
} else {
  latlong_only = data.table::fread("/Users/kprovost/Documents/Postdoc_Working/urbandata_latlong.txt",data.table=F)
}


## do this with worldclim data

if(!(file.exists("/Users/kprovost/Documents/Postdoc_Working/wcdata_latlong.txt"))) {
  if(!(file.exists("/Users/kprovost/Documents/Postdoc_Working/wcdata.txt"))) {
    
    r <- getData("worldclim",var="bio",res=10)
    
    ## crop wc to data
    wccells=cellFromXY(r,latlong_only[,c("LONGITUDE","LATITUDE")])
    latlong_only$wccells = wccells
    wcdata=extract(r,unique(wccells))
    wcdata = as.data.frame(wcdata)
    wcdata$wccells = unique(wccells)
    data.table::fwrite(wcdata,"/Users/kprovost/Documents/Postdoc_Working/wcdata.txt")
  } else {
    wcdata = data.table::fread("/Users/kprovost/Documents/Postdoc_Working/wcdata.txt",data.table=F) 
  }
  latlong_only = merge(latlong_only,wcdata,all=T)
  data.table::fwrite(latlong_only,"/Users/kprovost/Documents/Postdoc_Working/wcdata_latlong.txt")
} else {
  latlong_only = data.table::fread("/Users/kprovost/Documents/Postdoc_Working/wcdata_latlong.txt",data.table=F)
}

## do this with habitat -- gbbatsgeo
if(!(file.exists("/Users/kprovost/Documents/Postdoc_Working/habdata_latlong.txt"))) {
  if(!(file.exists("/Users/kprovost/Documents/Postdoc_Working/habdata.txt"))) {
    habitat_path = "/Users/kprovost/Documents/Postdoc_Working/MMRR/HABITAT_GBB/TIF/"
    hab_files = list.files(path=habitat_path,pattern="tif$",recursive=F,full.names = T)
    hab_stack = stack(hab_files)
    #writeRaster(hab_stack,paste(habitat_path,"gb_stack_geo20.tif",sep=""),format="GTiff") 
    habcells=cellFromXY(hab_stack,latlong_only[,c("LONGITUDE","LATITUDE")])
    latlong_only$habcells = habcells
    habdata=extract(hab_stack,unique(habcells))
    habdata = as.data.frame(habdata)
    habdata$habcells = unique(habcells)
    data.table::fwrite(habdata,"/Users/kprovost/Documents/Postdoc_Working/habdata.txt")
  } else {
    habdata = data.table::fread("/Users/kprovost/Documents/Postdoc_Working/habdata.txt",data.table=F) 
  }
  latlong_only = merge(latlong_only,habdata,all=T)
  data.table::fwrite(latlong_only,"/Users/kprovost/Documents/Postdoc_Working/habdata_latlong.txt")
} else {
  latlong_only = data.table::fread("/Users/kprovost/Documents/Postdoc_Working/habdata_latlong.txt",data.table=F)
}

## do this with soil
if(!(file.exists("/Users/kprovost/Documents/Postdoc_Working/soildata_latlong.txt"))) {
  if(!(file.exists("/Users/kprovost/Documents/Postdoc_Working/soildata.txt"))) {
    soil_path = "/Users/kprovost/Documents/Postdoc_Working/MMRR/SOIL/"
    soil_files = list.files(path=soil_path, pattern="tif$",full.names = T,recursive = T)
    soilstack = stack(soil_files)
    soilcells=cellFromXY(soilstack,latlong_only[,c("LONGITUDE","LATITUDE")])
    latlong_only$soilcells = soilcells
    soildata=extract(soilstack,unique(soilcells))
    soildata = as.data.frame(soildata)
    soildata$soilcells = unique(soilcells)
    data.table::fwrite(soildata,"/Users/kprovost/Documents/Postdoc_Working/soildata.txt")
  } else {
    soildata = data.table::fread("/Users/kprovost/Documents/Postdoc_Working/soildata.txt",data.table=F) 
  }
  
  latlong_only = merge(latlong_only,soildata,all=T)
  data.table::fwrite(latlong_only,"/Users/kprovost/Documents/Postdoc_Working/soildata_latlong.txt")
} else {
  latlong_only = data.table::fread("/Users/kprovost/Documents/Postdoc_Working/soildata_latlong.txt",data.table=F)
}

## do this with topography -- nasadata
if(!(file.exists("/Users/kprovost/Documents/Postdoc_Working/topodata_latlong.txt"))) {
  if(!(file.exists("/Users/kprovost/Documents/Postdoc_Working/topodata.txt"))) {
    topo_path = "/Users/kprovost/Documents/Postdoc_Working/MMRR/TOPOLOGY/"
    topo_files = list.files(path=topo_path, pattern="tif$",full.names = T,recursive = F)
    topo_files = c(topo_files,list.files(path=topo_path, pattern="asc$",full.names = T,recursive = F))
    topostack = stack(topo_files)
    topocells=cellFromXY(topostack,latlong_only[,c("LONGITUDE","LATITUDE")])
    latlong_only$topocells = topocells
    topodata=extract(topostack,unique(topocells))
    topodata = as.data.frame(topodata)
    topodata$topocells = unique(topocells)
    data.table::fwrite(topodata,"/Users/kprovost/Documents/Postdoc_Working/topodata.txt")
  } else {
    topodata = data.table::fread("/Users/kprovost/Documents/Postdoc_Working/topodata.txt",data.table=F) 
  }
  latlong_only = merge(latlong_only,topodata,all=T)
  data.table::fwrite(latlong_only,"/Users/kprovost/Documents/Postdoc_Working/topodata_latlong.txt")
} else {
  latlong_only = data.table::fread("/Users/kprovost/Documents/Postdoc_Working/topodata_latlong.txt",data.table=F)
}

## do this with landcover -- download copernicus_landcover
if(!(file.exists("/Users/kprovost/Documents/Postdoc_Working/landdata_latlong.txt"))) {
  if(!(file.exists("/Users/kprovost/Documents/Postdoc_Working/landdata.txt"))) {
    land_path = "/Users/kprovost/Documents/Postdoc_Working/MMRR/LANDCOVER_COPERNICUS/"
    land_files = list.files(path=land_path, pattern="tif$",full.names = T,recursive = F)
    landstack = stack(land_files)
    landcells=cellFromXY(landstack,latlong_only[,c("LONGITUDE","LATITUDE")])
    latlong_only$landcells = landcells
    landdata=extract(landstack,unique(landcells))
    landdata = as.data.frame(landdata)
    landdata$landcells = unique(landcells)
    data.table::fwrite(landdata,"/Users/kprovost/Documents/Postdoc_Working/landdata.txt")
  } else {
    landdata = data.table::fread("/Users/kprovost/Documents/Postdoc_Working/landdata.txt",data.table=F) 
  }
  latlong_only = merge(latlong_only,landdata,all=T)
  data.table::fwrite(latlong_only,"/Users/kprovost/Documents/Postdoc_Working/landdata_latlong.txt")
} else {
  latlong_only = data.table::fread("/Users/kprovost/Documents/Postdoc_Working/landdata_latlong.txt",data.table=F)
}

## merge latlong_only with df_pca_env

df_pca_env_ll = merge(df_pca_env,latlong_only,all=T)
data.table::fwrite(df_pca_env_ll,"/Users/kprovost/Documents/Postdoc_Working/df_pca_env_ll.txt")


## okay import the new data all compiled
df_edit = data.table::fread("/Users/kprovost/Documents/Postdoc_Working/files_for_mmrr/df_pca_env_ll_EDITED_17Mar2023_Passerellidae.txt",header=T,sep=",",fill=T,data.table=F)
land_path = "/Users/kprovost/Documents/Postdoc_Working/MMRR/LANDCOVER_COPERNICUS/"
land_files = list.files(path=land_path, pattern="tif$",full.names = T,recursive = F)
landstack = stack(land_files)
urbstack = stack("/Users/kprovost/Documents/Postdoc_Working/MMRR/URBANIZATION/STACK/stack_1940-2017-decade.tif")
r <- getData("worldclim",var="bio",res=10)
habitat_path = "/Users/kprovost/Documents/Postdoc_Working/MMRR/HABITAT_GBB/TIF/"
hab_files = list.files(path=habitat_path,pattern="tif$",recursive=F,full.names = T)
hab_stack = stack(hab_files)
soil_path = "/Users/kprovost/Documents/Postdoc_Working/MMRR/SOIL/"
soil_files = list.files(path=soil_path, pattern="tif$",full.names = T,recursive = T)
soilstack = stack(soil_files)
topo_path = "/Users/kprovost/Documents/Postdoc_Working/MMRR/TOPOLOGY/"
topo_files = list.files(path=topo_path, pattern="tif$",full.names = T,recursive = F)
topo_files = c(topo_files,list.files(path=topo_path, pattern="asc$",full.names = T,recursive = F))
topostack = stack(topo_files)
df_edit$LONGITUDE = as.numeric(df_edit$LONGITUDE)
df_edit$LATITUDE = as.numeric(df_edit$LATITUDE)

df_land = extract(landstack,cellFromXY(landstack,df_edit[,c("LONGITUDE","LATITUDE")]))
df_hab = extract(hab_stack,cellFromXY(hab_stack,df_edit[,c("LONGITUDE","LATITUDE")]))
df_wc = extract(r,cellFromXY(r,df_edit[,c("LONGITUDE","LATITUDE")]))
df_urb = extract(urbstack,cellFromXY(urbstack,df_edit[,c("LONGITUDE","LATITUDE")]))
df_soil = extract(soilstack,cellFromXY(soilstack,df_edit[,c("LONGITUDE","LATITUDE")]))
df_topo = extract(topostack,cellFromXY(topostack,df_edit[,c("LONGITUDE","LATITUDE")]))

df_edit2 = cbind(df_edit,df_land,df_hab,df_wc,df_urb,df_soil,df_topo)
data.table::fwrite(df_edit2,"/Users/kprovost/Documents/Postdoc_Working/files_for_mmrr/df_pca_env_ll_EDITED_17Mar2023_Passerellidae_FIX.txt")

## after manually changing names? 

