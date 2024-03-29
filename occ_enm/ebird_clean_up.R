## ebird cleanup data

## first trim the ebird data and zip it
files_list = list.files("~/Dropbox (AMNH)/Postdoc_Backup/GBIF/Aves/",
                        pattern="ebd.*txt$",full.names = T,recursive=T)
files_list = files_list[!(grepl("trim",files_list))]
len_files = length(files_list)
for(i in (1:len_files)) {
  filename = files_list[i]
  print(paste(i,"/",len_files,": ",basename(filename),sep=""))
  outfilename=paste(filename,".trim.txt",sep="")
  if(file.exists(outfilename)){
    print("skip")
  } else {
    df = NULL
    try({df = data.table::fread(filename,data.table=F,header = F)})
    #df <- read.delim("~/Work/ebd_relMay-2017.txt_Zonotrichia-leucophrys.txt")
    #df = unique(df)
    if(!(is.null(df)) & nrow(df)>0){
      
      ebird_cols=c("GLOBAL UNIQUE IDENTIFIER", "LAST EDITED DATE", "TAXONOMIC ORDER", 
                   "CATEGORY", "COMMON NAME", "SCIENTIFIC NAME", "SUBSPECIES COMMON NAME", 
                   "SUBSPECIES SCIENTIFIC NAME", "OBSERVATION COUNT", "BREEDING BIRD ATLAS CODE", 
                   "BREEDING BIRD ATLAS CATEGORY", "AGE/SEX", "COUNTRY", "COUNTRY CODE", 
                   "STATE", "STATE CODE", "COUNTY", "COUNTY CODE", "IBA CODE", "BCR CODE", 
                   "USFWS CODE", "ATLAS BLOCK", "LOCALITY", "LOCALITY ID", " LOCALITY TYPE", 
                   "LATITUDE", "LONGITUDE", "OBSERVATION DATE", "TIME OBSERVATIONS STARTED", 
                   "OBSERVER ID", "FIRST NAME", "LAST NAME", "SAMPLING EVENT IDENTIFIER", 
                   "PROTOCOL TYPE", "PROJECT CODE", "DURATION MINUTES", "EFFORT DISTANCE KM", 
                   "EFFORT AREA HA", "NUMBER OBSERVERS", "ALL SPECIES REPORTED", 
                   "GROUP IDENTIFIER", "HAS MEDIA", "APPROVED", "REVIEWED", "REASON", 
                   "TRIP COMMENTS", "SPECIES COMMENTS")
      ebird_cols = sub(" ",".",ebird_cols)
      colnames(df) = ebird_cols
      head(df)
      df_trim = df[,c("SCIENTIFIC.NAME","LATITUDE","LONGITUDE","OBSERVATION.DATE")]
      df_trim$YEAR = format(as.Date(df_trim$OBSERVATION.DATE,format="%Y-%m-%d"),"%Y")
      df_trim = df_trim[,c("SCIENTIFIC.NAME","LATITUDE","LONGITUDE","YEAR")]
      df_trim = unique(df_trim)
      write.table(df_trim,outfilename,row.names = F,
                  quote = F,sep="\t")
    }
  }
  ## gzip the original file 
  R.utils::gzip(filename)
}

## then merge the ebird data with the subspplabelR data 

## path
path="/Users/kprovost/Dropbox (AMNH)/Postdoc_Backup/GBIF/Aves/"
taxonomy = list.dirs(path,recursive = T)

for(directory in (taxonomy)){
  print(directory)
  ebird_list = list.files(path=directory,pattern="trim.txt$",full.names = T)
  occs_list = list.files(path=directory,pattern="subspplabelR.txt$",full.names = T)
  if(length(ebird_list)>0 & length(occs_list)>0) {
    ## need to match 
    
    ebird_names = sub(".txt.trim.txt","",sub("ebd_relMay-2017.txt_","",basename(ebird_list)))
    occs_names = sub("_occurrences_subspplabelR.txt","",basename(occs_list))
    
    matches = intersect(ebird_names,occs_names)
    ebird_kept = cbind(ebird_list[ebird_names %in% matches],ebird_names[ebird_names %in% matches])
    occs_kept = cbind(occs_list[occs_names %in% matches],occs_names[occs_names %in% matches])
    colnames(ebird_kept) = c("ebird_kept","names")
    colnames(occs_kept) = c("occs_kept","names")
    ebird_occs_merge = merge(ebird_kept,occs_kept,all=T)
    
    length_to_do = nrow(ebird_occs_merge)
    if(length_to_do>0){
      for(row_i in 1:length_to_do){
        print(paste(row_i,"/",length_to_do))
        ## ebird 
        #ebird="Accipitriformes/Accipitridae/Accipiter/ebd_relMay-2017.txt_Accipiter-albogularis.txt.trim.txt"
        ebird = ebird_occs_merge$ebird_kept[row_i]
        
        ## subspplabelr
        #occs="Accipitriformes/Accipitridae/Accipiter/Accipiter-albogularis_occurrences_subspplabelR.txt"
        occs = ebird_occs_merge$occs_kept[row_i]
        
        setwd(directory)
        ebird_df = data.table::fread(ebird,data.table = F)
        colnames(ebird_df) = str_to_upper(colnames(ebird_df))
        ebird_df$PROV = "ebird"
        occs_df = data.table::fread(occs,data.table = F,sep=" ")
        colnames(occs_df) = str_to_upper(colnames(occs_df))
        occs_df$YEAR = format(as.Date(occs_df$DATE,format="%Y-%m-%d"),"%Y")
        if(nrow(occs_df)>0 & nrow(ebird_df)>0){
          merge_df = rbind(ebird_df[,c("LONGITUDE","LATITUDE","PROV","YEAR")],
                           occs_df[,c("LONGITUDE","LATITUDE","PROV","YEAR")])
          merge_df = unique(merge_df)
          
          outfile = paste(occs,"_ebird.txt",sep="")
          write.table(merge_df,outfile)
          
          R.utils::gzip(ebird)
          R.utils::gzip(occs)
        }
        
      }
    }
  }
}


library(stringr)

# library(raster)
# library(rgdal)
# w = getData('worldclim',res=10,var="bio")
# #plot(w[[1]])
# bg = w[[1]]
# bg = crop(bg,extent(-172,-50,20,72))
# plot(bg)
# 
# cells=cellFromXY(bg,df_trim[,c("LONGITUDE","LATITUDE")])
# df_trim$CELL = cells
# df_cell = df_trim[,c("CELL","YEAR")]
# df_cell = unique(df_cell)
# cell_ras = bg
# cell_ras[!(is.na(cell_ras))] = 0 
# cell_table = table(df_cell$CELL)
# cell_ras[as.numeric(names(cell_table))] = cell_table
# plot(cell_ras)
# cell_bool = cell_ras
# cell_bool[cell_bool>0] = 1 
# plot(cell_bool)
# 
# bg_agg = aggregate(bg,5)
# plot(bg_agg)
# cells=cellFromXY(bg_agg,df_trim[,c("LONGITUDE","LATITUDE")])
# df_trim$CELL = cells
# df_cell = df_trim[,c("CELL","YEAR")]
# df_cell = unique(df_cell)
# cell_ras = bg_agg
# cell_ras[!(is.na(cell_ras))] = 0 
# cell_table = table(df_cell$CELL)
# cell_ras[as.numeric(names(cell_table))] = cell_table
# plot(cell_ras)
# cell_bool = cell_ras
# cell_bool[cell_bool>0] = 1 
# plot(cell_bool)
# 
# 
# cells_df=cellFromXY(bg,df[,c("LONGITUDE","LATITUDE")])
# df = as.data.frame(df)
# df$CELL = cells_df
# cell_big_tab = table(df$CELL)
# cell_ras_big = bg
# cell_ras_big[!(is.na(cell_ras_big))] = 0 
# cell_ras_big[as.numeric(names(cell_big_tab))] = cell_big_tab
# plot(cell_ras_big)
