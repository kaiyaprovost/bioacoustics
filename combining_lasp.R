## get folder with the annotation files
folder = "/Users/kprovost/Documents/Chondestes_grammacus"

## get a list of the files that have the annotations
files = list.files(path=folder,
                   pattern="Table.1.selections.txt$",
                   full.names = T)

## generate a list of the dataframes from reading in the files
df_list = lapply(files,FUN=function(my_file){
  ## read in the file
  df = read.table(my_file,header=TRUE,sep="\t")
  return(df)
})

## glue the dataframes together using smartbind and do.call
big_df = do.call(gtools::smartbind,df_list)

## make sure no duplicate rows
big_df = unique(big_df)

## write out the big dataframe
my_filename = "LASP_combined_annotations_18July2024.Table.1.selections.txt"
setwd(folder) ## change the working directory to output the big dataframe
write.table(big_df,file=my_filename,sep="\t",row.names=FALSE)

test_file = read.table("/Users/kprovost/Documents/Chondestes_grammacus/LASP_combined_annotations_18July2024.Table.1.selections.txt",sep="\t",header=T)
