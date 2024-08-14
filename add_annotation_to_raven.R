add_annotation = function(raven_df,annotation="1") {
  if("annotation" %in% colnames(raven_df)) {
    return(raven_df)
  } else {
    raven_df$annotation = annotation
    return(raven_df)
  }
}

## read file with check.names=F

my_files = list.files(path="/Users/kprovost/Documents/Research/",
                      pattern = ".Table.1.selections.txt$",
                      full.names = T,
                      recursive = T)

## get min duration
z=sapply(my_files,FUN=function(x){
  y = read.csv(x,header=T,sep="\t",check.names = F)
  dur = y$`End Time (s)`-y$`Begin Time (s)`
  mindur = min(dur,na.rm=T)
  return(mindur)
})
which(z == 0)

lapply(my_files,FUN=function(x){
  y = read.csv(x,header=T,sep="\t",check.names = F)
  z = add_annotation(y)
  write.table(z,x,row.names = F,sep="\t",quote = F)
})
