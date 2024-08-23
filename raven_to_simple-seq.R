## simple seq is 
## onset_s	offset_s	label
## one for each file
## Begin Time (s), End Time (s), annotation[if present]

convertRavenToSimpleseq = function(raven_df,annotation="1") {
  ## find col names
  keep_cols = which (colnames(raven_df) %in% c("Begin.Time..s.","End.Time..s.","annotation"))
  Simpleseq_df = add_annotation(raven_df[,keep_cols],annotation=annotation)
  colnames(Simpleseq_df) = c("onset_s","offset_s","label")
  return(Simpleseq_df)
}

add_annotation = function(raven_df,annotation="1") {
  if("annotation" %in% colnames(raven_df)) {
    return(raven_df)
  } else {
    raven_df$annotation = annotation
    return(raven_df)
  }
}

#Simpleseq_df = convertRavenToSimpleseq(raven_df)

my_files = list.files(path="/Users/kprovost/Documents/Research/Tyrannidae/",
                      pattern = ".Table.1.selections.txt$",
                      full.names = T,
                      recursive = T)
names = basename(my_files)
names = sapply(names,FUN=function(x){
  strsplit(x,"-")[[1]][2]
})
names(names) = NULL

lapply(1:length(my_files),FUN=function(i){
  input_file = my_files[i]
  my_name = names[i]
  raven_df = read.csv(input_file,header=T,sep="\t")
  Simpleseq_df = convertRavenToSimpleseq(raven_df,annotation=my_name)
  output_file = gsub(".Table.1.selections.txt",".wav.csv",input_file) 
  write.table(Simpleseq_df,output_file,row.names = F,sep=",",quote = F)
})
