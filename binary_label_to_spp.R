## change annotation label from 1 to the species name

path = "/Users/kprovost/Documents/Research/Tyrannidae/Wave/"
my_files = list.files(path=path,
                pattern=".wav.csv$",
                full.names = T,
                recursive=F)
lapply(my_files,FUN=function(x){
  print(basename(x))
  df = read.csv(x,header=T,sep=",")
  y=paste(strsplit(basename(x),split = "-")[[1]][1:2],sep="-",collapse="-")
  df$label = y
  write.csv(df,file=x,quote=F,row.names=F)
})
