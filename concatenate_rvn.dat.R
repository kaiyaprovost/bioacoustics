## given a folder, concatenate all of the rvn.dat*temp files 

folder = "/Users/kprovost/Documents/Postdoc_Working/MMRR/WAVS/Wave/0STATSDONE/"
pattern = "*.temp$"
myfiles=list.files(path=folder,pattern=pattern,recursive=T,full.names = T)

print("read")
filelist=lapply(myfiles,FUN=function(x){
  read.table(x)
})

print("bind")
df = do.call(gtools::smartbind,filelist)
df=unique(df)
print("write")
write.table(df,paste(folder,"rvn.dat_trimmed_spectro_fcts_collapsed_COMBINED_7Mar2023.txt.temp",sep=""))
lapply(myfiles,FUN=function(x){
  R.utils::gzip(x)
})


