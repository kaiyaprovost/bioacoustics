## count durations of wav.csv files

my_files = list.files(path="/Users/kprovost/Documents/Research/Tyrannidae/NewWave",
                      pattern="wav.csv",
                      full.names = T,
                      recursive = F)
df_list = lapply(my_files,FUN=function(x){
  print(x)
  df = read.table(x,header=T,sep=",")
  df$label[is.na(df$label)] = 1
  labelsx = df$label[1]
  if(labelsx==1){
    print(labelsx)
    
    labelsx = strsplit(basename(x),"-")[[1]][2]
    df$label = labelsx[2]
    write.table(df,x,sep=",",quote=F,row.names=F)
  } else {
    labelsx = strsplit(labelsx,"-")[[1]]
    if(length(labelsx)!=1){
      print(labelsx)
      
      df$label = labelsx[2]
      write.table(df,x,sep=",",quote=F,row.names=F)
    }
  }
  return(df)
})

df = do.call(rbind,df_list)
df=as.data.frame(df)
df$duration = df$offset_s-df$onset_s
agg = aggregate(df$duration~df$label,FUN=sum)
agg

filename = "1.mp3.wav"
filename2 = "1.wav"

sub(".mp3","",filename)
file.rename(filename,sub(".mp3","",filename))

## run it as a loop

my_files = list.files(path=path,
                      pattern=pattern)
for(filename in my_files){
  file.rename(filename,sub(".mp3","",filename))
}