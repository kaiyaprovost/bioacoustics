library(tuneR)

mypath="/Users/kprovost/Documents/Postdoc_Working/Sounds_and_Annotations/Aves"
metafiles = list.files(path=mypath,
                       pattern=".Table.1.selections.txt$",recursive = T,full.names = T)
#   file = "/Users/kprovost/OneDrive - The Ohio State University/BLB_Data/batch2/annotated/BLB23326.wav"
# meta = "/Users/kprovost/OneDrive - The Ohio State University/BLB_Data/batch2/annotated/BLB23326.Table.1.selections.txt"
metafiles=metafiles[!(grepl("wav_",metafiles))]
## load the files 

for(meta in metafiles) {
  print(meta)
  file = sub(".Table.1.selections.txt",".wav",meta)
  if(file.exists(file)){
    wav = readWave(file,units = "samples",from=0)
    samples_per_ms=wav@samp.rate
    
    
    seconds_separated = 1
    buffer = 1 
    buffer_samp = buffer*samples_per_ms
    
    
    
    
    df = read.table(meta,header=T,sep="\t",check.names = F)
    df=df[order(df[,4]),] ## begin time is col 4
    df$Selection = 1:nrow(df)
    
    ## get separations of more than 5 seconds
    differences = sapply(2:nrow(df),FUN=function(x){
      row1 = df[x,4] ## begin time is col 4
      row2 = df[(x-1),5] ## end time is col 5
      dif = row1-row2
      return(dif)
    })
    
    need_splitting = which(differences >= seconds_separated)
    lasts = df[c(need_splitting,nrow(df)),5] ## end time is col 5
    firsts = df[c(1,need_splitting+1),4] ## begin time is col 4
    soundlengths = lasts-firsts
    
    new_firsts = firsts-buffer
    new_lasts = lasts+buffer
    
    boundaries = cbind(floor(new_firsts*samples_per_ms),ceiling(new_lasts*samples_per_ms))
    
    for(rownum in 1:nrow(boundaries)) {
      this_buffer=buffer_samp
      row = boundaries[rownum,]
      seconds = row/samples_per_ms
      if(row[1]<0){
        row[1]=0
        seconds[1]=0
        this_buffer=this_buffer-abs(boundaries[rownum,1])
      }
      if(row[2]>full_duration){
        row[2]=full_duration
        seconds[2]=full_duration/samples_per_ms
      }
      this_buffer/samples_per_ms ## this is the amount of time between start of recording and start of first annotation
      
      ## THIS SUBSET IS NOT WORKING AND I DON'T KNOW WHY   
      subset_df = df[df[,4]>=seconds[1] & df[,5]<=seconds[2],] ## begin time is col 4, end time is col 5
      subset_df = subset_df[,1:7]
      
      seconds[2]-seconds[1]
      
      subset_df[,4] = subset_df[,4]-(seconds[1])
      subset_df[,5] = subset_df[,5]-(seconds[1])
      
      newmeta = paste(file,"_",row[1],"-",row[2],".Table.1.selections.txt",sep="")
      write.table(subset_df,newmeta,quote=F,sep="\t",row.names = F)
      
    }
    
  }
  
  
}

