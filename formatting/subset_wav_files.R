library(tuneR)
check_rate = F

## TODO: update so that formatted correctly

#mypath="/Users/kprovost/Documents/Postdoc_Working/JY_project/Zonotrichia/LEUCOPHRYS/"
mypath="/Users/kprovost/Documents/Postdoc_Working/Halkin/"
mypattern = ".Table.1.selections.txt"
#mypattern = ".selections.MASTER.txt"

seconds_separated = 0.4
buffer = 0.05

## look for files with selections and see if they fit the criteria
metafiles = list.files(path=mypath,
                       pattern=paste(mypattern,"$",sep=""),recursive = T,full.names = T)
#   file = "/Users/kprovost/OneDrive - The Ohio State University/BLB_Data/batch2/annotated/BLB23326.wav"
# meta = "/Users/kprovost/OneDrive - The Ohio State University/BLB_Data/batch2/annotated/BLB23326.Table.1.selections.txt"
metafiles=metafiles[!(grepl("wav_",metafiles))]
#metafiles = metafiles[grepl("Junco",metafiles)]
if(check_rate==T){
  for(meta in metafiles){
    print(meta)
    wavfile = sub(mypattern,".wav",meta)
    if(file.exists(wavfile)) {
      wav = readWave(wavfile,units = "samples",from=0)
      samples_per_ms=wav@samp.rate
      if(samples_per_ms!=48000){
        print(samples_per_ms)
        try({R.utils::gzip(wavfile,overwrite=T)})
      }
    }
  }
}

for(meta in (metafiles)) {
  print(meta)
  file = sub(mypattern,".wav",meta)
  gzfile =  sub(mypattern,".wav.gz",meta)
  
  try({
    
    if(file.exists(gzfile)){
      R.utils::gunzip(gzfile)
    }
    if(file.exists(file)) {
      
      wav = readWave(file,units = "samples",from=0) ## if you change this to seconds it can cut off bits
      samples_per_ms=wav@samp.rate
      full_duration=length(wav)
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
      
      # if (min(differences[c(need_splitting)],na.rm=T) > max(soundlengths,na.rm=T)){
      #   to_buffer = soundlengths
      #   #to_buffer = rep(buffer,length(soundlengths))
      # } else {
      #   to_buffer = rep( min(differences[c(need_splitting)],na.rm=T), length(soundlengths))
      # }
      new_firsts = firsts-buffer
      new_lasts = lasts+buffer
      
      boundaries = cbind(floor(new_firsts*samples_per_ms),ceiling(new_lasts*samples_per_ms))
      #boundaries[boundaries<1] = 1
      #boundaries[boundaries>full_duration] = full_duration
      
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
        newfile = paste(file,"_",row[1],"-",row[2],".wav",sep="")
        newmeta = paste(file,"_",row[1],"-",row[2],mypattern,sep="")
        if(!(file.exists(newmeta))){
          this_buffer/samples_per_ms ## this is the amount of time between start of recording and start of first annotation
          
          
          writeWave(wav[row[1]:row[2]],newfile,extensible = F)
          
          
          ## THIS SUBSET IS NOT WORKING AND I DON'T KNOW WHY   
          subset_df = df[df[,4]>=seconds[1] & df[,5]<=seconds[2],] ## begin time is col 4, end time is col 5
          if("TYPE" %in% colnames(subset_df)){
            subset_df = subset_df[,c(1:7,which(colnames(subset_df)=="TYPE"))]
          } else if ("type" %in%  colnames(subset_df)) {
            subset_df = subset_df[,c(1:7,which(colnames(subset_df)=="type"))]
          } else {
            subset_df = subset_df[,1:7]
          }
          
          seconds[2]-seconds[1]
          
          subset_df[,4] = subset_df[,4]-(seconds[1])
          subset_df[,5] = subset_df[,5]-(seconds[1])
          
          
          write.table(subset_df,newmeta,quote=F,sep="\t",row.names = F)
        } else {print("SKIPPING")}
        
        
        
      }
      try({R.utils::gzip(file,remove=T)})
      try({R.utils::gzip(meta,remove=T)})
    }
    
  })
}
beepr::beep(sound = 3)
