data_folder = "/Users/kprovost/Documents/Postdoc_Working/Sounds_and_Annotations/9SppBalanced/Wave/"
data_files = list.files(data_folder,pattern="resample.48000",recursive=F,full.names = T)
data_files = data_files[!grepl("xml",data_files)]
data_text = data_files[grepl("Table.1",data_files)]
data_wavs = data_files[!grepl("Table.1",data_files)]

outdf = data.frame()
outwavdf = data.frame()
for(i in 1:length(data_text)){
  print(i)
  textfile = data_text[i]
  #basefile=strsplit(basename(textfile),"_")[[1]][1]
  basefile=basename(textfile)
  basesplit = strsplit(basefile,"\\.")[[1]]
  genus=basesplit[1]
  species=basesplit[2]
  subspecies=basesplit[3]
  collection=basesplit[4]
  id=basesplit[5]
  
  df = read.table(textfile,sep="\t",header=T)
  num_syl = nrow(df)
  df$lengths = df$End.Time..s.-df$Begin.Time..s.
  df$bws = df$High.Freq..Hz.-df$Low.Freq..Hz.
  df$centerfreq = rowMeans(df[,c("High.Freq..Hz.","Low.Freq..Hz.")],na.rm=T)
  df$genus = genus
  df$species = species
  df$subspecies = subspecies
  df$collection = collection
  df$id = id
  df$file = basefile
  
  if(nrow(outdf)==0) { outdf=df } else { outdf = gtools::smartbind(outdf,df) }
  
  wavfile = sub("Table.1.selections.txt","wav",textfile)
  if(file.exists(wavfile)){
    r <- tuneR::readWave(wavfile,from=0,units="seconds")
    duration = length(r) / r@samp.rate
    wavdf=data.frame(file=basefile,duration=duration)
    wavdf$genus = genus
    wavdf$species = species
    wavdf$subspecies = subspecies
    wavdf$collection = collection
    wavdf$id = id
    if(nrow(outwavdf)==0) { outwavdf=wavdf } else { outwavdf = gtools::smartbind(outwavdf,wavdf) }
  }
  
}

outdf = unique(outdf)
outwavdf = unique(outwavdf)

write.table(outdf,paste(data_folder,"outdf.txt",sep=""))
write.table(outwavdf,paste(data_folder,"outwavdf.txt",sep=""))

png("~/syllable_bounding_box_len_per_spp.png",
    width=8,height=10,units="in",res=600)
par(mfrow=c(4,3))
for(spp in sort(unique(outdf$species))){
  hist(outdf$lengths[outdf$species==spp],
       main=spp,xlab="Syllable Bounding Box Lengths (sec)")
}
hist(outdf$lengths,
     main="ALL",xlab="Syllable Bounding Box Lengths (sec)")
dev.off()

agglenmn = aggregate(outdf$lengths~outdf$species,FUN=function(x){mean(x,na.rm=T)})
agglensd = aggregate(outdf$lengths~outdf$species,FUN=function(x){sd(x,na.rm=T)})
colnames(agglenmn) = c("species","meanlen")
colnames(agglensd) = c("species","sdlen")
agglen = merge(agglenmn,agglensd,all=T)

par(mfrow=c(4,3))
for(spp in sort(unique(outdf$species))){
  hist(outdf$bws[outdf$species==spp],
       main=spp,xlab="Syllable Bounding Box Bandwidths (Hz)")
}
hist(outdf$bws,
     main="ALL",xlab="Syllable Bounding Box Bandwidths (Hz)")


aggwav = aggregate(outwavdf$duration~outwavdf$file+outwavdf$species,FUN=function(x){sum(x,na.rm=T)})
sylls = as.data.frame(table(outdf$file))
colnames(sylls) = c("file","numsyll")
colnames(aggwav) = c("file","species","duration")
aggwav = merge(aggwav,sylls,all=T)
aggwav$syllpersec = aggwav$numsyll/aggwav$duration

write.table(aggwav,paste(data_folder,"aggwav.txt",sep=""))


png("~/syllable_per_sec_per_spp.png",
    width=8,height=10,units="in",res=600)
par(mfrow=c(4,3))
for(spp in sort(unique(aggwav$species))){
  hist(aggwav$syllpersec[aggwav$species==spp],
       main=spp,xlab="Syllables/Second")
}
hist(aggwav$syllpersec,
     main="ALL",xlab="Syllables/Second")
dev.off()

aggsylsecmn = aggregate(aggwav$syllpersec~aggwav$species,FUN=function(x){mean(x,na.rm=T)})
aggsylsecsd = aggregate(aggwav$syllpersec~aggwav$species,FUN=function(x){sd(x,na.rm=T)})
colnames(aggsylsecmn) = c("species","meanss")
colnames(aggsylsecsd) = c("species","sdss")
aggsylsec = merge(aggsylsecmn,aggsylsecsd,all=T)

aggsyllen = merge(aggsylsec,agglen,all=T)
plot(aggsyllen$meanss,aggsyllen$meanlen)
segments(x0=aggsyllen$meanss+aggsyllen$sdss,
         x1=aggsyllen$meanss-aggsyllen$sdss,
         y0=aggsyllen$meanlen,col="grey")
segments(y0=aggsyllen$meanlen+aggsyllen$sdlen,
         y1=aggsyllen$meanlen-aggsyllen$sdlen,
         x0=aggsyllen$meanss,col="grey")
mod=lm(aggsyllen$meanlen~aggsyllen$meanss)
abline(mod)
summary(mod)

outputdf=data.frame()
dffilepath = "/Users/kprovost/Documents/Postdoc_Working/Finished_Models/"
dffiles=list.files(dffilepath,"annot.csv$",full.names = T,recursive = T)
for(dffile in dffiles){
print(dffile)
  df=read.csv(dffile)
  df$file=gsub(".wav$",".Table.1.selections.txt",df$audio_path)
  
newdf=as.data.frame(table(df$file))
colnames(newdf) = c("file",strsplit(strsplit(dffile,"//")[[1]][2],"/")[[1]][1])
if(nrow(outputdf)==0) {
  outputdf = merge(aggwav,newdf)
} else {
  outputdf = merge(outputdf,newdf,all=T)
}
}
outputdf = unique(outputdf)
write.table(outputdf,paste(dffilepath,"outputdf.txt",sep=""))



testdf = read.table("/Users/kprovost/testdf.txt",sep="\t",header=T)
outdf4 = data.frame()
for(spp in sort(unique(testdf$species))){
  smalldf=testdf[testdf$species==spp,]
  toadd=as.data.frame(t(as.data.frame(colMeans(abs(smalldf[,c(3:27)]),na.rm=T))))
  toadd$species = spp
  outdf4 = rbind(outdf4,toadd)
}
rownames(outdf4) = NULL
write.table(outdf4,paste(dffilepath,"outdf4_abs_0.txt",sep=""))
