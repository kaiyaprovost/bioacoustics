## spectrogram comparions 

library(seewave)
library(tuneR)
library(ggplot2)
library(RColorBrewer)
library(data.table)

path="/Users/kprovost/Documents/Postdoc_Working/FOR_FIGURES_ONLY/"
setwd(path)
listwavs = list.files(path=path,pattern="*.wav$")
listtexts  = list.files(path=path,pattern="annot")

palette(c("black","red","pink","orange","goldenrod","green","cyan","blue",
          "purple","brown","grey"))

for(wavfile in listwavs){
  print(wavfile)
  
  ## FIRST GENERAL STUFF
  
  statsfile=paste(wavfile,"_SEGMENTS.STATS.txt",sep="")
  wav=readWave(wavfile)
  samp=wav@samp.rate
  sec=length(wav)/samp
  sequence = seq(0,sec,length.out=sec*1000)
  
  tab1 = read.table(gsub("wav$","Table.1.selections.txt",wavfile),sep="\t",header=T)
  
  counts_true=sapply(sequence,FUN=function(x){
    sum((tab1$Begin.Time..s. <= x) & (tab1$End.Time..s. >= x),na.rm=T)
  })
  
  base = gsub(".wav$","",wavfile)
  

  true_seg = nrow(tab1)
  
  ## NEXT 50 STUFF
  
  tables=listtexts[grep(base,listtexts)]
  tables  =  tables[grep("-50",tables)]
  tablab = gsub(".annot.csv.selections.txt","",tables)
  tablab = gsub(base,"",tablab)
  
  png(paste(wavfile,"_SEGMENTS-50.png",sep=""))
  par(mar=c(4,8,4,0))
  plot(0,xlim=c(0,sec),ylim=c(0,10),type="n",xlab="Time",yaxt="n",
       ylab="",
       main=base)
  axis(2,at=c(1:length(tablab),10),labels =c(tablab,"TRUE"),las=2)
  segments(x0=tab1$Begin.Time..s.,x1=tab1$End.Time..s.,y0=10,lwd=3)
  segments(x0=c(tab1$Begin.Time..s.,tab1$End.Time..s.),
           y0=10+0.2,y1=10-0.2)
  abline(v=tab1$Begin.Time..s.,col="blue",lty=3)
  abline(v=tab1$End.Time..s.,col="red",lty=3)
  
  for(i in 1:length(tables)){
    table = tables[i]
    type=tablab[i]
    df = read.table(table,header=T,sep="\t")
    segments(x0=df$Begin.Time..s.,x1=df$End.Time..s.,y0=i,lwd=3,col=i+1)
    segments(x0=c(df$Begin.Time..s.,df$End.Time..s.),
             y0=i+0.2,y1=i-0.2,col=i+1)
    
    x=as.data.table(tab1[,c("Begin.Time..s.","End.Time..s.")])
    y=as.data.table(df[,c("Begin.Time..s.","End.Time..s.")])
    
    counts_test=sapply(sequence,FUN=function(x){
      sum((df$Begin.Time..s. <= x) & (df$End.Time..s. >= x),na.rm=T)
    })
    
    test_seg = nrow(df)
    
    ## take differences between them 
    counts=as.data.frame(cbind(counts_true,counts_test))
    true_pos = sum(counts$counts_true==1 & counts$counts_test==1)/nrow(counts)
    true_neg = sum(counts$counts_true==0 & counts$counts_test==0)/nrow(counts)
    fals_pos = sum(counts$counts_true==0 & counts$counts_test==1)/nrow(counts)
    fals_neg = sum(counts$counts_true==1 & counts$counts_test==0)/nrow(counts)
    
    acc=true_pos+true_neg
    precision=true_pos / (true_pos+fals_pos)
    recalls = true_pos / (true_pos+fals_neg)
    fscore = 2 * ((precision * recalls) / (precision + recalls))
    
    seg_ratio = test_seg/true_seg
    
    row=cbind(type,acc,precision,recalls,fscore,true_pos,true_neg,fals_pos,fals_neg,true_seg,test_seg,seg_ratio)
    write.table(row,statsfile,append=T,row.names = F,quote = F)
    
    ## True Positives
    ## True Negatives
    ## False Positives
    ## False Negatives
    
  }
  
  dev.off()
  
  ## THEN 5 STUFF
  
  tables=listtexts[grep(base,listtexts)]
  tables  =  tables[grep("-5\\.",tables)]
  tablab = gsub(".annot.csv.selections.txt","",tables)
  tablab = gsub(base,"",tablab)
  
  png(paste(wavfile,"_SEGMENTS-5.png",sep=""))
  par(mar=c(4,8,4,0))
  plot(0,xlim=c(0,sec),ylim=c(0,10),type="n",xlab="Time",yaxt="n",
       ylab="",
       main=base)
  axis(2,at=c(1:length(tablab),10),labels =c(tablab,"TRUE"),las=2)
  segments(x0=tab1$Begin.Time..s.,x1=tab1$End.Time..s.,y0=10,lwd=3)
  segments(x0=c(tab1$Begin.Time..s.,tab1$End.Time..s.),
           y0=10+0.2,y1=10-0.2)
  abline(v=tab1$Begin.Time..s.,col="blue",lty=3)
  abline(v=tab1$End.Time..s.,col="red",lty=3)
  
  for(i in 1:length(tables)){
    table = tables[i]
    type=tablab[i]
    df = read.table(table,header=T,sep="\t")
    segments(x0=df$Begin.Time..s.,x1=df$End.Time..s.,y0=i,lwd=3,col=i+1)
    segments(x0=c(df$Begin.Time..s.,df$End.Time..s.),
             y0=i+0.2,y1=i-0.2,col=i+1)
    
    x=as.data.table(tab1[,c("Begin.Time..s.","End.Time..s.")])
    y=as.data.table(df[,c("Begin.Time..s.","End.Time..s.")])

    counts_test=sapply(sequence,FUN=function(x){
      sum((df$Begin.Time..s. <= x) & (df$End.Time..s. >= x),na.rm=T)
    })
    
    test_seg = nrow(df)
    
    ## take differences between them 
    counts=as.data.frame(cbind(counts_true,counts_test))
    true_pos = sum(counts$counts_true==1 & counts$counts_test==1)/nrow(counts)
    true_neg = sum(counts$counts_true==0 & counts$counts_test==0)/nrow(counts)
    fals_pos = sum(counts$counts_true==0 & counts$counts_test==1)/nrow(counts)
    fals_neg = sum(counts$counts_true==1 & counts$counts_test==0)/nrow(counts)
    
    acc=true_pos+true_neg
    precision=true_pos / (true_pos+fals_pos)
    recalls = true_pos / (true_pos+fals_neg)
    fscore = 2 * ((precision * recalls) / (precision + recalls))
    
    seg_ratio = test_seg/true_seg
    
    row=cbind(type,acc,precision,recalls,fscore,true_pos,true_neg,fals_pos,fals_neg,true_seg,test_seg,seg_ratio)
    write.table(row,statsfile,append=T,row.names = F,quote = F)
    
    ## True Positives
    ## True Negatives
    ## False Positives
    ## False Negatives
    
  }
  
  dev.off()
  
}



# wav=tuneR::readWave("/Users/kprovost/Documents/Postdoc_Working/FOR_FIGURES_ONLY/Melozone_fusca.BLB10118.wav_2162756-2354996.wav")
# spec = spectro(wav,wl=512,f=48000)
# plot(spec$time,spec$freq)
# 
# amp=as.matrix(spec$amp)
# heatmap(amp,Rowv=NA,Colv=NA,symm=F,scale="none",
#         col=colorRampPalette(brewer.pal(11,"RdYlBu"))(100),
#         margins=c(0,0))
# 
# tab1 = read.table("/Users/kprovost/Documents/Postdoc_Working/FOR_FIGURES_ONLY/Melozone_fusca.BLB10118.wav_2162756-2354996.Table.1.selections.txt")
# 
# spec$amp
# 
# 
