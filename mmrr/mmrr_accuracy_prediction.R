fil="/Users/kprovost/Documents/Postdoc_Working/MMRR/accuracy_metrics_combined.txt"
df=read.table(fil,header=T)
agg = aggregate(df$accuracy~df$genus+df$species+df$predicted,FUN=function(x){mean(x,na.rm=T)})
colnames(agg) = c("genus","species","predicted","accuracy")
#boxplot(agg$accuracy~agg$predicted)

#boxplot(agg$accuracy[agg$genus=="Melozone"]~
#          agg$predicted[agg$genus=="Melozone"])

#boxplot(agg$accuracy[agg$genus=="Zonotrichia"]~
#          agg$predicted[agg$genus=="Zonotrichia"])

#boxplot(agg$accuracy[agg$genus!="Zonotrichia" & agg$genus!="Melozone"]~
#          agg$predicted[agg$genus!="Zonotrichia" & agg$genus!="Melozone"])

df$predictedN = as.numeric(as.factor(df$predicted))
df=df[order(df$predictedN),]



#plot(df$predictedN,df$fscore,type="n",ylim=c(0,1))
# for(i in sort(unique(agg$genus))){
#   g = df[df$genus==i,]
#   for(j in sort(unique(g$species))) {
#     s = g[g$species==j,]
#     print(paste(i,j))
#     for(p in sort(unique(basename(s$predfile)))){
#       
#       x = s[grepl(p,s$predfile),]
#       lines(x$predictedN,x$fscore,col=rgb(0,0,0,0.1))
#     }
#   }
# }

agg = aggregate(df$fscore~df$genus+df$species+df$predicted,FUN=function(x){mean(x,na.rm=T)})
colnames(agg) = c("genus","species","predicted","fscore")
agg$predictedN = as.numeric(as.factor(agg$predicted))
agg=agg[order(agg$predictedN),]
# plot(agg$predictedN,agg$fscore,type="n",ylim=c(0,1))
# for(i in sort(unique(agg$genus))){
#   g = agg[agg$genus==i,]
#   for(j in sort(unique(g$species))) {
#     print(paste(i,j))
#     s = g[g$species==j,]
#     lines(s$predictedN,s$fscore,col=rgb(0,0,0,0.1))
#   }
# }


## segments
segfile = "/Users/kprovost/Documents/Postdoc_Working/MMRR/predicted_slices_29Oct2022.txt"
segdf = data.table::fread(segfile,header=F,sep="\t",fill=TRUE,data.table=F)
pred = segdf[segdf$V1=="PRED",]
pred$V2 = basename(pred$V2)

colsumdf = matrix(data=0,ncol=ncol(pred),nrow=0)

#hist(rowSums(pred[,3:ncol(pred)],na.rm=T))

for(i in sort(unique(pred$V2))){
  print(i)
  pred_i = pred[pred$V2==i,-c(1:2)]
  sums_i = colSums(pred_i,na.rm=T)
  colsumdf = rbind(colsumdf,c("AVG",i,sums_i))
}

write.table(colsumdf,"/Users/kprovost/Documents/Postdoc_Working/MMRR/predicted_slices_SUMS_2Nov2022.txt",
            row.names = F,quote=F,col.names = F)

colsumdf=read.table("/Users/kprovost/Documents/Postdoc_Working/MMRR/predicted_slices_SUMS_2Nov2022.txt",
                    header=F)
## convert the average to an annotation

window_size = 88 ## size of windows taken from spectrograms, in number of time bins, shonw to neural networks
fft_size = 512 ## size of window for Fast Fourier transform, number of time bins. Default is 512.
step_size = 32 ## step size for Fast Fourier transform. Default is 64.
sampling_rate = 48000
timebin_dur = 0.00067 ## how many seconds is one time bin
min_segment_dur = 0.05
lowhz=500
highhz=15000



for(file_i in 1:nrow(colsumdf)){
  file_i_file = colsumdf[file_i,2]
  print(file_i)
  outfile = gsub(".selections.txt",".selections.AVERAGE.txt",file_i_file)
  
  testrow=as.numeric(colsumdf[file_i,3:ncol(colsumdf)])
  testrow[testrow!=max(testrow,na.rm=T)]=0
  runlength=rle(testrow)
  stops=cumsum(runlength$lengths)
  starts = stops+1
  starts=c(1,starts)
  starts=starts[1:length(stops)]
  are_syll = which(runlength$values==max(testrow,na.rm=T))
  onsets = starts[are_syll]
  offsets = stops[are_syll]
  ## convert to seconds
  onsets_s = onsets*timebin_dur
  offsets_s = offsets*timebin_dur
  too_short = which(offsets_s - onsets_s < min_segment_dur)
  if(length(too_short)>=1){
    onsets_s=onsets_s[-too_short]
    offsets_s=offsets_s[-too_short]
  }
  if(length(onsets_s)>=1){
    ## write out 
    #Selection	View	Channel	Begin Time (s)	End Time (s)	Low Freq (Hz)	High Freq (Hz)	Begin File	type
    x=data.frame(1:length(onsets_s),
                 rep("Spectrogram",length(onsets_s)),
                 rep(1,length(onsets_s)),
                 onsets_s,
                 offsets_s,
                 rep(lowhz,length(onsets_s)),
                 rep(highhz,length(onsets_s)),
                 gsub(".selections.txt",".wav",colsumdf[file_i,2]),
                 rep(1,length(onsets_s))
    )
    colnames(x) = c("Selection",
                    "View",
                    "Channel",
                    "Begin Time (s)",
                    "End Time (s)",
                    "Low Freq (Hz)",
                    "High Freq (Hz)",
                    "Begin File",
                    "type")
    write.table(x,outfile,row.names = F,quote=F,sep="\t")
  } else {print("BAD")}
  
}

