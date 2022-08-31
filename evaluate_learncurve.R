eval = "/Users/kprovost/Documents/Postdoc_Working/Finished_Models/FINAL_EVALS_15Aug2022.csv"
evaldf = read.csv(eval,header=T)
evaldf = evaldf[order(evaldf$train.dur,evaldf$TRAIN),]

#plot(log10(evaldf$train.dur),evaldf$avg.acc,ylim=c(0,1))
allspp = sort(unique(evaldf$TRAIN))
#for(i in 1:length(allspp)){
#  spp = allspp[i]
#  evalsm = evaldf[evaldf$TRAIN==spp,]
#  points(log10(evalsm$train.dur),evalsm$avg.acc,col=i,pch=i,type="p")
#}
#boxplot(evaldf$avg.acc~evaldf$train.dur)

pdf("/Users/kprovost/Documents/Postdoc_Working/Finished_Models/FINAL_EVALS_15Aug2022_boxplots.pdf")
for(i in 1:length(allspp)){
  par(mfrow=c(2,1))
  spp = allspp[i]
  print(spp)
  evalsm = evaldf[evaldf$TRAIN==spp & evaldf$DISTANCE==0 & evaldf$PATIENCE==400,]
  if(nrow(evalsm)>0){
  boxplot(evalsm$avg.acc~evalsm$train.dur,main=spp,#ylim=c(0,1),
          varwidth=T)
  boxplot(evalsm$avg.segment.error.rate~evalsm$train.dur,main=spp,
          varwidth=T)
  }
}
dev.off()

evalagg = aggregate(cbind(evaldf$avg.acc,evaldf$avg.segment.error.rate)~evaldf$TRAIN+evaldf$train.dur+evaldf$TEST+evaldf$PATIENCE+evaldf$DISTANCE,
                    FUN=function(x){mean(x,na.rm=T)})
evalaggsd = aggregate(cbind(evaldf$avg.acc,evaldf$avg.segment.error.rate)~evaldf$TRAIN+evaldf$train.dur+evaldf$TEST+evaldf$PATIENCE+evaldf$DISTANCE,
                    FUN=function(x){sd(x,na.rm=T)})
colnames(evalagg) = c("TRAIN","train.dur","TEST","PATIENCE","DISTANCE","avg.acc.mean","avg.segment.error.rate.mean")
colnames(evalaggsd) = c("TRAIN","train.dur","TEST","PATIENCE","DISTANCE","avg.acc.sd","avg.segment.error.rate.sd")
evalagg = merge(evalagg,evalaggsd,all=T)
evalagg_sm = evalagg[evalagg$PATIENCE==400,]
evalagg_sm = evalagg[evalagg$DISTANCE==0,]
evalagg_sm = evalagg_sm[evalagg_sm$TRAIN==evalagg_sm$TEST,]
evalagg_sm = evalagg_sm[order(evalagg_sm$train.dur,evalagg_sm$TRAIN,evalagg_sm$avg.acc.mean,evalagg_sm$avg.acc.sd),]
evalagg_sm[is.na(evalagg_sm)] = 0

pdf("/Users/kprovost/Documents/Postdoc_Working/Finished_Models/FINAL_EVALS_15Aug2022_means.pdf")
for(i in 1:length(allspp)){
  par(mfrow=c(2,1))
  spp = allspp[i]
  print(spp)
  evalsm = evalagg_sm[evalagg_sm$TRAIN==spp & evalagg_sm$DISTANCE==0,]
  biggest = evalsm[evalsm$train.dur==max(evalsm$train.dur,na.rm=T),]
  if(nrow(evalsm)>0){
    plot(evalsm$train.dur,evalsm$avg.acc.mean,type="p",ylim=c(min(evalsm$avg.acc.mean-evalsm$avg.acc.sd),
                                                              max(evalsm$avg.acc.mean+evalsm$avg.acc.sd)),
         lwd=2,main=spp)
    points(evalsm$train.dur,evalsm$avg.acc.mean+evalsm$avg.acc.sd,type="p",col="grey")
    points(evalsm$train.dur,evalsm$avg.acc.mean-evalsm$avg.acc.sd,type="p",col="grey")
    abline(h=biggest$avg.acc.mean,col="red",lty=3)
    
    plot(evalsm$train.dur,evalsm$avg.segment.error.rate.mean,type="p",
         ylim=c(min(evalsm$avg.segment.error.rate.mean-evalsm$avg.segment.error.rate.sd),
                max(evalsm$avg.segment.error.rate.mean+evalsm$avg.segment.error.rate.sd)),
         lwd=2,main=spp)
    points(evalsm$train.dur,evalsm$avg.segment.error.rate.mean+evalsm$avg.segment.error.rate.sd,type="p",col="grey")
    points(evalsm$train.dur,evalsm$avg.segment.error.rate.mean-evalsm$avg.segment.error.rate.sd,type="p",col="grey")
    abline(h=biggest$avg.segment.error.rate.mean,col="red",lty=3)
    
  }
}
dev.off()

# ## averages: 19 Aug 2022, not including CA, with interpolated and extrapolated results
# stp=c(10,50,100,150,200,231,250,256,257,313,320,500,867,1000,2000,3878)
# avg=c(0.80,0.91,0.92,0.93,0.93,0.93,0.93,0.93,0.93,0.93,0.93,0.94,0.94,0.94,0.94,0.94)
# sdv=c(0.05,0.03,0.03,0.03,0.03,0.03,0.02,0.02,0.02,0.02,0.02,0.02,0.02,0.02,0.02,0.02)
# ## not interpolated -- NA is 0
# avg2=c(0.80,0.91,0.92,0.94,0.94,0.97,0.93,0.92,0.95,0.92,0.94,0.92,0.92,0.93,0.93,0.93)
# sdv2=c(0.05,0.03,0.03,0.02,0.02,0.00,0.02,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)
# plot(stp,avg,ylim=c(0.75,1),type="p",col="black",lwd=2)
# points(stp,avg+sdv,type="p",col="grey")
# points(stp,avg-sdv,type="p",col="grey")
# 
# plot(log10(stp),avg,ylim=c(0.75,1),type="p",col="black",lwd=2)
# points(log10(stp),avg+sdv,type="p",col="grey")
# points(log10(stp),avg-sdv,type="p",col="grey")
# 
# plot(log10(stp),avg2,ylim=c(0.75,1),type="p",col="red",lwd=2)
# points(log10(stp),avg2+sdv2,type="p",col="grey")
# points(log10(stp),avg2-sdv2,type="p",col="grey")
# 
# ## raw based on percent total
# percenttot=c(1.15,3.19,3.91,3.89,4.00,3.13,4.33,15.97,5.77,1.29,31.95,11.53,19.53,2.58,39.06,47.92,19.46,58.59,15.63,31.25,38.91,46.88,20.00,21.65,40.00,58.37,60.00,43.29,64.94,28.84,79.87,78.13,78.13,62.50,80.00,100.00,86.58,100.00,100.00,100.00,100.00,57.67,12.89,100.00,100.00,25.79,51.57,100.00)
# acc=c(0.73,0.74,0.79,0.80,0.84,0.84,0.86,0.88,0.88,0.88,0.89,0.89,0.89,0.90,0.90,0.90,0.91,0.92,0.92,0.93,0.93,0.94,0.94,0.94,0.94,0.95,0.95,0.96,0.96,0.91,0.92,0.92,0.93,0.94,0.95,0.95,0.97,0.97,0.92,0.92,0.95,0.91,0.92,0.92,0.94,0.93,0.93,0.93)
# plot(percenttot,acc)
# abline(h=c(0.92,0.97),col="red")

stp3 = c(10,50,100,150,200,250,300,400,500,1000,2000)
avg3 = c(0.76,0.88,0.89,0.91,0.92,0.92,0.89,0.91,0.91,0.93,0.93)
sdv3 = c(0.12,0.07,0.07,0.07,0.06,0.01,0.04,0.03,0.03,0.01,0.00)
savg3 = c(5.24,3.96,3.47,2.92,3.18,2.81,4.25,4.01,3.68,3.19,3.29)
ssdv3 = c(3.84,2.93,2.38,1.93,2.74,1.22,2.11,1.59,2.06,0.75,0.55)

plot(log10(stp3),avg3,ylim=c(0.64,1),type="p",col="black",lwd=2)
points(log10(stp3),avg3+sdv3,type="p",col="grey")
points(log10(stp3),avg3-sdv3,type="p",col="grey")




accavg=c(0.65411306,0.798352742,0.830025096,0.81485307,0.82577424,0.833963167,0.891628208,0.868419899,0.86809638)
sdvacc=c(0.105960789,0.087301474,0.040860317,0.054942751,0.088176837,0.07136586,0.024290563,0.101955629,0.069376699)
clades=c("1.NEOAVES","2.ORDER","3.SUBORDER","4.PARVORDER","5.SUPERFAMILY","6.FAMILY","7.GENUS","8.SPECIES","MIX")
segavg=c(10.06102445,9.190115913,7.883327218,7.44712544,4.80211473,11.25246195,3.161683008,3.938202871,4.667384682)
segsdv=c(8.293500483,9.997131204,5.636034818,7.090154506,2.607133704,15.23209615,1.699960447,3.174794178,3.333602937)
plot(1:9,accavg,ylim=c(0.5,1))
points(1:9,accavg+sdvacc,col="grey")
points(1:9,accavg-sdvacc,col="grey")
