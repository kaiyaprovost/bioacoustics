eval = "/Users/kprovost/Documents/Postdoc_Working/Finished_Models/Machine_Learning_TweetyNet_Results_25Aug2022.csv"
evaldf = read.csv(eval,header=T)
evaldf = evaldf[order(evaldf$train.dur,evaldf$TRAIN),]
evaldf_full = evaldf[evaldf$train.dur=="F",]

## 12 oct 2022
## species specific linear models for accuacy and SER
summary(lm(evaldf_full$manual.avg.acc[evaldf_full$TEST=="ZL"]
           ~evaldf_full$YEAR[evaldf_full$TEST=="ZL"]))
summary(lm(evaldf_full$corrected.seg.err.abs[evaldf_full$TEST=="ZL"]
           ~evaldf_full$YEAR[evaldf_full$TEST=="ZL"]))

## 23 sept 2022
## post fixing the metrics
summary(lm(evaldf_full$manual.avg.acc~evaldf_full$manual.avg.acc.MEAN))
summary(lm(evaldf_full$manual.precision~evaldf_full$manual.precision.MEAN)) ## not high
summary(lm(evaldf_full$manual.recall~evaldf_full$manual.recall.MEAN))
summary(lm(evaldf_full$manual.fscore~evaldf_full$manual.fscore.MEAN))
summary(lm(evaldf_full$corrected.seg.err.abs~evaldf_full$corrected.seg.err.abs.WRONG.)) ## not high
summary(lm(evaldf_full$corrected.seg.err.abs~evaldf_full$corrected.seg.err.abs.MEAN)) ## not high

plot(evaldf_full$manual.precision,evaldf_full$manual.precision.MEAN)
abline(a=0,b=1)
plot(evaldf_full$corrected.seg.err.abs,evaldf_full$corrected.seg.err.abs.WRONG.)
abline(a=0,b=1)
plot(evaldf_full$corrected.seg.err.abs,evaldf_full$corrected.seg.err.abs.MEAN)
abline(a=0,b=1)

plot(evaldf_full$YEAR,evaldf_full$manual.avg.acc,ylim=c(0,1))
points(evaldf_full$YEAR,evaldf_full$manual.avg.acc.MEAN,col="grey",pch=3)
segments(x0=evaldf_full$YEAR,y0=evaldf_full$manual.avg.acc.MEAN,
         y1=evaldf_full$manual.avg.acc.MEAN+evaldf_full$manual.avg.acc.SD,
         col="grey")
segments(x0=evaldf_full$YEAR,y0=evaldf_full$manual.avg.acc.MEAN,
         y1=evaldf_full$manual.avg.acc.MEAN-evaldf_full$manual.avg.acc.SD,
         col="grey")
points(evaldf_full$YEAR[evaldf_full$TRAIN=="9SPP"],
       evaldf_full$manual.avg.acc[evaldf_full$TRAIN=="9SPP"],col="red")
points(evaldf_full$YEAR[evaldf_full$TRAIN=="9SPP"],
       evaldf_full$manual.avg.acc.MEAN[evaldf_full$TRAIN=="9SPP"],col="darkred",pch=3)

evaldfx = evaldf[,c(55,62:66)]
evaldfx=unique(evaldfx)
summary(lm(evaldfx$Training.Time~evaldfx$Train.M.F.Complexity))
summary(lm(evaldfx$Training.Time~evaldfx$Train.M.F.InVar))
summary(lm(evaldfx$Training.Time~evaldfx$Train.M.F.SpVar))
summary(lm(evaldfx$Training.Time~evaldfx$Train.Hypervolume.Complexity.Mean))
summary(lm(evaldfx$Training.Time~evaldfx$Train.Hypervolume.Complexity.SD))

## t-tests? 
for(spp in unique(evaldf$TRAIN)){
  print(spp)
  temp = evaldf[evaldf$TRAIN==spp,]
  temp = temp[temp$TEST==spp,]
  fullval = temp$avg.acc[temp$train.dur=="F"]
  temp=temp[temp$train.dur!="F",]
  for(dur in unique(temp$train.dur)){
    print(paste(dur,spp))
    dur_temp = temp[temp$train.dur==dur,"avg.acc"]
    dur_temp_cor = dur_temp-fullval
    print(t.test(dur_temp_cor))
  }
}
for(spp in unique(evaldf$TRAIN)){
  temp = evaldf[evaldf$TRAIN==spp,]
  temp = temp[temp$TEST==spp,]
  fullval = temp$avg.acc[temp$train.dur=="F"]
  temp=temp[temp$train.dur!="F",]
  plot(evaldf$train.dur,evaldf$avg.acc,type="n")
  abline(h=fullval,col="blue")
  points(temp$train.dur,temp$avg.acc)
  aggM = aggregate(temp$avg.acc~as.numeric(temp$train.dur),FUN=function(x){mean(x,na.rm=T)})
  aggS = aggregate(temp$avg.acc~as.numeric(temp$train.dur),FUN=function(x){sd(x,na.rm=T)})
  points(aggM,col="red",pch=3,type="l")
  points(aggM[,1],aggM[,2]+aggS[,2],col="grey",pch=3,type="l")
  points(aggM[,1],aggM[,2]-aggS[,2],col="grey",pch=3,type="l")
  mod=lm(temp$avg.acc~log10(as.numeric(temp$train.dur)))
  points(as.numeric(temp$train.dur),predict(mod),col="green")
}


## 19 Sept 2022
## the means per test/train species wrt complexity
testagg=aggregate(cbind(as.numeric(evaldf_full$avg.acc),
                        as.numeric(evaldf_full$corrected.seg.err.abs),
                        as.numeric(evaldf_full$manual.precision),
                        as.numeric(evaldf_full$manual.recall),
                        as.numeric(evaldf_full$manual.fscore),
                        as.numeric(evaldf_full$Test.Spp.Training.Size),
                        as.numeric(evaldf_full$Test.Spp.Val.Size),
                        as.numeric(evaldf_full$Test.Spp.Test.Size))~evaldf_full$TEST,
                  FUN = function(x){mean(x,na.rm=T)})
colnames(testagg) = c("TEST","acc","seg","prec","rec","f","trainsize","valsize","testsize")
trainagg=aggregate(cbind(evaldf_full$avg.acc,
                         evaldf_full$corrected.seg.err.abs,
                         evaldf_full$manual.precision,
                         evaldf_full$manual.recall,
                         evaldf_full$manual.fscore,
                         evaldf_full$Training.Spp.Training.Size,
                         evaldf_full$Training.Spp.Val.Size,
                         evaldf_full$Training.Spp.Test.Size)~evaldf_full$TRAIN,
                   FUN = function(x){mean(x,na.rm=T)})
colnames(trainagg) = c("TRAIN","acc","seg","prec","rec","f","trainsize","valsize","testsize")
for(i in 2:6){
  for(j in 7:9) {
    print(paste("TEST",colnames(testagg)[i],colnames(testagg)[j]))
    print(summary(lm(testagg[,i]~testagg[,j])))
    print(paste("TRAIN",colnames(trainagg)[i],colnames(trainagg)[j]))
    print(summary(lm(trainagg[,i]~trainagg[,j])))
  }
}

## anovas per species
evaldf_full_ss = evaldf_full[evaldf_full$TRAIN!="9SPP",]
evaldf_full_ss = evaldf_full_ss[evaldf_full_ss$TRAIN!="MF",]
evaldf_full_ss = evaldf_full_ss[evaldf_full_ss$TEST!="MF",]
evaldf_full_ss = evaldf_full_ss[evaldf_full_ss$TEST!="9SPP",]
#summary(aov(evaldf_full$manual.recall~evaldf_full$TEST))
#TukeyHSD(aov(evaldf_full$manual.recall~evaldf_full$TEST))
summary(aov(evaldf_full_ss$avg.acc~evaldf_full_ss$TEST))
TukeyHSD(aov(evaldf_full_ss$avg.acc~evaldf_full_ss$TEST))
boxplot(evaldf_full_ss$avg.acc~evaldf_full_ss$TEST)
#summary(aov(evaldf_full$manual.recall~evaldf_full$TRAIN))
#TukeyHSD(aov(evaldf_full$manual.recall~evaldf_full$TRAIN))
summary(aov(evaldf_full_ss$avg.acc~evaldf_full_ss$TRAIN))
TukeyHSD(aov(evaldf_full_ss$avg.acc~evaldf_full_ss$TRAIN))
boxplot(evaldf_full_ss$avg.acc~evaldf_full_ss$TRAIN)

summary(lm(log10(trainagg$testsize)~
             trainagg$acc))
summary(lm(trainagg$testsize[testagg$TEST!="9SPP"]~
             trainagg$acc[testagg$TEST!="9SPP"]))


## making heatmaps
newdf=as.data.frame(matrix(nrow = 11,ncol=11))
colnames(newdf) = c("CA","EV","MT","VA","PC","ZL","MF","PA","CC","CS","9SPP")
rownames(newdf) = colnames(newdf)
for(col_spp in colnames(newdf)){
  for(row_spp in rownames(newdf)){
    evaldf_temp = evaldf[evaldf$TRAIN==row_spp & evaldf$TEST==col_spp & evaldf$train.dur=="F",]
    myvalue=log10(evaldf_temp$corrected.seg.err.abs)
    myvalue=ifelse(is.infinite(myvalue),0,myvalue)
    newdf[row_spp,col_spp] = as.numeric(myvalue)
  }
}
png("~/heatmap_train_test_correctedseg_abs_log.png")
corrplot::corrplot(as.matrix(newdf),is.corr=F,method="color")
dev.off()
## comparing manual and automatic avvuracy
plot(evaldf$avg.acc,evaldf$manual.avg.acc,
     ylab="Corrected Accuracy",
     xlab="Uncorrected Accuracy")
abline(a=0,b=1)
mod=lm(evaldf$manual.avg.acc~evaldf$avg.acc)
abline(mod,col="red")
summary(mod)

plot(evaldf$avg.acc,evaldf$manual.avg.acc-evaldf$avg.acc)
abline(h=0)
mod=lm(evaldf$manual.avg.acc-evaldf$avg.acc~evaldf$avg.acc)
abline(mod,col="red")
summary(mod)

plot(evaldf$YEAR,evaldf$manual.precision)
mod=lm(evaldf$manual.precision~evaldf$YEAR)
abline(mod,col="red")
summary(mod) ## p=2.51e-8, adjr2=0.224,f=35.65,df=119

plot(evaldf$YEAR,evaldf$manual.recall)
mod=lm(evaldf$manual.recall~evaldf$YEAR)
abline(mod,col="red")
summary(mod) ## p=9.25e-10, adjr2=0.265,f=44.26,df=119

plot(evaldf$YEAR,evaldf$manual.fscore)
mod=lm(evaldf$manual.fscore~evaldf$YEAR)
abline(mod,col="red")
summary(mod) ## p=7.7e-14, adjr2=0.3707,f=71.68,df=119

plot(evaldf$manual.precision,evaldf$manual.recall)
summary(lm(evaldf$manual.avg.acc~evaldf$manual.fscore))
summary(lm(evaldf$manual.avg.acc~evaldf$manual.precision))
summary(lm(evaldf$manual.avg.acc~evaldf$manual.recall))
summary(lm(evaldf$manual.fscore~evaldf$manual.precision))
summary(lm(evaldf$manual.fscore~evaldf$manual.recall))
summary(lm(evaldf$manual.precision~evaldf$manual.recall))

summary(lm(evaldf$corrected.seg.err.abs~evaldf$manual.fscore))
summary(lm(evaldf$corrected.seg.err.abs~evaldf$manual.precision))
summary(lm(evaldf$corrected.seg.err.abs~evaldf$manual.recall))

## 16 Sept 2022
## models
#Hypervolume ~ Complexity
evaldf_comp = evaldf[,c("TRAIN","Train.M.F.Complexity",
                        "Train.M.F.InVar","Train.M.F.SpVar",
                        "Train.Hypervolume.Complexity.Mean","Train.Hypervolume.Complexity.SD")]
evaldf_comp = unique(evaldf_comp)
plot(evaldf_comp[,2:6])
summary(lm(evaldf_comp$Train.M.F.Complexity~evaldf_comp$Train.M.F.InVar))
summary(lm(evaldf_comp$Train.M.F.Complexity~evaldf_comp$Train.M.F.SpVar))
summary(lm(evaldf_comp$Train.M.F.Complexity~evaldf_comp$Train.Hypervolume.Complexity.Mean))
summary(lm(evaldf_comp$Train.M.F.Complexity~evaldf_comp$Train.Hypervolume.Complexity.SD))
summary(lm(evaldf_comp$Train.M.F.InVar~evaldf_comp$Train.M.F.SpVar))
summary(lm(evaldf_comp$Train.M.F.InVar~evaldf_comp$Train.Hypervolume.Complexity.Mean))
summary(lm(evaldf_comp$Train.M.F.InVar~evaldf_comp$Train.Hypervolume.Complexity.SD))
summary(lm(evaldf_comp$Train.M.F.SpVar~evaldf_comp$Train.Hypervolume.Complexity.Mean))
summary(lm(evaldf_comp$Train.M.F.SpVar~evaldf_comp$Train.Hypervolume.Complexity.SD))
summary(lm(evaldf_comp$Train.Hypervolume.Complexity.Mean~evaldf_comp$Train.Hypervolume.Complexity.SD))

evaldf_acc = evaldf[evaldf$train.dur=="F",c("TRAIN","TEST","YEAR","avg.acc",
                                            "corrected.seg.err.abs","corrected.seg.err.neg")]
summary(lm(evaldf_acc$avg.acc~evaldf_acc$YEAR))
summary(lm(evaldf_acc$corrected.seg.err.abs~evaldf_acc$YEAR))
summary(lm(evaldf_acc$corrected.seg.err.neg~evaldf_acc$YEAR))

evaldf_compacc = evaldf[evaldf$train.dur=="F",]
evaldf_compacc = evaldf_compacc[,c("TRAIN","TEST","avg.acc",
                                   "corrected.seg.err.abs","corrected.seg.err.neg",
                                   "Train.M.F.Complexity",
                                   "Train.M.F.InVar","Train.M.F.SpVar",
                                   "Train.Hypervolume.Complexity.Mean","Train.Hypervolume.Complexity.SD",
                                   "Test.M.F.Complexity",
                                   "Test.M.F.InVar","Test.M.F.SpVar",
                                   "Test.Hypervolume.Complexity.Mean","Test.Hypervolume.Complexity.SD")]
evaldf_compacc_tr = evaldf_compacc[,c("TRAIN","avg.acc",
                                      "corrected.seg.err.abs","corrected.seg.err.neg",
                                      "Train.M.F.Complexity",
                                      "Train.M.F.InVar","Train.M.F.SpVar",
                                      "Train.Hypervolume.Complexity.Mean","Train.Hypervolume.Complexity.SD")]


## 14 Sept 2022
## looking at corrected segment errors
plot(evaldf$avg.acc[evaldf$train.dur=="F"],
     evaldf$corrected.seg.err.abs[evaldf$train.dur=="F"])

plot(evaldf$YEAR[evaldf$train.dur=="F"],
     evaldf$corrected.seg.err.abs[evaldf$train.dur=="F"])

summary(lm(evaldf$corrected.seg.err.abs[evaldf$train.dur=="F"]~
             evaldf$avg.acc[evaldf$train.dur=="F"]))


for(spp in spp)
  x=t.test(evaldf$avg.acc[evaldf$TRAIN=="CA" & evaldf$YEAR==0 & evaldf$train.dur==10]-
             evaldf$avg.acc[evaldf$TRAIN=="CA" & evaldf$YEAR==0 & evaldf$train.dur=="F"])



mf_complex=c(-1.7980465,-1.08634505,-0.69541879,-0.1061745,0.66587066,1.54087147,1.58605661)
hy_complex=c(0.000900418,0.000176939,0.146839949,0.030421424,0.410482847,0.053789883,0.022949805)
hy_complex_sd=c(0.004351538,0.000256558,0.227304337,0.04027349,0.410128509,0.147318145,0.038713402)
plot(mf_complex,hy_complex)
mod=lm(hy_complex~mf_complex)
summary(mod)
plot(mf_complex,hy_complex_sd)
plot(hy_complex,hy_complex_sd)


##### LEARNCURVE #####

#plot(log10(evaldf$train.dur),evaldf$avg.acc,ylim=c(0,1))
allspp = sort(unique(evaldf$TRAIN))
#for(i in 1:length(allspp)){
#  spp = allspp[i]
#  evalsm = evaldf[evaldf$TRAIN==spp,]
#  points(log10(evalsm$train.dur),evalsm$avg.acc,col=i,pch=i,type="p")
#}
#boxplot(evaldf$avg.acc~evaldf$train.dur)

trainagg = aggregate(evaldf$avg.acc~evaldf$train.dur+evaldf$TRAIN,FUN=function(x){mean(x,na.rm=T)})
colnames(trainagg) = c("train.dur","TRAIN","avg.acc")
trainagg = trainagg[order(trainagg$train.dur,trainagg$TRAIN,trainagg$avg.acc),]
trainagg2 = aggregate(evaldf$avg.acc~evaldf$train.dur+evaldf$TRAIN,FUN=function(x){sd(x,na.rm=T)})
colnames(trainagg2) = c("train.dur","TRAIN","avg.acc.sd")
trainagg=merge(trainagg,trainagg2)
trainagg = trainagg[trainagg$train.dur!="F",]

plot(log10(as.numeric(trainagg$train.dur[trainagg$train.dur!="F"])),trainagg$avg.acc[trainagg$train.dur!="F"],
     xlab="Log Training Duration",ylab="Average Accuracy",col=as.numeric(as.factor(trainagg$TRAIN)),
     pch=as.numeric(as.factor(trainagg$TRAIN)),type="p")
abline(lm(as.numeric(trainagg$avg.acc[trainagg$TRAIN=="CA"])~log10(as.numeric(trainagg$train.dur[trainagg$TRAIN=="CA"]))),col=1,pch=1,lty=1)
abline(lm(as.numeric(trainagg$avg.acc[trainagg$TRAIN=="CC"])~log10(as.numeric(trainagg$train.dur[trainagg$TRAIN=="CC"]))),col=2,pch=2,lty=2)
abline(lm(as.numeric(trainagg$avg.acc[trainagg$TRAIN=="CS"])~log10(as.numeric(trainagg$train.dur[trainagg$TRAIN=="CS"]))),col=3,pch=3,lty=3)
abline(lm(as.numeric(trainagg$avg.acc[trainagg$TRAIN=="EV"])~log10(as.numeric(trainagg$train.dur[trainagg$TRAIN=="EV"]))),col=4,pch=4,lty=4)
abline(lm(as.numeric(trainagg$avg.acc[trainagg$TRAIN=="MT"])~log10(as.numeric(trainagg$train.dur[trainagg$TRAIN=="MT"]))),col=5,pch=5,lty=5)
abline(lm(as.numeric(trainagg$avg.acc[trainagg$TRAIN=="PA"])~log10(as.numeric(trainagg$train.dur[trainagg$TRAIN=="PA"]))),col=6,pch=6,lty=6)
abline(lm(as.numeric(trainagg$avg.acc[trainagg$TRAIN=="PC"])~log10(as.numeric(trainagg$train.dur[trainagg$TRAIN=="PC"]))),col=7,pch=7,lty=7)
abline(lm(as.numeric(trainagg$avg.acc[trainagg$TRAIN=="VA"])~log10(as.numeric(trainagg$train.dur[trainagg$TRAIN=="VA"]))),col=8,pch=8,lty=8)
abline(lm(as.numeric(trainagg$avg.acc[trainagg$TRAIN=="ZL"])~log10(as.numeric(trainagg$train.dur[trainagg$TRAIN=="ZL"]))),col=9,pch=9,lty=9)

colors=c(rgb(1,0,0,0.3),
         rgb(0,1,0,0.3),
         rgb(0,0,1,0.3),
         rgb(1,1,0,0.3),
         rgb(1,0,1,0.3),
         rgb(0,1,1,0.3),
         rgb(1,1,1,0.3),
         rgb(0,0,0,0.3),
         rgb(0.5,0.5,0.5,0.3),
         rgb(0.3,0.3,0.3,0.3))
png("/Users/kprovost/Documents/Postdoc_Working/Finished_Models/FINAL_EVALS_28Sep2022_polygons.png",
    width=8,height=8,units="in",res=600)
par(mfrow=c(3,3),
    mar=c(4,4,0,0))
#plot(0,ylim=c(0.6,1),xlim=c(1,3.5),type="n",ylab="Average Accuracy",xlab="Log Training Duration")
for(spp_i in 1:length(sort(unique(trainagg$TRAIN)))){
  spp=sort(unique(trainagg$TRAIN))[spp_i]
  print(spp)
  plot(0,ylim=c(0.6,1),xlim=c(1,3.5),type="n",ylab="Average Accuracy",xlab="Log Training Duration")
  small=trainagg[trainagg$TRAIN==spp,]
  small=small[small$train.dur!="F",]
  small$train.dur=as.numeric(small$train.dur)
  small=small[order(small$train.dur),]
  polygon(x=c(log10(small$train.dur),rev(log10(small$train.dur))),
          y=c(small$avg.acc+small$avg.acc.sd,rev(small$avg.acc-small$avg.acc.sd)),
          col=colors[spp_i])
  points(log10(small$train.dur),small$avg.acc)
  for(i in 2:length(sort(unique(small$train.dur)))) {
    #print(i)
    dur0=sort(unique(small$train.dur))[i]
    dur1=sort(unique(small$train.dur))[i-1]
    segments(x0=log10(dur0),
             x1=log10(dur1),
             y0=small$avg.acc[small$train.dur==dur0],
             y1=small$avg.acc[small$train.dur==dur1])
  }
  legend(x="topright",legend=spp,fill=colors[spp_i])
  
  
}
dev.off()

segagg = aggregate(evaldf$avg.segment.error.rate~evaldf$train.dur+evaldf$TRAIN,FUN=function(x){mean(x,na.rm=T)})
colnames(segagg) = c("train.dur","TRAIN","avg.segment.error.rate")
segagg = segagg[order(segagg$train.dur,segagg$TRAIN,segagg$avg.segment.error.rate),]
segagg2 = aggregate(evaldf$avg.segment.error.rate~evaldf$train.dur+evaldf$TRAIN,FUN=function(x){sd(x,na.rm=T)})
colnames(segagg2) = c("train.dur","TRAIN","avg.segment.error.rate.sd")
segagg=merge(segagg,segagg2)
segagg = segagg[segagg$train.dur!="F",]

png("/Users/kprovost/Documents/Postdoc_Working/Finished_Models/FINAL_EVALS_28Sep2022_polygons_seg.png",
    width=8,height=8,units="in",res=600)
par(mfrow=c(3,3),
    mar=c(4,4,0,0))
#plot(0,ylim=c(0.6,1),xlim=c(1,3.5),type="n",ylab="Average Accuracy",xlab="Log Training Duration")
for(spp_i in 1:length(sort(unique(segagg$TRAIN)))){
  spp=sort(unique(segagg$TRAIN))[spp_i]
  plot(0,ylim=c(-3,13),xlim=c(1,3.5),type="n",ylab="Average Segment Accuracy",xlab="Log Training Duration")
  small=segagg[segagg$TRAIN==spp,]
  small=small[small$train.dur!="F",]
  small$train.dur=as.numeric(small$train.dur)
  small=small[order(small$train.dur),]
  polygon(x=c(log10(small$train.dur),rev(log10(small$train.dur))),
          y=c(small$avg.segment.error.rate+small$avg.segment.error.rate.sd,rev(small$avg.segment.error.rate-small$avg.segment.error.rate.sd)),
          col=colors[spp_i])
  points(log10(small$train.dur),small$avg.segment.error.rate)
  for(i in 2:length(sort(unique(small$train.dur)))) {
    #print(i)
    dur0=sort(unique(small$train.dur))[i]
    dur1=sort(unique(small$train.dur))[i-1]
    segments(x0=log10(dur0),
             x1=log10(dur1),
             y0=small$avg.segment.error.rate[small$train.dur==dur0],
             y1=small$avg.segment.error.rate[small$train.dur==dur1])
  }
  legend(x="topright",legend=spp,fill=colors[spp_i])
  
  
}
dev.off()


pdf("/Users/kprovost/Documents/Postdoc_Working/Finished_Models/FINAL_EVALS_28Sep2022_boxplots.pdf")
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

pdf("/Users/kprovost/Documents/Postdoc_Working/Finished_Models/FINAL_EVALS_28Sep2022_means.pdf")
for(i in 1:length(allspp)){
  par(mfrow=c(2,1))
  spp = allspp[i]
  print(spp)
  evalsm = evalagg_sm[evalagg_sm$TRAIN==spp & evalagg_sm$DISTANCE==0,]
  evalsm = evalsm[evalsm$train.dur!="F",]
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


##### ACC OVER CLADE #####

cladeaccavg=aggregate(evaldf$avg.acc~evaldf$CLADE+evaldf$train.dur,FUN=function(x){mean(x,na.rm=T)})
cladeaccsdv=aggregate(evaldf$avg.acc~evaldf$CLADE+evaldf$train.dur,FUN=function(x){sd(x,na.rm=T)})
cladeaccavg=cladeaccavg[cladeaccavg[,2]=="F",c(1,3)]
cladeaccsdv=cladeaccsdv[cladeaccsdv[,2]=="F",c(1,3)]
plot(cladeaccavg[,2],ylim=c(0.5,1),lwd=2,col="black",type="b")
points(cladeaccavg[,2]+cladeaccsdv[,2],col="grey",type="b")
points(cladeaccavg[,2]-cladeaccsdv[,2],col="grey",type="b")

png("~/clade_vs_acc_28Sep2022.png")
b=barplot(cladeaccavg[,2],names=cladeaccavg[,1],las=2,ylim=c(0,1))
segments(x0=b,y0=cladeaccavg[,2]+cladeaccsdv[,2],y1=cladeaccavg[,2]-cladeaccsdv[,2])
abline(h=cladeaccavg[8,2],col="red")
dev.off()

png("~/clade_vs_seg_28Sep2022.png")
b=barplot(cladesegavg[,2],names=cladesegavg[,1],las=2,ylim=c(0,25))
segments(x0=b,y0=cladesegavg[,2]+cladesegsdv[,2],y1=cladesegavg[,2]-cladesegsdv[,2])
abline(h=cladesegavg[8,2],col="red")
dev.off()

cladesegavg=aggregate(evaldf$corrected.seg.err.abs~evaldf$CLADE+evaldf$train.dur,FUN=function(x){mean(x,na.rm=T)})
cladesegsdv=aggregate(evaldf$corrected.seg.err.abs~evaldf$CLADE+evaldf$train.dur,FUN=function(x){sd(x,na.rm=T)})
cladesegavg=cladesegavg[cladesegavg[,2]=="F",c(1,3)]
cladesegsdv=cladesegsdv[cladesegsdv[,2]=="F",c(1,3)]
plot(cladesegavg[,2],lwd=2,col="black",type="b",ylim=c(-10,300))
points(cladesegavg[,2]+cladesegsdv[,2],col="grey",type="b")
points(cladesegavg[,2]-cladesegsdv[,2],col="grey",type="b")
abline(h=0)

png("~/clade_vs_seg_ABS_28Sep2022.png")
b=barplot(cladesegavg[,2],names=cladesegavg[,1],las=2,ylim=c(-10,300))
segments(x0=b,y0=cladesegavg[,2]+cladesegsdv[,2],y1=cladesegavg[,2]-cladesegsdv[,2])
abline(h=cladesegavg[8,2],col="red")
dev.off()

cladesegavg=aggregate(evaldf$corrected.seg.err.neg~evaldf$CLADE+evaldf$train.dur,FUN=function(x){mean(x,na.rm=T)})
cladesegsdv=aggregate(evaldf$corrected.seg.err.neg~evaldf$CLADE+evaldf$train.dur,FUN=function(x){sd(x,na.rm=T)})
cladesegavg=cladesegavg[cladesegavg[,2]=="F",c(1,3)]
cladesegsdv=cladesegsdv[cladesegsdv[,2]=="F",c(1,3)]
plot(cladesegavg[,2],lwd=2,col="black",type="b",ylim=c(-50,300))
points(cladesegavg[,2]+cladesegsdv[,2],col="grey",type="b")
points(cladesegavg[,2]-cladesegsdv[,2],col="grey",type="b")
abline(h=0)

cladesegavg=aggregate(evaldf$avg.segment.error.rate~evaldf$CLADE+evaldf$train.dur,FUN=function(x){mean(x,na.rm=T)})
cladesegsdv=aggregate(evaldf$avg.segment.error.rate~evaldf$CLADE+evaldf$train.dur,FUN=function(x){sd(x,na.rm=T)})
cladesegavg=cladesegavg[cladesegavg[,2]=="F",c(1,3)]
cladesegsdv=cladesegsdv[cladesegsdv[,2]=="F",c(1,3)]
plot(cladesegavg[,2],lwd=2,col="black",type="b",ylim=c(-5,25))
points(cladesegavg[,2]+cladesegsdv[,2],col="grey",type="b")
points(cladesegavg[,2]-cladesegsdv[,2],col="grey",type="b")
abline(h=0)


png("~/clade_vs_seg_NEG_28Sep2022.png")
b=barplot(cladesegavg[,2],names=cladesegavg[,1],las=2,ylim=c(-50,300))
segments(x0=b,y0=cladesegavg[,2]+cladesegsdv[,2],y1=cladesegavg[,2]-cladesegsdv[,2])
abline(h=cladesegavg[8,2],col="red")
dev.off()

png("~/est_div_time_vs_acc_28Sep2022.png")
plot(evaldf$YEAR[evaldf$train.dur=="F"],
     evaldf$avg.acc[evaldf$train.dur=="F"],
     ylab="Average Accuracy",xlab="Estimated Divergence (Mya)")
points(evaldf$YEAR[evaldf$train.dur=="F" & evaldf$TRAIN=="9SPP"],
       evaldf$avg.acc[evaldf$train.dur=="F" & evaldf$TRAIN=="9SPP"],
       col="red",cex=1.5)
points(evaldf$YEAR[evaldf$train.dur=="F" & evaldf$TEST=="9SPP"],
       evaldf$avg.acc[evaldf$train.dur=="F" & evaldf$TEST=="9SPP"],
       col="blue",cex=2,pch=0)
mod = lm(evaldf$avg.acc[evaldf$train.dur=="F"]~evaldf$YEAR[evaldf$train.dur=="F"])
abline(mod,col="red")
summary(mod)
dev.off()

png("~/est_div_time_vs_seg_28Sep2022.png")
plot(evaldf$YEAR[evaldf$train.dur=="F"],
     evaldf$avg.segment.error.rate[evaldf$train.dur=="F"],
     ylab="Average Seg Err rate",xlab="Estimated Divergence (Mya)")
points(evaldf$YEAR[evaldf$train.dur=="F" & evaldf$TRAIN=="9SPP"],
       evaldf$avg.segment.error.rate[evaldf$train.dur=="F" & evaldf$TRAIN=="9SPP"],
       col="red",cex=1.5)
points(evaldf$YEAR[evaldf$train.dur=="F" & evaldf$TEST=="9SPP"],
       evaldf$avg.segment.error.rate[evaldf$train.dur=="F" & evaldf$TEST=="9SPP"],
       col="blue",cex=2,pch=0)
mod = lm(evaldf$avg.segment.error.rate[evaldf$train.dur=="F"]~evaldf$YEAR[evaldf$train.dur=="F"])
abline(h=0)
abline(mod,col="red")
summary(mod)
dev.off()

png("~/est_div_time_vs_seg_ABS_28Sep2022.png")
plot(evaldf$YEAR[evaldf$train.dur=="F"],
     evaldf$corrected.seg.err.abs[evaldf$train.dur=="F"],
     ylab="Average Seg Err rate (Absolute, Corrected)",xlab="Estimated Divergence (Mya)")
points(evaldf$YEAR[evaldf$train.dur=="F" & evaldf$TRAIN=="9SPP"],
       evaldf$corrected.seg.err.abs[evaldf$train.dur=="F" & evaldf$TRAIN=="9SPP"],
       col="red",cex=1.5)
points(evaldf$YEAR[evaldf$train.dur=="F" & evaldf$TEST=="9SPP"],
       evaldf$corrected.seg.err.abs[evaldf$train.dur=="F" & evaldf$TEST=="9SPP"],
       col="blue",cex=2,pch=0)
mod = lm(evaldf$corrected.seg.err.abs[evaldf$train.dur=="F"]~evaldf$YEAR[evaldf$train.dur=="F"])
abline(h=0)
abline(mod,col="red")
summary(mod) ## p 1.7e-5 df 119 adjr2 0.1377 f 20.16
dev.off()

## remove MT and CA because have highest rates
evaldf2 = evaldf[evaldf$TRAIN!="CA",]
evaldf2 = evaldf2[evaldf2$TRAIN!="MT",]
plot(evaldf2$YEAR[evaldf2$train.dur=="F"],
     evaldf2$corrected.seg.err.abs[evaldf2$train.dur=="F"],
     ylab="Average Seg Err rate (Absolute, Corrected)",xlab="Estimated Divergence (Mya)")
points(evaldf2$YEAR[evaldf2$train.dur=="F" & evaldf2$TRAIN=="9SPP"],
       evaldf2$corrected.seg.err.abs[evaldf2$train.dur=="F" & evaldf2$TRAIN=="9SPP"],
       col="red",cex=1.5)
points(evaldf2$YEAR[evaldf2$train.dur=="F" & evaldf2$TEST=="9SPP"],
       evaldf2$corrected.seg.err.abs[evaldf2$train.dur=="F" & evaldf2$TEST=="9SPP"],
       col="blue",cex=2,pch=0)
mod = lm(evaldf2$corrected.seg.err.abs[evaldf2$train.dur=="F"]~evaldf2$YEAR[evaldf2$train.dur=="F"])
abline(h=0)
abline(mod,col="red")
summary(mod) ## p 6.1e-6 df 97 adjr2 0.1829 f 22.94

png("~/est_div_time_vs_seg_NEG_28Sep2022.png")
plot(evaldf$YEAR[evaldf$train.dur=="F"],
     evaldf$corrected.seg.err.neg[evaldf$train.dur=="F"],
     ylab="Average Seg Err rate (Corrected)",xlab="Estimated Divergence (Mya)")
points(evaldf$YEAR[evaldf$train.dur=="F" & evaldf$TRAIN=="9SPP"],
       evaldf$corrected.seg.err.neg[evaldf$train.dur=="F" & evaldf$TRAIN=="9SPP"],
       col="red",cex=1.5)
points(evaldf$YEAR[evaldf$train.dur=="F" & evaldf$TEST=="9SPP"],
       evaldf$corrected.seg.err.neg[evaldf$train.dur=="F" & evaldf$TEST=="9SPP"],
       col="blue",cex=2,pch=0)
mod = lm(evaldf$corrected.seg.err.neg[evaldf$train.dur=="F"]~evaldf$YEAR[evaldf$train.dur=="F"])
abline(h=0)
abline(mod,col="red")
summary(mod) ## p 0.00511 df 119 adjr2 0.05615 f 8.139
dev.off()

## model of best fit by species
png("~/per_species_ml_model_of_best_fit.png",
    width=8,height=10,units="in",res=600)
par(mfrow=c(4,3),
    mar=c(4,4,0,0))
for(spp in sort(unique(evaldf$TEST))) {
  print(spp)
  evaldf_sm = evaldf[evaldf$TEST==spp,]
  evaldf_sm=evaldf_sm[evaldf_sm$train.dur=="F",]
  plot(evaldf$YEAR,evaldf$avg.acc,type="n",main=spp)
  points(evaldf_sm$YEAR,evaldf_sm$avg.acc)
  points(evaldf_sm$YEAR[evaldf_sm$TRAIN=="9SPP"],
         evaldf_sm$avg.acc[evaldf_sm$TRAIN=="9SPP"],
         col="red",cex=1.5)
  if(spp=="9SPP"){
    points(evaldf_sm$YEAR,evaldf_sm$avg.acc,col="blue",
           cex=2,pch=0)
  }
  mod=lm(evaldf_sm$avg.acc~evaldf_sm$YEAR)
  abline(mod,col="red")
  print(summary(mod))
}
dev.off()

png("~/per_species_ml_model_of_best_fit_seg.png",
    width=8,height=10,units="in",res=600)
par(mfrow=c(4,3),
    mar=c(4,4,0,0))
for(spp in sort(unique(evaldf$TEST))) {
  print(spp)
  evaldf_sm = evaldf[evaldf$TEST==spp,]
  evaldf_sm=evaldf_sm[evaldf_sm$train.dur=="F",]
  plot(evaldf$YEAR,evaldf$corrected.seg.err.abs,type="n",main=spp)
  points(evaldf_sm$YEAR,evaldf_sm$corrected.seg.err.abs)
  points(evaldf_sm$YEAR[evaldf_sm$TRAIN=="9SPP"],
         evaldf_sm$corrected.seg.err.abs[evaldf_sm$TRAIN=="9SPP"],
         col="red",cex=1.5)
  if(spp=="9SPP"){
    points(evaldf_sm$YEAR,evaldf_sm$corrected.seg.err.abs,col="blue",
           cex=2,pch=0)
  }
  mod=lm(evaldf_sm$corrected.seg.err.abs~evaldf_sm$YEAR)
  abline(mod,col="red",lty=ifelse(summary(mod)$coefficients[2,4]<=(0.05/50),1,
                                  ifelse(summary(mod)$coefficients[2,4]<=(0.05/10),2,
                                         ifelse(summary(mod)$coefficients[2,4]<=(0.05/1),3,4))))
  print(summary(mod))
}
dev.off()

## model of best fit by species on full dataset
colors=RColorBrewer::brewer.pal(12,"Paired")[c(1:10,12)]
list_lty=c(1,2,3,1,2,3,1,2,3,1,2,3)
list_lwd=c(1,1,1,2,2,2,3,3,3,4,4,4)

png("~/per_species_ml_model_of_best_fit_big.png",
    width=8,height=8,units="in",res=600)
par(mfrow=c(1,1),
    mar=c(4,4,0,0))
plot(evaldf$YEAR[evaldf$train.dur=="F"],
     evaldf$avg.acc[evaldf$train.dur=="F"],
     type="p")
points(evaldf$YEAR[evaldf$train.dur=="F" & evaldf$TEST=="9SPP"],
       evaldf$avg.acc[evaldf$train.dur=="F" & evaldf$TEST=="9SPP"],
       type="p",
       col="blue",
       pch=0,cex=2)
points(evaldf$YEAR[evaldf$train.dur=="F" & evaldf$TRAIN=="9SPP"],
       evaldf$avg.acc[evaldf$train.dur=="F" & evaldf$TRAIN=="9SPP"],
       type="p",
       col="red",
       pch=1,cex=1.5)
for(i in 1:length(unique(evaldf$TEST))) {
  spp=sort(unique(evaldf$TEST))[i]
  print(spp)
  evaldf_sm = evaldf[evaldf$TEST==spp,]
  evaldf_sm=evaldf_sm[evaldf_sm$train.dur=="F",]
  mod=lm(evaldf_sm$avg.acc~evaldf_sm$YEAR,)
  abline(mod,col=colors[i],
         lwd=list_lwd[i],
         lty=list_lty[i])
  print(summary(mod))
}
legend("bottomleft",legend=sort(unique(evaldf$TEST)),
       col=colors,lty=list_lty[1:11],lwd=list_lwd[1:11],
       cex=2,ncol = 2,bty="n")
dev.off()


boxplot(evaldf$corrected.seg.err.abs~evaldf$TRAIN)
mod=aov(evaldf$corrected.seg.err.abs~evaldf$TRAIN)
summary(mod) ## p = 5e-15, df = 110, f = 13.3
TukeyHSD(mod)

