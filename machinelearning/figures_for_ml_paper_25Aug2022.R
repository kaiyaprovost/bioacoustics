eval = "/Users/kprovost/Documents/Postdoc_Working/Finished_Models/Machine_Learning_TweetyNet_Results_25Aug2022.csv"
evaldf = read.csv(eval,header=T)
evaldf = evaldf[order(evaldf$train.dur,evaldf$TRAIN),]

evaldf = evaldf[evaldf$train.dur=="F",]
evaldf = evaldf[evaldf$PATIENCE==400,]
pdf("/Users/kprovost/Documents/Postdoc_Working/Finished_Models/evaluate_test.pdf",height=7,width=10)
par(mfrow=c(1,2))
plot(evaldf$YEAR,evaldf$avg.acc)
mod = lm(evaldf$avg.acc~evaldf$YEAR)
summary(mod)
pval=summary(mod)$coefficients[2,4]
abline(mod,col="red",lty=ifelse(test=pval<0.05,yes=1,no=3))

plot(evaldf$YEAR,evaldf$avg.segment.error.rate,col="blue",pch=0)
mod = lm(evaldf$avg.segment.error.rate~evaldf$YEAR)
summary(mod)
pval=summary(mod)$coefficients[2,4]
abline(mod,col="green",lty=ifelse(test=pval<0.05,yes=1,no=3))

for(spp in sort(unique(evaldf$TEST))){
  print(spp)
  evalsm = evaldf[evaldf$TEST==spp,]
  plot(evaldf$YEAR,evaldf$avg.acc,type="n",main=spp)
  points(evalsm$YEAR,evalsm$avg.acc,type="p")
  mod = lm(evalsm$avg.acc~evalsm$YEAR)
  print(summary(mod))
  pval=summary(mod)$coefficients[2,4]
  abline(mod,col="blue",lty=ifelse(test=pval<0.05,yes=1,no=3))
  
  plot(evaldf$YEAR,evaldf$avg.segment.error.rate,type="n",main=spp)
  points(evalsm$YEAR,evalsm$avg.segment.error.rate,type="p",col="blue",pch=0)
  mod = lm(evalsm$avg.segment.error.rate~evalsm$YEAR)
  print(summary(mod))
  pval=summary(mod)$coefficients[2,4]
  abline(mod,col="green",lty=ifelse(test=pval<0.05,yes=1,no=3))
  
}
dev.off()


avg=aggregate(evaldf$avg.acc~evaldf$CLADE,FUN=function(x){mean(x,na.rm=T)})
sdv=aggregate(evaldf$avg.acc~evaldf$CLADE,FUN=function(x){sd(x,na.rm=T)})
b=barplot(avg[,2],ylim=c(0.5,1))
segments(b,avg[,2]+sdv[,2],b,avg[,2]-sdv[,2],col="black")

avg=aggregate(evaldf$Train.M.F.Complexity~evaldf$TRAIN,FUN=function(x){mean(x,na.rm=T)})

plot(evaldf$Train.M.F.Complexity[evaldf$YEAR==0],evaldf$avg.acc[evaldf$YEAR==0])

