df = read.table("/Users/kprovost/OneDrive - The Ohio State University/Song/Machine_Learning_TweetyNet_Results.csv",sep=",",header=T)
#df <- df[order(df$Tested.On,df$Estimated.Divergence..mya.,df$Accuracy),]

plot(df$Estimated.Divergence..mya.,df$Accuracy)

df1 = df[df$Tested.On!="Melozone fusca" & df$Type!="Multiple species",]
df2 = df[df$Tested.On=="Melozone fusca",]
df3 = df[df$Type=="Multiple species",]
df4 = df[df$Tested.On!="Melozone fusca",]


pdf("/Users/kprovost/OneDrive - The Ohio State University/Song/TweetyNet_Results_NoMelozone.pdf")
plot(df1$Estimated.Divergence..mya.,df1$Accuracy,type="n",
     ylab="Accuracy",xlab="Estimated Divergence (mya)",
     main="Single Species")
#abline(v=c(3.3,7.6,22.3,52,75.5),col="lightgrey")
legend("bottomleft",legend=unique(df1$Tested.On)[c(1,4,5,2,3)],
       col=c(1,4,5,2,3),
       pch=c(1,4,5,2,3),
       lty=c(1,4,5,2,3),
       text.font=3,title = "Test Species")
for(i in 1:length(unique(df1$Tested.On))){
        spp = unique(df1$Tested.On)[i]
        temp = df1[df1$Tested.On==spp,]
        temp = temp[temp$Estimated.Divergence..mya.!="varies",]
        points(temp$Estimated.Divergence..mya.,temp$Accuracy,
               col=i,type="b",pch=i,lty=i)
}
dev.off()

plot(df1$Estimated.Divergence..mya.,df1$Relative.Accuracy,type="n",
     ylab="Relative Accuracy",xlab="Estimated Divergence (mya)",
     main="Single Species")
#abline(v=c(3.3,7.6,22.3,52,75.5),col="lightgrey")
legend("bottomleft",legend=unique(df1$Tested.On)[c(1,4,5,2,3)],
       col=c(1,4,5,2,3),
       pch=c(1,4,5,2,3),
       lty=c(1,4,5,2,3),
       text.font=3,title = "Test Species")
for(i in 1:length(unique(df1$Tested.On))){
        spp = unique(df1$Tested.On)[i]
        temp = df1[df1$Tested.On==spp,]
        temp = temp[temp$Estimated.Divergence..mya.!="varies",]
        points(temp$Estimated.Divergence..mya.,temp$Relative.Accuracy,
               col=i,type="b",pch=i,lty=i)
}

pdf("/Users/kprovost/OneDrive - The Ohio State University/Song/TweetyNet_Results_Multiples.pdf")
plot(df3$Estimated.Divergence..mya.,df3$Accuracy,type="n",
     ylab="Accuracy",xlab="Estimated Divergence (mya)",
     main="Multiple Species")
#abline(v=c(3.3,7.6,22.3,52,75.5),col="lightgrey")
points(df3$Estimated.Divergence..mya.[df3$Trained.On=="5 Species"],
       df3$Accuracy[df3$Trained.On=="5 Species"],pch=0,cex=2)
points(df3$Estimated.Divergence..mya.[df3$Trained.On=="5 Species Balanced"],
       df3$Accuracy[df3$Trained.On=="5 Species Balanced"],pch=1,cex=2)
legend("bottomleft",legend=unique(df3$Tested.On)[c(1,4,5,6,2,3)],
       col=c(1,4,5,6,2,3),
       pch=c(1,4,5,6,2,3),
       lty=c(1,4,5,6,2,3),
       text.font=3,title = "Test Species")
legend("left",legend=c("Unbalanced","Balanced"),
       pch=c(0,1),pt.cex=2)
for(i in 1:length(unique(df3$Tested.On))){
        spp = unique(df3$Tested.On)[i]
        temp = df3[df3$Tested.On==spp,]
        temp = temp[temp$Estimated.Divergence..mya.!="varies",]
        points(temp$Estimated.Divergence..mya.,temp$Accuracy,
               col=i,type="b",pch=i,lty=i)
}
dev.off()

plot(df3$Estimated.Divergence..mya.,df3$Relative.Accuracy,type="n",
     ylab="Relative.Accuracy",xlab="Estimated Divergence (mya)",
     main="Multiple Species")
#abline(v=c(3.3,7.6,22.3,52,75.5),col="lightgrey")
points(df3$Estimated.Divergence..mya.[df3$Trained.On=="5 Species"],
       df3$Relative.Accuracy[df3$Trained.On=="5 Species"],pch=0,cex=2)
points(df3$Estimated.Divergence..mya.[df3$Trained.On=="5 Species Balanced"],
       df3$Relative.Accuracy[df3$Trained.On=="5 Species Balanced"],pch=1,cex=2)
legend("bottomleft",legend=unique(df3$Tested.On)[c(1,4,5,6,2,3)],
       col=c(1,4,5,6,2,3),
       pch=c(1,4,5,6,2,3),
       lty=c(1,4,5,6,2,3),
       text.font=3,title = "Test Species")
legend("left",legend=c("Unbalanced","Balanced"),
       pch=c(0,1),pt.cex=2)
for(i in 1:length(unique(df3$Tested.On))){
        spp = unique(df3$Tested.On)[i]
        temp = df3[df3$Tested.On==spp,]
        temp = temp[temp$Estimated.Divergence..mya.!="varies",]
        points(temp$Estimated.Divergence..mya.,temp$Relative.Accuracy,
               col=i,type="b",pch=i,lty=i)
}


pdf("/Users/kprovost/OneDrive - The Ohio State University/Song/TweetyNet_Results_NoMelozoneMult.pdf")
plot(df4$Estimated.Divergence..mya.,df4$Accuracy,type="n",
     ylab="Accuracy",xlab="Estimated Divergence (mya)",
     main="")
points(df4$Estimated.Divergence..mya.[df4$Trained.On=="5 Species"],
       df4$Accuracy[df4$Trained.On=="5 Species"],pch=0,cex=2)
points(df4$Estimated.Divergence..mya.[df4$Trained.On=="5 Species Balanced"],
       df4$Accuracy[df4$Trained.On=="5 Species Balanced"],pch=1,cex=2)
legend("bottomleft",legend=unique(df4$Tested.On)[c(1,4,5,2,3)],
       col=c(1,4,5,2,3),
       pch=c(1,4,5,2,3),
       lty=c(1,4,5,2,3),
       text.font=3,title = "Test Species")
legend("left",legend=c("Unbalanced","Balanced"),
       pch=c(0,1),pt.cex=2)
for(i in 1:length(unique(df4$Tested.On))){
        spp = unique(df4$Tested.On)[i]
        temp = df4[df4$Tested.On==spp,]
        temp = temp[temp$Estimated.Divergence..mya.!="varies",]
        points(temp$Estimated.Divergence..mya.,temp$Accuracy,
               col=i,type="b",pch=i,lty=i)
}
dev.off()


pdf("/Users/kprovost/OneDrive - The Ohio State University/Song/TweetyNet_Results_NoMelozoneMultPanels_blackwhite.pdf",width=10)
par(mfrow=c(2,3),mar=c(4,4,0.1,0.1))
plot(df4$Estimated.Divergence..mya.,df4$Accuracy,
     ylab="Accuracy",xlab="Estimated Divergence (mya)",
     main="",pch=16)
points(df4$Estimated.Divergence..mya.[df4$Trained.On=="5 Species"],
       df4$Accuracy[df4$Trained.On=="5 Species"],pch=0,cex=2)
points(df4$Estimated.Divergence..mya.[df4$Trained.On=="5 Species Balanced"],
       df4$Accuracy[df4$Trained.On=="5 Species Balanced"],pch=1,cex=2)
newdf = df4[,c("Estimated.Divergence..mya.","Accuracy")]
colnames(newdf) = c("x","y")
mod=lm(y~x,data=newdf)
abline(mod,col="red")
newx = seq(min(newdf$x),max(newdf$x),by = 0.05)
conf_interval <- predict(mod, newdata=data.frame(x=newx), interval="confidence",
                         level = 0.95)
lines(newx, conf_interval[,2], col="darkred", lty=2)
lines(newx, conf_interval[,3], col="darkred", lty=2)

legend("bottomleft",legend=c("Unbalanced","Balanced"),
       pch=c(0,1),pt.cex=2)
for(i in 1:length(unique(df4$Tested.On))){
        spp = unique(df4$Tested.On)[i]
        temp = df4[df4$Tested.On==spp,]
        temp = temp[temp$Estimated.Divergence..mya.!="varies",]
        plot(df4$Estimated.Divergence..mya.,df4$Accuracy,type="n",
             ylab="Accuracy",xlab="Estimated Divergence (mya)",
             main="")
        points(temp$Estimated.Divergence..mya.,temp$Accuracy,
               col="black",type="b",pch=1,lty=1)
        # legend("bottomleft",legend=spp,pch=1,col="black",lty=1)
        # points(temp$Estimated.Divergence..mya.[temp$Trained.On=="5 Species"],
        #        temp$Accuracy[temp$Trained.On=="5 Species"],pch=0,cex=2)
        #  points(temp$Estimated.Divergence..mya.[temp$Trained.On=="5 Species Balanced"],
        #        temp$Accuracy[temp$Trained.On=="5 Species Balanced"],pch=1,cex=2)
        legend("bottomleft",legend=c("Unbalanced","Balanced"),
               pch=c(0,1),pt.cex=2)
        
}
dev.off()

plot(df4$Estimated.Divergence..mya.,df4$Relative.Accuracy,type="n",
     ylab="Relative.Accuracy",xlab="Estimated Divergence (mya)",
     main="")
points(df4$Estimated.Divergence..mya.[df4$Trained.On=="5 Species"],
       df4$Relative.Accuracy[df4$Trained.On=="5 Species"],pch=0,cex=2)
points(df4$Estimated.Divergence..mya.[df4$Trained.On=="5 Species Balanced"],
       df4$Relative.Accuracy[df4$Trained.On=="5 Species Balanced"],pch=1,cex=2)
legend("bottomleft",legend=unique(df4$Tested.On)[c(1,4,5,2,3)],
       col=c(1,4,5,2,3),
       pch=c(1,4,5,2,3),
       lty=c(1,4,5,2,3),
       text.font=3,title = "Test Species")
legend("left",legend=c("Unbalanced","Balanced"),
       pch=c(0,1),pt.cex=2)
for(i in 1:length(unique(df4$Tested.On))){
        spp = unique(df4$Tested.On)[i]
        temp = df4[df4$Tested.On==spp,]
        temp = temp[temp$Estimated.Divergence..mya.!="varies",]
        points(temp$Estimated.Divergence..mya.,temp$Relative.Accuracy,
               col=i,type="b",pch=i,lty=i)
}


#pdf("/Users/kprovost/OneDrive - The Ohio State University/Song/TweetyNet_Results_Melozone.pdf")
pdf("/Users/kprovost/OneDrive - The Ohio State University/Song/TweetyNet_Results_Melozone_blackwhite.pdf")
plot(df2$Estimated.Divergence..mya.,df2$Accuracy,
     ylab="Accuracy",xlab="Estimated Divergence (mya)",
     col="black",main="Zero/Few Shot",pch=1,type="b",lty=1)
points(df2$Estimated.Divergence..mya.[df2$Trained.On=="5 Species"],
       df2$Accuracy[df2$Trained.On=="5 Species"],pch=0,cex=2)
points(df2$Estimated.Divergence..mya.[df2$Trained.On=="5 Species + Melozone fusca"],
       df2$Accuracy[df2$Trained.On=="5 Species + Melozone fusca"],pch=0,cex=2,col=6)
points(df2$Estimated.Divergence..mya.[df2$Trained.On=="5 Species Balanced"],
       df2$Accuracy[df2$Trained.On=="5 Species Balanced"],pch=1,cex=2)
points(df2$Estimated.Divergence..mya.[df2$Trained.On=="5 Species Balanced + Melozone fusca"],
       df2$Accuracy[df2$Trained.On=="5 Species Balanced + Melozone fusca"],pch=1,cex=2,col=6)
legend("bottomleft",legend=c("Unbalanced","Unbalanced-Transfer","Balanced","Balanced-Transfer"),
       pch=c(0,0,1,1),pt.cex=2,col=c(1,6,1,6))
# legend("bottomleft",legend=unique(df2$Tested.On),
#        col=c(6),
#        pch=c(6),
#        lty=c(6),
#        text.font=3,title = "Test Species")
dev.off()



df = read.table("/Users/kprovost/OneDrive - The Ohio State University/temp_compare_acc_tweetynet.txt",sep="\t",header=T)
head(df)
agg = aggregate(df$acc~df$test+df$DISTANCE+df$TRANS,FUN=function(x){mean(x,na.rm=T)})
colnames(agg) = c("test","DIS","TRAN","acc")
plot(agg[agg$test=="CA" & agg$TRAN==0,c("DIS","acc")],ylim=c(0.6,1),type="b",
     xaxt="n")
axis(1,at=c(0,1,2,3,4),labels = c("SPP","GEN","OSC","ORD","BRD"))
points(agg[agg$test=="CC" & agg$TRAN==0,c("DIS","acc")],ylim=c(0.6,1),col="red",type="b")
points(agg[agg$test=="CS" & agg$TRAN==0,c("DIS","acc")],ylim=c(0.6,1),col="blue",type="b")
points(agg[agg$test=="EV" & agg$TRAN==0,c("DIS","acc")],ylim=c(0.6,1),col="green",type="b")
points(agg[agg$test=="ZL" & agg$TRAN==0,c("DIS","acc")],ylim=c(0.6,1),col="goldenrod",type="b")
points(agg[agg$test=="CA" & agg$TRAN==1,c("DIS","acc")],ylim=c(0.6,1),type="b",pch=0,col="black",lty=3)
points(agg[agg$test=="CC" & agg$TRAN==1,c("DIS","acc")],ylim=c(0.6,1),type="b",pch=0,col="red",lty=3)
points(agg[agg$test=="CS" & agg$TRAN==1,c("DIS","acc")],ylim=c(0.6,1),type="b",pch=0,col="blue",lty=3)
points(agg[agg$test=="EV" & agg$TRAN==1,c("DIS","acc")],ylim=c(0.6,1),type="b",pch=0,col="green",lty=3)
points(agg[agg$test=="ZL" & agg$TRAN==1,c("DIS","acc")],ylim=c(0.6,1),type="b",pch=0,col="goldenrod",lty=3)
legend("topright",
       col=c("black","red","blue","green","goldenrod"),
       legend=c("CA","CC","CS","EV","ZL"),pch=1,
       )
legend("bottomleft",
       legend=c("No Transfer","Transfer"),pch=c(1,0),
)

times = c(34,34,42,14,73,233,31,9,8)
sizes = c(190,873,318,109,3328,4818,875,68,68)
plot(sizes,times)
plot(log10(sizes),log10(times))
mod=lm(log10(times)~log10(sizes))
summary(mod)
abline(mod,col="red")

acc=c(0.91,0.64,0.64,0.84,0.66,0.66,0.88,0.91,0.93,0.84,0.73,0.84,0.92,0.89,0.75,0.59,0.63,0.73,0.96,0.69,0.48,0.68,0.80,0.91,0.90)
div=c(0,85,85,85,85,85,0,6.6,66,38,85,6.6,0,66,38,85,66,66,0,66,85,38,38,66,0)
loss=c(0,0.24,0.28,0.12,0.24,0.25,0,0.01,0.03,0.06,0.18,0.04,0,0.07,0.15,0.32,0.25,0.19,0,0.21,0.43,0.2,0.12,0.05,0)
tested=c("CA","CC","CS","EV","ZL","CA","CC","CS","EV","ZL","CA","CC","CS","EV","ZL","CA","CC","CS","EV","ZL","CA","CC","CS","EV","ZL")
plot(div,acc,col=as.numeric(as.factor(tested)),pch=as.numeric(as.factor(tested)))
plot(div,loss*100,col=as.numeric(as.factor(tested)),pch=as.numeric(as.factor(tested)))
abline(h=seq(0,50,1),col="grey",lty=3)
abline(h=seq(0,50,10),col="grey",lty=1)

## training time vs accuracy
acc2=c(0.91,0.64,0.64,0.84,0.66,0.66,0.88,0.91,0.93,0.84,0.73,0.84,0.92,0.89,0.75,0.59,0.63,0.73,0.96,0.69,0.48,0.68,0.8,0.91,0.9)
spptraintime=c(34,34,34,34,34,34,34,34,34,34,42,42,42,42,42,14,14,14,14,14,73,73,73,73,73)
spptrainsize=c(190,190,190,190,190,873,873,873,873,873,318,318,318,318,318,109,109,109,109,109,3328,3328,3328,3328,3328)
spptesttime=c(34,34,42,14,73,34,34,42,14,73,34,34,42,14,73,34,34,42,14,73,34,34,42,14,73)
spptestsize=c(190,873,318,109,3328,190,873,318,109,3328,190,873,318,109,3328,190,873,318,109,3328,190,873,318,109,3328)
spptrain=c("CA","CA","CA","CA","CA","CC","CC","CC","CC","CC","CS","CS","CS","CS","CS","EV","EV","EV","EV","EV","ZL","ZL","ZL","ZL","ZL")
spptest=c("CA","CC","CS","EV","ZL","CA","CC","CS","EV","ZL","CA","CC","CS","EV","ZL","CA","CC","CS","EV","ZL","CA","CC","CS","EV","ZL")
aggtrain = aggregate(acc2~spptrain,FUN=function(x){mean(x,na.rm=T)})
aggtest = aggregate(acc2~spptest,FUN=function(x){mean(x,na.rm=T)})
plot(spptraintime,acc2,col=as.numeric(as.factor(spptest)),pch=as.numeric(as.factor(spptrain)))
plot(spptrainsize,acc2,col=as.numeric(as.factor(spptest)),pch=as.numeric(as.factor(spptrain)))
plot(spptraintime[c(1,6,11,16,21)],aggtrain[,2],)
plot(spptrainsize[c(1,6,11,16,21)],aggtrain[,2],)

plot(spptesttime,acc2,col=as.numeric(as.factor(spptest)),pch=as.numeric(as.factor(spptrain)))
plot(spptestsize,acc2,col=as.numeric(as.factor(spptest)),pch=as.numeric(as.factor(spptrain)))
plot(spptesttime[c(1:5)],aggtest[,2],)
plot(spptestsize[c(1:5)],aggtest[,2],)

summary(lm(acc2~spptraintime))
summary(lm(acc2~spptrainsize))
summary(lm(acc2~spptesttime))
summary(lm(acc2~spptestsize))
summary(lm(acc2~spptrain))
summary(lm(acc2~spptest))

plot(aggtest[,2])
