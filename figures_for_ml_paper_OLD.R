library(geomorph)

#df = read.table("/Users/kprovost/OneDrive - The Ohio State University/Song/Machine_Learning_TweetyNet_Results.csv",sep=",",header=T)
df=read.table("/Users/kprovost/Documents/Postdoc_Working/Finished_Models/Machine_Learning_TweetyNet_Results_5August2022.csv",sep=",",header=T)

#df <- df[order(df$Tested.On,df$Estimated.Divergence..mya.,df$Accuracy),]

#plot(df$Estimated.Divergence..mya.,df$Accuracy..5.)
#plot(df$Estimated.Divergence..mya.,df$Accuracy..50.)
plot(as.numeric(df$Estimated.Divergence..mya.),
     as.numeric(df$Accuracy..100.),col=as.numeric(as.factor(df$UPDATED)))
plot(as.numeric(df$Estimated.Divergence..mya.),
     log10(as.numeric(df$Segment.Rate..100.)),col=as.numeric(as.factor(df$UPDATED)))
abline(h=0)
plot(log10(df$Size.of.Dataset..sec.),log10(df$Training.Time..100..sec.))
abline(lm(log10(df$Training.Time..100..sec.)~log10(df$Size.of.Dataset..sec.)))

#plot(df$Estimated.Divergence..mya.,df$Accuracy..50.-df$Accuracy..5.)
abline(h=0)

df1 = df[df$Tested.On!="Melozone fusca" & df$Type!="Multiple species",]
df2 = df[df$Tested.On=="Melozone fusca",]
df3 = df[df$Type=="Multiple species",]
df4 = df[df$Tested.On!="Melozone fusca",]


accplots=T
if(accplots==T){
df_train = df[,c(#"Training.Time..5..sec.",
                 #"Training.Time..50..sec.",
    "Training.Time..100..sec.",
                 "Size.of.Dataset..sec.",
                 "Trained.On")]
df_train=unique(df_train)
#plot(log10(df_train$Size.of.Dataset..sec.),
#     log10(df_train$Training.Time..50..sec.),
#     pch=as.character(as.numeric(as.factor(df_train$Trained.On))),
#     col="darkblue")
#mod1=lm(log10(df_train$Training.Time..50..sec.)~log10(df_train$Size.of.Dataset..sec.))
#abline(mod1,col="blue")
#points(log10(df_train$Size.of.Dataset..sec.),
#     log10(df_train$Training.Time..5..sec.),
#     pch=as.character(as.numeric(as.factor(df_train$Trained.On))),
#     col="darkred")
#mod2=lm(log10(df_train$Training.Time..5..sec.)~log10(df_train$Size.of.Dataset..sec.))
#abline(mod2,col="red")
plot(log10(df_train$Size.of.Dataset..sec.),
    log10(df_train$Training.Time..100..sec.),
    pch=as.character(as.numeric(as.factor(df_train$Trained.On))),
    col="darkblue")
modA=lm(log10(df_train$Training.Time..100..sec.)~log10(df_train$Size.of.Dataset..sec.))
abline(modA,col="blue")


## checking without mf
df_train2 = df_train[df_train$Trained.On!="5 Species + Melozone fusca",]
df_train2 = df_train2[df_train2$Trained.On!="5 Species Balanced + Melozone fusca",]

#mod3=lm(log10(df_train2$Training.Time..50..sec.)~log10(df_train2$Size.of.Dataset..sec.))
#mod4=lm(log10(df_train2$Training.Time..5..sec.)~log10(df_train2$Size.of.Dataset..sec.))
#summary(mod1)
#summary(mod2)
#summary(mod3)
#summary(mod4)
modB=lm(log10(df_train2$Training.Time..100..sec.)~log10(df_train2$Size.of.Dataset..sec.))
summary(modA)
summary(modB)

pdf("~/TweetyNet_Results_NoMelozone_5August2022.pdf")
plot(c(df1$Estimated.Divergence..mya.),
     c(df1$Accuracy..100.),type="n",
     ylab="Accuracy",xlab="Estimated Divergence (mya)",
     main="Single Species",ylim=c(0.4,1))
for(i in 1:length(sort(unique(df1$Tested.On)))){
    spp = sort(unique(df1$Tested.On))[i]
    temp = df1[df1$Tested.On==spp,]
    temp = temp[temp$Estimated.Divergence..mya.!="varies",]
    points(temp$Estimated.Divergence..mya.,temp$Accuracy..100.,
           col="darkred",type="b",pch=i,lty=1)
}
dev.off()

# pdf("~/TweetyNet_Results_NoMelozone_REDO.pdf")
# plot(c(df1$Estimated.Divergence..mya.,df1$Estimated.Divergence..mya.),
#      c(df1$Accuracy..5.,df1$Accuracy..50.),type="n",
#      ylab="Accuracy",xlab="Estimated Divergence (mya)",
#      main="Single Species",ylim=c(0.4,1))
# #abline(v=c(3.3,7.6,22.3,52,75.5),col="lightgrey")
# legend("bottomleft",legend=sort(unique(df1$Tested.On))[c(1,4,5,2,3)],
#        col="black",
#        #pch=1,
#        pch=c(1,4,5,2,3),
#        #lty=c(1,4,5,2,3),
#        text.font=3,title = "Test Species")
# legend("bottomright",legend=c(5,50),
#        pch=16,lty=c(1,3),col=c("darkred","darkblue"))
# for(i in 1:length(sort(unique(df1$Tested.On)))){
#         spp = sort(unique(df1$Tested.On))[i]
#         temp = df1[df1$Tested.On==spp,]
#         temp = temp[temp$Estimated.Divergence..mya.!="varies",]
#         points(temp$Estimated.Divergence..mya.,temp$Accuracy..5.,
#                col="darkred",type="b",pch=i,lty=1)
#         points(temp$Estimated.Divergence..mya.,temp$Accuracy..50.,
#                col="darkblue",type="b",pch=i,lty=2)
# }
# dev.off()

# plot(c(df1$Estimated.Divergence..mya.,df1$Estimated.Divergence..mya.),
#      c(df1$Relative.Accuracy..5.,df1$Relative.Accuracy..50.),type="n",
#      ylab="Relative Accuracy",xlab="Estimated Divergence (mya)",
#      main="Single Species",ylim=c(-0.5,0))
# #abline(v=c(3.3,7.6,22.3,52,75.5),col="lightgrey")
# legend("bottomleft",legend=sort(unique(df1$Tested.On))[c(1,4,5,2,3)],
#        col="black",
#        #pch=1,
#        pch=c(1,4,5,2,3),
#        #lty=c(1,4,5,2,3),
#        text.font=3,title = "Test Species")
# legend("bottomright",legend=c(5,50),
#        pch=16,lty=c(1,3),col=c("darkred","darkblue"))
# for(i in 1:length(sort(unique(df1$Tested.On)))){
#         spp = sort(unique(df1$Tested.On))[i]
#         temp = df1[df1$Tested.On==spp,]
#         temp = temp[temp$Estimated.Divergence..mya.!="varies",]
#         points(temp$Estimated.Divergence..mya.,temp$Relative.Accuracy..5.,
#                col="darkred",type="b",pch=i,lty=1)
#         points(temp$Estimated.Divergence..mya.,temp$Relative.Accuracy..50.,
#                col="darkblue",type="b",pch=i,lty=2)
# }


pdf("~/TweetyNet_Results_Multiples_5August2022.pdf")
plot(df3$Estimated.Divergence..mya.,
     df3$Accuracy..100.,type="n",
     ylab="Accuracy",xlab="Estimated Divergence (mya)",
     main="Multiple Species",ylim=c(0.4,1))
#abline(v=c(3.3,7.6,22.3,52,75.5),col="lightgrey")
points(df3$Estimated.Divergence..mya.[df3$Trained.On=="5 Species"],
       df3$Accuracy..100.[df3$Trained.On=="5 Species"],pch=0,cex=2.5,col="darkblue")
points(df3$Estimated.Divergence..mya.[df3$Trained.On=="5 Species Balanced"],
       df3$Accuracy..100.[df3$Trained.On=="5 Species Balanced"],pch=1,cex=2.5,col="darkblue")
for(i in 1:length(sort(unique(df3$Tested.On)))){
    spp = sort(unique(df3$Tested.On))[i]
    temp = df3[df3$Tested.On==spp,]
    temp = temp[temp$Estimated.Divergence..mya.!="varies",]
    points(temp$Estimated.Divergence..mya.,temp$Accuracy..100.,
           col="darkred",type="b",pch=i,lty=1)
    
}
dev.off()


# pdf("~/TweetyNet_Results_Multiples_REDO.pdf")
# plot(c(df3$Estimated.Divergence..mya.,df3$Estimated.Divergence..mya.),
#      c(df3$Accuracy..5.,df3$Accuracy..50.),type="n",
#      ylab="Accuracy",xlab="Estimated Divergence (mya)",
#      main="Multiple Species",ylim=c(0.4,1))
# #abline(v=c(3.3,7.6,22.3,52,75.5),col="lightgrey")
# points(df3$Estimated.Divergence..mya.[df3$Trained.On=="5 Species"],
#        df3$Accuracy..5.[df3$Trained.On=="5 Species"],pch=0,cex=2,col="darkred")
# points(df3$Estimated.Divergence..mya.[df3$Trained.On=="5 Species Balanced"],
#        df3$Accuracy..5.[df3$Trained.On=="5 Species Balanced"],pch=1,cex=2,col="darkred")
# 
# points(df3$Estimated.Divergence..mya.[df3$Trained.On=="5 Species"],
#        df3$Accuracy..50.[df3$Trained.On=="5 Species"],pch=0,cex=2.5,col="darkblue")
# points(df3$Estimated.Divergence..mya.[df3$Trained.On=="5 Species Balanced"],
#        df3$Accuracy..50.[df3$Trained.On=="5 Species Balanced"],pch=1,cex=2.5,col="darkblue")
# 
# legend("bottomleft",legend=sort(unique(df1$Tested.On))[c(1,4,5,2,3)],
#        col="black",
#        #pch=1,
#        pch=c(1,4,5,2,3),
#        #lty=c(1,4,5,2,3),
#        text.font=3,title = "Test Species")
# legend("bottomright",legend=c(5,50),
#        pch=16,lty=c(1,3),col=c("darkred","darkblue"))
# legend("left",legend=c("Unbalanced","Balanced"),
#        pch=c(0,1),pt.cex=2)
# for(i in 1:length(sort(unique(df3$Tested.On)))){
#         spp = sort(unique(df3$Tested.On))[i]
#         temp = df3[df3$Tested.On==spp,]
#         temp = temp[temp$Estimated.Divergence..mya.!="varies",]
#         points(temp$Estimated.Divergence..mya.,temp$Accuracy..5.,
#                col="darkred",type="b",pch=i,lty=1)
#         points(temp$Estimated.Divergence..mya.,temp$Accuracy..50.,
#                col="darkblue",type="b",pch=i,lty=2)
#         
# }
# dev.off()

# plot(c(df3$Estimated.Divergence..mya.,df3$Estimated.Divergence..mya.),
#      c(df3$Relative.Accuracy..5.,df3$Relative.Accuracy..50.),type="n",
#      ylab="Relative.Accuracy",xlab="Estimated Divergence (mya)",
#      main="Multiple Species",ylim=c(-0.5,0))
# #abline(v=c(3.3,7.6,22.3,52,75.5),col="lightgrey")
# points(df3$Estimated.Divergence..mya.[df3$Trained.On=="5 Species"],
#        df3$Relative.Accuracy..5.[df3$Trained.On=="5 Species"],pch=0,cex=2,col="darkred")
# points(df3$Estimated.Divergence..mya.[df3$Trained.On=="5 Species Balanced"],
#        df3$Relative.Accuracy..5.[df3$Trained.On=="5 Species Balanced"],pch=1,cex=2,col="darkred")
# points(df3$Estimated.Divergence..mya.[df3$Trained.On=="5 Species"],
#        df3$Relative.Accuracy..50.[df3$Trained.On=="5 Species"],pch=0,cex=2.5,col="darkblue")
# points(df3$Estimated.Divergence..mya.[df3$Trained.On=="5 Species Balanced"],
#        df3$Relative.Accuracy..50.[df3$Trained.On=="5 Species Balanced"],pch=1,cex=2.5,col="darkblue")
# 
# legend("bottomleft",legend=sort(unique(df1$Tested.On))[c(1,4,5,2,3)],
#        col="black",
#        #pch=1,
#        pch=c(1,4,5,2,3),
#        #lty=c(1,4,5,2,3),
#        text.font=3,title = "Test Species")
# legend("bottomright",legend=c(5,50),
#        pch=16,lty=c(1,3),col=c("darkred","darkblue"))
# legend("left",legend=c("Unbalanced","Balanced"),
#        pch=c(0,1),pt.cex=2)
# for(i in 1:length(sort(unique(df3$Tested.On)))){
#         spp = sort(unique(df3$Tested.On))[i]
#         temp = df3[df3$Tested.On==spp,]
#         temp = temp[temp$Estimated.Divergence..mya.!="varies",]
#         points(temp$Estimated.Divergence..mya.,temp$Relative.Accuracy..5.,
#                col="darkred",type="b",pch=i,lty=1)
#         points(temp$Estimated.Divergence..mya.,temp$Relative.Accuracy..50.,
#                col="darkblue",type="b",pch=i,lty=2)
# }


pdf("~/TweetyNet_Results_NoMelozoneMult_5August2022.pdf")
plot(c(df4$Estimated.Divergence..mya.),
     c(df4$Accuracy..100.),type="n",
     ylab="Accuracy",xlab="Estimated Divergence (mya)",
     main="")
points(df4$Estimated.Divergence..mya.[df4$Trained.On=="5 Species"],
       df4$Accuracy..100.[df4$Trained.On=="5 Species"],pch=0,cex=2,col="darkred")
points(df4$Estimated.Divergence..mya.[df4$Trained.On=="5 Species Balanced"],
       df4$Accuracy..100.[df4$Trained.On=="5 Species Balanced"],pch=1,cex=2,col="darkred")
for(i in 1:length(sort(unique(df4$Tested.On)))){
    spp = sort(unique(df4$Tested.On))[i]
    temp = df4[df4$Tested.On==spp,]
    temp = temp[temp$Estimated.Divergence..mya.!="varies",]
    points(temp$Estimated.Divergence..mya.,temp$Accuracy..100.,
           col="darkred",type="b",pch=i,lty=1)
}
dev.off()

# pdf("~/TweetyNet_Results_NoMelozoneMult_REDO.pdf")
# plot(c(df4$Estimated.Divergence..mya.,df4$Estimated.Divergence..mya.),
#      c(df4$Accuracy..5.,df4$Accuracy..50.),type="n",
#      ylab="Accuracy",xlab="Estimated Divergence (mya)",
#      main="")
# points(df4$Estimated.Divergence..mya.[df4$Trained.On=="5 Species"],
#        df4$Accuracy..5.[df4$Trained.On=="5 Species"],pch=0,cex=2,col="darkred")
# points(df4$Estimated.Divergence..mya.[df4$Trained.On=="5 Species Balanced"],
#        df4$Accuracy..5.[df4$Trained.On=="5 Species Balanced"],pch=1,cex=2,col="darkred")
# points(df4$Estimated.Divergence..mya.[df4$Trained.On=="5 Species"],
#        df4$Accuracy..50.[df4$Trained.On=="5 Species"],pch=0,cex=2.5,col="darkblue")
# points(df4$Estimated.Divergence..mya.[df4$Trained.On=="5 Species Balanced"],
#        df4$Accuracy..50.[df4$Trained.On=="5 Species Balanced"],pch=1,cex=2.5,col="darkblue")
# 
# legend("bottomleft",legend=sort(unique(df1$Tested.On))[c(1,4,5,2,3)],
#        col="black",
#        #pch=1,
#        pch=c(1,4,5,2,3),
#        #lty=c(1,4,5,2,3),
#        text.font=3,title = "Test Species")
# legend("bottomright",legend=c(5,50),
#        pch=16,lty=c(1,3),col=c("darkred","darkblue"))
# legend("left",legend=c("Unbalanced","Balanced"),
#        pch=c(0,1),pt.cex=2)
# for(i in 1:length(sort(unique(df4$Tested.On)))){
#         spp = sort(unique(df4$Tested.On))[i]
#         temp = df4[df4$Tested.On==spp,]
#         temp = temp[temp$Estimated.Divergence..mya.!="varies",]
#         points(temp$Estimated.Divergence..mya.,temp$Accuracy..5.,
#                col="darkred",type="b",pch=i,lty=1)
#         points(temp$Estimated.Divergence..mya.,temp$Accuracy..50.,
#                col="darkred",type="b",pch=i,lty=2)
# }
# dev.off()


pdf("~/TweetyNet_Results_NoMelozoneMultPanels_blackwhite_5August2022.pdf",width=10)
par(mfrow=c(2,3),mar=c(4,4,0.1,0.1))
plot(c(df4$Estimated.Divergence..mya.),
     c(df4$Accuracy..100.),
     ylab="Accuracy",xlab="Estimated Divergence (mya)",
     main="",pch=16,col=c(rep("darkred",length(df4$Estimated.Divergence..mya.))),
     ylim=c(0.4,1))
points(df4$Estimated.Divergence..mya.[df4$Trained.On=="5 Species"],
       df4$Accuracy..100.[df4$Trained.On=="5 Species"],pch=0,cex=2,col="darkred")
points(df4$Estimated.Divergence..mya.[df4$Trained.On=="5 Species Balanced"],
       df4$Accuracy..100.[df4$Trained.On=="5 Species Balanced"],pch=1,cex=2,col="darkred")
newdf = df4[,c("Estimated.Divergence..mya.","Accuracy..100.")]
colnames(newdf) = c("x","y")
mod=lm(y~x,data=newdf)
abline(mod,col="red")
newx = seq(min(newdf$x),max(newdf$x),by = 0.05)
conf_interval <- predict(mod, newdata=data.frame(x=newx), interval="confidence",
                         level = 0.95)
lines(newx, conf_interval[,2], col="darkred", lty=2)
lines(newx, conf_interval[,3], col="darkred", lty=2)
for(i in 1:length(sort(unique(df4$Tested.On)))){
    spp = sort(unique(df4$Tested.On))[i]
    temp = df4[df4$Tested.On==spp,]
    temp = temp[temp$Estimated.Divergence..mya.!="varies",]
    plot(c(df4$Estimated.Divergence..mya.),
         c(df4$Accuracy..100.),type="n",
         ylab="Accuracy",xlab="Estimated Divergence (mya)",
         main=spp,ylim=c(0.4,1))
    
    legend("bottomright",legend=c(5,50),
           pch=16,lty=c(1,3),col=c("darkred","darkblue"))
    legend("bottomleft",legend=c("Unbalanced","Balanced"),
           pch=c(0,1),pt.cex=2)
    
    points(temp$Estimated.Divergence..mya.,temp$Accuracy..100.,
           type="b",pch=16,lty=1,col="darkred")
    points(temp$Estimated.Divergence..mya.[temp$Trained.On=="5 Species"],
           temp$Accuracy..100.[temp$Trained.On=="5 Species"],pch=0,cex=2,col="darkred")
    points(temp$Estimated.Divergence..mya.[temp$Trained.On=="5 Species Balanced"],
           temp$Accuracy..100.[temp$Trained.On=="5 Species Balanced"],pch=1,cex=2,col="darkred")
    
    
}
dev.off()

# pdf("~/TweetyNet_Results_NoMelozoneMultPanels_blackwhite_REDO.pdf",width=10)
# par(mfrow=c(2,3),mar=c(4,4,0.1,0.1))
# plot(c(df4$Estimated.Divergence..mya.,df4$Estimated.Divergence..mya.),
#      c(df4$Accuracy..5.,df4$Accuracy..50.),
#      ylab="Accuracy",xlab="Estimated Divergence (mya)",
#      main="",pch=16,col=c(rep("darkred",length(df4$Estimated.Divergence..mya.)),
#                           rep("darkblue",length(df4$Estimated.Divergence..mya.))),
#      ylim=c(0.4,1))
# points(df4$Estimated.Divergence..mya.[df4$Trained.On=="5 Species"],
#        df4$Accuracy..5.[df4$Trained.On=="5 Species"],pch=0,cex=2,col="darkred")
# points(df4$Estimated.Divergence..mya.[df4$Trained.On=="5 Species Balanced"],
#        df4$Accuracy..5.[df4$Trained.On=="5 Species Balanced"],pch=1,cex=2,col="darkred")
# points(df4$Estimated.Divergence..mya.[df4$Trained.On=="5 Species"],
#        df4$Accuracy..50.[df4$Trained.On=="5 Species"],pch=0,cex=2.5,col="darkblue")
# points(df4$Estimated.Divergence..mya.[df4$Trained.On=="5 Species Balanced"],
#        df4$Accuracy..50.[df4$Trained.On=="5 Species Balanced"],pch=1,cex=2.5,col="darkblue")
# 
# newdf = df4[,c("Estimated.Divergence..mya.","Accuracy..5.")]
# colnames(newdf) = c("x","y")
# mod=lm(y~x,data=newdf)
# abline(mod,col="red")
# newx = seq(min(newdf$x),max(newdf$x),by = 0.05)
# conf_interval <- predict(mod, newdata=data.frame(x=newx), interval="confidence",
#                          level = 0.95)
# lines(newx, conf_interval[,2], col="darkred", lty=2)
# lines(newx, conf_interval[,3], col="darkred", lty=2)
# 
# newdf50 = df4[,c("Estimated.Divergence..mya.","Accuracy..50.")]
# colnames(newdf50) = c("x","y")
# mod50=lm(y~x,data=newdf50)
# abline(mod50,col="blue")
# newx50 = seq(min(newdf50$x),max(newdf50$x),by = 0.05)
# conf_interval50 <- predict(mod50, newdata=data.frame(x=newx50), interval="confidence",
#                          level = 0.95)
# lines(newx50, conf_interval50[,2], col="darkblue", lty=2)
# lines(newx50, conf_interval50[,3], col="darkblue", lty=2)
# 
# legend("bottomright",legend=c(5,50),
#        pch=16,lty=c(1,3),col=c("darkred","darkblue"))
# legend("bottomleft",legend=c("Unbalanced","Balanced"),
#        pch=c(0,1),pt.cex=2)
# for(i in 1:length(sort(unique(df4$Tested.On)))){
#         spp = sort(unique(df4$Tested.On))[i]
#         temp = df4[df4$Tested.On==spp,]
#         temp = temp[temp$Estimated.Divergence..mya.!="varies",]
#         plot(c(df4$Estimated.Divergence..mya.,df4$Estimated.Divergence..mya.),
#              c(df4$Accuracy..5.,df4$Accuracy..50.),type="n",
#              ylab="Accuracy",xlab="Estimated Divergence (mya)",
#              main=spp,ylim=c(0.4,1))
#         
#         legend("bottomright",legend=c(5,50),
#                pch=16,lty=c(1,3),col=c("darkred","darkblue"))
#         legend("bottomleft",legend=c("Unbalanced","Balanced"),
#                pch=c(0,1),pt.cex=2)
#         
#         points(temp$Estimated.Divergence..mya.,temp$Accuracy..5.,
#                type="b",pch=16,lty=1,col="darkred")
#         points(temp$Estimated.Divergence..mya.,temp$Accuracy..50.,
#                type="b",pch=16,lty=2,col="darkblue")
#         # legend("bottomleft",legend=spp,pch=1,col="black",lty=1)
#         # points(temp$Estimated.Divergence..mya.[temp$Trained.On=="5 Species"],
#         #        temp$Accuracy[temp$Trained.On=="5 Species"],pch=0,cex=2)
#         #  points(temp$Estimated.Divergence..mya.[temp$Trained.On=="5 Species Balanced"],
#         #        temp$Accuracy[temp$Trained.On=="5 Species Balanced"],pch=1,cex=2)
#         points(temp$Estimated.Divergence..mya.[temp$Trained.On=="5 Species"],
#                temp$Accuracy..5.[temp$Trained.On=="5 Species"],pch=0,cex=2,col="darkred")
#         points(temp$Estimated.Divergence..mya.[temp$Trained.On=="5 Species Balanced"],
#                temp$Accuracy..5.[temp$Trained.On=="5 Species Balanced"],pch=1,cex=2,col="darkred")
#         points(temp$Estimated.Divergence..mya.[temp$Trained.On=="5 Species"],
#                temp$Accuracy..50.[temp$Trained.On=="5 Species"],pch=0,cex=2.5,col="darkblue")
#         points(temp$Estimated.Divergence..mya.[temp$Trained.On=="5 Species Balanced"],
#                temp$Accuracy..50.[temp$Trained.On=="5 Species Balanced"],pch=1,cex=2.5,col="darkblue")
#         
# 
#         
# }
# dev.off()

# plot(c(df4$Estimated.Divergence..mya.,df4$Estimated.Divergence..mya.),
#      c(df4$Relative.Accuracy..5.,df4$Relative.Accuracy..50.),type="n",
#      ylab="Relative.Accuracy",xlab="Estimated Divergence (mya)",
#      main="",ylim=c(-0.4,0))
# points(df4$Estimated.Divergence..mya.[df4$Trained.On=="5 Species"],
#        df4$Relative.Accuracy..5.[df4$Trained.On=="5 Species"],pch=0,cex=2,col="darkred")
# points(df4$Estimated.Divergence..mya.[df4$Trained.On=="5 Species Balanced"],
#        df4$Relative.Accuracy..5.[df4$Trained.On=="5 Species Balanced"],pch=1,cex=2,col="darkred")
# points(df4$Estimated.Divergence..mya.[df4$Trained.On=="5 Species"],
#        df4$Relative.Accuracy..50.[df4$Trained.On=="5 Species"],pch=0,cex=2.5,col="darkblue")
# points(df4$Estimated.Divergence..mya.[df4$Trained.On=="5 Species Balanced"],
#        df4$Relative.Accuracy..50.[df4$Trained.On=="5 Species Balanced"],pch=1,cex=2.5,col="darkblue")
# legend("bottomleft",legend=sort(unique(df1$Tested.On))[c(1,4,5,2,3)],
#        col="black",
#        #pch=1,
#        pch=c(1,4,5,2,3),
#        #lty=c(1,4,5,2,3),
#        text.font=3,title = "Test Species")
# legend("bottomright",legend=c(5,50),
#        pch=16,lty=c(1,3),col=c("darkred","darkblue"))
# legend("left",legend=c("Unbalanced","Balanced"),
#        pch=c(0,1),pt.cex=2)
# for(i in 1:length(sort(unique(df4$Tested.On)))){
#         spp = sort(unique(df4$Tested.On))[i]
#         temp = df4[df4$Tested.On==spp,]
#         temp = temp[temp$Estimated.Divergence..mya.!="varies",]
#         points(temp$Estimated.Divergence..mya.,temp$Relative.Accuracy..5.,
#                col="darkred",type="b",pch=i,lty=1)
#         points(temp$Estimated.Divergence..mya.,temp$Relative.Accuracy..50.,
#                col="darkblue",type="b",pch=i,lty=2)       
#         
#         
# }


#pdf("~/TweetyNet_Results_Melozone.pdf")
# pdf("~/TweetyNet_Results_Melozone_blackwhite_REDO.pdf")
# plot(c(df2$Estimated.Divergence..mya.,df2$Estimated.Divergence..mya.),
#      c(df2$Accuracy..5.,df2$Accuracy..5.),
#      ylab="Accuracy",xlab="Estimated Divergence (mya)",
#      col="black",main="Zero/Few Shot",pch=1,type="n",lty=1,
#      ylim=c(0.4,1))
# legend("bottomleft",legend=c("Unbalanced","Unbalanced-Transfer","Balanced","Balanced-Transfer"),
#        pch=c(0,7,1,13),pt.cex=2,col="black")
# legend("bottomright",legend=c(5,50),
#        pch=c(1,2),lty=c(1,3))
# points(c(df2$Estimated.Divergence..mya.),
#      c(df2$Accuracy..5.),
#      col="darkred",pch=16,type="b",lty=1)
# points(c(df2$Estimated.Divergence..mya.),
#        c(df2$Accuracy..50.),
#        col="darkblue",pch=16,type="b",lty=2)
# 
# points(df2$Estimated.Divergence..mya.[df2$Trained.On=="5 Species"],
#        df2$Accuracy..5.[df2$Trained.On=="5 Species"],pch=0,cex=2,col="darkred")
# points(df2$Estimated.Divergence..mya.[df2$Trained.On=="5 Species Balanced"],
#        df2$Accuracy..5.[df2$Trained.On=="5 Species Balanced"],pch=1,cex=2,col="darkred")
# points(df2$Estimated.Divergence..mya.[df2$Trained.On=="5 Species"],
#        df2$Accuracy..50.[df2$Trained.On=="5 Species"],pch=0,cex=2.5,col="darkblue")
# points(df2$Estimated.Divergence..mya.[df2$Trained.On=="5 Species Balanced"],
#        df2$Accuracy..50.[df2$Trained.On=="5 Species Balanced"],pch=1,cex=2.5,col="darkblue")
# 
# points(df2$Estimated.Divergence..mya.[df2$Trained.On=="5 Species + Melozone fusca"],
#        df2$Accuracy..5.[df2$Trained.On=="5 Species + Melozone fusca"],pch=7,cex=2,col="darkred")
# points(df2$Estimated.Divergence..mya.[df2$Trained.On=="5 Species Balanced + Melozone fusca"],
#        df2$Accuracy..5.[df2$Trained.On=="5 Species Balanced + Melozone fusca"],pch=13,cex=2,col="darkred")
# points(df2$Estimated.Divergence..mya.[df2$Trained.On=="5 Species + Melozone fusca"],
#        df2$Accuracy..50.[df2$Trained.On=="5 Species + Melozone fusca"],pch=7,cex=2.5,col="darkblue")
# points(df2$Estimated.Divergence..mya.[df2$Trained.On=="5 Species Balanced + Melozone fusca"],
#        df2$Accuracy..50.[df2$Trained.On=="5 Species Balanced + Melozone fusca"],pch=13,cex=2.5,col="darkblue")
# 
# 
# dev.off()
# }

## time vs accuracy
#plot(df$Training.Time..5..sec.,df$Accuracy..5.,col="darkred",pch=16)
#plot(df$Training.Time..50..sec.,df$Accuracy..50.,col="darkblue",pch=16)
plot(df$Training.Time..100..sec.,df$Accuracy..100.,col="darkblue",pch=16)

## dist vs accuracy
plot(df$Estimated.Divergence..mya.,df$Accuracy..100.,col="darkred",pch=16)
mod5 = lm(df$Accuracy..100.~df$Estimated.Divergence..mya.)
abline(mod5,col="red")
points(df$Estimated.Divergence..mya.[df$Type=="Multiple species"],df$Accuracy..100.[df$Type=="Multiple species"],col="darkred",pch=0,cex=2)

summary(mod5)
mod5_a=aov(df$Accuracy..100.~df$Estimated.Divergence..mya.+df$Tested.On)
summary(mod5_a)
abline(lm(df$Accuracy..100.[df$Tested.On=="Calypte anna"]~df$Estimated.Divergence..mya.[df$Tested.On=="Calypte anna"]),lty=3,col="red")
abline(lm(df$Accuracy..100.[df$Tested.On=="Zonotrichia leucophrys"]~df$Estimated.Divergence..mya.[df$Tested.On=="Zonotrichia leucophrys"]),lty=3,col="red")
abline(lm(df$Accuracy..100.[df$Tested.On=="Melozone fusca"]~df$Estimated.Divergence..mya.[df$Tested.On=="Melozone fusca"]),lty=3,col="red")
abline(lm(df$Accuracy..100.[df$Tested.On=="Cardinalis cardinalis"]~df$Estimated.Divergence..mya.[df$Tested.On=="Cardinalis cardinalis"]),lty=3,col="red")
abline(lm(df$Accuracy..100.[df$Tested.On=="Cardinalis sinuatus"]~df$Estimated.Divergence..mya.[df$Tested.On=="Cardinalis sinuatus"]),lty=3,col="red")
abline(lm(df$Accuracy..100.[df$Tested.On=="Empidonax virescens"]~df$Estimated.Divergence..mya.[df$Tested.On=="Empidonax virescens"]),lty=3,col="red")

 
# plot(df$Estimated.Divergence..mya.,df$Accuracy..5.,col="darkred",pch=16)
# mod5 = lm(df$Accuracy..5.~df$Estimated.Divergence..mya.)
# abline(mod5,col="red")
# points(df$Estimated.Divergence..mya.[df$Type=="Multiple species"],df$Accuracy..5.[df$Type=="Multiple species"],col="darkred",pch=0,cex=2)
# 
# summary(mod5)
# mod5_a=aov(df$Accuracy..5.~df$Estimated.Divergence..mya.+df$Tested.On)
# summary(mod5_a)
# abline(lm(df$Accuracy..5.[df$Tested.On=="Calypte anna"]~df$Estimated.Divergence..mya.[df$Tested.On=="Calypte anna"]),lty=3,col="red")
# abline(lm(df$Accuracy..5.[df$Tested.On=="Zonotrichia leucophrys"]~df$Estimated.Divergence..mya.[df$Tested.On=="Zonotrichia leucophrys"]),lty=3,col="red")
# abline(lm(df$Accuracy..5.[df$Tested.On=="Melozone fusca"]~df$Estimated.Divergence..mya.[df$Tested.On=="Melozone fusca"]),lty=3,col="red")
# abline(lm(df$Accuracy..5.[df$Tested.On=="Cardinalis cardinalis"]~df$Estimated.Divergence..mya.[df$Tested.On=="Cardinalis cardinalis"]),lty=3,col="red")
# abline(lm(df$Accuracy..5.[df$Tested.On=="Cardinalis sinuatus"]~df$Estimated.Divergence..mya.[df$Tested.On=="Cardinalis sinuatus"]),lty=3,col="red")
# abline(lm(df$Accuracy..5.[df$Tested.On=="Empidonax virescens"]~df$Estimated.Divergence..mya.[df$Tested.On=="Empidonax virescens"]),lty=3,col="red")
# 
# 
# plot(df$Estimated.Divergence..mya.,df$Accuracy..50.,col="darkblue",pch=16)
# mod50 = lm(df$Accuracy..50.~df$Estimated.Divergence..mya.)
# abline(mod50,col="blue")
# points(df$Estimated.Divergence..mya.[df$Type=="Multiple species"],df$Accuracy..50.[df$Type=="Multiple species"],col="darkblue",pch=0,cex=2)
# 
# summary(mod50)
# mod50_a=aov(df$Accuracy..50.~df$Estimated.Divergence..mya.+df$Tested.On)
# summary(mod50_a)
# abline(lm(df$Accuracy..50.[df$Tested.On=="Calypte anna"]~df$Estimated.Divergence..mya.[df$Tested.On=="Calypte anna"]),lty=3,col="blue")
# abline(lm(df$Accuracy..50.[df$Tested.On=="Zonotrichia leucophrys"]~df$Estimated.Divergence..mya.[df$Tested.On=="Zonotrichia leucophrys"]),lty=3,col="blue")
# abline(lm(df$Accuracy..50.[df$Tested.On=="Melozone fusca"]~df$Estimated.Divergence..mya.[df$Tested.On=="Melozone fusca"]),lty=3,col="blue")
# abline(lm(df$Accuracy..50.[df$Tested.On=="Cardinalis cardinalis"]~df$Estimated.Divergence..mya.[df$Tested.On=="Cardinalis cardinalis"]),lty=3,col="blue")
# abline(lm(df$Accuracy..50.[df$Tested.On=="Cardinalis sinuatus"]~df$Estimated.Divergence..mya.[df$Tested.On=="Cardinalis sinuatus"]),lty=3,col="blue")
# abline(lm(df$Accuracy..50.[df$Tested.On=="Empidonax virescens"]~df$Estimated.Divergence..mya.[df$Tested.On=="Empidonax virescens"]),lty=3,col="blue")

# plot(df$Estimated.Divergence..mya.[df$Type=="Multiple species"],df$Accuracy..50.[df$Type=="Multiple species"],col="darkblue",pch=0,cex=2)
# mod50_m = lm(df$Accuracy..50.[df$Type=="Multiple species"]~df$Estimated.Divergence..mya.[df$Type=="Multiple species"])
# abline(mod50_m,col="blue"); summary(mod50_m)
# 
# plot(df$Estimated.Divergence..mya.[df$Type=="Multiple species"],df$Accuracy..5.[df$Type=="Multiple species"],col="darkred",pch=0,cex=2)
# mod5_m = lm(df$Accuracy..5.[df$Type=="Multiple species"]~df$Estimated.Divergence..mya.[df$Type=="Multiple species"])
# abline(mod5_m,col="red"); summary(mod50_m)



## sample size training vs acc

plot(df$Training.N,df$Accuracy..100.,col="darkblue",pch=16)
mod50=lm(df$Accuracy..100.~df$Training.N)
abline(mod5,col="blue"); summary(mod50)

plot(df$Test.N,df$Accuracy..100.,col="darkred",pch=16)
mod5=lm(df$Accuracy..100.~df$Test.N)
abline(mod5,col="red"); summary(mod5)

# plot(df$Training.N,df$Accuracy..5.,col="darkred",pch=16)
# mod5=lm(df$Accuracy..5.~df$Training.N)
# abline(mod5,col="red"); summary(mod5)
# 
# plot(df$Training.N,df$Accuracy..50.,col="darkblue",pch=16)
# mod50=lm(df$Accuracy..50.~df$Training.N)
# abline(mod5,col="blue"); summary(mod50)
# 
# plot(df$Test.N,df$Accuracy..5.,col="darkred",pch=16)
# mod5=lm(df$Accuracy..5.~df$Test.N)
# abline(mod5,col="red"); summary(mod5)
# 
# plot(df$Test.N,df$Accuracy..50.,col="darkblue",pch=16)
# mod50=lm(df$Accuracy..50.~df$Test.N)
# abline(mod5,col="blue"); summary(mod50)


plot(log10(df$Training.N),df$Accuracy..5.,col="darkred",pch=16)
mod5=lm(df$Accuracy..5.~log10(df$Training.N))
abline(mod5,col="red"); summary(mod5)

plot(log10(df$Training.N),df$Accuracy..50.,col="darkblue",pch=16)
mod50=lm(df$Accuracy..50.~log10(df$Training.N))
abline(mod5,col="blue"); summary(mod50)

plot(log10(df$Test.N),df$Accuracy..5.,col="darkred",pch=16)
mod5=lm(df$Accuracy..5.~log10(df$Test.N))
abline(mod5,col="red"); summary(mod5)

plot(log10(df$Test.N),df$Accuracy..50.,col="darkblue",pch=16)
mod50=lm(df$Accuracy..50.~log10(df$Test.N))
abline(mod5,col="blue"); summary(mod50)





plot(log10(df$Training.N[df$Type=="Single species"]),df$Accuracy..5.[df$Type=="Single species"],col="darkred",pch=16)
mod5=lm(df$Accuracy..5.[df$Type=="Single species"]~log10(df$Training.N[df$Type=="Single species"]))
abline(mod5,col="red"); summary(mod5)

plot(log10(df$Training.N[df$Type=="Single species"]),df$Accuracy..50.[df$Type=="Single species"],col="darkblue",pch=16)
mod50=lm(df$Accuracy..50.[df$Type=="Single species"]~log10(df$Training.N[df$Type=="Single species"]))
abline(mod5,col="blue"); summary(mod50)

plot(log10(df$Test.N[df$Type=="Single species"]),df$Accuracy..5.[df$Type=="Single species"],col="darkred",pch=16)
mod5=lm(df$Accuracy..5.[df$Type=="Single species"]~log10(df$Test.N[df$Type=="Single species"]))
abline(mod5,col="red"); summary(mod5)

plot(log10(df$Test.N[df$Type=="Single species"]),df$Accuracy..50.[df$Type=="Single species"],col="darkblue",pch=16)
mod50=lm(df$Accuracy..50.[df$Type=="Single species"]~log10(df$Test.N[df$Type=="Single species"]))
abline(mod5,col="blue"); summary(mod50)

## accuracy vs species

sppmod5=aov(df$Accuracy..5.~df$Tested.On)
summary(sppmod5)
TukeyHSD(sppmod5)


sppmod50=aov(df$Accuracy..50.~df$Tested.On)
summary(sppmod50)
TukeyHSD(sppmod50)



## hypervolumes

df_u = unique(df[,c("Test.M.F.Complexity","Test.Hypervolume.Complexity.Mean","Test.Hypervolume.Complexity.SD")])

cor(unique(df[,c("Test.M.F.Complexity","Test.Hypervolume.Complexity.Mean","Test.Hypervolume.Complexity.SD")]),
    use="pairwise.complete.obs")

summary(lm(df_u$Test.Hypervolume.Complexity.SD~df_u$Test.Hypervolume.Complexity.Mean))

plot(log10(df$Test.Hypervolume.Complexity.Mean),
     log10(df$Test.Hypervolume.Complexity.SD))


par(mfrow=c(3,2))

plot(df$Train.M.F.Complexity,df$Accuracy..5.)
mod=lm(df$Accuracy..5.~df$Train.M.F.Complexity)
summary(mod)
abline(mod,col="red")
plot(df$Train.M.F.Complexity,df$Accuracy..50.)
mod=lm(df$Accuracy..50.~df$Train.M.F.Complexity)
summary(mod)
abline(mod,col="red")

plot(log10(df$Train.Hypervolume.Complexity.Mean),df$Accuracy..5.)
mod=lm(df$Accuracy..5.~log10(df$Train.Hypervolume.Complexity.Mean))
summary(mod)
abline(mod,col="red")
plot(log10(df$Train.Hypervolume.Complexity.Mean),df$Accuracy..50.)
mod=lm(df$Accuracy..50.~log10(df$Train.Hypervolume.Complexity.Mean))
summary(mod)
abline(mod,col="red")

plot(log10(df$Train.Hypervolume.Complexity.SD),df$Accuracy..5.)
mod=lm(df$Accuracy..5.~log10(df$Train.Hypervolume.Complexity.SD))
summary(mod)
abline(mod,col="red")
plot(log10(df$Train.Hypervolume.Complexity.SD),df$Accuracy..50.)
mod=lm(df$Accuracy..50.~log10(df$Train.Hypervolume.Complexity.SD))
summary(mod)
abline(mod,col="red")

par(mfrow=c(3,2))
plot(df$Test.M.F.Complexity,df$Accuracy..5.)
mod=lm(df$Accuracy..5.~df$Test.M.F.Complexity)
summary(mod)
abline(mod,col="red")
plot(df$Test.M.F.Complexity,df$Accuracy..50.)
mod=lm(df$Accuracy..50.~df$Test.M.F.Complexity)
summary(mod)
abline(mod,col="red")

## THESE ARE SIGNIFICANT
plot(log10(df$Test.Hypervolume.Complexity.Mean),df$Accuracy..5.)
mod=lm(df$Accuracy..5.~log10(df$Test.Hypervolume.Complexity.Mean))
summary(mod)
abline(mod,col="red")
plot(log10(df$Test.Hypervolume.Complexity.Mean),df$Accuracy..50.)
mod=lm(df$Accuracy..50.~log10(df$Test.Hypervolume.Complexity.Mean))
summary(mod)
abline(mod,col="red")

plot(log10(df$Test.Hypervolume.Complexity.SD),df$Accuracy..5.)
mod=lm(df$Accuracy..5.~log10(df$Test.Hypervolume.Complexity.SD))
summary(mod)
abline(mod,col="red")
plot(log10(df$Test.Hypervolume.Complexity.SD),df$Accuracy..50.)
mod=lm(df$Accuracy..50.~log10(df$Test.Hypervolume.Complexity.SD))
summary(mod)
abline(mod,col="red")


runbelow=F
if(runbelow==T){
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
points(agg[agg$test=="CA" & agg$TRAN==1,c("DIS","acc")],ylim=c(0.6,1),type="b",pch=0,col="black",lty=2)
points(agg[agg$test=="CC" & agg$TRAN==1,c("DIS","acc")],ylim=c(0.6,1),type="b",pch=0,col="red",lty=2)
points(agg[agg$test=="CS" & agg$TRAN==1,c("DIS","acc")],ylim=c(0.6,1),type="b",pch=0,col="blue",lty=2)
points(agg[agg$test=="EV" & agg$TRAN==1,c("DIS","acc")],ylim=c(0.6,1),type="b",pch=0,col="green",lty=2)
points(agg[agg$test=="ZL" & agg$TRAN==1,c("DIS","acc")],ylim=c(0.6,1),type="b",pch=0,col="goldenrod",lty=2)
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
abline(h=seq(0,50,1),col="grey",lty=2)
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
}

