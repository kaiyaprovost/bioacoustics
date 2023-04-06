# file9SPP = "/Users/kprovost/Documents/Postdoc_Working/Finished_Models/9SppBalanced/results_220823_135425/9SppBalanced_train_220823_135425.log.ACCURACY.csv"
# fileMF = "/Users/kprovost/Documents/Postdoc_Working/Finished_Models/Melozone.fusca/MF_trainlog_2022-09-08_16-14-37.txt.ACCURACY.csv"
# fileCA="/Users/kprovost/Documents/Postdoc_Working/Finished_Models/Calypte.anna/LOGS/CA_trainlog_2022-08-17_15-12-12.txt.ACCURACY.csv"
# fileCC="/Users/kprovost/Documents/Postdoc_Working/Finished_Models/Cardinalis.cardinalis/LOGS/Cardinalis.cardinalis_trainlog_2022-08-12_08-49-49.txt.ACCURACY.csv"
# fileCS="/Users/kprovost/Documents/Postdoc_Working/Finished_Models/Cardinalis.sinuatus/results_220811_112545/Cardinalis.sinuatus_train_220811_112545.log.ACCURACY.csv"
# fileEV="/Users/kprovost/Documents/Postdoc_Working/Finished_Models/Empidonax.virescens/results_220811_135235/Empidonax.virescens_train_220811_135235.log.ACCURACY.csv"
# fileMT="/Users/kprovost/Documents/Postdoc_Working/Finished_Models/Myiarchus.tuberculifer/results_220803_144442/Myiarchus.tuberculifer_train_220803_144442.log.ACCURACY.csv"
# filePA="/Users/kprovost/Documents/Postdoc_Working/Finished_Models/Passerina.amoena/results_220812_085458/Passerina.amoena_train_220812_085458.log.ACCURACY.csv"
# filePC="/Users/kprovost/Documents/Postdoc_Working/Finished_Models/Poecile.carolinensis/results_220804_111329/Poecile.carolinensis_train_220804_111329.log.ACCURACY.csv"
# fileVA="/Users/kprovost/Documents/Postdoc_Working/Finished_Models/Vireo.altiloquus/results_220802_171214/Vireo.altiloquus_train_220802_171214.log.ACCURACY.csv"
# fileZL="/Users/kprovost/Documents/Postdoc_Working/Finished_Models/Zonotrichia.leucophrys/results_220812_084943/Zonotrichia.leucophrys_train_220812_084943.log.ACCURACY.csv"
# 
# files=c(file9SPP,fileCA,fileCC,fileCS,fileEV,fileMF,fileMT,filePA,filePC,fileVA,fileZL)

#files=c("/Users/kprovost/Documents/Postdoc_Working/MMRR/Performance_FineTune_NoMFZL/Passerellidae_FULL_noMFZL_trainlog_2022-10-27_18-45-45.txt")
files=c("/Users/kprovost/Downloads/9SppBalanced_trainlog_2022-08-23_13-54-20.txt")

## process file
df_raw = readLines(files[1])
df_acc = df_raw[grep("acc",df_raw,ignore.case=T)]
df_avg = df_acc[grep("avg_acc",df_acc,ignore.case = T)]
df_chk = df_acc[grep("Accuracy",df_acc,ignore.case = T)]

standings=sapply(df_chk,FUN=function(x){
  y=strsplit(x,"-")[[1]][6]
})
names(standings)=NULL
standings[standings==" Accuracy on validation set improved. Saving max"]=0
standings=gsub(" Accuracy has not improved in ","",standings)
standings=gsub(" validation steps. Not saving max","",standings)
standings[standings==" Stopping training early, accuracy has not improved in 10 validation steps."]=max(as.numeric(standings),na.rm=T)
standings=as.numeric(standings)

accs = lapply(df_avg,FUN=function(x){
  y=strsplit(x,"-")[[1]]
  y=y[6:length(y)]
  y=paste(y,sep="-")
  z=strsplit(y,",")[[1]]
  names(z) = c("avg_acc","avg_levenshtein","avg_segment_error_rate","avg_loss")
  z=gsub(" avg_acc: ","",z)
  z=gsub(" avg_levenshtein: ","",z)
  z=gsub(" avg_segment_error_rate: ","",z)
  z=gsub(" avg_loss: ","",z)
  return(z)
})
accs_df = do.call(rbind,accs)
accs_df = as.data.frame(accs_df)
accs_df$RUN = 1:nrow(accs_df)
#accs_df$CHECKPOINT = max(standings,na.rm=T)
accs_df$CHECKPOINT=standings

par(mfrow=c(1,2))
plot(accs_df$RUN,accs_df$avg_acc,
      col="grey",type="l")
points(accs_df$RUN,accs_df$avg_acc,
     col=ifelse(accs_df$CHECKPOINT==0,"red","black"),
     pch=ifelse(accs_df$CHECKPOINT==0,0,1),
     type="p")
abline(v=which(accs_df$CHECKPOINT==0),col="red")
plot(accs_df$RUN,accs_df$avg_segment_error_rate,
     col="grey",type="l")
points(accs_df$RUN,accs_df$avg_segment_error_rate,
       col=ifelse(accs_df$CHECKPOINT==0,"red","black"),
       pch=ifelse(accs_df$CHECKPOINT==0,0,1),
       type="p")
abline(v=which(accs_df$CHECKPOINT==0),col="red")

right_before = which(accs_df$CHECKPOINT==0)-1
right_before=right_before[right_before!=0]
accs_df$CHECKPOINT[right_before]

plot(accs_df$CHECKPOINT[right_before],accs_df$avg_acc[right_before+1])

plot(accs_df$RUN[accs_df$CHECKPOINT==0],
     accs_df$avg_acc[accs_df$CHECKPOINT==0])

dflist=lapply(files,FUN=function(filen){
  df = read.csv(filen)
  #df$FILENAME = basename(filen)
  df$SPP = strsplit(basename(filen),"_")[[1]][1]
  return(df)
})
df = do.call(rbind,dflist)

colors_trans=c(
  rgb(0,0,0,0.1),
  rgb(0,0,1,0.1),
  rgb(0,1,0,0.1),
  rgb(1,0,0,0.1),
  rgb(1,1,0,0.1),
  rgb(0,1,1,0.1),
  rgb(1,0,1,0.1),
  rgb(0.5,1,1,0.1),
  rgb(1,0.5,1,0.1),
  rgb(1,1,0.5,0.1),
  rgb(0.5,0.5,1,0.1),
  rgb(1,0.5,0.5,0.1)
)
colors_full=c(
  rgb(0,0,0,1),
  rgb(0,0,1,1),
  rgb(0,1,0,1),
  rgb(1,0,0,1),
  rgb(1,1,0,1),
  rgb(0,1,1,1),
  rgb(1,0,1,1),
  rgb(0.5,1,1,1),
  rgb(1,0.5,1,1),
  rgb(1,1,0.5,1),
  rgb(0.5,0.5,1,1),
  rgb(1,0.5,0.5,1)
)

pdf("~/test_training2.pdf")
par(mfrow=c(1,2))
for(i in 1:length(sort(unique(df$SPP)))){
  spp=sort(unique(df$SPP))[i]
  dfsm = df[df$SPP==spp,]
  plot(df$ITER,df$avg_acc,type="p",col=colors_trans[as.numeric(as.factor(df$SPP))],main=spp,ylim=c(0.7,1))
  points(dfsm$ITER,dfsm$avg_acc,type="l",col=colors_full[i])
  plot(df$ITER,df$avg_segment_error_rate,type="p",col=colors_trans[as.numeric(as.factor(df$SPP))],ylim=c(0.5,20))
  points(dfsm$ITER,dfsm$avg_segment_error_rate,type="l",col=colors_full[i])
  
}
dev.off()

pdf("~/test_training.pdf")
par(mfrow=c(1,2))
for(filen in files){
  dfsm = read.csv(filen)
  spp = strsplit(basename(filen),"_")[[1]][1]
  plot(dfsm$ITER,dfsm$avg_acc,type="l",main=spp,ylim=c(0.7,1))
  plot(dfsm$ITER,dfsm$avg_segment_error_rate,type="l",ylim=c(0.5,20))
}
dev.off()
