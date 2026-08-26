## need may 9 to may 27th 
path = "/Users/kprovost/Documents/Research/Caro_Provost_Transit_Noise/"
my_files <- list.files(path=path,pattern="txt$",full.names = T,recursive = T)
my_files <- my_files[!grepl("TABLE",my_files)]
my_files <- my_files[!grepl("species_list",my_files)]
my_files <- my_files[!grepl("JulianDate",my_files)]
my_files <- my_files[!grepl("smli",my_files)]
my_files <- my_files[!grepl("state park",my_files)]
my_files <- my_files[!grepl("test",my_files)]
my_files <- my_files[!grepl("FieldSites",my_files)]

keep_dates <- c(20250510:20250513,
                20250517:20250520,
                20250524:20250527,
                20260509:20260512,
                20260516:20260519,
                20260523:20260526)

my_df_list <- lapply(my_files,FUN=function(x){
  print(x)
  df <- read.table(x,header=T,sep="\t",quote=NULL,fill = T)
  my_col_names <- colnames(df)
  if("Date" %in% my_col_names) {
    df <- df[df$Date %in% keep_dates,]
    return(df)
  } else if("Begin.Path" %in% my_col_names) {
    df <- df[grepl("202505",df$Begin.Path) | grepl("202605",df$Begin.Path),]
    return(df)
  } else {
    print(my_col_names)
  }
})
my_df <- do.call(gtools::smartbind,my_df_list)
write.table(my_df,"/Users/kprovost/Documents/Research/Caro_Provost_Transit_Noise/very_large_temp_file.txt",sep="\t",
            row.names=F,quote=F)
my_df <- my_df[,c("Common.Name","Confidence","Begin.Path","Deployment","Locality","Sublocality",
                  "Recorder","DateTime","Date","Time")]
my_df <- unique(my_df)
df <- read.table("/Users/kprovost/Documents/Research/Caro_Provost_Transit_Noise/very_large_temp_file.txt",
                 quote=NULL,fill=T,header=T,sep="\t")
df$Time[is.na(df$Time)] <- 10000000
df$Time[df$Time>10000000] <- df$DateTime[df$Time>10000000]
df$Time[df$Time>10000000] <- 0
df <- df[df$Time!=0,]
df <- df[df$Time!=-1,]
df <- df[complete.cases(df),]
df <- df[df$Deployment!="examples",]
df <- df[!(df$Recorder %in% c(20250524:20250531)),]
df <- df[df$Date %in% keep_dates,]
df$DateTime <- paste(df$Date,df$Time,sep="_")
df$Time[df$Time<70000] <- df$Time[df$Time<70000] + 40000

write.table(df,"/Users/kprovost/Documents/Research/Caro_Provost_Transit_Noise/long_island_strike_file.txt",sep="\t",
            row.names=F,quote=F)
## aggregate most confident detection by columns
agg <- aggregate(df$Confidence~df$Common.Name+df$Deployment+df$Locality+df$Sublocality+df$Recorder+df$Date+df$Time,
                 FUN=function(x){max(x,na.rm=T)})
colnames(agg) <- gsub("df\\$","",colnames(agg))
head(agg)
write.table(agg,"/Users/kprovost/Documents/Research/Caro_Provost_Transit_Noise/AGGREGATED_long_island_strike_file.txt",sep="\t",
            row.names=F,quote=F)

agg$Strike <- 0
agg$Strike[agg$Date %in% c(20250510:20250513)] <- "Before_2025"
agg$Strike[agg$Date %in% c(20250517:20250520)] <- "During_2025"
agg$Strike[agg$Date %in% c(20250524:20250527)] <- "Ending_2025"
agg$Strike[agg$Date %in% c(20260509:20260512)] <- "Before_2026"
agg$Strike[agg$Date %in% c(20260516:20260519)] <- "During_2026"
agg$Strike[agg$Date %in% c(20260523:20260526)] <- "Ending_2026"
agg <- agg[agg$Recorder!="KProvost_18",]


write.table(agg,"/Users/kprovost/Documents/Research/Caro_Provost_Transit_Noise/AGGREGATED_long_island_strike_file.txt",sep="\t",
            row.names=F,quote=F)

## this is definitely not all the data 
## subset only to localities that we have recordings for during all six periods
## or if not enough data, pairs 
before_during <- c(
  "Riparian",
  "Woods",
  "Perimeter",
  "GCBSMeadow",
  "ClarkTracks",
  "RoseGarden",
  "Field",
  "Meadow",
  "ParcelD"
)
ending_list <- c(
  "Generator", 
  "UC",
  "BasinTrail",
  "SouthAve",
  "InnerTrail",
  "TrainTracks",
  "GCBSMeadow",
  "ClarkTracks",
  "RoseGarden",
  "Field",
  "Meadow",
  "ParcelD"
)

## how do we analyze this 
table(agg$Deployment)
table(agg$Recorder,agg$Date)

small <- unique(agg[,c("Recorder","Date","Time")])

table(small$Recorder,small$Date)

## generate actual recorder_date pairs
agg$RecorderDate <- paste(agg$Recorder,agg$Date,sep="_")
agg <- agg[agg$Common.Name!="nocall",]
noises <- c("Human vocal","Gun","Siren",
            "Power tools","Dog","Engine",
            "Fireworks","Human vocal","Human non-vocal")
agg_noise <- agg[agg$Common.Name %in% noises,]
agg <- agg[!(agg$Common.Name %in% noises),]


## we need to know which recorders were offline on which days 
before <- agg[agg$Sublocality %in% before_during,]
during <- agg[agg$Sublocality %in% before_during,]
ending <- agg[agg$Sublocality %in% ending_list,]
before <- before[before$Strike %in% c("Before_2025","Before_2026"),]
during <- during[during$Strike %in% c("During_2025","During_2026"),]
ending <- ending[ending$Strike %in% c("Ending_2025","Ending_2026"),]

before <- before[before$Recorder!="KProvost_18",]
ending <- ending[ending$Recorder!="KProvost_18",]


write.table(unique(agg[,c("Sublocality","RecorderDate","Strike")]),"~/test.txt",sep="\t",row.names=F,quote=F)

table(before$Strike)
table(before$Sublocality[before$Strike=="Before_2025"])
table(before$Sublocality[before$Strike=="Before_2026"])
table(before$RecorderDate[before$Sublocality=="GCBSMeadow"])
table(during$RecorderDate[during$Sublocality=="Riparian"])
table(ending$RecorderDate[ending$Sublocality=="GCBSMeadow"])

table(before$Sublocality,before$Strike)
table(during$Sublocality,during$Strike)
table(ending$Sublocality,ending$Strike)

## aggregating counts
b25 <- as.data.frame(table(agg[agg$Strike=="Before_2025",c("Common.Name","Sublocality")]))
b26 <- as.data.frame(table(agg[agg$Strike=="Before_2026",c("Common.Name","Sublocality")]))
d25 <- as.data.frame(table(agg[agg$Strike=="During_2025",c("Common.Name","Sublocality")]))
d26 <- as.data.frame(table(agg[agg$Strike=="During_2026",c("Common.Name","Sublocality")]))
e25 <- as.data.frame(table(agg[agg$Strike=="Ending_2025",c("Common.Name","Sublocality")]))
e26 <- as.data.frame(table(agg[agg$Strike=="Ending_2026",c("Common.Name","Sublocality")]))
colnames(b25)[3] <- "Before_2025"
colnames(b26)[3] <- "Before_2026"
colnames(d25)[3] <- "During_2025"
colnames(d26)[3] <- "During_2026"
colnames(e25)[3] <- "Ending_2025"
colnames(e26)[3] <- "Ending_2026"
b2X <- merge(b25,b26,all=T)
d2X <- merge(d25,d26,all=T)
e2X <- merge(e25,e26,all=T)
bd <- merge(b2X,d2X,all=T)
bde <- merge(bd,e2X,all=T)
write.table(bde,"/Users/kprovost/Documents/Research/Caro_Provost_Transit_Noise/TABLE_long_island_strike_file.txt",sep="\t",
            row.names=F,quote=F)

## these are sans engine sounds and standardized by recording effort
bde_2 <- read.table("/Users/kprovost/Documents/Research/Caro_Provost_Transit_Noise/TABLE_long_island_strikes_by_day.txt",
                    header=T,sep="\t",row.names = 1)
boxplot(bde_2)
boxplot(t(bde_2),las=2)

bde_2_df <- as.data.frame(as.table(as.matrix(bde_2)))
colnames(bde_2_df) <- c("Sublocality","Strike","Detections")
bde_2_df <- bde_2_df[complete.cases(bde_2_df),]
mod <- aov(bde_2_df$Detections~bde_2_df$Sublocality+bde_2_df$Strike)
summary(mod)
TukeyHSD(mod)

## rarefaction curve, one per each recorder date per locality?
## maybe do it for each sublocality
1:length(unique(agg$RecorderDate)) ## ~590 recorder dates
rare_df <- data.frame(n=NULL,i=NULL,nspp=NULL,subloc=NULL)
for(subloc in sort(unique(agg$Sublocality))) {
  print(subloc)
  agg_subloc <- agg[agg$Sublocality==subloc,]
  rd <- sort(unique(agg_subloc$RecorderDate))
  nrd <- length(rd)
  for(n in 1:nrd) {
    ## do this 20 times and generate 95% conf interval
    for (i in 1:100) {
      my_samp <- sample(rd,n)
      agg_samp <- agg_subloc[agg_subloc$RecorderDate %in% my_samp,]
      nspp <- length(unique(agg_samp$Common.Name))
      my_row <- cbind(n,i,nspp,subloc)
      rare_df <- rbind(rare_df,my_row)
    }
    
  }
}
head(rare_df)
plot(rare_df$n,rare_df$nspp,col=as.numeric(as.factor(rare_df$subloc)))
rare_df$nspp <- as.numeric(rare_df$nspp)
## calculate the 95% CI from the rarefaction data for each site 

q05 <- aggregate(rare_df$nspp~rare_df$n+rare_df$subloc,FUN=function(x){
  quantile(x,0.05)
})
q95 <- aggregate(rare_df$nspp~rare_df$n+rare_df$subloc,FUN=function(x){
  quantile(x,0.95)
})
qmean <- aggregate(rare_df$nspp~rare_df$n+rare_df$subloc,FUN=function(x){
  mean(x,na.rm=T)
})
colnames(q05) <- c("n","subloc","nspp05")
colnames(q95) <- c("n","subloc","nspp95")
colnames(qmean) <- c("n","subloc","nsppmean")
q_all <- merge(q05,q95,all=T)
q_all <- merge(q_all,qmean,all=T)
q_all$n <- as.numeric(q_all$n)
q_all <- q_all[order(q_all$n),]
write.table(q_all,"~/rarefaction.txt")

par(mfrow=c(5,5),mar=c(3,3,0,0))
for(loc in sort(unique(q_all$subloc))) {
  plot(q_all$n,q_all$nsppmean,type="n",
       ylab="N Species",xlab="N RecorderDates",
       main=loc)
  my_loc <- q_all[q_all$subloc==loc,]
  polygon(x=c(my_loc$n,rev(my_loc$n)),
          y=c(my_loc$nspp05,rev(my_loc$nspp95)),
          col=rgb(1,0,0,0.1))
  lines(my_loc$n,my_loc$nsppmean)
  
}

temp <- read.table("/Users/kprovost/Documents/Research/Caro_Provost_Transit_Noise/BirdNET_SelectionTable_17Feb2026_AGGREGATED.txt",header=T,sep="\t",quote=NULL,fill=T)
temp2 <- read.table("/Users/kprovost/Documents/Research/Caro_Provost_Transit_Noise/BirdNET_Master_10March2026.txt",header=T,sep="\t",quote=NULL,fill=T)
temp2a <- aggregate(temp2$Confidence~temp2$Begin.Path+temp2$Common.Name,FUN=function(x){max(x,na.rm=T)})
colnames(temp2a) <- c("Begin.Path","Common.Name","Confidence")
temp2a_dates <- sapply((temp2a$Begin.Path),FUN=function(x){
  strsplit(x,"/")[[1]][6]
})
temp2a$Date <- temp2a_dates
temp2_dates <- sapply((temp$Date),FUN=function(x){strsplit(unique(x),"_")[[1]][1]})
temp$Date <- temp2_dates
temp2_dates <- sort(unique(temp2_dates))
temp2a_dates <- sort(unique(sapply(unique(temp2a_dates),FUN=function(x){strsplit(unique(x),"_")[[1]][1]})))
temp_dates <- c(temp2_dates,temp2a_dates)
temp_dates <- as.numeric(unique(temp_dates))
temp_dates <- temp_dates[temp_dates>=20240101]
temp_dates <- temp_dates[temp_dates<=20261231]
temp_dates <- temp_dates[!is.na(temp_dates)]

temp <- merge(temp,temp2a,by="Date",all=T)
## rarefaction curve by date
## rarefaction curve, one per each recorder date per locality?
## maybe do it for each sublocality
rare_df2 <- data.frame(n=NULL,i=NULL,nspp=NULL)
nrd <- length(temp_dates)
for(n in 1:nrd) {
  print(n)
  ## do this 20 times and generate 95% conf interval
  for (i in 1:100) {
    my_samp <- sample(temp_dates,n)
    samp1 <- unique(temp$Common.Name[temp$Date %in% my_samp])
    samp2 <- unique(temp2a$Common.Name[temp2a$Date %in% my_samp])
    nspp <- length(unique(c(samp1,samp2)))
    my_row <- cbind(n,i,nspp)
    rare_df2 <- rbind(rare_df2,my_row)
  }
}
head(rare_df2)



rare_df2$nspp <- as.numeric(rare_df2$nspp)
## calculate the 95% CI from the rarefaction data for each site 

q05 <- aggregate(rare_df2$nspp~rare_df2$n,FUN=function(x){
  quantile(x,0.05)
})
q95 <- aggregate(rare_df2$nspp~rare_df2$n,FUN=function(x){
  quantile(x,0.95)
})
qmean <- aggregate(rare_df2$nspp~rare_df2$n,FUN=function(x){
  mean(x,na.rm=T)
})
colnames(q05) <- c("n","nspp05")
colnames(q95) <- c("n","nspp95")
colnames(qmean) <- c("n","nsppmean")
q_all2 <- merge(q05,q95,all=T)
q_all2 <- merge(q_all2,qmean,all=T)
q_all2$n <- as.numeric(q_all2$n)
q_all2 <- q_all2[order(q_all2$n),]
write.table(q_all2,"~/rarefaction2.txt")

dim(q_all2)
plot(q_all2$n,q_all2$nsppmean,type="n",
     ylab="N Species",xlab="N Dates")
polygon(x=c(q_all2$n,rev(q_all2$n)),
        y=c(q_all2$nspp05,rev(q_all2$nspp95)),
        col=rgb(1,0,0,0.1))
lines(q_all2$n,q_all2$nsppmean)

## need to subset to only the good birds