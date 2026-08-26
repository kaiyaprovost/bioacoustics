#remotes::install_github("maRce10/suwo")
library(suwo)
df <- query_xenocanto(species='sp:"Calidris himantopus"',
                api_key = "e7ef1c5168ec27bafe26facc3451b137f47a06f2",
                verbose=T)
#write.table(df,"~/Documents/Research/Student_Projects/Calidris/calidris_C_18nov2025.csv",
#            sep=",")
download_media(df,path="~/Documents/Research/Student_Projects/Calidris/")

genera <- c("Aimophila","Ammodramus","Ammospiza","Amphispiza","Amphispizopsis","Arremon","Arremonops","Artemisiospiza","Atlapetes","Calamospiza","Centronyx","Chlorospingus","Chondestes","Junco","Melospiza","Melozone","Oriturus","Passerculus","Passerella","Peucaea","Pipilo","Pooecetes","Rhynchospiza","Spizella","Spizelloides","Zonotrichia")

df_list <- lapply(genera,FUN=function(genus){
  df <- query_xenocanto(species=paste("gen:",genus,sep=""),
                        api_key = "e7ef1c5168ec27bafe26facc3451b137f47a06f2",
                        verbose=T)
})

df <- do.call(gtools::smartbind,df_list)
head(df)
## 5 july 2022
lat_mean<-aggregate(df$latitude~df$species,FUN=function(x){mean(x,na.rm=T)})
lat_max<-aggregate(df$latitude~df$species,FUN=function(x){max(x,na.rm=T)})
lat_min<-aggregate(df$latitude~df$species,FUN=function(x){min(x,na.rm=T)})
colnames(lat_mean) <- c("species","mean")
colnames(lat_max) <- c("species","max")
colnames(lat_min) <- c("species","min")
lat <- merge(lat_mean,lat_max,all=T)
lat <- merge(lat,lat_min,all=T)
write.table(lat,"~/templat.txt",sep="\t",quote=F,row.names = F)



df <- read.table("/Users/kprovost/test.txt",header=T,sep="\t")
df[,2] <- as.numeric(df[,2])
df[,3] <- as.numeric(df[,3])
df[,4] <- as.numeric(df[,4])
df[,5] <- as.numeric(df[,5])
df[,6] <- as.numeric(df[,6])
df[,7] <- as.numeric(df[,7])
df[,9] <- as.numeric(df[,9])
df[,10] <- as.numeric(df[,10])

colnames(df) <- c("taxon","EOO","Midpoint Lat","Mean Lat","Max Lat","Min Lat","Lat Breadth","common","NSongs","NLats")

corrplot::corrplot(cor(df[,c(2,3,7,9)],use="pairwise.complete.obs"),method="color",
                   order="hclust",diag=F)


hist(log10(df$EOO),xlim=c(1,8),breaks=7,main="Log Scale Extent of Occupancy",
     xlab="Log Scale Extent of Occupancy",ylab="N Species")

hist(sqrt(df$EOO),main="Square Root Extent of Occupancy",
     xlab="Square Root Extent of Occupancy",ylab="N Species")

plot(sqrt(df$EOO),df$`Lat Breadth`*111,ylab="Latitudinal Breadth*111",xlab="Square Root Extent of Occupancy")
abline(a=0,b=1)
mod<-lm(df$`Lat Breadth`*111~sqrt(df$EOO))
abline(mod,col="red")
summary(mod)

hist(df$`Midpoint Lat`,main="Midpoint Latitiude",
     xlab="Midpoint Latitude",ylab="N Species")

hist(df$`Lat Breadth`,main="Latitiudinal Breadth",
     xlab="Latitiudinal Breadth",ylab="N Species")

summary(lm(df$NSongs~df$`Lat Breadth`))
#summary(lm(df$NSongs~log10(df$EOO)))
plot((df$`Lat Breadth`),df$NSongs)
