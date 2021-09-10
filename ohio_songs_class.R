library(raster)
data =  read.table("/Users/kprovost/OneDrive - The Ohio State University/ohio_ml_blb_xc.csv",sep=",",fill=T,header=T)


meantemp = raster::raster("/Users/kprovost/OneDrive - The Ohio State University/WorldClim2.1_jan2020/bio_2-5m_bil/bio1.bil")
meantemp = crop(meantemp,raster::extent(-85,-80.5,38.25,42))
blank = aggregate(meantemp,fact=5)
## crop this to ohio 
values(blank)[!(is.na(values(blank)))] = -1

cells=raster::cellFromXY(blank,data[,c("LONGITUDE","LATITUDE")])

data$CELLS = cells

years = seq(1940,2020,10)

shp = shapefile("/Users/kprovost/OneDrive - The Ohio State University/Environmental_Layers_Dissertation/cb_2016_us_state_500k/cb_2016_us_state_500k.shp")

table = rowSums(table(unique(data[,c("CELLS","YEAR")])),na.rm=T)
numyears = blank
par(mfrow=c(1,2))
numyears[as.numeric(names(table))] = (as.numeric(table))
plot(numyears,col=c("grey",viridis::plasma(60)),zlim=c(-1,60))
plot(shp,add=T)
numyears[as.numeric(names(table))] = log(as.numeric(table))
plot(numyears,col=c("grey",viridis::plasma(5)),zlim=c(-1,5))
plot(shp,add=T)

agg = aggregate(data$YEAR~data$CELLS,FUN=function(x){mean(x,na.rm=T)})
avgyears  = blank
avgyears[as.numeric(agg[,1])] = as.numeric(agg[,2])
plot(avgyears,zlim=c(1952,2020),col=c("grey",viridis::plasma(70)))
plot(shp,add=T)

#pdf("~/ohio_by_decade.pdf",height=18,width=18)
png("~/ohio_by_decade.png",height=18,width=18,units="in",res=300)
par(mfrow=c(3,3))
for(i in 1:length(years)){
  start = years[i]
  stop = start+9
  subset = data[data$YEAR>=start & data$YEAR<=stop,]
  table=table(subset$CELLS)
  
  
  ohio=blank
  ohio[as.numeric(names(table))] = log(as.numeric(table))
  plot(ohio,main=start,zlim=c(-1,7),col=c("grey",viridis::plasma(8)))
  plot(shp,add=T)
}
dev.off()
