blb_string = "/Users/kprovost/Dropbox (AMNH)/BLB_klp.csv"
blb = read.table(blb_string,sep="\t",header=T,fill=T,flush=F)
dim(blb)
head(blb)
blb$LATITUDE[blb$LATITUDE>=90 & !(is.na(blb$LATITUDE))] = blb$LATITUDE[blb$LATITUDE>=90 & !(is.na(blb$LATITUDE))]/10
png("blb_latlong.test.png")
plot(blb$LONGITUDE,blb$LATITUDE)
dev.off()

latlong=blb[,c("LONGITUDE","LATITUDE")]
latlong = latlong[complete.cases(latlong),]

library(raster)
library(viridis)
bio1=raster("~/Downloads/bio1.bil")
bg2=raster()
bg2=crop(bg2,extent(bio1a))
bio1a = aggregate(x=bio1,fact=24,na.rm=T)
bio1a=crop(bio1a,extent(c(-180,-50,0,90)))
plot(bio1a)
bg2=crop(bg2,extent(bio1a))
latlongcoor = SpatialPoints(coords = latlong)
ras = rasterize(latlong, bg2,fun='count',background=10e-1,update=F,na.rm=T)
values(ras) = log(values(ras))
ras[is.na(bio1)] = NA
cuts=c(0,1,2,3,4,5,6,7,8) #set breaks
#png("usasongs.png")
png("All_BLB_bird_songs.png")
par(mar=c(0,0,0,0))
plot(bio1a,col="#BBBBBBFF",legend=F)
plot(ras,breaks=cuts,col=c("#BBBBBB00",plasma(7)),main="log USA songs per cell",
     #ylim=c(20,50),xlim=c(-150,-50),
     add=T,
     interpolate=F,bty="n",xaxt="n",yaxt="n",col.axis="white",ann=F
)
dev.off()


hist(blb$YEAR_COLLECTED)

sort(table(blb$GENUS_NAME))
plot(blb$LONGITUDE[blb$GENUS_NAME=="Setophaga"],
     blb$LATITUDE[blb$GENUS_NAME=="Setophaga"])
