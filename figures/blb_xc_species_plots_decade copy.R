library(raster)
library(sf)
library(rgdal)
library(sp)
library(rgeos)
shp = shapefile("/Users/kprovost/Downloads/cb_2016_us_state_500k/cb_2016_us_state_500k.shp")
plot(shp)
shp_ohio = (shp[shp$NAME == "Ohio",])

st_intersects(data[,c("LONGITUDE","LATITUDE")], shp_ohio)

data=read.table("/Users/kprovost/Documents/Postdoc_Working/ohio_xc_blb.csv",header=T,sep=",")
data=data[complete.cases(data[,c("LATITUDE","LONGITUDE")]),]
xy <- data[,c("LONGITUDE","LATITUDE")]
spdf <- SpatialPointsDataFrame(coords = xy, data = data,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
inohio=sapply(1:nrow(spdf),function(i){
  print(i)
  (gContains(shp_ohio,SpatialPoints(spdf[i,c("LONGITUDE","LATITUDE")],proj4string=CRS(proj4string(shp_ohio)))))
})
data_oh=data[inohio,]

max_lon = max(data_oh$LONGITUDE,na.rm=T)
min_lon = min(data_oh$LONGITUDE,na.rm=T)
max_lat = max(data_oh$LATITUDE,na.rm=T)
min_lat = min(data_oh$LATITUDE,na.rm=T)
plot(shp_ohio)
points(data_oh$LONGITUDE,data_oh$LATITUDE)

for(genus in sort(unique(data_oh$GENUS_NAME))){
  print(genus)
  png(paste("/Users/kprovost/Documents/Postdoc_Working/KR_project/Species_By_Year/",genus,"_blbxc_points.png",sep=""))
  par(mfrow=c(1,1))
  gen_data = data_oh[data_oh$GENUS_NAME==genus,]
  plot(shp_ohio,main=paste(genus,"N =",nrow(gen_data)))
  points(gen_data$LONGITUDE,gen_data$LATITUDE,col="red")
  dev.off()
  
  uniq_spp = sort(unique(gen_data$SPECIES_EPITHET))
  for(spp in uniq_spp){
    print(spp)
    spp_data = gen_data[gen_data$SPECIES_EPITHET==spp,]
    png(paste("/Users/kprovost/Documents/Postdoc_Working/KR_project/Species_By_Year/",genus,"_",spp,"_YEAR_blbxc_points.png",sep=""))
    par(mfrow=c(3,3))
    plot(shp_ohio,main=paste(genus,spp,"1948-1949"))
    points(spp_data[spp_data$YEAR %in% c(1948:1949),c("LONGITUDE","LATITUDE")],col="red")
    plot(shp_ohio,main=paste(genus,spp,"1950-1959"))
    points(spp_data[spp_data$YEAR %in% c(1950:1959),c("LONGITUDE","LATITUDE")],col="red")
    plot(shp_ohio,main=paste(genus,spp,"1960-1969"))
    points(spp_data[spp_data$YEAR %in% c(1960:1969),c("LONGITUDE","LATITUDE")],col="red")
    plot(shp_ohio,main=paste(genus,spp,"1970-1979"))
    points(spp_data[spp_data$YEAR %in% c(1970:1979),c("LONGITUDE","LATITUDE")],col="red")
    plot(shp_ohio,main=paste(genus,spp,"1980-1989"))
    points(spp_data[spp_data$YEAR %in% c(1980:1989),c("LONGITUDE","LATITUDE")],col="red")
    plot(shp_ohio,main=paste(genus,spp,"1990-1999"))
    points(spp_data[spp_data$YEAR %in% c(1990:1999),c("LONGITUDE","LATITUDE")],col="red")
    plot(shp_ohio,main=paste(genus,spp,"2000-2009"))
    points(spp_data[spp_data$YEAR %in% c(2000:2009),c("LONGITUDE","LATITUDE")],col="red")
    plot(shp_ohio,main=paste(genus,spp,"2010-2019"))
    points(spp_data[spp_data$YEAR %in% c(2010:2019),c("LONGITUDE","LATITUDE")],col="red")
    plot(shp_ohio,main=paste(genus,spp,"2020-2022"))
    points(spp_data[spp_data$YEAR %in% c(2020:2022),c("LONGITUDE","LATITUDE")],col="red")
    dev.off()
    
  }
}
