devtools::install_github('kaiyaprovost/subsppLabelR')
library(subsppLabelR,verbose=T)

Env = raster::stack(list.files(
  path='/Users/kprovost/OneDrive - The Ohio State University/Environment/WorldClim2.1_jan2020/bio_2-5m_bil/',
  pattern="\\.bil$",
  full.names=T))
ext = raster::extent(c(-170,-55,20,70))
Env = raster::crop(Env, ext)
bg = Env[[1]]


spp="Zonotrichia leucophrys"
subsppList=c("gambelii","leucophrys","pugetensis","nuttalli","oriantha")
pointLimit=100
dbToQuery=c("gbif","bison","inat","ebird","ecoengine","vertnet")
bgLayer=bg
outputDir="~/"
quantile = 0.95
xmin = -125
xmax = -60
ymin = 10
ymax = 50
plotIt = F
datafile = NULL
epsilon = 1e-6

zono = databaseToAssignedSubspecies(spp=spp,subsppList=subsppList,pointLimit=pointLimit,dbToQuery=dbToQuery,
                             bgLayer=bgLayer,outputDir=outputDir,quantile=quantile,
                             xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,plotIt=plotIt,datafile=datafile,epsilon=epsilon)
suspect_occurrences = zono$loc_suspect
good_occurrences = zono$loc_good
subspecies_polygons = zono$pol
write.table(rbind(good_occurrences,suspect_occurrences),file="Zono_subspplabelR_occ.txt",row.names = F,sep="\t")


plot(bg)
lapply(length(subspecies_polygons):1,FUN=function(i){
  plot(subspecies_polygons[[i]],add=T,border=i,lwd=i)
})
legend("bottomleft",legend=names(subspecies_polygons),
       col=1:length(subspecies_polygons),
       lwd=1:length(subspecies_polygons))

df=read.table("/Users/kprovost/Zono_subspplabelR_occ_edit.txt",sep="\t",header=T)
par(mfrow=c(3,3))
for(spp in unique(df$assigned)){
     plot(df$longitude,df$latitude,col="grey",main=spp)
     points(df$longitude[df$assigned==spp],df$latitude[df$assigned==spp],main=spp,col="red")
}

library(sp)

par(mfrow=c(1,1))
plot(df$longitude,df$latitude,col="grey")
cols=c("goldenrod","black","green","magenta","cyan")
for(colnum in 7:11) {
  z<-chull(df$longitude[df[,colnum]==1],df$latitude[df[,colnum]==1]) 
  dfHull <-cbind(df$longitude[df[,colnum]==1][z],df$latitude[df[,colnum]==1][z])
  points(dfHull,col=cols[colnum-6])
  polygon(dfHull,border=cols[colnum-6])
}
  

par(mfrow=c(1,1))     
mult = df[df$assigned=="MULTI",]
unique(mult[,c(7:11)])

## NO OVERLAP GL
plot(df$longitude,df$latitude,col="grey",main="GL")
points(df$longitude[df$assigned=="gambelii"],df$latitude[df$assigned=="gambelii"],col="goldenrod",main="GL")
points(df$longitude[df$assigned=="leucophrys"],df$latitude[df$assigned=="leucophrys"],col="black",main="GL")
points(mult$longitude[mult$gambelii==1 & mult$leucophrys==1],
       mult$latitude[mult$gambelii==1 & mult$leucophrys==1],col="blue",pch=0)

plot(df$longitude,df$latitude,col="grey",main="GN")
points(df$longitude[df$assigned=="gambelii"],df$latitude[df$assigned=="gambelii"],col="goldenrod",main="GL")
points(df$longitude[df$assigned=="nuttalli"],df$latitude[df$assigned=="nuttalli"],col="magenta",main="GL")
points(mult$longitude[mult$gambelii==1 & mult$nuttalli==1],
       mult$latitude[mult$gambelii==1 & mult$nuttalli==1],col="blue",pch=0)

plot(df$longitude,df$latitude,col="grey",main="GO")
points(df$longitude[df$assigned=="gambelii"],df$latitude[df$assigned=="gambelii"],col="goldenrod",main="GL")
points(df$longitude[df$assigned=="oriantha"],df$latitude[df$assigned=="oriantha"],col="cyan",main="GL")
points(mult$longitude[mult$gambelii==1 & mult$oriantha==1],
       mult$latitude[mult$gambelii==1 & mult$oriantha==1],col="blue",pch=0)

plot(df$longitude,df$latitude,col="grey",main="GP")
points(df$longitude[df$assigned=="gambelii"],df$latitude[df$assigned=="gambelii"],col="goldenrod",main="GL")
points(df$longitude[df$assigned=="pugetensis"],df$latitude[df$assigned=="pugetensis"],col="green",main="GL")
points(mult$longitude[mult$gambelii==1 & mult$pugetensis==1],
       mult$latitude[mult$gambelii==1 & mult$pugetensis==1],col="blue",pch=0)

## NO OVERLAP LN
plot(df$longitude,df$latitude,col="grey",main="LN")
points(df$longitude[df$assigned=="leucophrys"],df$latitude[df$assigned=="leucophrys"],col="black",main="GL")
points(df$longitude[df$assigned=="nuttalli"],df$latitude[df$assigned=="nuttalli"],col="magenta",main="GL")
points(mult$longitude[mult$leucophrys==1 & mult$nuttalli==1],
       mult$latitude[mult$leucophrys==1 & mult$nuttalli==1],col="blue",pch=0)

plot(df$longitude,df$latitude,col="grey",main="LO")
points(df$longitude[df$assigned=="leucophrys"],df$latitude[df$assigned=="leucophrys"],col="black",main="GL")
points(df$longitude[df$assigned=="oriantha"],df$latitude[df$assigned=="oriantha"],col="cyan",main="GL")
points(mult$longitude[mult$leucophrys==1 & mult$oriantha==1],
       mult$latitude[mult$leucophrys==1 & mult$oriantha==1],col="blue",pch=0)

## NO OVERLAP LP
plot(df$longitude,df$latitude,col="grey",main="LP")
points(df$longitude[df$assigned=="leucophrys"],df$latitude[df$assigned=="leucophrys"],col="black",main="GL")
points(df$longitude[df$assigned=="pugetensis"],df$latitude[df$assigned=="pugetensis"],col="green",main="GL")
points(mult$longitude[mult$leucophrys==1 & mult$pugetensis==1],
       mult$latitude[mult$leucophrys==1 & mult$pugetensis==1],col="blue",pch=0)

plot(df$longitude,df$latitude,col="grey",main="NO")
points(df$longitude[df$assigned=="nuttalli"],df$latitude[df$assigned=="nuttalli"],col="magenta",main="GL")
points(df$longitude[df$assigned=="oriantha"],df$latitude[df$assigned=="oriantha"],col="cyan",main="GL")
points(mult$longitude[mult$nuttalli==1 & mult$oriantha==1],
       mult$latitude[mult$nuttalli==1 & mult$oriantha==1],col="blue",pch=0)

plot(df$longitude,df$latitude,col="grey",main="NP")
points(df$longitude[df$assigned=="nuttalli"],df$latitude[df$assigned=="nuttalli"],col="magenta",main="GL")
points(df$longitude[df$assigned=="pugetensis"],df$latitude[df$assigned=="pugetensis"],col="green",main="GL")
points(mult$longitude[mult$nuttalli==1 & mult$pugetensis==1],
       mult$latitude[mult$nuttalli==1 & mult$pugetensis==1],col="blue",pch=0)

plot(df$longitude,df$latitude,col="grey",main="OP")
points(df$longitude[df$assigned=="oriantha"],df$latitude[df$assigned=="oriantha"],col="cyan",main="GL")
points(df$longitude[df$assigned=="pugetensis"],df$latitude[df$assigned=="pugetensis"],col="green",main="GL")
points(mult$longitude[mult$oriantha==1 & mult$pugetensis==1],
       mult$latitude[mult$oriantha==1 & mult$pugetensis==1],col="blue",pch=0)


pts1=spocc::occ(
  query = "Zonotrichia leucophrys leucophrys",
  limit = 5000,
  has_coords = T,
  from = dbToQuery
)
pts1_df = occ2df_subspeciesLabels(pts1,"leucophrys")
write.table(pts1_df,("Z_l_leucophrys_rawocc.txt"))
