#library(warbleR)
devtools::install_github("maRce10/warbleR")
library(warbleR)
#library(raster) 
#library(viridis)


#list_of_species = c("Zonotrichia leucophrys","Zonotrichia albicollis","Xenops minutus","Vireo hypochryseus","Vireo gilvus","Vireo bellii","Turdus migratorius","Troglodytes aedon","Sturnus vulgaris","Setophaga striata","Setophaga ruticilla","Setophaga petechia","Setophaga coronata",
#"Regulus calendula","Ramphocaenus melanurus","Polioptila plumbea","Polioptila caerulea","Poecile atricapillus","Phaethornis longirostris","Passer domesticus","Momotus mexicanus","Mionectes oleagineus","Microbates cinereiventris","Melospiza melodia","Melospiza lincolnii","Melanerpes chrysogenys","Leiothlypis celata","Junco hyemalis","Hirundo rustica","Henicorhina leucophrys","Habia rubica","Eremophila alpestris",
#"Catharus ustulatus","Catharus minimus","Catharus guttatus","Cardellina pusilla","Branta canadensis","Automolus ochrolaemus","Artemisiospiza belli",
#"Ardea herodias","Anas platyrhynchos","Acanthis flammea","Jacana spinosa","Branta hutchinsii")
#list_of_genera = c(#"Zonotrichia","Xiphorhynchus","Troglodytes","Tringa","Thamnophilus","Tangara","Sterna","Spinus","Sitta","Regulus","Accipiter","Eremophila","Anas","Aythya","Branta","Egretta","Ardea","Momotus","Certhia","Vireo","Melanerpes","Picoides","Passerina","Habia","Cardinalis",#"Pyrocephalus","Myiarchus","Empidonax",#"Setophaga","Cardellina","Dendrocincla","Automolus",#"Geothlypis","Ramphocaenus","Corvus",#"Charadrius",#"Leiothlypis",
#"Artemisiospiza","Melospiza","Junco","Pipilo",#"Polioptila",#"Acanthis",#"Jacana",#"Larus",#"Lanius",#"Catharus","Anthus",#"Passer",#"Lagopus",#"Falco",#"Rallus",#"Poecile",#"Calidris",##"Icterus",##"Phaethornis",##"Henicorhina",##"Turdus"#)
#list_of_families = c(#"Passerellidae","Tyrannidae","Sturnidae","Troglodytidae",#"Rhipiduridae","Ramphastidae","Procellariidae",
  #"Pipridae","Phasianidae",#"Parulidae","Paridae",#"Pachycephalidae","Meliphagidae","Laridae","Icteridae","Hirundinidae","Fringillidae",
  #"Emberizidae","Cuculidae","Corvidae","Columbidae","Charadriidae","Anatidae","Alcidae","Accipitridae","Cardinalidae")



## just get metadata
test = query_xc(qword="Zonotrichia leucophrys",download=F)
write.table(test,"~/test.txt")

## download a particular species
path = "~/Documents/OneDrive - The Ohio State University/Song/XenoCanto/Falconiformes/"
#path = "/Users/kprovost/Documents/XenoCanto/0_DONE/"
list_of_families=c("Falconidae")
genera=sample(c("Falco"))
setwd(path)
for(family in rev(list_of_families)) {
  if(!(dir.exists(paste(path,family,sep="/")))) {
    dir.create(paste(path,family,sep="/"))
  } 
  setwd(paste(path,family,sep="/"))
  print(paste(path,family,sep="/"))
  #x = query_xc(qword=paste(family,"q:A",sep=" "),download=F)
  #x = query_xc(qword=paste(family,"q:A type:song area:america",sep=" "),download=F)
  #genera=sort(unique(x$Genus))
  print(genera)
  for(genus in genera) {
    if(!(dir.exists(paste(path,family,genus,sep="/")))) {
      dir.create(paste(path,family,genus,sep="/"))
    }
    setwd(paste(path,family,genus,sep="/"))
    print(genus)
    #try({x = query_xc(qword=paste("gen:",genus," q:A type:song area:america",sep=""),download=T)})
    #try({x = query_xc(qword=paste("gen:",genus," q:B type:song area:america",sep=""),download=T)})
    #try({x = query_xc(qword=paste("gen:",genus," type:song area:america",sep=""),download=T)})
    try({x = query_xc(qword=paste("gen:",genus," type:song area:america",sep=""),download=T)})
    
  }
  #x = query_xc(qword=paste(species,"q:A type:song",sep=" "),download=T)
}








afr=c('cnt:Algeria','cnt:Angola','cnt:Benin','cnt:Botswana','cnt:Burkina','cnt:Burundi','cnt:Cameroon','cnt:"Cape Verde"','cnt:Central African Republic','cnt:Chad','cnt:Comoros','cnt:Congo','cnt:Congo, Democratic Republic of','cnt:Djibouti','cnt:Egypt','cnt:"Equatorial Guinea"','cnt:Eritrea','cnt:Ethiopia','cnt:Gabon','cnt:Gambia','cnt:Ghana','cnt:Guinea','cnt:Guinea-Bissau','cnt:"Ivory Coast"','cnt:Kenya','cnt:Lesotho','cnt:Liberia','cnt:Libya','cnt:Madagascar','cnt:Malawi','cnt:Mali','cnt:Mauritania','cnt:Mauritius','cnt:Morocco','cnt:Mozambique','cnt:Namibia','cnt:Niger','cnt:Nigeria','cnt:Rwanda','cnt:Sao Tome and Principe','cnt:Senegal','cnt:Seychelles','cnt:"Sierra Leone"','cnt:Somalia','cnt:"South Africa"','cnt:"South Sudan"','cnt:Sudan','cnt:Swaziland','cnt:Tanzania','cnt:Togo','cnt:Tunisia','cnt:Uganda','cnt:Zambia','cnt:Zimbabwe')
asi=c('cnt:Afghanistan','cnt:Bahrain','cnt:Bangladesh','cnt:Bhutan','cnt:Brunei','cnt:Burma (Myanmar)','cnt:Cambodia','cnt:China','cnt:"East Timor"','cnt:India','cnt:Indonesia','cnt:Iran','cnt:Iraq','cnt:Israel','cnt:Japan','cnt:Jordan','cnt:Kazakhstan','cnt:Korea, North','cnt:Korea, South','cnt:Kuwait','cnt:Kyrgyzstan','cnt:Laos','cnt:Lebanon','cnt:Malaysia','cnt:Maldives','cnt:Mongolia','cnt:Nepal','cnt:Oman','cnt:Pakistan','cnt:Philippines','cnt:Qatar','cnt:"Russian Federation"','cnt:"Saudi Arabia"','cnt:Singapore','cnt:"Sri Lanka"','cnt:Syria','cnt:Tajikistan','cnt:Thailand','cnt:Turkey','cnt:Turkmenistan','cnt:United Arab Emirates','cnt:Uzbekistan','cnt:Vietnam','cnt:Yemen')
eur=c('cnt:Albania','cnt:Andorra','cnt:Armenia','cnt:Austria','cnt:Azerbaijan','cnt:Belarus','cnt:Belgium','cnt:Bosnia and Herzegovina','cnt:Bulgaria','cnt:Croatia','cnt:Cyprus','cnt:"Czech Republic"','cnt:Denmark','cnt:Estonia','cnt:Finland','cnt:France','cnt:Georgia','cnt:Germany','cnt:Greece','cnt:Hungary','cnt:Iceland','cnt:Ireland','cnt:Italy','cnt:Latvia','cnt:Liechtenstein','cnt:Lithuania','cnt:Luxembourg','cnt:Macedonia','cnt:Malta','cnt:Moldova','cnt:Monaco','cnt:Montenegro','cnt:Netherlands','cnt:Norway','cnt:Poland','cnt:Portugal','cnt:Romania','cnt:"San Marino"','cnt:Serbia','cnt:Slovakia','cnt:Slovenia','cnt:Spain','cnt:Sweden','cnt:Switzerland','cnt:Ukraine','cnt:"United Kingdom"','cnt:"Vatican City"')
nam=c('cnt:Antigua and Barbuda','cnt:Bahamas','cnt:Barbados','cnt:Belize','cnt:Canada','cnt:"Costa Rica"','cnt:Cuba','cnt:Dominica','cnt:"Dominican Republic"','cnt:"El Salvador"','cnt:Grenada','cnt:Guatemala','cnt:Haiti','cnt:Honduras','cnt:Jamaica','cnt:Mexico','cnt:Nicaragua','cnt:Panama','cnt:Saint Kitts and Nevis','cnt:"Saint Lucia"','cnt:Saint Vincent and the Grenadines','cnt:Trinidad and Tobago','cnt:"United States"')
oce=c('cnt:Australia','cnt:Fiji','cnt:Kiribati','cnt:"Marshall Islands"','cnt:Micronesia','cnt:Nauru','cnt:"New Zealand"','cnt:Palau','cnt:Papua New Guinea','cnt:Samoa','cnt:"Solomon Islands"','cnt:Tonga','cnt:Tuvalu','cnt:Vanuatu')
sam=c('cnt:Argentina','cnt:Bolivia','cnt:Brazil','cnt:Chile','cnt:Colombia','cnt:Ecuador','cnt:Guyana','cnt:Paraguay','cnt:Peru','cnt:Suriname','cnt:Uruguay','cnt:Venezuela')

#all=c(afr,asi,eur,nam,oce,sam)
#all=c(nam)
all=c(afr,asi,eur,oce,sam,nam)

alls=sapply(all,FUN=function(x){strsplit(x,":")[[1]][2]},simplify=T)
alls=(gsub('"',"",alls))


#full = c()
#fullx = c()
for (country in all) {
  x = querxc(country)
  
  if(is.null(x)) {
    print("pass")
  } else {
    
    if(is.null(fullx)) {
      fullx = x
    } else {
      fullx = gtools::smartbind(fullx,x)
    }
    
    
    num_rec = nrow(x)
    sppdf = paste(x$Genus,x$Specific_epithet)
    spptb = as.data.frame(table(sppdf))
    num_spp = length(unique(sppdf))
    sppdf2=lapply(1:num_spp,FUN=function(i){ return(paste(as.character(spptb[i,"sppdf"]),as.character(spptb[i,"Freq"]),collapse="-",sep="-")) })
    
    all_spp = paste(unique(sppdf2),col=";",sep="")
    row = cbind(country,num_rec,num_spp,all_spp)
    
    if(is.null(full)) {
      full = row
    } else {
      full = rbind(full,row)
    }
    
    
  } 
}
#write.table(full,"xc-songs.txt")
#write.table(full,"xc-northamerica-songs.txt")
#full=read.table("~/Dropbox (AMNH)/Dissertation/xc-northamerica-songs.txt")



full = as.data.frame(full)
full$continent = 0

full$continent[which(full$country %in% afr)] = "AFRICA"
full$continent[which(full$country %in% asi)] = "ASIA"
full$continent[which(full$country %in% nam)] = "NORTHAMERICA"
full$continent[which(full$country %in% sam)] = "SOUTHAMERICA"
full$continent[which(full$country %in% eur)] = "EUROPE"
full$continent[which(full$country %in% oce)] = "OCEANIA"

full = full[,c(1:3,5)]
full = unique(full)

#write.table(full,"xc-songs.countries")
#write.table(full,"xc-northamerica-songs.countries")

## this should query everything maybe?
z = querxc('nr:1-628187') ## as of 11 march 2021 there are this many recordings
write.table(z,file="xenocanto_fullrecords_upto_628187.txt",append=T,row.names = F,col.names = T,sep="\t")

## test figure 
filename="/Users/kprovost/Downloads/xenocanto_fullrecords_upto_628187_TRIMMED.txt"
#filename="/Users/kprovost/OneDrive - The Ohio State University/XenoCanto/xenocanto_fullrecords_TRIMMED_2.txt"

y = data.table::fread(filename,header=T,sep="\t",fill=T)
#y = y[,1:25] ## only keep through "Uploaded"
y = unique(y)
y = y[y$Specific_epithet!="Mystery",]
y = y[,c("Recording_ID","Genus","Specific_epithet","Subspecies","Country","Latitude","Longitude","Vocalization_type","Quality","Date","Altitude","Length","Uploaded")]
y = unique(y)
#y = y[y$Country %in% alls,]

problem_rows = names(table(y$Recording_ID)[table(y$Recording_ID)>1])

y=y[!(y$Recording_ID %in% as.numeric(problem_rows)),]

y$Latitude = as.numeric(y$Latitude)
y$Longitude = as.numeric(y$Longitude)

y=y[y$Longitude<=180,]
y=y[y$Longitude>=-180,]
y=y[y$Latitude<=90,]
y=y[y$Latitude>=-90,]

#write.table(y,file="xenocanto_fullrecords_upto_628187_TRIMMED.txt",append=F,row.names = F,col.names = T,sep="\t")

latlong = y[,c("Longitude","Latitude")]
latlong[,1] = as.numeric(latlong[,1])
latlong[,2] = as.numeric(latlong[,2])
latlong=latlong[complete.cases(latlong),]

plot(unique(latlong))

r <- getData("worldclim",var="bio",res=10)

pdf("worldclim.pdf")
for(i in 1:length(names(r))){
  print(i)
  plot(r[[i]],main=names(r)[i],col=viridis::plasma(200))
}
dev.off()

bg=r[[1]]

#Env = raster::stack('/Users/kprovost/OneDrive - The Ohio State University/raw_bbs_data/bio1/bio1.bil')
#bg = Env[[1]]

#ext = raster::extent(c(#min(latlong$Longitude,na.rm = T)-1, 
#  -180,
#  #max(latlong$Longitude,na.rm = T)+1, 
#  -50,
#  #min(latlong$Latitude,na.rm = T)-1,
#  -20,
#  #max(latlong$Latitude,na.rm = T)+1
#  90
#)
#)
#bg = raster::crop(bg, ext)

bg[!(is.na(bg))] = 0
#bg=raster::aggregate(bg,40)
bg=raster::aggregate(bg,10)

#writeRaster(bg,'/Users/kprovost/OneDrive - The Ohio State University/raw_bbs_data/bio1/bio1_AGGREGATE-40.asc',format="ascii")

plot(bg)
points(latlong)


#r <- raster(xmn=0, ymn=0, xmx=10, ymx=10, res=1)
#r[] <- 0
latlongcoor = SpatialPoints(coords = latlong)
#bg2 = aggregate(bg,fact=20)
rasnorm = rasterize(latlong, bg,fun='count',background=10e-1,update=F,na.rm=T)
rasnorm[is.na(bg)] = NA
raster::writeRaster(rasnorm,"~/xenocanto-data_everything_26May2022.asc",format="ascii",overwrite=T)
ras=rasnorm
values(ras) = log10(values(rasnorm))
ras[is.na(bg)] = NA
ras[rasnorm<0] = -0.1
raster::writeRaster(ras,"~/xenocanto-data_everything_log_26May2022.asc",format="ascii",overwrite=T)

cuts=c(-0.1,0,0.5,1,1.5,2,2.5,3,3.5,4,4.5)
#png("usasongs.png")
png("~/All_XC_songs_everything_26May2022.png")
par(mar=c(0,0,0,0))
plot(ras,breaks=cuts,col=c("#BBBBBBFF",viridis::plasma(9)),main="log USA songs per cell",
     #ylim=c(20,50),xlim=c(-150,-50),
     interpolate=F,bty="n",xaxt="n",yaxt="n",col.axis="white",ann=F
)
dev.off()

png("~/All_XC_songs_NAm_26May2022.png")
par(mar=c(0,0,0,0))
plot(ras,breaks=cuts,col=c("#BBBBBBFF",viridis::plasma(9)),main="log USA songs per cell",
     ylim=c(0,90),xlim=c(-180,-50),
     interpolate=F,bty="n",xaxt="n",yaxt="n",col.axis="white",ann=F
)
dev.off()


## for the blb data:

#blb = read.table("/Users/kprovost/OneDrive - The Ohio State University/BLB_Data/BLB_klp.csv",header=T,sep=",",fill=T)
blb = read.table("~/Downloads/BLB_klp.csv",header=T,sep=",",fill=T)







latlong = blb[,c("LONGITUDE","LATITUDE")]
latlong[,1] = as.numeric(latlong[,1])
latlong[,2] = as.numeric(latlong[,2])
latlong=latlong[complete.cases(latlong),]
latlongcoor = SpatialPoints(coords = latlong)
#bg2 = aggregate(bg,fact=20)
normras = raster::rasterize(latlong, bg,fun='count',background=-0.1,update=F,na.rm=T)
normras[is.na(bg)] = NA
raster::writeRaster(normras,"~/blb-data_26May2022.asc",format="ascii",overwrite=T)
ras=normras
values(ras) = log10(values(normras))
ras[is.na(bg)] = NA
raster::writeRaster(ras,"~/blb-data_log_26May2022.asc",format="ascii",overwrite=T)

#plot(ras)
ras[normras<0] = -0.1

points(latlong)
cuts=c(-0.1,0,0.5,1,1.5,2,2.5,3,3.5,4) #set breaks
#png("usasongs.png")
png("~/All_BLB_songs_26May2022.png")
par(mar=c(0,0,0,0))
plot(ras,breaks=cuts,
     col=c("#BBBBBBFF",viridis::plasma(8)),main="log USA songs per cell",
     #ylim=c(20,60),xlim=c(-150,-50),
     interpolate=F,bty="n",xaxt="n",yaxt="n",col.axis="white",ann=F
)
dev.off()

png("~/All_BLB_songs_NAm_26May2022.png")
par(mar=c(0,0,0,0))
plot(ras,breaks=cuts,
     col=c("#BBBBBBFF",viridis::plasma(8)),main="log USA songs per cell",
     ylim=c(0,90),xlim=c(-180,-50),
     interpolate=F,bty="n",xaxt="n",yaxt="n",col.axis="white",ann=F
)
dev.off()

## make combined 

songs_combo = raster::stack(c("/Users/kprovost/OneDrive - The Ohio State University/BLB_Data/blb-data.asc",
                              "/Users/kprovost/OneDrive - The Ohio State University/XenoCanto/xenocanto-data_everything.asc"))
writeRaster(sum(songs_combo,na.rm=F),"BLB-and-XC_songs.asc",format="ascii",overwrite=T)

cor(log(values(songs_combo[[1]])),log(values(songs_combo[[2]])),use = "pairwise.complete.obs")
plot(log(values(songs_combo[[1]])),log(values(songs_combo[[2]])),
     xlab="Log BLB Songs per Cell",
     ylab="Log XC Songs per Cell")
mod=lm(log(values(songs_combo[[2]]))~log(values(songs_combo[[1]])))
## significant
abline(mod,col="red")

sampling = raster("/Users/kprovost/OneDrive - The Ohio State University/Phylogatr_Data/sampling_across_world.asc")

vals = cbind(values(sampling),values(songs_combo[[1]]),values(songs_combo[[2]]))
vals[vals<0] = 0
vals=vals[complete.cases(vals),]

logvals = log10(vals)
logvals[is.infinite(logvals)] = NA
logvals=logvals[complete.cases(logvals),]

par(mfrow=c(1,3))
plot(logvals[,1],logvals[,2],xlab="Genetic Sampling (Log)",ylab="BLB Sampling (Log)")
mod2=lm(logvals[,2]~logvals[,1])
## nearly significant
abline(mod2,col="red")

plot(logvals[,1],logvals[,3],xlab="Genetic Sampling (Log)",ylab="XC Sampling (Log)")
mod3=lm(logvals[,3]~logvals[,1])
## very significant
abline(mod3,col="red")

plot(logvals[,2],logvals[,3],xlab="BLB Sampling (Log)",ylab="XC Sampling (Log)")
mod=lm(logvals[,3]~logvals[,2])
## significant
abline(mod,col="red")

##

# namspp=c(773,755,683,607,595,590,549,529,464,457,451,435,428,415,382,380,377,371,370,352,349,342,332,329,322,318,307,306,303,296,287,284,284,278,273,272,267,266,264,260,260,257,253,247,246,244,242,236,235,234,233,233,228,224,223,223,223,220,220,217,216,215,214,214,214,214,212,210,210,209,208,208,208,206,206,206,206,204,203,202,202,200,199,198,198,198,197,196,195,194,193,191,191,189,186,183,181,181,179,178,175,173,173,172,170,170,169,167,166,164,164,164,162,162,162,161,160,159,159,158,158,157,157,157,157,156,156,155,155,155,155,152,152,151,150,150,150,149,148,148,148,148,147,147,146,146,146,146,146,145,145,143,143,143,142,142,141,139,139,138,137,137,137,136,136,136,136,136,135,135,135,135,134,134,133,133,133,133,133,133,132,132,130,130,130,129,129,128,127,126,126,125,125,124,123,123,122,122,120,119,118,118,118,117,117,117,117,116,115,115,115,114,114,114,114,114,113,113,112,112,112,111,110,110,109,109,109,109,108,108,108,107,107,106,106,106,105,104,104,104,103,103,102,102,102,102,102,102,101,101,101,101,100,100,99,99,99,98,98,98,98,98,98,98,98,97,97,97,97,97,97,96,96,95,95,94,94,94,94,92,91,91,91,91,90,90,90,90,89,89,89,88,88,88,88,88,88,88,88,87,87,87,87,87,87,87,87,87,87,86,86,86,85,85,84,84,84,84,84,83,83,83,83,83,82,82,82,81,81,81,80,80,80,80,80,80,80,80,80,79,79,79,79,79,79,78,77,77,77,77,77,77,76,76,76,76,76,75,75,75,75,75,75,75,75,75,74,74,73,73,73,72,72,72,72,72,72,72,72,71,71,71,71,70,70,70,70,69,69,69,69,69,69,69,69,68,68,68,68,
#             68,68,67,67,67,67,66,66,66,65,65,65,65,65,65,65,65,65,64,64,64,64,64,64,64,63,63,63,63,63,63,62,62,62,62,62,62,62,62,61,61,61,61,60,60,60,60,60,59,59,59,59,59,58,58,58,58,58,57,57,57,57,57,57,57,57,57,56,56,56,56,56,55,55,55,55,55,55,55,55,54,54,54,54,54,54,54,54,53,53,53,53,53,52,52,52,52,52,52,52,52,52,52,52,52,51,51,51,51,51,50,50,50,50,50,
#             50,50,50,50,50,49,49,49,49,49,49,49,49,49,49,49,49,49,49,48,48,48,48,48,48,48,48,48,48,48,48,48,47,47,47,47,47,46,46,46,46,46,46,46,45,45,45,45,45,45,45,45,45,45,45,44,44,44,44,44,44,44,44,44,44,44,43,43,43,43,43,43,43,43,43,42,42,42,42,42,42,42,42,42,42,41,41,41,41,41,40,40,40,40,40,40,40,40,40,40,40,39,39,39,39,39,39,39,39,38,38,38,38,38,38,38,38,38,38,37,37,37,37,37,37,37,37,37,36,36,36,36,36,36,36,36,36,36,36,36,35,35,35,35,35,35,35,35,35,35,35,35,35,35,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,32,32,32,32,32,32,32,32,32,32,32,31,31,31,31,31,31,31,31,31,31,31,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,28,28,28,28,28,28,28,28,28,28,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,
#             17,17,17,17,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
#            
# namgenera=c(3793,2609,1881,1582,1353,1197,1191,1139,1127,1123,1068,963,943,942,940,929,846,829,774,758,719,708,689,683,679,665,638,636,625,598,590,572,569,563,562,552,541,541,538,527,509,504,504,500,479,473,464,457,444,436,436,426,423,421,418,415,404,403,400,
#             398,391,391,385,382,381,380,370,364,361,344,344,338,327,325,294,294,288,285,279,278,273,268,266,261,257,257,243,237,236,230,229,227,225,223,220,215,214,211,209,209,207,207,203,198,195,195,191,191,191,189,184,183,181,179,177,177,176,176,176,165,164,164,162,162,160,160,159,158,157,157,157,157,156,155,155,152,152,152,151,151,151,150,149,149,148,147,147,146,144,142,140,136,136,135,133,131,126,126,126,124,123,122,122,122,122,120,117,117,116,116,115,114,114,113,112,112,112,112,111,111,110,110,110,109,109,109,108,106,106,105,104,104,103,102,102,102,102,102,101,101,100,100,99,98,98,97,97,95,95,95,95,95,94,94,93,91,91,91,90,89,88,88,87,87,87,86,86,86,85,84,84,83,83,83,82,81,81,81,80,80,80,79,79,79,77,77,76,75,75,75,74,74,74,73,73,70,70,69,69,68,68,68,67,66,
#             66,65,65,64,64,63,63,62,62,62,62,61,61,60,60,60,60,59,59,59,59,59,59,59,59,58,58,57,54,54,53,53,53,53,52,52,52,52,52,52,51,51,50,50,50,50,50,50,49,49,49,48,48,48,47,46,46,
#             46,46,46,46,45,45,45,45,44,44,43,43,43,43,42,42,41,41,41,41,40,40,40,40,40,40,39,39,39,39,38,38,38,38,38,38,37,37,37,37,36,36,36,36,36,35,35,35,35,35,34,34,34,34,33,33,33,33,33,33,33,33,33,32,32,32,32,32,31,31,31,31,30,30,30,29,29,29,29,29,29,28,28,28,27,27,27,
#             27,27,27,27,27,27,27,27,27,27,27,27,26,26,26,26,26,26,25,24,24,24,23,23,23,23,23,23,22,22,22,22,22,22,22,21,21,21,20,20,20,20,20,20,20,20,19,19,19,19,19,19,19,19,19,18,18,18,18,18,18,18,17,17,17,17,17,17,17,17,16,16,16,16,16,16,16,16,16,16,15,15,15,15,15,15,15,14,14,14,14,14,14,13,13,13,13,13,13,13,13,13,12,12,12,12,12,12,12,12,12,12,12,11,11,11,11,11,11,11,11,11,11,11,11,11,10,10,10,10,10,10,10,10,10,10,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,7,7,7,7,7,7,7,7,7,7,7,
#             6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,5,5,5,5,5,5,5,5,5,5,5,5,5,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1) 
# 
# hist((namspp),lwd=3,breaks=seq(0,4000,50),freq = F,col=rgb(0,0,1,0.3))
# hist((namgenera),col=rgb(1,0,0,0.3),lwd=3,lty=3,breaks=seq(0,4000,50),add=T,freq = F)
# 
# boxplot(namspp)








## seeing which species co-occur
y = data.table::fread("/Users/kprovost/OneDrive - The Ohio State University/XenoCanto/xenocanto_fullrecords_TRIMMED_2.txt",header=T,sep="\t",fill=T)
y = unique(y)
y = y[y$Specific_epithet!="Mystery",]
y = y[y$Genus!="Mystery",]
y = y[,c("Recording_ID","Genus","Specific_epithet","Subspecies","Country","Latitude","Longitude","Vocalization_type","Quality","Date","Altitude","Length","Uploaded")]
y = unique(y)
problem_rows = names(table(y$Recording_ID)[table(y$Recording_ID)>1])
y=y[!(y$Recording_ID %in% as.numeric(problem_rows)),]
y$Latitude = as.numeric(y$Latitude)
y$Longitude = as.numeric(y$Longitude)
y=y[y$Longitude<=180,]
y=y[y$Longitude>=-180,]
y=y[y$Latitude<=90,]
y=y[y$Latitude>=-90,]

blb = read.table("/Users/kprovost/OneDrive - The Ohio State University/BLB_Data/BLB_klp.csv",header=T,sep=",",fill=T)

y_tomerge = y[,c("Genus","Specific_epithet","Subspecies","Longitude","Latitude")]
y_tomerge$YEAR_COLLECTED = as.numeric(substr(y$Date,1,4))
y_tomerge$MONTH_COLLECTED = as.numeric(substr(y$Date,6,7))
y_tomerge$DAY_COLLECTED = as.numeric(substr(y$Date,9,10))

blb_tomerge = blb[,c("GENUS_NAME","SPECIES_EPITHET","SUBSPECIES_EPITHET","LONGITUDE","LATITUDE","YEAR_COLLECTED","MONTH_COLLECTED","DAY_COLLECTED")]

colnames(blb_tomerge) = colnames(y_tomerge)

together = rbind(blb_tomerge,y_tomerge)
together$Longitude = as.numeric(together$Longitude)
together$Latitude = as.numeric(together$Latitude)
together$YEAR_COLLECTED = as.numeric(together$YEAR_COLLECTED)
together$MONTH_COLLECTED = as.numeric(together$MONTH_COLLECTED)
together$DAY_COLLECTED = as.numeric(together$DAY_COLLECTED)

together=together[together$Longitude<=180,]
together=together[together$Longitude>=-180,]
together=together[together$Latitude<=90,]
together=together[together$Latitude>=-90,]
together=together[together$YEAR_COLLECTED>=1900,]
together=together[together$YEAR_COLLECTED<=2021,]
together=together[together$MONTH_COLLECTED>=1,]
together=together[together$MONTH_COLLECTED<=12,]
together=together[together$DAY_COLLECTED>=1,]
together=together[together$DAY_COLLECTED<=31,]

hist(together$YEAR_COLLECTED)

together = together[together$Specific_epithet!="",]
together = together[together$Genus!="",]

## make inverted index?
# Example, if you have transactions
# transaction1 = ('apple','grape')
# transaction2 = ('apple','banana','mango')
# transaction3 = ('grape','mango')
# The inverted index will be:
# 'apple' -> [1,2]
# 'grape' -> [1,3]
# 'banana' -> [2]
# 'mango' -> [2,3]

bg = raster::raster('/Users/kprovost/OneDrive - The Ohio State University/raw_bbs_data/bio1/bio1_AGGREGATE-40.asc')


cells=raster::cellFromXY(bg,together[,c("Longitude","Latitude")])
together$cells = cells
together$scientific = paste(together$Genus,together$Specific_epithet,sep="_")

## JUST NORTH AMERICA
together=together[together$Longitude<=-40,]
together=together[together$Latitude>=10,]

## shared cells over total cells?
unique_scientific = sort(unique(together$scientific))

overlap_df = matrix(nrow=0,ncol=3)
for(i in 1:length(unique_scientific)) {
  for(j in 1:length(unique_scientific)) {
    if (j > i) {
      if(j %% 1000 == 0) {
        print(paste("i:",i,"j:",j,length(unique_scientific)))
      }
      spp_i = unique_scientific[i]
      spp_j = unique_scientific[j]
      cells_i = sort(unique(together$cells[together$scientific==spp_i]))
      cells_j = sort(unique(together$cells[together$scientific==spp_j]))
      overlap = length(intersect(cells_i,cells_j)) /length(union(cells_i,cells_j))
      if(overlap > 0) {
        overlap_df = rbind(overlap_df,cbind(spp_i,spp_j,overlap))
      }
    }
  }
}
write.table(overlap_df,"~/overlap_table_song_species.txt")
hist(overlap_df$overlap)


## sorensons similarity
# Sørensen coefficient (syn. coefficient of community, CC)
# A very simple index, similar to Jaccard's index
# Give greater "weight" to species common to the quadrats than to those found in only one quadrat
# Uses presence/absence data:
#     SS = 2a/(2a + b + c), where
#     Sørensen similarity coefficient,
#     a = number of species common to both quadrats,
#     b = number of species unique to the first quadrat, and
#     c = number of species unique to the second quadrat
# SS usually is multiplied by 100% (i.e., SS = 67%), and may be represented in terms of dissimilarity (i.e., DS = 1.0 - SS)


overlap_cells = matrix(nrow=0,ncol=3)
unique_cells = sort(unique(together$cells))
for(i in 1:length(unique_cells)) {
  for(j in 1:length(unique_cells)) {
    if (j > i) {
      if(j %% 100 == 0) {
        print(paste("i:",i,"j:",j,length(unique_cells)))
      }
      
      cell_i = unique_cells[i]
      cell_j = unique_cells[j]
      
      species_i = sort(unique(together$scientific[together$cells==cell_i]))
      species_j = sort(unique(together$scientific[together$cells==cell_j]))
      
      A = length(intersect(species_i,species_j))
      B = length(species_i[!(species_i %in% A)])
      C = length(species_j[!(species_j %in% A)])
      
      SS = 2*A / (2*A + B + C) * 100
      
      overlap_cells = rbind(overlap_cells,cbind(cell_i,cell_j,SS))
      
    }
  }
}

write.table(overlap_cells,"~/overlap_table_song_species_per_cell.txt")
hist(overlap_cells$SS)
dist_df = matrix(nrow=length(unique_cells),ncol=length(unique_cells),data = NA)
rownames(dist_df) = unique_cells
colnames(dist_df) = unique_cells

for(i in 1:length(unique_cells)) {
  print(i)
  this_cell = unique_cells[i]
  data_ss = overlap_cells[overlap_cells[,1]==this_cell,]
  if(class(data_ss)=="numeric") {
    data_ss = rbind(data_ss)
  }
  rows_to_change = which(unique_cells %in% c(data_ss[,2]))
  values_to_add = c(data_ss[,3])
  
  dist_df[rows_to_change,i] = values_to_add
  dist_df[i,rows_to_change] = values_to_add
}

dist_df=dist_df[,colSums(dist_df,na.rm=T)>0]
dist_df=dist_df[rowSums(dist_df,na.rm=T)>0,]



plot(together[together$cells==4644,c("Longitude","Latitude")])

dist_df=dist_df[colnames(dist_df) %in% sort(unique(together$cells)),colnames(dist_df) %in% sort(unique(together$cells))]

write.table(dist_df,"~/overlap_table_song_species_per_cell_DISTANCE.txt",sep="\t")

## see if any groups are non-overlapping?
## get the most similar pairs for each?
## need to find out where cells are
library(raster)
cell = 5000
r = bg
r[is.na(bg)] = NA
r[r>=0] = -1
r[as.numeric(colnames(dist_df))] = 0

values_to_color = dist_df[,which(colnames(dist_df)==cell)]
r[as.numeric(names(values_to_color))] = as.numeric(values_to_color)
r[cell] = 100
plot(r)


library(igraph)

#Create a graph adjacency based on correlation distances between genes in  pairwise fashion.
g <- graph.adjacency(
  dist_df,
  mode="undirected",
  weighted=TRUE,
  diag=FALSE
)
plot(g)