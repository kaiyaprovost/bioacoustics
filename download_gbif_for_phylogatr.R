## download gbif data via r

library(rgbif)
library(spocc)

orders = c(## under 1000
  "Cariamiformes", "Apterygiformes", "Rheiformes", "Eurypygiformes", 
  "Phoenicopteriformes", "Ciconiiformes", "Sphenisciformes", 
  "Pteroclidiformes", "Phaethontiformes", "Cuculiformes", 
  "Bucerotiformes", "Tinamiformes", 
  
  ## under 10000
  "Gaviiformes", "Trogoniformes", "Podicipediformes", "Caprimulgiformes", 
  "Psittaciformes", "Suliformes", "Coraciiformes", "Gruiformes", 
  "Falconiformes", "Procellariiformes", "Columbiformes", 
  
  ## under 20000
  "Galliformes","Strigiformes", "Pelecaniformes", "Anseriformes", "Apodiformes", 
  "Piciformes", "Accipitriformes", 
  
  ## lots
  "Charadriiformes", "Passeriformes")


years = c(1600,1700,1800,1850,seq(1875,1900,5),seq(1901,2020))

## taxononKey == 212
## orders 1:12=1000, 13:23=10000, 24:30=20000, 31:32=lots

for(order in orders[32]) {
  print(order)
  for(i in 2:length(years)) {
    year1=as.character(years[i-1])
    year2=as.character(years[i])
    print(year1)
    print(year2)
    gbif=spocc::occ(query=order,has_coords=T,from="gbif",
                    limit = 100000,date=c(year1,year2),
                    gbifopts = list(basisOfRecord="PRESERVED_SPECIMEN",
                                    hasGeospatialIssue=FALSE,
                                    establishmentMeans="NATIVE"))
    print(gbif)
    gbif_df = data.frame(spocc::occ2df(gbif))
    write.table(gbif_df,file="gbif_specimens.txt",sep="\t",quote=F,append=T,row.names = F,col.names = F)
  }
}

## bison -- aves won't work
## inat -- need to write
## vertnet -- need to write
## 


## 2000-present = 436k 
## 1900-2000 = 2.6 mil
## 1900-1950 = 1.3 mil
## 1800-1900 = 394 k 
## 1700-1800 = 992
## 1600-1700 = 11