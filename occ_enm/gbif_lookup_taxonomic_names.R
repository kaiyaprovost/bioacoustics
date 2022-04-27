library(subsppLabelR)
library(spocc)
library(taxize)

## Rscript ~/Documents/GitHub/bioacoustics/occ_enm/gbif_lookup_taxonomic_names.R

run_forward = F

ebird_api = "f49839r87f7g"

{
if(file.exists("~/order_list_aves.txt")) {
  order_df=read.table("~/order_list_aves.txt")
} else {
  id=get_gbifid("Aves")[1]
  order_df=gbif_downstream(id=id,downto="order")
  write.table(order_df,"~/order_list_aves.txt")
}

if(file.exists("~/family_list_aves.txt")){
  family_df=read.table("~/family_list_aves.txt")
} else {
  family_df=data.frame()
  for(rownum in 1:nrow(order_df)){
    print(paste(rownum,"/",nrow(order_df)))
    order_name=order_df$name[rownum]
    this_order_families = gbif_downstream(id=order_df$key[rownum],downto="family",order=order_name)
    this_order_families$order=order_name
    family_df=rbind(family_df,this_order_families)
  }
  write.table(family_df,"~/family_list_aves.txt")
}

if(file.exists("~/genus_list_aves.txt")){
  genus_df=read.table("~/genus_list_aves.txt")
} else {
  genus_df=data.frame()
  for(famnum in 1:nrow(family_df)){
    print(paste(famnum,"/",nrow(family_df)))
    family_name=family_df$name[famnum]
    this_family_genera = gbif_downstream(id=family_df$key[famnum],downto="genus",family=family_name)
    if(nrow(this_family_genera)>0){
      this_family_genera$family=family_name
      genus_df=rbind(genus_df,this_family_genera)
    }
  }
  write.table(genus_df,"~/genus_list_aves.txt")
}

if(file.exists("~/species_list_aves.txt")){
  species_df=read.table("~/species_list_aves.txt")
} else {
  species_df=data.frame()
  for(gennum in 1:nrow(genus_df)){
    print(paste(gennum,"/",nrow(genus_df)))
    genus_name=genus_df$name[gennum]
    this_genus_species = gbif_downstream(id=genus_df$key[gennum],downto="species",genus=genus_name)
    if(nrow(this_genus_species)>0){
      this_genus_species$genus=genus_name
      species_df=rbind(species_df,this_genus_species)
    }
  }
  write.table(species_df,"~/species_list_aves.txt")
}

if(file.exists("~/subspecies_list_aves.txt")){
  subspecies_df=read.table("~/subspecies_list_aves.txt")
} else {
  subspecies_df=data.frame()
  for(sppnum in 1:nrow(species_df)){
    print(paste(sppnum,"/",nrow(species_df)))
    species_name=species_df$name[sppnum]
    this_species_subspecies=data.frame()
    try({this_species_subspecies = gbif_downstream(id=species_df$key[sppnum],downto="subspecies",species_df=species_name)})
    if(nrow(this_species_subspecies)>0){
      this_species_subspecies$species_df=species_name
      subspecies_df=rbind(subspecies_df,this_species_subspecies)
    }
  }
  subspecies_df=unique(subspecies_df)
  write.table(subspecies_df,"~/subspecies_list_aves.txt")
}
}

##  do this by species now
if(run_forward==T){
for(order_i in (c(26))){
  #for(order_i in sample(c(1:nrow(order_df)))){
  #for(order_i in sample(1:nrow(order_df))){
  order = order_df$name[order_i]
  order_dir = paste("/Users/kprovost/Documents/Postdoc_Working/GBIF/Aves/",sub(" ","-",order),"/",sep="")
  if(!(dir.exists(order_dir))){dir.create(order_dir)}
  ## this family order etc is not complete
  family_order_df = family_df[family_df$order==order,]
  for(family_i in sample(1:nrow(family_order_df))){
    family = family_order_df$name[family_i]
    family_dir = paste("/Users/kprovost/Documents/Postdoc_Working/GBIF/Aves/",sub(" ","-",order),"/",sub(" ","-",family),"/",sep="")
    if(!(dir.exists(family_dir))){dir.create(family_dir)}
    genus_family_df = genus_df[genus_df$family==family,]
    for(genus_i in sample(1:nrow(genus_family_df))){
      genus = genus_family_df$name[genus_i]
      genus_dir = paste("/Users/kprovost/Documents/Postdoc_Working/GBIF/Aves/",sub(" ","-",order),"/",sub(" ","-",family),"/",sub(" ","-",genus),"/",sep="")
      if(!(dir.exists(genus_dir))){dir.create(genus_dir)}
      species_genus_df = species_df[species_df$genus==genus,]
      for(i in sample(1:nrow(species_genus_df))){
        spp  = species_genus_df$name[i]
        outfile=paste("/Users/kprovost/Documents/Postdoc_Working/GBIF/Aves/",sub(" ","-",order),"/",sub(" ","-",family),"/",sub(" ","-",genus),"/",sub(" ","-",spp),"_occurrences_subspplabelR.txt",sep="")
        outfile2=paste("/Users/kprovost/Documents/Postdoc_Working/GBIF/Aves/",sub(" ","-",spp),"_occurrences_subspplabelR.txt",sep="")
        
        if((file.exists(outfile2))){ file.rename(outfile2,outfile)}
        
        if(!(file.exists(outfile))){
          print(paste("ORDER:",order_i,"/",nrow(order_df),order))
          print(paste("FAMILY:",family_i,"/",nrow(family_order_df),family))
          print(paste("GENUS:",genus_i,"/",nrow(genus_family_df),genus))
          print(paste("SPECIES:",i,"/",nrow(species_genus_df),spp))
          sppDf=data.frame()
          try({
            test = spocc::occ(query = spp,limit = 10000,has_coords = T,
                              from =  c("gbif","inat","bison","vertnet","ebird"),
                              ebirdopts=list(key=ebird_api))
            sppDf = data.frame(spocc::occ2df(test))
            keptcols = intersect(colnames(sppDf),c("longitude","latitude","prov","date"))
            sppDf = sppDf[,keptcols]
            sppDf = unique(sppDf)
          })
          if(nrow(sppDf)>0){
            write.table(sppDf,file=outfile)
          } else {
            file.create(outfile)
          }
        }
      }
    }
  }
}
}

## do this from the bottom up this time

master = read.table("/Users/kprovost/Documents/taxonomy_ebird_master.csv",header=T,sep=",")
master = master[,c("order","family","genus","scientific","species","subspecies")]
#master = master[master$order=="Accipitriformes",]
#master = master[master$family!="Muscicapidae",]
#master=master[master$genus=="Vireo",]
sort(table(master$order))

for(spp in sample(sort(unique(master$scientific)))){
  if(spp != ""){
    genus = unique(master$genus[master$scientific==spp])
    family = unique(master$family[master$genus==genus])
    order = unique(master$order[master$family==family])
    print(paste(spp,family,order))
    
    order_dir = paste("/Users/kprovost/Documents/Postdoc_Working/GBIF/Aves/",sub(" ","-",order[1]),"/",sep="")
    if(!(dir.exists(order_dir))){dir.create(order_dir)}
    family_dir = paste("/Users/kprovost/Documents/Postdoc_Working/GBIF/Aves/",sub(" ","-",order[1]),"/",sub(" ","-",family[1]),"/",sep="")
    if(!(dir.exists(family_dir))){dir.create(family_dir)}
    genus_dir = paste("/Users/kprovost/Documents/Postdoc_Working/GBIF/Aves/",sub(" ","-",order[1]),"/",sub(" ","-",family[1]),"/",sub(" ","-",genus),"/",sep="")
    if(!(dir.exists(genus_dir))){dir.create(genus_dir)}
    
    outfile=paste("/Users/kprovost/Documents/Postdoc_Working/GBIF/Aves/",sub(" ","-",order[1]),"/",sub(" ","-",family[1]),"/",sub(" ","-",genus),"/",sub(" ","-",spp),"_occurrences_subspplabelR.txt",sep="")
    outfile2=paste("/Users/kprovost/Documents/Postdoc_Working/GBIF/Aves/",sub(" ","-",spp),"_occurrences_subspplabelR.txt",sep="")
    
    if((file.exists(outfile2))){ file.rename(outfile2,outfile)}
    
    if(!(file.exists(outfile))){
      sppDf=data.frame()
      try({
        test = spocc::occ(query = spp,limit = 10000,has_coords = T,
                          from =  c("gbif","inat","bison","vertnet","ebird"),
                          ebirdopts=list(key=ebird_api))
        sppDf = data.frame(spocc::occ2df(test))
        keptcols = intersect(colnames(sppDf),c("longitude","latitude","prov","date"))
        sppDf = sppDf[,keptcols]
        sppDf = unique(sppDf)
      })
      if(nrow(sppDf)>0){
        write.table(sppDf,file=outfile)
      } else {
        file.create(outfile)
      }
    }
  }
}




## remember: derived_dataset to get a DOI for publication
