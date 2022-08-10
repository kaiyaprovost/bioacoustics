centpca=list.files(path="~/Documents/Postdoc_Working/Sounds_and_Annotations/Aves/",
           pattern="centroid_locations_per_individual_",
           recursive=T,full.names = T)
centpca = centpca[!grepl("PCA",centpca)]
#centpca = centpca[!grepl("fcts",centpca)]

centpcalist = lapply(centpca,FUN=function(x){
  print(x)
  y=NULL
  try({y=read.table(x,header=T)})
  if(is.null(y)){
    y=read.table(x,header=T,sep="\t")
  }
  return(y)
  })
centdfpca = do.call(gtools::smartbind,centpcalist)
#centdfpca = centdfpca[,c(1,2,3,4,5,6,7,8,9,10,11)]
centdfpca = unique(centdfpca)
write.table(centdfpca,"~/Documents/Postdoc_Working/Sounds_and_Annotations/centroid_locations_per_individual_MASTER.txt",
            row.names = F,quote = F,sep="\t")

df=read.table("/Users/kprovost/Documents/Postdoc_Working/gene_song_species_distances_mmrr_MASTER.csv",header=T,sep="\t")
df=df[df$FAMILY=="Passerellidae",]
df$SIG_TYPE.GENE[df$SIG_TYPE.GENE==""] = "?"
df$SIG_TYPE.SONG[df$SIG_TYPE.SONG==""] = "?"
df$SIG_TYPE.GENE[is.na(df$SIG_TYPE.GENE)] = "?"
df$SIG_TYPE.SONG[is.na(df$SIG_TYPE.SONG)] = "?"
df$SIG_ECO.SONG[df$SIG_ECO.SONG==""] = NA
df$SIG_GEO.SONG[df$SIG_GEO.SONG==""] = NA
df$SIG_ECO.GENE[df$SIG_ECO.GENE==""] = NA
df$SIG_GEO.GENE[df$SIG_GEO.GENE==""] = NA

for(i in c(28:47)){
  print(colnames(df)[i])
  #dfdata = df[,c(26,27,i)]
  #mod=(aov(dfdata[,3]~dfdata[,2]))
  #print(TukeyHSD(mod))
  mod1=aov(df[,i]~df$SIG_ECO.SONG)
  mod2=aov(df[,i]~df$SIG_GEO.SONG)
  mod3=aov(df[,i]~df$SIG_ECO.GENE)
  mod4=aov(df[,i]~df$SIG_GEO.GENE)
  print("SIG ECO SONG")
  print(summary(mod1))
  print("SIG GEO SONG")
  print(summary(mod2))
  print("SIG ECO GENE")
  print(summary(mod3))
  print("SIG GEO GENE")
  print(summary(mod4))
  print("xxx")
  
}

mod1=aov(df$KippsDistance_mean~df$SIG_ECO.SONG)
mod2=aov(df$KippsDistance_mean~df$SIG_GEO.SONG)
mod3=aov(df$KippsDistance_mean~df$SIG_ECO.GENE)
mod4=aov(df$KippsDistance_mean~df$SIG_GEO.GENE)

summary(mod1)
summary(mod2)
summary(mod3)
summary(mod4)





distdf = read.table("/Users/kprovost/Downloads/mean_centroid_distances_individuals_MASTER.txt",header=T,sep="\t")
x=lapply(rownames(distdf),FUN=function(x){
  splits=strsplit(x,"\\.")[[1]]
  gen=splits[1]
  spp=splits[2]
  subspp=splits[3]
  return(cbind(gen,spp,subspp))
})
passerell_gen = c("Aimophila","Ammodramus","Ammospiza","Amphispiza","Amphispizopsis",
                  "Arremon","Arremonops","Artemisiospiza","Atlapetes","Calamospiza",
                  "Centronyx","Chlorospingus","Chondestes","Junco","Melospiza",
                  "Melozone","Oreothraupis","Oriturus","Passerculus","Passerella",
                  "Peucaea","Pezopetes","Pipilo","Pooecetes","Pselliophorus",
                  "Rhynchospiza","Spizella","Spizelloides","Torreornis","Xenospiza",
                  "Zonotrichia")
taxa=as.data.frame(do.call(rbind,x))
passer_taxa = taxa[which(taxa$gen %in% passerell_gen),]
passer = distdf[which(taxa$gen %in% passerell_gen),which(taxa$gen %in% passerell_gen)]

max_passer = max(as.matrix(passer),na.rm=T)
mean_passer = mean(as.matrix(passer),na.rm=T)
sd_passer = sd(as.matrix(passer),na.rm=T)
sumstats_passer = cbind(group="Passerellidae",
                        maxdif=max_passer,
                        meandif=mean_passer,
                        sddif=sd_passer,
                        level="Family",
                        N=nrow(passer))

for(gen in sort(unique(passer_taxa$gen))){
  print(gen)
  passer_gen = passer[which(passer_taxa$gen==gen),which(passer_taxa$gen==gen)]
  max_gen = max(as.matrix(passer_gen),na.rm=T)
  mean_gen = mean(as.matrix(passer_gen),na.rm=T)
  sd_gen = sd(as.matrix(passer_gen),na.rm=T)
  toadd=cbind(group=gen,
        maxdif=max_gen,
        meandif=mean_gen,
        sddif=sd_gen,
        level="Genus",
        N=nrow(passer_gen))
  sumstats_passer = rbind(sumstats_passer,toadd)
  passer_taxa_gen = passer_taxa[passer_taxa$gen==gen,]
  for(spp in sort(unique(passer_taxa_gen$spp))){
    print(spp)
    passer_spp = passer_gen[which(passer_taxa_gen$spp==spp),which(passer_taxa_gen$spp==spp)]
    
    max_spp = max(as.matrix(passer_spp),na.rm=T)
    mean_spp = mean(as.matrix(passer_spp),na.rm=T)
    sd_spp = sd(as.matrix(passer_spp),na.rm=T)
    toadd=cbind(group=paste(gen,spp,sep="."),
                maxdif=max_spp,
                meandif=mean_spp,
                sddif=sd_spp,
                level="Species",
                N=nrow(passer_spp))
    try({sumstats_passer = rbind(sumstats_passer,toadd)})
    
    passer_taxa_spp = passer_taxa_gen[passer_taxa_gen$spp==spp,]
    for(subspp in sort(unique(passer_taxa_spp$subspp))){
      print(subspp)
      if(subspp != "unknown"){
        passer_subspp = passer_spp[which(passer_taxa_spp$subspp==subspp),which(passer_taxa_spp$subspp==subspp)]
        max_subspp = max(as.matrix(passer_subspp),na.rm=T)
        mean_subspp = mean(as.matrix(passer_subspp),na.rm=T)
        sd_subspp = sd(as.matrix(passer_subspp),na.rm=T)
        toadd=cbind(group=paste(gen,spp,subspp,sep="."),
                    maxdif=max_subspp,
                    meandif=mean_subspp,
                    sddif=sd_subspp,
                    level="Subspecies",
                    N=nrow(passer_subspp))
        sumstats_passer = rbind(sumstats_passer,toadd)
      }
    }
  }
  
}
sumstats_passer=unique(sumstats_passer)
sumstats_passer = as.data.frame(sumstats_passer)
write.table(sumstats_passer,"~/Summary_statistics_mean_distances_etc_passerellidae.txt",sep="\t",
            row.names = F,quote = F)
barplot(as.numeric(sumstats_passer$maxdif),
        #width=as.numeric(sumstats_passer$N),
        col=as.numeric(as.factor(sumstats_passer$level)))
plot(log10(as.numeric(sumstats_passer$N)),sumstats_passer$maxdif)
plot(log10(as.numeric(sumstats_passer$N)),sumstats_passer$meandif)
plot(log10(as.numeric(sumstats_passer$N)),sumstats_passer$sddif)
