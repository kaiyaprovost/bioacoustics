library(pegas)
library(ape)
library(viridisLite)
library(RColorBrewer)
library(corrplot)

input = "/Users/kprovost/OneDrive - The Ohio State University/Phylogatr_Data/Birds-phylogatr-results_7dec2020/just_zono/Zono_COI.afa"
d = ape::read.dna(input,format="fasta")


e = ape::dist.dna(d)
corrplot(as.matrix(e),is.corr=F,method="color",order="hclust")



h = pegas::haplotype(d,strict=F)

occurence="/Users/kprovost/OneDrive - The Ohio State University/Phylogatr_Data/WhiteCrownedSparrow_phylogatr-results_19jan2021/occurrences.txt"
od = read.table(occurence,header=T,sep="\t")

h = sort(h,what="label")
net=pegas::haploNet(h)
ind.hap<-with(
  stack(setNames(attr(h, "index"), rownames(h))),
  table(hap=ind, pop=rownames(d)[values])
)

colors = c(
  brewer.pal(11,"RdYlBu"),
  brewer.pal(11,"PRGn"),
  brewer.pal(8,"Dark2"),
  brewer.pal(9,"Greys")
)

plot(h)

plot(net, size=attr(net, "freq"), scale.ratio=1, pie=ind.hap,bg=colors)
legend("topleft", colnames(ind.hap), col=colors, pch=19, ncol=2,
       cex=0.5)

## for these guys
## haplotype 1 = 2
## haplotype 3 = 7,8,16,23
## haplotype 4 = 11
## haplotype 5 = 15
## the rest are haplotype 2

colnames(ind.hap) = unlist(sapply(colnames(ind.hap),FUN=function(x){strsplit(x,"_")[[1]][1]}))
newind = as.data.frame(t(ind.hap))
newind = newind[newind$Freq==1,]
colnames(newind)=c("accession","hap","Freq")

od = od[,c("accession","latitude","longitude")]
od2 = merge(od,newind,by="accession")

palette(c("red","black","cyan","goldenrod","grey"))
plot(od2$longitude,od2$latitude,col=as.factor(od2$hap),pch=16)

