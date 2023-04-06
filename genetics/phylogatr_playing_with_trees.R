library(ape)
library(phangorn)
library(phytools)

input = "/Users/kprovost/OneDrive - The Ohio State University/Phylogatr_Data/Birds-phylogatr-results_7dec2020/ALLONEFILE/ATP6.merged.fasta.aligned"
d = ape::read.dna(input,format="fasta")

dm  <- dist.ml(d)
treeUPGMA  <- upgma(dm)
treeNJ  <- NJ(dm)
plot(treeUPGMA)
plot(treeNJ)

dphy = as.phyDat(d)
treePars=optim.parsimony(treeUPGMA, dphy)

pdf("treeparstest.pdf",height=20)
plot(treePars,cex=0.5)
dev.off()
