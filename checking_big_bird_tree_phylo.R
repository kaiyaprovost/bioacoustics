tree="/Users/kprovost/Downloads/5_jan_2021_mtgenomes.concatenated.out.contree"
library(ape)

phylo=read.tree(tree)
phylo_r = root(phylo,outgroup = "Colinus_virginanus_353721")
small=keep.tip(phylo_r,c("Melozone_fusca","Vermivora_bachmanii_AMNH380147","Cardinalis_sinuatus","FRA104_Ptho_N",
                       "Auriparus_flaviceps","Campylorhynchus_brunneicapillus","Polioptila_melanura","Toxostoma_crissale","Toxostoma_curvirostre","Vireo_bellii",
                       "Dryobates_villosus_AMNH363005","Phainopepla_nitens","Amphispiza_bilineara"))
pdf("test.pdf",height=20,width=20)
plot(phylo_r,cex=0.5,type = "radial")
#plot(small)
dev.off()
