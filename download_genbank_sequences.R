install.packages("ape")
install.packages("seqinr")
install.packages("rentrez")
library(ape)
library(seqinr)
library(rentrez)

dnabin_sequence <- read.GenBank(access.nb=c("JF795790.1"),
                                seq.names=c("JF795790.1"),
                                as.character=F)
character_sequence <- read.GenBank(access.nb=c("JF795790.1"),
                                   seq.names=c("JF795790.1"),
                                   as.character=T) ## less useful
write.dna(x=dnabin_sequence, 
          file ="JF795790.1_download.fasta", 
          format = "fasta", 
          append = F, 
          nbcol = 6, 
          colsep = " ", 
          colw = 10)

NOCA_cytb_search=entrez_search(
  db="nuccore",
  term="Cardinalis cardinalis[Organism] AND cytb[Gene]",
  retmax=10
)

NOCA_cytb = entrez_fetch(db="nuccore",
                         id=NOCA_cytb_search$ids,
                         rettype="fasta")
write(NOCA_cytb, "NOCA_cytb.fasta", sep="\n")
