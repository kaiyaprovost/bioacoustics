## split gbif by species

outpath="/Users/kprovost/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Environment/GBIF 0158933-200613084148143/split_taxonomy/"

if(!dir.exists(outpath)){dir.create(outpath)}

filename = "/Users/kprovost/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Environment/GBIF 0158933-200613084148143/occurrence_small.txt"

df = data.table::fread(filename,data.table = F)
colnames(df)
## kingdom phylum class order family genus species subspecies
n_unique = sapply(df,dplyr::n_distinct)
n_unique_tax = n_unique[c("kingdom","phylum","class","order","family","genus","specificEpithet","infraspecificEpithet")]

for(kingdom in sort(unique(df$kingdom))){
  print(kingdom)
}
