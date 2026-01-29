#remotes::install_github("maRce10/suwo")
library(suwo)
df <- query_xenocanto(species='sp:"Calidris himantopus"',
                api_key = "e7ef1c5168ec27bafe26facc3451b137f47a06f2",
                verbose=T)
#write.table(df,"~/Documents/Research/Student_Projects/Calidris/calidris_C_18nov2025.csv",
#            sep=",")
download_media(df,path="~/Documents/Research/Student_Projects/Calidris/")
