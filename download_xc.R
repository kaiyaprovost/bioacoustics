library(warbleR)

#path = "~/bioacoustics/Sounds_and_Annotations/Aves/Passeriformes/Oscines/Passerellidae/"
path = "~/Documents/Tyrannidae/"

setwd(path)

test = query_xc("Sayornis phoebe type:song q:A",download=TRUE)
write.csv(test,"Sayornis_phoebe.csv")

test = query_xc("Contopus sordidulus type:song q:A",download=TRUE)
write.csv(test,"Contopus_sordidulus.csv")

test = query_xc("Contopus virens type:song q:A",download=TRUE)
write.csv(test,"Contopus_virens.csv")

test = query_xc("Contopus cooperi type:song q:A",download=TRUE)
write.csv(test,"Contopus_cooperi.csv")

test = query_xc("Myiarchus crinitus type:song q:A",download=TRUE)
write.csv(test,"Myiarchus_crinitus.csv")

eastern phoebe, 
eastern wood-pewee, 
great crested flycatcher, 
olive-sided flycatcher 
western wood-pewee