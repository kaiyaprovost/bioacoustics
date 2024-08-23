library(warbleR)

#path = "~/bioacoustics/Sounds_and_Annotations/Aves/Passeriformes/Oscines/Passerellidae/"
path = "~/Documents/Research/Tyrannidae/ToPredict/"

setwd(path)

test = query_xc("Molothrus ater",download=F)

#test = query_xc("Sayornis phoebe type:song",download=TRUE)
#write.csv(test,"Tyrannidae.csv",append = T)

#test = query_xc("Contopus sordidulus type:song",download=TRUE)
#write.csv(test,"Tyrannidae.csv",append = T)

#test = query_xc("Contopus virens type:song",download=TRUE)
#write.csv(test,"Tyrannidae.csv",append = T)

#test = query_xc("Contopus cooperi type:song",download=TRUE)
#write.csv(test,"Tyrannidae.csv",append = T)

test = query_xc("Myiarchus crinitus type:song",download=TRUE)
write.csv(test,"Tyrannidae.csv",append = T)

#test = query_xc("Myiarchus crinitus type:song",download=TRUE)
#write.csv(test,"Myiarchus_crinitus.csv")

#eastern phoebe, 
#eastern wood-pewee, 
#great crested flycatcher, 
#olive-sided flycatcher 
#western wood-pewee