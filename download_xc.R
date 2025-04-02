library(warbleR)

path = "/Users/kprovost/Documents/Research/Student_Projects/Calidris/"

setwd(path)

test <- query_xc("Calidris alba",download=FALSE)
write.table(test,"Calidris alba.csv",append = TRUE,sep=",")







#path = "~/bioacoustics/Sounds_and_Annotations/Aves/Passeriformes/Oscines/Passerellidae/"




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