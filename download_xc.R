library(warbleR)

path = "/Users/kprovost/Documents/Research/Student_Projects/LASP/"

setwd(path)

test <- query_xc("Chondestes grammacus",download=FALSE)

write.table(test,"/Users/kprovost/Documents/Research/Student_Projects/LASP/LASP_metadata_18June2025.txt",sep="\t",row.names = F,quote=F)





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