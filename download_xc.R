library(warbleR)

#code == "i am code, but i don't wanna run
## something == "i am a note to the programmer"
#path = "~/bioacoustics/Sounds_and_Annotations/Aves/Passeriformes/Oscines/Passerellidae/"
#setwd(path)

?query_xc

lasp_xc_recordings = query_xc(qword="Chondestes grammacus") ## give me all recorded by Passerllidae

getwd()
setwd("/Users/kprovost/Documents/Chondestes_grammacus/")
getwd()

test = query_xc(qword="Chondestes grammacus type:song q:A", download = FALSE)

write.table(test,"/Users/kprovost/Documents/Chondestes_grammacus/testDownload2.csv",
            sep=",",row.names = FALSE)

## BELOW HERE: messing around with dataframes

View(lasp_xc_recordings)

class(lasp_xc_recordings) ## data.frame

str(lasp_xc_recordings)

rec_IDs = lasp_xc_recordings$Recording_ID ## get only the column Recording_ID
## this format is called a vector
class(rec_IDs) ## "character" vector
str(rec_IDs)

rec_IDs[3] ## grabs the third item in the vector

## dataframe[rownumber,columnnumber]
lasp_xc_recordings[3,] ## grab the third ROW of the dataframe, but all the columns
lasp_xc_recordings[,3] ## grab the third COLUMN of the dataframe, but all of the rows
lasp_xc_recordings[3,10] ## grab the third ROW of the dataframe, and the tenth COLUMN
lasp_xc_recordings[3:5,] ## grab the third, fourth, and fifth ROW of the dataframe, but all the columns

lasp_small = lasp_xc_recordings[1:10,1:5]
## lets extract only rows where the Recording_ID is "857366"
#A = B ## set A to B, or A <- B
#A == B ## check if A and B are equivalent
rec_IDs = lasp_small$Recording_ID ## by the way, we overwrote rec_IDs from before
rec_IDs == "857366"

lasp_small$Recording_ID == "857366"

lasp_small[ lasp_small$Recording_ID == "857366" ,]

lasp_small[ lasp_small$Specific_epithet == "grammacus" ,]

lasp_xc_recordings[ lasp_xc_recordings$Subspecies == "strigatus", ]

