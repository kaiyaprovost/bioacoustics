## read in the file of annotations
filetext <- "~/Documents/Research/Calidris/Calidris-pusilla-406428.Table.1.selections.txt"
df <- read.table(file=filetext,header=TRUE,sep="\t")
View(df)

## get some summary statistics about the file 
summary(df)
## take the mean of the Begin Time column
mean(df$Begin.Time..s.)
## take the standard deviation of the End Time column
sd(df$End.Time..s.)
## take the sum of the Selection column
sum(df$Selection)
## get how many items are in the Selection column
length(df$Selection)

## plot stuff!
## scatter plot with trendline
plot(x=df$Selection,
     y=df$Low.Freq..Hz.)
## calculate trendline
trendline <- lm(df$Low.Freq..Hz.~df$Selection)
## plot the trendline on our plot
abline(trendline,
       col="red")
summary(trendline)

## boxplot
boxplot(df$High.Freq..Hz.)
## compare two things in a boxplot
boxplot(df$High.Freq..Hz.,df$Low.Freq..Hz.)
## boxplot of something vs a variable
boxplot(df$High.Freq..Hz.~df$Type)

## histograms
hist(df$Dur.50...s.,
     col="blue")

## barplots
barplot(df$Dur.50...s.)
barplot(c(5,7,14,3))



## statistics!!!

## linear model, y=mx+b
y=df$Peak.Freq..Hz.
x=df$Dur.50...s.
model <- lm(y~x)
model
summary(model)
plot(x=df$Dur.50...s.,
     y=df$Peak.Freq..Hz.)
abline(model)

## putting a bunch of data together
data_folder <- "/Users/kprovost/Documents/Research/Calidris/"

## find all the text files in that data_folder
file_list <- list.files(path=data_folder,
                        pattern=".txt",
                        full.names = TRUE)
file_list

## read all of the files one at a time and save them in a list 
df_list <- lapply(file_list,
                  FUN=function(filetext){
                    ## read in the filetext as a dataframe
                    df <- read.table(file=filetext,header=TRUE,sep="\t")
                    ## save the dataframe to the list 
                    return(df)
                  }
  
)
df_list
## convert a list of files to one big file
library(gtools)
big_df <- do.call(what=smartbind,
        args=df_list)
big_df

## linear model, y=mx+b
y=big_df$Peak.Freq..Hz.
x=big_df$Dur.50...s.
model <- lm(y~x)
model
summary(model)
plot(x=big_df$Dur.50...s.,
     y=big_df$Peak.Freq..Hz.)
abline(model)

## how to do an ANOVA
y=big_df$Peak.Freq..Hz.
x=big_df$Type ## or Category
## convert x to a factor, which is a fancy category
x <- as.factor(x)
model2 <- aov(y~x)
model2
summary(model2)

## test pairwise sig difs in an anova
TukeyHSD(model2)
boxplot(y~x)


## principal components analysis
pca <- prcomp(big_df[,c(14,23,30,68)],
       scale. = TRUE,
       center = TRUE)

plot(big_df$Dur.50...s.,big_df$Dur.90...s.)

#colSums(is.na(big_df[,c(14,23,30,68)]))

pcadata <- as.data.frame(pca$x)
summary(pca)
pca
pca$rotation

plot(pcadata$PC1,big_df$BW.90...Hz.)

#plot(big_df$Begin.Time..s.,big_df$Low.Freq..Hz.)
plot(pcadata$PC1,pcadata$PC2)

## combine back with our original data
big_df_pca <- cbind(big_df,pcadata)
boxplot(big_df_pca$PC1~big_df_pca$Type)

plot(big_df_pca$PC1,big_df_pca$PC2,
     col=as.numeric(as.factor(big_df_pca$Type)),
     pch=as.numeric(as.factor(big_df_pca$Type)))
legend("bottomleft",
       legend=unique(as.factor(big_df_pca$Type)),
       col=unique(as.numeric(as.factor(big_df_pca$Type))),
       pch=unique(as.numeric(as.factor(big_df_pca$Type))))

big_df_pca[big_df_pca$Type==8,c("PC2","Selection","Begin.File")]


cor(big_df_pca[,c(14,23,30,68)])

plot(big_df_pca$Delta.Freq..Hz.,big_df$High.Freq..Hz.)





temp = read.table("/Users/kprovost/Documents/Research/Calidris/Calidris-pusilla-276968.Table.1.selections.txt",
                  header=T,sep="\t")
temp_waveform = temp[temp$View=="Waveform 1",]
temp_spectrogram = temp[temp$View=="Spectrogram 1",]

is.na(temp_waveform) ## tell us where the missing data is
## but we want missing per column

colSums(is.na(temp_waveform)) ## the number of missing data per column
## where in my case 20 means all are missing

## find only the row names where they are all missing
## first, need to determine the number that means all missing
## all missing means same as the number of rows
nrow(temp_waveform)

colSums(is.na(temp_waveform)) == nrow(temp_waveform)
which(colSums(is.na(temp_waveform)) == nrow(temp_waveform))
## which will give us the numbers of the columns where they are all missing

## we can get rid of those columns 
bad_columns <- which(colSums(is.na(temp_waveform)) == nrow(temp_waveform))

temp_waveform_nomissing <- temp_waveform[,-bad_columns]
dim(temp_waveform_nomissing)

bad_columns <- which(colSums(is.na(temp_spectrogram)) == nrow(temp_spectrogram))
temp_spectrogram_nomissing <- temp_spectrogram[,-bad_columns]
dim(temp_spectrogram_nomissing)

## now i can put them together so that all the rows and columns have no missing data

## see what column names are the same
intersect(colnames(temp_spectrogram_nomissing),colnames(temp_waveform_nomissing))

merge(temp_spectrogram_nomissing,temp_waveform_nomissing,
      all=TRUE)

temp_fixed <- cbind(temp_spectrogram_nomissing,temp_waveform_nomissing)
View(temp_fixed)


df_1$Type
df_2$Category
big_df_12$Type
big_df_12$Category
## where ever Type is blank, replace it with the value in Category and v.v.
is.na(big_df_12$Type) ## returns T/F if it is blank or not
big_df_12$Type[is.na(big_df_12$Type)] ## return a bunch of NA data at those spots

big_df_12$Type[is.na(big_df_12$Type)] <- big_df_12$Category[is.na(big_df_12$Type)]
big_df_12$Category[is.na(big_df_12$Category)] <- big_df_12$Type[is.na(big_df_12$Category)]



