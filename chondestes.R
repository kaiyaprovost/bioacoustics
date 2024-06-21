## read in one file
song_df <- read.delim("~/Documents/Chondestes_grammacus/Chondestes-grammacus-596715.Table.1.selections.txt")

## look at peak frequency and duration
song_df$Peak.Freq..Hz.
song_df$Dur.90...s.

## look at the number of rows and columns
nrow(song_df)
ncol(song_df)

## xy plot
plot(song_df$Peak.Freq..Hz.,
     song_df$Dur.90...s.)
## make a linear model y~x
mod = lm(song_df$Dur.90...s. ~ song_df$Peak.Freq..Hz.)
## show the details of the linear model
summary(mod)
## plot it anyway
abline(mod,col="red",lwd=2,lty=3)

## boxplot
## separate out short and long songs
long_songs = song_df[song_df$Dur.90...s.>=0.1,]
short_songs = song_df[song_df$Dur.90...s.<0.1,]

## plot long and short songs by duration
boxplot(long_songs$Dur.90...s.,
        short_songs$Dur.90...s.,
        names=c("long","short"),
        ylab="duration",
        main="duration of long vs short syllables",
        col=c("pink","red"))

## barplots
barplot(song_df$Peak.Freq..Hz.,
        col=c("red","green","blue"))

## histogram
hist(song_df$Peak.Freq..Hz.)

## statistics time

## mean
mean(song_df$Peak.Freq..Hz., na.rm=TRUE)
## standard deviation
sd(song_df$Peak.Freq..Hz., na.rm=TRUE)
## overall summary
summary(song_df$Peak.Freq..Hz.)


## compare the six tables for mean peak frequency

## import all the tables as objects

## folder that the tables are in
folder = "~/Documents/Chondestes_grammacus/"

## get a list of all the tables in that folder
my_files = list.files(path=folder,
                      pattern="Table.1.selections.txt",
                      full.names = TRUE)

##for every book on the floor, place it on the bookshelf 
# for(book in floor_books) {
#   placeBook(book)
# }

## for every file with song data in it, calculate a mean and a standard deviation
for (file in my_files) {
  ## print the name of the file
  print(paste("My file is:",file))
  
  ## read in the file
  song_df = read.delim(file)
  
  ## take the mean of PeakFreq
  my_mean = mean(song_df$Peak.Freq..Hz.,na.rm=TRUE)
  print(paste("mean:",my_mean))
  
  ## take the sd of PeakFreq
  my_sd = sd(song_df$Peak.Freq..Hz.,na.rm=TRUE)
  print(paste("sd:",my_sd))
  
}

## lapply functions to replace the loop, so that we can save mean and sd

## lapply(what_you_loop_over,what_you_do_to_it)

song_meansd = lapply(my_files,
       FUN = function(file) {
         ## print the name of the file
         print(paste("My file is:",file))
         
         ## read in the file
         song_df = read.delim(file)
         
         ## take the mean of PeakFreq
         my_mean = mean(song_df$Peak.Freq..Hz.,na.rm=TRUE)
         print(paste("mean:",my_mean))
         
         ## take the sd of PeakFreq
         my_sd = sd(song_df$Peak.Freq..Hz.,na.rm=TRUE)
         print(paste("sd:",my_sd))
         
         return(c(my_mean,my_sd))
       }
)

## name our mean and standard deviation after the files
names(song_meansd) = my_files
## that, but short file names
names(song_meansd) = basename(my_files)

## lets make it a dataframe
meansd_df = do.call(rbind,song_meansd) ## row bind, glue together like rows of a dataframe
## do.call defaults to making a matrix, we need a data.frame
## dollar signs do not work with a matrix
meansd_df = as.data.frame(meansd_df)
colnames(meansd_df) = c("meanPF","sdPF")

barplot(meansd_df$meanPF)


## lets compare to our metadata 

## import our metadata
metadata <- read.csv("~/Documents/Chondestes_grammacus/testDownload.csv")

## we need to make our data labels match the metadata labels
## the metadata labels are just numeric recording ids

my_rownames = rownames(meansd_df)

## remove ".Table.1.selections.txt" from the rownames
## AND
## remove "Chondestes-grammacus-" from the rownames

my_rownames = gsub(pattern=".Table.1.selections.txt",
     replacement = "",
     x=my_rownames)

my_rownames = gsub(pattern="Chondestes-grammacus-",
                   replacement = "",
                   x=my_rownames)

## add the short IDs back to the data 
meansd_df$Recording_ID = my_rownames

## match the metadata to the data so that the Recording_IDs are the same
meansd_df_meta = merge(meansd_df,metadata,by="Recording_ID")

## make a boxplot between Recordist and meanPF
boxplot(meansd_df_meta$meanPF ~ meansd_df_meta$Recordist)

meansd_df_meta$Locality ## this order is OR, CA, CA, TX, CA, TX
meansd_df_meta$State = c("OR", "CA", "CA", "TX", "CA", "TX")
boxplot(meansd_df_meta$meanPF ~ meansd_df_meta$State)

## make a lat long plot, xy plot
plot(meansd_df_meta$Longitude,
     meansd_df_meta$Latitude)




## 18 Jun 2024
## modify the above code to calculate the sum of the number of seconds
## across each file

## folder that the tables are in
folder = "~/Documents/Chondestes_grammacus/"

## get a list of all the tables in that folder
my_files = list.files(path=folder,
                      pattern="Table.1.selections.txt",
                      full.names = TRUE)

## for every file with song data in it, calculate the duration of each syllable,
## and then sum them all 
## then take the sum of the sums

## initialize my running total of sums
running_sum = 0 

for (file in my_files) {
  ## print the name of the file
  print(paste("My file is:",file))
  
  ## read in the file
  song_df = read.delim(file)
  
  ## calculate the duration of each syllable
  ## duration is the end time minus the start time
  end_time = song_df$End.Time..s.
  start_time = song_df$Begin.Time..s.
  duration = abs(end_time - start_time)
  
  ## sum the durations of each syllable
  my_sum = sum(duration,na.rm=TRUE)
  
  ## print my_sum
  print(paste("My sum is:",my_sum))
  
  ## add my_sum to my running_sum 
  running_sum = my_sum + running_sum
  
}
print(paste("My running sum is:",running_sum))



