## this is a comment about my code, it starts with hashtags

## this is how you assign variables/objects
## variableName <- variableValue
x <- 5
y <- 6
x + y

## this is how you call a function
## functionName(arguments,for,the,function)
print("Hello world!")
print(x)
print("x")

## other simple functions that are important
odd_numbers <- c(1, 3, 5, 7, 9)
print(odd_numbers)
mean(odd_numbers)

## are things equal to each other?
1 == 1 
1 == 2 
1 > 2
1 < 2 
1 >= 2
1 <= 2

## not equal to 
1 != 2 
!(1 == 2)

## read in a spreadsheet
## get the file path where it is located
myfile <- "/Users/kprovost/Documents/20241111_174700.Table.1.selections.txt"
## ?read.table will tell you how to use a function
selections_df <- read.table(myfile,sep="\t",header=TRUE)

## want to know the column names?
colnames(selections_df)
## subsetting a dataframe by column using $ 
selections_df$Begin.Time..s.
selections_df$View
selections_df$End.Time..s. - selections_df$Begin.Time..s.
## only want the fifth beginning time
selections_df$Begin.Time..s.[5]
## fifth, sixth, and seventh
selections_df$Begin.Time..s.[5:7]
## five and seven, NOT six
selections_df$Begin.Time..s.[c(5,7)]

## quickly plotting things  
plot(selections_df$Begin.Time..s.)
barplot(selections_df$Begin.Time..s.)
hist(selections_df$Begin.Time..s.)
boxplot(selections_df$Begin.Time..s.)
plot(selections_df$Begin.Time..s.,
     selections_df$End.Time..s.)

## linear model and anova
predictor <- selections_df$BW.50...Hz.
response <- selections_df$Freq.25...Hz.
model1 <- lm(response~predictor)
model2 <- lm(Freq.25...Hz.~BW.50...Hz.,data=selections_df)
model3 <- lm(selections_df$Freq.25...Hz.~selections_df$BW.50...Hz.)
plot(predictor,response)
abline(model1) ## note to self abline only does straight lines
summary(model1)

## anovas need categorical predictors and continuous responses 
predictor2 <- selections_df$Species
response2 <- selections_df$Freq.25...Hz.
boxplot(response2~predictor2)
model2 <- aov(response2~predictor2)
summary(model2)

## principal components analysis
## 1) only use columns that have numbers in them
## 2) get rid of any data that is missing a value in that column
## 2.5) only use uncorrelated columns
## 3) run the prcomp() function

## frequency, bandwidth, inflections, slope, duration
my_columns <- c("Peak.Freq..Hz.","BW.90...Hz.",
                "PFC.Avg.Slope..Hz.ms.","PFC.Num.Inf.Pts","Dur.90...s.") ## get the columns I want
selections_df_smaller <- selections_df[,my_columns] ## only keep the columns I want
selections_df_smaller <- selections_df_smaller[complete.cases(selections_df_smaller),] ## only keep things that have data
pca <- prcomp(selections_df_smaller, scale.=TRUE, center=TRUE)
pca
summary(pca)
pca_data <- pca$x
pca_data <- as.data.frame(pca_data)
plot(pca_data$PC1,pca_data$PC2)
## you can color code it but you need to glue it back to the original data
rownames(pca_data)
rownames(selections_df)
selections_df_pca <- cbind(pca_data[rownames(pca_data),],selections_df[rownames(pca_data),])
plot(selections_df_pca$PC1,selections_df_pca$PC2,
     col=as.numeric(as.factor(selections_df_pca$Species)))

## light pollution
df = read.table("/Users/kprovost/Documents/Research/Julissa/KLP_LT1_22368216.csv",sep=",",header=T)
plot(as.factor(df$Time..GMT.04.00),df$Intensity..Lux)
df$Time..GMT.04.00
split_time <- strsplit(df$Time..GMT.04.00,":")
my_hours <- sapply(split_time,FUN=function(individual_time){
  individual_time[1]
})
df$my_hours <- my_hours
boxplot(df$Intensity..Lux~df$my_hours)



my_files <- list.files(path="~/PATH/TO/BIG/FOLDER/Selections/",
                       pattern=".Table.1.selections.txt$",
                       recursive = TRUE,
                       full.names = TRUE)
my_files
## one at a time, read in those files with read.table()
my_list <- lapply(X=my_files,
                  FUN=function(single_file){
                    my_sel_table <- read.table(single_file,
                                               header=TRUE,
                                               sep="\t")
                  })
## glue those files together into one big file
library(gtools)
my_big_sel_table <- do.call(what=smartbind,
                            args=my_list)
dim(my_big_sel_table)



