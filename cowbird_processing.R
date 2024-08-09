## 16 July 2024
## after first revision

## import the data and metadata we already have
library(raster)
df <- read.delim("~/Work/OSU/Molothrus ater/Data/Molothrus.ater.combined.PCA.merged_nocorrelations_metadata_28Nov2023.txt")
raw_anthromes_folder = "~/Work/OSU/Molothrus ater/Data/anthromes/"
setwd(raw_anthromes_folder)
raw_anthromes_files = list.files(path=raw_anthromes_folder,
                 pattern="AD.tif",full.names = T)
raw_anthromes_files = raw_anthromes_files[!grepl("stack",raw_anthromes_files)]
years = c("1940AD","1950AD","1960AD","1970AD","1980AD","1990AD","2000AD")

for(year in years) {
  print(year)
  this_year = raw_anthromes_files[grepl(year,raw_anthromes_files)]
  this_year_stack = raster::stack(this_year)
  name_this_year = paste("stack_",year,".tif",sep="")
  try({
    raster::writeRaster(this_year_stack,name_this_year,overwrite=T,format="GTiff")
  })
}

## import the data for each year between 1940 and 2000 from the unique lat-longs
df_latlong = df[,c("Long","Lat")]
df_latlong = unique(df_latlong)
df_latlong = df_latlong[complete.cases(df_latlong),]


stack_anthromes_files = list.files(path=raw_anthromes_folder,
                                   pattern="stack",full.names=T)

for(stack_file in stack_anthromes_files) {
  this_stack = stack(stack_file)
  names(this_stack) = c("cropland","grazing","ir_rice","popc","tot_irri","uopp")
  names(this_stack) = paste(names(this_stack),basename(stack_file),sep=".")
  cells = cellFromXY(this_stack,df_latlong)
  new_data = extract(this_stack,cells)
  df_latlong = cbind(df_latlong,new_data)
}

df_merged = merge(df,df_latlong,all=T)

write.table(df_merged,"~/Work/OSU/Molothrus ater/Data/Molothrus.ater.combined.PCA.merged_nocorrelations_metadata_16July2024.txt",
            sep="\t",row.names = F)

## use a general linear mixed model for each principal component that includes 
## time, urbanization, and their interaction as fixed effects and location as a random effect,
## This should allow you to assess whether songs change across time and with cropland cover, 
## while controlling for location. 

## read in the edited version 
df_edit = read.table("~/Work/OSU/Molothrus ater/Data/Molothrus.ater.combined.PCA.merged_nocorrelations_metadata_16July2024_EDITED.csv",
                     sep=",",header=T)
keep_colnames = c("BLB_ID","COUNTY","Lat","Long","YEAR","Shp.PC1","Shp.PC2","Shp.PC3","Shp.PC4",
                  "Shp.PC5","Prop.PC1","Prop.PC2","Prop.PC3","Prop.PC4","Prop.PC5",
                  "cropland_year","grazing_year","popc_year","uopp_year")

df_edit = df_edit[,keep_colnames]

library(lme4)
library(lmerTest)
prop_pc1_glm = lmer(Prop.PC1~cropland_year+grazing_year+uopp_year+(1|COUNTY)+YEAR,data=df_edit)
prop_pc2_glm = lmer(Prop.PC2~cropland_year+grazing_year+uopp_year+(1|COUNTY)+YEAR,data=df_edit) ## singular fit?
prop_pc3_glm = lmer(Prop.PC3~cropland_year+grazing_year+uopp_year+(1|COUNTY)+YEAR,data=df_edit) 
prop_pc4_glm = lmer(Prop.PC4~cropland_year+grazing_year+uopp_year+(1|COUNTY)+YEAR,data=df_edit)
prop_pc5_glm = lmer(Prop.PC5~cropland_year+grazing_year+uopp_year+(1|COUNTY)+YEAR,data=df_edit) 
summary(prop_pc1_glm)
summary(prop_pc2_glm)
summary(prop_pc3_glm)
summary(prop_pc4_glm)
summary(prop_pc5_glm)

prop_pc2_glm_A = lmer(Prop.PC2~cropland_year+uopp_year+(1|COUNTY)+YEAR,data=df_edit) ## singular fit?
summary(prop_pc2_glm_A)

shp_pc1_glm = lmer(Shp.PC1~cropland_year+grazing_year+uopp_year+(1|COUNTY)+YEAR,data=df_edit)
shp_pc2_glm = lmer(Shp.PC2~cropland_year+grazing_year+uopp_year+(1|COUNTY)+YEAR,data=df_edit) 
shp_pc3_glm = lmer(Shp.PC3~cropland_year+grazing_year+uopp_year+(1|COUNTY)+YEAR,data=df_edit) 
shp_pc4_glm = lmer(Shp.PC4~cropland_year+grazing_year+uopp_year+(1|COUNTY)+YEAR,data=df_edit)
shp_pc5_glm = lmer(Shp.PC5~cropland_year+grazing_year+uopp_year+(1|COUNTY)+YEAR,data=df_edit) 
summary(shp_pc1_glm)
summary(shp_pc2_glm)
summary(shp_pc3_glm)
summary(shp_pc4_glm)
summary(shp_pc5_glm)





library(AICcmodavg)
AICc(prop_pc1_glm)

corx=cor(df_edit[,c("Lat","Long","YEAR","Shp.PC1","Shp.PC2","Shp.PC3","Shp.PC4",
               "Shp.PC5","Prop.PC1","Prop.PC2","Prop.PC3","Prop.PC4","Prop.PC5",
               "cropland_year","grazing_year","uopp_year")],
    use ="everything")
corrplot::corrplot(corx,method = "number")



## below is from first submission
## TODO: clean this up when finished

## broken stick function
brokenstick = function(n) {
  to_sum = 1/(1:n)
  sums = sapply(1:n,FUN=function(i){
    sum(to_sum[i:n])
  })
  broken = sums/n
  return(broken)
}
x = brokenstick(1040)

## generate PCA for biostats

{
BLB26 <-
  read.delim(
    "/Users/kprovost/Documents/Postdoc_Working/Molothrus.ater.selections/BLB26.Table.1.selections.txt"
  )
colnames(BLB26) ## this prints the column names of this table

BLB26$Begin.Time..s.
plot(BLB26$Begin.Time..s.)
summary(BLB26)

## run a pca on single data file
prcomp(BLB26) ## this doesn't work -- not numerical

## get only numerical data and complete data
colSums(BLB26[, 4:5])
is.na(BLB26)
colSums(is.na(BLB26))
nrow(BLB26)
colSums(is.na(BLB26)) == nrow(BLB26)
badcolumns = which(colSums(is.na(BLB26)) == nrow(BLB26))

## which function
values = c("A", "B", "C", "B")
which(values == "B")

## make a copy of the data
BLB26.full <- BLB26
BLB26.full <- BLB26.full[,-badcolumns]

## check if numeric
for (item in colnames(BLB26.full)) {
  ## do something
  print(item)
  check = is.numeric(BLB26.full[, item])
  
  if (check == TRUE) {
    ## do something
    ## keep this column
    ## do nothing
    print("keep!")
  } else {
    ## do something else
    ## get rid of this column
    badcolumn = which(colnames(BLB26.full) == item) ## get the column number
    BLB26.full <- BLB26.full[,-badcolumn]
  }
}

## run our pca
pca = prcomp(BLB26.full, center = TRUE, scale. = FALSE)
## we need to scale this
str(pca)
summary(pca)
rotation = pca$rotation
importance = summary(pca)$importance
data = pca$x

sapply(BLB26.full, dplyr::n_distinct)

badcolumns2 <- which(sapply(BLB26.full, dplyr::n_distinct) == 1)
BLB26.full <- BLB26.full[,-badcolumns2]

## run our pca
pca2 = prcomp(BLB26.full, center = TRUE, scale. = TRUE)
str(pca2)
summary(pca2)
rotation2 = pca2$rotation
importance2 = summary(pca2)$importance
data2 = pca2$x

## oops these need to be dataframes
data = as.data.frame(data)
data2 = as.data.frame(data2)
plot(data$PC1, data2$PC1)
}
{
## 2 November 2022

folder = "/Users/kprovost/Documents/Postdoc_Working/Molothrus.ater.selections"
files = list.files(path = folder,
                   pattern = "selections.txt",
                   full.names = T)

blb_data = lapply(
  files,
  FUN = function(f) {
    ## with each file called "f" in files
    ## read the file f
    return(read.delim(f))
  }
)

blb_data[[1]][, 1:3]

install.packages("gtools")

big_blb_data = do.call(what = gtools::smartbind, args = blb_data)
hist(big_blb_data$Low.Freq..Hz.)

new_file = "/Users/kprovost/Documents/Postdoc_Working/Molothrus.ater.selections/Molothrus.ater.combined.Table.1.selections.txt"
write.table(
  big_blb_data,
  new_file,
  quote = F,
  sep = "\t",
  row.names = F
)
new_file = "~/Work/Molothrus.ater.combined.Table.1.selections.txt"

big_blb_data = read.table(new_file,sep="\t",header=T)

big_blb_data$Begin.Time..s.
plot(big_blb_data$Begin.Time..s.)
summary(big_blb_data)

## get only numerical data and complete data
colSums(big_blb_data[, 4:5])
is.na(big_blb_data)
colSums(is.na(big_blb_data))
nrow(big_blb_data)
colSums(is.na(big_blb_data)) == nrow(big_blb_data)
badcolumns = which(colSums(is.na(big_blb_data)) == nrow(big_blb_data))

## make a copy of the data
big_blb_data.full <- big_blb_data
big_blb_data.full <- big_blb_data.full[,-badcolumns]

## check if numeric
for (item in colnames(big_blb_data.full)) {
  ## do something
  print(item)
  check = is.numeric(big_blb_data.full[, item])
  
  if (check == TRUE) {
    ## do something
    ## keep this column
    ## do nothing
    print("keep!")
  } else {
    ## do something else
    ## get rid of this column
    badcolumn = which(colnames(big_blb_data.full) == item) ## get the column number
    big_blb_data.full <- big_blb_data.full[,-badcolumn]
  }
}

complete.cases(big_blb_data.full) ## remove stuff with only some missing data
big_blb_data.full <-
  big_blb_data.full[complete.cases(big_blb_data.full),]

sapply(big_blb_data.full, dplyr::n_distinct)

badcolumns2 <-
  which(sapply(big_blb_data.full, dplyr::n_distinct) == 1)
big_blb_data.full <- big_blb_data.full[,-badcolumns2]

## check for highly correlated variables
big_blb_data.full = big_blb_data.full[,!(colnames(big_blb_data.full) %in% cols_to_remove)]
r = cor(big_blb_data.full)
r2 = r^2
corrplot::corrplot(r2,"ellipse",order="hclust")
diag(r2) = 0
r2_thresh = r2
r2_thresh[r2_thresh<0.75] =0
#corrplot::corrplot(r2_thresh,"ellipse",order="hclust",diag=F)

colsum=colSums(r2)
x=lapply(1:ncol(r2_thresh),FUN=function(i){
  coln = r2_thresh[,i]
  maxn=round(max(coln,na.rm=T),2)
  if(maxn!=0){
    bad = c(colnames(r2_thresh)[i],maxn,":",names(which(coln == maxn)))
    print(bad)
    return(bad)
  }
})
which(r2_thresh == max(r2_thresh,na.rm=T))

## list of ones to remove
cols_to_remove = c("Begin.Clock.Time",
                   "Begin.Date.Time",
                   "File.Offset..s.",
                   "Beg.File.Samp..samples.",
                   "Begin.Time..s.",
                   "Begin.Sample..samples.",
                   "Time.5...s.",
                   "Time.25...s.",
                   "Center.Time..s.",
                   "Max.Time..s.",
                   "Peak.Time..s.",
                   "Time.75...s.",
                   "Time.95...s.",
                   "End.Sample..samples.",
                   "End.File.Samp..samples.",
                   "End.Time..s.",
                   "End.Clock.Time",
                   "Length..frames.",
                   "Sample.Length..samples.",
                   "Delta.Time..s.",
                   "Dur.90...s.",
                   "Dur.50...s.",
                   "PFC.Min.Slope..Hz.ms.",
                   "High.Freq..Hz.",
                   "Low.Freq..Hz.",
                   "Inband.Power..dB.FS.",
                   "Peak.Power.Density..dB.FS.Hz.",
                   "Energy..dB.FS.",
                   "Freq.25...Hz.",
                   "Freq.75...Hz.",
                   "Center.Freq..Hz.",
                   "Max.Freq..Hz."
                   )

## run our pca
pca2 = prcomp(big_blb_data.full, center = TRUE, scale. = TRUE)
str(pca2)
summary(pca2)
rotation2 = pca2$rotation
importance2 = summary(pca2)$importance
data2 = pca2$x

write.table(rotation2,"~/Work/cowbird_rotation_pca_fix_28Nov2023.txt",sep="\t")
write.table(importance2,"~/Work/cowbird_importance_pca_fix_28Nov2023.txt",sep="\t")
write.table(pca2$sdev,"~/Work/cowbird_sdev_pca_fix_28Nov2023.txt",sep="\t")
write.table(pca2$center,"~/Work/cowbird_center_pca_fix_28Nov2023.txt",sep="\t")
write.table(pca2$scale,"~/Work/cowbird_scale_pca_fix_28Nov2023.txt",sep="\t")

## oops these need to be dataframes
data2 = as.data.frame(data2)
plot(data2$PC1, data2$PC2)

pca_plus_big_data = cbind(big_blb_data.full, data2)


## note to self: add the full raw data to this as well
## merge
big_blb_data = read.table(new_file,sep="\t",header=T)
merged_data = merge(big_blb_data, pca_plus_big_data, all = T)
meta = read.delim("~/Work/cowbird data metadata urbanization - Sheet1.tsv")
merged_data = merge(meta,merged_data, all = T)

new_file_pca = "~/Work/Molothrus.ater.combined.PCA.merged_nocorrelations_metadata_28Nov2023.txt"
write.table(
  merged_data,
  new_file_pca,
  quote = F,
  sep = "\t",
  row.names = F
)
}

## look at correlations and stuff
{
## import some data
new_file_pca = "/Users/kprovost/Documents/Postdoc_Working/Molothrus.ater.selections/Molothrus.ater.combined.PCA.merged.txt"
corr_data_original = read.delim(new_file_pca, header = T, sep = "\t")
colnames(corr_data_original)
corr_data = corr_data_original[, c(86:140)] ## plot all PCs and how correlated they are
corr = cor(corr_data, use = "pairwise.complete.obs")
install.packages("corrplot")
library(corrplot)
corrplot::corrplot(corr, method = "color")

corr_data_2 = corr_data_original[, c(2:40)] ## plot all the numerical raw data I can plot right now
corr_data_2 = corr_data_original[, c("Freq.25...Hz.", "Freq.95...Hz.")]
corr_2 = cor(corr_data_2, use = "pairwise.complete.obs")
corrplot::corrplot(corr_2, method = "ellipse", order = "hclust")
}

## soundshape
{
library(SoundShape)
install.packages("SoundShape")

path = "/Users/kprovost/Documents/Postdoc_Working/Molothrus.ater.selections"

wav.at = file.path(path, "ClippedWavs")
dir.create(wav.at)

store_dir = file.path(wav.at, "Stored")
dir.create(store_dir)

## cut our wavfiles
## read in our wavfiles one at a time
## first we get the wavfiles
files = list.files(path = path,
                   pattern = "wav$",
                   full.names = T)
for (my_file in files) {
  print(my_file)
  #my_file = files[30]
  wav = tuneR::readWave(my_file)
  ## look up the data with this file
  my_data_file = sub(pattern = "wav", replacement = "Table.1.selections.txt", my_file)
  
  if (file.exists(my_data_file) == TRUE) {
    wav_data = read.delim(my_data_file)
    ## use the start and end time of the data to tell R where to clip the wavfile
    #row_number = 1
    for (row_number in 1:nrow(wav_data)) {
      print(row_number)
      start_time = wav_data$Begin.Time..s.[row_number]
      stop_time  = wav_data$End.Time..s.[row_number]
      cut_wav = seewave::cutw(
        wave = wav,
        from = start_time,
        to = stop_time,
        output = "Wave"
      )
      tuneR::writeWave(cut_wav, filename = file.path(wav.at, paste(
        basename(my_file), row_number, "wav", sep = "."
      )))
    }
  }
}

## align our wavfiles!

## we need that custom function Kaiya wrote to do this because soundshape sucks sometimes

align.wave.custom <-
  function(wav.at = NULL,
           wav.to = "Aligned",
           time.length = 1,
           time.perc = 0.0,
           dBlevel = 25,
           f = 44100,
           wl = 512,
           ovlp = 70,
           overwrite = F,
           verbose = T,
           alignCheckpoint = 1)  {
    if (is.null(wav.at)) {
      stop("Use 'wav.at' to specify folder path where '.wav' files are stored")
    }
    
    # Create folder to store aligned calls
    if (!dir.exists(file.path(wav.at, wav.to)))
      dir.create(file.path(wav.at, wav.to))
    
    # Replace sounds for each ".wav" file in a folder
    filestoalign = list.files(wav.at, pattern = ".wav")
    numalignfiles = length(filestoalign)
    for (j in alignCheckpoint:numalignfiles) {
      file = filestoalign[j]
      if (verbose == T) {
        print(paste(j, file, numalignfiles))
      }
      if (overwrite == T ||
          !(file.exists(file.path(wav.at, wav.to, file)))) {
        orig.wav0 <- tuneR::readWave(paste(wav.at, "/", file, sep = ""))
        
        # Add silence to fill sound window and prevent error
        orig.wav <-
          seewave::addsilw(
            orig.wav0,
            f = f,
            at = "end",
            d = (time.length * 10),
            output = "Wave"
          )
        
        # create spectro object
        orig.spec <-
          seewave::spectro(
            orig.wav,
            f = f,
            wl = wl,
            ovlp = ovlp,
            osc = F,
            grid = F,
            plot = F
          )
        
        # Acquire contours
        cont.spec <-
          grDevices::contourLines(
            x = orig.spec$time,
            y = orig.spec$freq,
            z = t(orig.spec$amp),
            levels = seq(-dBlevel,-dBlevel, 1)
          )
        
        # vectors to store minimum and maximum time values
        min.spec <- numeric(length(cont.spec))
        max.spec <- numeric(length(cont.spec))
        
        # minimum and maximum time values among contours detected
        for (i in 1:length(min.spec)) {
          min.spec[i] <- min(cont.spec[[i]]$x)
        }
        for (i in 1:length(max.spec)) {
          max.spec[i] <- max(cont.spec[[i]]$x)
        }
        
        # minimum and maximum time values
        t.min <- min(min.spec)
        t.max <- max(max.spec)
        
        if ((t.min - (time.perc * time.length)) < 0)
          stop("Time percentage is too large. Consider a smaller value of 'time.perc'")
        
        # cut Wave file using minimum and maximum time values
        short.wav0 <- seewave::deletew(
          orig.wav,
          f = f,
          output = "Wave",
          from = (t.max + (time.perc * time.length)),
          to = max(orig.spec$time)
        )
        
        short.wav <-
          seewave::deletew(
            short.wav0,
            f = f,
            output = "Wave",
            from = 0,
            to = (t.min - (time.perc * time.length))
          )
        
        
        # Add silence to fill sound window
        final.wav <-
          seewave::addsilw(
            short.wav,
            f = f,
            at = "end",
            d = time.length,
            output = "Wave"
          )
        
        tuneR::writeWave(final.wav, file.path(wav.at, wav.to, file), extensible = F)
      } else {
        if (verbose == T) {
          print("SKIPPING")
        }
      }
    } #end loop
    
  } #end function

align.wave.custom(
  wav.at = wav.at,
  wav.to = "Aligned",
  time.length = 1.0,
  f = 48000,
  time.perc = 0,
  alignCheckpoint = 583,
  overwrite = F
)
## time.length is the percent of the sound
## try playing around with time.length if things are not working

## generate the eigensample
eig.sample <- eigensound(
  analysis.type = "threeDshape",
  f = 48000,
  log.scale = TRUE,
  wav.at = file.path(wav.at, "Aligned"),
  store.at = store_dir
)
pca.eig.sample <- stats::prcomp(
  geomorph::two.d.array(
    eig.sample
  )
)
summary(pca.eig.sample)
rotation = pca.eig.sample$rotation
importance = summary(pca.eig.sample)$importance
data = pca.eig.sample$x
rotation[1:5,1:10]
importance[,1:10]
data[1:5,1:10]
data = as.data.frame(data)
plot(data$PC1,data$PC2)

write.table(data,"~/cowbird_pca_data.txt")
write.table(rotation,"~/cowbird_pca_rotation.txt")
write.table(importance,"~/cowbird_pca_importance.txt")
}

## TODO: urbanization layers

install.packages("raster")
library(raster)

wcdata = getData("worldclim",download=T,res=10,var="bio")
wcdata[[1]]
plot(wcdata[[1]])
##
crop_extent = extent(-85,-80,35,45)
wcdata_oh = crop(wcdata,crop_extent)
plot(wcdata_oh[[1]])

cowbird_data = read.csv("/Users/kprovost/Documents/Postdoc_Working/Molothrus.ater.selections/BLB_klp_Molothrus.ater.csv")
plot(wcdata_oh[[1]])
points(cowbird_data$LONGITUDE,cowbird_data$LATITUDE)

cells = cellFromXY(wcdata_oh,cowbird_data[,c("LONGITUDE","LATITUDE")])

cell_data = extract(wcdata_oh,cells)

## put together the env data and the cowbird data
cowbird_env = cbind(cowbird_data,cell_data)

old_file = "/Users/kprovost/Downloads/dataverse_files/Anthromes-12k-DGG/raw-data/HYDE/stack_1940AD.tif"
new_file = "/Users/kprovost/Downloads/dataverse_files/Anthromes-12k-DGG/raw-data/HYDE/stack_2017AD.tif"

new_urban = stack(new_file)
plot(new_urban)

layer_names = c("cropland","grazing","ir_rice","popc","tot_irri","uopp")
names(new_urban) = layer_names

crop_extent = extent(-85,-80,35,45)
new_urban_oh = crop(new_urban,crop_extent)
names(new_urban_oh) = layer_names
plot(new_urban_oh)

cells_new = cellFromXY(new_urban_oh,cowbird_data[,c("LONGITUDE","LATITUDE")])
cells_new_data = extract(new_urban_oh,cells_new)

cowbird_new_urban = cbind(cowbird_data,cells_new_data)


old_urban = stack(old_file)
plot(old_urban)

layer_names = c("cropland","grazing","ir_rice","popc","tot_irri","uopp")
names(old_urban) = layer_names

crop_extent = extent(-85,-80,35,45)
old_urban_oh = crop(old_urban,crop_extent)
names(old_urban_oh) = layer_names
plot(old_urban_oh)

cells_old = cellFromXY(old_urban_oh,cowbird_data[,c("LONGITUDE","LATITUDE")])
cells_old_data = extract(old_urban_oh,cells_old)

cowbird_old_urban = cbind(cowbird_data,cells_old_data)

## compare new data and old data
cowbird_old_urban
cowbird_new_urban
## find which columns to compare
colnames(cowbird_new_urban) ## cols 26-31 are our new data in both

cowbird_dif_urban = cowbird_new_urban[,26:31] - cowbird_old_urban[,26:31]

dif_urban_oh = new_urban_oh - old_urban_oh
plot(dif_urban_oh)

cowbird_dif_urban = cbind(cowbird_data,cowbird_dif_urban)

dev.off()
boxplot(cowbird_dif_urban$cropland,xlab="cropland",ylab="difference")
boxplot(cowbird_dif_urban$popc,xlab="population",ylab="difference")

boxplot(cowbird_dif_urban$grazing~cowbird_dif_urban$COUNTY,xlab="county",ylab="difference in grazing")
boxplot(cowbird_dif_urban$grazing~cowbird_dif_urban$SEX,xlab="sex",ylab="difference in grazing")

plot(cowbird_dif_urban$grazing,cowbird_dif_urban$LATITUDE)
plot(cowbird_dif_urban$grazing,cowbird_dif_urban$YEAR_COLLECTED)

write.table(cowbird_dif_urban,"~/cowbird_dif_urban.txt")
write.table(cowbird_new_urban,"~/cowbird_new_urban.txt")
write.table(cowbird_old_urban,"~/cowbird_old_urban.txt")
writeRaster(dif_urban_oh,"~/dif_urban_oh.tif",format="GTiff")









## read.table(blah blah blah)
cowbird=read.table("/Users/kprovost/cowbird_dif_urban.txt")
cowbird$SEX
table(cowbird$SEX)
cowbird$cropland
mean(cowbird$cropland)

plot(cowbird$cropland)
plot(cowbird$ID,cowbird$cropland)
plot(cowbird$YEAR_COLLECTED,cowbird$cropland)

boxplot(cowbird$cropland)
boxplot(cowbird$cropland~cowbird$SEX)
boxplot(cowbird$cropland~cowbird$YEAR_COLLECTED)

hist(cowbird$cropland)

barplot(cowbird$cropland)
barplot(table(cowbird$SEX))
barplot(table(cowbird$YEAR_COLLECTED))

cowbird_urban=read.table("/Users/kprovost/cowbird_dif_urban.txt")
cowbird_song =read.table("/Users/kprovost/Documents/Postdoc_Working/Molothrus.ater.selections/combined_cowbird_metadata_shape_properties.txt",
                         header=T)

cowbird_urban_song = merge(cowbird_urban,cowbird_song)
head(cowbird_urban_song)

plot(cowbird_urban_song$cropland,cowbird_urban_song$Prop.PC1,col="red") ## note about what i found
plot(cowbird_urban_song$cropland,cowbird_urban_song$Prop.PC1,col=as.numeric(as.factor(cowbird_urban_song$SEX))) ## note about what i found

mod = lm(cowbird_urban_song$Prop.PC2~cowbird_urban_song$cropland)
plot(cowbird_urban_song$cropland,cowbird_urban_song$Prop.PC2,col="red") 
abline(mod)
summary(mod)

boxplot(cowbird_urban_song$Prop.PC1~cowbird_urban_song$COUNTY)
mod2 = aov(cowbird_urban_song$Prop.PC1~cowbird_urban_song$COUNTY)
summary(mod2)
TukeyHSD(mod2)

t.test(cowbird_urban_song$cropland[cowbird_urban_song$COUNTY=="Franklin"],
       cowbird_urban_song$cropland[cowbird_urban_song$COUNTY=="Ottawa"])

boxplot(cowbird_urban_song$Prop.PC1~cowbird_urban_song$MONTH_COLLECTED)


## MAPS
library(raster)
shp=raster::shapefile("/Users/kprovost/Downloads/cb_2016_us_state_500k/cb_2016_us_state_500k.shp")
plot(shp,xlim=c(-85,-80),ylim=c(38,42))
cowbird_song =read.table("/Users/kprovost/Downloads/combined_cowbird_metadata_shape_properties.txt",
                         header=T)
points(cowbird_song$Long,cowbird_song$Lat)

wcdata = getData("worldclim",download=T,res=10,var="bio")



png(filename="/Users/kprovost/Downloads/climate_and_cowbird_ohio_map.png",
    width=600,height=600)
plot(wcdata[[1]],xlim=c(-85,-80),ylim=c(38,42)) 
## we can change this to the urbanization data if we want
plot(shp,xlim=c(-85,-80),ylim=c(38,42),add=T,col="grey") 
points(cowbird_song$Long,cowbird_song$Lat,col="red")
dev.off()

#stack_1940 = raster::raster("~/Desktop/stack_1940AD.tif")
stack_1940 = raster::stack("~/Desktop/stack_1940AD.tif")
stack_2017 = raster::stack("~/Desktop/stack_2017AD.tif")
plot(stack_1940[[1]],xlim=c(-85,-80),ylim=c(38,42)) 

## panels
par(mfrow=c(2,2))
plot(stack_1940[[1]],xlim=c(-85,-80),ylim=c(38,42),main="cropland, 1940AD") 
plot(stack_1940[[2]],xlim=c(-85,-80),ylim=c(38,42),main="grazing, 1940AD") 
#plot(stack_1940[[3]],xlim=c(-85,-80),ylim=c(38,42),main="rice irrigation, 1940AD") 
plot(stack_1940[[4]],xlim=c(-85,-80),ylim=c(38,42),main="census population, 1940AD") 
#plot(stack_1940[[5]],xlim=c(-85,-80),ylim=c(38,42),main="total irrigation, 1940AD") 
plot(stack_1940[[6]],xlim=c(-85,-80),ylim=c(38,42),main="urban occupancy, 1940AD") 



## going to redo some analyses 
cowbird_data <- read.delim("~/Work/OSU/Molothrus ater/cowbird pca and anthromes data 5Dec2023.txt")
## lms and aovs
## make sure correct for latitude


m1 = lm(cowbird_data$PC2~cowbird_data$cropland.new*cowbird_data$Lat)
summary(m1)
par(mfrow=c(1,2))
plot(cowbird_data$Lat,cowbird_data$PC2)
abline(lm(cowbird_data$PC2~cowbird_data$Lat),col="red")
plot(cowbird_data$cropland.new,cowbird_data$PC2)
abline(lm(cowbird_data$PC2~cowbird_data$cropland.new),col="red")

par(mfrow=c(1,1))
plot(cowbird_data$Lat,cowbird_data$cropland.dif)

par(mfrow=c(1,2))
boxplot(cowbird_data$PC2~cowbird_data$COUNTY)
boxplot(cowbird_data$PC2~cowbird_data$MONTH)



sink(file="~/Work/OSU/Molothrus ater/test_aovs.txt")
for(i in c(1:4,9:18,21:41)) {
  print(i)
  print(colnames(cowbird_data)[i])
  as1=aov(cowbird_data$Shp.PC1~cowbird_data[,i])
  as2=aov(cowbird_data$Shp.PC2~cowbird_data[,i])
  as3=aov(cowbird_data$Shp.PC3~cowbird_data[,i])
  as4=aov(cowbird_data$Shp.PC4~cowbird_data[,i])
  as5=aov(cowbird_data$Shp.PC5~cowbird_data[,i])
  ap1=aov(cowbird_data$PC1~cowbird_data[,i])
  ap2=aov(cowbird_data$PC2~cowbird_data[,i])
  ap3=aov(cowbird_data$PC3~cowbird_data[,i])
  print(summary(as1))
  print(summary(as2))
  print(summary(as3))
  print(summary(as4))
  print(summary(as5))
  print(summary(ap1))
  print(summary(ap2))
  print(summary(ap3))
}
sink(file=NULL)
sink(file="~/Work/OSU/Molothrus ater/test_lms.txt")
for(i in c(1:4,9:18,21:41)) {
  print(i)
  print(colnames(cowbird_data)[i])
  as1=lm(cowbird_data$Shp.PC1~cowbird_data[,i])
  as2=lm(cowbird_data$Shp.PC2~cowbird_data[,i])
  as3=lm(cowbird_data$Shp.PC3~cowbird_data[,i])
  as4=lm(cowbird_data$Shp.PC4~cowbird_data[,i])
  as5=lm(cowbird_data$Shp.PC5~cowbird_data[,i])
  ap1=lm(cowbird_data$PC1~cowbird_data[,i])
  ap2=lm(cowbird_data$PC2~cowbird_data[,i])
  ap3=lm(cowbird_data$PC3~cowbird_data[,i])
  print(summary(as1))
  print(summary(as2))
  print(summary(as3))
  print(summary(as4))
  print(summary(as5))
  print(summary(ap1))
  print(summary(ap2))
  print(summary(ap3))
}
sink(file=NULL)
sink(file="~/Work/OSU/Molothrus ater/test_aovs_latitude.txt")
for(i in c(1:4,9:18,21:41)) {
  print(i)
  print(colnames(cowbird_data)[i])
  as1=aov(cowbird_data$Shp.PC1~cowbird_data[,i]+cowbird_data$Lat)
  as2=aov(cowbird_data$Shp.PC2~cowbird_data[,i]+cowbird_data$Lat)
  as3=aov(cowbird_data$Shp.PC3~cowbird_data[,i]+cowbird_data$Lat)
  as4=aov(cowbird_data$Shp.PC4~cowbird_data[,i]+cowbird_data$Lat)
  as5=aov(cowbird_data$Shp.PC5~cowbird_data[,i]+cowbird_data$Lat)
  ap1=aov(cowbird_data$PC1~cowbird_data[,i]+cowbird_data$Lat)
  ap2=aov(cowbird_data$PC2~cowbird_data[,i]+cowbird_data$Lat)
  ap3=aov(cowbird_data$PC3~cowbird_data[,i]+cowbird_data$Lat)
  print(summary(as1))
  print(summary(as2))
  print(summary(as3))
  print(summary(as4))
  print(summary(as5))
  print(summary(ap1))
  print(summary(ap2))
  print(summary(ap3))
}
sink(file=NULL)
sink(file="~/Work/OSU/Molothrus ater/test_lms_latitude.txt")
for(i in c(1:4,9:18,21:41)) {
  print(i)
  print(colnames(cowbird_data)[i])
  as1=lm(cowbird_data$Shp.PC1~cowbird_data[,i]+cowbird_data$Lat)
  as2=lm(cowbird_data$Shp.PC2~cowbird_data[,i]+cowbird_data$Lat)
  as3=lm(cowbird_data$Shp.PC3~cowbird_data[,i]+cowbird_data$Lat)
  as4=lm(cowbird_data$Shp.PC4~cowbird_data[,i]+cowbird_data$Lat)
  as5=lm(cowbird_data$Shp.PC5~cowbird_data[,i]+cowbird_data$Lat)
  ap1=lm(cowbird_data$PC1~cowbird_data[,i]+cowbird_data$Lat)
  ap2=lm(cowbird_data$PC2~cowbird_data[,i]+cowbird_data$Lat)
  ap3=lm(cowbird_data$PC3~cowbird_data[,i]+cowbird_data$Lat)
  print(summary(as1))
  print(summary(as2))
  print(summary(as3))
  print(summary(as4))
  print(summary(as5))
  print(summary(ap1))
  print(summary(ap2))
  print(summary(ap3))
}
sink(file=NULL)

## kaiya doing some stuff that Kristen does not have to do 
{
## 25 shape PCAs to get over 50%
shape_pca = read.table("/Users/kprovost/Documents/Postdoc_Working/Molothrus.ater.selections/cowbird_pca_data.txt")
shape_pca$ID = rownames(shape_pca)
shape_pca_small = shape_pca[,c("ID","PC1","PC2","PC3","PC4","PC5")]
colnames(shape_pca_small) = c("ID","Shp.PC1","Shp.PC2","Shp.PC3","Shp.PC4","Shp.PC5")
shape_pca_small$Genus = "Molothrus"
shape_pca_small$Species = "ater"
shape_pca_small$ID=sub("Molothrus.ater.BLB.","",shape_pca_small$ID)
shape_pca_small$ID=sub(".wav","",shape_pca_small$ID)
## split hte ID into ID and syllable
syllable=sapply(shape_pca_small$ID,FUN=function(x){
  strsplit(x,"\\.")[[1]][2]
})
IDs=sapply(shape_pca_small$ID,FUN=function(x){
  strsplit(x,"\\.")[[1]][1]
})
shape_pca_small$Selection = as.numeric(syllable)
shape_pca_small$ID = as.numeric(IDs)

properties_pca = read.table("/Users/kprovost/Documents/Postdoc_Working/Molothrus.ater.selections/Molothrus.ater.combined.PCA.merged.txt",sep="\t",header=T)
properties_pca_small = properties_pca[,c("Begin.File","PC1","PC2","PC3","PC4","PC5","Selection")]
colnames(properties_pca_small) = c("ID","Prop.PC1","Prop.PC2","Prop.PC3","Prop.PC4","Prop.PC5","Selection")
properties_pca_small$Genus = "Molothrus"
properties_pca_small$Species = "ater"
properties_pca_small$ID=sub("BLB","",properties_pca_small$ID)
properties_pca_small$ID=sub(".wav","",properties_pca_small$ID)

head(shape_pca_small)
head(properties_pca_small)
shape_prop_pca_small = merge(shape_pca_small,properties_pca_small,all=T)
plot(shape_prop_pca_small$Prop.PC1,shape_prop_pca_small$Shp.PC1)


metadata = read.csv("/Users/kprovost/Documents/Postdoc_Working/Molothrus.ater.selections/BLB_klp_Molothrus.ater.csv")
metadata_small = metadata[,c("ID","GENUS_NAME","SPECIES_EPITHET","YEAR_COLLECTED","LONGITUDE","LATITUDE")]
colnames(metadata_small) = c("ID","Genus","Species","Year","Long","Lat")

shape_prop_pca_small$Year = 0
shape_prop_pca_small$Long = 0
shape_prop_pca_small$Lat = 0

for(i in 1:nrow(metadata_small)){
  print(i)
  meta_df = metadata_small[i,]
  shape_prop_pca_small$Year[shape_prop_pca_small$ID == meta_df$ID] = meta_df$Year
  shape_prop_pca_small$Long[shape_prop_pca_small$ID == meta_df$ID] = meta_df$Long
  shape_prop_pca_small$Lat[shape_prop_pca_small$ID == meta_df$ID] = meta_df$Lat
}
write.table(shape_prop_pca_small,"~/combined_cowbird_metadata_shape_properties.txt")
shape_prop_pca_small = read.table("/Users/kprovost/Documents/Postdoc_Working/combined_cowbird_metadata_shape_properties.txt",header=T)
plot(shape_prop_pca_small$Prop.PC1,shape_prop_pca_small$Shp.PC1)
boxplot(shape_prop_pca_small$Shp.PC1~shape_prop_pca_small$Year)

corrplot::corrplot(cor(shape_prop_pca_small[,5:14],use="pairwise.complete.obs"),method="ellipse")

## trying out a clustering algorithm
id=1322
small = shape_prop_pca_small[shape_prop_pca_small$ID==id,]
plot(small$Shp.PC1,small$Shp.PC2)
}


### code from 2 July 2024
df <- read.delim("~/Work/OSU/Molothrus ater/Data/combined_cowbird_metadata_shape_properties.txt")

## need to extract the env data

## run a generalized linear mixed model for each pc
## time*urb + 1|lat + 1|lon + 1|recording

library(raster)
ras_files = list.files(path="~/Work/OSU/Molothrus ater/Data/anthromes/raw-data/HYDE",
                       pattern="*.tif$",full.names = T)

for(file in ras_files[6]) {
  ## iterate over each raster in the folder
  print(file)
  r = stack(file)
  r_names = sub("X","",names(r))
  
  ## iterate over each layer
  for(i in 1:dim(r)[3]){
    cat(i)
    ri = r[[i]]
   
    ## generate a filename
    ri_filename = sub("tif",paste(r_names[i],".tif",sep=""),file)
    
    ## write out the single layer as its own raster
   writeRaster(ri,ri_filename,format="GTiff",overwrite=T)
  }
}
## gzip anything with BC in the name?
#R.utils::gzip()
