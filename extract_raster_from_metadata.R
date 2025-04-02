library(raster)
library(terra)

#devtools::install_github("rspatial/terra")
#install.packages('terra', repos='https://rspatial.r-universe.dev')

## read in the raster
light_ras = raster("/Users/kprovost/Documents/Research/Tyrannidae/F16_20100111-20110731_rad_v4.geotiff/F16_20100111-20110731_rad_v4.avg_vis_CROPPED.tif")
light_ras

## read in the data
my_csv = "/Users/kprovost/Documents/Research/Tyrannidae/metadata_csvs/Tyrannidae.csv"
df = read.csv(my_csv,header=TRUE,sep=",")
head(df)

## extract only the Lat and Long from the data
df_latlong = df[,c("Longitude","Latitude")]

## looking up the raster file cell associated with each lat long
cells = cellFromXY(object=light_ras,
          xy=df_latlong)

## combine the cells with the data as a new column
df$cells = cells
colnames(df)

## look up the light pollution value at each of those cells
## get the values of the light pollution -- warning, this is a big list of numbers
## and subset by the cells, which are the number in the values(light_ras)
light_pollution = values(light_ras)[cells]

## really quick summary of the light_pollution
summary(light_pollution)

## add the light pollution back to the original dataframe
df$light_pollution = light_pollution
colnames(df)

## plots
plot(hist(light_pollution))
boxplot(df$light_pollution~df$Quality)
plot(df$light_pollution~df$central_frequency) ## this won't work yet, but it's what we want

## first, output the new metadata
new_csv = "/Users/kprovost/Documents/Research/Tyrannidae/metadata_csvs/Tyrannidae_LightPollution.csv"
write.csv(df,
          new_csv,
          sep=",")

## second, glue the new metadata to the actual sound data
sound_csv = "/Users/kprovost/Documents/Research/Tyrannidae/predictresults/rvn.dat_trimmed_qABC.txt"
sound_df = read.table(sound_csv,
                      header=TRUE,
                      sep=" ")

head(df) ## metadata, we want Recording_ID
head(sound_df) ## we need to make a Recording_ID column from Begin.File

## remove the extra information from Begin.File
begin_file = sound_df$Begin.File
## take out any ".resample.48000" from the data with sub
begin_file = gsub(".resample.48000","",begin_file)
## remove the .mp3 and the .wav, in two separate steps
## first we are splitting by .wav
begin_file = sapply(begin_file,FUN=function(x){
  strsplit(x,".wav")[[1]][1]
})
## remove the .mp3
begin_file = gsub(".mp3","",begin_file)
## convert this into genus, species, recording_id
begin_file = strsplit(begin_file,split="-")
## convert the splits into a dataframe 
begin_file_df = do.call(rbind,begin_file)
colnames(begin_file_df) = c("Genus","Specific_epithet","Recording_ID")
## make sure the col names are capitalized the same as in the metadata df
head(begin_file_df)
head(df)
## convert our recording_id to numeric not text
begin_file_df = as.data.frame(begin_file_df)
begin_file_df$Recording_ID = as.numeric(begin_file_df$Recording_ID)

## glue our begin_file_df to sound_df
sound_df = cbind(sound_df,begin_file_df)
head(sound_df)

## do a quick intersect check of the Recording_IDs
intersect(sound_df$Recording_ID,
          df$Recording_ID)
## NULL means you don't have any overlaps to merge, check your dataframes
unique(sound_df$Recording_ID)
unique(df$Recording_ID)

## glue our sound_df to our metadata df via merge
df_merge = merge(x=sound_df,
      y=df,
      by=c("Genus","Specific_epithet","Recording_ID"),
      all.x=TRUE,
      all.y=FALSE)
df_merge = unique(df_merge)
View(df_merge)

## compare light pollution to sound traits
boxplot(df_merge$light_pollution~df_merge$Specific_epithet)
plot(df_merge$bottom.freq,df_merge$light_pollution)

## write out the merged data
write.csv(df_merge,
          file="/Users/kprovost/Documents/Research/Tyrannidae/predictresults/rvn.dat_trimmed_qABC_Metadata.txt")

## check where the lat longs are
View(unique(df_merge[,c("Longitude","Latitude")]))

## optionally re-run the PCA at this stage on these data, 
## rather than using the data before all the metadata was there

## average over every syllable in the recording for each sound variable

df_agg = aggregate(df_merge$difference~df_merge$Recording_ID,
          FUN=function(x){mean(x,na.rm=T)})
colnames(df_agg) = c("Recording_ID","difference")
hist(df_agg$difference)

x = df_merge$end - df_merge$start
unique(df_merge$Begin.File[which(x > 10)])
## remove syllables that are too long
df_merge = df_merge[df_merge$difference < 10,]
hist(df_merge$difference)

## generate a mean value for an entire recording across any statistic we want
df_agg = aggregate(cbind(df_merge$difference,df_merge$start,df_merge$end,df_merge$light_pollution)
                   ~df_merge$Recording_ID,
                   FUN=function(x){mean(x,na.rm=T)})
colnames(df_agg) = c("Recording_ID","difference","start","end","light_pollution")
head(df_agg)
plot(df_agg$light_pollution,df_agg$difference)

## standard deviation within individuals 
df_sd = aggregate(cbind(df_merge$difference,df_merge$start,df_merge$end,df_merge$light_pollution)
                   ~df_merge$Recording_ID,
                   FUN=function(x){sd(x,na.rm=T)})
colnames(df_sd) = c("Recording_ID","difference","start","end","light_pollution")
head(df_sd)
plot(df_sd$light_pollution,df_sd$difference)

## cropping -- do not need to do again
path = "/Users/kprovost/Documents/Research/Tyrannidae/metadata_csvs/"
pattern = "Tyrannidae"
files = list.files(path=path,pattern=pattern,full.names = T,recursive = F)
dflist = lapply(files,FUN=function(x){read.csv(x,header=T,sep=",")})
df = do.call(rbind,dflist)
df = df[c("Latitude","Longitude")]
df = unique(df)
summary(df)
maxlat = 68
minlat = -23
maxlon = -43
minlon = -155

tif = "/Users/kprovost/Documents/Research/Tyrannidae/F16_20100111-20110731_rad_v4.geotiff/F16_20100111-20110731_rad_v4.avg_vis.tif"
ras = raster(tif)
## need to crop to a smaller size I think
extent = extent(minlon,maxlon,minlat,maxlat)
ras2 = crop(ras,extent)
writeRaster(ras2,"/Users/kprovost/Documents/Research/Tyrannidae/F16_20100111-20110731_rad_v4.geotiff/F16_20100111-20110731_rad_v4.avg_vis_CROPPED.tif",overwrite=T)
png("~/test.png")
plot(sort(values(ras2)))
dev.off()