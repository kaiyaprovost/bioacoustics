## do enms
library(raster)
library(spThin)

## first get the background data
#wc=raster::getData("pworldclim",res=10,var="bio")
#wc_p=raster::getData("worldclim_past",res=10,var="bio")
wc = stack(list.files("/Users/kprovost/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Environment/paleoclim2.1_jan2020/Paleoclim_Current_1979-2013_10arcmin_CHELSA_cur_V1_2B_r10m/10min/",
                      pattern=".tif$",full.names = T))

## and the occurrence data
occ = read.table("/Users/kprovost/Documents/Postdoc_Working/GBIF/Zonotrichia-leucophrys_occurrences_subspplabelR.txt")

min_lon = min(occ$longitude,na.rm=T)
min_lat = min(occ$latitude,na.rm=T)
max_lon = max(occ$longitude,na.rm=T)
max_lat = max(occ$latitude,na.rm=T)

bg = wc[[1]]
bg[!(is.na(bg))] = 0
plot(bg)
bg = crop(bg,extent(min_lon-10,max_lon+10,min_lat-10,max_lat+10))
plot(bg)

wc_sm = crop(wc,extent(min_lon-10,max_lon+10,min_lat-10,max_lat+10))

## thin to one per grid cell 
cells = cellFromXY(bg,occ[,c("longitude","latitude")])
occ$cells=cells
uniq_cells = unique(cells)
bool = bg
bool[as.numeric(uniq_cells)]=1
plot(bool)
mids = xyFromCell(bool,uniq_cells)

library(ENMeval)
res = ENMevaluate(occ=mids, env = wc_sm, method='block', 
                  parallel=T, numCores=4, fc=c("L", "LQ"), 
                  RMvalues=seq(0.5,4,0.5), rasterPreds=T,
                  updateProgress = T)
## this uses your occurences and trimmed env data. it will
## separate the data into 4 blocks, split sample into 4 equal quads
## running in parallel, with 4 cores. 
## the feature classes used are Linear, Linear+Quadratic, and Hunge.
## the regularization values will range from 1/2 to 4 by 1/2s.
## and rasterPreds=T means not running the predict function we ran before
## and thus can't have any AICc

setsort = res@results[order(res@results[,'avg.test.or10pct']),] ## previously Mean.ORmin
setsort2 = setsort[order(setsort[,'avg.test.AUC'], decreasing=TRUE),] ## previously Mean.AUC
top = setsort2[1,]
print(top)
write.csv(setsort2,file=paste("~/ENMeval_Zonotrichia-leucophrys_ResultsTable_paleoclim.csv",sep=""))

best = which(as.character(res@results[,1]) == as.character(setsort2[1,1]))

pred.raw = predict(wc_sm, res@models[[best]])
writeRaster(pred.raw,filename="~/ENMeval_Zonotrichia-leucophrys_ResultsTable_paleoclim.asc",
            format="ascii",overwrite=T)

ev.set <- evaluate(mids, res@bg.pts, res@models[[best]], wc_sm)
th1 = threshold(ev.set) ## omission options

write.csv(th1,file="~/ENMeval_Zonotrichia-leucophrys_ThreshTable_paleoclim.csv")

p1.nomit = pred.raw>= th1$no_omission ## the highest thresh where no points are omittec
p1.equal = pred.raw>= th1$equal_sens_spec ## equal sensitivity and specificity according to ROC curve
p1.spsen = pred.raw>= th1$spec_sens ## maximum sensitivity and specificity according to ROC curve

writeRaster(p1.nomit,filename="~/ENMeval_Zonotrichia-leucophrys_NotOmit_paleoclim.asc",
            format="ascii",overwrite=T)
writeRaster(p1.equal,filename="~/ENMeval_Zonotrichia-leucophrys_EqualSensSpec_paleoclim.asc",
            format="ascii",overwrite=T)
writeRaster(p1.spsen,filename="~/ENMeval_Zonotrichia-leucophrys_MaxSensSpec_paleoclim.asc",
            format="ascii",overwrite=T)

p1.sum = sum(p1.nomit,p1.equal,p1.spsen,na.rm=T)
plot(p1.sum)


## predict to LGM
## WARNING: THIS LGM STUFF IS PROBABLY NOT COMPATIBLE WITH THE 2.0 WC 
#lgmfiles = list.files("/Users/kprovost/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Environment/paleoclim_mar2022/1.4/LGM/cclgmbi_10m/",
#                      full.names = T)
lgmfiles = list.files("/Users/kprovost/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Environment/paleoclim2.1_jan2020/Paleoclim_LGM_21ka_10arcmin_chelsa_LGM_v1_2B_r10m/10min/",
                      full.names = T,pattern=".tif$")
LGM=stack(lgmfiles)
#LGM = stack(lgmfiles[c(1,12:19,2:11)])
LGM_sm = crop(LGM,extent(min_lon-10,max_lon+10,min_lat-10,max_lat+10))
#names(LGM_sm) = paste("bio",1:19,sep="")
pred.LGM = predict(LGM_sm, res@models[[best]])
plot(pred.LGM)

writeRaster(pred.LGM,filename="~/ENMeval_Zonotrichia-leucophrys_ResultsTable_paleoclim_LGM.asc",
            format="ascii",overwrite=T)

LGM.nomit = pred.LGM>= th1$no_omission ## the highest thresh where no points are omittec
LGM.equal = pred.LGM>= th1$equal_sens_spec ## equal sensitivity and specificity according to ROC curve
LGM.spsen = pred.LGM>= th1$spec_sens ## maximum sensitivity and specificity according to ROC curve

writeRaster(LGM.nomit,filename="~/ENMeval_Zonotrichia-leucophrys_NotOmit_paleoclim_LGM.asc",
            format="ascii",overwrite=T)
writeRaster(LGM.equal,filename="~/ENMeval_Zonotrichia-leucophrys_EqualSensSpec_paleoclim_LGM.asc",
            format="ascii",overwrite=T)
writeRaster(LGM.spsen,filename="~/ENMeval_Zonotrichia-leucophrys_MaxSensSpec_paleoclim_LGM.asc",
            format="ascii",overwrite=T)
plot(LGM.equal)

