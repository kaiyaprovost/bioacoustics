## jy project 
outfilename="~/outfile_GET_27Feb2023.txt"
skip=T
if(skip==F) {
        #annots = read.table("/Users/kprovost/Documents/Postdoc_Working/JY_Project/ALL.annot.csv",sep=",",header=T)
        #centroids_sub = read.table("/Users/kprovost/Documents/Postdoc_Working/JY_Project/centroid_locations_per_individual_PCA_LabeledAsSubspecies.txt",header=T)
        centroids_pca = read.table("/Users/kprovost/Documents/Postdoc_Working/JY_Project/centroid_locations_per_individual_PCA_seasons.txt",header=T)
        #centroids = read.table("/Users/kprovost/Documents/Postdoc_Working/JY_Project/centroid_locations_per_individual.txt",header=T)
        #pca_table = read.table("/Users/kprovost/Documents/Postdoc_Working/JY_Project/combined_table_for_pca.txt",header=T)
        centroids_dist = read.table("/Users/kprovost/Documents/Postdoc_Working/JY_Project/mean_centroid_distances_individuals.txt",header=T)
        song_meta = read.table("/Users/kprovost/Documents/Postdoc_Working/JY_Project/song_metadata_edited (1).txt",sep="\t",header=T)
        yeardist = read.table("/Users/kprovost/Documents/Postdoc_Working/JY_Project/year_distances_individuals.txt")
        climdist = read.table("/Users/kprovost/Documents/Postdoc_Working/JY_Project/climate_distances_per_individual_allWC_31Jan2023.txt",header=T)
        ibddist = read.table("/Users/kprovost/Documents/Postdoc_Working/JY_Project/ibd_distances_per_individual_31Jan2023.txt",header=T)
        
        ## recalc the pca from the raw syllables? 
        df=read.table("/Users/kprovost/Documents/Postdoc_Working/JY_Project/centroid_locations_per_individual.txt",header=T)
        pca = prcomp(df[,1:5],scale.=T,center=T)
        data = pca$x
        pcadata = cbind(df,data)
        rotation = pca$rotation
        importance = summary(pca)$importance
        
        df_pca = merge(pcadata,centroids_pca,by="Ind")
        corrplot::corrplot(cor(df_pca[,c(2:11,12:21)]),method="number")
        
        
        song_meta = song_meta[song_meta$GENUS=="Zonotrichia",]
        song_meta = song_meta[song_meta$SPECIES=="leucophrys",]
        song_meta$SUBSPECIES[song_meta$SUBSPECIES=="gambellii"] = "gambelii"
        song_meta$SUBSPECIES[song_meta$SUBSPECIES==""] = "unknown"
        song_meta$SEASON = song_meta$MONTH
        song_meta$SEASON[song_meta$MONTH %in% c(1,2,3,4,9,10,11,12)] = "WINTER"
        song_meta$SEASON[song_meta$MONTH %in% c(5,6,7,8)] = "SUMMER"
        song_meta$SEASON[is.na(song_meta$MONTH)] = "UNKNOWN"
        
        ##separate song meta by subspecies
        song_meta_unk = song_meta[song_meta$SUBSPECIES=="unknown",]
        song_meta_leu = song_meta[song_meta$SUBSPECIES=="leucophrys",]
        song_meta_nut = song_meta[song_meta$SUBSPECIES=="nuttalli",]
        song_meta_ori = song_meta[song_meta$SUBSPECIES=="oriantha",]
        song_meta_pug = song_meta[song_meta$SUBSPECIES=="pugetensis",]
        song_meta_gam = song_meta[song_meta$SUBSPECIES=="gambelii",]
        
        par(mfrow=c(2,3))
        plot(song_meta_unk$LONGITUDE,song_meta_unk$LATITUDE,xlab="LONG",ylab="LAT",ylim=c(25,70),
             xlim=c(-170,-65),col=c("blue","grey","red")[as.numeric(as.factor(song_meta_unk$SEASON))],
             pch=c(16,0,2)[as.numeric(as.factor(song_meta_unk$SEASON))],main="Z. l. unknown")
        plot(song_meta_leu$LONGITUDE,song_meta_leu$LATITUDE,xlab="LONG",ylab="LAT",ylim=c(25,70),
             xlim=c(-170,-65),col=c("blue","grey","red")[as.numeric(as.factor(song_meta_leu$SEASON))],
             pch=c(16,0,2)[as.numeric(as.factor(song_meta_leu$SEASON))],main="Z. l. leucophrys")
        plot(song_meta_nut$LONGITUDE,song_meta_nut$LATITUDE,xlab="LONG",ylab="LAT",ylim=c(25,70),
             xlim=c(-170,-65),col=c("blue","grey","red")[as.numeric(as.factor(song_meta_nut$SEASON))],
             pch=c(16,0,2)[as.numeric(as.factor(song_meta_nut$SEASON))],main="Z. l. nuttali")
        plot(song_meta_ori$LONGITUDE,song_meta_ori$LATITUDE,xlab="LONG",ylab="LAT",ylim=c(25,70),
             xlim=c(-170,-65),col=c("blue","grey","red")[as.numeric(as.factor(song_meta_ori$SEASON))],
             pch=c(16,0,2)[as.numeric(as.factor(song_meta_ori$SEASON))],main="Z. l. oriantha")
        plot(song_meta_pug$LONGITUDE,song_meta_pug$LATITUDE,xlab="LONG",ylab="LAT",ylim=c(25,70),
             xlim=c(-170,-65),col=c("blue","grey","red")[as.numeric(as.factor(song_meta_pug$SEASON))],
             pch=c(16,0,2)[as.numeric(as.factor(song_meta_pug$SEASON))],main="Z. l. pugetensis")
        plot(song_meta_gam$LONGITUDE,song_meta_gam$LATITUDE,xlab="LONG",ylab="LAT",ylim=c(25,70),
             xlim=c(-170,-65),col=c("blue","grey","red")[as.numeric(as.factor(song_meta_gam$SEASON))],
             pch=c(16,0,2)[as.numeric(as.factor(song_meta_gam$SEASON))],main="Z. l. gambelii")
        
        ## just pug and nut
        par(mfrow=c(1,2))
        plot(song_meta_pug$LONGITUDE,song_meta_pug$LATITUDE,xlab="LONG",ylab="LAT",ylim=c(30,50),
             xlim=c(-126,-111),col=c("red","pink","darkred")[as.numeric(as.factor(song_meta_pug$SEASON))],
             pch=c(17,17,17)[as.numeric(as.factor(song_meta_pug$SEASON))],main="Z. l. pugetensis")
        plot(song_meta_nut$LONGITUDE,song_meta_nut$LATITUDE,xlab="LONG",ylab="LAT",ylim=c(30,50),
             xlim=c(-126,-111),col=c("blue","lightblue","darkblue")[as.numeric(as.factor(song_meta_nut$SEASON))],
             pch=c(16,16,16)[as.numeric(as.factor(song_meta_nut$SEASON))],main="Z. l. nuttali")
        ## 30:50, -126:-118
        ## 33:41, -124:-111
        
        ## nope need to separate out by season
        
        
        song_meta_pug_hull = song_meta_pug[,c("SEASON","LONGITUDE","LATITUDE")]
        song_meta_pug_hull = unique(song_meta_pug_hull)
        song_meta_pug_hull = song_meta_pug_hull[complete.cases(song_meta_pug_hull),]
        song_meta_pug_hull_win = song_meta_pug_hull[song_meta_pug_hull$SEASON=="WINTER",]
        song_meta_pug_hull_sum = song_meta_pug_hull[song_meta_pug_hull$SEASON=="SUMMER",]
        
        song_meta_nut_hull = song_meta_nut[,c("SEASON","LONGITUDE","LATITUDE")]
        song_meta_nut_hull = unique(song_meta_nut_hull)
        song_meta_nut_hull = song_meta_nut_hull[complete.cases(song_meta_nut_hull),]
        song_meta_nut_hull_win = song_meta_nut_hull[song_meta_nut_hull$SEASON=="WINTER",]
        song_meta_nut_hull_sum = song_meta_nut_hull[song_meta_nut_hull$SEASON=="SUMMER",]
        
        hull_pug_win = chull(x=song_meta_pug_hull_win[,c("LONGITUDE","LATITUDE")])
        hull_nut_win = chull(x=song_meta_nut_hull_win[,c("LONGITUDE","LATITUDE")])
        hull_pug_win = c(hull_pug_win,hull_pug_win[1])
        hull_nut_win = c(hull_nut_win,hull_nut_win[1])
        
        hull_pug_sum = chull(x=song_meta_pug_hull_sum[,c("LONGITUDE","LATITUDE")])
        hull_nut_sum = chull(x=song_meta_nut_hull_sum[,c("LONGITUDE","LATITUDE")])
        hull_pug_sum = c(hull_pug_sum,hull_pug_sum[1])
        hull_nut_sum = c(hull_nut_sum,hull_nut_sum[1])
        
        library(raster)
        shp=raster::shapefile("/Users/kprovost/Downloads/cb_2016_us_state_500k/cb_2016_us_state_500k.shp")
        
        ## winter only
        par(mfrow=c(1,2))
        plot(shp,ylim=c(30,50),xlim=c(-126,-111))
        polygon(song_meta_nut_hull_win[hull_nut_win,c("LONGITUDE","LATITUDE")],col=rgb(0,0,1,0.3))
        polygon(song_meta_pug_hull_win[hull_pug_win,c("LONGITUDE","LATITUDE")],col=rgb(1,0,0,0.3))
        points(song_meta_pug$LONGITUDE[song_meta_pug$SEASON=="WINTER"],song_meta_pug$LATITUDE[song_meta_pug$SEASON=="WINTER"],
               ylim=c(30,50),xlim=c(-126,-111),col="darkred",pch=2,xlab="LONG",ylab="LAT")
        points(song_meta_nut$LONGITUDE[song_meta_nut$SEASON=="WINTER"],song_meta_nut$LATITUDE[song_meta_nut$SEASON=="WINTER"],
               ylim=c(30,50),xlim=c(-126,-111),col="darkblue",pch=1,xlab="LONG",ylab="LAT",)
        
        ## summer only
        plot(shp,ylim=c(30,50),xlim=c(-126,-111))
        polygon(song_meta_nut_hull_sum[hull_nut_sum,c("LONGITUDE","LATITUDE")],col=rgb(0,0,1,0.3))
        polygon(song_meta_pug_hull_sum[hull_pug_sum,c("LONGITUDE","LATITUDE")],col=rgb(1,0,0,0.3))
        points(song_meta_pug$LONGITUDE[song_meta_pug$SEASON=="SUMMER"],song_meta_pug$LATITUDE[song_meta_pug$SEASON=="SUMMER"],
               ylim=c(30,50),xlim=c(-126,-111),col="darkred",pch=2,xlab="LONG",ylab="LAT")
        points(song_meta_nut$LONGITUDE[song_meta_nut$SEASON=="SUMMER"],song_meta_nut$LATITUDE[song_meta_nut$SEASON=="SUMMER"],
               ylim=c(30,50),xlim=c(-126,-111),col="darkblue",pch=1,xlab="LONG",ylab="LAT",)
        
        
        ## pca and centroids and such
        ## subset centroids by season
        centroids_pca$Ind
        centroids_pca$SEASON = "UNKNOWN"
        centroids_pca$YEAR = NA
        centroids_pca$LONGITUDE = NA
        centroids_pca$LATITUDE = NA
        song_meta$COLLID = paste(song_meta$COLLECTION,".",song_meta$ID,sep="")
        for(i in 1:nrow(song_meta)){
                collid_i = song_meta$COLLID[i]
                season_i = song_meta$SEASON[i]
                year_i = song_meta$YEAR[i]
                long_i = song_meta$LONGITUDE[i]
                lat_i = song_meta$LATITUDE[i]
                matches = (grepl(collid_i,centroids_pca$Ind))
                numpresent = sum(matches)
                if(numpresent >= 1){
                        centroids_pca$SEASON[matches] = season_i
                        centroids_pca$YEAR[matches] = year_i
                        centroids_pca$LONGITUDE[matches] = long_i
                        centroids_pca$LATITUDE[matches] = lat_i
                }
        }
        centroids_pca$SUBSPECIES = "unknown"
        centroids_pca$SUBSPECIES[grepl("nuttalli",centroids_pca$Ind)] = "nuttalli"
        centroids_pca$SUBSPECIES[grepl("pugetensis",centroids_pca$Ind)] = "pugetensis"
        write.table(centroids_pca,"/Users/kprovost/Documents/Postdoc_Working/JY_Project/centroid_locations_per_individual_PCA_seasons.txt")
        
        model1=aov(centroids_pca$PC1~centroids_pca$SEASON+centroids_pca$SUBSPECIES)
        model2=aov(centroids_pca$PC2~centroids_pca$SEASON+centroids_pca$SUBSPECIES)
        model3=aov(centroids_pca$PC3~centroids_pca$SEASON+centroids_pca$SUBSPECIES)
        model4=aov(centroids_pca$PC4~centroids_pca$SEASON+centroids_pca$SUBSPECIES)
        model5=aov(centroids_pca$PC5~centroids_pca$SEASON+centroids_pca$SUBSPECIES)
        
        modelA=aov(centroids_pca$Bandwidth~centroids_pca$SEASON+centroids_pca$SUBSPECIES)
        modelB=aov(centroids_pca$Time~centroids_pca$SEASON+centroids_pca$SUBSPECIES)
        modelC=aov(centroids_pca$Center~centroids_pca$SEASON+centroids_pca$SUBSPECIES)
        modelD=aov(centroids_pca$Inflection~centroids_pca$SEASON+centroids_pca$SUBSPECIES)
        modelE=aov(centroids_pca$Slope~centroids_pca$SEASON+centroids_pca$SUBSPECIES)
        
        summary(model1) ## season and subspecies
        summary(model2) ## subspecies
        summary(model3) ## neither
        summary(model4) ## season
        summary(model5) ## neither
        
        summary(modelA) ## neither
        summary(modelB) ## season
        summary(modelC) ## season and subspecies
        summary(modelD) ## season and subspecies
        summary(modelE) ## subspecies
        
        TukeyHSD(model1) ## win-sum, pug-nut
        TukeyHSD(model2) ## pug-nut
        TukeyHSD(model4) ## (unk-sum)
        TukeyHSD(modelB) ## win-sum
        TukeyHSD(modelC) ## pug-nut, (win-unk)
        TukeyHSD(modelD) ## win-sum, pug-nut
        TukeyHSD(modelE) ## pug-nut
        
        ## remove unknown season
        
        centroids_pca_small = centroids_pca[centroids_pca$SEASON!="UNKNOWN",]
        
        model1=aov(centroids_pca_small$PC1~centroids_pca_small$SEASON+centroids_pca_small$SUBSPECIES)
        model2=aov(centroids_pca_small$PC2~centroids_pca_small$SEASON+centroids_pca_small$SUBSPECIES)
        model3=aov(centroids_pca_small$PC3~centroids_pca_small$SEASON+centroids_pca_small$SUBSPECIES)
        model4=aov(centroids_pca_small$PC4~centroids_pca_small$SEASON+centroids_pca_small$SUBSPECIES)
        model5=aov(centroids_pca_small$PC5~centroids_pca_small$SEASON+centroids_pca_small$SUBSPECIES)
        
        modelA=aov(centroids_pca_small$Bandwidth~centroids_pca_small$SEASON+centroids_pca_small$SUBSPECIES)
        modelB=aov(centroids_pca_small$Time~centroids_pca_small$SEASON+centroids_pca_small$SUBSPECIES)
        modelC=aov(centroids_pca_small$Center~centroids_pca_small$SEASON+centroids_pca_small$SUBSPECIES)
        modelD=aov(centroids_pca_small$Inflection~centroids_pca_small$SEASON+centroids_pca_small$SUBSPECIES)
        modelE=aov(centroids_pca_small$Slope~centroids_pca_small$SEASON+centroids_pca_small$SUBSPECIES)
        
        summary(model1) ## season and subspecies
        summary(model2) ## subspecies
        summary(model3) ## neither
        summary(model4) ## season
        summary(model5) ## neither
        
        summary(modelA) ## neither
        summary(modelB) ## season
        summary(modelC) ## subspecies
        summary(modelD) ## season and subspecies
        summary(modelE) ## subspecies
        
        TukeyHSD(model1) ## win-sum, pug-nut
        TukeyHSD(model2) ## pug-nut
        TukeyHSD(model4) ## win-sum
        TukeyHSD(modelB) ## win-sum
        TukeyHSD(modelC) ## pug-nut 
        TukeyHSD(modelD) ## win-sum, pug-nut
        TukeyHSD(modelE) ## pug-nut
        
        x=as.data.frame(table(song_meta_pug[,c("SEASON","YEAR")]))
        x$YEAR=as.numeric(as.character(x$YEAR))
        y=as.data.frame(table(song_meta_nut[,c("SEASON","YEAR")]))
        y$YEAR=as.numeric(as.character(y$YEAR))
        
        par(mfrow=c(2,1))
        plot(as.numeric(x$YEAR[x$SEASON=="SUMMER"]),x$Freq[x$SEASON=="SUMMER"],type="l",col="red")
        points(as.numeric(x$YEAR[x$SEASON=="WINTER"]),x$Freq[x$SEASON=="WINTER"],type="l",col="blue")
        plot(as.numeric(y$YEAR[y$SEASON=="SUMMER"]),y$Freq[y$SEASON=="SUMMER"],type="l",col="red")
        points(as.numeric(y$YEAR[y$SEASON=="WINTER"]),y$Freq[y$SEASON=="WINTER"],type="l",col="blue")
        
        ## we might need to control for year
        ## generate euclidean distances for year
        year = centroids_pca[,c("Ind","YEAR")]
        rownames(year) = year$Ind
        mydist=(dist(year$YEAR))
        mydist = as.data.frame(as.matrix(mydist))
        colnames(mydist) = rownames(year)
        rownames(mydist) = rownames(year)
        write.table(mydist,"/Users/kprovost/Documents/Postdoc_Working/JY_Project/year_distances_individuals.txt")
        
        mydist=mydist[order(rownames(mydist)),order(colnames(mydist))]
        centroids_dist=mydist[order(rownames(centroids_dist)),order(colnames(centroids_dist))]
        #ibddist=mydist[order(rownames(ibddist)),order(colnames(ibddist))]
        #climdist=mydist[order(rownames(climdist)),order(colnames(climdist))]
        
        ## genertate some distances for the songs for different partitions as well
        rownames(centroids_pca) = centroids_pca$Ind
        dist_raw_all = dist(centroids_pca[,c("Bandwidth","Time","Center","Inflection","Slope")]) ## USE THIS ONE
        dist_raw_win = dist(centroids_pca[centroids_pca$SEASON=="WINTER",c("Bandwidth","Time","Center","Inflection","Slope")]) ## USE THIS ONE
        dist_raw_sum = dist(centroids_pca[centroids_pca$SEASON=="SUMMER",c("Bandwidth","Time","Center","Inflection","Slope")]) ## USE THIS ONE
        dist_raw_win_noout = dist(centroids_pca[centroids_pca$SEASON=="WINTER" & centroids_pca$OUTLIER==0,c("Bandwidth","Time","Center","Inflection","Slope")]) ## USE THIS ONE
        dist_raw_sum_noout = dist(centroids_pca[centroids_pca$SEASON=="SUMMER" & centroids_pca$OUTLIER==0,c("Bandwidth","Time","Center","Inflection","Slope")]) ## USE THIS ONE
        dist_raw_all_noout = dist(centroids_pca[centroids_pca$OUTLIER==0,c("Bandwidth","Time","Center","Inflection","Slope")]) ## USE THIS ONE
        dist_nut_all = dist(centroids_pca[centroids_pca$SUBSPECIES=="nuttalli",c("Bandwidth","Time","Center","Inflection","Slope")]) ## USE THIS ONE
        dist_pug_all = dist(centroids_pca[centroids_pca$SUBSPECIES=="pugetensis",c("Bandwidth","Time","Center","Inflection","Slope")]) ## USE THIS ONE
        dist_nut_win = dist(centroids_pca[centroids_pca$SUBSPECIES=="nuttalli" & centroids_pca$SEASON=="WINTER",c("Bandwidth","Time","Center","Inflection","Slope")]) ## USE THIS ONE
        dist_pug_win = dist(centroids_pca[centroids_pca$SUBSPECIES=="pugetensis" & centroids_pca$SEASON=="WINTER",c("Bandwidth","Time","Center","Inflection","Slope")]) ## USE THIS ONE
        dist_nut_sum = dist(centroids_pca[centroids_pca$SUBSPECIES=="nuttalli" & centroids_pca$SEASON=="SUMMER",c("Bandwidth","Time","Center","Inflection","Slope")]) ## USE THIS ONE
        dist_pug_sum = dist(centroids_pca[centroids_pca$SUBSPECIES=="pugetensis" & centroids_pca$SEASON=="SUMMER",c("Bandwidth","Time","Center","Inflection","Slope")]) ## USE THIS ONE
        dist_nut_all_noout = dist(centroids_pca[centroids_pca$SUBSPECIES=="nuttalli" & centroids_pca$OUTLIER==0,c("Bandwidth","Time","Center","Inflection","Slope")]) ## USE THIS ONE
        dist_pug_all_noout = dist(centroids_pca[centroids_pca$SUBSPECIES=="pugetensis" & centroids_pca$OUTLIER==0,c("Bandwidth","Time","Center","Inflection","Slope")]) ## USE THIS ONE
        dist_nut_win_noout = dist(centroids_pca[centroids_pca$SUBSPECIES=="nuttalli" & centroids_pca$OUTLIER==0 & centroids_pca$SEASON=="WINTER",c("Bandwidth","Time","Center","Inflection","Slope")]) ## USE THIS ONE
        dist_pug_win_noout = dist(centroids_pca[centroids_pca$SUBSPECIES=="pugetensis" & centroids_pca$OUTLIER==0 & centroids_pca$SEASON=="WINTER",c("Bandwidth","Time","Center","Inflection","Slope")]) ## USE THIS ONE
        dist_nut_sum_noout = dist(centroids_pca[centroids_pca$SUBSPECIES=="nuttalli" & centroids_pca$OUTLIER==0 & centroids_pca$SEASON=="SUMMER",c("Bandwidth","Time","Center","Inflection","Slope")]) ## USE THIS ONE
        dist_pug_sum_noout = dist(centroids_pca[centroids_pca$SUBSPECIES=="pugetensis" & centroids_pca$OUTLIER==0 & centroids_pca$SEASON=="SUMMER",c("Bandwidth","Time","Center","Inflection","Slope")]) ## USE THIS ONE
        
        write.table(as.matrix(dist_raw_all),"/Users/kprovost/Documents/Postdoc_Working/JY_Project/mean_centroid_distances_individuals_RAW_BOTH_YEAR.txt")
        write.table(as.matrix(dist_raw_win),"/Users/kprovost/Documents/Postdoc_Working/JY_Project/mean_centroid_distances_individuals_RAW_BOTH_WINTER.txt")
        write.table(as.matrix(dist_raw_sum),"/Users/kprovost/Documents/Postdoc_Working/JY_Project/mean_centroid_distances_individuals_RAW_BOTH_SUMMER.txt")
        write.table(as.matrix(dist_raw_all_noout),"/Users/kprovost/Documents/Postdoc_Working/JY_Project/mean_centroid_distances_individuals_RAW_BOTH_YEAR_NOOUTLIERS.txt")
        write.table(as.matrix(dist_raw_win_noout),"/Users/kprovost/Documents/Postdoc_Working/JY_Project/mean_centroid_distances_individuals_RAW_BOTH_WINTER_NOOUTLIERS.txt")
        write.table(as.matrix(dist_raw_sum_noout),"/Users/kprovost/Documents/Postdoc_Working/JY_Project/mean_centroid_distances_individuals_RAW_BOTH_SUMMER_NOOUTLIERS.txt")
        
        write.table(as.matrix(dist_nut_all),"/Users/kprovost/Documents/Postdoc_Working/JY_Project/mean_centroid_distances_individuals_RAW_NUTTALLI_YEAR.txt")
        write.table(as.matrix(dist_nut_win),"/Users/kprovost/Documents/Postdoc_Working/JY_Project/mean_centroid_distances_individuals_RAW_NUTTALLI_WINTER.txt")
        write.table(as.matrix(dist_nut_sum),"/Users/kprovost/Documents/Postdoc_Working/JY_Project/mean_centroid_distances_individuals_RAW_NUTTALLI_SUMMER.txt")
        write.table(as.matrix(dist_nut_all_noout),"/Users/kprovost/Documents/Postdoc_Working/JY_Project/mean_centroid_distances_individuals_RAW_NUTTALLI_YEAR_NOOUTLIERS.txt")
        write.table(as.matrix(dist_nut_win_noout),"/Users/kprovost/Documents/Postdoc_Working/JY_Project/mean_centroid_distances_individuals_RAW_NUTTALLI_WINTER_NOOUTLIERS.txt")
        write.table(as.matrix(dist_nut_sum_noout),"/Users/kprovost/Documents/Postdoc_Working/JY_Project/mean_centroid_distances_individuals_RAW_NUTTALLI_SUMMER_NOOUTLIERS.txt")
        
        write.table(as.matrix(dist_pug_all),"/Users/kprovost/Documents/Postdoc_Working/JY_Project/mean_centroid_distances_individuals_RAW_PUGETENSIS_YEAR.txt")
        write.table(as.matrix(dist_pug_win),"/Users/kprovost/Documents/Postdoc_Working/JY_Project/mean_centroid_distances_individuals_RAW_PUGETENSIS_WINTER.txt")
        write.table(as.matrix(dist_pug_sum),"/Users/kprovost/Documents/Postdoc_Working/JY_Project/mean_centroid_distances_individuals_RAW_PUGETENSIS_SUMMER.txt")
        write.table(as.matrix(dist_pug_all_noout),"/Users/kprovost/Documents/Postdoc_Working/JY_Project/mean_centroid_distances_individuals_RAW_PUGETENSIS_YEAR_NOOUTLIERS.txt")
        write.table(as.matrix(dist_pug_win_noout),"/Users/kprovost/Documents/Postdoc_Working/JY_Project/mean_centroid_distances_individuals_RAW_PUGETENSIS_WINTER_NOOUTLIERS.txt")
        write.table(as.matrix(dist_pug_sum_noout),"/Users/kprovost/Documents/Postdoc_Working/JY_Project/mean_centroid_distances_individuals_RAW_PUGETENSIS_SUMMER_NOOUTLIERS.txt")
        
        
        dist_pcs_all = dist(centroids_pca[,c("PC1","PC2","PC3","PC4","PC5")])
        dist_pcs_win = dist(centroids_pca[centroids_pca$SEASON=="WINTER",c("PC1","PC2","PC3","PC4","PC5")])
        dist_pcs_sum = dist(centroids_pca[centroids_pca$SEASON=="SUMMER",c("PC1","PC2","PC3","PC4","PC5")])
        
        dist_rawpcs_all = dist(centroids_pca[,c("Bandwidth","Time","Center","Inflection","Slope","PC1","PC2","PC3","PC4","PC5")])
        dist_rawpcs_win = dist(centroids_pca[centroids_pca$SEASON=="WINTER",c("Bandwidth","Time","Center","Inflection","Slope","PC1","PC2","PC3","PC4","PC5")])
        dist_rawpcs_sum = dist(centroids_pca[centroids_pca$SEASON=="SUMMER",c("Bandwidth","Time","Center","Inflection","Slope","PC1","PC2","PC3","PC4","PC5")])
        
        write.table(as.matrix(dist_pcs_all),"/Users/kprovost/Documents/Postdoc_Working/JY_Project/mean_centroid_distances_individuals_PCA_BOTH_YEAR.txt")
        write.table(as.matrix(dist_pcs_win),"/Users/kprovost/Documents/Postdoc_Working/JY_Project/mean_centroid_distances_individuals_PCA_BOTH_WINTER.txt")
        write.table(as.matrix(dist_pcs_sum),"/Users/kprovost/Documents/Postdoc_Working/JY_Project/mean_centroid_distances_individuals_PCA_BOTH_SUMMER.txt")
        write.table(as.matrix(dist_rawpcs_all),"/Users/kprovost/Documents/Postdoc_Working/JY_Project/mean_centroid_distances_individuals_RAWPCA_BOTH_YEAR.txt")
        write.table(as.matrix(dist_rawpcs_win),"/Users/kprovost/Documents/Postdoc_Working/JY_Project/mean_centroid_distances_individuals_RAWPCA_BOTH_WINTER.txt")
        write.table(as.matrix(dist_rawpcs_sum),"/Users/kprovost/Documents/Postdoc_Working/JY_Project/mean_centroid_distances_individuals_RAWPCA_BOTH_SUMMER.txt")
        
        
        
        
        
        # ## need to re-generate ibd and clim
        # IBD=as.matrix(dist(centroids_pca[,c("LONGITUDE","LATITUDE")],diag=T,upper=T))
        # rownames(IBD) = centroids_pca$Ind
        # colnames(IBD) = centroids_pca$Ind
        # ibddist = IBD[order(rownames(IBD)),order(colnames(IBD))]
        # write.table(ibddist,"/Users/kprovost/Documents/Postdoc_Working/JY_Project/ibd_distances_per_individual_31Jan2023.txt")
        # 
        # r <- getData("worldclim",var="bio",res=10)
        # points = extract(r,centroids_pca[,c("LONGITUDE","LATITUDE")])
        # rownames(points) = centroids_pca$Ind
        # climdist = as.matrix(dist(points,diag=T,upper = T))
        # climdist = climdist[order(rownames(climdist)),order(colnames(climdist))]
        # write.table(climdist,"/Users/kprovost/Documents/Postdoc_Working/JY_Project/climate_distances_per_individual_allWC_31Jan2023.txt")
        
        ## run some mmrrs
}

# MMRR performs Multiple Matrix Regression with Randomization analysis
# Y is a dependent distance matrix
# X is a list of independent distance matrices (with optional names)
MMRR<-function(Y,X,nperm=999,center=T,scale=T){
        #compute regression coefficients and test statistics
        nrowsY<-nrow(Y)
        y<-unfold(Y,center=center,scale=scale)
        if(is.null(names(X)))names(X)<-paste("X",1:length(X),sep="")
        Xmats<-sapply(X,FUN=function(x){unfold(x,center=center,scale=scale)})
        fit<-lm(y~Xmats)
        coeffs<-fit$coefficients
        summ<-summary(fit)
        r.squared<-summ$r.squared
        confint = confint(fit)
        
        tstat<-summ$coefficients[,"t value"]
        Fstat<-summ$fstatistic[1]
        tprob<-rep(1,length(tstat))
        Fprob<-1
        
        #perform permutations
        for(i in 1:nperm){
                rand<-sample(1:nrowsY)
                Yperm<-Y[rand,rand]
                yperm<-unfold(Yperm)
                fit<-lm(yperm~Xmats)
                summ<-summary(fit)
                Fprob<-Fprob+as.numeric(summ$fstatistic[1]>=Fstat)
                tprob<-tprob+as.numeric(abs(summ$coefficients[,"t value"])>=abs(tstat))
        }
        
        #return values
        tp<-tprob/(nperm+1)
        Fp<-Fprob/(nperm+1)
        names(r.squared)<-"r.squared"
        names(coeffs)<-c("Intercept",names(X))
        names(tstat)<-paste(c("Intercept",names(X)),"(t)",sep="")
        names(tp)<-paste(c("Intercept",names(X)),"(p)",sep="")
        names(Fstat)<-"F-statistic"
        names(Fp)<-"F p-value"
        #names(confint)<-"Confidence interval"
        
        return(list(r.squared=r.squared,
                    coefficients=coeffs,
                    tstatistic=tstat,
                    tpvalue=tp,
                    Fstatistic=Fstat,
                    Fpvalue=Fp,
                    Conf=confint))
}
# unfold converts the lower diagonal elements of a matrix into a vector
# unfold is called by MMRR
unfold<-function(X,center=T,scale=T){
        x<-vector()
        for(i in 2:nrow(X)) x<-c(x,X[i,1:i-1])
        x<-scale(x, center=center, scale=scale)  # Comment this line out if you wish to perform the analysis without standardizing the distance matrices! 
        return(x)
}
outputMMRR = function(genMat,Xmats,nummmrr,thislength,name_gene,scale=T,center=T){
        samples = nrow(genMat)
        mmrr=NULL
        try({mmrr=MMRR(Y=as.matrix(genMat),X=Xmats,nperm=nummmrr,scale=scale,center=center)})
        if(is.null(mmrr)){
                mmrr_row=cbind(name_gene,thislength,samples,nummmrr,NA)
        } else {
                mmrr_row=cbind(name_gene,thislength,samples,nummmrr,
                               mmrr$r.squared,
                               ifelse(nrow(mmrr$Conf)>=2,mmrr$coefficients[2],NA),
                               ifelse(nrow(mmrr$Conf)>=3,mmrr$coefficients[3],NA),
                               ifelse(nrow(mmrr$Conf)>=4,mmrr$coefficients[4],NA),
                               ifelse(nrow(mmrr$Conf)>=2,mmrr$tpvalue[2],NA),
                               ifelse(nrow(mmrr$Conf)>=3,mmrr$tpvalue[3],NA),
                               ifelse(nrow(mmrr$Conf)>=4,mmrr$tpvalue[4],NA),
                               mmrr$Fpvalue,
                               mmrr$Fstatistic,
                               ifelse(nrow(mmrr$Conf)>=2,mmrr$tstatistic[2],NA),
                               ifelse(nrow(mmrr$Conf)>=3,mmrr$tstatistic[3],NA),
                               ifelse(nrow(mmrr$Conf)>=4,mmrr$tstatistic[4],NA),
                               ifelse(nrow(mmrr$Conf)>=2,mmrr$Conf[2,1],NA),
                               ifelse(nrow(mmrr$Conf)>=3,mmrr$Conf[3,1],NA),
                               ifelse(nrow(mmrr$Conf)>=4,mmrr$Conf[4,1],NA),
                               ifelse(nrow(mmrr$Conf)>=2,mmrr$Conf[2,2],NA),
                               ifelse(nrow(mmrr$Conf)>=3,mmrr$Conf[3,2],NA),
                               ifelse(nrow(mmrr$Conf)>=4,mmrr$Conf[4,2],NA)
                )
        }
        colnames(mmrr_row) = c("spp-gene","N","samples","nummmrr","rsq",
                               ifelse(nrow(mmrr$Conf)>=2,paste(names(Xmats)[1],"_coef",sep=""),NA),
                               ifelse(nrow(mmrr$Conf)>=3,paste(names(Xmats)[2],"_coef",sep=""),NA),
                               ifelse(nrow(mmrr$Conf)>=4,paste(names(Xmats)[3],"_coef",sep=""),NA),
                               ifelse(nrow(mmrr$Conf)>=2,paste(names(Xmats)[1],"_p",sep=""),NA),
                               ifelse(nrow(mmrr$Conf)>=3,paste(names(Xmats)[2],"_p",sep=""),NA),
                               ifelse(nrow(mmrr$Conf)>=4,paste(names(Xmats)[3],"_p",sep=""),NA),
                               "overall_p","F",
                               ifelse(nrow(mmrr$Conf)>=2,paste(names(Xmats)[1],"_t",sep=""),NA),
                               ifelse(nrow(mmrr$Conf)>=3,paste(names(Xmats)[2],"_t",sep=""),NA),
                               ifelse(nrow(mmrr$Conf)>=4,paste(names(Xmats)[3],"_t",sep=""),NA),
                               ifelse(nrow(mmrr$Conf)>=2,paste(names(Xmats)[1],"_2.5",sep=""),NA),
                               ifelse(nrow(mmrr$Conf)>=3,paste(names(Xmats)[2],"_2.5",sep=""),NA),
                               ifelse(nrow(mmrr$Conf)>=4,paste(names(Xmats)[3],"_2.5",sep=""),NA),
                               ifelse(nrow(mmrr$Conf)>=2,paste(names(Xmats)[1],"_97.5",sep=""),NA),
                               ifelse(nrow(mmrr$Conf)>=3,paste(names(Xmats)[2],"_97.5",sep=""),NA),
                               ifelse(nrow(mmrr$Conf)>=4,paste(names(Xmats)[3],"_97.5",sep=""),NA)
        )
        return(mmrr_row)
}

## 

generateMMRR = function(songdist,ibddist,climdist,yeardist,name_gene,nummmrr=10,outfilename="~/outfile.txt"){
        print("matching")
        genMat = as.matrix(songdist)
        geoMat = as.matrix(ibddist)
        ecoMat = as.matrix(climdist)
        timMat = as.matrix(yeardist)
        
        genMat = genMat[order(rownames(genMat)),order(colnames(genMat))]
        geoMat = geoMat[order(rownames(geoMat)),order(colnames(geoMat))]
        ecoMat = ecoMat[order(rownames(ecoMat)),order(colnames(ecoMat))]
        timMat = timMat[order(rownames(timMat)),order(colnames(timMat))]
        
        matches = intersect(rownames(genMat),rownames(geoMat))
        matches = intersect(rownames(ecoMat),matches)
        matches = intersect(rownames(timMat),matches)
        
        genMat = genMat[which(matches %in% rownames(genMat)),which(matches %in% colnames(genMat))]
        geoMat = geoMat[which(matches %in% rownames(geoMat)),which(matches %in% colnames(geoMat))]
        ecoMat = ecoMat[which(matches %in% rownames(ecoMat)),which(matches %in% colnames(ecoMat))]
        timMat = timMat[which(matches %in% rownames(timMat)),which(matches %in% colnames(timMat))]
        
        print("listing")
        Xmats_G = list(geography=as.matrix(geoMat))
        Xmats_E = list(ecology=as.matrix(ecoMat))
        Xmats_T = list(time=as.matrix(timMat))
        Xmats_GE = list(geography=as.matrix(geoMat),ecology=as.matrix(ecoMat))
        Xmats_GT = list(geography=as.matrix(geoMat),time=as.matrix(timMat))
        Xmats_ET = list(ecology=as.matrix(ecoMat),time=as.matrix(timMat))
        Xmats_GET = list(geography=as.matrix(geoMat),ecology=as.matrix(ecoMat),time=as.matrix(timMat))
        
        output_G=NULL
        output_E=NULL
        output_T=NULL
        output_GE=NULL
        output_GT=NULL
        output_ET=NULL
        output_GET=NULL
        
        #print("G")
        #output_G=outputMMRR(genMat=genMat,Xmats=Xmats_G,nummmrr,thislength=length(Xmats_G),name_gene=name_gene,scale=T,center=T)
        ##write.table(output_G,outfilename,append=T,sep="\t",quote=F,row.names = F,col.names = T)
        #print("E")
        #output_E=outputMMRR(genMat=genMat,Xmats=Xmats_E,nummmrr,thislength=length(Xmats_E),name_gene=name_gene,scale=T,center=T)
        #write.table(output_E,outfilename,append=T,sep="\t",quote=F,row.names = F,col.names = T)
        #print("T")
        # output_T=outputMMRR(genMat=genMat,Xmats=Xmats_T,nummmrr,thislength=length(Xmats_T),name_gene=name_gene,scale=T,center=T)
        # write.table(output_T,outfilename,append=T,sep="\t",quote=F,row.names = F,col.names = T)
        #print("GE")
        #output_GE=outputMMRR(genMat=genMat,Xmats=Xmats_GE,nummmrr,thislength=length(Xmats_GE),name_gene=name_gene,scale=T,center=T)
        #write.table(output_GE,outfilename,append=T,sep="\t",quote=F,row.names = F,col.names = T)
        # print("GT")
        # output_GT=outputMMRR(genMat=genMat,Xmats=Xmats_GT,nummmrr,thislength=length(Xmats_GT),name_gene=name_gene,scale=T,center=T)
        #  write.table(output_GT,outfilename,append=T,sep="\t",quote=F,row.names = F,col.names = T)
        #  print("ET")
        #  output_ET=outputMMRR(genMat=genMat,Xmats=Xmats_ET,nummmrr,thislength=length(Xmats_ET),name_gene=name_gene,scale=T,center=T)  
        #  write.table(output_ET,outfilename,append=T,sep="\t",quote=F,row.names = F,col.names = T)
        print("GET")
        output_GET=outputMMRR(genMat=genMat,Xmats=Xmats_GET,nummmrr,thislength=length(Xmats_GET),name_gene=name_gene,scale=T,center=T) 
        write.table(output_GET,outfilename,append=T,sep="\t",quote=F,row.names = F,col.names = T)
        
        return(list(output_G,output_E,output_T,output_GE,output_GT,output_ET,output_GET))
}

dist_raw_all = read.table("/Users/kprovost/Documents/Postdoc_Working/JY_Project/mean_centroid_distances_individuals_RAW_BOTH_YEAR.txt")
dist_raw_win = read.table("/Users/kprovost/Documents/Postdoc_Working/JY_Project/mean_centroid_distances_individuals_RAW_BOTH_WINTER.txt")
dist_raw_sum = read.table("/Users/kprovost/Documents/Postdoc_Working/JY_Project/mean_centroid_distances_individuals_RAW_BOTH_SUMMER.txt")
dist_raw_all_noout = read.table("/Users/kprovost/Documents/Postdoc_Working/JY_Project/mean_centroid_distances_individuals_RAW_BOTH_YEAR_NOOUTLIERS.txt")
dist_raw_sum_noout = read.table("/Users/kprovost/Documents/Postdoc_Working/JY_Project/mean_centroid_distances_individuals_RAW_BOTH_SUMMER_NOOUTLIERS.txt")
dist_nut_all = read.table("/Users/kprovost/Documents/Postdoc_Working/JY_Project/mean_centroid_distances_individuals_RAW_NUTTALLI_YEAR.txt")
dist_nut_win = read.table("/Users/kprovost/Documents/Postdoc_Working/JY_Project/mean_centroid_distances_individuals_RAW_NUTTALLI_WINTER.txt")
dist_nut_sum = read.table("/Users/kprovost/Documents/Postdoc_Working/JY_Project/mean_centroid_distances_individuals_RAW_NUTTALLI_SUMMER.txt")
dist_nut_all_noout = read.table("/Users/kprovost/Documents/Postdoc_Working/JY_Project/mean_centroid_distances_individuals_RAW_NUTTALLI_YEAR_NOOUTLIERS.txt")
dist_nut_sum_noout = read.table("/Users/kprovost/Documents/Postdoc_Working/JY_Project/mean_centroid_distances_individuals_RAW_NUTTALLI_SUMMER_NOOUTLIERS.txt")
dist_pug_all = read.table("/Users/kprovost/Documents/Postdoc_Working/JY_Project/mean_centroid_distances_individuals_RAW_PUGETENSIS_YEAR.txt")
dist_pug_win = read.table("/Users/kprovost/Documents/Postdoc_Working/JY_Project/mean_centroid_distances_individuals_RAW_PUGETENSIS_WINTER.txt")
dist_pug_sum = read.table("/Users/kprovost/Documents/Postdoc_Working/JY_Project/mean_centroid_distances_individuals_RAW_PUGETENSIS_SUMMER.txt")

yeardist = read.table("/Users/kprovost/Documents/Postdoc_Working/JY_Project/year_distances_individuals.txt")
climdist = read.table("/Users/kprovost/Documents/Postdoc_Working/JY_Project/climate_distances_per_individual_allWC_31Jan2023.txt",header=T)
ibddist = read.table("/Users/kprovost/Documents/Postdoc_Working/JY_Project/ibd_distances_per_individual_31Jan2023.txt",header=T)


dist_pcs_all = read.table("/Users/kprovost/Documents/Postdoc_Working/JY_Project/mean_centroid_distances_individuals_PCA_BOTH_YEAR.txt")
dist_pcs_win = read.table("/Users/kprovost/Documents/Postdoc_Working/JY_Project/mean_centroid_distances_individuals_PCA_BOTH_WINTER.txt")
dist_pcs_sum = read.table("/Users/kprovost/Documents/Postdoc_Working/JY_Project/mean_centroid_distances_individuals_PCA_BOTH_SUMMER.txt")
dist_rawpcs_all = read.table("/Users/kprovost/Documents/Postdoc_Working/JY_Project/mean_centroid_distances_individuals_RAWPCA_BOTH_YEAR.txt")
dist_rawpcs_win = read.table("/Users/kprovost/Documents/Postdoc_Working/JY_Project/mean_centroid_distances_individuals_RAWPCA_BOTH_WINTER.txt")
dist_rawpcs_sum = read.table("/Users/kprovost/Documents/Postdoc_Working/JY_Project/mean_centroid_distances_individuals_RAWPCA_BOTH_SUMMER.txt")

for(nummmrr in (c(#
        1,#3,5,10,13,15,30,50,100,130,150,300,500,
        1000
))) {
        
        #outlist = generateMMRR(songdist=dist_pcs_all,ibddist,climdist,yeardist,name_gene="dist_pcs_all",outfilename=outfilename,nummmrr=nummmrr)
        #outlist = generateMMRR(songdist=dist_pcs_win,ibddist,climdist,yeardist,name_gene="dist_pcs_win",outfilename=outfilename,nummmrr=nummmrr)
        #outlist = generateMMRR(songdist=dist_pcs_sum,ibddist,climdist,yeardist,name_gene="dist_pcs_sum",outfilename=outfilename,nummmrr=nummmrr)
        #outlist = generateMMRR(songdist=dist_rawpcs_all,ibddist,climdist,yeardist,name_gene="dist_rawpcs_all",outfilename=outfilename,nummmrr=nummmrr)
        #outlist = generateMMRR(songdist=dist_rawpcs_win,ibddist,climdist,yeardist,name_gene="dist_rawpcs_win",outfilename=outfilename,nummmrr=nummmrr)
        #outlist = generateMMRR(songdist=dist_rawpcs_sum,ibddist,climdist,yeardist,name_gene="dist_rawpcs_sum",outfilename=outfilename,nummmrr=nummmrr)
        
        outlist = generateMMRR(songdist=dist_pug_all,ibddist,climdist,yeardist,name_gene="dist_pug_all",outfilename=outfilename,nummmrr=nummmrr)
        outlist = generateMMRR(songdist=dist_pug_sum,ibddist,climdist,yeardist,name_gene="dist_pug_sum",outfilename=outfilename,nummmrr=nummmrr)
        outlist = generateMMRR(songdist=dist_pug_win,ibddist,climdist,yeardist,name_gene="dist_pug_win",outfilename=outfilename,nummmrr=nummmrr)
        
        outlist = generateMMRR(songdist=dist_nut_all_noout,ibddist,climdist,yeardist,name_gene="dist_nut_all_noout",outfilename=outfilename,nummmrr=nummmrr)
        outlist = generateMMRR(songdist=dist_nut_sum_noout,ibddist,climdist,yeardist,name_gene="dist_nut_sum_noout",outfilename=outfilename,nummmrr=nummmrr)
        outlist = generateMMRR(songdist=dist_nut_all,ibddist,climdist,yeardist,name_gene="dist_nut_all",outfilename=outfilename,nummmrr=nummmrr)
        outlist = generateMMRR(songdist=dist_nut_sum,ibddist,climdist,yeardist,name_gene="dist_nut_sum",outfilename=outfilename,nummmrr=nummmrr)
        outlist = generateMMRR(songdist=dist_nut_win,ibddist,climdist,yeardist,name_gene="dist_nut_win",outfilename=outfilename,nummmrr=nummmrr)
        
        outlist = generateMMRR(songdist=dist_raw_all_noout,ibddist,climdist,yeardist,name_gene="dist_raw_all_noout",outfilename=outfilename,nummmrr=nummmrr)
        outlist = generateMMRR(songdist=dist_raw_sum_noout,ibddist,climdist,yeardist,name_gene="dist_raw_sum_noout",outfilename=outfilename,nummmrr=nummmrr)
        outlist = generateMMRR(songdist=dist_raw_all,ibddist,climdist,yeardist,name_gene="dist_raw_all",outfilename=outfilename,nummmrr=nummmrr)
        outlist = generateMMRR(songdist=dist_raw_sum,ibddist,climdist,yeardist,name_gene="dist_raw_sum",outfilename=outfilename,nummmrr=nummmrr)
        outlist = generateMMRR(songdist=dist_raw_win,ibddist,climdist,yeardist,name_gene="dist_raw_win",outfilename=outfilename,nummmrr=nummmrr)
        
        
}

## hey are the envs different? are the lats dif? are the longs dif?
library(raster)
library(RStoolbox)
centroids_pca = read.table("/Users/kprovost/Documents/Postdoc_Working/JY_Project/centroid_locations_per_individual_PCA_seasons.txt",header=T)
r <- getData("worldclim",var="bio",res=10)
rc = crop(r,extent(-130,-100,25,55))

## generate a rasterpca
rc_pca=rasterPCA(rc,center=T,scale.=T)
summary(rc_pca$model) ## importance
loadings(rc_pca$model) ## rotation
rc_pca_map = rc_pca$map
plot(rc_pca_map[[1]])

points = extract(rc,centroids_pca[,c("LONGITUDE","LATITUDE")])
points_pca = extract(rc_pca_map,centroids_pca[,c("LONGITUDE","LATITUDE")])
cent_point = cbind(centroids_pca,points)
cent_point = cent_point[,c(12,13,15:36)]
cent_point = cent_point[cent_point$SEASON!="UNKNOWN",]
cent_point = cent_point[cent_point$OUTLIER!=1,]
cent_point = cent_point[complete.cases(cent_point),]
cent_point = unique(cent_point)

biopca = prcomp(cent_point[,6:24],center=T,scale.=T)
cent_biopca = cbind(cent_point,biopca$x)
biopca$rotation
summary(biopca)$importance

envmodbio1 = aov(cent_biopca$bio1~cent_point$SEASON+cent_point$SUBSPECIES)
envmodbio2 = aov(cent_biopca$bio2~cent_point$SEASON+cent_point$SUBSPECIES)
envmodbio3 = aov(cent_biopca$bio3~cent_point$SEASON+cent_point$SUBSPECIES)
envmodbio4 = aov(cent_biopca$bio4~cent_point$SEASON+cent_point$SUBSPECIES)
envmodbio5 = aov(cent_biopca$bio5~cent_point$SEASON+cent_point$SUBSPECIES)
envmodbio6 = aov(cent_biopca$bio6~cent_point$SEASON+cent_point$SUBSPECIES)
envmodbio7 = aov(cent_biopca$bio7~cent_point$SEASON+cent_point$SUBSPECIES)
envmodbio8 = aov(cent_biopca$bio8~cent_point$SEASON+cent_point$SUBSPECIES)
envmodbio9 = aov(cent_biopca$bio9~cent_point$SEASON+cent_point$SUBSPECIES)
envmodbio10 = aov(cent_biopca$bio10~cent_point$SEASON+cent_point$SUBSPECIES)
envmodbio11 = aov(cent_biopca$bio11~cent_point$SEASON+cent_point$SUBSPECIES)
envmodbio12 = aov(cent_biopca$bio12~cent_point$SEASON+cent_point$SUBSPECIES)
envmodbio13 = aov(cent_biopca$bio13~cent_point$SEASON+cent_point$SUBSPECIES)
envmodbio14 = aov(cent_biopca$bio14~cent_point$SEASON+cent_point$SUBSPECIES)
envmodbio15 = aov(cent_biopca$bio15~cent_point$SEASON+cent_point$SUBSPECIES)
envmodbio16 = aov(cent_biopca$bio16~cent_point$SEASON+cent_point$SUBSPECIES)
envmodbio17 = aov(cent_biopca$bio17~cent_point$SEASON+cent_point$SUBSPECIES)
envmodbio18 = aov(cent_biopca$bio18~cent_point$SEASON+cent_point$SUBSPECIES)
envmodbio19 = aov(cent_biopca$bio19~cent_point$SEASON+cent_point$SUBSPECIES)

summary(envmodbio1) ## both
summary(envmodbio2) ## season
summary(envmodbio3) ## both
summary(envmodbio4) ## subspp
summary(envmodbio5) ## both
summary(envmodbio6) ## both
summary(envmodbio7) ## subspp
summary(envmodbio8) ## both
summary(envmodbio9) ## both
summary(envmodbio10) ## both
summary(envmodbio11) ## both
summary(envmodbio12) ## both
summary(envmodbio13) ## both
summary(envmodbio14) ## both
summary(envmodbio15) ## both
summary(envmodbio16) ## both
summary(envmodbio17) ## both
summary(envmodbio18) ## both
summary(envmodbio19) ## both


envmodPC1=aov(cent_biopca$PC1~cent_point$SEASON+cent_point$SUBSPECIES)
summary(envmodPC1) ## both
envmodPC2=aov(cent_biopca$PC2~cent_point$SEASON+cent_point$SUBSPECIES)
summary(envmodPC2) ## subspp
envmodPC3=aov(cent_biopca$PC3~cent_point$SEASON+cent_point$SUBSPECIES)
summary(envmodPC3) ## neither
envmodPC4 = aov(cent_biopca$PC4~cent_point$SEASON+cent_point$SUBSPECIES)
envmodPC5 = aov(cent_biopca$PC5~cent_point$SEASON+cent_point$SUBSPECIES)
envmodPC6 = aov(cent_biopca$PC6~cent_point$SEASON+cent_point$SUBSPECIES)
envmodPC7 = aov(cent_biopca$PC7~cent_point$SEASON+cent_point$SUBSPECIES)
envmodPC8 = aov(cent_biopca$PC8~cent_point$SEASON+cent_point$SUBSPECIES)
envmodPC9 = aov(cent_biopca$PC9~cent_point$SEASON+cent_point$SUBSPECIES)
envmodPC10 = aov(cent_biopca$PC10~cent_point$SEASON+cent_point$SUBSPECIES)
envmodPC11 = aov(cent_biopca$PC11~cent_point$SEASON+cent_point$SUBSPECIES)
envmodPC12 = aov(cent_biopca$PC12~cent_point$SEASON+cent_point$SUBSPECIES)
envmodPC13 = aov(cent_biopca$PC13~cent_point$SEASON+cent_point$SUBSPECIES)
envmodPC14 = aov(cent_biopca$PC14~cent_point$SEASON+cent_point$SUBSPECIES)
envmodPC15 = aov(cent_biopca$PC15~cent_point$SEASON+cent_point$SUBSPECIES)
envmodPC16 = aov(cent_biopca$PC16~cent_point$SEASON+cent_point$SUBSPECIES)
envmodPC17 = aov(cent_biopca$PC17~cent_point$SEASON+cent_point$SUBSPECIES)
envmodPC18 = aov(cent_biopca$PC18~cent_point$SEASON+cent_point$SUBSPECIES)
envmodPC19 = aov(cent_biopca$PC19~cent_point$SEASON+cent_point$SUBSPECIES)
summary(envmodPC4) 
summary(envmodPC5) 
summary(envmodPC6) 
summary(envmodPC7) 
summary(envmodPC8) 
summary(envmodPC9) 
summary(envmodPC10)
summary(envmodPC11) 
summary(envmodPC12)
summary(envmodPC13) 
summary(envmodPC14) 
summary(envmodPC15) 
summary(envmodPC16) 
summary(envmodPC17)
summary(envmodPC18) 
summary(envmodPC19) 

plot(cent_biopca$PC1,cent_biopca$PC2,type="n")
for(i in 1:length(unique(cent_biopca$SUBSPECIES))) {
        spp = sort(unique(cent_biopca$SUBSPECIES))[i]
        toplot = cent_biopca[cent_biopca$SUBSPECIES==spp,c("PC1","PC2")]
        toplot = toplot[complete.cases(toplot),]
        points(toplot$PC1,toplot$PC2,col="grey")
        car::dataEllipse(toplot$PC1, toplot$PC2, levels=c(0.75),lty=1,
                         add=T,col=i,plot.points=F,center.pch = F)
}
plot(cent_biopca$PC1,cent_biopca$PC2,type="n")
for(i in 1:length(unique(cent_biopca$SEASON))) {
        spp = sort(unique(cent_biopca$SEASON))[i]
        toplot = cent_biopca[cent_biopca$SEASON==spp,c("PC1","PC2")]
        toplot = toplot[complete.cases(toplot),]
        points(toplot$PC1,toplot$PC2,col="grey")
        car::dataEllipse(toplot$PC1, toplot$PC2, levels=c(0.75),lty=1,
                         add=T,col=i,plot.points=F,center.pch = F)
}

plot(cent_biopca$PC1,cent_biopca$PC2,col="black")
#points(cent_biopca$PC1[cent_biopca$SUBSPECIES=="nuttalli" & cent_biopca$SEASON=="SUMMER"],cent_biopca$PC2[cent_biopca$SUBSPECIES=="nuttalli" & cent_biopca$SEASON=="SUMMER"],col="lightblue",pch=16)
points(cent_biopca$PC1[cent_biopca$SUBSPECIES=="nuttalli" & cent_biopca$SEASON=="WINTER"],cent_biopca$PC2[cent_biopca$SUBSPECIES=="nuttalli" & cent_biopca$SEASON=="WINTER"],col="blue",pch=16)
#points(cent_biopca$PC1[cent_biopca$SUBSPECIES=="pugetensis" & cent_biopca$SEASON=="SUMMER"],cent_biopca$PC2[cent_biopca$SUBSPECIES=="pugetensis" & cent_biopca$SEASON=="SUMMER"],col="pink",pch=16)
#points(cent_biopca$PC1[cent_biopca$SUBSPECIES=="pugetensis" & cent_biopca$SEASON=="WINTER"],cent_biopca$PC2[cent_biopca$SUBSPECIES=="pugetensis" & cent_biopca$SEASON=="WINTER"],col="red",pch=16)

for(i in 1:length(unique(cent_biopca$SUBSPECIES))) {
        spp = sort(unique(cent_biopca$SUBSPECIES))[i]
        for(j in 1:length(unique(cent_biopca$SEASON))) {
                if(i == 1) {
                        shape=1
                        if(j == 1){
                                color="lightblue"
                        } else {
                                color="blue"
                        }     
                } else {
                        shape=2
                        if(j == 1){
                                color="pink" 
                        } else {
                                color="red"
                        }    
                }
                seas = sort(unique(cent_biopca$SEASON))[j]
                toplot = cent_biopca[cent_biopca$SUBSPECIES==spp & cent_biopca$SEASON==seas,c("PC1","PC2")]
                toplot = toplot[complete.cases(toplot),]
                points(toplot$PC1,toplot$PC2,col=color,pch=shape)
                car::dataEllipse(toplot$PC1, toplot$PC2, levels=c(0.75),lty=1,
                                 add=T,col=color,plot.points=F,center.pch = F)
        }
}
legend("topright",legend=c("N-S","N-W","P-S","P-W"),
       col=c("lightblue","blue","pink","red"),lty=c(1,1,1,1),pch=c(1,1,2,2))



boxplot(cent_biopca$PC1~cent_point$SEASON+cent_point$SUBSPECIES)
boxplot(cent_biopca$PC2~cent_point$SEASON+cent_point$SUBSPECIES)
boxplot(cent_biopca$PC3~cent_point$SEASON+cent_point$SUBSPECIES)
boxplot(cent_biopca$PC4~cent_point$SEASON+cent_point$SUBSPECIES)
boxplot(cent_biopca$PC5~cent_point$SEASON+cent_point$SUBSPECIES)
boxplot(cent_biopca$PC6~cent_point$SEASON+cent_point$SUBSPECIES)





distancePlot = function(songdist,ibddist,climdist,yeardist,pngname="~/test.png"){
        print("matching")
        genMat = as.matrix(songdist)
        geoMat = as.matrix(ibddist)
        ecoMat = as.matrix(climdist)
        timMat = as.matrix(yeardist)
        
        genMat = genMat[order(rownames(genMat)),order(colnames(genMat))]
        geoMat = geoMat[order(rownames(geoMat)),order(colnames(geoMat))]
        ecoMat = ecoMat[order(rownames(ecoMat)),order(colnames(ecoMat))]
        timMat = timMat[order(rownames(timMat)),order(colnames(timMat))]
        
        matches = intersect(rownames(genMat),rownames(geoMat))
        matches = intersect(rownames(ecoMat),matches)
        matches = intersect(rownames(timMat),matches)
        
        genMat = genMat[which(matches %in% rownames(genMat)),which(matches %in% colnames(genMat))]
        geoMat = geoMat[which(matches %in% rownames(geoMat)),which(matches %in% colnames(geoMat))]
        ecoMat = ecoMat[which(matches %in% rownames(ecoMat)),which(matches %in% colnames(ecoMat))]
        timMat = timMat[which(matches %in% rownames(timMat)),which(matches %in% colnames(timMat))]
        
        geogenlm = lm(as.numeric(genMat)~as.numeric(geoMat))
        ecogenlm = lm(as.numeric(genMat)~as.numeric(ecoMat))
        timgenlm = lm(as.numeric(genMat)~as.numeric(timMat))
        geoecolm = lm(as.numeric(ecoMat)~as.numeric(geoMat))
        geotimlm = lm(as.numeric(timMat)~as.numeric(geoMat))
        ecotimlm = lm(as.numeric(timMat)~as.numeric(ecoMat))
        
        png(pngname)
        par(mfrow=c(3,2))
        plot(geoMat,genMat,col=rgb(0,0,0,0.1),xlim=c(0,20),ylim=c(0,360)); abline(geogenlm,col="red")
        plot(ecoMat,genMat,col=rgb(0,0,0,0.1),xlim=c(0,6000),ylim=c(0,360)); abline(ecogenlm,col="red")
        plot(timMat,genMat,col=rgb(0,0,0,0.1),xlim=c(0,60),ylim=c(0,360)); abline(timgenlm,col="red")
        plot(geoMat,ecoMat,col=rgb(0,0,0,0.1),xlim=c(0,20),ylim=c(0,6000)); abline(geoecolm,col="red")
        plot(geoMat,timMat,col=rgb(0,0,0,0.1),xlim=c(0,20),ylim=c(0,60)); abline(geotimlm,col="red")
        plot(ecoMat,timMat,col=rgb(0,0,0,0.1),xlim=c(0,6000),ylim=c(0,60)); abline(ecotimlm,col="red")
        dev.off()
}

distancePlot(songdist=dist_raw_all,ibddist,climdist,yeardist,pngname="~/dist_raw_all.png")
distancePlot(songdist=dist_pug_all,ibddist,climdist,yeardist,pngname="~/dist_pug_all.png")
distancePlot(songdist=dist_nut_all,ibddist,climdist,yeardist,pngname="~/dist_nut_all.png")
distancePlot(songdist=dist_raw_win,ibddist,climdist,yeardist,pngname="~/dist_raw_win.png")
distancePlot(songdist=dist_pug_win,ibddist,climdist,yeardist,pngname="~/dist_pug_win.png")
distancePlot(songdist=dist_nut_win,ibddist,climdist,yeardist,pngname="~/dist_nut_win.png")
distancePlot(songdist=dist_raw_sum,ibddist,climdist,yeardist,pngname="~/dist_raw_sum.png")
distancePlot(songdist=dist_pug_sum,ibddist,climdist,yeardist,pngname="~/dist_pug_sum.png")
distancePlot(songdist=dist_nut_sum,ibddist,climdist,yeardist,pngname="~/dist_nut_sum.png")
distancePlot(songdist=dist_raw_all_noout,ibddist,climdist,yeardist,pngname="~/dist_raw_all_noout.png")
distancePlot(songdist=dist_nut_all_noout,ibddist,climdist,yeardist,pngname="~/dist_nut_all_noout.png")
distancePlot(songdist=dist_raw_sum_noout,ibddist,climdist,yeardist,pngname="~/dist_raw_sum_noout.png")
distancePlot(songdist=dist_nut_sum_noout,ibddist,climdist,yeardist,pngname="~/dist_nut_sum_noout.png")


## check if the outliers are different

boxplot(centroids_pca$Bandwidth~centroids_pca$OUTLIER + centroids_pca$SUBSPECIES)
plot(centroids_pca$PC1,centroids_pca$PC2,pch=as.numeric(as.factor(centroids_pca$SUBSPECIES)),
     col = as.numeric(as.factor(centroids_pca$SEASON)),type="n")

par(mfrow=c(2,1))
plot(centroids_pca$PC1,centroids_pca$PC2,type="n")
for(i in 1:length(unique(centroids_pca$SUBSPECIES))) {
        spp = sort(unique(centroids_pca$SUBSPECIES))[i]
        toplot = centroids_pca[centroids_pca$SUBSPECIES==spp,c("PC1","PC2")]
        toplot = toplot[complete.cases(toplot),]
        points(toplot$PC1,toplot$PC2,col="grey")
        car::dataEllipse(toplot$PC1, toplot$PC2, levels=c(0.75),lty=1,
                         add=T,col=i,plot.points=F,center.pch = F)
}
plot(centroids_pca$PC1,centroids_pca$PC2,type="n")
for(j in 1:length(unique(centroids_pca$SEASON))) {
        spp = sort(unique(centroids_pca$SEASON))[j]
        toplot = centroids_pca[centroids_pca$SEASON==spp,c("PC1","PC2")]
        toplot = toplot[complete.cases(toplot),]
        points(toplot$PC1,toplot$PC2,col="grey")
        car::dataEllipse(toplot$PC1, toplot$PC2, levels=c(0.75),lty=1,
                         add=T,col=j,plot.points=F,center.pch = F)
}
plot(centroids_pca$PC1,centroids_pca$PC2,type="n")



## ellipse by subspecies by season
png("~/subspp_season_zono_PC1PC2_FULL.png")
plot(centroids_pca$PC1,centroids_pca$PC2,col="grey")
points(centroids_pca$PC1[centroids_pca$OUTLIER==1],centroids_pca$PC2[centroids_pca$OUTLIER==1],col="darkblue")
car::dataEllipse(centroids_pca$PC1[centroids_pca$SUBSPECIES=="nuttalli" & centroids_pca$SEASON=="SUMMER" & !is.na(centroids_pca$PC1) & !is.na(centroids_pca$PC2)], 
                 centroids_pca$PC2[centroids_pca$SUBSPECIES=="nuttalli" & centroids_pca$SEASON=="SUMMER" & !is.na(centroids_pca$PC1) & !is.na(centroids_pca$PC2)], 
                 levels=c(0.75),lty=1,add=T,col="darkblue",plot.points=F,center.pch = F)
car::dataEllipse(centroids_pca$PC1[centroids_pca$SUBSPECIES=="nuttalli" & centroids_pca$SEASON=="WINTER" & !is.na(centroids_pca$PC1) & !is.na(centroids_pca$PC2)], 
                 centroids_pca$PC2[centroids_pca$SUBSPECIES=="nuttalli" & centroids_pca$SEASON=="WINTER" & !is.na(centroids_pca$PC1) & !is.na(centroids_pca$PC2)], 
                 levels=c(0.75),lty=1,add=T,col="blue",plot.points=F,center.pch = F)
car::dataEllipse(centroids_pca$PC1[centroids_pca$SUBSPECIES=="nuttalli" & centroids_pca$SEASON=="UNKNOWN" & !is.na(centroids_pca$PC1) & !is.na(centroids_pca$PC2)], 
                 centroids_pca$PC2[centroids_pca$SUBSPECIES=="nuttalli" & centroids_pca$SEASON=="UNKNOWN" & !is.na(centroids_pca$PC1) & !is.na(centroids_pca$PC2)], 
                 levels=c(0.75),lty=1,add=T,col="lightblue",plot.points=F,center.pch = F)
car::dataEllipse(centroids_pca$PC1[centroids_pca$SUBSPECIES=="pugetensis" & centroids_pca$SEASON=="SUMMER" & !is.na(centroids_pca$PC1) & !is.na(centroids_pca$PC2)], 
                 centroids_pca$PC2[centroids_pca$SUBSPECIES=="pugetensis" & centroids_pca$SEASON=="SUMMER" & !is.na(centroids_pca$PC1) & !is.na(centroids_pca$PC2)], 
                 levels=c(0.75),lty=1,add=T,col="darkred",plot.points=F,center.pch = F)
car::dataEllipse(centroids_pca$PC1[centroids_pca$SUBSPECIES=="pugetensis" & centroids_pca$SEASON=="WINTER" & !is.na(centroids_pca$PC1) & !is.na(centroids_pca$PC2)], 
                 centroids_pca$PC2[centroids_pca$SUBSPECIES=="pugetensis" & centroids_pca$SEASON=="WINTER" & !is.na(centroids_pca$PC1) & !is.na(centroids_pca$PC2)], 
                 levels=c(0.75),lty=1,add=T,col="red",plot.points=F,center.pch = F)
car::dataEllipse(centroids_pca$PC1[centroids_pca$SUBSPECIES=="pugetensis" & centroids_pca$SEASON=="UNKNOWN" & !is.na(centroids_pca$PC1) & !is.na(centroids_pca$PC2)], 
                 centroids_pca$PC2[centroids_pca$SUBSPECIES=="pugetensis" & centroids_pca$SEASON=="UNKNOWN" & !is.na(centroids_pca$PC1) & !is.na(centroids_pca$PC2)], 
                 levels=c(0.75),lty=1,add=T,col="pink",plot.points=F,center.pch = F)
legend("topleft",
       legend=c("n-SUM","n-WIN","n-?",
                "p-SUM","p-WIN","p-?"),
       col=c("darkblue","blue","lightblue",
             "darkred","red","pink"),
       lty=1)
dev.off()

png("~/subspp_season_zono_PC1PC4_FULL.png")
plot(centroids_pca$PC1,centroids_pca$PC4,col="grey")
points(centroids_pca$PC1[centroids_pca$OUTLIER==1],centroids_pca$PC4[centroids_pca$OUTLIER==1],col="darkblue")
car::dataEllipse(centroids_pca$PC1[centroids_pca$SUBSPECIES=="nuttalli" & centroids_pca$SEASON=="SUMMER" & !is.na(centroids_pca$PC1) & !is.na(centroids_pca$PC4)], 
                 centroids_pca$PC4[centroids_pca$SUBSPECIES=="nuttalli" & centroids_pca$SEASON=="SUMMER" & !is.na(centroids_pca$PC1) & !is.na(centroids_pca$PC4)], 
                 levels=c(0.75),lty=1,add=T,col="darkblue",plot.points=F,center.pch = F)
car::dataEllipse(centroids_pca$PC1[centroids_pca$SUBSPECIES=="nuttalli" & centroids_pca$SEASON=="WINTER" & !is.na(centroids_pca$PC1) & !is.na(centroids_pca$PC4)], 
                 centroids_pca$PC4[centroids_pca$SUBSPECIES=="nuttalli" & centroids_pca$SEASON=="WINTER" & !is.na(centroids_pca$PC1) & !is.na(centroids_pca$PC4)], 
                 levels=c(0.75),lty=1,add=T,col="blue",plot.points=F,center.pch = F)
car::dataEllipse(centroids_pca$PC1[centroids_pca$SUBSPECIES=="nuttalli" & centroids_pca$SEASON=="UNKNOWN" & !is.na(centroids_pca$PC1) & !is.na(centroids_pca$PC4)], 
                 centroids_pca$PC4[centroids_pca$SUBSPECIES=="nuttalli" & centroids_pca$SEASON=="UNKNOWN" & !is.na(centroids_pca$PC1) & !is.na(centroids_pca$PC4)], 
                 levels=c(0.75),lty=1,add=T,col="lightblue",plot.points=F,center.pch = F)
car::dataEllipse(centroids_pca$PC1[centroids_pca$SUBSPECIES=="pugetensis" & centroids_pca$SEASON=="SUMMER" & !is.na(centroids_pca$PC1) & !is.na(centroids_pca$PC4)], 
                 centroids_pca$PC4[centroids_pca$SUBSPECIES=="pugetensis" & centroids_pca$SEASON=="SUMMER" & !is.na(centroids_pca$PC1) & !is.na(centroids_pca$PC4)], 
                 levels=c(0.75),lty=1,add=T,col="darkred",plot.points=F,center.pch = F)
car::dataEllipse(centroids_pca$PC1[centroids_pca$SUBSPECIES=="pugetensis" & centroids_pca$SEASON=="WINTER" & !is.na(centroids_pca$PC1) & !is.na(centroids_pca$PC4)], 
                 centroids_pca$PC4[centroids_pca$SUBSPECIES=="pugetensis" & centroids_pca$SEASON=="WINTER" & !is.na(centroids_pca$PC1) & !is.na(centroids_pca$PC4)], 
                 levels=c(0.75),lty=1,add=T,col="red",plot.points=F,center.pch = F)
car::dataEllipse(centroids_pca$PC1[centroids_pca$SUBSPECIES=="pugetensis" & centroids_pca$SEASON=="UNKNOWN" & !is.na(centroids_pca$PC1) & !is.na(centroids_pca$PC4)], 
                 centroids_pca$PC4[centroids_pca$SUBSPECIES=="pugetensis" & centroids_pca$SEASON=="UNKNOWN" & !is.na(centroids_pca$PC1) & !is.na(centroids_pca$PC4)], 
                 levels=c(0.75),lty=1,add=T,col="pink",plot.points=F,center.pch = F)
legend("topleft",
       legend=c("n-SUM","n-WIN","n-?",
                "p-SUM","p-WIN","p-?"),
       col=c("darkblue","blue","lightblue",
             "darkred","red","pink"),
       lty=1)
dev.off()

## gbif

csv=read.csv("/Users/kprovost/Downloads/gbif_pugetensis_10.15468:dl.vg5xnc.csv")
plot(csv$decimalLongitude,csv$decimalLatitude)
par(mfrow=c(3,4))
for(m in sort(unique(csv$month))){
        plot(csv$decimalLongitude,csv$decimalLatitude,type="n",main=m)
        points(csv$decimalLongitude[csv$month==m],csv$decimalLatitude[csv$month==m],main=m)
        
}
csv$decade = round(csv$year,-1)
sort(unique(csv$decade))
par(mfrow=c(4,5))
for(d in sort(unique(csv$decade))){
        plot(csv$decimalLongitude,csv$decimalLatitude,type="n",main=d)
        points(csv$decimalLongitude[csv$decade==d],csv$decimalLatitude[csv$decade==d],main=d)
        
}
