# Rscript "/Users/kprovost/Documents/GitHub/bioacoustics/mmrr_redo.R" "Pipilo"

rm(list=ls())
args = commandArgs(trailingOnly=TRUE)
do_mmrr = T
doX2 = F
nperm=1000

## import command line values or default to own species
if (length(args)<=0) {
  group_to_pick_for_mmrr=".txt"
  pcaOnly=F
} else if (length(args)==1) {
  group_to_pick_for_mmrr=args[1]
  pcaOnly=F
} else {
  group_to_pick_for_mmrr=args[1]
  pcaOnly=T
}

if(do_mmrr==F){
  
  df1 = read.table("/Users/kprovost/Documents/Postdoc_Working/song_genetics_metadata_9March2023_Passerellidae.txt",header=T,sep="\t")
  df2 = read.table("/Users/kprovost/Documents/Postdoc_Working/MMRR/WAVS/Wave/0STATSDONE/centroid_locations_per_individual_08Mar2023.txt_PCA-DATA.txt",header=T,sep="\t")
  
  #df = gtools::smartbind(df1,df2)
  df = merge(df1,df2,all=T)
  df = unique(df)
  
  write.table(df,"/Users/kprovost/Documents/Postdoc_Working/MMRR/WAVS/Wave/0STATSDONE/centroid_locations_per_individual_08Mar2023.txt_PCA-DATA-METADATA.txt",sep="\t",row.names=F,quote=F)
  
  ##
  
  df = data.table::fread("/Users/kprovost/Documents/Postdoc_Working/files_for_mmrr/df_pca_env_ll_EDITED_17Mar2023_Passerellidae_FIX.csv",header=T,sep=",",fill=T,data.table=F)
  
  ## time to calculate distance matrices and metadata
  meta_col = c("COLLECTION","county","family","order","ID",
               "LONGITUDE","Ind","inds","IDFIRST","IDSECOND","GENUS","SPECIES","SUBSPECIES",
               "state","sex","elevation.meta","quality","day","decade","month")
  time_col = c("year")
  geo_col = c("LATITUDE","ADJUSTED_LONGITUDE")
  wc_col = c("bio1","bio2","bio3","bio4","bio5","bio6","bio7","bio8","bio9","bio10","bio11","bio12","bio13","bio14","bio15","bio16","bio17","bio18","bio19")
  song_colraw = c("bandwidth","center","time","inflection","slope.song")
  song_colpca = c("pc1","pc2","pc3","pc4","pc5")
  urb_col = c("cropland.DECADE","grazing.DECADE","ir_rice.DECADE","popc.DECADE","tot_irri.DECADE","uopp.DECADE")
  hab_col = c("gbbatsgeo20","gbigbpgeo20","gblulcgeo20","gbogegeo20","gbsbm2geo20","gbsbmgeo20","gbvlgeo20")
  topo_col = c("Elevation","slope","Roughness","ruggednessIndex","relief")
  landcover_col = c("Bare.CoverFraction","BuiltUp.CoverFraction","Crops.CoverFraction",
                    "Forest.Type","Grass.CoverFraction","MossLichen.CoverFraction","PermanentWater.CoverFraction",
                    "SeasonalWater.CoverFraction","Shrub.CoverFraction","Snow.CoverFraction","Tree.CoverFraction")
  landcoverdiscrete_col = c("Discrete.Classification.map","Discrete.Classification.proba","DataDensityIndicator")
  soil_col = c("SOIL.ACIDITY","SOIL.TEXTURE")
  
  df_geo = df[,unique(c(meta_col,geo_col))]
  df_wc = df[,unique(c(meta_col,wc_col))]
  df_songraw = df[,unique(c(meta_col,song_colraw))]
  df_songpca = df[,unique(c(meta_col,song_colpca))]
  df_urb = df[,unique(c(meta_col,urb_col))]
  df_hab = df[,unique(c(meta_col,hab_col))]
  df_topo = df[,unique(c(meta_col,topo_col))]
  df_landcover = df[,unique(c(meta_col,landcover_col))]
  df_time = df[,unique(c(meta_col,time_col))]
  df_soil = df[,unique(c(meta_col,soil_col))]
  df_landcoverdiscrete = df[,unique(c(meta_col,landcoverdiscrete_col))]
  
  ## split df out by genus
  for(genus in sort(unique(df$GENUS))){
    print(genus)
    df_genus = df[df$GENUS==genus,]
    data.table::fwrite(df_genus,paste("/Users/kprovost/Documents/Postdoc_Working/df_pca_env_ll_EDITED_29Mar2023_",genus,".txt",sep=""),
                       sep=",")
    for(species in sort(unique(df_genus$SPECIES))){
      print(paste(genus,species))
      df_species = df_genus[df_genus$SPECIES==species,]
      data.table::fwrite(df_species,paste("/Users/kprovost/Documents/Postdoc_Working/df_pca_env_ll_EDITED_29Mar2023_",genus,".",species,".txt",sep=""),
                         sep=",")
      
      for(subspecies in sort(unique(df_species$SUBSPECIES))){
        print(paste(genus,species,subspecies))
        df_subspecies = df_species[df_species$SUBSPECIES==subspecies,]
        data.table::fwrite(df_subspecies,paste("/Users/kprovost/Documents/Postdoc_Working/df_pca_env_ll_EDITED_29Mar2023_",genus,".",species,".",subspecies,".txt",sep=""),
                           sep=",")
      }
      
    }
  }
  
  ## re-import the dfs
  
  df_list = list.files(path="/Users/kprovost/Documents/Postdoc_Working/",
                       pattern="df_pca_env_ll_EDITED_29Mar2023_",
                       full.names = T)
  df_list = df_list[!(grepl("dist",df_list))]
  df_list = df_list[!(grepl("gz",df_list))]
  ## sort by file size
  #x <- file.info(df_list)
  #df_list = df_list[match(1:length(df_list),rank(x$size))]
  #df_list = df_list[complete.cases(df_list)]
  
  for(df_file in df_list){
    print(df_file)
    df_temp = data.table::fread(df_file,header=T,sep=",",fill=T,data.table=F)
    rownames(df_temp) = paste(df_temp$GENUS,df_temp$SPECIES,df_temp$SUBSPECIES,df_temp$COLLECTION,df_temp$ID,sep=".")
    
    if(nrow(df_temp)>1){
      if(file.exists(paste(df_file,"_landcoverdiscrete.dist.txt",sep=""))){
        print("SKIP BECAUSE FILE EXISTS")
        R.utils::gzip(df_file,remove=T)
      } else {
        
        df_geo = df_temp[,geo_col]
        df_wc = df_temp[,wc_col]
        df_songraw = df_temp[,song_colraw]
        df_songpca = df_temp[,song_colpca]
        df_urb = df_temp[,urb_col]
        df_hab = df_temp[,hab_col]
        df_topo = df_temp[,topo_col]
        df_landcover = df_temp[,landcover_col]
        df_time = df_temp[,time_col]
        df_soil = df_temp[,soil_col]
        df_landcoverdiscrete = df_temp[,landcoverdiscrete_col]
        
        df_geo_dist = as.data.frame(as.matrix(dist(df_geo)))
        df_wc_dist = as.data.frame(as.matrix(dist(df_wc)))
        df_songraw_dist = as.data.frame(as.matrix(dist(df_songraw)))
        df_songpca_dist = as.data.frame(as.matrix(dist(df_songpca)))
        df_urb_dist = as.data.frame(as.matrix(dist(df_urb)))
        df_hab_dist = as.data.frame(as.matrix(dist(df_hab)))
        df_topo_dist = as.data.frame(as.matrix(dist(df_topo)))
        df_landcover_dist = as.data.frame(as.matrix(dist(df_landcover)))
        df_time_dist = as.data.frame(as.matrix(dist(df_time)))
        colnames(df_time_dist) = rownames(df_temp)
        rownames(df_time_dist) = rownames(df_temp)
        df_soil_dist = as.data.frame(as.matrix(dist(df_soil)))
        df_landcoverdiscrete_dist = as.data.frame(as.matrix(dist(df_landcoverdiscrete)))
        
        data.table::fwrite((df_geo_dist),paste(df_file,"_geo.dist.txt",sep=""),sep=",",row.names = T,col.names = T)
        data.table::fwrite((df_wc_dist),paste(df_file,"_wc.dist.txt",sep=""),sep=",",row.names = T,col.names = T)
        data.table::fwrite((df_songraw_dist),paste(df_file,"_songraw.dist.txt",sep=""),sep=",",row.names = T,col.names = T)
        data.table::fwrite((df_songpca_dist),paste(df_file,"_songpca.dist.txt",sep=""),sep=",",row.names = T,col.names = T)
        data.table::fwrite((df_urb_dist),paste(df_file,"_urb.dist.txt",sep=""),sep=",",row.names = T,col.names = T)
        data.table::fwrite((df_hab_dist),paste(df_file,"_hab.dist.txt",sep=""),sep=",",row.names = T,col.names = T)
        data.table::fwrite((df_topo_dist),paste(df_file,"_topo.dist.txt",sep=""),sep=",",row.names = T,col.names = T)
        data.table::fwrite((df_landcover_dist),paste(df_file,"_landcover.dist.txt",sep=""),sep=",",row.names = T,col.names = T)
        data.table::fwrite((df_time_dist),paste(df_file,"_time.dist.txt",sep=""),sep=",",row.names = T,col.names = T)
        data.table::fwrite((df_soil_dist),paste(df_file,"_soil.dist.txt",sep=""),sep=",",row.names = T,col.names = T)
        data.table::fwrite((df_landcoverdiscrete_dist),paste(df_file,"_landcoverdiscrete.dist.txt",sep=""),sep=",",row.names = T,col.names = T)
      }
    } else {
      print("ONLY ONE LINE -- SKIPPING")
    }
    
  }
  
  ## and also the big one
  df_temp = df
  df_file = "/Users/kprovost/Documents/Postdoc_Working/files_for_mmrr/df_pca_env_ll_EDITED_29Mar2023_Passerellidae_FIX.txt"
  rownames(df_temp) = paste(df_temp$GENUS,df_temp$SPECIES,df_temp$SUBSPECIES,df_temp$COLLECTION,df_temp$ID,sep=".")
  if(nrow(df_temp)>1){
    if(file.exists(paste(df_file,"_landcoverdiscrete.dist.txt",sep=""))){
      print("SKIP BECAUSE FILE EXISTS")
    } else {
      
      df_geo = df_temp[,geo_col]
      df_wc = df_temp[,wc_col]
      df_songraw = df_temp[,song_colraw]
      df_songpca = df_temp[,song_colpca]
      df_urb = df_temp[,urb_col]
      df_hab = df_temp[,hab_col]
      df_topo = df_temp[,topo_col]
      df_landcover = df_temp[,landcover_col]
      df_time = df_temp[,time_col]
      df_soil = df_temp[,soil_col]
      df_landcoverdiscrete = df_temp[,landcoverdiscrete_col]
      
      df_geo_dist = as.data.frame(as.matrix(dist(df_geo)))
      df_wc_dist = as.data.frame(as.matrix(dist(df_wc)))
      df_songraw_dist = as.data.frame(as.matrix(dist(df_songraw)))
      df_songpca_dist = as.data.frame(as.matrix(dist(df_songpca)))
      df_urb_dist = as.data.frame(as.matrix(dist(df_urb)))
      df_hab_dist = as.data.frame(as.matrix(dist(df_hab)))
      df_topo_dist = as.data.frame(as.matrix(dist(df_topo)))
      df_landcover_dist = as.data.frame(as.matrix(dist(df_landcover)))
      df_time_dist = as.data.frame(as.matrix(dist(df_time)))
      colnames(df_time_dist) = rownames(df_temp)
      rownames(df_time_dist) = rownames(df_temp)
      df_soil_dist = as.data.frame(as.matrix(dist(df_soil)))
      df_landcoverdiscrete_dist = as.data.frame(as.matrix(dist(df_landcoverdiscrete)))
      
      data.table::fwrite((df_geo_dist),paste(df_file,"_geo.dist.txt",sep=""),sep=",",row.names = T,col.names = T)
      data.table::fwrite((df_wc_dist),paste(df_file,"_wc.dist.txt",sep=""),sep=",",row.names = T,col.names = T)
      data.table::fwrite((df_songraw_dist),paste(df_file,"_songraw.dist.txt",sep=""),sep=",",row.names = T,col.names = T)
      data.table::fwrite((df_songpca_dist),paste(df_file,"_songpca.dist.txt",sep=""),sep=",",row.names = T,col.names = T)
      data.table::fwrite((df_urb_dist),paste(df_file,"_urb.dist.txt",sep=""),sep=",",row.names = T,col.names = T)
      data.table::fwrite((df_hab_dist),paste(df_file,"_hab.dist.txt",sep=""),sep=",",row.names = T,col.names = T)
      data.table::fwrite((df_topo_dist),paste(df_file,"_topo.dist.txt",sep=""),sep=",",row.names = T,col.names = T)
      data.table::fwrite((df_landcover_dist),paste(df_file,"_landcover.dist.txt",sep=""),sep=",",row.names = T,col.names = T)
      data.table::fwrite((df_time_dist),paste(df_file,"_time.dist.txt",sep=""),sep=",",row.names = T,col.names = T)
      data.table::fwrite((df_soil_dist),paste(df_file,"_soil.dist.txt",sep=""),sep=",",row.names = T,col.names = T)
      data.table::fwrite((df_landcoverdiscrete_dist),paste(df_file,"_landcoverdiscrete.dist.txt",sep=""),sep=",",row.names = T,col.names = T)
    }
  } else {
    print("ONLY ONE LINE -- SKIPPING")
  }
  
  ## run mmrr for each combination?
  
  gene_dist = "/Users/kprovost/Documents/Postdoc_Working/Genetics/Birds-phylogatr-results_7dec2020/Aves/Passeriformes/Passerellidae/master.passerellidae.COI.1Dec2022.afa.DISTANCES.txt"
  gene_df = data.table::fread(gene_dist,data.table=F)
  rownames(gene_df) = gene_df$V1
  gene_df = gene_df[,rownames(gene_df)]
  colnames(gene_df)
  
  newcolnames = c()
  for(i in colnames(gene_df)){
    df_g = df[df$ID == i,c("GENUS","SPECIES","SUBSPECIES","COLLECTION","ID")]
    if(nrow(df_g)!=1){
      print("PROBLEM")
      print(i)
      print(df_g)
      
    }
    newcolnames = c(newcolnames,paste(df_g$GENUS,df_g$SPECIES,df_g$SUBSPECIES,df_g$COLLECTION,df_g$ID,sep="."))
  }
  cbind(colnames(gene_df),newcolnames)
  ## need to change the gene_dist row and col names to match these ones 
  
  gene_dist_new = "/Users/kprovost/Documents/Postdoc_Working/master.passerellidae.COI.29Mar2023.afa.DISTANCES.txt"
  colnames(gene_df) = newcolnames
  rownames(gene_df) = newcolnames
  data.table::fwrite(gene_df,file=gene_dist_new,quote=F,sep=",",row.names = T,col.names = T)
  
  ## split the gene distances we just made by genus species and subspecies 
  gene_df = data.table::fread("/Users/kprovost/Documents/Postdoc_Working/master.passerellidae.COI.29Mar2023.afa.DISTANCES.txt",sep=",",data.table=F)
  rownames(gene_df) = gene_df$V1
  gene_df = gene_df[,rownames(gene_df)]
  
  inds = rownames(gene_df)
  splitinds=strsplit(inds,"\\.")
  genera = sapply(splitinds,FUN=function(x){x[1]})
  species = sapply(splitinds,FUN=function(x){x[2]})
  subspecies = sapply(splitinds,FUN=function(x){x[3]}) ## all unknown
  
  for(genus in sort(unique(genera))){
    print(genus)
    keep=which(genera %in% genus)
    gene_df[keep,keep]
    if(file.exists(paste("/Users/kprovost/Documents/Postdoc_Working/df_pca_env_ll_EDITED_29Mar2023_",genus,".txt_gene.dist.txt",sep=""))){
      print("skip")
    } else {
      if(nrow(gene_df[keep,keep])>0){
        data.table::fwrite(gene_df[keep,keep],file=paste("/Users/kprovost/Documents/Postdoc_Working/df_pca_env_ll_EDITED_29Mar2023_",genus,".txt_gene.dist.txt",sep=""),quote=F,sep=",",row.names = T,col.names = T)
      }
    }
    for(species_i in sort(unique(species))){
      print(paste(genus,species_i))
      keep = which(genera %in% genus & species %in% species_i)
      gene_df[keep,keep]
      if(file.exists(paste("/Users/kprovost/Documents/Postdoc_Working/df_pca_env_ll_EDITED_29Mar2023_",genus,".",species_i,".txt_gene.dist.txt",sep=""))) {
        print("skip")
      } else {
        if(nrow(gene_df[keep,keep])>0){
          data.table::fwrite(gene_df[keep,keep],file=paste("/Users/kprovost/Documents/Postdoc_Working/df_pca_env_ll_EDITED_29Mar2023_",genus,".",species_i,".txt_gene.dist.txt",sep=""),quote=F,sep=",",row.names = T,col.names = T)
          ## genes are not to the subspecies level so need to also make subspecies with "unknown"
          data.table::fwrite(gene_df[keep,keep],file=paste("/Users/kprovost/Documents/Postdoc_Working/df_pca_env_ll_EDITED_29Mar2023_",genus,".",species_i,".unknown.txt_gene.dist.txt",sep=""),quote=F,sep=",",row.names = T,col.names = T)
        }
      }
    }
  }
  
} else if (do_mmrr==T) {
  ## and now, lets do some mmrr
  ## test with spizella pusilla
  
  ## loop over the response df list while doing mmrr on the predictor df list 
  
  MMRR<-function(Y,X,nperm=1000,center=T,scale=T){
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
  
  unfold<-function(X,center=T,scale=T){
    # unfold converts the lower diagonal elements of a matrix into a vector
    # unfold is called by MMRR
    x<-vector()
    for(i in 2:nrow(X)) x<-c(x,X[i,1:i-1])
    x = unlist(x)
    x<-scale(x, center=center, scale=scale)  # Comment this line out if you wish to perform the analysis without standardizing the distance matrices! 
    return(x)
  }
  
  
  ## make a function that will match all of the predictors and responses colnames
  ## for all given combinations of stuff 
  get_set_colnames_xy = function(Y,X){
    if(class(X)=="list"){
      pred_colnames = lapply(X,FUN=function(x){
        return(colnames(x))
      })
      set_pred_colnames = Reduce(intersect, pred_colnames)
      
    } else {
      set_pred_colnames = colnames(X)
    }
    
    set_all_names = intersect(set_pred_colnames,colnames(Y))
    return(set_all_names)
  }
  
  
  reduce_to_setnames = function(X,set_all_names){
    if(class(X)=="list") {
      reduce_X = lapply(X,FUN=function(x){
        x = x[set_all_names,set_all_names]
        #x = data.frame(sapply(x, function(y) as.numeric(as.character(y))))
      })
    } else {
      reduce_X = X[set_all_names,set_all_names]
      #reduce_X = data.frame(sapply(reduce_X, function(y) as.numeric(as.character(y))))
    }
    return(reduce_X)
  }
  
  ## now do this for combinations of X and Y predictors
  run_mmrr_set = function(Y,X,nperm=1000,center=T,scale=T){
    print(paste("nperm:",nperm))
    set_all_names = get_set_colnames_xy(Y,X)
    reduce_X = reduce_to_setnames(X,set_all_names)
    reduce_Y = reduce_to_setnames(Y,set_all_names)
    mmrr = MMRR(Y=reduce_Y,X=reduce_X,
                nperm=nperm,center=T,scale=T)
    return(mmrr)
  }
  
  ## format mmrr to line
  format_mmrr = function(mmrr){
    rsq = mmrr$r.squared
    coeff = mmrr$coefficients
    tstat = mmrr$tstatistic
    tpval = mmrr$tpvalue
    Fstat = mmrr$Fstatistic
    Fpval = mmrr$Fpvalue
    confmat = c(mmrr$Conf)
    confnames = c()
    for(j in 1:ncol(mmrr$Conf)){
      for(i in 1:nrow(mmrr$Conf)){
        row_i = rownames(mmrr$Conf)[i]
        col_j = colnames(mmrr$Conf)[j]
        i_j = paste(row_i,col_j)
        confnames = c(confnames,i_j)
      }
    }
    names(confmat) = confnames
    newmmrr=rbind(c(rsq,coeff,tstat,tpval,Fstat,Fpval,confmat))
    return(newmmrr)
  }
  
  ## need to  loop over Y and  need to loop over X combinations
  run_mmrr_full_1spp = function(predictor_df_list,response_df_list,nperm=1000,
                                outfile="/Users/kprovost/Documents/Postdoc_Working/mmrr_results_31Mar2023.txt"){
    numX=length(predictor_df_list)
    numY=length(response_df_list)
    for(resp_i in 1:numY){
      #for(resp_i in 1:numY){
      Y=response_df_list[[resp_i]]
      for(pred_j1 in 1:numX){
        print(paste(resp_i,pred_j1))
        X_1 = predictor_df_list[pred_j1]
        ## mmrr with just one variable
        try({
          mmrr1=run_mmrr_set(Y=Y,X=X_1,nperm=nperm)
          mmrr1f=format_mmrr(mmrr1) 
          rownames(mmrr1f) = paste(names(response_df_list)[resp_i],
                                   names(predictor_df_list[pred_j1]),
                                   nperm)
          data.table::fwrite(as.data.frame(mmrr1f),paste(outfile,"_",group_to_pick_for_mmrr,"_X1.txt",sep=""),quote=F,sep="\t",append=T,row.names = T,col.names=T)
        })
        ## mmrr with two variables because we don't have all day
        if(doX2==T){
          for(pred_j2 in (pred_j1+1):numX){
            print(paste(resp_i,pred_j1,pred_j2))
            if(pred_j1!=pred_j2 & pred_j2 <= numX){
              
              X_2 = predictor_df_list[c(pred_j1,pred_j2)]
              try({
                mmrr2=run_mmrr_set(Y=Y,X=X_2,nperm=nperm)
                mmrr2f=format_mmrr(mmrr2)
                rownames(mmrr2f) = paste(names(response_df_list)[resp_i],
                                         names(predictor_df_list)[pred_j1],
                                         names(predictor_df_list)[pred_j2],
                                         nperm)
                data.table::fwrite(as.data.frame(mmrr2f),paste(outfile,"_",group_to_pick_for_mmrr,"_X2.txt",sep=""),quote=F,sep="\t",append=T,row.names = T,col.names=T)
              })
            }
          }
        }
      }
    }
    
  }
  
  ## get list of species 
  
  path="/Users/kprovost/Documents/Postdoc_Working/"
  pattern="df_pca_env_ll_EDITED_29Mar2023_"
  dist_files = list.files(path=path,pattern=pattern,
                          full.names =T)
  dist_files = dist_files[grepl("dist",dist_files)]
  dist_split = strsplit(basename(dist_files),"_")
  species_list = sapply(dist_split,FUN=function(x){return(x[7])})
  species_list = unique(species_list)
  
  generate_mmrr_species = function(dist_files,this_species="Spizella.pusilla.unknown",nperm=1000){
    
    dist_files = dist_files[grepl(this_species,dist_files)]
    
    if(pcaOnly==T){
      response_files = dist_files[grepl("song_pca",dist_files)]
    } else {
      response_files = dist_files[grepl("_song",dist_files) | grepl("_gene",dist_files)]
    }
    predictor_files = dist_files[!grepl("_song",dist_files) & !grepl("_gene",dist_files)]
    
    response_df_list = lapply(response_files,FUN=function(x){
      df = data.table::fread(x,sep=",",data.table=F)
      rownames(df) = df$V1
      df = df[,rownames(df)]
      ## remove columns with large number of NA
      sum_na = colSums(is.na(df),na.rm=T) 
      if(max(sum_na,na.rm=T)>0){
        badcol = which(sum_na == max(sum_na,na.rm=T))
        df = df[,-badcol]
        row_na = rowSums(is.na(df),na.rm=T) 
        badrow = which(row_na == max(row_na,na.rm=T))
        df = df[-badrow,]
      }
      return(df)
    })
    names(response_df_list) = basename(response_files)
    
    predictor_df_list = lapply(predictor_files,FUN=function(x){
      df = data.table::fread(x,sep=",",data.table=F)
      rownames(df) = df$V1
      df = df[,rownames(df)]
      ## remove columns with large number of NA
      sum_na = colSums(is.na(df),na.rm=T) 
      if(max(sum_na,na.rm=T)>0){
        badcol = which(sum_na == max(sum_na,na.rm=T))
        df = df[,-badcol]
        row_na = rowSums(is.na(df),na.rm=T) 
        badrow = which(row_na == max(row_na,na.rm=T))
        df = df[-badrow,]
      }
      return(df)
    })
    names(predictor_df_list) = basename(predictor_files)
    
    run_mmrr_full_1spp(predictor_df_list,response_df_list,nperm=nperm)
    
  }
  
  species_list_x = species_list[grepl(group_to_pick_for_mmrr,species_list)]
  
  for(spp in species_list_x){
    print(spp)
    generate_mmrr_species(dist_files,this_species=spp,nperm)
  }
}
