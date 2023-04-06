
file="/Users/kprovost/Documents/Postdoc_Working/MMRR/WAVS/Wave/0STATSDONE/Amphispiza/rvn.dat_trimmed_spectro_fcts_collapsed_COMBINED_2Feb2023.txt.temp"
df=read.table(file)
badcolumns = which(colSums(is.na(df)) == nrow(df))
df.full <- df
df.full <- df.full[,-badcolumns]
badcol2 = which(colnames(df.full) %in% c("Selection","View","Channel","Low.Freq..Hz.","High.Freq..Hz.","type"))
df.full = df.full[,-badcol2]
df.full = df.full[complete.cases(df.full),]

## check if numeric
for (item in colnames(df.full)) {
  ## do something
  print(item)
  check = is.numeric(df.full[, item])
  
  if (check == TRUE) {
    ## do something
    ## keep this column
    ## do nothing
    print("keep!")
  } else {
    ## do something else
    ## get rid of this column
    badcolumn = which(colnames(df.full) == item) ## get the column number
    df.full <- df.full[,-badcolumn]
  }
}

head(df.full)
pca = prcomp(df.full, center = T, scale. = T)
data = as.data.frame(pca$x)
rotation = pca$rotation
importance = summary(pca)$importance

plot(data$PC1,data$PC2)
dfpca = cbind(df.full,data)
dfpca = unique(dfpca)

df.merge = merge(x=df,y=dfpca,all.x=T)
for(i in sort(unique(df.merge$Begin.File))){
  print(i)
  df.i = df.merge[df.merge$Begin.File == i,]
  plot(df.i$PC1,df.i$PC2)
}



