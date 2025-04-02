my_file = "/Users/kprovost/Documents/147161 table.txt"
my_df = read.table(my_file,sep="\t",header=T)
my_df[my_df==""] = NA
df_waveform = my_df[grepl("Waveform",my_df$View),]
df_spectrogram = my_df[grepl("Spectrogram",my_df$View),]
## remove cols with all NA
df_waveform_small = df_waveform[,colSums(is.na(df_waveform))!=nrow(df_waveform)]
df_spectrogram_small = df_spectrogram[,colSums(is.na(df_spectrogram))!=nrow(df_spectrogram)]
same_cols = intersect(colnames(df_waveform_small),colnames(df_spectrogram_small))
df_waveform_small[,same_cols] == df_spectrogram_small[,same_cols]
## different: View, Length..frames, Max.Time..s., Peak.Time..s., Peak.Time.Relative

colnames(df_waveform_small)[which(colnames(df_waveform_small) %in% c("View","Length..frames.","Max.Time..s.","Peak.Time..s.","Peak.Time.Relative"))] = c("View.waveform","Length..frames.waveform","Max.Time..s.waveform","Peak.Time..s.waveform","Peak.Time.Relative.waveform")
colnames(df_spectrogram_small)[which(colnames(df_spectrogram_small) %in% c("View","Length..frames.","Max.Time..s.","Peak.Time..s.","Peak.Time.Relative"))] = c("View.spectrogram","Length..frames.spectrogram","Max.Time..s.spectrogram","Peak.Time..s.spectrogram","Peak.Time.Relative.spectrogram")

df_merged = merge(df_waveform_small,df_spectrogram_small,all=T)
write.table(df_merged,gsub(".txt",".merged.txt",my_file))
