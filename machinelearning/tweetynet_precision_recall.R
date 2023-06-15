## precision recall of tweetynet selections files 

true_selections_folder = "/Users/kprovost/Documents/Postdoc_Working/JY_project/2023_Zonotrichia_Model/Wave"
predicted_selections_folder = "/Users/kprovost/Documents/Postdoc_Working/JY_project/2023_Zonotrichia_Model"
setwd(predicted_selections_folder)

accuracy_metrics_file = "accuracy_metrics_07Jun2023.2.txt"
slices_file = "predicted_slices_07Jun2023.2.txt"
confusion_file = "confusion_matrix_07Jun2023.2.txt"

true_pattern = "Table.1.selections.txt"
pred_pattern = "selections.MASTER.txt"

true_list = list.files(path=true_selections_folder,
                       pattern=paste(true_pattern,"$",sep=""),
                       recursive = T,full.names = T)
pred_list = list.files(path=predicted_selections_folder,
                       pattern=paste(pred_pattern,"$",sep=""),
                       recursive = T,full.names = T)
pred_list = pred_list[!grepl("Table.1",pred_list)]

for(true_selections in true_list){
  print(true_selections)
  #true_selections = "/Users/kprovost/Documents/Postdoc_Working/Sounds_and_Annotations/9SppBalanced/Wave/Calypte.anna.unknown.BLB.28330.resample.48000.wav_413474-613957.Table.1.selections.txt"
  #wav_file = "/Users/kprovost/Documents/Postdoc_Working/Sounds_and_Annotations/9SppBalanced/Wave/Calypte.anna.unknown.BLB.28330.resample.48000.wav_413474-613957.wav"
  wav_file = sub(".Table.1.selections.txt",".wav",true_selections)
  slices_out = sub(".Table.1.selections.txt",".PREDICTED.SLICES.OUT.txt",true_selections)
  
  true_df = read.table(true_selections,sep="\t",header=T)
  wav <- tuneR::readWave(wav_file,from=0,units="seconds")
  
  window_size = 88 ## size of windows taken from spectrograms, in number of time bins, shonw to neural networks
  fft_size = 512 ## size of window for Fast Fourier transform, number of time bins. Default is 512.
  step_size = 32 ## step size for Fast Fourier transform. Default is 64.
  sampling_rate = 48000
  timebin_dur = 0.00067 ## how many seconds is one time bin
  wav_len = length(wav)
  num_bins = ceiling((wav_len/sampling_rate)/timebin_dur)
  bin_bounds = seq(timebin_dur/2,by=timebin_dur,length.out=num_bins) ## gets the middle of each time bin
  
  true_data = rep(0,length(bin_bounds))
  true_data_type = rep(0,length(bin_bounds))

  for(row_i in 1:nrow(true_df)){
    row=true_df[row_i,]
    true_positives=which(bin_bounds >= row$Begin.Time..s. & bin_bounds<= row$End.Time..s.)
    true_data[true_positives] = 1
    if(is.null(row$type)) {
      true_data_type[true_positives] = row$TYPE
    } else {
      true_data_type[true_positives] = row$type
    }
  }
  
  true_data_write = c("TRUE","NUM",true_selections,true_data)
  true_data_type_write = c("TRUE","TYPE",true_selections,true_data_type)
  write.table(as.data.frame(rbind(true_data_write)),slices_file,row.names = F,sep="\t",col.names = F,append=T)
  write.table(as.data.frame(rbind(true_data_type_write)),slices_file,row.names = F,sep="\t",col.names = F,append=T)
  
  true_file_pattern=sub(true_pattern,"",basename(true_selections))
  pred_list_sub = pred_list[grep(true_file_pattern,pred_list)]
  
  for(predicted_selections in pred_list_sub) {
    
    #predicted_selections = "/Users/kprovost/Documents/Postdoc_Working/Finished_Models/9SppBalanced/Calypte.anna.unknown.BLB.28330.resample.48000.wav_413474-613957.selections.txt"
    pred_df = read.table(predicted_selections,sep="\t",header=T)
    
    pred_data = rep(0,length(bin_bounds))
    pred_data_type = rep(0,length(bin_bounds))

    for(row_i in 1:nrow(pred_df)){
      row=pred_df[row_i,]
      pred_positives=which(bin_bounds >= row$Begin.Time..s. & bin_bounds<= row$End.Time..s.)
      pred_data[pred_positives] = 1
      if(is.null(row$type)) {
        pred_data_type[pred_positives] = row$TYPE
      } else {
        pred_data_type[pred_positives] = row$type
      }
    }

    pred_data_write = c("PRED","NUM",predicted_selections,pred_data)
    pred_data_type_write = c("PRED","TYPE",predicted_selections,pred_data_type)
    write.table(as.data.frame(rbind(pred_data_write)),slices_file,row.names = F,sep="\t",col.names = F,append=T)
    write.table(as.data.frame(rbind(pred_data_type_write)),slices_file,row.names = F,sep="\t",col.names = F,append=T)
    
    true_positives = sum(true_data==1 & pred_data==1)
    true_negatives = sum(true_data==0 & pred_data==0)
    false_positives = sum(true_data==0 & pred_data==1)
    false_negatives = sum(true_data==1 & pred_data==0)
    total = length(true_data)
    accuracy = (true_positives + true_negatives) / total
    precision = true_positives / (true_positives + false_positives)
    recall = true_positives / (true_positives + false_negatives)
    fscore = (2*precision*recall)/(precision+recall)

    true_data_type_f = as.factor(true_data_type)
    #levels(true_data_type_f) = c("0","S","W","C","T","B")
    pred_data_type_f = as.factor(pred_data_type)
    #levels(pred_data_type_f) = c("0","S","W","C","T","B")
    
    df=data.frame(truefile=true_selections,predfile=predicted_selections,total=total,true_positives=true_positives,true_negatives=true_negatives,
                  false_positives=false_positives,false_negatives=false_negatives,
                  accuracy=accuracy,precision=precision,recall=recall,fscore=fscore)
    write.table(df,file=accuracy_metrics_file,append=T,quote=F,sep="\t",row.names = F,col.names = T)
    
    ## confusion matrix file -- slicewise
    true_pred_type_t = table(true_data_type_f,pred_data_type_f)
    x=expand.grid(rownames(true_pred_type_t),colnames(true_pred_type_t))
    
    true_pred_type = matrix(true_pred_type_t,nrow=1)
    colnames(true_pred_type) = paste(x[,1],x[,2],sep=".")
    y = expand.grid(c("0","S","W","C","T","B","N","1","SB","BS","A"),c("0","S","W","C","T","B","N","1","SB","BS","A"))
    extra_colnames = paste(y[,1],y[,2],sep=".")
    missing_colnames = extra_colnames[which(!(extra_colnames %in% colnames(true_pred_type)))]
    to_add = rbind(rep(0,length(missing_colnames)))
    colnames(to_add) = missing_colnames
    true_pred_type = cbind(true_pred_type,to_add)
    true_pred_type = true_pred_type[ , order(colnames(true_pred_type))]
    true_pred_type = rbind(true_pred_type)
    
    #colnames(true_pred_type) = c("00","S0","W0","C0","T0","B0",
    #                             "0S","SS","WS","CS","TS","BS",
    #                             "0W","SW","WW","CW","TW","BW",
    #                             "0C","SC","WC","CC","TC","BC",
    #                             "0T","ST","WT","CT","TT","BT",
    #                             "0B","SB","WB","CB","TB","BB")
    rownames(true_pred_type) = predicted_selections
    write.table(true_pred_type,file=confusion_file,append=T,quote=F,sep="\t",row.names = T,col.names = T)
  }
}
