##  Rscript "/Users/kprovost/Documents/GitHub/bioacoustics/formatting/aggregate_selections_raven.R"

#predicted_selections_folder = "/Users/kprovost/Documents/Postdoc_Working/MMRR/WAVS/OLDANNOT/"
predicted_selections_folder = "/Users/kprovost/Documents/Postdoc_Working/MMRR/WAVS/Wave/0STATSDONE/Zonotrichia/Zonotrichia.leucophrys.nuttalli/"
#predicted_selections_folder = "/Users/kprovost/Documents/Postdoc_Working/MMRR/WAVS/Wave/0FIXDONE/"
setwd(predicted_selections_folder)

slices_file = "/Users/kprovost/Documents/Postdoc_Working/MMRR/WAVS/Wave/predicted_slices_MMRR_02Mar2023.txt"

#suffix="resample.48000..*selections.*txt"; newsuffix="resample.48000.selections.MASTER.txt"
suffix = "selections.*txt"
#suffix="2205.annot.csv.selections.txt"
#suffix="221207.annot.csv.selections.txt"
#suffix="22120..annot.csv.selections.MASTER.txt"
#suffix="selections.AVERAGE.txt"
#suffix="selections.COMBINED.txt"
newsuffix = "selections.MASTER.txt"
#suffix="Table.1.selections.*.txt"; newsuffix="Table.1.selections.MASTER.txt"
#suffix="resample.48000.selections.txt"; newsuffix="resample.48000.selections.MASTER.txt"

pred_list = list.files(
  path = predicted_selections_folder,
  pattern = paste(suffix, "$", sep = ""),
  recursive = T,
  full.names = T
)
#pred_list = pred_list[!(grepl(newsuffix,pred_list))]

window_size = 88 ## size of windows taken from spectrograms, in number of time bins, shonw to neural networks
fft_size = 512 ## size of window for Fast Fourier transform, number of time bins. Default is 512.
step_size = 32 ## step size for Fast Fourier transform. Default is 64.
sampling_rate = 48000
timebin_dur = 0.00067 ## how many seconds is one time bin
min_segment_dur = 0.05
lowhz = 500
highhz = 15000
minmatch = 1

thislen = length(pred_list)
for (predicted_selections in (sort(pred_list))) {
  print(paste(predicted_selections, thislen))
  pred_df = read.table(predicted_selections, sep = "\t", header = T)
  if (colnames(pred_df)[1] != "Selection") {
    pred_df = read.table(predicted_selections, sep = "\t", header = F)
    header_row = which(pred_df[, 1] == "Selection")
    colnames(pred_df) = make.names(pred_df[header_row, ])
    pred_df = pred_df[-header_row, ]
    
  }
  pred_df = unique(pred_df)
  
  wav_len = max(as.numeric(pred_df$End.Time..s.), na.rm = T) ## length of wav (according to the max annotation)
  num_bins = ceiling(wav_len / timebin_dur)
  bin_bounds = seq(timebin_dur / 2, by = timebin_dur, length.out = num_bins) ## gets the middle of each time binpred_data = rep(0,length(bin_bounds))
  
  pred_data = rep(0, length(bin_bounds))
  
  for (row_i in 1:nrow(pred_df)) {
    #print(paste(row_i,nrow(pred_df)))
    row = pred_df[row_i, ]
    pred_positives = which(
      bin_bounds >= as.numeric(row$Begin.Time..s.) &
        bin_bounds <= as.numeric(row$End.Time..s.)
    )
    pred_data[pred_positives] = pred_data[pred_positives] + 1
  }
  
  pred_data_write = c("PRED", basename(predicted_selections), pred_data)
  
  outfile = gsub(suffix, newsuffix, predicted_selections)
  
  testrow = as.numeric(pred_data_write[3:length(pred_data_write)])
  testrow[testrow < minmatch] = 0
  testrow[testrow > minmatch] = 1
  runlength = rle(testrow)
  stops = cumsum(runlength$lengths)
  starts = stops + 1
  starts = c(1, starts)
  starts = starts[1:length(stops)]
  are_syll = which(runlength$values == max(testrow, na.rm = T))
  onsets = starts[are_syll]
  offsets = stops[are_syll]
  ## convert to seconds
  onsets_s = onsets * timebin_dur
  offsets_s = offsets * timebin_dur
  too_short = which(offsets_s - onsets_s < min_segment_dur)
  if (length(too_short) >= 1) {
    onsets_s = onsets_s[-too_short]
    offsets_s = offsets_s[-too_short]
  }
  if (length(onsets_s) >= 1) {
    ## write out
    #Selection	View	Channel	Begin Time (s)	End Time (s)	Low Freq (Hz)	High Freq (Hz)	Begin File	type
    x = data.frame(
      1:length(onsets_s),
      rep("Spectrogram", length(onsets_s)),
      rep(1, length(onsets_s)),
      onsets_s,
      offsets_s,
      rep(lowhz, length(onsets_s)),
      rep(highhz, length(onsets_s)),
      gsub("Table.1.", "", gsub(suffix, "wav", pred_data_write[2])),
      rep(1, length(onsets_s))
    )
    x = unique(x)
    colnames(x) = c(
      "Selection",
      "View",
      "Channel",
      "Begin Time (s)",
      "End Time (s)",
      "Low Freq (Hz)",
      "High Freq (Hz)",
      "Begin File",
      "type"
    )
    
    if (predicted_selections != outfile) {
      write.table(
        x,
        outfile,
        row.names = F,
        quote = F,
        sep = "\t",
        append = T
      )
    } else {
      write.table(
        x,
        outfile,
        row.names = F,
        quote = F,
        sep = "\t",
        append = F
      )
    }
  } else {
    print("BAD")
  }
  
  if (predicted_selections != outfile) {
    try({
      R.utils::gzip(predicted_selections, overwrite = T)
    })
  }
}
