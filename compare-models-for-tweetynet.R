## zonotrichia

true = read.table("/Users/kprovost/Documents/TweetyNet/testing_wcs/Zonotrichia_leucophrys_train/Wave/BLB218.wav_0-141906.Table.1.selections.txt",
                  sep="\t",header=T)
diff = read.table("/Users/kprovost/Documents/TweetyNet/testing_wcs/Zonotrichia_leucophrys_train/BLB218.wav_0-141906.Wave_prep_210602_153028_model_Cardinalis_cardinalis.annot.csv.selections.txt",
                  sep="\t",header=T)
both1 = read.table("/Users/kprovost/Documents/TweetyNet/testing_wcs/Zonotrichia_leucophrys_train/BLB218.wav_0-141906.Wave_prep_210603_101417_model_transfer_BOTHZC_to_Zonotrichia.annot.csv.selections.txt",
                        sep="\t",header=T)
trans = read.table("/Users/kprovost/Documents/TweetyNet/testing_wcs/Zonotrichia_leucophrys_train/BLB218.wav_0-141906.Wave_prep_210602_180024_model_transfer_Cardinalis_to_Zonotrichia.annot.csv.selections.txt",
                        sep="\t",header=T)

both2 = read.table("/Users/kprovost/Documents/TweetyNet/testing_wcs/Multiple_train/BLB218.wav_0-141906.Wave_prep_210603_102012_model_BOTHZC.annot.csv.selections.txt",
                   sep="\t",header=T)


plot(0,ylim=c(1,5),xlim=c(0,2),yaxt="n",main="Zonotrichia leucophrys BLB218 section 1",ylab="")
axis(2,at=c(1:5),labels=c("both2","both1","trans","diff","true"),las=2)
segments(true$Begin.Time..s.,rep(5,nrow(true)),true$End.Time..s.,rep(5,nrow(true)),lwd=5)
segments(diff$Begin.Time..s.,rep(4,nrow(diff)),diff$End.Time..s.,rep(4,nrow(diff)),lwd=5,col="red")
segments(trans$Begin.Time..s.,rep(3,nrow(trans)),trans$End.Time..s.,rep(3,nrow(trans)),lwd=5,col="blue")
segments(both1$Begin.Time..s.,rep(2,nrow(both1)),both1$End.Time..s.,rep(2,nrow(both1)),lwd=5,col="green")
segments(both2$Begin.Time..s.,rep(1,nrow(both2)),both2$End.Time..s.,rep(1,nrow(both2)),lwd=5,col="goldenrod")
abline(v=c(true$Begin.Time..s.),col="black",lty=3)
abline(v=c(true$End.Time..s.),col="black",lty=3)


## cardinalis

true = read.table("/Users/kprovost/Documents/TweetyNet/testing_wcs/Cardinalis_cardinalis_train/Wave/Cardinalis_cardinalis.BLB27.wav_0-216027.Table.1.selections.txt",
                  sep="\t",header=T)
same=read.table("/Users/kprovost/Documents/TweetyNet/testing_wcs/Cardinalis_cardinalis_train/Cardinalis_cardinalis.BLB27.wav_0-216027.Wave_prep_210602_151853_model_Cardinalis_cardinalis.annot.csv.selections.txt",
                sep="\t",header=T)
diff = read.table("/Users/kprovost/Documents/TweetyNet/testing_wcs/Cardinalis_cardinalis_train/Cardinalis_cardinalis.BLB27.wav_0-216027.Wave_prep_210602_165944_model_Zonotrichia_leucophrys.annot.csv.selections.txt",
                  sep="\t",header=T)
both1 = read.table("/Users/kprovost/Documents/TweetyNet/testing_wcs/Cardinalis_cardinalis_train/Cardinalis_cardinalis.BLB27.wav_0-216027.Wave_prep_210603_101330_model_transfer_BOTHZC_to_Cardinalis.annot.csv.selections.txt",
                   sep="\t",header=T)
trans = read.table("/Users/kprovost/Documents/TweetyNet/testing_wcs/Cardinalis_cardinalis_train/Cardinalis_cardinalis.BLB27.wav_0-216027.Wave_prep_210602_175839_model_transfer_Zonotrichia_to_Cardinalis_.annot.csv.selections.txt",
                   sep="\t",header=T)

both2 = read.table("/Users/kprovost/Documents/TweetyNet/testing_wcs/Multiple_train/Cardinalis_cardinalis.BLB27.wav_0-216027.Wave_prep_210603_102012_model_BOTHZC.annot.csv.selections.txt",
                   sep="\t",header=T)


plot(0,ylim=c(1,6),xlim=c(0,2),yaxt="n",main="Cardinalis cardinalis BLB27 section 1",ylab="")
axis(2,at=c(1:6),labels=c("both2","both1","trans","diff","same","true"),las=2)
segments(true$Begin.Time..s.,rep(6,nrow(true)),true$End.Time..s.,rep(6,nrow(true)),lwd=5)
segments(same$Begin.Time..s.,rep(5,nrow(same)),same$End.Time..s.,rep(5,nrow(same)),lwd=5,col="grey")
segments(diff$Begin.Time..s.,rep(4,nrow(diff)),diff$End.Time..s.,rep(4,nrow(diff)),lwd=5,col="red")
segments(trans$Begin.Time..s.,rep(3,nrow(trans)),trans$End.Time..s.,rep(3,nrow(trans)),lwd=5,col="blue")
segments(both1$Begin.Time..s.,rep(2,nrow(both1)),both1$End.Time..s.,rep(2,nrow(both1)),lwd=5,col="green")
segments(both2$Begin.Time..s.,rep(1,nrow(both2)),both2$End.Time..s.,rep(1,nrow(both2)),lwd=5,col="goldenrod")
abline(v=c(true$Begin.Time..s.),col="black",lty=3)
abline(v=c(true$End.Time..s.),col="black",lty=3)


