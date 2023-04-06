## playing with freq_ts code
library(warbleR)
date=format(Sys.time(), "%d%b%Y")
which_selection=1
path="/Users/kprovost/Documents/Postdoc_Working/Sounds_and_Annotations/Aves/SUBSETS/"
setwd(path)
fctsfile=paste("rvn.dat_trimmed_fcts_",date,".txt",sep="")
rvn.dat.fcts=read.table(fctsfile,header=T)
rvn.dat.fcts=rvn.dat.fcts[rvn.dat.fcts$sound.files=="Acanthis.flammea.XC.141708.wav_0-1082881.wav",]
rvn.dat.fcts = rvn.dat.fcts[order(as.numeric(rvn.dat.fcts$selec)),]
r <- warbleR::read_wave(X = rvn.dat.fcts, path = path, index = which_selection)
samples = length(r@left)

songdur=rvn.dat.fcts$end[which_selection]-rvn.dat.fcts$start[which_selection]
freq_bound=c(rvn.dat.fcts$bottom.freq[which_selection],rvn.dat.fcts$top.freq[which_selection])
#par(mfrow=c(2,1))
#plot(r)
spec=spectro(r, wl = 512, grid = FALSE, scale = FALSE, ovlp = 0,flim=freq_bound)
## can i figure out  how much overlap raven has when it calculates these things? 
print(length(spec$time))
print(songdur)


dur_sec=rvn.dat.fcts$end[i]-rvn.dat.fcts$start[i]
dur_samp = seconds_to_samples(seconds=dur_sec,samplerate=48000)
length_out =  round(dur_samp / (512 * (50/100)),0)-1
freq=freq_ts(rvn.dat.fcts[which_selection,c("selec","start","end","bottom.freq","top.freq","sound.files")],
             img=F,ff.method = "tuneR",raw.contour = T,type="fundamental",pb=F,
             length.out=length_out,wl=512,
             bp=c(freq_bound),clip.edges = F,threshold = 50,ovlp=50)
points(y=as.numeric(freq[,-c(1:2)]),x=seq(0,songdur,length.out=length_out),type="b")

## 6 ## duration 0.59458, length 111 ## overlap ~50  -- 257.1171 samples per 
#actual=c(2718.750,3656.250,3656.250,3562.500,3187.500,3093.750,3843.750,3375.000,3656.250,3375.000,5250.000,5718.750,5625.000,5812.500,6000.000,6000.000,4781.250,5156.250,3750.000,3656.250,3562.500,3281.250,3093.750,3843.750,3562.500,3656.250,3468.750,5906.250,6000.000,6000.000,6000.000,6000.000,6000.000,4218.750,5156.250,3656.250,3468.750,3656.250,3187.500,3093.750,3937.500,3468.750,3562.500,3281.250,2812.500,2906.250,5812.500,5906.250,5906.250,5906.250,6000.000,5906.250,4781.250,3656.250,3656.250,3468.750,3281.250,3093.750,4312.500,3656.250,3187.500,3562.500,5437.500,5343.750,5062.500,5812.500,5250.000,5343.750,6000.000,4312.500,5062.500,5250.000,3562.500,3375.000,3562.500,3187.500,4312.500,3562.500,3187.500,3468.750,3562.500,4875.000,5625.000,2812.500,5531.250,5531.250,5625.000,5531.250,4687.500,3750.000,3750.000,3562.500,3468.750,3375.000,3093.750,3843.750,3562.500,3562.500,2718.750,4875.000,4593.750,4781.250,2718.750,5343.750,5531.250,5625.000,5718.750,3843.750,3656.250,3562.500,3562.500)

## 5 ## duration 0.5245, length 97 ## overlap ~50 -- 259.5464 samples per 
#actual=c(3468.750,3468.750,3562.500,3562.500,5812.500,5812.500,4500.000,4312.500,4218.750,4031.250,3843.750,4593.750,4500.000,4593.750,4125.000,3656.250,3843.750,4500.000,4500.000,4312.500,3937.500,4218.750,4406.250,4500.000,4406.250,4312.500,3843.750,4406.250,4500.000,4500.000,4312.500,4125.000,4218.750,5906.250,4500.000,4406.250,4312.500,4312.500,4218.750,4500.000,4406.250,4406.250,4218.750,4406.250,6093.750,4500.000,4312.500,4312.500,4125.000,4500.000,6093.750,4406.250,4125.000,4218.750,4875.000,5625.000,4406.250,4500.000,4406.250,4125.000,4218.750,4218.750,4406.250,4218.750,4406.250,4031.250,4218.750,6000.000,4500.000,4312.500,4312.500,4312.500,4312.500,5906.250,4218.750,4312.500,4218.750,3843.750,5531.250,4312.500,4406.250,4406.250,4312.500,4312.500,4312.500,5437.500,4218.750,4125.000,4031.250,4031.250,5250.000,4312.500,4218.750,4125.000,4406.250,4218.750,4125.000)

## 1 ## duration 0.1762498, length 32 ## overlap ~50 -- 264.375 samples per 
actual=c(4312.500,3843.750,3937.500,4031.250,4031.250,4125.000,4218.750,4218.750,4218.750,4218.750,4218.750,4218.750,4218.750,4218.750,4218.750,4218.750,4218.750,4406.250,4406.250,4406.250,3000.000,3093.750,3093.750,3187.500,3281.250,3375.000,3468.750,3562.500,3562.500,3562.500,3656.250,3562.500)
points(x=seq(0,songdur,length.out=length(actual)),y=actual/1000,col="blue",type="b",pch=2)
#print(samples/length(actual))


par(mfrow=c(1,1))
thresholds=seq(5,95,5)
overlaps=50
#thresh_lengths=c()
thresh_ovlp_len=c()
t_vals = c()
o_vals = c()
for(i in thresholds){
  for(j in overlaps){
  ## overlap does not matter for this statistic?
    print(paste(i,j,sep=" "))
    t_vals = c(t_vals,i)
    o_vals = c(o_vals,j)
  freq=freq_ts(rvn.dat.fcts[which_selection,c("selec","start","end","bottom.freq","top.freq","sound.files")],
               img=F,ff.method = "tuneR",raw.contour = T,type="fundamental",pb=T,
               bp=c(0.5,10),clip.edges = F,threshold = i,ovlp=j)
  print(freq)
  thresh_ovlp_len=c(thresh_ovlp_len,length(freq)-2)
  }
}
plot(thresh_ovlp_len,col=as.numeric(as.factor(t_vals)),pch=as.numeric(as.factor(o_vals)))
abline(h=length(actual))
thresh_ovlp_len[which(thresh_ovlp_len==length(actual))]
cbind(t_vals,o_vals,thresh_ovlp_len)

plot(thresholds,thresh_ovlp_len,col=(thresh_ovlp_len==length(actual))+1)
print(thresholds[which(thresh_lengths==length(actual))])






par(mfrow=c(1,1))
x=c(0.59458,0.5248,0.1762498)
y=c(111,97,32)
plot(x,y)
summary(lm(y~x))


par(mfrow=c(1,1))
amps=spec$amp
colnames(amps) = spec$time
rownames(amps) = spec$freq
#corrplot::corrplot(amps,method="color",is.corr=F)

par(mfrow=c(1,1))
overlaps=seq(0,99,0.1)
ovlp_lengths=c()
for(i in overlaps){
  print(i)
  spec2=spectro(r, wl = 512, grid = FALSE, scale = FALSE, ovlp = i,flim=freq_bound,plot = F)
  ovlp_lengths=c(ovlp_lengths,length(spec2$time))
  
}
plot(overlaps,ovlp_lengths,col=(ovlp_lengths==length(actual))+1)
abline(h=length(actual))
print(overlaps[which(ovlp_lengths==length(actual))])




## for the acanthis, the relationship between duration in ms and the number of things is by ~0.18887151
## for the calypte, the relationship between duration in ms and the number of things is by ~0.187571
## card this is ~0.1874083
## this is 1/~5.3? 5.4? 

temp=read.table("/Users/kprovost/Dropbox (AMNH)/Postdoc_Backup/Cardinalis_cardinalis/Full Annotated Files/rvn.dat.ravenversion_11Feb2022.txt",
                header=T,sep=" ")
#temp$pcf = stringr::str_count(temp$Peak.Freq.Contour..Hz.,";")+1
temp$pcf = stringr::str_count(temp$Peak.Corr..U.,";")+1
#temp$dur = (temp$End.Time..s.-temp$Begin.Time..s.)*1000
temp$dur = (temp$end-temp$start)*1000
#plot(temp$dur,temp$pcf)
#summary(lm(temp$pcf~temp$dur))
plot(temp$dur[temp$pcf!=1],temp$pcf[temp$pcf!=1])
summary(lm(temp$pcf[temp$pcf!=1]~temp$dur[temp$pcf!=1]))

hist(temp$dur[temp$pcf!=1]/temp$pcf[temp$pcf!=1],breaks=40)
mean(temp$dur[temp$pcf!=1]/temp$pcf[temp$pcf!=1],na.rm=T)


## can we do this converted to samples?


