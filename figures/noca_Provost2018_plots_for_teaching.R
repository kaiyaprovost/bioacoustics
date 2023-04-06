data = "/Users/kprovost/OneDrive - The Ohio State University/noca_test_boxplots.txt"
df = read.table(data,header=T,sep="\t",stringsAsFactors = F)

levels(df$Song.Type.Played) = c("Cactus Wren Song","Other Desert","Far-Away in Same Desert","Local Population")
levels(df$Desert.Population.Tested) = c("Sonoran Desert","Chihuahuan Desert")

df$Song.Type.Played = factor(df$Song.Type.Played,levels=c("Cactus Wren Song","Other Desert","Far-Away in Same Desert","Local Population"))
df$Desert.Population.Tested = factor(df$Desert.Population.Tested,levels=c("Sonoran Desert","Chihuahuan Desert"))

pdf("noca_for_lecture.pdf")
par(bg="black",fg="white",col.axis="white",col.main="white",
    col.sub="white",col.lab="white")

boxplot(df$Distance.to.Speaker.After.Trial~df$Song.Type.Played+df$Desert.Population.Tested,
        las=2,col=c(rep("green",4),rep("cyan",4)),xaxt="n",
        ylab="Distance to Speaker After Trial (m)",xlab="")
labels=c("Wren-C","Across-C","Distant-C","Local-C",
         "Wren-S","Across-S","Distant-S","Local-S")
axis(1,at=1:8,labels=labels,las=2)

boxplot(df$Attacks.on.Speaker.After.Trial~df$Song.Type.Played+df$Desert.Population.Tested,
        las=2,col=c(rep("green",4),rep("cyan",4)),xaxt="n",
        ylab="Attacks on Speaker After Trial",xlab="")
labels=c("Wren-C","Across-C","Distant-C","Local-C",
         "Wren-S","Across-S","Distant-S","Local-S")
axis(1,at=1:8,labels=labels,las=2)

boxplot(df$Songs.Sung.After.Trial~df$Song.Type.Played+df$Desert.Population.Tested,
        las=2,col=c(rep("green",4),rep("cyan",4)),xaxt="n",
        ylab="Songs Sung After Trial",xlab="")
labels=c("Wren-C","Across-C","Distant-C","Local-C",
         "Wren-S","Across-S","Distant-S","Local-S")
axis(1,at=1:8,labels=labels,las=2)

boxplot(df$Aggression.Index.After.Trial~df$Song.Type.Played+df$Desert.Population.Tested,
        las=2,col=c(rep("green",4),rep("cyan",4)),xaxt="n",
        ylab="Aggression Index After Trial",xlab="")
labels=c("Wren-C","Across-C","Distant-C","Local-C",
         "Wren-S","Across-S","Distant-S","Local-S")
axis(1,at=1:8,labels=labels,las=2)


df$Distance.Difference = df$Distance.to.Speaker.After.Trial-df$Distance.to.Speaker.Before.Trial
df$Attacks.Difference = df$Attacks.on.Speaker.After.Trial-df$Attacks.on.Speaker.Before.Trial
df$Songs.Difference = df$Songs.Sung.After.Trial-df$Songs.Sung.Before.Trial
df$Aggression.Difference = df$Aggression.Index.After.Trial-df$Aggression.Index.Before.Trial



par(bg="black",fg="white",col.axis="white",col.main="white",
    col.sub="white",col.lab="white")

boxplot(df$Distance.Difference~df$Song.Type.Played+df$Desert.Population.Tested,
        las=2,col=c(rep("cyan",4),rep("green",4)),xaxt="n",
        ylab="Distance to Speaker Change Between Trials",xlab="")
labels=c("Wren-C","Across-C","Distant-C","Local-C",
         "Wren-S","Across-S","Distant-S","Local-S")
abline(h=0,lty=3)
axis(1,at=1:8,labels=labels,las=2)

boxplot(df$Attacks.Difference~df$Song.Type.Played+df$Desert.Population.Tested,
        las=2,col=c(rep("cyan",4),rep("green",4)),xaxt="n",
        ylab="Attacks on Speaker Change Between Trials",xlab="")
labels=c("Wren-C","Across-C","Distant-C","Local-C",
         "Wren-S","Across-S","Distant-S","Local-S")
abline(h=0,lty=3)
axis(1,at=1:8,labels=labels,las=2)

boxplot(df$Songs.Difference~df$Song.Type.Played+df$Desert.Population.Tested,
        las=2,col=c(rep("cyan",4),rep("green",4)),xaxt="n",
        ylab="Songs Sung Change Between Trials",xlab="")
labels=c("Wren-C","Across-C","Distant-C","Local-C",
         "Wren-S","Across-S","Distant-S","Local-S")
abline(h=0,lty=3)
axis(1,at=1:8,labels=labels,las=2)

boxplot(df$Aggression.Difference~df$Song.Type.Played+df$Desert.Population.Tested,
        las=2,col=c(rep("green",4),rep("cyan",4)),xaxt="n",
        ylab="Aggression Index Change Between Trials",xlab="")
labels=c("Wren-C","Across-C","Distant-C","Local-C",
         "Wren-S","Across-S","Distant-S","Local-S")
abline(h=0,lty=3)
axis(1,at=1:8,labels=labels,las=2)
dev.off()
