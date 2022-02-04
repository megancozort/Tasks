setwd("~/Desktop/Evolution/Tasks/Task_04T")
results<- read.csv("http://jonsmitchell.com/data/biol112labresults.csv", 
                   stringsAsFactors=F)
write.csv(Data, 'rawdata.csv', quote=F)
results
counts<- results[,c("yellow", "red", "green", "blue", "black", "tan")]
counts
backgrounds<- c("White", "Red", "Yellow", "Green", "Blue", "Black")
backgrounds
backgroundCol<- c("white", "#d53e4f", "#fee08b", "#abdda4", "#3288bd", "black")
calcChi(counts[1,])
#55.2
Chisqs<- apply(counts, 1, calcChi)
Chisqs
plotChis(counts)
#When the chi square value is high the bars are very uneven, the lower the number
#the more even all the bars become. The plotChis shows you to assume
#the bigger your chi sq value is the more uneven your bars are
mean(Chisqs)
#The average Chi sq was 61, HOW WOULD YOU INTERPRET
Avg<- mean(Chisqs)
Avg
# IN WHAT PACKET THE ONE ON ECAMPUS?
#Yes, the value differs by the background 
backgroundAvgs<- tapply(Chisqs, results[,3], mean)
backgroundAvgs
propSig<- length(which( Chisqs >11.70)/ length(Chisqs))
propSig
percSig<- round(100 * propSig)
percSig
#Very surprised I got 50,000 that seems very high
#No, I could imagine the freshmen didn't properly follow the directions exactly
#making for some variation in the results besides just natural selection
par(las= 1, mar=c(4,4,1,1), mgp=c(2, 0.5, 0), tck= -0.01, cex.axis=1)
hist(Chisqs, main="", xlab="chi-squared values", ylab="frequency")
par(las=1, mar=c(4,4,1,1), mgp=c(2,0.5,0), tck=-0.01, cex.axis=1)
plot(1,1, xlim=c(0, 400), ylim=c(1, 8.5), xlab="", ylab="", type="n", yaxt="n")
axis(2, at=1:length (backgrounds), labels=backgrounds)
mtext(side=1, expression(chi^2), cex=1.75, line=2.5)
counter<- 1
for( i in backgrounds) {
  Data<- Chisqs[which(results[,3]==i)]
  addHist(Y=counter, Dat=Data, Color=backgroundCol[counter])
  counter<- counter + 1
}
abline(v=11.70, lty=2, lwd=2, col='black')
#No, it seems like the black line is pretty straight and there's no meaningful
#difference between the backgrounds
Simulation<- simDraws(10000)
addHist(Y=7, Dat=Simulation, Color="lightgray")
mtext(side=2, at=7, line=0, "simulated")
abline(v= 11.70, lty=2, lwd=2)
#ANSWER LINE 247
#ANSWER LINE 250
Fit<- c(1,1,1,1,1,1)
names(Fit)<- 1:6
Simulation2<- simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation2, Color=rgb(0,0,0,0.25))
Fit<- c(0.1,1,1,1,1,1)
names(Fit)<- 1:6
Simulation3<- simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation3, Color=rgb(0,0,0,0.25))
Fit<- c(0.5, 0.6, 0.7, 1,1,1)
names(Fit)<- 1:6
Simulation4<- simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation4, Color=rgb(0,0,0,0.25))
Fit<-c(0.1,0.2,0.3,0.4,0.5,1)
names(Fit)<- 1:6
Simulation5<- simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation5, Color=rgb(0,0,0,0.25))
Fit<-c(0.1,0.1,0.1,0.1,0.1,1)
names(Fit)<- 1:6
Simulation6<-simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation6, Color=rgb(0,0,0,0.25))
mtext(side=2, at=8, line=0, "sel.sim.")
Simulation7<- c(Simulation2, Simulation3, Simulation4, Simulation5, Simulation6)
addHist(Y=8, Dat=Simulation7, Color=rgb(0,0,1,0.25))
#ANSWER QUESTION LINE 297
#Evolution by selection
#I HAVE NO IDEA
#IDK