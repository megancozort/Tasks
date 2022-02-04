setwd("/Users/megancozort/Desktop/Evolution/Tasks/Task_04")
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
#The average Chi sq was 61, meaning the bars were not very even
#Observed and expected differ by a lot
#The avg chi sq is larger than the critical value
Avg<- mean(Chisqs)
Avg
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
#The red and black background seem to exptend the most meaning they're more
#to the right of the critical value number therefore more sig trials
Simulation<- simDraws(10000)
addHist(Y=7, Dat=Simulation, Color="lightgray")
mtext(side=2, at=7, line=0, "simulated")
abline(v= 11.70, lty=2, lwd=2)
#95% of time
#Selection 
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
#The mixture has more peaks and a more narrow tail compared to the student 
#generated data. Yes, most students show high selection but not as high as the 
#mixture simulation. Selection was relatively high across groups
#Inference
#Evolution by natural selection
#natural selection
#the graph suggest that evolutionary processes the graph stimulated is 
#relatively strong 
#looks like high selection 
#comparing the student numbers to the stimulated numbers was better 
#than a single critical value 
#if a toothpick mutated to a different type, that would increase the x2 values
#because increasing mutation/ difference is going to increase the uneveness
#of the bars on the graph

#Extra credit
function(Popsize=100, nGeneration=100, h=1, s=0, initial_p=0.5, mu=1, twoway=TRUE)
  if (is.null(w))
simPop
