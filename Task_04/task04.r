source("http://jonsmitchell.com/code/fxn05.R")
#No, genetic drift can prevent it
Pop1 <- simPop (Popsize =50, nGenerations =100, initial_p= 0.5, h=1, s=0)
plot (1:nrow(Pop1), Pop1[,1], ylim=c(0, 1), type ="l", xlab= "generation",
      ylab= "allele freq.", lwd=2) 
lines(1:nrow(Pop1), Pop1[,2], lwd=2, col='red')
legend("topleft", legend=c ("a", "b"), col=c("black", "red"), lwd=2, bty="n")
plotFit( nruns=10, n=50, ngens=100, init_p= 0.5, h=1, s=0)
Expectation <- c(10,10,10,10)
Observed <-c(15,15,5,5)
#chi sq of 10
Chisq<-sum(((Expectation- Observed)^2)/ Expectation)
Chisq
barplot(rbind (Expectation, Observed), beside= T, main=bquote (chi^2 ~ "=" ~ .
  (Chisq)), legend.text=c ("expected", "observed"))
Observed <-c(5, 0, 0, 35)
#This gives a chi sq value of 85
Observed<- c(2,3,10,30)
#This gives a chi sq value of 51
Observed <-c(10,10,10,10)
#This gives a chi sq value of 0
#The smaller X^2 value is th more even the bar are
Observed<-c(40,0,0,0)
#When I set the observed to only one category I got a ch sq value larger than
#previous values meaning observed and expected are not even
#lower ch sq more even the bars are 
#with a chi sq of 0 all bars are even 