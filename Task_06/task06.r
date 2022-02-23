source("http://jonsmitchell.com/code/reformatData07.R")
source("http://jonsmitchell.com/code/simFxn.R")
overallFreq
plot(1, 1, type="n", xlim=c(1998, 2013), ylim=c(0,1))
s<- apply(overallFreq, 2, function(x) lines (overallFreq[,1], x, col=
                                               rgb(0,0,0,0,01)))
rescaleFreq<- apply(overallFreq[,3: ncol(overallFreq)], 2, function(x) 
  x - x[1]) 
plot(1,1, type="n", xlim=c(1998, 2013), ylim=c(-0.25, 0.25))
s<- apply(rescaleFreq, 2, function(x) lines(overallFreq[,1], x, col=
                                              rgb(0,0,0,0.01)))
dYear<- c()
dAlleles<- c()
for (i in 3:ncol(overallFreq)) {
  dYear<- c(dYear, overallFreq [,1])
  Vec<- overallFreq[,i]
  Init<- overallFreq[1,i]
  dAlleles<- c(dAlleles, Vec-Init)
}
smoothScatter(dYear, dAlleles, colramp= Pal, nbin=100)
smoothScatter(dYear, dAlleles, colramp= Pal, nbin=100, xlab="year", 
              ylab="change in allele freq. since 1998")
addFit (nruns= 50, n=100,ngens=18, startT=1997, simCol= "gray40", 
        rescale=TRUE)
plot(alleleFreqs$d_freq, alleleFreqs$d_immm , xlim=c(-0.15, 0.15), xlab="
     overall freq. change", ylab="freq. change in subset")
points(alleleFreqs$d_freq, alleleFreqs$d_birth, col='blue')
points(alleleFreqs$d_freq, alleleFreqs$d_surv, col='red')

#new graph 
plot(1,1, type="n", xlim=c(1998, 2013), ylim=c(-0.25, 0.25))
s<- apply(rescaleFreq, 2, function(x) lines(overallFreq[,1], x, col=
                                              rgb(0,0,0,0.01)))
smoothScatter(dYear, dAlleles, colramp= Pal, nbin=100)
smoothScatter(dYear, dAlleles, colramp= Pal, nbin=100, xlab="year", 
              ylab="change in allele freq. since 1998")
#what i changed to "fix" graph
addFit (nruns=100, n=1000, ngens=18, init_p= 0.5, h=1, s=0, startT=1997, simCol="gray40", rescale=TRUE)