trueMean1<- 5
trueSD1<-5
population1<- rnorm(1e6, trueMean1, trueSD1)
head(population1)
trueMean2<- 4
trueSD2<- 5
population2<- rnorm(1e6, trueMean2, trueSD2)
head(population2)
Size<- 50
Sample1<- sample(population1, Size)
Sample2<- sample(population2, Size)
head(Sample1)
head(Sample2)
#Yes, the samples and populations were different
boxplot(Sample1, Sample2)
plot(Sample1, Sample2)
source("http://jonsmitchell.com/code/simFxn04.R")
MatGrandma<- makeFounder("grandma_mom")
head(MatGrandma)
nrow(MatGrandma)
MatGrandpa<- makeFounder("grandpa_mom")
head(MatGrandpa)
nrow(MatGrandpa)
PatGrandma<- makeFounder("grandma_da")
head(PatGrandma)
nrow(PatGrandma)
PatGrandpa<- makeFounder("grandpa_da")
head(PatGrandpa)
nrow(PatGrandpa)
Alan<- makeBaby(PatGrandma, PatGrandpa)
head(Alan)
nrow(Alan)
Brenda<- makeBaby(MatGrandma, MatGrandpa)
head(Brenda)
Focus<- makeBaby(Brenda, Alan)
head(Focus)
#The number should be 50%, 1/2 from mom 1/2 from dad.
ToMom<- length( grep("mom", Focus))/ length( Focus)
head(ToMom)
#The number matched with grandparents should be 25%, no the true number doesnt match
ToMomMom<- length( grep("grandma_mom", Focus))/ length(Focus)
head(ToMomMom)
ToMomDad<-length( grep("grandpa_mom", Focus))/length(Focus)
head(ToMomDad)
ToDad<- length(grep("da", Focus))/ length(Focus)
head(ToDad)
ToDadMom<- length(grep("grandma_da", Focus))/ length(Focus)
head(ToDadMom)
ToDadDad<- length( grep("grandpa_da", Focus))/ length( Focus)
head(ToDadDad)
#NO, the Focus is not equally related to each maternal and paternal grandparent
#however, the average relatedness was around 25% 
#Therefore, it didnt match what I expected
#The average relatedness to all 4 grandparents was 25%
Sibling_01<- makeBaby(Brenda, Alan)
#I expect the sibling will share 50% DNA with Focus, when I do the function 
#I am getting that 66% is shared
ToSib<- length( intersect (Focus, Sibling_01))/ length( Focus)
ManySiblings<- replicate(1e3, length( intersect(Focus, makeBaby(Brenda, Alan)))/ length(Focus))
head(ToSib)
head(ManySiblings)
#Most often siblings will share 50% of their genes, however
#this can vary which is shown through this simulation
quantile(ManySiblings)
mean(ManySiblings)
plot(density(ManySiblings), main="", xlab="proportion shared genes")
head(plot)
#Theres a range of values because it representing the portion of genes shared 
#which can vary depending of the relatedness of each individual, the density 
#also changes as the relatedness between individuals changes. Could be due to 
#genetic recombination.
HWE<- function(p) {
  aa<- p^2
  ab<- 2* p * (1-p)
  bb<- (1-p)^2
  return(c(aa=aa, ab=ab, bb=bb))
}
HWE(0.5)
plot(1, 1, type="n", xlim=c(0,1), ylim=c(0,1), xlab="freq. allele a", ylab="geno.freq")
p<- seq(from=0, to=1, by=0.01)
head(p)
GenoFreq<- t(sapply(p, HWE))
GenoFreq
lines(p, GenoFreq[, "aa"], lwd=2, col="red")
#The geno freq of the aa individuals increases as the freq of allele a increase
#As the freq of allele aa decreases the geno freq also decreases
#No, there is no time or geographic space shown on this plot
lines(p, GenoFreq[, "ab"], lwd=2, col="purple")
lines(p, GenoFreq[, "bb"], lwd=2, col="blue")
legend("top", legend=c("aa", "ab", "bb"), col=c("red", "purple", "blue"), 
       lty=1, lwd=2, bty="n")
Pop<- simPop(500)
Pop
points(Pop[,"freqa"], Pop[,"Genotypes.aa"]/500, pch=21, bg="red")
#The frequency of the genotype is close to Hardy-Weignberg but not exact
Pop<- simPop(50)
Pop
points(Pop[, "freqa"], Pop[, "Genotypes.aa"]/ 50, pch=22, bg="red")
#The smaller population varies more than the larger, its more spread out
#and there's more deviation compared to larger
install.packages("learnPopGen")
library(learnPopGen)
x<- genetic.drift(Ne=200, nrep=5, pause=0.01)
x
# When Ne=100 p decreases and time increases,when Ne=200 for the majority
#as time increases p decreases, however theres one outliar where as time
#increases frequency increases
#when Ne=20 for the majority as time increases p increases, 
#however there are two outlairs 
#where as time increases p decreases
PopSizes<- 5:50
Samples<- rep(PopSizes, 5)
tExt<-sapply(Samples, function(x) nrow(simPop(x,500)))
tExt
Line<- lm(tExt ~ Samples)
Line
summary(Line)
Line$coef
plot(Samples, tExt)
abline(Line)
Line2<- lm(tExt~Samples +0)
Line2
abline(Line2)
#The line is a little above line 2, the +0 means only sample no intercept
#As the population size increases the points become more spread out and 
# further away from the line. This most likely means theres more variation
#in the data and more outliars. 
#One way to detect heteroskedasticity is by creating a residual plot
install.packages("MASS")
library(MASS)
LineA<- rlm(tExt ~ Samples)
coef(LineA)
Line4<- lm(tExt ~ Samples)
coef(Line4)
abline(Line4)
abline(LineA)
summary(LineA)
#The slope for lineA, which is the line fixed for heteroskedasticity is less with 
# a value of 2.31 which is 
# less than Line4 which had a slop of 2.74 and showed heteroskedasticity. 