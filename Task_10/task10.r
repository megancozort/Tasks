library(phytools)
trees<- list()
births<- vector()
Fractions<- vector()
netdiversification<- vector()
speciationrate<- vector()
Avgbranchlength<- vector()

pbtree
for (i in 1:100) {
 births [i]<- runif(1,0,1)
 Fractions [i]<- runif(1,0,1)
 trees[[i]]<- pbtree(n=100, b= births [i], d=Fractions [i] * births[i])
 netdiversification[i]<- (births [i] -Fractions[i]* births[i]) 
 speciationrate[i]<- births[i] 
 Avgbranchlength[[i]] <- mean(trees[[i]] $edge.length)
 
}
pbtree
#Question 4
netdiversification <- (births [i] -Fractions[i]* births[i])
totaltreetips <-log(sapply(trees, Ntip))
#2 tips
plot(netdiversification, totaltreetips)
line<- lm(totaltreetips ~ netdiversification)
abline(line) 
setwd('/Users/megancozort/Desktop/Evolution/Tasks/Task_10')
pdf("Question 4 netdiversification vs totaltreetips.pdf")
plot(netdiversification, totaltreetips)
line<- lm(totaltreetips ~ netdiversification)
abline(line) 
dev.off() 
#As netdiversification increases the log of total tips increases 
#Question 5
plot(speciationrate, Avgbranchlength)
setwd('/Users/megancozort/Desktop/Evolution/Tasks/Task_10')
pdf("Question 5 speciationrate vs Avgbranchlength.pdf")
plot(speciationrate, Avgbranchlength)
dev.off()
#As speciation rate increases, the average branch length decreases 
#Question 6
M<- cor(speciationrate, Avgbranchlength)
#-0.4782202
#Question 7
trees
trees[5]
TreeM<- trees[[5]]
rates<- vector()
traits <- list()
plot(TreeM)
setwd('/Users/megancozort/Desktop/Evolution/Tasks/Task_10')
pdf("Question 7 TreeM.pdf")
plot(TreeM)
dev.off()

for (i in 1:100) {
  rates[i]<- runif(1)
  traits[[i]]<- fastBM(TreeM, sig2=rates[i])
}
#Question 8
?cor
head(traits)
length(traits)
#100
rates
sapply(traits,mean)
mTraitsM<- sapply(traits,mean)
cor(mTraitsM, rates)
#0.1121604
plot(mTraitsM, rates)
setwd('/Users/megancozort/Desktop/Evolution/Tasks/Task_10')
pdf("Question 8 mTraitsM vs rates.pdf")
plot(mTraitsM, rates)
dev.off()
#Theres essentially no correlation between mean of traits and rates 
#Question 9
sapply(traits, var)
vTraitsM<- sapply(traits, var)
A<- cor(vTraitsM, rates)
#0.7149481
plot(vTraitsM, rates)
setwd('/Users/megancozort/Desktop/Evolution/Tasks/Task_10')
pdf("Question 9.pdf")
plot(vTraitsM, rates)
dev.off()
#Theres a positive correlation between variance of traits and rates 
#Question 10
element1<- sapply(traits, "[[", 1)
element2<- sapply(traits, "[[", 2)
TraitMat <-cbind(element1, element2)
K<-cor(element1, element2)
#0.1985199 
plot(element1, element2)
setwd('/Users/megancozort/Desktop/Evolution/Tasks/Task_10')
pdf("Question 10.pdf")
plot(element1, element2)
dev.off()
#Between the first element of traits and second elements of traits there 
#is a positive correlation. 
#This slight correlation could be due to the relatedness in the phylogeny tree. 
#Even though its positively correlated, its not significant because 
#The trait elements are generated randomly each time and each
#time there is a positive correlation, making it not significant. 

#extra credit 
?phylomorphospace
TreeZ<- pbtree(n=100)
X <- fastBM(TreeZ, nsim=2)
phylomorphospace(TreeZ, X, xlab="element1", ylab="element2")
setwd('/Users/megancozort/Desktop/Evolution/Tasks/Task_10')
pdf("EC.pdf")
phylomorphospace(TreeZ, X, xlab="element1", ylab="element2")
dev.off()


