install.packages("phytools")
install.packages("coala")
install.packages("rehh", dep=T)
install.packages("assertthat", dep=T)
install.packages("RcppArmadillo", dep=T)
install.packages("https://cran.r-project.org/src/contrib/Archive/scrm/scrm_1.7.3-1.tar.gz"
                 , repos=NULL, type="source")
install.packages("https://cran.r-project.org/src/contrib/Archive/coala/coala_0.6.0.tar.gz"
                 , repos=NULL, type="source")
library(coala)
library(phytools)
library(learnPopGen)
coalescent.plot(5,10)
coalescent.plot(5,10)
coalescent.plot(5,10)
#Q1-Simulation 1, 2, and 3 had 5 different alleles, you could change the n 
#number of haploid individuals or gene copies 
#Q2-Around 6 generations is when fixation starts really setting in 
mean(c(4,6,8))
#Q3- On average each individual has one offspring 
var(c(4,6,8)) 
#the variance is 4
#Q4-Fitness doesn't play a role in these simulation because they're random
#Q5- No, not alive at generation 0. Theres a closer common ansestor 
model<- coal_model(sample_size=5 , loci_number=10, loci_length=500, 
                   ploidy=2) +
  feat_mutation(10) +
  feat_recombination(10) +
  sumstat_trees () +
  sumstat_nucleotide_div ()
head(model)
stats<-simulate(model, nsim=1)
stats
Diversity<- stats$pi
head(Diversity)
#No, all the numbers are not the same. The difference could come from
#recombination
Nloci<- length(stats$trees)
t1<- read.tree(text=stats$trees[[1]][1])
plot(t1)
axisPhylo()
#Q6-Because each individual has two copies of the alleles 
Age1<- max(nodeHeights(t1))
head(Age1)
t2<- read.tree(text=stats$trees[[2]][1]) 
plot(t2)
axisPhylo ()
Age2<- max(nodeHeights(t2))
head(Age2)
#1.45, no the two trees do not date back to the same time 
par(mfrow=c (1,2))
plot(t1)
axisPhylo()
plot(t2)
axisPhylo()
compare.chronograms(t1, t2)
#Q7- no they do not match t2 was older than t1
t1_1<- read.tree(text=stats$trees [[1]][1])
t1_2<- read.tree(text=stats$trees[[1]][2])
compare.chronograms(t1_1, t1_2)
for (locus in 1:Nloci) {
  ntrees<-length(stats$trees[[locus]])
  for (n in 1:ntrees) {
    if (locus==1 && n==1) {
      outPhy<- read.tree(text=stats$trees[[locus]][n])
    }
    else {
      outPhy<- ape:::c.phylo(outPhy, read.tree(text=stats$trees[[locus]][n]))
    }
  }
}
par(mfrow=c (1,1))
densityTree(outPhy)    
#Model    
model3<- coal_model(10,50) +
  feat_mutation (par_prior ("theta", sample.int(100, 1))) +
  sumstat_nucleotide_div()
stats<- simulate(model3, nsim=40)
mean_pi<- sapply(stats, function(x)mean(x$pi))
theta<- sapply(stats, function(x) x$pars[["theta"]])
plot(mean_pi)  
plot(mean_pi, theta)
Line<- lm(mean_pi ~theta)
abline(Line)

#Extra Credit
?feat_mutation
?feat_size_change
activate_msms(jar=NULL, java=NULL, priority=200, download=TRUE)
modelk<- coal_model(c(12,22),6) +
  feat_size_change(.2, population=2, time="1") +
  feat_selection(strength_A= 400, population="all", time=0, locus_group="all") +
  feat_migration(0.5, 1,2) +
  feat_migration(1, 2, 2) +
  feat_growth(12, time=0) +
  feat_growth(5, time=1)
#This one didnt work

modelk<- coal_model(c(12,22),6) +
feat_size_change(.2, population=2, time="1") +
  feat_mutation(par_prior("theta", sample.int(100,1))) + 
  feat_migration(0.5, 1,2) +
  feat_migration(1, 2, 2) + 
  feat_growth(12, time=0) + 
  feat_growth(5, time=1) +
  feat_selection(strength_A = 0.8, population = 2, time=1, locus_group = "all") +
  feat_selection(strength_A = 1.2, population= 1, time =0, locus_group = "all") +
  sumstat_nucleotide_div()
#This one works   
  
  
  
  




check_model(modelk)
simulate (modelk, nsim=2)
Output<- simulate(modelk, nsim=100)
Pis<- sapply(Output, function(x) x$pi)
statsk <- simulate (modelk, nsim=2)
population <- statsk$pi
#This goes with model that works 
for(i in 1:100) {
  population1<- length(statsk$)
}