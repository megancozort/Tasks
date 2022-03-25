setwd('/Users/megancozort/Desktop/Evolution/Tasks/Task_08')
library(phytools)
tree<- force.ultrametric(read.tree("https://jonsmitchell.com/data/anolis.tre"))
setwd('/Users/megancozort/Desktop/Evolution/Tasks/Task_09')
plot(tree, type="fan")
Ntip(AnolisTree)
tree$edge.length
#Q1-This tree has 82 tips, and the branch lengths are present theres 162 branches
data<- read.csv("https://jonsmitchell.com/data/svl.csv", stringsAsFactors=F, 
row.names=1)
data
#Q2-Data is a list of species of lizards names,and their corresponding svls
svl<- setNames(data$svl, rownames(data))
svl
Ancestors<- fastAnc(tree, svl, vars=TRUE, CI=TRUE)
head(Ancestors)
?fastAnc
#Q3- Estimated values stored in the nodes
#CI95 is the variance on a 95% confidence intervals, 
#Q4- Two assumptions made for fastAnc is that the estimates from the data falls 
#within the 95% confidence interval and that state compound for root node equals 
#the MLE of the root node 
par(mar=c(0.1,0.1,0.1,0.1))
plot(tree, type="fan", lwd=2, show.tip.label=F)
tiplabels(pch=16, cex=0.25*svl[tree$tip.label])
nodelabels(pch=16, cex=0.25*svl[tree$tip.label])
obj<- contMap(tree, svl, plot=F)
plot(obj, type="fan", legend=0.7*max(nodeHeights(tree)), sig=2, fsize=c(0.7, 0.9))
fossilData<- data.frame(svl=log(c(25.4, 23.2, 17.7, 19.7, 24, 31)), tip1=c(
  "Anolis_aliniger", "Anolis_aliniger", "Anolis_occultus", "Anolis_ricordii", 
  "Anolis_cristatellus", "Anolis_occultus"), tip2=c("Anolis_chlorocyanus", "Anolis_coelestinus",
                                                    "Anolis_hendersoni", "Anolis_cybotes", "Anolis_angusticeps", "Anolis_angusticeps"))
#Q5_
fossilNodes<-c()
nodeN<- c()
head(fossilData)
for (i in 1:6) {
  Node<- fastMRCA(tree, fossilData[i, "tip1"], fossilData[i, "tip2"])
  fossilNodes [i]<- fossilData[i, "svl"]
  nodeN[i] <- Node 

}
i
names(fossilNodes)<- nodeN
Ancestors_withFossils<- fastAnc(tree, svl, anc.states=fossilNodes,
                                CI=TRUE, var=TRUE)


#Q7- 
head(Ancestors)
head(Ancestors_withFossils)
Ancestors_withFossils
Ancestors_withoutFossils <- fastAnc(tree, svl, CI=TRUE, var=TRUE)
Ancestors_withoutFossils
plot(Ancestors_withFossils$ace, Ancestors_withoutFossils$ace, xlab='with fossils'
     , ylab= 'without fossils')
#The plot increased the overall ancestral size 
     

#Q8-
install.packages("geiger")
library(geiger)
lambdaFit<- fitContinuous(tree, data, model="lambda")
#AIC= -4.51
brownianFit<- fitContinuous(tree, data, model="BM")
#AIC= -6.51
ouFit<- fitContinuous(tree, data, model="OU")
#AIC=-4.51
ebFit<- fitContinuous(tree, data, model="EB")
#AIC= -7.24
ratetrendFit<- fitContinuous(tree, data, model="rate_trend")
#AIC= -6.98 
kappaFit<- fitContinuous(tree, data, model="kappa")
#AIC= -4.51
deltaFit<- fitContinuous(tree, data, model="delta")
#AIC= -6.11
meantrendFit <- fitContinuous( tree, data, model="mean_trend")
#AIC= -4.51
whiteFit<- fitContinuous(tree, data, model="white")
#AIC= 91.39 
# Model ebFit is the best fit model for the data becuase it has the lowest AIC
#FastAnc performs fast estimation of ML ancestral states while fitContinuous 
#compares continuous comparative data. Both assume continous traits and 
#and have the same confidence intervals. 


