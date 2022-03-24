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
#Q3- CI95 is the variance on a 95% confidence intervals, 
#Q4- Two assumptions made 
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


