library(phytools)
library(ape)
text.string<- 
  "(((((((cow,pig),whale), (bat,(lemur,human))), (robin, iguana)), coelacanth), 
(goldfish, trout)), shark) ;" 
vert.tree<- read.tree (text=text.string)
plot(vert.tree, edge.width=2)
nodelabels(frame="circle", bg='white', cex=1) 
#Question 1- Goldfish and shark are more closely related 
vert.tree
#Question 2- no there are no branch lengths 
str(vert.tree)
tree<-read.tree(text="(((A,B), (C,D)), E) ;")
plotTree(tree, offset=1)
tiplabels(frame="circle", bg='lightblue', cex=1)
nodelabels(frame="circle", bg='white', cex=1)
tree$tip.label
tree$edge
AnolisTree<- force.ultrametric(read.tree("https://jonsmitchell.com/data/anolis.tre"))
par(las=1)
hist(AnolisTree$edge.length, col='black', border='white', main="",  xlab="
     edge lengths for the Anolis tree", ylim=c(0,50), xlim=c(0,6))
tipEdges<- which(AnolisTree$edge [,2] <= Ntip (AnolisTree))
Lengths<- AnolisTree$edge.length
names(Lengths)<- AnolisTree$tip.label
names(Lengths)[which(Lengths==min(Lengths))]
plot(AnolisTree, cex=0.25)
Labs<-sapply(AnolisTree$edge.length, round, digits=2)
edgelabels(text=Labs, cex=0.25)
?plot.phylo
#Question 3
plot(AnolisTree, cex=0.25, show.tip.label=FALSE)
#Question 4
plot(AnolisTree, type="fan", cex=0,25, show.tip.label=FALSE)
#Question 5
plot(AnolisTree, type= "fan", cex=0.25, tip.color="red")
?which
which(AnolisTree == 'minedge.length')
AnolisTree[which(AnolisTreeedge.length==short)]
#Question 6-8 
which.min(AnolisTree$edge.length) 
#82
NewAnolisTree<- drop.tip(AnolisTree, 82)
plot(NewAnolisTree, cex=0.25)
ltt(AnolisTree)
abline(0,1,lwd=2, col='red', lty=2)
#The line never goes down, meaning more lineage are being created over time
#and diversity increases over time. The slop
#of the line gets steeper with time. There seems to be a stabilizing period near 
#the end, the slope changes but it will never be negative. 
#Question 10 
fit.bd(AnolisTree,rho=0.2)
#ML(b/lamba)=0.8031 new rate formed
#ML(d/mu)=0 disappear
#log(L)=132.9163