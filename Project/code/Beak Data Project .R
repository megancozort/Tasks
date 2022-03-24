library(phytools)
setwd("/Users/megancozort/Desktop/Evolution/Tasks/Project/Data")
Data<- read.csv('Evolution Final Project.csv')

#Species <- unique(Data[,1])
#Species <- gsub("\\_", " ", Species)
#write.csv(Species, "birdnames.csv")

library(phytools)
trees <- read.nexus("output.nex")
tree <- trees[[1]]

Match <- intersect(tree$tip.label, Data[,1])
Different <- setdiff(Data[,1], tree$tip.label)

newDat <- Data
for (i in 1:length(Different)){
  Rows <- which(newDat[,1] == Different[i])
  newDat <- newDat[-Rows,]
}


wing <- tapply(newDat[,2], newDat[,1], mean, na.rm=T)
beak <- tapply(newDat[,3], newDat[,1], mean, na.rm=T)
datmat <- cbind(wing, beak)

Missing <- beak[is.na(beak)]
datmat2 <- datmat[!is.na(beak),]
tree2 <- drop.tip(tree, names(Missing))

VCV <- phyl.vcv(datmat2, vcv(tree2), 1)
PearsonR <- cov2cor(VCV$R)["wing","beak"] # these might need to be "wing" and "beak" not sure how Liam set this up

# plot of beak vs wing 
# This will be slow
phylomorphospace(tree2, datmat2, label="off")


#pearson correlation test between beak depth and wing span I said 
#they were positively correlated 
#Graph of Beak depth on x and wing length on y
#Do i also need regression anaylsis
#Not sure how to get tree on here or how to turn it into link
