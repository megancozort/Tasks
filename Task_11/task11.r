x<-rnorm(100, 5, 2)
y<- (x*5) +2 +runif(100, min=0, max=0.1)
lm(y~x)
#The slope is 5.003 and the intercept is 2.033 
#The slope and the mean are very similar numbers as well as the variance and intercept.
#The slope and y-int are dependent on the restrictions of the mean and variance.
slope<- c()
yint<- c()
x<- c()
y<- c()
z<- c()
number<- c()
for (i in 1:100) {
  x[i] <- rnorm(100 ,5 ,2)
  z[i] <-rnorm(1)
  number[i] <-x * z
  y[i] <- (number*5) +2 +runif(100, min=0, max=0.1)
  mod<- lm(y~x)
  cf <-coef(mod)
  slope[i] <-cf["x"]
}
plot(z, slope)
#The plot reveals a constant/stable slope between z and the slope 

#Second Question
library(dplyr)
library(ggplot2)
doors <- 1:3
sample_doors <- function() { return(sample(doors, size = 1000, replace = TRUE))}
games <- data.frame(prize = sample_doors(), pick = sample_doors())

games$strategy <- factor(ifelse(games$prize == games$pick, 'stay', 'switch'))

monte_show <- function(prize, pick) {
  remaining <- setdiff(doors, c(prize, pick))
  return(ifelse(length(remaining)==1,
                remaining,
                sample(remaining, 1)))
}

games <- games %>%
  rowwise %>%
  mutate(shown = monte_show(prize, pick),
         stay = pick,
         switch = setdiff(doors, c(pick, shown)),
         strategy = factor(ifelse(prize == stay, 'stay', 'switch')))

print(summary(games$strategy) / nrow(games))

qplot(strategy, data = games, fill = strategy, geom = 'bar') + 
  xlab('Winning Strategy') +
  ggtitle('Monty Hall Problem Simulation')
setwd('/Users/megancozort/Desktop/Evolution/Tasks/Task_11')
pdf("Monty Plot")
dev.off()

#Question 3
install.packages('meme')
library(meme)
u<- ("https://upload.wikimedia.org/wikipedia/commons/c/cd/Two_american_alligators.jpg")
meme(u,size=1.2, "FriendZone?", "No, It's Called  Natural Selection And Your Genes Are Not Meant To Be Passed On")
meme