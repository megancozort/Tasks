install.packages("rfishbase")
library(rfishbase)
fish<- c("trout")
fish
install.packages("rvertnet")
library('rvertnet')
res <- searchbyterm(class="Aves", state="California", limit=10, messages=FALSE)
install.packages("auk")
library(auk)
install.packages("readr")
library(readr)
datam<-read.table("ebd_US-AL-101_202103_202103_relMar-2021.txt", sep="\t")
setwd("/Users/megancozort/Desktop/Evolution/Tasks/Task_07")
