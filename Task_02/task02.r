setwd("~/Desktop/Evolution/Tasks/Task_02")
Data<-read.csv('http://jonsmitchell.com/data/beren.csv',stringsAsFactors=F)
write.csv(Data, 'rawdata.csv', quote=F)
'Data'
length(Data)
nrow(Data)
ncol(Data)
colnames(Data)
head(Data)
Data[1,]
Data[2,]
Data[1:3,]
Data[1:3, 4]
Data[1:5, 1:3]
Feeds<-which(Data[,9]=='bottle')
berenMilk<-Data[Feeds,]
head(berenMilk)
Feeds<-which(Data[, 'event']=='bottle')
Feeds<-which(Data$event=='bottle')
head(berenMilk)
dayID<- apply(Data, 1, function(x) paste(x[1:3], collapse='-'))
dateID<- sapply(dayID, as.Date, format = "%Y-%m-%d", origin="2019-04-18")
Data$age<-dateID-dateID[which(Data$event=='birth')]
head(Data)
beren2<-Data
order(Data$age)
beren3<-beren2[order(beren2$age),]
head(beren)
head(beren2)
head(beren3)
write.csv(beren3, 'beren_new.csv', quote=F, row.names=FALSE)
setwd("~/Desktop/Evolution/Tasks/Task_02")
Data<-read.csv('http://jonsmitchell.com/data/beren.csv',stringsAsFactors=F)
write.csv(Data, 'rawdata.csv', quote=F)
'Data'
length(Data)
nrow(Data)
ncol(Data)
colnames(Data)
head(Data)
Data[1,]
Data[2,]
Data[1:3,]
Data[1:3, 4]
Data[1:5, 1:3]
Feeds<-which(Data[,9]=='bottle')
berenMilk<-Data[Feeds,]
head(berenMilk)
Feeds<-which(Data[, 'event']=='bottle')
Feeds<-which(Data$event=='bottle')
head(berenMilk)
dayID<- apply(Data, 1, function(x) paste(x[1:3], collapse='-'))
dateID<- sapply(dayID, as.Date, format = "%Y-%m-%d", origin="2019-04-18")
Data$age<-dateID-dateID[which(Data$event=='birth')]
head(Data)
beren2<-Data
order(Data$age)
beren3<-beren2[order(beren2$age),]
head(beren)
head(beren2)
head(beren3)
write.csv(beren3, 'beren_new.csv', quote=F, row.names=FALSE)
beren3
Feeds<- which(beren3$event=="bottle")
avgMilk<- mean(beren3$value[Feeds])
#ounces
#The value column is the amount in ounces
#You use the bracket because thats what we named the object
avgFeed<- tapply(beren3$value[Feeds], beren3$age[Feeds], mean)
head(avgFeed)
varFeed<- tapply(beren3$value[Feeds], beren3$age[Feeds], var)
totalFeed<- tapply(beren3$value[Feeds], beren3$age[Feeds], sum)
numFeeds<- tapply(beren3$value[Feeds], beren3$age[Feeds], length)
cor(beren3$value[Feeds], beren3$age[Feeds])
cor.test(beren3$value[Feeds], beren3$age[Feeds])
berenCor<- cor.test(beren3$value[Feeds], beren3$age[Feeds])
summary(berenCor)
berenANOVA<-aov(beren3$value[Feeds] ~ beren3$caregiver[Feeds])
boxplot(beren3$value[Feeds] ~ beren3$caregiver[Feeds], xlab= "who gave the bottle" , ylab= "amount of milk consumed (oz)")
?par
par(las=1 , mar=c(5,5,1,1), mgp=c(2,0.5,0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days" , ylab="ounces of milk")
abline(h=mean(totalFeed), lty=2, col='red')
pdf("r02b-totalMilkByDay.pdf", height=4, width=4)
par(las=1, mar=c(5,5,1,1), mgp=c(2,0.5,0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=mean(totalFeed), lty=2, col='red')
dev.off()
source("http://jonsmitchell.com/code/plotFxn02b.R")
pdf("r02b-cumulativeMilkBytime.pdf" , height=4 , width=4)
par(las=1, mar=c(5,5,1,1), mgp=c(2,0.5,0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=mean(totalFeed), lty=2, col='red')
dev.off()
getwd()
#Hypothesis 3 is the testable hypothesis, hypothesis 1 would be fine if it tested data, 
#however, how much he eats isn't measured, also 'amount' is very vague it needs to be more specific 
#like how many ounces ect. Hypothesis 2 isn't great either because it just says relationship which is to 
#vague and again 'amount', well what kind of amount?
#Q2: The total milk, age in days, time of day graph is impossible to interpret because
#there's three axis labels but there is only two axis.
#We also have to keep in mind that as he gets older he starts drinking less milk and eating more food.
#There's also not sufficient end times of the data recorded. Also, the graph of nap y axis has no units. 
unique(beren3$event)
#Hypothesis: The length of Beren in cm increases as he ages by days
traitlength<-which(beren3$event=="trait_length")
traitlength
avgtraitlength<-mean(beren3$value[traitlength])
avgtraitlength
cor.test(beren3$value[traitlength], beren3$age[traitlength])
t.test(beren3$value[traitlength], beren3$age[traitlength])
Naps<-which(beren3$event=='nap')
beren4<- beren3[Naps, ]
beren4
Start <-beren4$start_hour + beren4$start_minute/60
Start
End <- beren4$end_hour + beren4$end_minute/60
End
duration<- End-Start
duration
plot(beren4$age, duration)
cor.test(beren4$age, duration)
#This is a negative correlation, the duration of Berens nap time decreases as his
#age increases.
beren3
#########################################














Starttime<-tapply(beren4$start_hour, beren4$start_minute)
time1<-"2019-08-22 12:30"
time1E<-"2019-08-22 13:30"
Aug22nap<-difftime(time1E,time1,units="hours")
Aug22nap
time2<-"2019-08-23 8:55"
time2E<-"2019-08-23 9:36"
Aug23nap<-difftime(time2E,time2, units="hours")
Aug23nap
time3<-"2019-08-23 11:52"
time3E<-"2019-08-23 12:46"
Aug23nap2<-difftime(time3E,time3, units="hours")
Aug23naptotal<-sum(Aug23nap, Aug23nap2)
Aug23naptotal
time4<-"2019-08-26 8:40"
time4E<-"2019-08-26 10:10"
Aug26nap<-difftime(time4E, time4, units="hours")
Aug26nap
time5<-"2019-08-26 12:45"
time5E<-"2019-08-26 13:20"
Aug26nap2<-difftime(time5E,time5, units="hours")
Aug26nap2
Aug26naptotal<-sum(Aug26nap, Aug26nap2)
Aug26naptotal
time6<-"2019-08-27 11:10"
time6E<-"2019-08-27 12:40"
Aug27nap<-difftime(time6E, time6, units="hours")
Aug27nap
time7<-"2019-08-27 14:10"
time7E<-"2019-08-27 14:45"
Aug27nap2<-difftime(time7E,time7, units="hours")
Aug27nap2
Aug27naptotal<-sum(Aug27nap, Aug27nap2)
Aug27naptotal
time8<-"2019-08-30 7:30"
time8E<-"2019-08-30 8:42"
Aug30nap<-difftime(time8E,time8, units="hours")
Aug30nap
time9<-"2019-08-30 14:27"
time9E<-"2019-08-30 14:37"
Aug30nap2<-difftime(time9E,time9, units="hours")
Aug30nap2
Aug30naptotal<-sum(Aug30nap, Aug30nap2)
Aug30naptotal
time10<-"2019-09-04 8:04"
time10E<-"2019-09-04 9:19"
Sept4nap<-difftime(time10E,time10, units="hours")
Sept4nap
time11<-"2019-09-04 11:07"
time11E<-"2019-09-04 12:56"
Sept4nap2<-difftime(time11E,time11, units="hours")
Sept4nap2
Sept4naptotal<-sum(Sept4nap, Sept4nap2)
Sept4naptotal
time12<-"2019-09-05 8:00"
time12E<-"2019-09-05 9:19"
Sept5nap<-difftime(time12E,time12, units="hours")
Sept5nap
time13<-"2019-09-05 12:15"
time13E<-"2019-09-05 12:33"
Sept5nap2<-difftime(time13E,time13, units="hours")
Sept5nap2
Sept5naptotal<-sum(Sept5nap,Sept5nap2)
Sept5naptotal
time14<-"2019-09-05 12:55"
time14E<-"2019-09-05 14:17"
Sept5nap3<-difftime(time14E,time14,units="hours")
Sept5nap3
Sept5naptotal2<-sum(Sept5nap,Sept5nap2,Sept5nap3)
Sept5naptotal2
time15<-"2019-09-06 8:48"
time15E<-"2019-09-06 9:32"
Sept6nap<-difftime(time15E,time15, units="hours")
Sept6nap
time16<-"2019-09-06 11:34"
time16E<-"2019-09-06 12:10"
Sept6nap2<-difftime(time16E,time16, units="hours")
Sept6nap2
time17<-"2019-09-06 14:10"
time17E<-"2019-09-06 14:40"
Sept6nap3<-difftime(time17E,time17, units="hours")
Sept6nap3
Sept6naptotal<-sum(Sept6nap,Sept6nap2,Sept6nap3)
Sept6naptotal
time18<-"2019-09-09 8:46"
time18E<-"2019-09-09 9:43"
Sept9nap<-difftime(time18E,time18, units="hours")
Sept9nap
time19<-"2019-09-09 12:38"
time19E<-"2019-09-09 13:23"
Sept9nap2<-difftime(time19E,time19, units="hours")
Sept9nap2
Sept9naptotal<-sum(Sept9nap,Sept9nap2)
Sept9naptotal
time20<-"2019-09-10 9:00"
time20E<-"2019-09-10 9:40"
Sept10nap<-difftime(time20E,time20, units="hours")
Sept10nap
time21<-"2019-09-10 12:00"
time21E<-"2019-09-10 12:35"
Sept10nap2<-difftime(time21E,time21, units="hours")
Sept10nap2
Sept10naptotal<-sum(Sept10nap,Sept10nap2)
Sept10naptotal
time22<-"2019-09-11 9:49"
time22E<-"2019-09-11 10:36"
Sept11nap<-difftime(time22E,time22, units="hours")
Sept11nap
time23<-"2019-09-11 11:49"
time23E<-"2019-09-11 12:05"
Sept11nap2<-difftime(time23E,time23, units="hours")
Sept11nap2
time24<-"2019-09-11 14:16"
time24E<-"2019-09-11 14:45"
Sept11nap3<-difftime(time24E,time24, units="hours")
Sept11nap3
Sept11naptotal<-sum(Sept11nap,Sept11nap2,Sept11nap3)
Sept11naptotal
time25<-"2019-09-12 9:08"
time25E<-"2019-09-12 9:46"
Sept12nap<-difftime(time25E,time25, units="hours")
Sept12nap
time26<-"2019-09-12 12:07"
time26E<-"2019-09-12 13:05"
Sept12nap2<-difftime(time26E,time26, units="hours")
Sept12nap2
Sept12naptotal<-sum(Sept12nap,Sept12nap2)
Sept12naptotal
time27<-"2019-09-13 8:22"
time27E<-"2019-09-13 9:17"
Sept13nap<-difftime(time27E,time27, units="hours")
Sept13nap
time28<-"2019-09-13 11:45"
time28E<-"2019-09-13 12:15"
Sept13nap2<-difftime(time28E,time28, units="hours")
Sept13nap2
Sept13naptotal<-sum(Sept13nap, Sept13nap2)
Sept13naptotal
time29<-"2019-09-16 12:15"
time29E<-"2019-09-16 12:53"
Sept16nap<-difftime(time29E,time29, units="hours")
Sept16nap
time30<-"2019-09-18 10:40"
time30E<-"2019-09-18 11:02"
Sept18nap<-difftime(time30E,time30, units="hours")
Sept18nap
time31<-"2019-09-18 14:35"
time31E<-"2019-09-18 14:45"
Sept18nap2<-difftime(time31E,time31, units="hours")
Sept18nap2
Sept18naptotal<-sum(Sept18nap,Sept18nap2)
Sept18naptotal
time32<-"2019-09-20 9:40"
time32E<-"2019-09-20 10:23"
Sept20nap<-difftime(time32E,time32, units="hours")
Sept20nap
time33<-"2019-09-20 13:21"
time33E<-"2019-09-20 14:14"
Sept20nap2<-difftime(time33E,time33, units="hours")
Sept20nap2
Sept20naptotal<-sum(Sept20nap,Sept20nap2)
Sept20naptotal
time34<-"2019-09-23 11:17"
time34E<-"2019-09-23 11:32"
Sept23nap<-difftime(time34E,time34, units="hours")
Sept23nap
time35<-"2019-09-24 10:35"
time35E<-"2019-09-24 11:20"
Sept24nap<-difftime(time35E,time35,units="hours")
Sept24nap
time36<-"2019-09-24 14:21"
time36E<-"2019-09-24 15:03"
Sept24nap2<-difftime(time36E,time36, units="hours")
Sept24nap2
Sept24naptotal<-sum(Sept24nap,Sept24nap2)
Sept24naptotal
time37<-"2019-09-25 8:40"
time37E<-"2019-09-25 9:30"
Sept25nap<-difftime(time37E,time37, units="hours")
Sept25nap
time38<-"2019-09-30 9:36"
time38E<-"2019-09-30 10:10"
Sept30nap<-difftime(time38E,time38, units="hours")
Sept30nap
time39<-"2019-10-02 8:57"
time39E<-"2019-10-02 10:14"
Oct2nap<-difftime(time39E,time39, units="hours")
Oct2nap
time40<-"2019-10-02 13:23"
time40E<-"2019-10-02 14:00"
Oct2nap2<-difftime(time40E,time40,units="hours")
Oct2nap2
Oct2naptotal<-sum(Oct2nap,Oct2nap2)
Oct2naptotal
time41<-"2019-10-03 9:10"
time41E<-"2019-10-03 10:25"
Oct3nap<-difftime(time41E,time41,units="hours")
Oct3nap
time42<-"2019-10-04 7:52"
time42E<-"2019-10-04 9:10"
Oct4nap<-difftime(time42E,time42, units="hours")
Oct4nap
time43<-"2019-10-04 14:09"
time43E<-"2019-10-04 15:20"
Oct4nap2<-difftime(time43E,time43, units="hours")
Oct4nap2
Oct4naptotal<-sum(Oct4nap,Oct4nap2)
Oct4naptotal
time44<-"2019-10-07 9:10"
time44E<-"2019-10-07 9:43"
Oct7nap<-difftime(time44E,time44, units="hours")
Oct7nap
time45<-"2019-10-07 12:28"
time45E<-"2019-10-07 13:22"
Oct7nap2<-difftime(time45E,time45, units="hours")
Oct7nap2
Oct7naptotal<-sum(Oct7nap,Oct7nap2)
Oct7naptotal
time46<-"2019-10-08 9:23"
time46E<-"2019-10-08 10:00"
Oct8nap<-difftime(time46E,time46, units="hours")
Oct8nap
time47<-"2019-10-08 12:15"
time47E<-"2019-10-08 12:40"
Oct8nap2<-difftime(time47E,time47, units="hours")
Oct8nap2
time48<-"2019-10-08 14:45"
time48E<-"2019-10-08 15:10"
Oct8nap3<-difftime(time48E,time48, units="hours")
Oct8nap3
Oct8naptotal<-sum(Oct8nap,Oct8nap2,Oct8nap3)
Oct8naptotal
time49<-"2019-10-09 9:20"
time49E<-"2019-10-09 10:55"
Oct9nap<-difftime(time49E,time49, units="hours")
Oct9nap
time50<-"2019-10-10 9:18"
time50E<-"2019-10-10 9:40"
Oct10nap<-difftime(time50E,time50, units="hours")
Oct10nap
time51<-"2019-10-15 9:30"
time51E<-"2019-10-15 10:00"
Oct15nap<-difftime(time51E,time51,units="hours")
Oct15nap
time52<-"2019-10-15 11:35"
time52E<-"2019-10-15 12:15"
Oct15nap2<-difftime(time52E,time52,units="hours")
Oct15nap2
Oct15naptotal<-sum(Oct15nap,Oct15nap2)
Oct15naptotal
time53<-"2019-10-16 9:05"
time53E<-"2019-10-16 9:45"
Oct16nap<-difftime(time53E,time53, units="hours")
Oct16nap
time54<-"2019-10-16 13:30"
time54E<-"2019-10-16 14:10"
Oct16nap2<-difftime(time54E,time54, units="hours")
Oct16nap
Oct16naptotal<-sum(Oct16nap,Oct16nap2)
Oct16naptotal
time55<-"2019-10-17 9:21"
time55E<-"2019-10-17 9:52"
Oct17nap<-difftime(time55E,time55,units="hours")
Oct17nap
time56<-"2019-10-17 14:12"
time56E<-"2019-10-17 14:45"
Oct17nap2<-difftime(time56E,time56,units="hours")
Oct17nap2
Oct17naptotal<-sum(Oct17nap,Oct17nap2)
Oct17naptotal
time57<-"2019-10-18 9:10"
time57E<-"2019-10-18 9:58"
Oct18nap<-difftime(time57E,time57, units="hours")
Oct18nap
time58<-"2019-10-18 13:01"
time58E<-"2019-10-18 13:30"
Oct18nap2<-difftime(time58E,time58,units="hours")
Oct18nap2
Oct18naptotal<-sum(Oct18nap,Oct18nap2)
Oct18naptotal
time59<-"2019-10-21 9:27"
time59E<-"2019-10-21 10:00"
Oct21nap<-difftime(time59E,time59,units="hours")
Oct21nap
time60<-"2019-10-21 13:00"
time60E<-"2019-10-21 13:30"
Oct21nap2<-difftime(time60E,time60, units="hours")
Oct21nap2
Oct21naptotal<-sum(Oct21nap,Oct21nap2)
Oct21naptotal
time61<-"2019-10-22 9:17"
time61E<-"2019-10-22 10:30"
Oct22nap<-difftime(time61E,time61, units="hours")
Oct22nap
time62<-"2019-10-22 13:10"
time62E<-"2019-10-22 13:25"
Oct22nap2<-difftime(time62E,time62, units="hours")
Oct22nap2
Oct22naptotal<-sum(Oct22nap,Oct22nap2)
Oct22naptotal
time63<-"2019-10-23 9:00"
time63E<-"2019-10-23 9:45"
Oct23nap<-difftime(time63E,time63,units="hours")
Oct23nap
time64<-"2019-10-23 11:32"
time64E<-"2019-10-23 12:55"
Oct23nap2<-difftime(time64E,time64, units="hours")
Oct23nap2
Oct23naptotal<-sum(Oct23nap,Oct23nap2)
Oct23naptotal
time65<-"2019-10-24 8:45"
time65E<-"2019-10-24 9:15"
Oct24nap<-difftime(time65E,time65, units="hours")
Oct24nap
time66<-"2019-10-24 11:10"
time66E<-"2019-10-24 12:40"
Oct24nap2<-difftime(time66E,time66, units="hours")
Oct24nap2
Oct24naptotal<-sum(Oct24nap,Oct24nap2)
Oct24naptotal
time67<-"2019-10-25 11:40"
time67E<-"2019-10-25 12:14"
Oct25nap<-difftime(time67E,time67,units="hours")
Oct25nap
time68<-"2019-10-28 10:53"
time68E<-"2019-10-28 11:45"
Oct28nap<-difftime(time68E,time68, units="hours")
Oct28nap
