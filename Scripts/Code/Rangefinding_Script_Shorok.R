#######################
### Shorok's Script ###
#######################
library(sciplot)
library(vegan)
library(here)

rm(list=ls())
set.seed(1234)
setwd(here("MScProject","Scripts","Data"))


dd<-read.csv("growth_curves_conc.csv", header=T)

dd<-subset(dd, dd$stressor!="Control")
dd<-droplevels(dd)

plot(log(dd$max.running.average)~log(dd$actual.concentration..mg.ml.))
#-3, 0


layout(matrix(1:9, ncol=3))
for( i in 1:nlevels(dd$stressor)){
  temp<-subset(dd, dd$stressor==unique(dd$stressor)[i])
  temp<-droplevels(temp)
  plot(log(temp$max.running.average)~log(temp$actual.concentration..mg.ml.), pch=16, type="n", main=paste(temp$stressor[1]), ylab="Log(Max OD)", xlab="Log(Conc; mg/ml)", ylim=c(-3.5, 0))
  for( j in 1:nlevels(temp$Species)){
    points(log(temp$max.running.average[temp$Species==unique(temp$Species)[j]])~log(temp$actual.concentration..mg.ml.[temp$Species==unique(temp$Species)[j]]), pch=16, col=j)
    abline(lm(log(temp$max.running.average[temp$Species==unique(temp$Species)[j]])~log(temp$actual.concentration..mg.ml.[temp$Species==unique(temp$Species)[j]])), col=j)
  }}

plot(temp$max.running.average~log(temp$actual.concentration..mg.ml.), type="n", ylab="", xlab="", ylim=c(0, 1), xaxt="n", yaxt="n", bty="n")
legend(y=1.5, x=(-8), pch=16, legend=levels(dd$Species), bty="n", col=1:7, y.intersp = 0.2, x.intersp = 0.2, cex=0.8, ncol=2)