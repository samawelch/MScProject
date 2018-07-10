rm(list=ls())

setwd("~/Dropbox/Experiments/Isolate profiles/Antibiotic screen") #this wd should only contain the growth csv

window=5 #how many time points to look at at one time
max.running.average=c() # after this calculate the average of all of the window, it takes the max, which is the max OD
max.slopes=c() #this calculates the slope of the of all of the window, it takes the max, which is the max slope- it wont include any negatives because it takes the max
well=c() #location well
plate.dir=c() #plate ID name from the dir
plate=c() #plate number

for(k in 1:length(dir())){
  
dd=read.csv(dir()[k],header=T,row.names=1)
time=seq(from=0,to=ncol(dd)/2,by=0.5) # this is because the reads are every 30 minutes so it takes the number of cols and divides by 2
time=time[1:ncol(dd)]

name<-as.character(dir()[k])

setwd("~/Dropbox/Experiments/Isolate profiles/Graph") #you need to set this to a new wd for your graphs to go into, make sure your graphs dont go into the wd with the csv
pdf(sprintf('antibiotic.growth_%s.pdf',k),paper='a4r')

for(j in 1:nrow(dd)){
  #making the calculations
  running.average=c()
  average.time=c()
  slopes=c()
  for(i in 1:(ncol(dd)-window)){
    running.average=c(running.average,median(as.numeric(dd[j,i:(i+5)])))
    slopes=c(slopes,coef(lm(as.numeric(dd[j,i:(i+5)])~time[i:(i+5)]))[2])
    average.time=c(average.time,mean(as.numeric(time[i:(i+5)])))
  }
 #makes the graph so you can check if it worked
  plot(as.numeric(dd[j,])~time,type="l",xlab="",ylab="",axes=F,ylim=range(dd));box()
  lines(running.average~average.time,col="red")
  par(new=T)
  plot(slopes~average.time,col="blue",type="l",ylab="",axes=F,ylim=c(-0.2,0.2));box()
  #c the max values into vector
  max.running.average=c(max.running.average,max(running.average))
  max.slopes=c(max.slopes,max(slopes))
  well=c(well, rownames(dd)[j])
  plate=c(plate, name)
}

dev.off()
#switch back to the dir that contains the csv
setwd("~/Dropbox/Experiments/Isolate profiles/Antibiotic screen")
#counter to know how many its done
print(k/length(dir()))

}

#bind all the data together to make a dataframe
growth.curve<-cbind(max.running.average,max.slopes, well, plate)

#write the csv file
write.csv(growth.curve, "growth.curve.csv")
