rm(list=ls())

setwd("C:/Users/Sam Welch/Google Drive/ICL Ecological Applications/Project/Work/Scripts/Data/Bug_Rangefinding/Working_Dir") #this wd should only contain the growth csv

window = 5 # how many time points to look at at one time
max.running.average= c() # after this calculate the average of all of the window, it takes the max, which is the max OD
max.slopes = c() # this calculates the slope of the of all of the window, it takes the max, which is the max slope- it wont include any negatives because it takes the max
well = c() # location well
plate.dir = c() # plate ID name from the dir
plate = c() # plate number
  
for(k in 1:length(dir())){
  
  dd = read.csv(dir()[k],header=T,row.names=1)
  time = seq(from=0,to=ncol(dd),by=0.5) 
  time = time[1:ncol(dd)]
  
  name <- as.character(dir()[k])

  setwd("C:/Users/Sam Welch/Google Drive/ICL Ecological Applications/Project/Work/Scripts/Results/Bug_Rangefinding/") 

  # make a blank graph for a given stressor
  plot(1, type="n", axes=F, xlab="", ylab="", main=k)
  
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
    # makes the graph so you can check if it worked
    par(new=T)
    plot(as.numeric(dd[j,])~time,type="l",xlab="",ylab="OD",axes=F,ylim=range(dd));box()
    #c the max values into vector
    max.running.average=c(max.running.average,max(running.average))
    max.slopes=c(max.slopes,max(slopes))
    well=c(well, rownames(dd)[j])
    plate=c(plate, name)
  }
  #switch back to the dir that contains the csv
  setwd("C:/Users/Sam Welch/Google Drive/ICL Ecological Applications/Project/Work/Scripts/Data/Bug_Rangefinding/Working_Dir")
  dev.off()
}

#switch back to the dir that contains the csv
setwd("C:/Users/Sam Welch/Google Drive/ICL Ecological Applications/Project/Work/Scripts/Data/Bug_Rangefinding/Working_Dir")

#bind all the data together to make a dataframe
growth.curve<-cbind(max.running.average,max.slopes, well, plate)

#write the csv file
write.csv(growth.curve, "../growth_curves.csv")


