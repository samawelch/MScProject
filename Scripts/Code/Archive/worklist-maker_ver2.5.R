#dd = design matrix dataframe (read in)

#richness = richness of each microcosm
#n.sp = number of isolates in the pool of available species
#vol.source = volume of liquid in the source plates
#vol.per.isolate = calculated volume required per species for the experiment as a whole
#n.dest.plates = calculated number of destination plates required in the experiment as a whole

#n.microcosms =  number of microcosms in the experiment
#destination.rows = vector of length n.microcosms that holds the row of each microcosm.

#destination.cols = vector of length n.microcosms that holds the column of each microcosm.
#destination.plate = vector of length n.microcosms that holds the plate number of each microcosm.

#source.rows =  vector of length 384 that holds the row of each source species
#source.cols =  vector of length 384 that holds the row of each source species
#source.plate =  vector of length 384 that holds the row of each source species

#source.plate.layout = layout of each source plate showing location of each isolate

#source.volume.tracker = vector of length n.sp which holds the volume remaining in the source well currently being used, with starting volume of vol.source
#source.increment = vector of length n.sp. If all of the liquid is used up according to source.volume.tracker for species X, source.increment for X increases by one, which shifts the well being used as the source well for X over by one.

#worklist = holds the main worklist

#outer.rows = defines the outer rows of each destination plate
#outer.cols = defines the outer columns of each destination plate

#media.worklist = holds the main media worklist

rm(list=ls())

setwd("C:/Users/Sam Welch/Google Drive/ICL Ecological Applications/Project/Work/Scripts/Data/")

dd=read.csv("sam_plate.csv",header=T)

#nb randomise here
#dd.randomise=sample(size=nrow(dd),x=1:nrow(dd),replace=FALSE)
#dd=dd[dd.randomise,]

#richness = number of species per microcosm
richness=rowSums(dd)

#n.sp = number of species in the pool of available species
n.sp=ncol(dd)

#n.microcosms = number of microcosms
n.microcosms=nrow(dd)

#volume in the source plate
vol.source=1000

#SAM NEW: volume of each stressor pipetted
vol.stressor=10


#volume required per isolate
vol.per.isolate=c()
for(i in 1:n.sp){
	vol.per.isolate=c(vol.per.isolate,sum(dd[,i]*vol.stressor))
	}

#get number of destination plates
n.dest.plates=nrow(dd)/96 # /96 as we're using all the wells on a 96-well plate

#if number of destination plates is an integer, remove the decimal part and add one
n.dest.plates=(as.integer(nrow(dd)/96)+1)

#make sure it's an integer
n.dest.plates=as.integer(n.dest.plates)

#SAM NEW: using all 96 wells, not just central wells
destination.rows=rep(rep(LETTERS[1:8],12),n.dest.plates)
destination.cols=rep(rep(1:12,each=8),n.dest.plates)
destination.plate=rep(1:n.dest.plates,each=96)

#define source plates using every well of 4 destination plates
source.rows=rep(rep(LETTERS[1:8],12),4)
source.cols=rep(rep(1:12,each=8),4)
source.plate=rep(1:4,each=96)

#reps.sp = number of times the stressor is replicated in the source plate
reps.sp=24
#number of wells of each isolate required in the experiment. Allow 30% extra (so *1.3)
#reps.sp=as.integer((vol.per.isolate/vol.source)*1.4)+1

#sp.start = well in source plate where the species starts. 
sp.start=seq(from=1,to=(1+n.sp*max(reps.sp)),by=max(reps.sp))

#source.plate.layout = layout of the source plates
temp=matrix(NA,ncol=12,nrow=8);rownames(temp)=LETTERS[1:8];colnames(temp)=1:12
source.plate.layout=list(temp,temp,temp,temp)
source.plate.sequence=rep(1:n.sp,each=max(reps.sp))
for(i in 1:4){
	source.plate.layout[[i]][1:8,]=source.plate.sequence[(((i-1)*96)+1):(((i)*96))]
}
source.plate.layout

#counter=1 #old
#source.increment=0 #old
#source.volume tracker holds the volume remaining in each well, with a separate volume tracker for each isolate
source.volume.tracker=matrix(vol.source,nrow=n.sp,ncol=1)

#source.increment is a counter that keeps track of how many source wells we've gone through for each isolate
source.increment=matrix(0,nrow=n.sp,ncol=1)

#worklist holds the worklist that will be fed into Darth
worklist=c()

#loop through the i each microcosm
for(i in 1:nrow(dd)){

	#identify which isolates are in the microcosm
	sp=which(dd[i,]==1)

	#vol.i = volume of ith microcosm
	vol.i=vol.stressor

	#loop through each species in the microcosm
	for(j in 1:length(sp)){
	
		#add volume to sp[j] source.volume.tracker
		source.volume.tracker[sp[j]]=source.volume.tracker[sp[j]]-vol.i
		
		#check to see if we have only 10% of the volume remaining; if so, move to next source well and reset the volume tracker
		if(source.volume.tracker[sp[j]]<=250){
			source.increment[sp[j]]=source.increment[sp[j]]+1
			source.volume.tracker[sp[j]]=vol.source-vol.i
		}

		worklist=rbind(worklist,c(
			source.plate[sp.start[sp[j]]+source.increment[sp[j]]],
			source.rows[sp.start[sp[j]]+source.increment[sp[j]]],
			source.cols[sp.start[sp[j]]+source.increment[sp[j]]],
			destination.plate[i],
			destination.rows[i],
			destination.cols[i],
			vol.stressor)
			)
	}

if(max(source.increment)>min(reps.sp)){print("WARNING: you have insufficient replication of your source isolates!!")}

}

colnames(worklist)=c("source.plate","source.row","source.column","destination.plate","destination.row","destination.column","vol.ul")
worklist=as.data.frame(worklist)

#SAM NEW: what's this? - I think it's meant to print the worklist matrices to the console for visual inspection
tapply(as.numeric
       (as.matrix
         (worklist$vol.ul)),
          INDEX=list(worklist$source.row,
          worklist$source.column,
          worklist$source.plate),
          FUN=function(x){sum(x,na.rm=T)}) # I have no idea, but right now it doesn't do anything?

write.csv(worklist,"../Results/Robot_Worklists/comb_worklist.csv")

#media worklist

#total volume in microcosm
total.vol=100

#extend richness by 72 10s - a total bodge but should fix the media worklist by ensuring that '10' richness wells contain no media
richness_ext <- c(richness, rep(10, 72))

media.rows=destination.rows
media.cols=destination.cols
media.plates=destination.plate

media.worklist=cbind(media.plates,media.rows,media.cols,total.vol-(richness_ext*vol.stressor))
colnames(media.worklist)=c("destination.plate","destination.row","destination.column","media.vol.ul")
write.csv(media.worklist,file="../Results/Robot_Worklists/media_worklist_comb_exp.csv")



