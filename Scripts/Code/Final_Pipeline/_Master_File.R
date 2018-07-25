# Can I write a short script here that executes all my individual scripts? or do I have to make them into functions?
# No, because:
# a) I can't get source to work properly
# b) Apparently it's a bad idea? I'm not really clear on why.

rm(list=ls())
library(dplyr)
library(tibble)
library(tidyr)
library(ggplot2)
library(growthcurver)
library(gridBase)
library(gridExtra)

setwd("C:/Users/Sam Welch/Google Drive/ICL Ecological Applications/Project/Work/Scripts/Code/Final_Pipeline")

#########################
### DATA MANIPULATION ###
#########################

# TODO: None of this currently works. I don't know why...

# Load well data from source .CSVs
source("Well_Data_Loader.R")
setwd("C:/Users/Sam Welch/Google Drive/ICL Ecological Applications/Project/Work/Scripts/Code/Final_Pipeline")

# Make a tibble of growth curve parameters per well. Currently also plots all the growth curves from plate 1, 2 and 3. Very slow due to a reliance on long for loops.
source("Growth_Curve_Loop.R")
setwd("C:/Users/Sam Welch/Google Drive/ICL Ecological Applications/Project/Work/Scripts/Code/Final_Pipeline")

#########################
### GRAPHICAL OUTPUTS ###
#########################

# Manipulate data and plot growth parameters against stressor richness
source("Plot_RichnessXGrowth.R")
setwd("C:/Users/Sam Welch/Google Drive/ICL Ecological Applications/Project/Work/Scripts/Code/Final_Pipeline")

# Manipulate data and plot single stressor growth over time by isolate 
source("Plot_isolateXSingleStressor.R")
setwd("C:/Users/Sam Welch/Google Drive/ICL Ecological Applications/Project/Work/Scripts/Code/Final_Pipeline")

# Manipulate data and plot binary stressor effect on growth parameters by isolate 
source("Plot_Binary_interactionsXIsolate.R")
setwd("C:/Users/Sam Welch/Google Drive/ICL Ecological Applications/Project/Work/Scripts/Code/Final_Pipeline")

# Lay well growth curves produced by Growth_Curve_Loop out across pdfs, one page per well
source("Plot_Plate_Growth_Curves.R")
setwd("C:/Users/Sam Welch/Google Drive/ICL Ecological Applications/Project/Work/Scripts/Code/Final_Pipeline")
