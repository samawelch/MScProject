rm(list=ls())
library(dplyr)
library(tidyr)
library(ggplot2)
library(growthcurver)

setwd("C:/Users/Sam Welch/Google Drive/ICL Ecological Applications/Project/Work/Scripts")

plate_layout_matrix <- read.csv("Data/Final_Pipeline/256comb_8bact_plate.csv")