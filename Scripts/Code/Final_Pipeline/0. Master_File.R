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

setwd(here())

### Which runs are we including?
runs_vector <- c(2,3,4)

#########################
### DATA MANIPULATION ###
#########################

# Load well data from source .CSVs
source("Scripts/Code/Final_Pipeline/Well_Data_Loader.R")

# Make a tibble of growth curve parameters per well. Plotting currently commented out.
source("Scripts/Code/Final_Pipeline/Growth_Curve_Loop.R")

# Create a new tibble of mean and SD from tidy_growth_data using any given metric of bacterial growth
source("Scripts/Code/Final_Pipeline/Summarise_Growth_Data.R")

# Calculate control baselines per isolate, then single stressor effects, which are used to qualify and quantify interactions
source("Scripts/Code/Final_Pipeline/Define_Interaction.R")

#########################
### GRAPHICAL OUTPUTS ###
#########################

# Manipulate data and plot growth parameters against stressor richness
source("Scripts/Code/Final_Pipeline/Plot_RichnessXGrowth.R")

# Manipulate data and plot single stressor growth over time by isolate 
source("Scripts/Code/Final_Pipeline/Plot_isolateXSingleStressor.R")

# Manipulate data and plot binary stressor effect on growth parameters by isolate 
source("Scripts/Code/Final_Pipeline/Plot_Binary_interactionsXIsolate.R")

# Lay well growth curves produced by Growth_Curve_Loop out across pdfs, one page per well. Doesn't currently work (intentionally)
source("Scripts/Code/Final_Pipeline/Plot_Plate_Growth_Curves.R")

# Plot observed effect against predicted effect by isolate, to a pdf. 
source("Scripts/Code/Final_Pipeline/Plot_ObsXPred_Effect.R")

#########################
#### MISC. FUNCTIONS ####
#########################

# Borrowed variant t-test
source("Scripts/Code/Final_Pipeline/t.test2.R")

# Take a tibble with stressor presence/absence data and isolate names, convert to functional group presence (0-2) and species name
source("Scripts/Code/Final_Pipeline/aggregate_functional_groups")

