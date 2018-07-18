rm(list=ls())
library(dplyr)
library(tidyverse)
library(ggplot2)
library(growthcurver)

setwd("C:/Users/Sam Welch/Google Drive/ICL Ecological Applications/Project/Work/Scripts")

# Load in the plate layout csv for combination and isolate location data
plate_layout <- read.csv("Data/Final_Pipeline/256comb_8bact_plate.csv") %>%
  unite(loc, Dest.Row, Dest.Column, sep = "") %>%
  unite(location, loc, plate, sep = ".")

# Load in plate .CSVs from a seperate folder using a for loop. Make a tibble to contain the data.
setwd("C:/Users/Sam Welch/Google Drive/ICL Ecological Applications/Project/Work/Scripts/Data/Final_Pipeline/Read_Plates")
tidy_data <- tibble()

for (k in 1:length(dir()))
{
  # Make a df for each plate with a numbered name
  temp_name <- paste("plate", k ,sep = "_")
  temp_df <- read.csv(dir()[k])
  temp_df$time <- as.numeric(substr(temp_df$time, 1, 2))
  assign(temp_name, temp_df)                         # turn our temporary df into a real df with a for-loop-generated name
  
  # let's also make a massive tidy dataset that's better able to store stressor presence/absence and isolate species
  temp_tidy <- as.tibble(temp_df) %>%
    gather(well, OD, 2:97) %>%                       # gather the data from wide to tall
    mutate(plate = k) %>%                            # add a plate column based on where we are in the for loop
    unite(location, well, plate, sep = ".")          # bring the location naming scheme in line with plate_layout
  tidy_data <- bind_rows(tidy_data, temp_tidy)       # add the temporary data to our massive dataset
         
}

# Join isolate/stressor data to growth data by observations
tidy_data <- left_join(tidy_data, plate_layout, by = "location")

# A few checks
glimpse(tidy_data)
# How many wells do we have?
wells_count <- length(unique(tidy_data$location)) # this should be 6240
# How many time points do we have?
timepoints_count <- length(unique(tidy_data$time))

# Can we, for instance, take tidy_data and easily do something with it?
A1_data <- filter(tidy_data, location == "A1.1")
A1_growth <- SummarizeGrowth(A1_data$time,A1_data$OD)
plot(A1_growth)

# Let's graph a bunch of growth parameters against stressor richness 
# First things first: transmute stressor presence/absence to a single richness column
richness_data <- tidy_data %>%
  mutate(Richness = Copper + Nickel + Chloramphenicol + Ampicillin + Atrazine + Metaldehyde + Tebuconazole + Azoxystrobin)
richness_data <- richness_data %>%                   # winnow the tibble down to the bare essentials for this graphic
  select(time, location, Richness, OD, Isolate)

# We can use a for loop to calculate area under the curve (for example) for every well


