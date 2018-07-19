rm(list=ls())
library(dplyr)
library(tidyverse)
library(ggplot2)
library(growthcurver)
library(knitr)

setwd("C:/Users/Sam Welch/Google Drive/ICL Ecological Applications/Project/Work/Scripts")

# Load in the plate layout csv for combination and isolate location data
plate_layout <- read.csv("Data/Final_Pipeline/256comb_8bact_plate.csv") %>%
  unite(loc, Dest.Row, Dest.Column, sep = "") %>%
  unite(location, loc, plate, sep = ".")

# How many plates are there?
plate_count = 0 

# Load in plate .CSVs from a seperate folder using a for loop. Make a tibble to contain the data.
setwd("C:/Users/Sam Welch/Google Drive/ICL Ecological Applications/Project/Work/Scripts/Data/Final_Pipeline/Read_Plates")
tidy_data <- tibble()

for (k in 1:length(dir()))
{
  # Make a df for each plate with a numbered name
  temp_plate_name <- paste("plate", k ,sep = "_")
  temp_plate_df <- read.csv(dir()[k])
  temp_plate_df$time <- as.numeric(substr(temp_plate_df$time, 1, 2))
  assign(temp_plate_name, temp_plate_df)                         # turn our temporary df into a real df with a for-loop-generated name
  
  # let's also make a massive tidy dataset that's better able to store stressor presence/absence and isolate species
  temp_plate_tidy <- as.tibble(temp_plate_df) %>%
    gather(well, OD, 2:97) %>%                       # gather the data from wide to tall
    mutate(plate = k) %>%                            # add a plate column based on where we are in the for loop
    unite(location, well, plate, sep = ".")          # bring the location naming scheme in line with plate_layout
  tidy_data <- bind_rows(tidy_data, temp_plate_tidy)       # add the temporary data to our massive dataset
  plate_count = plate_count + 1
}

setwd("C:/Users/Sam Welch/Google Drive/ICL Ecological Applications/Project/Work/Scripts")

# Join isolate/stressor data to growth data by observations
tidy_data <- left_join(tidy_data, plate_layout, by = "location")

# A few checks
glimpse(tidy_data)
# How many wells do we have?
wells_count <- length(unique(tidy_data$location)) # this should be 6240
# How many time points do we have?
timepoints_count <- length(unique(tidy_data$time)) + 1 # should be 49 (but is 54 here); needs a +1 as it seems to ignore 0?
# How many plates
plate_count

# Can we just calculate a general growth stat for all the wells, keeping stressor and isolate data
tidy_growth_data <- tidy_data[0,] %>%
  select(-time, -OD)
# We can use a for loop to calculate empirical area under the curve (auc_e) (for example) for every well
for (l in 1:wells_count)
{
  temp_data_growth_0 <- tidy_data[((l-1) * timepoints_count + 1):(l * timepoints_count),]                              # Turn every timepoints_count measurements into a single df
  temp_data_growth_1 <- SummarizeGrowth(temp_data_growth_0$time, temp_data_growth_0$OD)                                # Generate a logistic model for this well's growth
  temp_data_growth_2 <- bind_cols(location = temp_data_growth_0[[1,2]],
                                  Growth_auc_e = temp_data_growth_1$vals$auc_e,
                                  Growth_k = temp_data_growth_1$vals$k,
                                  Growth_r = temp_data_growth_1$vals$r,
                                  Growth_n0 = temp_data_growth_1$vals$n0,
                                  Growth_sigma = temp_data_growth_1$vals$sigma,
                                  Copper = temp_data_growth_0[[1,4]],
                                  Nickel = temp_data_growth_0[[1,5]],
                                  Chloramphenicol = temp_data_growth_0[[1,6]],
                                  Ampicillin = temp_data_growth_0[[1,7]],
                                  Metaldehyde = temp_data_growth_0[[1,8]],
                                  Atrazine = temp_data_growth_0[[1,9]],
                                  Tebuconazole = temp_data_growth_0[[1,10]],
                                  Azoxystrobin = temp_data_growth_0[[1,11]],
                                  Isolate = temp_data_growth_0[[1,12]])
  tidy_growth_data <- bind_rows(tidy_growth_data, temp_data_growth_2)
  # append the area under the empirical curve (auc_e, etc. to tidy_growth_data)
}

# Let's graph a bunch of growth parameters against stressor richness 
# First things first: transmute stressor presence/absence to a single richness column
richness_growth_data <- tidy_growth_data %>%
  mutate(Richness = Copper + Nickel + Chloramphenicol + Ampicillin + Atrazine + Metaldehyde + Tebuconazole + Azoxystrobin) %>%
  select(location, Richness, Growth_auc_e, Isolate)

# And graph growth against richness, by species of bacteria
growthXrichness_auc_e <- ggplot(richness_growth_data, aes(Richness, Growth_auc_e)) +
  geom_point(aes(colour = Isolate, shape = 1)) +
  scale_shape_identity() +
  geom_smooth(aes(group = Isolate, colour = Isolate), method = "lm", se = FALSE) +
  geom_smooth(aes(colour = "Overall"), method = "lm", se = FALSE)
# We can do some linear modelling later and obtain some idea of the statistical soundness behind our measurements...
pdf("growthXrichness_auc_e.pdf")
growthXrichness_auc_e.pdf
dev.off()

# We can also graph the effects of different single stressors on bacteria. For instance:
KUE4_10_single_stress <- tidy_growth_data %>%
  filter(Isolate == "KUE4_10") %>%
  filter((Copper + Nickel + Chloramphenicol + Ampicillin + Atrazine + Metaldehyde + Tebuconazole + Azoxystrobin) == 1) %>%
  select(-location, -Isolate)
