# Loads modified Synergy 2 well data as CSVs. Compiles into a tidy dataset of OD by time, well, stressor presence/absence and isolate. 
# Also implements a number of counters needed by future for loops.
# TODO: Implement the ability to handle data replicates.
library(dplyr)
library(tidyverse)
library(ggplot2)
library(growthcurver)
library(gridBase)
library(gridExtra)

setwd("C:/Users/Sam Welch/Google Drive/ICL Ecological Applications/Project/Work/Scripts")

# Load in the plate layout csv for combination and isolate location data
plate_layout <- read.csv("Data/Final_Pipeline/256comb_8bact_plate.csv") %>%
  unite(loc, Dest.Row, Dest.Column, sep = "") %>%
  unite(location, loc, plate, sep = ".")

# How many plates are there?
plate_count = 0 

# How many of the growth curves are bad or questionable fits?
bad_fit_count = 0 

# Make a vector of isolates
isolates_vector <- as.vector(unique(plate_layout$Isolate))
isolates_species_vector <- c("KUE4_10 - S. acidaminiphila", "NUE1_1 - B. muralis", "LUF4_5 - L. rhizovicinus", "NUF1_3 - V. paradoxus", "KUB5_13 - V. paradoxus", "KUE4_4 - B. muralis", "E. coli OP50", "Nash's Field Soil Community")
  
# Make a vector of stressors
stressors_vector <- as.vector(colnames(plate_layout[1:8]))
# And a colour vector for consistent colouring
stressor_colours <- c("Copper" = "red3", "Nickel" = "firebrick", "Chloramphenicol" = "plum", "Ampicillin" = "plum4", "Atrazine" = "darkgreen", "Metaldehyde" = "forestgreen", "Tebuconazole" = "steelblue", "Azoxystrobin" = "lightblue3", "None" = "black")

# Load in plate .CSVs from a seperate folder using a for loop. Make a tibble to contain the data.
setwd("C:/Users/Sam Welch/Google Drive/ICL Ecological Applications/Project/Work/Scripts/Data/Final_Pipeline/Run_2/csvs")
# Make sure your plates are correctly ordered in the wd. You will need leading 0s on your plate numbers for the below loop to read them in order.
tidy_data <- tibble()

for (k in 1:length(dir()))
{
  # Make a df for each plate with a numbered name
  temp_plate_name <- paste("plate", str_pad(k, 2, pad = "0") ,sep = "_") # pad plate names with leading 0s so R orders them properly
  temp_plate_df <- read.csv(dir()[k])
  colnames(temp_plate_df)[1] <- "time" # fix a pesky capitalisation mismatch
  temp_plate_df$time <- as.numeric(substr(temp_plate_df$time, 1, 2)) # turn the reader's odd time format in to something useful
  temp_plate_width <- 97 # hacky way to get gather on line 42 to work properly
  # we need to remove the last 32 wells from every third plate. This is complicated because it's plates 9,10,11 & 12...
  if ((k %% 3) == 0)
  {
    temp_plate_df <- temp_plate_df %>%
      select(-contains("9")) %>%
      select(-contains("10")) %>%
      select(-contains("11")) %>%
      select(-contains("12"))
    temp_plate_width <- 65 # if it's a third plate it'll have 64 wells (and thus 65 columns including time)
  }
  assign(temp_plate_name, temp_plate_df)                         # turn our temporary df into a real df with a for-loop-generated name
  
  # let's also make a massive tidy dataset that's better able to store stressor presence/absence and isolate species
  temp_plate_width <- 
  temp_plate_tidy <- as.tibble(temp_plate_df) %>%
    gather(well, OD, 2:temp_plate_width) %>%                       # gather the data from wide to tall
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
timepoints_count <- length(unique(tidy_data$time))  # should be 49 
# How many plates
plate_count

