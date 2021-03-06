# Loads modified Synergy 2 well data as CSVs from Run_X folders. Compiles into a tidy dataset of OD by time, well, stressor presence/absence and isolate. 
# Also implements a number of counters needed by future for loops. Can (theoretically) handle as many replicates as needed.
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(growthcurver)
library(gridBase)
library(gridExtra)
library(here)
library(stringr)


setwd(here("Scripts")) # Hopefully a slighly more rational way to handle WDs. Run here() for project directory. 

#Start from a tabula rasa
rm(list=ls())

#######################
### SET INPUTS HERE ###
#######################

# How many timepoints does your data have?
read_timepoints <- 49
# This is no longer usable for a bncuh of stuff because runs 4 & 5 only read every 4 hours

# Every how many hours do we want to take a reading?
read_rate <- 4

# Which ones do we actually want to include
runs_vector <- c(2,3,4,5)

# Load in the plate layout csv for combination and isolate location data
plate_layout <- read.csv("Data/Final_Pipeline/256comb_8bact_plate.csv") %>%
  unite(loc, Dest.Row, Dest.Column, sep = "") %>%
  unite(location, loc, plate, sep = ".")

#######################

# Misc counters:

# How many plates are there?
plate_count = 0 

# How many of the growth curves are bad or questionable fits?
bad_fit_count = 0 

# Make a vector of isolates
isolates_vector <- as.vector(unique(plate_layout$Isolate))
isolates_species_vector <- c("KUE4_10 - S. acidaminiphila", "NUE1_1 - B. simplex", "LUF4_5 - L. rhizovicinus", "NUF1_3 - V. paradoxus", "KUB5_13 - V. paradoxus", "KUE4_4 - B. simplex", "E. coli OP50", "Nash's Field Soil Community")
  
# Make a vector of stressors
stressors_vector <- as.vector(colnames(plate_layout[1:8]))
stressors_vector_short <- abbreviate(stressors_vector, minlength = 2)

# Load in plate .CSVs from a seperate folder using a for loop. Make a tibble to contain the data.
# Make sure your plates are correctly ordered in the wd. You will need leading 0s on your plate numbers for the below loop to read them in order.
tidy_data <- tibble()

#######################
##### Main Script #####
#######################

load_run_data <- function(run_number)
{
  # run_number is just the run number, as an integer. Your run folders should be in the format "Run_X", with the .csvs in a nested folder "csvs"
  setwd(here("Scripts", "Data", "Final_Pipeline", paste("Run_", run_number, sep = ""), "csvs"))
  for (k in 1:length(dir()))
  {
    # Make a df for each plate with a numbered name & pad plate names with leading 0s so R orders them properly
    temp_plate_name <- paste("plate", str_pad(k, 2, pad = "0") ,sep = "_")
    temp_plate_df <- read.csv(dir()[k])
    # fix a pesky capitalisation mismatch
    colnames(temp_plate_df)[1] <- "time" 
    # turn the reader's odd time format in to something useful
    temp_plate_df$time <- as.numeric(substr(temp_plate_df$time, 1, 2)) 
    # turn the reader's odd time format in to something useful
    # trim down each well to the number of time points set in read_timepoints
    temp_plate_df <- filter(temp_plate_df, time <= read_timepoints)
    temp_plate_width <- 97
    # we need to remove the last 32 wells from every third plate. This is complicated because it's rows 9,10,11 & 12...
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
      temp_plate_tidy <- as.tibble(temp_plate_df) %>%
      gather(well, OD, 2:temp_plate_width) %>%                       # gather the data from wide to tall
      mutate(plate = k) %>%                            # add a plate column based on where we are in the for loop
      unite(location, well, plate, sep = ".") %>%      # bring the location naming scheme in line with plate_layout
      mutate(Run = run_number)
      
    tidy_data <<- bind_rows(tidy_data, temp_plate_tidy)       # add the temporary data to our massive dataset (global assign as we're now in a function)
    plate_count <<- plate_count + 1
  }
}

# Load run data per run and append it to tidy_data. Starts at 2, because Run 1 was a write-off.
for (r in runs_vector)
{
  load_run_data(r)
}

setwd(here("Scripts"))

# Join isolate/stressor data to growth data by observations
tidy_data <- left_join(tidy_data, plate_layout, by = "location")

# If we filter reads down to every 4 hours here we can save some thinking in Growth_Curve_Loop.R
tidy_data <- tidy_data %>%
  filter((time %% read_rate) == 0) 
# Now:
timepoints_count <- 13

# A few checks
glimpse(tidy_data)
# How many time points do we have?
timepoints_count <- length(unique(tidy_data$time))  # should be 49 
timepoints_count
# How many wells do we have? this is wrong for runs 4 & 5
wells_count <- nrow(tidy_data) / timepoints_count # run count * 2144
wells_count
# How many plates
plate_count

setwd(here())