# Loops across tidy_data from Well_Data_Loader.R to produce tidy_growth_data, which contains various growth metrics against stressor and isolate data.
# TODO: Save growth curve models with well data for use in future graphics
# TODO: Implement rendering growth curve graphics for all wells.
library(dplyr)
library(tidyverse)
library(ggplot2)
library(growthcurver)
library(gridBase)
library(gridExtra)

setwd("C:/Users/Sam Welch/Google Drive/ICL Ecological Applications/Project/Work/Scripts")

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
                                  Growth_auc_l = temp_data_growth_1$vals$auc_l,
                                  Growth_k = temp_data_growth_1$vals$k, # TODO: Carrying capacity == max growth?
                                  Growth_r = temp_data_growth_1$vals$r,
                                  Growth_n0 = temp_data_growth_1$vals$n0,
                                  Growth_sigma = temp_data_growth_1$vals$sigma,
                                  Fit_notes = temp_data_growth_1$vals$note,
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
  # count bad fits 
  if (temp_data_growth_1$vals$note != "")
  {
    bad_fit_count = bad_fit_count + 1
  }
}
# TODO: Add max growth and max slope here, graph against richness, rate of death (?)

# Just to give an idea of how goo the fits are...
cat("bad fits: ", bad_fit_count)
cat("of total wells: ", wells_count)
