# Modifies tidier_growth_data to create Piggott et al. interaction definitions for every combination of stressors by isolate
# Also converts presence/absence data into columns for S1, S2, S3, etc
# Requires a whole buncha stuff.
# TODO: Make.

library(dplyr)
library(tibble)
library(tidyr)
library(ggplot2)
library(growthcurver)
source("Code/Final_Pipeline/t.test2.R")

# Calculate the effect of individual stressors relative to controls, by isolate
tidier_growth_data

for (s in 1:8)
{
  # Create a tibble of all measurements for our chosen isolate
  isolate_tidier_growth_data <- tidier_growth_data %>%
    select(Copper, Nickel, Chloramphenicol, Ampicillin, Metaldehyde, Atrazine, Tebuconazole, Azoxystrobin, Isolate, Mean, SD, Richness, n) %>%
    filter(Isolate == isolates_vector[s]) 
  
  # Calculate a baseline from controls
  control_baseline <- isolate_tidier_growth_data %>%
    filter(Richness == 0) %>%
    select(Isolate, Mean, SD, n)
  
  # A tibble of single stressors
  single_stressor_growth_data <- isolate_tidier_growth_data %>%
    filter(Richness == 1) %>%
    mutate(mean_effect = Mean - control_baseline$Mean) %>%
    mutate(sd_effect = sqrt((SD ** 2) + (control_baseline$SD ** 2))) %>% # New SD is the square root of the summed squared SDs
    select(mean_effect, sd_effect, n) %>%
    mutate(stressor = as.list((stressors_vector)))
  
  # And all mixtures
  mixture_tidier_growth_data <- isolate_tidier_growth_data %>%
    filter(Richness > 1) %>%
    mutate(obs_mean = Mean - control_baseline$Mean) %>%
    mutate(obs_sd = sqrt((SD ** 2) + (control_baseline$SD ** 2))) %>%
    mutate(obs_n = n)
    mutate(pred_mean = 0) %>%
    mutate(pred_sd = 0) %>%
    mutate(pred_n = 0) %>%
    select(-Mean, -SD, -n)
  
  # For loop across all the mixtures
  for (p in 1:nrow(mixture_tidier_growth_data))
  {
    mixture_counter = 0
    for (q in 1:8)
    {
      if ((mixture_tidier_growth_data[p,q] == 1) && (mixture_counter < mixture_tidier_growth_data$Richness[p]))
      {
        # If a stressor is present, add the relevant means and sd from single_stressor_growth_data
        mixture_tidier_growth_data$pred_mean[p] <- mixture_tidier_growth_data$pred_mean[p] + single_stressor_growth_data$mean_effect[q]
        mixture_tidier_growth_data$pred_sd[p] <- sqrt((mixture_tidier_growth_data$pred_sd[p] ^ 2) + (single_stressor_growth_data$sd_effect[q] ^ 2))
        mixture_counter = mixture_counter + 1
      } else if (mixture_counter = mixture_tidier_growth_data$Richness[p])
      {
        # Once the additive mean and sd are calculated, we can compare the two to determine the interaction type. This is where it gets complicated.
        # First we need to t-test the observed vs predicted effect. We'll be rejecting the null hypothesis for p < 0.05
        t.test2(mixture_tidier_growth_data$pred_mean[p], mixture_tidier_growth_data$obs_mean[p], 
                mixture_tidier_growth_data$pred_sd[p], mixture_tidier_growth_data$sd[p],
                mixture_tidier_growth_data$n[p], mixture_tidier_growth_data$n[p])
        
      }
    }
  }
}

