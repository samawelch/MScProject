# Take tidy_growth_data from Growth_Curve_Loop and get cracking on some means and standard deviations
# TODO: Be really, really sure about the difference between SE and SD and which is more appropriate to use here

library(dplyr)
library(tibble)
library(tidyr)
library(ggplot2)
library(growthcurver)
library(gridBase)
library(gridExtra)
library(growthmodels)

setwd(here("Scripts"))

#######################
### SET INPUTS HERE ###
#######################

# Pick a growth metric. This will be used in all dependent scripts, so choose carefully...
growth_metric <- "Growth_auc_e"
# (UQ(rlang::sym(growth_metric))) notation is used to cleanly insert your chosen growth metric into data manipulation functions

#######################
##### Main Script #####
#######################

# Make a new tibble with a clever name. And a measure of stressor richness.
tidier_growth_data <-
  tidy_growth_data %>%
  select(location, Growth_auc_e, Growth_auc_l, Growth_k, Growth_r, Growth_n0, Growth_sigma, Fit_notes, Copper, Nickel, Chloramphenicol, Ampicillin, Atrazine, Metaldehyde, Tebuconazole, Azoxystrobin, Isolate) %>%
  mutate(Richness = Copper + Nickel + Chloramphenicol + Ampicillin + Atrazine + Metaldehyde + Tebuconazole + Azoxystrobin)

# Pull out our controls so we can average them by Isolate rather than location
temp_control_means <- tidier_growth_data %>%
  filter(Richness == 0) %>%
  select(-location) %>%
  group_by(Isolate) %>%
  mutate(Mean = mean(UQ(rlang::sym(growth_metric)))) %>%
  mutate(SD = sd(UQ(rlang::sym(growth_metric)))) %>%
  mutate(n = n()) %>%
  select(-Growth_auc_e, -Growth_auc_l, -Growth_k, -Growth_r, -Growth_n0, -Growth_sigma, -Fit_notes) %>%
  distinct() %>% # TODO:  I still don't know a better way to average across rows in one fell swoop
  ungroup()

# Average the remaining treatment wells by location
tidier_growth_data <- tidier_growth_data %>%
  mutate(Richness = Copper + Nickel + Chloramphenicol + Ampicillin + Atrazine + Metaldehyde + Tebuconazole + Azoxystrobin) %>%
  filter(Richness != 0) %>%
  group_by(location) %>%
  mutate(Mean = mean(UQ(rlang::sym(growth_metric)))) %>%
  mutate(SD = sd(UQ(rlang::sym(growth_metric)))) %>%
  mutate(n = n()) %>%
  select(-Growth_auc_e, -Growth_auc_l, -Growth_k, -Growth_r, -Growth_n0, -Growth_sigma, -Fit_notes) %>%
  distinct() %>% 
  ungroup() %>%
  select(-location)

# And merge them back together! Which now that I come to think of it may not be necessary?
tidier_growth_data <- bind_rows(tidier_growth_data, temp_control_means)

# How many problems are we likely to have with poor replication (spoiler: lots)
paste("bad wells", nrow(filter(tidier_growth_data, n == 1)))
paste("total wells", nrow(tidier_growth_data))