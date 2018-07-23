# Modifies tidy_growth_data to produce datasets of binary stressor interaction effect vs predicted effect (additive null hypothesis)
# Prints bubble plots per isolate to pdf depicting size and duration of interactions by isolate
# Requires Well_Data_Loader.R, Growth_Curve_Loop.R, and grid_arrange_shared_legend.R
# TODO: Add a properly defined concept of additivism vs synergism vs antagonism
# TODO: Work out how to measure and represent the statistical validity of these things
# TODO: Add Piggott and Schafer's updaded interaction defintions
setwd("C:/Users/Sam Welch/Google Drive/ICL Ecological Applications/Project/Work/Scripts")

library(dplyr)
library(tidyverse)
library(ggplot2)
library(growthcurver)
library(gridBase)
library(gridExtra)

# What metric of growth do we want to use for our comparison? Make sure it's included in tidy_growth_data!
growth_metric <- "Growth_r"

# How can we look for additivism vs synergism and antagonism? which is the whole point...
# And generalise the specific implementation with a massive for loop
for (s in 1:8)
{
  example_stressor_growth_data <- tidy_growth_data %>%
    mutate(Richness = Copper + Nickel + Chloramphenicol + Ampicillin + Atrazine + Metaldehyde + Tebuconazole + Azoxystrobin) %>%
    select(Copper, Nickel, Chloramphenicol, Ampicillin, Metaldehyde, Atrazine, Tebuconazole, Azoxystrobin, Isolate, growth_metric, Fit_notes, Richness) %>%
    filter(Richness <= 2) %>%
    filter(Isolate == isolates_vector[s])
  
  # Calculate a baseline from controls
  control_baseline <- as.numeric(example_stressor_growth_data %>%
                                   filter(Richness == 0) %>%
                                   summarise_at(growth_metric,funs(mean)))
  
  # A tibble of single stressors
  single_stressor_growth_data <- example_stressor_growth_data %>%
    filter(Richness == 1) %>%
    mutate(observed_effect := UQ(rlang::sym(growth_metric)) - control_baseline) %>% # What a lot of extra stuff to get growth_metric to update dynamically...
    select(observed_effect) %>%
    mutate(stressor = as.list((stressors_vector)))
  
  # And binary mixtures
  binary_stressor_growth_data <- example_stressor_growth_data %>%
    filter(Richness == 2) %>%
    mutate(observed_effect := UQ(rlang::sym(growth_metric)) - control_baseline) %>%
    mutate(predicted_effect = 0) %>%
    mutate(S1 = "") %>%
    mutate(S2 = "")
  
  # Can we for loop through the binary mixtures?
  for (p in 1:nrow(binary_stressor_growth_data))
  {
    binary_counter = 1
    for (q in 1:8)
    {
      if (binary_stressor_growth_data[p,q] == 1)
      {
        # If a stressor is present, add the relevant effect from single_stressor_growth_data
        binary_stressor_growth_data$predicted_effect[p] <- binary_stressor_growth_data$predicted_effect[p] + single_stressor_growth_data$observed_effect[q]
        # And assign it to either S1 or S2 depending on if it's the first or second stressor
        if (binary_counter == 1)
        {
          binary_stressor_growth_data$S1[p] <- colnames(binary_stressor_growth_data[q])
          binary_counter = binary_counter + 1
        }
        else
        {
          binary_stressor_growth_data$S2[p] <- colnames(binary_stressor_growth_data[q])
        }
      }
    }
  }
  
  interaction_binary_stressor_growth_data <- binary_stressor_growth_data %>%
    mutate(delta_growth = observed_effect - predicted_effect) %>%
    mutate(interaction = "") %>%
    select(observed_effect, predicted_effect, S1, S2, delta_growth, interaction)
  
  # Loop across the rows of interaction_binary_stressor_growth_data, assigning interaction categories by the direction and magnitude of delta_growth
  for (r in 1:nrow(interaction_binary_stressor_growth_data))
  {
    if (interaction_binary_stressor_growth_data$delta_growth[r] > 1) 
    {
      interaction_binary_stressor_growth_data$interaction[r] <- "Synergistic"
    }
    else if (interaction_binary_stressor_growth_data$delta_growth[r] < -1) 
    {
      interaction_binary_stressor_growth_data$interaction[r] <- "Antagonistic"
    }
    else
    {
      interaction_binary_stressor_growth_data$interaction[r] <- "Additive"
    }
  }
  
  # Make sure our stressor levels are ordered properly
  interaction_binary_stressor_growth_data$S1 <- factor(interaction_binary_stressor_growth_data$S1, levels = stressors_vector)
  interaction_binary_stressor_growth_data$S2 <- factor(interaction_binary_stressor_growth_data$S2, levels = stressors_vector)
  
  # And generate our plots
  temp_plot <- ggplot(interaction_binary_stressor_growth_data) +
    geom_point(aes(x = S1, y = S2, size = abs(delta_growth), colour = interaction))  +
    geom_text(aes(x = S1, y = S2, label = round(delta_growth, digits = 2), size = 0.5), colour = "black", show.legend = FALSE) +
    scale_size_continuous(limit = c(0,20), range = c(1,15)) +
    ggtitle(isolates_species_vector[s]) +
    theme(axis.title.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank()) +
    scale_x_discrete(labels = abbreviate, limits = stressors_vector, position = "top") +
    scale_y_discrete(labels = abbreviate, limits = rev(stressors_vector)) +
    scale_colour_manual(values = c(Additive = "#619CFF", Antagonistic = "#F8766D", Synergistic = "#00BA38")) # TODO: Make a proper vector of interactions
  
  temp_plot_name <- paste("bubble", s , sep = "")
  assign(temp_plot_name, temp_plot)
}

# Print bubble1 to bubble8 in a grid with a shared legend
bs_plots <- annotate_figure(
            ggarrange(
            bubble1, bubble2, bubble3, bubble4, bubble5, bubble6, bubble7, bubble8,
            ncol = 4, 
            nrow = 2,
            common.legend = TRUE,
            legend = "bottom"), 
            top = growth_metric)

pdf("Results/Final_Pipeline/binary_bubble_interactions.pdf", width = 16, height = 8, onefile = FALSE) # setting onefile to false prevents a blank leading page
bs_plots
dev.off()

