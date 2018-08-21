# Modifies tidier_growth_data to create Piggott et al. interaction definitions for every combination of stressors by isolate
# Also converts presence/absence data into columns for S1, S2, S3, etc
# Requires a whole buncha stuff.

library(dplyr)
library(tibble)
library(tidyr)
library(ggplot2)
library(growthcurver)
library(data.table)
library(viridis)
library(forcats) # sadly nothing to do with cats
library(ggpubr)
library(here)


setwd(here("Scripts"))
source("Code/Final_Pipeline/t.test2.R")
source("Code/Final_Pipeline/Function_Aggregate_Fun_Groups.R")

# Set your p-cutoff for additivism here
p_cutoff <- 0.05

# Calculate the effect of individual stressors relative to controls, by isolate
all_interactions_tibble <- tibble()
# A big single stressor tibble that we can use in other scripts
big_single_stressor_data <- tibble()

for (s in 1:8)
{
  # Create a tibble of all measurements for our chosen isolate
  isolate_tidier_growth_data <- tidier_growth_data %>%
    select(Copper, Nickel, Chloramphenicol, Ampicillin, Metaldehyde, Atrazine, Tebuconazole, Azoxystrobin, Isolate, Mean, SD, Complexity, n) %>%
    filter(Isolate == isolates_vector[s]) 
  
  # Calculate a baseline from controls
  control_baseline <- isolate_tidier_growth_data %>%
    filter(Complexity == 0) %>%
    select(Isolate, Mean, SD, n)
  
  # A tibble of single stressors
  single_stressor_growth_data <- isolate_tidier_growth_data %>%
    filter(Complexity == 1) %>%
    mutate(mean_effect = Mean - control_baseline$Mean) %>%
    mutate(sd_effect = sqrt((SD ^ 2) + (control_baseline$SD ^ 2))) %>% # New SD is the square root of the summed squared SDs
    mutate(n_effect = n) %>%
    mutate(Stressor = "")
  # This breaks a lot currently as I don't always have a measurement for every single stressor/isolate combination
  
  # Loop across the single stressor info to make sure they all get the right stressor assigned to the right effect 
  for (p in 1:nrow(single_stressor_growth_data))
  {
    for (q in 1:8)
    {
      if (single_stressor_growth_data[p,q] == 1)
      {
        single_stressor_growth_data$Stressor[p] <- colnames(single_stressor_growth_data[q])
      }
    }
  }
  
  # get rid of presence/absence columns
  single_stressor_growth_data <- single_stressor_growth_data %>%
    select(-Copper, -Nickel, -Chloramphenicol, -Ampicillin, -Metaldehyde, -Atrazine, -Tebuconazole, -Azoxystrobin, -Mean, -SD, -Complexity, -n)
  
  # This ugly boy here reorders the single stressor data to stressors_vector so I don't accidentally predict using the wrong data!
  single_stressor_growth_data$Stressor <- factor(single_stressor_growth_data$Stressor, levels = stressors_vector)
  single_stressor_growth_data <- single_stressor_growth_data[order(single_stressor_growth_data$Stressor),]
  big_single_stressor_data <- bind_rows(big_single_stressor_data, single_stressor_growth_data)
  
  # And all mixtures
  mixture_tidier_growth_data <- isolate_tidier_growth_data %>%
    filter(Complexity > 1) %>%
    mutate(obs_mean = Mean - control_baseline$Mean) %>%
    mutate(obs_sd = sqrt((SD ** 2) + (control_baseline$SD ** 2))) %>%
    mutate(obs_n = n) %>%
    mutate(pred_mean = 0) %>%
    mutate(pred_sd = 0) %>%
    mutate(pred_n = 0) %>%
    mutate(Interaction = "") %>%
    select(-Mean, -SD, -n)
  
  # For loop across all the mixtures 
  for (p in 1:nrow(mixture_tidier_growth_data))
  {
    mixture_counter <- 0
    temp_stressors <- vector(mode = "numeric", length = 8)
    for (q in 1:8)
    {
      if ((mixture_tidier_growth_data[p,q] == 1) && (mixture_counter < mixture_tidier_growth_data$Complexity[p]))
      {
        # If a stressor is present, add the relevant means and sd from single_stressor_growth_data
        # We're summming means here because we're calculating an additive effect
        mixture_tidier_growth_data$pred_mean[p] <- mixture_tidier_growth_data$pred_mean[p] + single_stressor_growth_data$mean_effect[q]
        mixture_tidier_growth_data$pred_sd[p] <- sqrt((mixture_tidier_growth_data$pred_sd[p] ^ 2) + (single_stressor_growth_data$sd_effect[q] ^ 2))
        mixture_tidier_growth_data$pred_n[p] <- mixture_tidier_growth_data$pred_n[p] + single_stressor_growth_data$n_effect[q]
        mixture_counter <- mixture_counter + 1
        # And append the stressor effect to temp_stressors so we can calculate the regions of Piggott synergy and antagonism
        temp_stressors[q] <- single_stressor_growth_data$mean_effect[q]
      } 
      if (mixture_counter == mixture_tidier_growth_data$Complexity[p])
      {
        # Once the additive mean and sd are calculated, we can compare the two to determine the interaction type. This is where it gets complicated.
        
        # Divide the pred_n by richness and round it up
        mixture_tidier_growth_data$pred_n[p] <- ceiling((mixture_tidier_growth_data$pred_n[p])/(mixture_tidier_growth_data$Complexity[p]))
        
        # Now we need to t-test the observed vs predicted effect. We'll be rejecting the null hypothesis for p < 0.05
        additive_test <- t.test2(mixture_tidier_growth_data$pred_mean[p], mixture_tidier_growth_data$obs_mean[p], 
                                 mixture_tidier_growth_data$pred_sd[p], mixture_tidier_growth_data$obs_sd[p],
                                 mixture_tidier_growth_data$pred_n[p], mixture_tidier_growth_data$obs_n[p])
        # We also need to catch NaNs and NAs
        # Set some upper and lower bounds of synergies for cleaner code
        upper_bound <- max(mixture_tidier_growth_data$pred_mean[p], temp_stressors, 0)
        lower_bound <- min(mixture_tidier_growth_data$pred_mean[p], temp_stressors, 0)
        if (is.na(additive_test[4]) || is.nan(additive_test[4]))
          {
            mixture_tidier_growth_data$Interaction[p] <- "T-test error"
          } else if (additive_test[4] >= p_cutoff)
          {
            mixture_tidier_growth_data$Interaction[p] <- "Additive"
          } else if (mixture_tidier_growth_data$obs_mean[p] > upper_bound)
          {
            mixture_tidier_growth_data$Interaction[p] <- "+ Synergy"
          } else if (mixture_tidier_growth_data$obs_mean[p] < lower_bound)
          {
            mixture_tidier_growth_data$Interaction[p] <- "- Synergy"
          } else if (mixture_tidier_growth_data$obs_mean[p] < mixture_tidier_growth_data$pred_mean[p])
          {
            mixture_tidier_growth_data$Interaction[p] <- "- Antagonism"
          } else if (mixture_tidier_growth_data$obs_mean[p] > mixture_tidier_growth_data$pred_mean[p])
          {
            mixture_tidier_growth_data$Interaction[p] <- "+ Antagonism"
          } else
          {
            mixture_tidier_growth_data$Interaction[p] <- "Classification Error"
          }
        # This is crude, I think it works.
          mixture_counter <- 0
        }
      }
    }
    all_interactions_tibble <- bind_rows(all_interactions_tibble, mixture_tidier_growth_data)
    
    # Plot a rather ugly bar chart of interactions by richness, by isolate
    
    interaction_order <- c("+ Synergy", "- Antagonism", "Additive", "+ Antagonism", "- Synergy", "T-test error") 
    
    all_interactions_tibble$Interaction <- factor(all_interactions_tibble$Interaction, levels = interaction_order)
    
    temp_plot <- 
      ggplot(data = filter(all_interactions_tibble, Isolate == isolates_vector[s]), 
             aes(x = as.factor(Complexity), 
                 fill = Interaction,
                 width = 0.9)) +
      
      scale_colour_viridis_d(aesthetics = "fill", 
                             option = "viridis", 
                             direction = -1, 
                             drop = FALSE) +
      
      geom_bar(position = "stack") +
      
      ggtitle(paste(isolates_species_vector[s])) +
      theme_gray() +
      theme(legend.position = "none",
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.grid.major.x = element_blank())
    
    temp_plot_name <- paste("p", s , sep = "")
    assign(temp_plot_name, temp_plot)
}
dummy_legend <- get_legend(
  temp_plot +
    theme(legend.position = "right")
)

setwd(here("Scripts"))

pdf("Results/Final_Pipeline/histogram_interaction_basic.pdf", width = 9, height = 9)
annotate_figure(ggarrange(p1, p2, p3, p4, p5, p6, p7, p8, dummy_legend),
left = text_grob("Count", rot = 90),
bottom = text_grob("Mixture Complexity"))
dev.off()
dev.off()
