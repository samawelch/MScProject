# A test for emergent interactions, using Beppler's measurement of bacterial fitness to predict complex interactions from component interactions
# The magnum opus of my 'throw a lot of for loops at a wall and see what sticks' approach to programming in R

library(dplyr)
library(rlang)
library(here)

setwd(here("Scripts"))

# We essentially want to run Define_Interaction.R, but rather than simply using single stressor effects to predict growth
# We want to use the sum of all (component effects at (richness - 1) divided by richness)
# I'm not planning to use all component levels of richness to try and predict interactions, because life's too short

# Set your p-cutoff for additivism here
p_cutoff <- 0.05

# We'll need this vector for filtering component combinations later
temp_filter_vector <- vector(length = 8, mode = "character")

# Calculate the effect of individual stressors relative to controls, by isolate
emergent_interactions_tibble <- all_interactions_tibble %>%
  mutate(pred_comp_mean = 0, pred_comp_sd = 0, pred_comp_n = 0, pred_comp_interaction = "")

# This is ugly. 
emergent_interactions_tibble_app <- 
  emergent_interactions_tibble[0,] %>%
  mutate(pred_comp_interaction = "")

# By isolate
for (i in 1:8)
{
  for (o in 3:8)
  {
    # For a given higher-order (n > 2) mixture, filter the tibble down
    temp_ho_emergent_interaction_tibble <- emergent_interactions_tibble %>%
      filter(Complexity == o) %>% # in retrospect Complexity == o is not very human-friendly
      filter(Isolate == isolates_vector[i])
    
    # Also generate a tibble of n-1 order interactions
    temp_component_emergent_interaction_tibble <- emergent_interactions_tibble %>%
      filter(Complexity == o-1) %>%
      filter(Isolate == isolates_vector[i])
    
    # Now let's work out way through the combinations
    for (r in 1:nrow(temp_ho_emergent_interaction_tibble))
    {
      # loop across stressor presence/absence data. append the names of any absent stressors to a vector we'll be using for filtering
      for (q in 1:8)
      {
        if (temp_ho_emergent_interaction_tibble[r,q] == 1)
        {
          temp_filter_vector[q] <- colnames(temp_ho_emergent_interaction_tibble[q])
        }
      }
      # Now that we've got a vector of all the stressors we care about, we can use it to filter the Complexity == (n-1) dataset
      # Remove empty elements
      temp_filter_vector <- temp_filter_vector[temp_filter_vector != ""]
      # paste paste paste
      temp_filter <- paste("(", paste(paste("(", temp_filter_vector, " == 1", ")", sep = ""), collapse = " | "), ")", sep = "")
      # And give it a stupid name
      component_emergent_filtered_interaction_tibble <- temp_component_emergent_interaction_tibble %>%
        filter_(temp_filter) 
      # filter_() is technically deprecated; however I am practically unable to understand quasiquotation though, so here it stays
      # an elegant function for a more civilized age
        
      # At some point when we're done we'll need to empty this vector again
      temp_filter_vector[1:8] <- ""
      
      # We can now calculate a pred_comp_mean from the divided sum of our component obs_means, likewise for n and sd
      temp_ho_emergent_interaction_tibble$pred_comp_mean[r] <-  
        as.numeric(summarize(component_emergent_filtered_interaction_tibble, 
                             adjusted_mean = (sum(obs_mean)/nrow(component_emergent_filtered_interaction_tibble))))
      # I believe here the standard deviation should be the equal to the square root of the sum of (the squared component SDs) divided by the number of component combinations
      temp_ho_emergent_interaction_tibble$pred_comp_sd[r] <-  
        as.numeric(summarize(component_emergent_filtered_interaction_tibble, 
                             adjusted_sd = sqrt(sum((obs_mean ^ 2)))/nrow(component_emergent_filtered_interaction_tibble)))
      # And our new sample size is summed, divided by the number of component combinations, and rounded to the nearest whole number
      temp_ho_emergent_interaction_tibble$pred_comp_n[r] <-  
        as.numeric(summarize(component_emergent_filtered_interaction_tibble, 
                             adjusted_n = round(sum(obs_n) / nrow(component_emergent_filtered_interaction_tibble))))
    
      # And now we're gonna T-test observed effect vs component predicted effect
      # I really need to come up with better names for these things.
      
      # Now we need to t-test the observed vs predicted effect. We'll be rejecting the null hypothesis for p < 0.05
      # If m1 > m2, then additive_test[1] > 0 
      additive_test <- t.test2(temp_ho_emergent_interaction_tibble$pred_comp_mean[r], temp_ho_emergent_interaction_tibble$obs_mean[r], 
                               temp_ho_emergent_interaction_tibble$pred_comp_sd[r], temp_ho_emergent_interaction_tibble$obs_sd[r],
                               temp_ho_emergent_interaction_tibble$pred_comp_n[r], temp_ho_emergent_interaction_tibble$obs_n[r])
      # We also need to catch NaNs and NAs
      # We'll just use simple definitions of synergy and antagonism here.
      if (is.na(additive_test[4]) || is.nan(additive_test[4]))
      {
        temp_ho_emergent_interaction_tibble$pred_comp_interaction[r] <- "T-test error"
      } else if (additive_test[4] >= p_cutoff)
      {
        temp_ho_emergent_interaction_tibble$pred_comp_interaction[r] <- "Predicted"
      } else if (additive_test[1] < 0)
      {
        temp_ho_emergent_interaction_tibble$pred_comp_interaction[r] <- "Emergent Synergy"
      } else if (additive_test[1] > 0)
      {
        temp_ho_emergent_interaction_tibble$pred_comp_interaction[r] <- "Emergent Antagonism"
      } else
      {
        temp_ho_emergent_interaction_tibble$pred_comp_interaction[r] <- "Classification Error"
      }
      # And append to emergent_interactions_tibble_app
      emergent_interactions_tibble_app <- bind_rows(emergent_interactions_tibble_app, temp_ho_emergent_interaction_tibble[r,])
    }
  }
  # Plot time!
  interaction_order <- c("Emergent Synergy", "Predicted", "Emergent Antagonism", "T-test error") 
  
  emergent_interactions_tibble_app$pred_comp_interaction <- factor(emergent_interactions_tibble_app$pred_comp_interaction, levels = interaction_order)
  
  temp_plot <- 
    ggplot(data = filter(emergent_interactions_tibble_app, Isolate == isolates_vector[i]), 
           aes(x = as.factor(Complexity), 
               fill = pred_comp_interaction),
           width = 0.9) +
    
    scale_colour_viridis_d(aesthetics = "fill", 
                           option = "viridis", 
                           direction = -1, 
                           drop = FALSE, 
                           begin = 0,
                           end = 1) +
    
    geom_bar(position = "stack") +
    
    ggtitle(paste(isolates_species_vector[i])) +
    theme_gray() +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major.x = element_blank())
  
  temp_plot_name <- paste("p", i , sep = "")
  assign(temp_plot_name, temp_plot)
}

dummy_legend <- get_legend(
  temp_plot +
    theme(legend.position = "right") +
    labs(fill = 'Emergent Interaction') 
)

pdf("Results/Final_Pipeline/histogram_interaction_emergent.pdf", width = 9, height = 9)
annotate_figure(ggarrange(p1, p2, p3, p4, p5, p6, p7, p8, dummy_legend),
                left = text_grob("Count", rot = 90),
                bottom = text_grob("Mixture Complexity"))
dev.off()
dev.off()
