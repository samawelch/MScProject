# Plot all_interactions_tibble's observed effects against predicted effects.
# Requires a lot of stuff - but most importantly Define_Interactions.R
# Includes viridis for colourblind-friendly (and prettier) colour gradients

library(dplyr)
library(tibble)
library(tidyr)
library(ggplot2)
library(growthcurver)
library(gridBase)
library(gridExtra)
library(ggpubr)
library(viridis)

setwd(here("Scripts"))

# For loop through 2-7 way interactions and graph them out
for (i in 1:8)
{
  interactions_temp <- filter(all_interactions_tibble, Isolate == isolates_vector[i])
  
  plot_i_temp <- ggplot(interactions_temp, aes(x = pred_mean, y = obs_mean)) +
    geom_point(aes(colour = Richness), size = 2, alpha = 0.9, shape = 16) +
    scale_colour_viridis(discrete = FALSE) +
    theme(legend.position="none") +
    geom_abline(slope = 1, intercept = 0, colour = "grey", size = 1) +
    ggtitle(isolates_species_vector[i]) +
    # Consistent scaling isn't great...
    scale_x_continuous(limits = c(-15, 15)) +
    scale_y_continuous(limits = c(-6, 6)) +
    xlab(label = "Predicted Additive Mean") +
    ylab(label = "Observed Mean") +
    annotate("text", x = -13.5, y = 6, label = "Synergy", colour = "grey") +
    annotate("text", x = 12.5, y = -6, label = "Antagonism", colour = "grey")
  
  temp_plot_name <- paste("p", i , sep = "")
  assign(temp_plot_name, plot_i_temp)
}

# Plot together, aligned by species
png("Results/Final_Pipeline/ObservedXPredicted.png", width = 1800, height = 800)
ggarrange(p2, p4, p1, p7, p6, p5, p3, p8, ncol = 4, nrow = 2, common.legend = TRUE, legend = "right")
dev.off()

