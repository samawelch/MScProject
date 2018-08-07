# Modifies tidy_data to produce isolate_single_stress, which is limited to growth data by isolate by stressor. Prints to pdf.
# Requires Well_Data_Loader.R, and Growth_Curve_Loop.R

library(dplyr)
library(tibble)
library(tidyr)
library(ggplot2)
library(growthcurver)
library(gridBase)
library(gridExtra)
library(ggpubr)
library(cowplot)
setwd(here("scripts"))
source('Code/Final_Pipeline/Function_Aggregate_Fun_Groups.R')

# A colour vector for consistent colouring
stressor_colours <- c("Copper" = "red", "Nickel" = "darkred", "Chloramphenicol" = "darkmagenta", "Ampicillin" = "magenta", "Atrazine" = "green", "Metaldehyde" = "darkgreen", "Tebuconazole" = "blue", "Azoxystrobin" = "darkblue", "None" = "black")

# We can graph the effects of different single stressors on bacteria. For instance:
isolate_single_stress <- tidy_data %>%
  filter((Copper + Nickel + Chloramphenicol + Ampicillin + Atrazine + Metaldehyde + Tebuconazole + Azoxystrobin) <= 1) %>%
  select(-location) %>%
  mutate(Stressor = "None")

# A for loop to turn presence/absence data into a single variable (only works for single stressors). I am not proud of how long this took to implement.
for (m in 1:nrow(isolate_single_stress))
{
  for (n in 1:8)
  {
    if (isolate_single_stress[m,n+3] == 1)
    {
      isolate_single_stress$Stressor[m] = colnames(isolate_single_stress[,n+3])
    }
  }
}

# Get rid of the presence/absence stressor data
isolate_single_stress <- isolate_single_stress %>%
  select(-Copper, -Nickel, -Chloramphenicol, -Ampicillin, -Metaldehyde, -Atrazine, -Tebuconazole, -Azoxystrobin, -Run)

# Calculates one mean and SD for every combination of isolate, stressor and time point
isolate_single_stress <- isolate_single_stress %>%
  group_by(Isolate, Stressor, time) %>%
  summarise(Mean_OD = mean(OD), SD_OD = sd(OD)) %>%
  distinct() %>%
  filter(time < 26)

# for loop across the 8 isolates to produce a 4x2 lattice of graphs
# TODO: still needs logistic curves fitted
for (o in 1:8)
{
  temp_isolate <- isolates_vector[o]
  temp_plot <- ggplot(filter(isolate_single_stress, Isolate == temp_isolate), aes(time, Mean_OD)) +
    geom_point(aes(colour = Stressor), size = 1, shape = 16, alpha = 1) +
    scale_colour_manual(values = stressor_colours) +
    theme(legend.position="none") +
    ylim(0.04,0.35) +
    scale_shape_identity() +
    geom_smooth(aes(colour = Stressor), method="loess", se = FALSE) +
    ggtitle(isolates_species_vector[o]) +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank()) +
    # Add a control line
    geom_hline(yintercept = 0.05, colour = "grey")
  #scale_colour_manual(values = stressor_colours)
  temp_plot_name <- paste("p", o ,sep = "")
  assign(temp_plot_name, temp_plot)   
}

# Plot a dummy graph so we can get a legend.
dummy_plot <- ggplot(filter(isolate_single_stress, Isolate == temp_isolate), aes(time, Mean_OD)) +
  geom_point(aes(colour = Stressor), size = 1, shape = 16, alpha = 1) +
  scale_colour_manual(values = stressor_colours, limits = names(stressor_colours)) +
  scale_shape_identity() +
  geom_smooth(aes(colour = Stressor), method="loess", se = FALSE) +
  geom_hline(yintercept = 0.05, colour = "grey") 


grob_leg <- get_legend(dummy_plot)
# Arange the plots 4x2 with a shared legend
ss_plots <- ggarrange(p2, p4, p1, p7, p6, p5, p3, p8, grob_leg, ncol = 3, nrow = 3)
# Print to PDF
png("Results/Final_Pipeline/single_stressor_plots.png", width = 1500, height = 1500) # setting onefile to false prevents a blank leading page
ss_plots
dev.off()

setwd(here())