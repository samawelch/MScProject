# Modifies tidy_data to produce isolate_single_stress, which is limited to growth data by isolate by stressor. Prints to pdf.
# Requires Well_Data_Loader.R, Growth_Curve_Loop.R, and grid_arrange_shared_legend.R
# TODO: Save growth curve models with well data for use in future graphics
# TODO: Implement rendering growth curve graphics for all wells.
library(dplyr)
library(tidyverse)
library(ggplot2)
library(growthcurver)
library(gridBase)
library(gridExtra)
source('grid_arrange_shared_legend.R')


setwd("C:/Users/Sam Welch/Google Drive/ICL Ecological Applications/Project/Work/Scripts")

# We can also graph the effects of different single stressors on bacteria. For instance:
isolate_single_stress <- tidy_data %>%
  filter((Copper + Nickel + Chloramphenicol + Ampicillin + Atrazine + Metaldehyde + Tebuconazole + Azoxystrobin) <= 1) %>%
  select(-location) %>%
  mutate(Stressor = "None")

# A for loop to turn presence/absence data into a single variable (only works for single stressors). I am not proud of how long this took to implement.
for (m in 1:nrow(isolate_single_stress))
{
  for (n in 1:8)
  {
    if (isolate_single_stress[m,n+2] == 1)
    {
      isolate_single_stress[m,12] = colnames(isolate_single_stress[,n+2])
    }
  }
}

# Get rid of the presence/absence stressor data
isolate_single_stress <- isolate_single_stress %>%
  select(-(3:10))

# for loop across the 8 isolates to produce a 4x2 lattice of graphs
# TODO: add control baselines
for (o in 1:8)
{
  temp_isolate <- isolates_vector[o]
  temp_plot <- ggplot(filter(isolate_single_stress, Isolate == temp_isolate), aes(time, OD)) +
    geom_point(aes(colour = Stressor), size = 1, shape = 16, alpha = 0.5) +
    theme(legend.position="none") +
    ylim(0,0.65) +
    scale_shape_identity() +
    geom_smooth(aes(colour = Stressor), method="loess", se = FALSE) +
    ggtitle(isolates_species_vector[o]) 
  #scale_colour_manual(values = stressor_colours)
  temp_plot_name <- paste("p", o ,sep = "")
  assign(temp_plot_name, temp_plot)   
}

# Arange the plots 4x2 with a shared legend
pdf("Results/Final_Pipeline/single_stressor_plots.pdf", width = 16, height = 8, onefile = FALSE) # setting onefile to false prevents a blank leading page
ss_plots <- grid_arrange_shared_legend(p1,p2,p3,p4,p5,p6,p7,p8,ncol = 4, nrow = 2, position = "right")
dev.off()