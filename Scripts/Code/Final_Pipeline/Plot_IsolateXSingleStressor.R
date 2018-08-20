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
library(RColorBrewer)
setwd(here("scripts"))
source('Code/Final_Pipeline/Function_Aggregate_Fun_Groups.R')

# A colour vector for consistent colouring. Uses Brewer's "Paired" palette, plus black for control.
stressor_colours <- c("Copper" = "#A6CEE3", "Nickel" = "#1F78B4", "Chloramphenicol" = "#B2DF8A", "Ampicillin" = "#33A02C", "Atrazine" = "#FB9A99", "Metaldehyde" = "#E31A1C", "Tebuconazole" = "#FDBF6F", "Azoxystrobin" = "#FF7F00", "Control" = "#000000")

# We can graph the effects of different single stressors on bacteria. For instance:
isolate_single_stress <- tidy_data %>%
  filter((Copper + Nickel + Chloramphenicol + Ampicillin + Atrazine + Metaldehyde + Tebuconazole + Azoxystrobin) <= 1) %>%
  select(-location) %>%
  mutate(Stressor = "Control")

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
   summarise(Mean_OD = mean(OD), SD_OD = sd(OD), n = n()) %>%
   distinct() 

# What happens if we gate it down to every 4th point?
isolate_single_stress <- isolate_single_stress %>%
  filter((time %% 4 == 0))

# for loop across the 8 isolates to produce a 4x2 lattice of graphs
for (o in 1:8)
{
  temp_isolate <- isolates_vector[o]
  temp_plot <- ggplot(filter(isolate_single_stress, Isolate == temp_isolate), aes(time, Mean_OD)) +
    ylim(0,0.5) +
    # geom_errorbar(aes(colour = Stressor, ymin = Mean_OD - SD_OD, ymax = Mean_OD + SD_OD), position = position_dodge(width = 5)) +
    geom_smooth(aes(colour = Stressor), se = FALSE, method = "loess") +
    ggtitle(isolates_species_vector[o]) +
    theme_grey() +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position = "none") +
    scale_colour_manual(labels = c("Copper", "Nickel", "Chloramphenicol", "Ampicillin", "Atrazine", "Metaldehyde", "Tebuconazole", "Azoxystrobin", "Control"),
                        values = unname(stressor_colours)) +
    # Add a control line
    geom_hline(yintercept = 0.05, colour = "grey")
  #scale_colour_manual(values = stressor_colours)
  temp_plot_name <- paste("p", o ,sep = "")
  assign(temp_plot_name, temp_plot)   
}

# Plot a dummy graph so we can get a legend.
grob_leg <- get_legend(p1 + theme(legend.position = "left")) 

# Arange the plots with a shared legend
ss_plots <- ggarrange(p2, p4, p1, p7, p6, p5, p3, p8, grob_leg, ncol = 3, nrow = 3)
# Print to PDF
pdf("Results/Final_Pipeline/single_stressor_plots.pdf", width = 9, height = 9) # setting onefile to false prevents a blank leading page
annotate_figure(ss_plots,
                left = text_grob("OD at 590 nm", rot = 90),
                bottom = text_grob("Time (h)"))
dev.off()
dev.off()

setwd(here())