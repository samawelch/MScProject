# Generate a graphic to depict extended definitions of stressor interactions
# Based on Figure 2 of Piggott, Townsend and Mathaei (2015)
# TODO: Refine these graphics more, whether that means bringing them in line with PGM(2015) or modifying them...

library(dplyr)
library(tibble)
library(tidyr)
library(ggplot2)
library(growthcurver)
library(gridBase)
library(gridExtra)
library(ggpubr)

interaction_order_vector <- c("Control", "A", "B", "Additive", "+ Synergy", "- Synergy", "+ Antagonism", "- Antagonism" )

setwd("C:/Users/Sam Welch/Google Drive/ICL Ecological Applications/Project/Work/Scripts/Data")

data <- read.csv("Expanded_Interaction_Definition_Data.csv")

p1 <- ggplot(data = filter(data, Example == 1), aes(x = Interaction, y = Value)) +
  geom_col(width = 0.7, fill = "#F8766D") +
  ylab(label = "Growth Metric") +
  scale_x_discrete(limits = interaction_order_vector) +
  scale_y_continuous(expand = c(0,0), limits = c(0,40)) +
  geom_text(aes(label = Value), vjust = -0.5) +
  ggtitle(label = "Double Negative Stressor Interactions", subtitle = "Example: Antibacterial x Antibacterial") +
  theme(axis.text.x = element_text(face = "bold", size = 10, angle = 315))
p2 <- ggplot(data = filter(data, Example == 2), aes(x = Interaction, y = Value)) +
  geom_col(width = 0.7, fill = "#C49A00") +
  ylab(label = "Growth Metric") +
  scale_x_discrete(limits = interaction_order_vector) +
  scale_y_continuous(expand = c(0,0), limits = c(0,40)) +
  geom_text(aes(label = Value), vjust = -0.5) +
  ggtitle(label = "Opposing Stressor Interactions", subtitle = "Example: Antibacterial x Food Source") +
  theme(axis.text.x = element_text(face = "bold", size = 10, angle = 315))
p3 <- ggplot(data = filter(data, Example == 3), aes(x = Interaction, y = Value)) +
  geom_col(width = 0.7, fill = "#00BA38") +
  ylab(label = "Growth Metric") +
  scale_x_discrete(limits = interaction_order_vector) +
  scale_y_continuous(expand = c(0,0), limits = c(0,40)) +
  geom_text(aes(label = Value), vjust = -0.5) +
  ggtitle(label = "Double Positive Stressor Interactions", subtitle = "Example: Food Source x Food Source") +
  theme(axis.text.x = element_text(face = "bold", size = 10, angle = 315))

setwd("C:/Users/Sam Welch/Google Drive/ICL Ecological Applications/Project/Work/Scripts/Results")

png("Plot_Interactions_Illustrative.png", height = 1200, width = 1200)
ggarrange(p1,p2,p3, ncol = 2, nrow = 2)
dev.off()
