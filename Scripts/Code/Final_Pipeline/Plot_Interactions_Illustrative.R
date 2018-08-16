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
library(here)
library(viridis)

interaction_order_vector <- c("A", "B", "Additive", "+S", "-S", "+A", "-A" )

setwd(here("Scripts"))

data <- read.csv("Data/Expanded_Interaction_Definition_Data.csv")

data1 <- filter(data, Example == 1)

p1 <- ggplot(data = data1) +
  geom_rect(aes(ymin = ymin, ymax = ymax, xmax = ID - 0.45, xmin = ID + 0.45, fill = Interaction)) +
  ylab(label = "Response") +
  scale_x_discrete(limits = interaction_order_vector) +
  scale_color_viridis_d(aesthetics = "fill") +
  scale_y_continuous(expand = c(0,0), limits = c(-5.5,5.5)) +
  theme(panel.grid.major.x = element_blank()) +
  ggtitle(label = "Double Negative Stressor Interactions", subtitle = "Example: Antibacterial x Antibacterial") +
  theme(axis.text.x = element_text(size = 10, angle = 0),
        legend.position = "none",
        axis.title.y = element_blank())

data2 <- filter(data, Example == 2)

p2 <- ggplot(data = data2) +
  geom_rect(aes(ymin = ymin, ymax = ymax, xmax = ID - 0.45, xmin = ID + 0.45, fill = Interaction)) +
  ylab(label = "Response") +
  scale_x_discrete(limits = interaction_order_vector) +
  scale_color_viridis_d(aesthetics = "fill") +
  scale_y_continuous(expand = c(0,0), limits = c(-5.5,5.5)) +
  theme(panel.grid.major.x = element_blank()) +
  ggtitle(label = "Opposing Stressor Interactions", subtitle = "Example: Antibacterial x Food Source") +
  theme(axis.text.x = element_text(size = 10, angle = 0),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank())

data3 <- filter(data, Example == 3)

p3 <- ggplot(data = data3) +
  geom_rect(aes(ymin = ymin, ymax = ymax, xmax = ID - 0.45, xmin = ID + 0.45, fill = Interaction)) +
  ylab(label = "Response") +
  scale_x_discrete(limits = interaction_order_vector) +
  scale_color_viridis_d(aesthetics = "fill") +
  scale_y_continuous(expand = c(0,0), limits = c(-5.5,5.5)) +
  theme(panel.grid.major.x = element_blank()) +
  ggtitle(label = "Double Positive Stressor Interactions", subtitle = "Example: Food Source x Food Source") +
  theme(axis.text.x = element_text(size = 10, angle = 0),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank())

setwd(here("Scripts","Results"))


pdf("Plot_Interactions_Illustrative.pdf", height = 4, width = 12)
annotate_figure(
ggarrange(p1,p2,p3, ncol = 3, nrow = 1),
left = text_grob("Response", rot = 90))
dev.off()

