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

interaction_order_vector <- c("A", "B", "Additive\n(i)", "+S\n(ii)", "-S\n(iii)", "+A\n(iv)", "-A\n(v)" )

interaction_colour_vector <- c("A" = "#440154","B" = "#440154","Additive" = "#22a884", "+S" = "#fde725","-S" = "#7ad151", "+A" = "#414487", "-A" = "#2a788e")

minor_interaction_labels <- c("", "", "i", "ii", "iii", "iv", "v")

setwd(here("Scripts"))

data <- read.csv("Data/Expanded_Interaction_Definition_Data.csv")

data1 <- filter(data, Example == 1)

p1 <- ggplot(data = data1) +
  geom_rect(aes(ymin = ymin, ymax = ymax, xmax = ID - 0.45, xmin = ID + 0.45, fill = Interaction)) +
  ylab(label = "Stressor Effect") +
  scale_x_discrete(limits = interaction_order_vector, labels = interaction_order_vector) +
  scale_fill_manual(values = interaction_colour_vector, breaks = interaction_order_vector) +
  scale_y_continuous(expand = c(0,0), 
                     limits = c(-5.5, 5.5),
                     breaks = c(-5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5)) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  ggtitle(label = "Double Negative Interactions", subtitle = "Example: Antibacterial x Antibacterial") +
  theme(axis.text.x = element_text(size = 9, angle = 0),
        legend.position = "none",
        axis.title.y = element_blank())

data2 <- filter(data, Example == 2)

p2 <- ggplot(data = data2) +
  geom_rect(aes(ymin = ymin, ymax = ymax, xmax = ID - 0.45, xmin = ID + 0.45, fill = Interaction)) +
  ylab(label = "Stressor Effect") +
  scale_x_discrete(limits = interaction_order_vector, labels = interaction_order_vector) +
  scale_fill_manual(values = interaction_colour_vector, breaks = interaction_order_vector) +
  scale_y_continuous(expand = c(0,0), 
                     limits = c(-5.5, 5.5),
                     breaks = c(-5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5)) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  ggtitle(label = "Opposing Interactions", subtitle = "Example: Antibacterial x Food Source") +
  theme(axis.text.x = element_text(size = 9, angle = 0),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank())

data3 <- filter(data, Example == 3)

p3 <- ggplot(data = data3) +
  geom_rect(aes(ymin = ymin, ymax = ymax, xmax = ID - 0.45, xmin = ID + 0.45, fill = Interaction)) +
  ylab(label = "Stressor Effect") +
  scale_x_discrete(limits = interaction_order_vector, labels = interaction_order_vector) +
  scale_fill_manual(values = interaction_colour_vector, breaks = interaction_order_vector) +
  scale_y_continuous(expand = c(0,0), 
                     limits = c(-5.5, 5.5),
                     breaks = c(-5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5)) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  ggtitle(label = "Double Positive Interactions", subtitle = "Example: Food Source x Food Source") +
  theme(axis.text.x = element_text(size = 9, angle = 0),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank())

setwd(here("Scripts","Results"))

pdf("Plot_Interactions_Illustrative.pdf", height = 3, width = 9)
annotate_figure(
ggarrange(p1,p2,p3, ncol = 3, nrow = 1),
left = text_grob("Stressor Effect", rot = 90))
dev.off()

