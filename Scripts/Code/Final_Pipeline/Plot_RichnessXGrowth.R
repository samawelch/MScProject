# Produces a basic plot of mixture complexity against a variety of metrics of growth from Growth_Curve_Loop.R. Prints to pdf.
# TODO: What's the statistical validity of any of this? How can I best represent it?
# TODO: Figure out the big deal with carrying capacity.
library(dplyr)
library(tidyverse)
library(ggplot2)
library(growthcurver)
library(gridBase)
library(gridExtra)

setwd("C:/Users/Sam Welch/Google Drive/ICL Ecological Applications/Project/Work/Scripts")

# Let's graph a bunch of growth parameters against stressor richness 
# First things first: transmute stressor presence/absence to a single richness column
richness_growth_data <- tidy_growth_data %>%
  mutate(Richness = Copper + Nickel + Chloramphenicol + Ampicillin + Atrazine + Metaldehyde + Tebuconazole + Azoxystrobin) %>%
  select(location, Richness, Isolate, Growth_auc_e, Growth_auc_l, Growth_k, Growth_r)

# And graph growth against richness, by species of bacteria
growthXrichness_auc_e <- ggplot(richness_growth_data, aes(Richness, Growth_auc_e)) +
  geom_point(aes(colour = Isolate, shape = 1)) +
  scale_shape_identity() +
  geom_smooth(aes(group = Isolate, colour = Isolate), method = "lm", se = FALSE) +
  geom_smooth(aes(colour = "Overall"), method = "lm", se = FALSE) +
  ggtitle("auc_e")

# Same for auc_l
growthXrichness_auc_l <- ggplot(richness_growth_data, aes(Richness, Growth_auc_l)) +
  geom_point(aes(colour = Isolate, shape = 1)) +
  scale_shape_identity() +
  geom_smooth(aes(group = Isolate, colour = Isolate), method = "lm", se = FALSE) +
  geom_smooth(aes(colour = "Overall"), method = "lm", se = FALSE) +
  ggtitle("auc_l")

# Same for k (max growth?)
growthXrichness_k <- ggplot(richness_growth_data, aes(Richness, Growth_k)) +
  geom_point(aes(colour = Isolate, shape = 1)) +
  scale_shape_identity() +
  geom_smooth(aes(group = Isolate, colour = Isolate), method = "lm", se = FALSE) +
  geom_smooth(aes(colour = "Overall"), method = "lm", se = FALSE) +
  ylim(0, 1) + # TODO: What's going on here? More of the Ks ~= 0
  ggtitle("carrying capacity")

# might as well also do
growthXrichness_r <- ggplot(richness_growth_data, aes(Richness, Growth_r)) +
  geom_point(aes(colour = Isolate, shape = 1)) +
  scale_shape_identity() +
  geom_smooth(aes(group = Isolate, colour = Isolate), method = "lm", se = FALSE) +
  geom_smooth(aes(colour = "Overall"), method = "lm", se = FALSE) +
  ggtitle("growth rate")

# We can do some linear modelling later and obtain some idea of the statistical soundness behind our measurements...
pdf("Results/Final_Pipeline/growthXrichness.pdf", width = 16, height = 8, onefile = FALSE)
ggarrange(growthXrichness_auc_e, growthXrichness_auc_l, growthXrichness_k, growthXrichness_r, common.legend = TRUE, legend = "right", ncol = 2, nrow = 2)
dev.off()