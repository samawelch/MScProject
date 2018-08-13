# Produces a basic plot of mixture complexity against a variety of metrics of growth from Growth_Curve_Loop.R. Prints to pdf.
# TODO: What's the statistical validity of any of this? How can I best represent it?
# TODO: Figure out the big deal with carrying capacity.
library(dplyr)
library(tidyverse)
library(ggplot2)
library(growthcurver)
library(gridBase)
library(gridExtra)
library(ggpubr)
library(tikzDevice)
library(here)

setwd(here("Scripts"))

source('Code/Final_Pipeline/Function_Aggregate_Fun_Groups.R')

# Let's graph a bunch of growth parameters against stressor richness 
# First things first: transmute stressor presence/absence to a single richness column
richness_growth_data <- tidier_growth_data %>%
  select(Richness, Isolate, Mean, SD) %>%
  filter(Isolate != "Control")

# and for the sake of completeness
richness_functional_growth_data <- aggregate_functional_groups(tidier_growth_data) %>%
  select(Richness, Species, Mean, SD) %>%
  filter(Species != "Control")

# And graph growth against richness, by species of bacteria
growthXrichness_mean <- ggplot(richness_growth_data, aes(Richness, Mean)) +
  geom_jitter(aes(colour = Isolate, shape = 16), width = 0.3) +
  scale_shape_identity() +
  geom_smooth(aes(group = Isolate, colour = Isolate), method = "lm", se = FALSE) +
  geom_smooth(aes(colour = "Overall"), method = "lm", se = FALSE) +
  ggtitle("Mean auc_e")

growthXfunc_richness_mean <- ggplot(richness_functional_growth_data, aes(as.factor(Richness), Mean)) +
  geom_smooth(aes(group = Species, colour = Species), method = "lm", se = FALSE) +
  geom_smooth(aes(colour = "Overall"), method = "lm", se = FALSE) +
  ggtitle("Mean Growth by Species") +
  xlab("Mixture Complexity") +
  ylab("Mean Growth")

# # Same for auc_l
# growthXrichness_auc_l <- ggplot(richness_growth_data, aes(Richness, Growth_auc_l)) +
#   geom_point(aes(colour = Isolate, shape = 1)) +
#   scale_shape_identity() +
#   geom_smooth(aes(group = Isolate, colour = Isolate), method = "lm", se = FALSE) +
#   geom_smooth(aes(colour = "Overall"), method = "lm", se = FALSE) +
#   ggtitle("auc_l")
# 
# # Same for k (max growth?)
# growthXrichness_k <- ggplot(richness_growth_data, aes(Richness, Growth_k)) +
#   geom_point(aes(colour = Isolate, shape = 1)) +
#   scale_shape_identity() +
#   geom_smooth(aes(group = Isolate, colour = Isolate), method = "lm", se = FALSE) +
#   geom_smooth(aes(colour = "Overall"), method = "lm", se = FALSE) +
#   ylim(0, 1) + # TODO: What's going on here? More of the Ks ~= 0
#   ggtitle("carrying capacity")
# 
# # might as well also do
# growthXrichness_r <- ggplot(richness_growth_data, aes(Richness, Growth_r)) +
#   geom_point(aes(colour = Isolate, shape = 1)) +
#   scale_shape_identity() +
#   geom_smooth(aes(group = Isolate, colour = Isolate), method = "lm", se = FALSE) +
#   geom_smooth(aes(colour = "Overall"), method = "lm", se = FALSE) +
#   ggtitle("growth rate")

# We can do some linear modelling later and obtain some idea of the statistical soundness behind our measurements...
png("Results/Final_Pipeline/growthXrichness.png", width = 1600, height = 800)
# ggarrange(growthXrichness_auc_e, growthXrichness_auc_l, growthXrichness_k, growthXrichness_r, common.legend = TRUE, legend = "right", ncol = 2, nrow = 2)
growthXfunc_richness_mean
dev.off()

# A non-visual measure of fit goodness would also be a good idea
lm_MxR <- lm(Mean ~ Richness + Isolate, richness_growth_data)
par(mfrow = c(2, 2), mar = c(5, 5, 1.5, 1.5))
plot(lm_MxR)
cor.test(richness_growth_data$Mean, richness_growth_data$Richness, use = "pairwise")
summary(lm_MxR)

# By species
lm_MxR_F <- lm(Mean ~ Species + Richness, richness_functional_growth_data)
par(mfrow = c(2, 2), mar = c(5, 5, 1.5, 1.5))
plot(lm_MxR_F)
summary(lm_MxR_F)

# Basically species and isolate currently explain variation well, and richness doesn't. Which doesn't seem right...
lm_anova <- anova(lm_MxR, lm_MxR_F)

# Let's try tikz
tikz("Results/Final_Pipeline/growthXrichness.tex", width = 3.5, height = 3.5)
growthXfunc_richness_mean
dev.off()

library(tikzDevice)
tikz('Results/simpleEx.tex',width=3.5,height=3.5)
plot(1,main='Hello World!')
dev.off()
