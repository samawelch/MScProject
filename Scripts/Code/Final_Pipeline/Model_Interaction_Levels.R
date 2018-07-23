# Makes those linear models Tom wanted.
# TODO: Actually Make
# Requires: Growth_Curve_Loop
library(dplyr)
library(tidyverse)
library(ggplot2)
library(growthcurver)
library(gridBase)
library(gridExtra)
library(cowplot)
library(growthmodels)

# lm(formula = as.matrix(tidy_growth_data$Growth_auc_e) ~ 
#   as.matrix(tidy_growth_data$Copper) ~ 
#   as.matrix(tidy_growth_data$Nickel) ~ 
#   as.matrix(tidy_growth_data$Chloramphenicol) ~ 
#   as.matrix(tidy_growth_data$Ampicillin) ~ 
#   as.matrix(tidy_growth_data$Metaldehyde) ~ 
#   as.matrix(tidy_growth_data$Atrazine) ~ 
#   as.matrix(tidy_growth_data$Tebuconazole),
#   data = tidy_growth_data)

ss_mlr <- lm(formula = Growth_auc_e ~ Copper + Nickel + Chloramphenicol + Ampicillin + Metaldehyde + Atrazine + Tebuconazole, data = tidy_growth_data)

# Make a list of 2-stressor combinations we can plug into a linear model
test0 <- combn(stressors_vector, 2)
test2 <- vector(mode = "list", length = 28)
for (a in 1:28)
{
  test1 <- paste("(", test0[1,a], " + ", test0[2,a], ")", sep = "")
  test2[a] <- test1
}

test3 <- paste(test2, collapse = " + ")

bs_mlr <- lm(formula = Growth_auc_e ~ 
     (Copper + Nickel) + 
     (Copper + Chloramphenicol) + 
     (Copper + Ampicillin) + 
     (Copper + Metaldehyde) + 
     (Copper + Atrazine) + 
     (Copper + Tebuconazole) + 
     (Copper + Azoxystrobin) + 
     (Nickel + Chloramphenicol) + 
     (Nickel + Ampicillin) + 
     (Nickel + Metaldehyde) + 
     (Nickel + Atrazine) + 
     (Nickel + Tebuconazole) + 
     (Nickel + Azoxystrobin) + 
     (Chloramphenicol + Ampicillin) + 
     (Chloramphenicol + Metaldehyde) + 
     (Chloramphenicol + Atrazine) + 
     (Chloramphenicol + Tebuconazole) + 
     (Chloramphenicol + Azoxystrobin) + 
     (Ampicillin + Metaldehyde) + 
     (Ampicillin + Atrazine) + 
     (Ampicillin + Tebuconazole) + 
     (Ampicillin + Azoxystrobin) + 
     (Metaldehyde + Atrazine) + 
     (Metaldehyde + Tebuconazole) + 
     (Metaldehyde + Azoxystrobin) + 
     (Atrazine + Tebuconazole) + 
     (Atrazine + Azoxystrobin) + 
     (Tebuconazole + Azoxystrobin), 
   data = tidy_growth_data)

# Make a list of 3-stressor combinations we can plug into a linear model
test0 <- combn(stressors_vector, 3)
test2 <- vector(mode = "list", length = dim(test0)[2])
for (a in 1:dim(test0)[2])
{
  test1 <- paste("(", test0[1,a], " + ", test0[2,a], " + ", test0[3,a], ")", sep = "")
  test2[a] <- test1
}

test3 <- paste(test2, collapse = " + ")

# Not sure this is working right now
ts_mlr <- lm(formula = Growth_auc_e ~ (Copper + Nickel + Chloramphenicol) + (Copper + Nickel + Ampicillin) + (Copper + Nickel + Metaldehyde) + (Copper + Nickel + Atrazine) + (Copper + Nickel + Tebuconazole) + (Copper + Nickel + Azoxystrobin) + (Copper + Chloramphenicol + Ampicillin) + (Copper + Chloramphenicol + Metaldehyde) + (Copper + Chloramphenicol + Atrazine) + (Copper + Chloramphenicol + Tebuconazole) + (Copper + Chloramphenicol + Azoxystrobin) + (Copper + Ampicillin + Metaldehyde) + (Copper + Ampicillin + Atrazine) + (Copper + Ampicillin + Tebuconazole) + (Copper + Ampicillin + Azoxystrobin) + (Copper + Metaldehyde + Atrazine) + (Copper + Metaldehyde + Tebuconazole) + (Copper + Metaldehyde + Azoxystrobin) + (Copper + Atrazine + Tebuconazole) + (Copper + Atrazine + Azoxystrobin) + (Copper + Tebuconazole + Azoxystrobin) + (Nickel + Chloramphenicol + Ampicillin) + (Nickel + Chloramphenicol + Metaldehyde) + (Nickel + Chloramphenicol + Atrazine) + (Nickel + Chloramphenicol + Tebuconazole) + (Nickel + Chloramphenicol + Azoxystrobin) + (Nickel + Ampicillin + Metaldehyde) + (Nickel + Ampicillin + Atrazine) + (Nickel + Ampicillin + Tebuconazole) + (Nickel + Ampicillin + Azoxystrobin) + (Nickel + Metaldehyde + Atrazine) + (Nickel + Metaldehyde + Tebuconazole) + (Nickel + Metaldehyde + Azoxystrobin) + (Nickel + Atrazine + Tebuconazole) + (Nickel + Atrazine + Azoxystrobin) + (Nickel + Tebuconazole + Azoxystrobin) + (Chloramphenicol + Ampicillin + Metaldehyde) + (Chloramphenicol + Ampicillin + Atrazine) + (Chloramphenicol + Ampicillin + Tebuconazole) + (Chloramphenicol + Ampicillin + Azoxystrobin) + (Chloramphenicol + Metaldehyde + Atrazine) + (Chloramphenicol + Metaldehyde + Tebuconazole) + (Chloramphenicol + Metaldehyde + Azoxystrobin) + (Chloramphenicol + Atrazine + Tebuconazole) + (Chloramphenicol + Atrazine + Azoxystrobin) + (Chloramphenicol + Tebuconazole + Azoxystrobin) + (Ampicillin + Metaldehyde + Atrazine) + (Ampicillin + Metaldehyde + Tebuconazole) + (Ampicillin + Metaldehyde + Azoxystrobin) + (Ampicillin + Atrazine + Tebuconazole) + (Ampicillin + Atrazine + Azoxystrobin) + (Ampicillin + Tebuconazole + Azoxystrobin) + (Metaldehyde + Atrazine + Tebuconazole) + (Metaldehyde + Atrazine + Azoxystrobin) + (Metaldehyde + Tebuconazole + Azoxystrobin) + (Atrazine + Tebuconazole + Azoxystrobin), 
   data = tidy_growth_data)

anova(ss_mlr, bs_mlr, ts_mlr)
