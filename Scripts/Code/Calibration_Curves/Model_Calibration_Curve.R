library(dplyr)
library(tidyr)
library(ggplot2)
library(growthcurver)
library(gridBase)
library(gridExtra)
library(here)

setwd(here("Scripts"))

data <- read.csv("Data/Calibration_Curves/CT_Data_Merged.csv")[2:5]

data_meaned <- data %>%
  group_by(Isolate, Dilution) %>%
  summarise(Mean.Count = mean(Cell.Count), Mean.OD = mean(OD))

ggplot(data_meaned, aes(x = Mean.OD, y = Mean.Count, colour = Isolate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

lm_vector <- vector(length = 8)

for (j in 1:8)
{
  temp_lm <- lm(formula = Cell.Count ~ OD, data = filter(data, Isolate == isolates_vector[j]))
  temp_lm_name <- paste("lm", j, sep = "")
  assign(temp_lm_name, temp_lm)
  
  lm_vector[j] <- temp_lm$coefficients[1]
}

coeff_tibble <- tibble(isolate = isolates_vector[1:8], coeff = lm_vector)


