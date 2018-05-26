# Sam Welch
# A data processing and analysis pipeline for various stressor combinations.
# 10th May 2018
rm(list=ls())

library(tibble)

# Make a vector of 4 0s and a vector of 4 random 0-100 numbers
zero_vector <- c(0,0,0,0)
growth_test_data <- runif(4, min=0, max=100)

# Set up an 8(+1)x4 tibble, crudely
comb_tibble_old <- tibble(
  `1` = zero_vector, 
  `2` = zero_vector,
  `3` = zero_vector,
  `4` = zero_vector,
  `5` = zero_vector,
  `6` = zero_vector,
  `7` = zero_vector,
  `8` = zero_vector,
  Growth = growth_test_data
)

# add ones to relevant columns, rows to show combinations
for (i in 1:4){
  comb_tibble_old[i , 1:2^(i-1)] = 1
  i <- i + 1
}

# Make a linear model based on stressor dummy value by growth
model <- lm(`1` + `2` + `3` + `4` + `5` + `6` + `7` + `8` ~ Growth, comb_tibble_old)
model