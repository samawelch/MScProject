# Sam Welch
# A data processing and analysis pipeline for various stressor combinations.
# 10th May 2018
setwd("C:/Users/Sam Welch/Google Drive/ICL Ecological Applications/Project/Scripts")
library(tibble)

stressors <- c("Ch", "Am", "At", "Me", "Cu", "Cd", "Bp", "Bz")

# Make a 4x8 tibble
stressor_combinations <- data.frame("stressors" = stressors)

row_vector <- c(1,2,3,4,5,6,7,8)
zero_vector <- c(0,0,0,0)
growth_test_data <- runif(4, min=0, max=100)

data.frame (row_vector, row_vector)

# Set up an 8(+1)x4 tibble, crudely
comb_tibble <- tibble(
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
  comb_tibble[i , 1:2^(i-1)] = 1
  i <- i + 1
}

model <- lm(`1` + `2` + `3` + `4` + `5` + `6` + `7` + `8` ~ Growth, comb_tibble)
model