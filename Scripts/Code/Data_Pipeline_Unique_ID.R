# Sam Welch
# A script that produces a unique stressor combination ID, fills in a binary matrix based on it, then generates dummy growth data. Boolean version.
# 26th May 2018
rm(list=ls())

library(tidyverse)

vector_stressors <- c("1","2","3","4","5","6","7","8")

# Generate matrices of every 1-, 2-, 4-, and 8- combination of stressors.
matrix_combn_eight <- combn(vector_stressors,8)
matrix_combn_four <- combn(vector_stressors,4)
matrix_combn_two <- combn(vector_stressors,2)
matrix_combn_one <- combn(vector_stressors,1)

# Turn matrices into vectors of strings of combinations
vector_combn_eight <- apply(matrix_combn_eight,2, paste, collapse="")
vector_combn_four <- apply(matrix_combn_four,2, paste, collapse="")
vector_combn_two <- apply(matrix_combn_two,2, paste, collapse="")
vector_combn_one <- apply(matrix_combn_one,2, paste, collapse="")

# Combine them into one vector of length 107
vector_combn <- c(vector_combn_one, vector_combn_two, vector_combn_four, vector_combn_eight)

# Create a tibble of unique combination IDs, presence/absence of stressors and five replicates of growth
comb_tibble <- tibble(
  vector_combn,
  `s1` = FALSE,
  `s2` = FALSE,
  `s3` = FALSE,
  `s4` = FALSE,
  `s5` = FALSE,
  `s6` = FALSE,
  `s7` = FALSE,
  `s8` = FALSE,
  `GrowthA` = 0,
  `GrowthB` = 0,
  `GrowthC` = 0,
  `GrowthD` = 0,
  `GrowthE` = 0
)

# Loop over all combination IDs and assign stressors as present
# Yes, I am aware of the inefficiency here - I just don't know how to do it better.
for (i in 1:107) {
  comb <- comb_tibble[i,1]            # For each combination ID: assign to the variable comb
  for (j in 1:8) {                    # Loop across comb for j = 1:8
    if (grepl(toString(j),comb)) {    # if comb contains the string of j, 
      comb_tibble[i, j + 1] = TRUE    # change the value in the relevant position of the tibble to 1
    }
    j <- j + 1
  }
  i <- i + 1
}

# Another loop to stick some random data into the growth columns. Can this be done more elegantly with apply?
for (k in 1:107) {
  for (l in 10:14){
    comb_tibble[k,l] = runif(1, min=0, max=100)
    l <- l + 1
  }
  k <- k + 1
}

# Add a means column.
summary(comb_tibble)
comb_tibble <- mutate(comb_tibble,GrowthAvg = rowMeans(comb_tibble[,10:14]))

# What I've read online suggests a multiple linear regression or a non-linear multiple regression - so I'm going to try both...

# Multiple linear regression
mlr <- lm(
  formula = GrowthAvg ~ s1 + s2 + s3 + s4 + s5 + s6 + s7 + s8,
  data = comb_tibble)
summary(mlr)
plot(mlr)
# I can't remember how to interpret any of this, but at least I'll have a choice when it comes to working with the real data...

# Non-linear multiple regression - doesn't work because I don't understand it...
nlmr <- nls(
  formula = GrowthAvg ~ s1 + s2 + s3 + s4 + s5 + s6 + s7 + s8, # This is the wrong sort of formula for nls()
  data = comb_tibble)