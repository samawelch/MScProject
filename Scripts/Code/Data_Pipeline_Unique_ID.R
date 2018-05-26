# Sam Welch
# A script that produces a unique stressor combination ID, fills in a binary matrix based on it, then generates dummy growth data.
# 26th May 2018
rm(list=ls())

library(tibble)

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
  `s1` = 0,
  `s2` = 0,
  `s3` = 0,
  `s4` = 0,
  `s5` = 0,
  `s6` = 0,
  `s7` = 0,
  `s8` = 0,
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
      comb_tibble[i, j + 1] = 1       # change the value in the relevant position of the tibble to 1
    }
    j <- j + 1
  }
  i <- i + 1
}
