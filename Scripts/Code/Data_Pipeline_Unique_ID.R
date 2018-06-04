# Sam Welch
# A script that produces a unique stressor combination ID, fills in a binary matrix based on it, then generates dummy growth data. Boolean version.
# 26th May 2018
rm(list=ls())

library(tidyverse)
library(ggplot2)
library(pwr)

vector_stressors <- c("1","2","3","4","5","6","7","8")

# Generate matrices of every 1-, 2-, 4-, and 8- combination of stressors.
matrix_combn_eight <- combn(vector_stressors,8)
matrix_combn_seven <- combn(vector_stressors,7)
matrix_combn_six <- combn(vector_stressors,6)
matrix_combn_five <- combn(vector_stressors,5)
matrix_combn_four <- combn(vector_stressors,4)
matrix_combn_three <- combn(vector_stressors,3)
matrix_combn_two <- combn(vector_stressors,2)
matrix_combn_one <- combn(vector_stressors,1)

# Turn matrices into vectors of strings of combinations
vector_combn_eight <- apply(matrix_combn_eight,2, paste, collapse="")
vector_combn_seven <- apply(matrix_combn_seven,2, paste, collapse="")
vector_combn_six <- apply(matrix_combn_six,2, paste, collapse="")
vector_combn_five <- apply(matrix_combn_five,2, paste, collapse="")
vector_combn_four <- apply(matrix_combn_four,2, paste, collapse="")
vector_combn_three <- apply(matrix_combn_three,2, paste, collapse="")
vector_combn_two <- apply(matrix_combn_two,2, paste, collapse="")
vector_combn_one <- apply(matrix_combn_one,2, paste, collapse="")

# Combine them into one vector of length 107
vector_combn <- c(vector_combn_one, vector_combn_two, vector_combn_three, vector_combn_four, vector_combn_five, vector_combn_six, vector_combn_seven, vector_combn_eight)

# Create a tibble of unique combination IDs, presence/absence of stressors and five replicates of growth
comb_tibble <- tibble(
  vector_combn,
  species = 0,
  legacy = 0,
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
for (i in 1:255) {
  comb <- comb_tibble[i,1]            # For each combination ID: assign to the variable comb
  for (j in 1:8) {                    # Loop across comb for j = 1:8
    if (grepl(toString(j),comb)) {    # if comb contains the string of j, 
      comb_tibble[i, j + 3] = 1    # change the value in the relevant position of the tibble to 1
    }
    j <- j + 1
  }
  i <- i + 1
}

# Another loop to stick some random data into the growth columns. Can this be done more elegantly with apply?
for (k in 1:255) {
  for (l in 12:16){
    comb_tibble[k,l] = runif(1, min=0, max=100)
    l <- l + 1
  }
  k <- k + 1
}

# Add a means column.
summary(comb_tibble)
comb_tibble <- mutate(comb_tibble,GrowthAvg = rowMeans(comb_tibble[,12:16]))

# What I've read online suggests a multiple linear regression or a non-linear multiple regression - so I'm going to try both...

# Multiple linear regression
mlr <- lm(
  formula = GrowthAvg ~ s1 + s2 + s3 + s4 + s5 + s6 + s7 + s8,
  data = comb_tibble)
summary(mlr)
plot(mlr)

# row sums is an easier way to look at interaction by stressor richness
plot(comb_tibble$GrowthAvg~rowSums(comb_tibble[,4:11]))


lm0=lm(growth~s1+s2+s3)
lm1=lm(growth~s1+s2+s3+s1:s2+s1:s3+s2:s3)
lm2=lm(growth~s1+s2+s3+s1:s2+s1:s3+s2:s3+s1:s2:s3)

anova(lm0,lm1,lm2)

stepAIC(lm2)

# Make your life easier by automating your X-way interaction formulae
as.formula(c("growth~",paste("s",1:10, sep="")))

# Iterate for each species - do we get the same answers? How would you illustate this?
# We also want to know if there are groups of stressors that are acting similarly:
# hyp: synergy between stressors from different functional groups, etc. -  need to lit search this...
# e.g. negative interactions on growth when they're from the same functional group?

# I can't remember how to interpret any of this, but at least I'll have a choice when it comes to working with the real data...

# Non-linear multiple regression - doesn't work because I don't understand it...
# nlmr <- nls(
#  formula = GrowthAvg ~ s1 + s2 + s3 + s4 + s5 + s6 + s7 + s8, # This is the wrong sort of formula for nls()
#  data = comb_tibble)

# Boxplot of average growth by stressor
comb_tibble_tidy <- gather(comb_tibble, Rep, Growth, 12:16) # Gathering the data for a boxplot of all stressors containing s1
ggplot(
      subset(comb_tibble_tidy, s1 = 1),
      aes(
      x = vector_combn,
      y = Growth
      )) + geom_boxplot()

# Run an ANOVA/ANCOVA?
anova(mlr)

# Power Analysis?
pwr.anova.test(k = 255, n=4, power = 0.1)

