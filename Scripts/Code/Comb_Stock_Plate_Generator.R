# Sam Welch
# A script that attempts to generate 3 96-well plates of stressor combinations from combinations generated in Data_Pipeline_Unique_ID. Early days.
# 5th June 2018
rm(list=ls())
setwd("C:/Users/Sam Welch/Google Drive/ICL Ecological Applications/Project/Work/Scripts")

library(tidyverse)
library(ggplot2)

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
vector_combn <- sort(vector_combn, decreasing = FALSE)

# If you know a better way to make an empty tibble I'd like to hear it. I'm serious: please let me know.
comb_stock_plate_1 = tibble(A = 1:12 * 0, B = 0, C = 0, D = 0, E = 0, F = 0, G = 0, H = 0) 
comb_stock_plate_2 = comb_stock_plate_1
comb_stock_plate_3 = comb_stock_plate_2

l = 1
for (i in 1:3)
{
  for (j in 1:8)
  {
    for (k in 1:12)
    {
      if (i == 1)
      {
        comb_stock_plate_1[k,j] <- vector_combn[l]
        print(c(vector_combn[i], "plate 1"))
      }
      else if (i == 2)
      {
        comb_stock_plate_2[k,j] <- vector_combn[l]
        print(c(vector_combn[i], "plate 2"))
      }
      else if (i == 3)
      {
        comb_stock_plate_3[k,j] <- vector_combn[l]
        print(c(vector_combn[i], "plate 3"))
      }
      l = l + 1
    }
  }
}

