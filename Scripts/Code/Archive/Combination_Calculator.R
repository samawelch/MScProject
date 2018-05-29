# Sam Welch
# A simple script to determine the possible 1,2,4 and 8 combinations of 8 stressors across 4 categories.
# 7th May 2018
setwd("C:/Users/Sam Welch/Google Drive/ICL Ecological Applications/Project/Scripts")

stressors <- c("Chloramphenicol", "Amoxycillin", "Atrazine", "Metaldehyde", "Copper", "Cadmium", "Benzo[a]pyrene", "Benzene")

mixture_one <- combn(stressors, 1)
mixture_two <- combn(stressors, 2)
mixture_four <- combn(stressors, 4)
mixture_eight <- combn(stressors, 8)

mixture_four

write.csv(mixture_two, file = "Results/mixture_two.csv", row.names = FALSE, col.names = FALSE)
write.csv(mixture_four, file = "Results/mixture_four.csv", row.names = FALSE, col.names = FALSE)
write.csv(mixture_eight, file = "Results/mixture_eight.csv", row.names = FALSE, col.names = FALSE)

