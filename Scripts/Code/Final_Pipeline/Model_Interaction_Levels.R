# Makes those linear models Tom wanted.
# TODO: Actually Make
# Requires: Growth_Curve_Loop
library(dplyr)
library(tidyverse)

# I think we need to allow for the effect of species???
isolate_lm <- lm(Growth_auc_e ~ Isolate, tidy_growth_data)
summary(isolate_lm)

# I tried a for loop and it was bad, so by hand:
for (b in 1:8)
{
  test0 <- combn(stressors_vector, b)
  test2 <- vector(mode = "list", length = dim(test0)[2])
  for (a in 1:dim(test0)[2])
  {
    test1 <- paste("(", test0[1,a], sep = "")
    if (b > 1)
    {
      for (c in 1:(b-1))
      {
        test1 <- paste(test1, ":", test0[(c+1),a])  
      }
    }
      test1 <- paste(test1, ")", sep = "")
      test2[a] <- test1
  }
  test3 <- paste(test2, collapse = " + ")
  test3 <- paste("Growth_auc_e ~ ", test3, sep = "")
  temp_lmf_name <- paste("lmf_", b , sep = "")
  assign(temp_lmf_name, test3)
  
  temp_lm_name <- paste("lm_", b , sep = "")
  temp_lm <- do.call("lm", list(test3, tidy_growth_data))
  assign(temp_lm_name, temp_lm)
  
}

# Can we pick out the models that best explain the variation
anova <- anova(lm_1, lm_2, lm_3, lm_4, lm_5, lm_6, lm_7, lm_8)
# Is a multivariate analysis of variance more appropriate? No.
my_manova <- manova(lm_1, lm_2, lm_3, lm_4, lm_5, lm_6, lm_7, lm_8)

aic <- AIC(lm_1, lm_2, lm_3, lm_4, lm_5, lm_6, lm_7, lm_8)
# And then pick out interactions on the basis of effect size/statistical significance...
summary(lm_1)
anova(lm_1)
# Currently dealing with model saturation. Needs work.

aic_tib <- as.tibble(aic) %>%
  bind_cols(lm = c("lm1","lm2","lm3","lm4","lm5","lm6","lm7","lm8")) 

ggplot(data = aic_tib, aes(x = lm, y = AIC)) +
  geom_point(aes(size = df))
# Lower AIC is better? 
