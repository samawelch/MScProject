# Makes those linear models Tom wanted.
# Requires: Growth_Curve_Loop
library(dplyr)
library(here)

isolate_lm <- lm(Max_Growth ~ Isolate, tidy_growth_data)
summary(isolate_lm)

# Setting this as an empty vector now so that the lmf_1 will only contain itself, but
# lmf_n > 1 will contain all of lmf_m < n. If that makes sense... 
test4 <- c()

# It's another nested for loop.
for (b in 1:8)
{
  test0 <- combn(stressors_vector, b)
  test2 <- vector(mode = "list", length = dim(test0)[2])
  for (a in 1:dim(test0)[2])
  {
    test1 <- paste("(", test0[1,a], sep = "")
    if (b > 1)
    {
      # We only need to start worrying about : for n > 1 levels of complexity
      for (c in 1:(b-1))
      {
        test1 <- paste(test1, ":", test0[(c+1),a])  
      }
    }
      test1 <- paste(test1, ")", sep = "")
      test2[a] <- test1
  }
  test3 <- paste(test2, collapse = " + ")
  # We need to actually nest the model by concatenating the formulae each loop
  # This is a crude way to avoid sticking an extraneous + in lmf_1
  if (is.null(test4))
    test4 <- test3
  else
    test4 <- paste(test3, test4, sep = " + ")
  test5 <- paste("Growth_auc_e ~ ", test4, sep = "")
  temp_lmf_name <- paste("lmf_", b , sep = "")
  assign(temp_lmf_name, test5)
  temp_lm_name <- paste("lm_", b , sep = "")
  temp_lm <- do.call("lm", list(test5, tidy_growth_data))
  assign(temp_lm_name, temp_lm)
}

# Can we pick out the models that best explain the variation
lm_anova <- anova(lm_1, lm_2, lm_3, lm_4, lm_5, lm_6, lm_7, lm_8)

aic <- AIC(lm_1, lm_2, lm_3, lm_4, lm_5, lm_6, lm_7, lm_8)
# And then pick out interactions on the basis of effect size/statistical significance...
summary(lm_1)
anova(lm_1)

aic_tib <- as.tibble(aic) %>%
  bind_cols(lm = c("lm1","lm2","lm3","lm4","lm5","lm6","lm7","lm8")) %>%
  rename(AIC_df = df)

lm_anova_tibble <- as.tibble(lm_anova) %>%
  bind_cols(aic_tib)

# Lower AIC is better?

setwd(here("Scripts"))
write.csv(lm_anova_tibble, "Results/Final_Pipeline/lm_stats_results.csv")

# What's our critical F-value?
alpha = 0.05
qf(1-alpha, lm_anova[1,1], lm_anova[2,1])
# 1.042
