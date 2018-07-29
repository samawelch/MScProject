# Interaction type classifier. 
# Takes two single_stressor_effects and one observed_binary_effect. Lets you know what sort of interaction you have on your hands.
# Based on Piggott, Townsend and Matthaei (2015)
# TODO: Adapt this to be suitable for lots of data.
# TODO: There is an issue here. When a predicted effect and at least one individual effect are equal, + and - antagonism can mask off each other
# Maybe I should ask about this? I dunno.

stressor_effect_A <- 2
stressor_effect_B <- 1
stressor_effect_C <- -1
observed_effect <- 1.7
summed_effect <- stressor_effect_A + stressor_effect_B + stressor_effect_C

interaction_type <- c("")

# Stick 3 versions of the above in a tibble with a wee dram of variation
observed_stressor_effect <- tibble("stressor" = rep(c("A + B", "AxB"), 3), "response" = c((summed_effect), observed_effect, (summed_effect + 0.1), observed_effect + 0.1,  (summed_effect - 0.1), observed_effect - 0.1))

# Run a t-test on the data. Are the observed and predicted effects significantly different?
ttest <- t.test(observed_stressor_effect$response ~ observed_stressor_effect$stressor)

# With the t-test out of the way we can boil our data down to averages
boiled_stressor_effect <- observed_stressor_effect %>%
  group_by(stressor) %>%
  mutate(mean = mean(response)) %>%
  mutate(sd = sd(response)) %>%
  select (-response) %>%
  distinct()
# Add individual stressor data back in? And calculate interaction size...
# TODO: To do this better.
boiled_stressor_effect <- boiled_stressor_effect %>%
  bind_rows(tibble(stressor = "A", mean = stressor_effect_A, sd = 0.1)) %>%
  bind_rows(tibble(stressor = "B", mean = stressor_effect_B, sd = 0.1)) %>%
  bind_rows(tibble(stressor = "C", mean = stressor_effect_C, sd = 0.1)) %>%
  # This is ugly and specific.
  bind_rows(tibble(stressor = "Interaction", mean = observed_effect - summed_effect, sd = 0.1))

# Turn means into their own variables to stop me from going crazy
int <- subset(boiled_stressor_effect, stressor == "Interaction")$mean
pred <- summed_effect
obs <- observed_effect
A <- stressor_effect_A
B <- stressor_effect_B
C <- stressor_effect_C

if (ttest$p.value > 0.05)
{
  # If not, call it additive and call it a day.
  interaction_type <- "Additive"
} else 
{
  # set the bounds of different defintions
  upper_bound <- max(A, B, C, pred, 0)
  pred <- pred
  lower_bound <- min(A, B, C, pred, 0)

if (obs > upper_bound)
  interaction_type <- "+ Synergy"
else if (obs < lower_bound)
  interaction_type <- "- Synergy"
else if (obs < pred)
  interaction_type <- "- Antagonism"
else if (obs > pred)
  interaction_type <- "+ Antagonism"  
else
  interaction_type <- "Error"
}  
interaction_type  
