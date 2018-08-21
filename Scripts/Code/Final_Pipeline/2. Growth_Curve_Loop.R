# Loops across tidy_data from Well_Data_Loader.R to produce tidy_growth_data, which contains various growth metrics against stressor and isolate data.
# TODO: Implement rendering growth curve graphics for all wells.
# TODO: OPTIMISE, OPTIMISE, OPTIMISE

library(dplyr)
library(tibble)
library(tidyr)
library(ggplot2)
library(growthcurver)
library(gridBase)
library(gridExtra)
library(growthmodels)
library(here)

setwd(here("Scripts"))

#######################
### SET INPUTS HERE ###
#######################

# When do we want to cut off growth data. Logistic curves don't fit well on parabolas. 
time_cutoff <- 28

# Misc counters:

# This'll be useful later... I think.
# Actually it's broken now, which makes it even less useful...
# well_curve_plot_vector <- vector("list", length = (2144 * run_count))

# Not actually a list, despite its name. TODO: Find a more graceful solution.
temp_well_stressor_list <- 0

# Some idiot forgot to reset the bad fit count to 0 at the start of the script...
bad_fit_count <- 0

#######################
##### Main Script #####
#######################
# Warning: Very Slow 

# Can we just calculate a general growth stat for all the wells, keeping stressor and isolate data
tidy_growth_data <- tidy_data[0,] %>%
  select(-time, -OD)
# We can use a for loop to calculate empirical area under the curve (auc_e) (for example) for every well
# We can add a second for loop to this lumbering monstrosity to do so across all runs. Jesus wept.

# This loop will run across the first three recorded runs. This is because the four run has a different number of timepoints per well, and it's easier
# (but slower and cruder) to use two separate loops.
for (l in 1:6432)
{
  temp_data_growth_0 <- 
    tidy_data[((l-1) * timepoints_count + 1):(l * timepoints_count),] %>%     # Turn every timepoints_count measurements into a single df
    filter(time <= time_cutoff) %>%
    filter((time %% read_rate) == 0)                                          # Filter your data set down to readings ever n hours                      
  temp_data_growth_1 <- SummarizeGrowth(temp_data_growth_0$time, temp_data_growth_0$OD)        # Generate a logistic model for this well's growth
  
  # If it's a good fit, it goes in. This is going to break a lot of interaction calculations for the time being.
  if (temp_data_growth_1$vals$note == "")
  {
    temp_data_growth_2 <- bind_cols(location = temp_data_growth_0$location[[1]],
                                    Run = temp_data_growth_0$Run[[1]],
                                    Growth_auc_e = temp_data_growth_1$vals$auc_e,
                                    Growth_auc_l = temp_data_growth_1$vals$auc_l,
                                    Growth_k = temp_data_growth_1$vals$k, # TODO: Change coordinates to colnames.
                                    Growth_r = temp_data_growth_1$vals$r,
                                    Growth_n0 = temp_data_growth_1$vals$n0,
                                    Growth_sigma = temp_data_growth_1$vals$sigma,
                                    Fit_notes = temp_data_growth_1$vals$note,
                                    Max_Growth = max(temp_data_growth_0$OD),
                                    Copper = temp_data_growth_0$Copper[[1]],
                                    Nickel = temp_data_growth_0$Nickel[[1]],
                                    Chloramphenicol = temp_data_growth_0$Chloramphenicol[[1]],
                                    Ampicillin = temp_data_growth_0$Ampicillin[[1]],
                                    Metaldehyde = temp_data_growth_0$Metaldehyde[[1]],
                                    Atrazine = temp_data_growth_0$Atrazine[[1]],
                                    Tebuconazole = temp_data_growth_0$Tebuconazole[[1]],
                                    Azoxystrobin = temp_data_growth_0$Azoxystrobin[[1]],
                                    Isolate = temp_data_growth_0$Isolate[[1]])
    
    tidy_growth_data <- bind_rows(tidy_growth_data, temp_data_growth_2)
  }
  
  # 
  # 
  #   # Delete bad fits for the well if we now have a good one
  #   tidy_growth_data <- tidy_growth_data %>%
  #     filter(((location != temp_data_growth_0$location[[1]]) && (temp_data_growth_1$vals$note != "")))
  
  # append the area under the empirical curve (auc_e, etc. to tidy_growth_data)
  # count bad fits 
  if (temp_data_growth_1$vals$note != "")
  {
    bad_fit_count = bad_fit_count + 1
  }
  # Cannabalised for loop to generate a text string of stressors present, to annotate growth curve printouts. Ugly and fragile.
  # for (q in 4:11)
  # {
  #   if (temp_data_growth_0[1,q] == 1)
  #   {
  #     # If a stressor is present, append it to the cell name
  #     temp_well_stressor_list <- paste(temp_well_stressor_list,abbreviate(colnames(temp_data_growth_0[q]), minlength = 2), sep = "")
  #   }
  # }
  # # we can also generate plate curve graphics:
  # temp_data_growth_4 <- as.tibble(temp_data_growth_1$data)
  # # TODO: Fix this horrible if statement.
  # if (is.character(temp_data_growth_1$model) == FALSE) 
  #   # Apparently if growthcurver can't fit a curve it returns a character instead. This is hardly helpful. So we need an if/else statement to catch these.
  # {
  #   temp_predict <- data.frame(time = temp_data_growth_0$time, pred.wt = predict(temp_data_growth_1$model, sigma = temp_data_growth_1$model$sigma))
  #   
  #   # Render a temporary plot of the data points and the logistic curve from growthcurver (stored in temp_predict)
  #   temp_plot <- ggplot(temp_data_growth_0, aes(x = time, y = OD)) +
  #     geom_point() +
  #     geom_line(data = temp_predict, aes(x = time, y = pred.wt), colour = "deepskyblue1") +
  #     ggtitle(temp_data_growth_2$location) +
  #     theme(axis.title.x=element_blank(),
  #           axis.ticks.x=element_blank(),
  #           axis.title.y=element_blank(),
  #           axis.ticks.y=element_blank()) +
  #     ylim(0,1) +
  #     annotate("text", x = 10, y = 0.5, label = temp_well_stressor_list, colour = "deepskyblue1") +
  #     annotate("text", x = 10, y = 0.7, label = temp_data_growth_1$vals$note, colour = "deepskyblue1") +
  #     annotate("text", x = 10, y = 0.3, label = temp_data_growth_2$Run, colour = "deepskyblue1")
  # }
  # else
  # {
  #   temp_plot <- ggplot(temp_data_growth_0, aes(x = time, y = OD)) +
  #     geom_point() +
  #     ggtitle(temp_data_growth_2$location) +
  #     theme(axis.title.x=element_blank(),
  #           axis.ticks.x=element_blank(),
  #           axis.title.y=element_blank(),
  #           axis.ticks.y=element_blank()) +
  #     ylim(0,1) +
  #     annotate("text", x = 25, y = 0.5, label = temp_well_stressor_list, colour = "deepskyblue1") +
  #     annotate("text", x = 10, y = 0.3, label = temp_data_growth_2$Run, colour = "deepskyblue1")
  # }
  # 
  # temp_plot_name <- paste("well_curve", temp_data_growth_2$location, "Run", run_count ,sep = "_")
  # assign(temp_plot_name, temp_plot)
  # well_curve_plot_vector[[l]] <- temp_plot
  # temp_well_stressor_list <- "" # Don't forget to clear it here...
}

# Loop #2
timepoints_count <- 10
for (l in 6432:8576)
{
  temp_data_growth_0 <- 
    tidy_data[((l-1) * timepoints_count + 1):(l * timepoints_count),] %>%     # Turn every timepoints_count measurements into a single df
    filter(time <= time_cutoff) %>%
    filter((time %% read_rate) == 0)                                          # Filter your data set down to readings ever n hours                      
  temp_data_growth_1 <- SummarizeGrowth(temp_data_growth_0$time, temp_data_growth_0$OD)        # Generate a logistic model for this well's growth
  
  # If it's a good fit, it goes in. This is going to break a lot of interaction calculations for the time being.
  if (temp_data_growth_1$vals$note == "")
  {
    temp_data_growth_2 <- bind_cols(location = temp_data_growth_0$location[[1]],
                                    Run = temp_data_growth_0$Run[[1]],
                                    Growth_auc_e = temp_data_growth_1$vals$auc_e,
                                    Growth_auc_l = temp_data_growth_1$vals$auc_l,
                                    Growth_k = temp_data_growth_1$vals$k, # TODO: Change coordinates to colnames.
                                    Growth_r = temp_data_growth_1$vals$r,
                                    Growth_n0 = temp_data_growth_1$vals$n0,
                                    Growth_sigma = temp_data_growth_1$vals$sigma,
                                    Fit_notes = temp_data_growth_1$vals$note,
                                    Max_Growth = max(temp_data_growth_0$OD),
                                    Copper = temp_data_growth_0$Copper[[1]],
                                    Nickel = temp_data_growth_0$Nickel[[1]],
                                    Chloramphenicol = temp_data_growth_0$Chloramphenicol[[1]],
                                    Ampicillin = temp_data_growth_0$Ampicillin[[1]],
                                    Metaldehyde = temp_data_growth_0$Metaldehyde[[1]],
                                    Atrazine = temp_data_growth_0$Atrazine[[1]],
                                    Tebuconazole = temp_data_growth_0$Tebuconazole[[1]],
                                    Azoxystrobin = temp_data_growth_0$Azoxystrobin[[1]],
                                    Isolate = temp_data_growth_0$Isolate[[1]])
    
    tidy_growth_data <- bind_rows(tidy_growth_data, temp_data_growth_2)
  }
  if (temp_data_growth_1$vals$note != "")
  {
    bad_fit_count = bad_fit_count + 1
  }

}

# Just to give an idea of how good the fits are...
cat("bad fits: \n", bad_fit_count, "\n")
cat("of total wells: \n", wells_count, "\n")
# This is broken now that there are different numbers of timepoint per well