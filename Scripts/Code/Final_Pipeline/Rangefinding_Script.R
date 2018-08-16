# Load in data from my rangefinding experiment. Plot some growth curves?

library(tidyr)
library(growthcurver)
library(dplyr)
library(stringr)
library(here)
library(ggpubr)

#######################

# Misc counters:

# How about a unique time cutoff
rf_time_cutoff <- 24
rf_read_rate <- 4
# It's still 24 though.

# Make a vector of isolates. This should be the same between rangefinding and actual experiments, but:
# TODO: Check paperwork for plate layouts
isolates_rf_vector <- c("KUE4_10 - S. acidaminiphila", "NUE1_1 - B. simplex", "LUF4_5 - L. rhizovicinus", "NUF1_3 - V. paradoxus",
                        "KUB5_13 - V. paradoxus", "KUE4_4 - B. simplex", "E. coli OP50", "Nash's Field Soil Community")
names(isolates_rf_vector) <- c("A", "B", "C", "D", "E", "F", "G", "H")

concentrations_mod_vector <- c(0.1, 1, 10, 100, 0.1, 1, 10, 100, 0.1, 1, 10, 100)
names(concentrations_mod_vector) <- c(as.character(seq(1,12)))

concentration_stressor_vector <- c(0.02, 0.002, 0.0000002, 0.000002, 0.000004, 0.000002, 0.001, 0.001, 0)
names(concentration_stressor_vector) <- c("Copper", "Nickel", "Chloramphenicol", "Ampicillin", "Atrazine", "Metaldehyde", "Tebuconazole", "Azoxystrobin", "Control")

# Load in plate .CSVs from a seperate folder using a for loop. Make a tibble to contain the data.
# Make sure your plates are correctly ordered in the wd. You will need leading 0s on your plate numbers for the below loop to read them in order.
rf_growth_data <- tibble()
rangefinding_data <- tibble()

#######################
##### Main Script #####
#######################

setwd(here("Scripts", "Data","Bug_Rangefinding","Working_Dir"))

# Let's load in some plates

for (k in 1:length(dir()))
{
  # Grab the sheet name and remove the ".csv"
  temp_plate_name <- substr(dir()[k], 1, str_length(dir()[k]) - 4)
  temp_plate_df <- read.csv(dir()[k])
  # fix a pesky capitalisation mismatch
  colnames(temp_plate_df)[1] <- "time" 
  # turn the reader's odd time format in to something useful
  temp_plate_df$time <- as.numeric(substr(temp_plate_df$time, 1, 2)) 
  
  assign(temp_plate_name, temp_plate_df)                         

  temp_conc_thing <- unname(concentration_stressor_vector[temp_plate_name])
  
  # tidy data
  temp_plate_tidy <- as_tibble(temp_plate_df) %>%
    gather(well, OD, 2:97) %>%   
    # Match our premade concentration and isolate vectors rather haphazardly to the first or last character(s) of the location
    mutate(stressor = temp_plate_name) %>%
    mutate(Concentration = concentrations_mod_vector[substr(well, 2, str_length(well))] * temp_conc_thing) %>%
    mutate(Isolate = isolates_rf_vector[substr(well, 1, 1)])
  
  rangefinding_data <- bind_rows(rangefinding_data, temp_plate_tidy)  
}

for (l in 1:(nrow(rangefinding_data) / 55))
{
  temp_data_growth_0 <- 
    rangefinding_data[((l-1) * 55):(l * 55),] %>%
    filter(time <= rf_time_cutoff) %>%
    filter((time %% rf_read_rate) == 0)                                          # Filter your data set down to readings ever n hours                      
  temp_data_growth_1 <- SummarizeGrowth(temp_data_growth_0$time, temp_data_growth_0$OD)        # Generate a logistic model for this well's growth

  temp_data_growth_2 <- bind_cols(Growth_auc_e = temp_data_growth_1$vals$auc_e,
                                  Growth_k = temp_data_growth_1$vals$k,
                                    Fit_notes = temp_data_growth_1$vals$note,
                                    Stressor = temp_data_growth_0$stressor[[1]],
                                    Concentration = temp_data_growth_0$Concentration[[1]],
                                    Isolate = temp_data_growth_0$Isolate[[1]])
    
  rf_growth_data <- bind_rows(rf_growth_data, temp_data_growth_2)
}

# Let's make some dose response curves
for (k in 1:8)
{
  temp_rangefinding <- rf_growth_data %>%
    filter(Stressor == names(concentration_stressor_vector[k])) %>%
    group_by(Isolate, Concentration, Stressor) %>%
    summarise(Mean_growth = mean(Growth_k))
  
  # (Don't) Add mean growth
  # temp_rangefinding <- temp_rangefinding %>%
  #   bind_rows(temp_rangefinding %>% 
  #               group_by(Concentration, Stressor) %>% 
  #               summarise(Mean_growth = mean(Mean_growth)) %>% 
  #               mutate(Isolate = "Mean"))
  
  temp_plot_name <- paste("p", k, sep = "")
  
  temp_plot <- 
    ggplot(data = temp_rangefinding, aes(
      x = log(Concentration),
      y = log(Mean_growth), 
      colour = Isolate)) +
    geom_point() +
    ggtitle(label = names(concentration_stressor_vector[k])) +
    geom_smooth(method = "loess", se = FALSE) +
    geom_vline(xintercept = log(concentration_stressor_vector[k]), colour = "grey", linetype = "dashed") +
    annotate("text", 
             label = paste("Target concentration = ", concentration_stressor_vector[k], " μg/L", sep = ""), 
             hjust = 1,
             vjust = 1,
             color = "grey",
             angle = 90) +
    theme(legend.position = "none",
          axis.title.y = element_blank(),
          axis.title.x = element_blank())	
  
  assign(temp_plot_name, temp_plot)
}

dummy_legend <- get_legend(
    ggplot(data = temp_rangefinding, aes(
      x = log(Concentration),
      y = Mean_growth, 
      colour = Isolate)) +
    geom_point()
)

setwd(here("Scripts","Results","Bug_Rangefinding"))
pdf("plots_rangefinding.pdf", width = 9, height = 9)
annotate_figure(ggarrange(p1, p2, p3, p4, p5, p6, p7, p8, dummy_legend, ncol = 3, nrow = 3),
                left = text_grob("Mean Carrying Capacity (OD)", size = 16, rot = 90),
                bottom = text_grob("Log Concentration", size = 16))
dev.off()
dev.off()
