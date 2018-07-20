rm(list=ls())
library(dplyr)
library(tidyverse)
library(ggplot2)
library(growthcurver)
library(gridBase)
library(gridExtra)

setwd("C:/Users/Sam Welch/Google Drive/ICL Ecological Applications/Project/Work/Scripts")

# Load in the plate layout csv for combination and isolate location data
plate_layout <- read.csv("Data/Final_Pipeline/256comb_8bact_plate.csv") %>%
  unite(loc, Dest.Row, Dest.Column, sep = "") %>%
  unite(location, loc, plate, sep = ".")

# How many plates are there?
plate_count = 0 

# Make a vector of isolates
isolates_vector <- as.vector(unique(plate_layout$Isolate))

# Make a vector of stressors
stressors_vector <- as.vector(colnames(plate_layout[1:8]))
# And a colour vector for consistent colouring
stressor_colours <- c("Copper" = "red3", "Nickel" = "firebrick", "Chloramphenicol" = "plum", "Ampicillin" = "plum4", "Atrazine" = "darkgreen", "Metaldehyde" = "forestgreen", "Tebuconazole" = "steelblue", "Azoxystrobin" = "lightblue3", "None" = "black")

# Load in plate .CSVs from a seperate folder using a for loop. Make a tibble to contain the data.
setwd("C:/Users/Sam Welch/Google Drive/ICL Ecological Applications/Project/Work/Scripts/Data/Final_Pipeline/Run_2/csvs")
# Make sure your plates are correctly ordered in the wd. You will need leading 0s on your plate numbers for the below loop to read them in order.
tidy_data <- tibble()

for (k in 1:length(dir()))
{
  # Make a df for each plate with a numbered name
  temp_plate_name <- paste("plate", str_pad(k, 2, pad = "0") ,sep = "_") # pad plate names with leading 0s so R orders them properly
  temp_plate_df <- read.csv(dir()[k])
  colnames(temp_plate_df)[1] <- "time" # fix a pesky capitalisation mismatch
  temp_plate_df$time <- as.numeric(substr(temp_plate_df$time, 1, 2)) # turn the reader's odd time format in to something useful
  temp_plate_width <- 97 # hacky way to get gather on line 42 to work properly
  # we need to remove the last 32 wells from every third plate. This is complicated because it's plates 9,10,11 & 12...
  if ((k %% 3) == 0)
  {
    temp_plate_df <- temp_plate_df %>%
      select(-contains("9")) %>%
      select(-contains("10")) %>%
      select(-contains("11")) %>%
      select(-contains("12"))
    temp_plate_width <- 65 # if it's a third plate it'll have 64 wells (and thus 65 columns including time)
  }
  assign(temp_plate_name, temp_plate_df)                         # turn our temporary df into a real df with a for-loop-generated name
  
  # let's also make a massive tidy dataset that's better able to store stressor presence/absence and isolate species
  temp_plate_width <- 
  temp_plate_tidy <- as.tibble(temp_plate_df) %>%
    gather(well, OD, 2:temp_plate_width) %>%                       # gather the data from wide to tall
    mutate(plate = k) %>%                            # add a plate column based on where we are in the for loop
    unite(location, well, plate, sep = ".")          # bring the location naming scheme in line with plate_layout
  tidy_data <- bind_rows(tidy_data, temp_plate_tidy)       # add the temporary data to our massive dataset
  plate_count = plate_count + 1
}

setwd("C:/Users/Sam Welch/Google Drive/ICL Ecological Applications/Project/Work/Scripts")

# Join isolate/stressor data to growth data by observations
tidy_data <- left_join(tidy_data, plate_layout, by = "location")

# A few checks
glimpse(tidy_data)
# How many wells do we have?
wells_count <- length(unique(tidy_data$location)) # this should be 6240
# How many time points do we have?
timepoints_count <- length(unique(tidy_data$time)) + 1 # should be 49 (but is 54 here); needs a +1 as it seems to ignore 0?
# How many plates
plate_count

# Can we just calculate a general growth stat for all the wells, keeping stressor and isolate data
tidy_growth_data <- tidy_data[0,] %>%
  select(-time, -OD)
# We can use a for loop to calculate empirical area under the curve (auc_e) (for example) for every well
for (l in 1:wells_count)
{
  temp_data_growth_0 <- tidy_data[((l-1) * timepoints_count + 1):(l * timepoints_count),]                              # Turn every timepoints_count measurements into a single df
  temp_data_growth_1 <- SummarizeGrowth(temp_data_growth_0$time, temp_data_growth_0$OD)                                # Generate a logistic model for this well's growth
  temp_data_growth_2 <- bind_cols(location = temp_data_growth_0[[1,2]],
                                  Growth_auc_e = temp_data_growth_1$vals$auc_e,
                                  Growth_auc_l = temp_data_growth_1$vals$auc_l,
                                  Growth_k = temp_data_growth_1$vals$k,
                                  Growth_r = temp_data_growth_1$vals$r,
                                  Growth_n0 = temp_data_growth_1$vals$n0,
                                  Growth_sigma = temp_data_growth_1$vals$sigma,
                                  Copper = temp_data_growth_0[[1,4]],
                                  Nickel = temp_data_growth_0[[1,5]],
                                  Chloramphenicol = temp_data_growth_0[[1,6]],
                                  Ampicillin = temp_data_growth_0[[1,7]],
                                  Metaldehyde = temp_data_growth_0[[1,8]],
                                  Atrazine = temp_data_growth_0[[1,9]],
                                  Tebuconazole = temp_data_growth_0[[1,10]],
                                  Azoxystrobin = temp_data_growth_0[[1,11]],
                                  Isolate = temp_data_growth_0[[1,12]])
  tidy_growth_data <- bind_rows(tidy_growth_data, temp_data_growth_2)
  # append the area under the empirical curve (auc_e, etc. to tidy_growth_data)
}

# Let's graph a bunch of growth parameters against stressor richness 
# First things first: transmute stressor presence/absence to a single richness column
richness_growth_data <- tidy_growth_data %>%
  mutate(Richness = Copper + Nickel + Chloramphenicol + Ampicillin + Atrazine + Metaldehyde + Tebuconazole + Azoxystrobin) %>%
  select(location, Richness, Isolate, Growth_auc_e, Growth_auc_l)

# And graph growth against richness, by species of bacteria
growthXrichness_auc_e <- ggplot(richness_growth_data, aes(Richness, Growth_auc_e)) +
  geom_point(aes(colour = Isolate, shape = 1)) +
  scale_shape_identity() +
  geom_smooth(aes(group = Isolate, colour = Isolate), method = "lm", se = FALSE) +
  geom_smooth(aes(colour = "Overall"), method = "lm", se = FALSE) +
  ggtitle("auc_e")

# Same for auc_l
growthXrichness_auc_l <- ggplot(richness_growth_data, aes(Richness, Growth_auc_l)) +
  geom_point(aes(colour = Isolate, shape = 1)) +
  scale_shape_identity() +
  geom_smooth(aes(group = Isolate, colour = Isolate), method = "lm", se = FALSE) +
  geom_smooth(aes(colour = "Overall"), method = "lm", se = FALSE) +
  ggtitle("auc_l")

# We can do some linear modelling later and obtain some idea of the statistical soundness behind our measurements...
pdf("Results/Final_Pipeline/growthXrichness.pdf")
growthXrichness_auc_e
growthXrichness_auc_l
dev.off()

# We can also graph the effects of different single stressors on bacteria. For instance:
isolate_single_stress <- tidy_data %>%
  filter((Copper + Nickel + Chloramphenicol + Ampicillin + Atrazine + Metaldehyde + Tebuconazole + Azoxystrobin) <= 1) %>%
  select(-location) %>%
  mutate(Stressor = "None")

# A for loop to turn presence/absence data into a single variable (only works for single stressors). This took a stupidly long time to implement. 
for (m in 1:nrow(isolate_single_stress))
{
  for (n in 1:8)
  {
    if (isolate_single_stress[m,n+2] == 1)
    {
      isolate_single_stress[m,12] = colnames(isolate_single_stress[,n+2])
    }
  }
}

# Get rid of the presence/absence stressor data
isolate_single_stress <- isolate_single_stress %>%
  select(-(3:10))

# for loop across the 8 isolates to produce a 4x2 lattice of graphs
for (o in 1:8)
{
  temp_isolate <- isolates_vector[o]
  temp_plot <- ggplot(filter(isolate_single_stress, Isolate == temp_isolate), aes(time, OD)) +
    geom_point(aes(colour = Stressor), size = 1, shape = 16, alpha = 0.5) +
    theme(legend.position="none") +
    ylim(0,0.65) +
    scale_shape_identity() +
    geom_smooth(aes(colour = Stressor), method="loess", se = FALSE) +
    ggtitle(temp_isolate) +
    scale_colour_manual(values = stressor_colours)
  temp_plot_name <- paste("p", o ,sep = "")
  assign(temp_plot_name, temp_plot)   
}

# grab a function that allows shared legends
# source: https://github.com/tidyverse/ggplot2/wiki/share-a-legend-between-two-ggplot2-graphs
# credit: baptiste
grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + 
                    theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x +
                 theme(legend.position = "none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl), 
                                            legend,ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend, ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  
  grid.newpage()
  grid.draw(combined)
  
  # return gtable invisibly
  invisible(combined)
}

# Arange the plots 4x2 with a shared legend
pdf("Results/Final_Pipeline/single_stressor_plots.pdf", width = 16, height = 8, onefile = FALSE) # setting onefile to false prevents a blank leading page
ss_plots <- grid_arrange_shared_legend(p1,p2,p3,p4,p5,p6,p7,p8,ncol = 4, nrow = 2, position = "right")
dev.off()

# How can we look for additivism vs synergism and antagonism? which is the whole point..
example_stressor_growth_data <- tidy_growth_data %>%
  mutate(Richness = Copper + Nickel + Chloramphenicol + Ampicillin + Atrazine + Metaldehyde + Tebuconazole + Azoxystrobin) %>%
  select(-location, -Growth_k, -Growth_r, -Growth_n0, -Growth_sigma, -Growth_auc_l) %>%
  filter(Richness <= 2) %>%
  filter(Isolate == "KUE4_10") # keep life simple for the time being

# Calculate a baseline from controls
control_baseline <- as.numeric(example_stressor_growth_data %>%
  filter(Richness == 0) %>%
  summarise_at("Growth_auc_e",funs(mean)))

# A tibble of single stressors
single_stressor_growth_data <- example_stressor_growth_data %>%
  filter(Richness == 1) %>%
  mutate(Effect_Size = Growth_auc_e - control_baseline) %>%
  select(Effect_Size) %>%
  mutate(stressor = as.list((stressors_vector)))

# And binary mixtures
binary_stressor_growth_data <- example_stressor_growth_data %>%
  filter(Richness == 2) %>%
  mutate(Effect_Size = Growth_auc_e - control_baseline) %>%
  mutate(Summed_Effect = 0) %>%
  mutate(S1 = "") %>%
  mutate(S2 = "")

# Can we for loop through the binary mixtures?
for (p in 1:nrow(binary_stressor_growth_data))
{
  binary_counter = 1
  for (q in 1:8)
  {
    if (binary_stressor_growth_data[p,q] == 1)
    {
      # If a stressor is present, add the relevant effect from single_stressor_growth_data
      binary_stressor_growth_data[p,13] <- binary_stressor_growth_data[p,13] + single_stressor_growth_data[q,1]
      # And assign it to either S1 or S2 depending on if it's the first or second stressor
      if (binary_counter == 1)
      {
        binary_stressor_growth_data[p,14] <- colnames(binary_stressor_growth_data[q])
        binary_counter = binary_counter + 1
      }
      else
      {
        binary_stressor_growth_data[p,15] <- colnames(binary_stressor_growth_data[q])
      }
    }
  }
}

interaction_binary_stressor_growth_data <- binary_stressor_growth_data %>%
  mutate(delta_growth = Effect_Size - Summed_Effect) %>%
  mutate(interaction = "") %>%
  select(Effect_Size, Summed_Effect, S1, S2, delta_growth, interaction)

# Loop across the rows of interaction_binary_stressor_growth_data, assigning interaction categories by the direction and magnitude of delta_growth
for (r in 1:nrow(interaction_binary_stressor_growth_data))
{
  if (interaction_binary_stressor_growth_data[r,1] >interaction_binary_stressor_growth_data[r,2])
  {
    interaction_binary_stressor_growth_data[r,6] <- "Synergistic"
  }
  else if (interaction_binary_stressor_growth_data[r,1] < interaction_binary_stressor_growth_data[r,2])
  {
    interaction_binary_stressor_growth_data[r,6] <- "Antagonistic"
  }
  else
  {
    interaction_binary_stressor_growth_data[r,6] <- "Additive"
  }
}

# Make sure our stressor levels are ordered properly
interaction_binary_stressor_growth_data$S1 <- factor(interaction_binary_stressor_growth_data$S1, levels = stressors_vector)
interaction_binary_stressor_growth_data$S2 <- factor(interaction_binary_stressor_growth_data$S2, levels = stressors_vector)

# And make a bubble plot of interaction and effect size against a combination matrix
pdf("Results/Final_Pipeline/KUE4_10_interactions.pdf", width = 8, height = 8) 
KUE4_10_interaction_bubble <- ggplot(interaction_binary_stressor_growth_data, aes(x = S1, y = S2, size = abs(delta_growth), colour = interaction)) +
  geom_point()  +
  geom_text(aes(label = round(delta_growth, digits = 2), size = 1), colour = "black", show.legend = FALSE) +
  scale_size_area(max_size = 20)
KUE4_10_interaction_bubble
dev.off()


