# Takes the plots produced by Growth_Curve_Loop.R and prints them by species to 9 pdfs. More elegant than having them both in the same script, but not by much.
# TODO: Save growth curve models with well data for use in future graphics
# TODO: Implement rendering growth curve graphics for all wells.
# TODO: OPTIMISE, OPTIMISE, OPTIMISE
# TODO: Integrate this back into Growth_Curve_Loop.R
library(dplyr)
library(tidyverse)
library(ggplot2)
library(growthcurver)
library(gridBase)
library(gridExtra)
library(cowplot)
library(growthmodels)
library(ggpubr)

# A terrifyingly big grid of well plots... The for loop doesn't actually work though so we'll crank the wheel by hand
setwd("C:/Users/Sam Welch/Google Drive/ICL Ecological Applications/Project/Work/Scripts/Results/Final_Pipeline/Plate_Plots")
# Poor man's for loop...
b = 1
temp_page_name <- paste("plot_isolate", isolates_species_vector[b], ".pdf" ,sep = "_")
pdf(temp_page_name, height = 16, width = 24)
annotate_figure(plot_grid(plotlist = well_curve_plot_vector[(1 + 256 * (b - 1)):(96 + 256 * (b - 1))], ncol = 12), top = text_grob(isolates_species_vector[b]))
annotate_figure(plot_grid(plotlist = well_curve_plot_vector[(97 + 256 * (b - 1)): (192 + 256 * (b - 1))], ncol = 12), top = text_grob(isolates_species_vector[b]))
annotate_figure(plot_grid(plotlist = well_curve_plot_vector[(193 + 256 * (b - 1)):(b * 256)], ncol = 8), top = text_grob(isolates_species_vector[b]))
dev.off()

b = 2
temp_page_name <- paste("plot_isolate", isolates_species_vector[b], ".pdf" ,sep = "_")
pdf(temp_page_name, height = 16, width = 24)
annotate_figure(plot_grid(plotlist = well_curve_plot_vector[(1 + 256 * (b - 1)):(96 + 256 * (b - 1))], ncol = 12), top = text_grob(isolates_species_vector[b]))
annotate_figure(plot_grid(plotlist = well_curve_plot_vector[(97 + 256 * (b - 1)): (192 + 256 * (b - 1))], ncol = 12), top = text_grob(isolates_species_vector[b]))
annotate_figure(plot_grid(plotlist = well_curve_plot_vector[(193 + 256 * (b - 1)):(b * 256)], ncol = 8), top = text_grob(isolates_species_vector[b]))
dev.off()

b = 3
temp_page_name <- paste("plot_isolate", isolates_species_vector[b], ".pdf" ,sep = "_")
pdf(temp_page_name, height = 16, width = 24)
annotate_figure(plot_grid(plotlist = well_curve_plot_vector[(1 + 256 * (b - 1)):(96 + 256 * (b - 1))], ncol = 12), top = text_grob(isolates_species_vector[b]))
annotate_figure(plot_grid(plotlist = well_curve_plot_vector[(97 + 256 * (b - 1)): (192 + 256 * (b - 1))], ncol = 12), top = text_grob(isolates_species_vector[b]))
annotate_figure(plot_grid(plotlist = well_curve_plot_vector[(193 + 256 * (b - 1)):(b * 256)], ncol = 8), top = text_grob(isolates_species_vector[b]))
dev.off()

b = 4
temp_page_name <- paste("plot_isolate", isolates_species_vector[b], ".pdf" ,sep = "_")
pdf(temp_page_name, height = 16, width = 24)
annotate_figure(plot_grid(plotlist = well_curve_plot_vector[(1 + 256 * (b - 1)):(96 + 256 * (b - 1))], ncol = 12), top = text_grob(isolates_species_vector[b]))
annotate_figure(plot_grid(plotlist = well_curve_plot_vector[(97 + 256 * (b - 1)): (192 + 256 * (b - 1))], ncol = 12), top = text_grob(isolates_species_vector[b]))
annotate_figure(plot_grid(plotlist = well_curve_plot_vector[(193 + 256 * (b - 1)):(b * 256)], ncol = 8), top = text_grob(isolates_species_vector[b]))
dev.off()

b = 5
temp_page_name <- paste("plot_isolate", isolates_species_vector[b], ".pdf" ,sep = "_")
pdf(temp_page_name, height = 16, width = 24)
annotate_figure(plot_grid(plotlist = well_curve_plot_vector[(1 + 256 * (b - 1)):(96 + 256 * (b - 1))], ncol = 12), top = text_grob(isolates_species_vector[b]))
annotate_figure(plot_grid(plotlist = well_curve_plot_vector[(97 + 256 * (b - 1)): (192 + 256 * (b - 1))], ncol = 12), top = text_grob(isolates_species_vector[b]))
annotate_figure(plot_grid(plotlist = well_curve_plot_vector[(193 + 256 * (b - 1)):(b * 256)], ncol = 8), top = text_grob(isolates_species_vector[b]))
dev.off()

b = 6
temp_page_name <- paste("plot_isolate", isolates_species_vector[b], ".pdf" ,sep = "_")
pdf(temp_page_name, height = 16, width = 24)
annotate_figure(plot_grid(plotlist = well_curve_plot_vector[(1 + 256 * (b - 1)):(96 + 256 * (b - 1))], ncol = 12), top = text_grob(isolates_species_vector[b]))
annotate_figure(plot_grid(plotlist = well_curve_plot_vector[(97 + 256 * (b - 1)): (192 + 256 * (b - 1))], ncol = 12), top = text_grob(isolates_species_vector[b]))
annotate_figure(plot_grid(plotlist = well_curve_plot_vector[(193 + 256 * (b - 1)):(b * 256)], ncol = 8), top = text_grob(isolates_species_vector[b]))
dev.off()

b = 7
temp_page_name <- paste("plot_isolate", isolates_species_vector[b], ".pdf" ,sep = "_")
pdf(temp_page_name, height = 16, width = 24)
annotate_figure(plot_grid(plotlist = well_curve_plot_vector[(1 + 256 * (b - 1)):(96 + 256 * (b - 1))], ncol = 12), top = text_grob(isolates_species_vector[b]))
annotate_figure(plot_grid(plotlist = well_curve_plot_vector[(97 + 256 * (b - 1)): (192 + 256 * (b - 1))], ncol = 12), top = text_grob(isolates_species_vector[b]))
annotate_figure(plot_grid(plotlist = well_curve_plot_vector[(193 + 256 * (b - 1)):(b * 256)], ncol = 8), top = text_grob(isolates_species_vector[b]))
dev.off()

b = 8
temp_page_name <- paste("plot_isolate", isolates_species_vector[b], ".pdf" ,sep = "_")
pdf(temp_page_name, height = 16, width = 24)
annotate_figure(plot_grid(plotlist = well_curve_plot_vector[(1 + 256 * (b - 1)):(96 + 256 * (b - 1))], ncol = 12), top = text_grob(isolates_species_vector[b]))
annotate_figure(plot_grid(plotlist = well_curve_plot_vector[(97 + 256 * (b - 1)): (192 + 256 * (b - 1))], ncol = 12), top = text_grob(isolates_species_vector[b]))
annotate_figure(plot_grid(plotlist = well_curve_plot_vector[(193 + 256 * (b - 1)):(b * 256)], ncol = 8), top = text_grob(isolates_species_vector[b]))
dev.off()

# and the control plate
b = 9
pdf("control_plot.pdf", height = 16, width = 24)
annotate_figure(plot_grid(plotlist = well_curve_plot_vector[(1 + 256 * (b - 1)):(96 + 256 * (b - 1))], ncol = 12), top = text_grob("Control"))
dev.off()
# Please don't tell anyone how I code...
# Delete out plots for the sake of R and the user's sanity.
rm(list = ls()[grep("^well_curve", ls())])
# Isn't that so much nicer!?
dev.off()
dev.off()
dev.off()
dev.off()
