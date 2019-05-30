# Script for building graphs for multiple runs of the shoaling model, currently
# over 100 steps and 100 runs.

# Current data outputs from the model are:
# 1. Nearest Neighbour Distance
# 2. Polarization (median absolute deviation in heading)
# 3. Shoal Area (convex hull)
# 4. Mean distance from centroid

# The means are calculated two ways:
# 1. Mean of all runs, calculated for every step of the model - mean runs.
# 2. Mean of all steps, calculated for every run of the model - mean steps.

library(ggplot2)
source("~/Desktop/Local/Mackerel/shoal-model-in-R/multiplot.R")

# path <- "~/Desktop/DO NOT ERASE/1NUIG/Mackerel/Mackerel Data/"  # for laptop
path <- "~/Desktop/Local/Mackerel/Mackerel Data/"  # for desktop

run_means <- read.csv(paste0(path,"batch_means_runs.csv"))
step_means <- read.csv(paste0(path,"batch_means_steps.csv"))


polar_runs <- ggplot() + 
  theme_classic() + 
  geom_line(data=run_means, aes(x=X, y=polar), colour="#C830CC", size = 1) +  # line
  theme(axis.text.y = element_text(size = 14, color = "white"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "white")) + 
  xlab("step") + ylab("Mean Polarization") +  # axis labels
  theme(plot.title = element_text(size = 16, face = "bold")) +  # title formatting
  theme(text = element_text(colour = "white", size = 16, face = "bold"))  + # label text
  theme(axis.line = element_line(color="white", size = 1),  # axis line & tick color
        axis.ticks = element_line(color="white")) +
  theme(panel.background = element_rect(fill = "#27292F"),  # transparent background & no outline
        plot.background = element_rect(fill = "#27292F", size = 0))
nnd_runs <- ggplot() + 
  theme_classic() + 
  geom_line(data=run_means, aes(x=X, y=nnd), colour="#C830CC", size = 1) +  # line
  theme(axis.text.y = element_text(size = 14, color = "white"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "white")) + 
  xlab("step") + ylab("Mean Nearest Neighbour Distance") +  # axis labels
  theme(plot.title = element_text(size = 16, face = "bold")) +  # title formatting
  theme(text = element_text(colour = "white", size = 16, face = "bold")) + # label text
  theme(axis.line = element_line(color="white", size = 1), # axis line & tick color
        axis.ticks = element_line(color="white")) +
  theme(panel.background = element_rect(fill = "#27292F"),  # transparent background & no outline
        plot.background = element_rect(fill = "#27292F", size = 0))
area_runs <- ggplot() + 
  theme_classic() + 
  geom_line(data=run_means, aes(x=X, y=area), colour="#C830CC", size = 1) +  # line
  theme(axis.text.y = element_text(size = 14, color = "white"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "white")) + 
  xlab("step") + ylab("Mean Shoal Area") +  # axis labels
  theme(plot.title = element_text(size = 16, face = "bold")) +  # title formatting
  theme(text = element_text(colour = "white", size = 16, face = "bold"))  + # label text
  theme(axis.line = element_line(color="white", size = 1),  # axis line & tick color
        axis.ticks = element_line(color="white")) +
  theme(panel.background = element_rect(fill = "#27292F"),  # transparent background & no outline
        plot.background = element_rect(fill = "#27292F", size = 0))
cent_runs <- ggplot() + 
  theme_classic() + 
  geom_line(data=run_means, aes(x=X, y=centroid), colour="#C830CC", size = 1) +  # line
  theme(axis.text.y = element_text(size = 14, color = "white"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "white")) + 
  xlab("step") + ylab("Mean Distance from Centroid") +  # axis labels
  theme(plot.title = element_text(size = 16, face = "bold")) +  # title formatting
  theme(text = element_text(colour = "white", size = 16, face = "bold")) + # label text
  theme(axis.line = element_line(color="white", size = 1),  # axis line & tick color
        axis.ticks = element_line(color="white")) +
  theme(panel.background = element_rect(fill = "#27292F"),  # transparent background & no outline
        plot.background = element_rect(fill = "#27292F", size = 0))
ggsave(cent_mean_graph, filename = "cent_mean.png", width = 17.09, height = 11.76, units = "cm", 
       dpi = 300, bg = "transparent")

# Call multiplot function for mean graphs
png("run_means.png", width = 11, height = 8, units = 'in', res = 300)
multiplot(polar_runs, nnd_runs, area_runs, cent_runs, cols=2)
dev.off()
