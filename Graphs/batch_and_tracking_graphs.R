# This script combines the batch graphs and the tracking graphs for 
# presentations and the like.

library(ggplot2)
library(foreach)
source("~/Desktop/Local/Mackerel/shoal-model-in-R/multiplot.R")

# path <- "~/Desktop/DO NOT ERASE/1NUIG/Mackerel/Mackerel Data/"  # for laptop
path <- "~/Desktop/Local/Mackerel/Mackerel Data/"  # for desktop


# Read in data ----------------------------------------------------------------
run_means <- read.csv(paste0(path,"batch_means_runs.csv"))
step_means <- read.csv(paste0(path,"batch_means_steps.csv"))
tracking <- read.csv(paste0(path, "stepwise_data.csv"))

# Run means for every step of the models --------------------------------------

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
  theme(panel.background = element_rect(fill = "#27282E"),  # plot & background colors, no outline
        plot.background = element_rect(fill = "#27282E", size = 0))

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
  theme(panel.background = element_rect(fill = "#27282E"),  # plot & background colors, no outline
        plot.background = element_rect(fill = "#27282E", size = 0))

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
  theme(panel.background = element_rect(fill = "#27282E"),  # plot & background colors, no outline
        plot.background = element_rect(fill = "#27282E", size = 0))

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
  theme(panel.background = element_rect(fill = "#27282E"),  # plot & background colors, no outline
        plot.background = element_rect(fill = "#27282E", size = 0))



# Step means for every run of the models --------------------------------------

polar_steps <- ggplot() + 
  theme_classic() + 
  geom_line(data=step_means, aes(x=X, y=polar), colour="#8971E1", size = 1) +  # line
  theme(axis.text.y = element_text(size = 14, color = "white"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "white")) + 
  xlab("run") + ylab("Mean Polarization") +  # axis labels
  theme(plot.title = element_text(size = 16, face = "bold")) +  # title formatting
  theme(text = element_text(colour = "white", size = 16, face = "bold"))  + # label text
  theme(axis.line = element_line(color="white", size = 1),  # axis line & tick color
        axis.ticks = element_line(color="white")) +
  theme(panel.background = element_rect(fill = "#27282E"),  # plot & background colors, no outline
        plot.background = element_rect(fill = "#27282E", size = 0))

nnd_steps <- ggplot() + 
  theme_classic() + 
  geom_line(data=step_means, aes(x=X, y=nnd), colour="#8971E1", size = 1) +  # line
  theme(axis.text.y = element_text(size = 14, color = "white"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "white")) + 
  xlab("run") + ylab("Mean Nearest Neighbour Distance") +  # axis labels
  theme(plot.title = element_text(size = 16, face = "bold")) +  # title formatting
  theme(text = element_text(colour = "white", size = 16, face = "bold")) + # label text
  theme(axis.line = element_line(color="white", size = 1), # axis line & tick color
        axis.ticks = element_line(color="white")) +
  theme(panel.background = element_rect(fill = "#27282E"),  # plot & background colors, no outline
        plot.background = element_rect(fill = "#27282E", size = 0))

area_steps <- ggplot() + 
  theme_classic() + 
  geom_line(data=step_means, aes(x=X, y=area), colour="#8971E1", size = 1) +  # line
  theme(axis.text.y = element_text(size = 14, color = "white"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "white")) + 
  xlab("run") + ylab("Mean Shoal Area") +  # axis labels
  theme(plot.title = element_text(size = 16, face = "bold")) +  # title formatting
  theme(text = element_text(colour = "white", size = 16, face = "bold"))  + # label text
  theme(axis.line = element_line(color="white", size = 1),  # axis line & tick color
        axis.ticks = element_line(color="white")) +
  theme(panel.background = element_rect(fill = "#27282E"),  # plot & background colors, no outline
        plot.background = element_rect(fill = "#27282E", size = 0))

cent_steps <- ggplot() + 
  theme_classic() + 
  geom_line(data=step_means, aes(x=X, y=centroid), colour="#8971E1", size = 1) +  # line
  theme(axis.text.y = element_text(size = 14, color = "white"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "white")) + 
  xlab("run") + ylab("Mean Distance from Centroid") +  # axis labels
  theme(plot.title = element_text(size = 16, face = "bold")) +  # title formatting
  theme(text = element_text(colour = "white", size = 16, face = "bold")) + # label text
  theme(axis.line = element_line(color="white", size = 1),  # axis line & tick color
        axis.ticks = element_line(color="white")) +
  theme(panel.background = element_rect(fill = "#27282E"),  # plot & background colors, no outline
        plot.background = element_rect(fill = "#27282E", size = 0))



# Tracking graphs -------------------------------------------------------------

track_polar <- ggplot() + 
  theme_classic() + 
  geom_line(data=tracking, aes(x=X, y=polar), colour="#92D050", size = 1) +  # line
  theme(axis.text.y = element_text(size = 14, color = "white"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "white")) + 
  xlab("step") + ylab("Polarization") +  # axis labels
  theme(plot.title = element_text(size = 16, face = "bold")) +  # title formatting
  theme(text = element_text(colour = "white", size = 16, face = "bold"))  + # label text
  theme(axis.line = element_line(color="white", size = 1),  # axis line & tick color
        axis.ticks = element_line(color="white")) +
  theme(panel.background = element_rect(fill = "#27282E"),  # plot & background colors, no outline
        plot.background = element_rect(fill = "#27282E", size = 0))

track_nnd <- ggplot() + 
  theme_classic() + 
  geom_line(data=tracking, aes(x=X, y=nnd), colour="#92D050", size = 1) +  # line
  theme(axis.text.y = element_text(size = 14, color = "white"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "white")) + 
  xlab("step") + ylab("Nearest Neighbour Distance") +  # axis labels
  theme(plot.title = element_text(size = 16, face = "bold")) +  # title formatting
  theme(text = element_text(colour = "white", size = 16, face = "bold")) + # label text
  theme(axis.line = element_line(color="white", size = 1), # axis line & tick color
        axis.ticks = element_line(color="white")) +
  theme(panel.background = element_rect(fill = "#27282E"),  # plot & background colors, no outline
        plot.background = element_rect(fill = "#27282E", size = 0))

track_area <- ggplot() + 
  theme_classic() + 
  geom_line(data=tracking, aes(x=X, y=area), colour="#92D050", size = 1) +  # line
  theme(axis.text.y = element_text(size = 14, color = "white"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "white")) + 
  xlab("step") + ylab("Shoal Area") +  # axis labels
  theme(plot.title = element_text(size = 16, face = "bold")) +  # title formatting
  theme(text = element_text(colour = "white", size = 16, face = "bold"))  + # label text
  theme(axis.line = element_line(color="white", size = 1),  # axis line & tick color
        axis.ticks = element_line(color="white")) +
  theme(panel.background = element_rect(fill = "#27282E"),  # plot & background colors, no outline
        plot.background = element_rect(fill = "#27282E", size = 0))

track_cent <- ggplot() + 
  theme_classic() + 
  geom_line(data=tracking, aes(x=X, y=centroid), colour="#92D050", size = 1) +  # line
  theme(axis.text.y = element_text(size = 14, color = "white"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "white")) + 
  xlab("step") + ylab("Distance from Centroid") +  # axis labels
  theme(plot.title = element_text(size = 16, face = "bold")) +  # title formatting
  theme(text = element_text(colour = "white", size = 16, face = "bold")) + # label text
  theme(axis.line = element_line(color="white", size = 1),  # axis line & tick color
        axis.ticks = element_line(color="white")) +
  theme(panel.background = element_rect(fill = "#27282E"),  # plot & background colors, no outline
        plot.background = element_rect(fill = "#27282E", size = 0))

# Call multiplot function to combine graphs -----------------------------------
png("means_with_tracking.png", width = 20, height = 18, units = 'in', res = 300)
multiplot(polar_runs, nnd_runs, area_runs, cent_runs,  # multiplot fills by column
          polar_steps, nnd_steps, area_steps, cent_steps,
          track_polar, track_nnd, track_area, track_cent, cols=3)
dev.off()