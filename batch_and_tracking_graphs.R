# This script combines the batch graphs and the tracking graphs for 
# presentations and the like.

# Colors for ICES poster:
# Pink: #C830CC
# Purple: "#8971E1"
# Green: "#92D050"
# Background: "#27282E"

library(ggplot2)
library(foreach)

path <- "~/Desktop/DO NOT ERASE/1NUIG/Mackerel/Mackerel Data/"  # for laptop
# path <- "~/Desktop/Local/Mackerel/Mackerel Data/"  # for desktop

source(paste0(path,"multiplot.R"))

# Read in data ----------------------------------------------------------------
run_means <- read.csv(paste0(path,"batch_means_runs.csv"))
model_means <- read.csv(paste0(path,"means_var-speed.csv"))
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


# Tracking graphs -------------------------------------------------------------

track_polar <- ggplot() + 
  theme_classic() + 
  geom_line(data=tracking, aes(x=X, y=polar), colour="#8971E1", size = 1) +  # line
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
  geom_line(data=tracking, aes(x=X, y=nnd), colour="#8971E1", size = 1) +  # line
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
  geom_line(data=tracking, aes(x=X, y=area), colour="#8971E1", size = 1) +  # line
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
  geom_line(data=tracking, aes(x=X, y=centroid), colour="#8971E1", size = 1) +  # line
  theme(axis.text.y = element_text(size = 14, color = "white"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "white")) + 
  xlab("step") + ylab("Distance from Centroid") +  # axis labels
  theme(plot.title = element_text(size = 16, face = "bold")) +  # title formatting
  theme(text = element_text(colour = "white", size = 16, face = "bold")) + # label text
  theme(axis.line = element_line(color="white", size = 1),  # axis line & tick color
        axis.ticks = element_line(color="white")) +
  theme(panel.background = element_rect(fill = "#27282E"),  # plot & background colors, no outline
        plot.background = element_rect(fill = "#27282E", size = 0))


# Boxplots for mean data collectors over all steps & runs ---------------------
polar_box <- ggplot() + 
  theme_classic() +
  geom_boxplot(data = model_means, aes(x=var, y=polar, group=var), # boxplot
               colour="white",  fill="#C830CC", width=4) +  # line & box fill colors; box width
  theme(axis.text.y = element_text(size = 14, color = "white"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "white")) + 
  xlab("speed") + ylab("Mean Polarization") +  # axis labels
  theme(plot.title = element_text(size = 16, face = "bold")) +  # title formatting
  theme(text = element_text(colour = "white", size = 16, face = "bold")) + # label text
  theme(axis.line = element_line(color="white", size = 1),  # axis line & tick color
        axis.ticks = element_line(color="white")) +
  theme(panel.background = element_rect(fill = "#27282E"),  # plot & background colors, no outline
        plot.background = element_rect(fill = "#27282E", size = 0))

nnd_box <- ggplot() + 
  theme_classic() +
  geom_boxplot(data = model_means, aes(x=var, y=nnd, group=var), # boxplot
               colour="white",  fill="#C830CC", width=4) +  # line & box fill colors; box width
  theme(axis.text.y = element_text(size = 14, color = "white"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "white")) + 
  xlab("speed") + ylab("Mean Nearest Neighbour Distance") +  # axis labels
  theme(plot.title = element_text(size = 16, face = "bold")) +  # title formatting
  theme(text = element_text(colour = "white", size = 16, face = "bold")) + # label text
  theme(axis.line = element_line(color="white", size = 1),  # axis line & tick color
        axis.ticks = element_line(color="white")) +
  theme(panel.background = element_rect(fill = "#27282E"),  # plot & background colors, no outline
        plot.background = element_rect(fill = "#27282E", size = 0))

area_box <- ggplot() + 
  theme_classic() +
  geom_boxplot(data = model_means, aes(x=var, y=area, group=var), # boxplot
               colour="white",  fill="#C830CC", width=4) +  # line & box fill colors; box width
  theme(axis.text.y = element_text(size = 14, color = "white"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "white")) + 
  xlab("speed") + ylab("Mean Shoal Area") +  # axis labels
  theme(plot.title = element_text(size = 16, face = "bold")) +  # title formatting
  theme(text = element_text(colour = "white", size = 16, face = "bold")) + # label text
  theme(axis.line = element_line(color="white", size = 1),  # axis line & tick color
        axis.ticks = element_line(color="white")) +
  theme(panel.background = element_rect(fill = "#27282E"),  # plot & background colors, no outline
        plot.background = element_rect(fill = "#27282E", size = 0))

cent_box <- ggplot() + 
  theme_classic() +
  geom_boxplot(data = model_means, aes(x=var, y=centroid, group=var), # boxplot
               colour="white",  fill="#C830CC", width=4) +  # line & box fill colors; box width
  theme(axis.text.y = element_text(size = 14, color = "white"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "white")) + 
  xlab("speed") + ylab("Mean Distance from Centroid") +  # axis labels
  theme(plot.title = element_text(size = 16, face = "bold")) +  # title formatting
  theme(text = element_text(colour = "white", size = 16, face = "bold")) + # label text
  theme(axis.line = element_line(color="white", size = 1),  # axis line & tick color
        axis.ticks = element_line(color="white")) +
  theme(panel.background = element_rect(fill = "#27282E"),  # plot & background colors, no outline
        plot.background = element_rect(fill = "#27282E", size = 0))

# Call multiplot function to combine graphs -----------------------------------
png("~/Desktop/means_with_tracking.png", width = 20, height = 18, units = 'in', res = 300)
multiplot(polar_box, nnd_box, area_box, cent_box, polar_runs,  # multiplot fills by column
          nnd_runs, area_runs, cent_runs,
          track_polar, track_nnd, track_area, track_cent, cols=3)
dev.off()