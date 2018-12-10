# This script is for graphs of the tracking data collected in LoggerPro. These
# data were collected as a point for each fish for selected frames of the video. 

# There are three different functions used on the data and graphed here:
  # Mean distance from the centroid
  # Mean nearest neighbour distance
  # Shoal area (area of the convex hull)

# Polarization can be added for videos with multiple points tracked per fish.

setwd("~/Desktop/Local/Mackerel/Mackerel Data")

library(foreach)
library(ggplot2)

cent_dist <- read.csv("step1_cent_dist.csv")
colnames(cent_dist) <- c("step", "dist")

nnd <- read.csv("step1_nnd.csv")
colnames(nnd) <- c("step", "nnd")

area <- read.csv("step1_shoal_area.csv")
colnames(area) <- c("step", "area")

polar <- read.csv("step1_polar.csv")
colnames(polar) <- c("step", "polar")


cent_dist_graph <- ggplot() + 
  theme_classic() + 
  geom_line(data=cent_dist, aes(x=step, y=dist), colour="blue", size = 1) +  # line
  theme(axis.text.y = element_text(size = 14, color = "black"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "black")) + 
  theme(axis.line = element_line(color="black", size = 1)) +
  xlab("step") + ylab("Mean Distance from Centroid (mm)") +  # axis labels
  theme(plot.title = element_text(size = 16, face = 'bold')) +  # title formatting
  theme(text = element_text(colour = 'black', size = 16, face = 'bold')) + # label text
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", size = 0))
cent_dist_graph
ggsave(cent_dist_graph, filename = "step1_cent_dist.png", width=8, height=5.5, units="in", dpi = 300, bg = "transparent")


nnd_graph <- ggplot() + 
  theme_classic() + 
  geom_line(data=nnd, aes(x=step, y=nnd), colour="blue", size = 1) +  # line
  theme(axis.text.y = element_text(size = 14, color = "black"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "black")) + 
  theme(axis.line = element_line(color="black", size = 1)) +
  xlab("step") + ylab("Mean Nearest Neighbour Distance (mm)") +  # axis labels
  theme(plot.title = element_text(size = 16, face = 'bold')) +  # title formatting
  theme(text = element_text(colour = 'black', size = 16, face = 'bold')) + # label text
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", size = 0))
nnd_graph
ggsave(nnd_graph, filename = "step1_nnd.png", width=8, height=5.5, units="in", dpi = 300, bg = "transparent")

area_graph <- ggplot() + 
  theme_classic() + 
  geom_line(data=area, aes(x=step, y=area), colour="blue", size = 1) +  # line
  theme(axis.text.y = element_text(size = 14, color = "black"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "black")) + 
  theme(axis.line = element_line(color="black", size = 1)) +
  xlab("step") + ylab("Shoal Area (mm2)") +  # axis labels
  theme(plot.title = element_text(size = 16, face = 'bold')) +  # title formatting
  theme(text = element_text(colour = 'black', size = 16, face = 'bold'))  + # label text
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", size = 0))
area_graph
ggsave(area_graph, filename = "step1_area.png", width=8, height=5.5, units="in", dpi = 300, bg = "transparent")

polar_graph <- ggplot() + 
  theme_classic() + 
  geom_line(data=polar, aes(x=step, y=polar), colour="blue", size = 1) +  # line
  theme(axis.text.y = element_text(size = 14, color = "black"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "black")) + 
  theme(axis.line = element_line(color="black", size = 1)) +
  xlab("step") + ylab("Polarisation") +  # axis labels
  theme(plot.title = element_text(size = 16, face = 'bold')) +  # title formatting
  theme(text = element_text(colour = 'black', size = 16, face = 'bold'))  + # label text
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", size = 0))
polar_graph
ggsave(polar_graph, filename = "step1_polar.png", width=8, height=5.5, units="in", dpi = 300, bg = "transparent")
