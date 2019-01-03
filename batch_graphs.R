# Script for building graphs for multiple runs of the shoaling model, currently
# over 100 steps and 10 runs.
# Current data outputs from the model are:
# 1. Nearest Neighbour Distance
# 2. Polarization (median absolute deviation in heading)
# 3. Shoal Area (convex hull)
# 4. Mean distance from centroid

setwd("~/Desktop/Local/Mackerel/Mackerel Data")

library(ggplot2)

  
polar_mean <- read.csv("polar_mean.csv")
colnames(polar_mean) <- c("step", "polar")

nnd_mean <- read.csv("nnd_mean.csv")
colnames(nnd_mean) <- c("step", "nnd")

area_mean <- read.csv("area_mean.csv")
colnames(area_mean) <- c("step", "area")

cent_mean <- read.csv("cent_mean.csv")
colnames(cent_mean) <- c("step", "cent")


polar_mean_graph <- ggplot() + 
  theme_classic() + 
  geom_line(data=polar_mean, aes(x=step, y=polar), colour="#DA7318", size = 1) +  # line
  theme(axis.text.y = element_text(size = 14, color = "#737373"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "#737373")) + 
  xlab("step") + ylab("Mean Polarisation") +  # axis labels
  theme(plot.title = element_text(size = 16, face = "bold")) +  # title formatting
  theme(text = element_text(colour = "#A6B7C8", size = 16, face = "bold"))  + # label text
  theme(axis.line = element_line(color="#737373", size = 1),  # axis line & tick color
        axis.ticks = element_line(color="#737373")) +
  theme(panel.background = element_rect(fill = "transparent"),  # transparent background & no outline
        plot.background = element_rect(fill = "transparent", size = 0))
ggsave(polar_mean_graph, filename = "polar_mean.png", width = 17.09, height = 11.76, units = "cm", 
       dpi = 300, bg = "transparent")

nnd_mean_graph <- ggplot() + 
  theme_classic() + 
  geom_line(data=nnd_mean, aes(x=step, y=nnd), colour="#DA7318", size = 1) +  # line
  theme(axis.text.y = element_text(size = 14, color = "#737373"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "#737373")) + 
  xlab("step") + ylab("Mean Nearest Neighbour Distance") +  # axis labels
  theme(plot.title = element_text(size = 16, face = "bold")) +  # title formatting
  theme(text = element_text(colour = "#A6B7C8", size = 16, face = "bold")) + # label text
  theme(axis.line = element_line(color="#737373", size = 1), # axis line & tick color
        axis.ticks = element_line(color="#737373")) +
  theme(panel.background = element_rect(fill = "transparent"),  # transparent background & no outline
        plot.background = element_rect(fill = "transparent", size = 0))
ggsave(nnd_mean_graph, filename = "nnd_mean.png", width = 17.09, height = 11.76, units = "cm", 
       dpi = 300, bg = "transparent")

area_mean_graph <- ggplot() + 
  theme_classic() + 
  geom_line(data=area_mean, aes(x=step, y=area), colour="#DA7318", size = 1) +  # line
  theme(axis.text.y = element_text(size = 14, color = "#737373"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "#737373")) + 
  xlab("step") + ylab("Mean Shoal Area") +  # axis labels
  theme(plot.title = element_text(size = 16, face = "bold")) +  # title formatting
  theme(text = element_text(colour = "#A6B7C8", size = 16, face = "bold"))  + # label text
  theme(axis.line = element_line(color="#737373", size = 1),  # axis line & tick color
        axis.ticks = element_line(color="#737373")) +
  theme(panel.background = element_rect(fill = "transparent"),  # transparent background & no outline
        plot.background = element_rect(fill = "transparent", size = 0))
ggsave(area_mean_graph, filename = "area_mean.png", width = 17.09, height = 11.76, units = "cm", 
       dpi = 300, bg = "transparent")

cent_mean_graph <- ggplot() + 
  theme_classic() + 
  geom_line(data=cent_mean, aes(x=step, y=cent), colour="#DA7318", size = 1) +  # line
  theme(axis.text.y = element_text(size = 14, color = "#737373"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "#737373")) + 
  xlab("step") + ylab("Mean Distance from Centroid") +  # axis labels
  theme(plot.title = element_text(size = 16, face = "bold")) +  # title formatting
  theme(text = element_text(colour = "#A6B7C8", size = 16, face = "bold")) + # label text
  theme(axis.line = element_line(color="#737373", size = 1),  # axis line & tick color
        axis.ticks = element_line(color="#737373")) +
  theme(panel.background = element_rect(fill = "transparent"),  # transparent background & no outline
        plot.background = element_rect(fill = "transparent", size = 0))
ggsave(cent_mean_graph, filename = "cent_mean.png", width = 17.09, height = 11.76, units = "cm", 
       dpi = 300, bg = "transparent")
