# Script for making graphs for a presentation just comparing NND between the
# model and the stickleback video. Saved with transparent backgrounds & a 
# gorgeous color scheme to match the presentation, obvs.

setwd("~/Desktop/Local/Mackerel/Mackerel Data")

library(ggplot2)

model_nnd <- read.csv("nnd_mean.csv")
colnames(model_nnd) <- c("step", "nnd")

video_nnd <- read.csv("step1_nnd.csv")
colnames(video_nnd) <- c("step", "nnd")

model_graph <- ggplot() + 
  theme_classic() + 
  geom_line(data=model_nnd, aes(x=step, y=nnd), colour="#b742c6", size = 1) +  # line
  theme(axis.text.y = element_text(size = 14, color = "#d8d9dd"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "#d8d9dd")) + 
  xlab("step") + ylab("Mean Nearest Neighbour Distance") +  # axis labels
  theme(text = element_text(colour = "#ffffff", size = 16, face = "bold")) + # label text
  theme(axis.line = element_line(color="#d8d9dd", size = 1), # axis line & tick color
        axis.ticks = element_line(color="#d8d9dd")) +
  theme(panel.background = element_rect(fill = "transparent"),  # transparent background & no outline
        plot.background = element_rect(fill = "transparent", size = 0))
ggsave(model_graph, filename = "~/Desktop/model_graph.png", width = 7, height = 5, units = "in", 
       dpi = 300, bg = "transparent")


video_graph <- ggplot() + 
  theme_classic() + 
  geom_line(data=video_nnd, aes(x=step, y=nnd), colour="#d1438f", size = 1) +  # line
  theme(axis.text.y = element_text(size = 14, color = "#d8d9dd"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "#d8d9dd")) + 
  xlab("step") + ylab("Mean Nearest Neighbour Distance") +  # axis labels
  theme(plot.title = element_text(size = 16, face = "bold")) +  # title formatting
  theme(text = element_text(colour = "#ffffff", size = 16, face = "bold")) + # label text
  theme(axis.line = element_line(color="#d8d9dd", size = 1), # axis line & tick color
        axis.ticks = element_line(color="#d8d9dd")) +
  theme(panel.background = element_rect(fill = "transparent"),  # transparent background & no outline
        plot.background = element_rect(fill = "transparent", size = 0))
ggsave(video_graph, filename = "~/Desktop/video_graph.png", width = 7, height = 5, units = "in", 
       dpi = 300, bg = "transparent")