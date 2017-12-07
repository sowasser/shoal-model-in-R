# Script for building graphs for sensitivity analysis of the shoal model.

setwd("~/Desktop/Local/Mackerel/Mackerel_Data")

library(ggplot2)
source("~/Desktop/Local/Mackerel/Mackerel_Data/shoal-model-in-R/multiplot.R")

columns <- c('step', 'nnd', 'polar', 'area')  # create new column names

num_50 <- read.csv("shoal_data_50.csv")
colnames(num_50) <- columns
num_100 <- read.csv("shoal_data_100.csv")
colnames(num_100) <- columns
num_200 <- read.csv("shoal_data_200.csv")
colnames(num_200) <- columns

# Colors for BES poster
# Cream: #FEF9F3
# Dark blue: #1B4460
# Light blue: #0A7794
# Burgundy: #6F115D

# Find overall ranges so each graph has axes of the same scale
max_nnd <- c(max(num_50$nnd), max(num_100$nnd), max(num_200$nnd))
max_nnd <- max(max_nnd)
min_nnd <- c(min(num_50$nnd), min(num_100$nnd), min(num_200$nnd))
min_nnd <- min(min_nnd)

max_polar <- c(max(num_50$polar), max(num_100$polar), max(num_200$polar))
max_polar <- max(max_polar)
min_polar <- c(min(num_50$polar), min(num_100$polar), min(num_200$polar))
min_polar <- min(min_polar)

max_area <- c(max(num_50$area), max(num_100$area), max(num_200$area))
max_area <- max(max_area)
min_area <- c(min(num_50$area), min(num_100$area), min(num_200$area))
min_area <- min(min_area)

# Create graphs for nearest neighbour distance and polarization for each
# variation of agent number (50, 100, 200).
nnd50 <- ggplot() + 
  theme_classic() + 
  geom_line(data=num_50, aes(x=step, y=nnd), colour="#0A7794", size = 1) +  # line
  theme(axis.text.y = element_text(size = 14, color = "#1B4460"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "#1B4460")) + 
  scale_y_continuous(limits = c(min_nnd, max_nnd)) +  # y-axis scale
  theme(axis.line = element_line(color="#1B4460", size = 1)) +
  xlab("step") + ylab("Nearest Neighbour Distance") +  # axis labels
  ggtitle("n = 50") +  # chart title text, left justified
  theme(plot.title = element_text(size = 16, face = 'bold')) +  # title formatting
  theme(text = element_text(colour = '#1B4460', size = 16, face = 'bold')) +  # label text
  theme(
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent")
  )

nnd100 <- ggplot() + 
  theme_classic() + 
  geom_line(data=num_100, aes(x=step, y=nnd), colour="#0A7794", size = 1) +  # line
  theme(axis.text.y = element_text(size = 14, color = "#1B4460"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "#1B4460")) + 
  scale_y_continuous(limits = c(min_nnd, max_nnd)) +  # y-axis scale
  theme(axis.line = element_line(color="#1B4460", size = 1)) +
  xlab("step") + ylab("Nearest Neighbour Distance") +  # axis labels
  ggtitle("n = 100") +  # chart title text, left justified
  theme(plot.title = element_text(size = 16, face = 'bold')) +  # title formatting
  theme(text = element_text(colour = '#1B4460', size = 16, face = 'bold')) +  # label text
  theme(
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent")
  )

nnd200 <- ggplot() + 
  theme_classic() + 
  geom_line(data=num_200, aes(x=step, y=nnd), colour="#0A7794", size = 1) +  # line
  theme(axis.text.y = element_text(size = 14, color = "#1B4460"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "#1B4460")) + 
  scale_y_continuous(limits = c(min_nnd, max_nnd)) +  # y-axis scale
  theme(axis.line = element_line(color="#1B4460", size = 1)) +
  xlab("step") + ylab("Nearest Neighbour Distance") +  # axis labels
  ggtitle("n = 200") +  # chart title text, left justified
  theme(plot.title = element_text(size = 16, face = 'bold')) +  # title formatting
  theme(text = element_text(colour = '#1B4460', size = 16, face = 'bold')) +  # label text
  theme(
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent")
  )



polar50 <- ggplot() + 
  theme_classic() + 
  geom_line(data=num_50, aes(x=step, y=polar), colour="#6F115D", size = 1) +  # line
  theme(axis.text.y = element_text(size = 14, color = "#1B4460"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "#1B4460")) + 
  scale_y_continuous(limits = c(min_polar, max_polar)) +  # y-axis scale
  theme(axis.line = element_line(color="#1B4460", size = 1)) +
  xlab("step") + ylab("Polarization") +  # axis labels
  ggtitle("n = 50") +  # chart title text, left justified
  theme(plot.title = element_text(size = 16, face = 'bold')) +  # title formatting
  theme(text = element_text(colour = '#1B4460', size = 16, face = 'bold')) +  # label text
  theme(
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent")
  )

polar100 <- ggplot() + 
  theme_classic() + 
  geom_line(data=num_100, aes(x=step, y=polar), colour="#6F115D", size = 1) +  # line
  theme(axis.text.y = element_text(size = 14, color = "#1B4460"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "#1B4460")) + 
  scale_y_continuous(limits = c(min_polar, max_polar)) +  # y-axis scale
  theme(axis.line = element_line(color="#1B4460", size = 1)) +
  xlab("step") + ylab("Polarization") +  # axis labels
  ggtitle("n = 100") +  # chart title text, left justified
  theme(plot.title = element_text(size = 16, face = 'bold')) +  # title formatting
  theme(text = element_text(colour = '#1B4460', size = 16, face = 'bold')) +  # label text
  theme(
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent")
  )

polar200 <- ggplot() + 
  theme_classic() + 
  geom_line(data=num_200, aes(x=step, y=polar), colour="#6F115D", size = 1) +  # line
  theme(axis.text.y = element_text(size = 14, color = "#1B4460"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "#1B4460")) + 
  scale_y_continuous(limits = c(min_polar, max_polar)) +  # y-axis scale
  theme(axis.line = element_line(color="#1B4460", size = 1)) +
  xlab("step") + ylab("Polarization") +  # axis labels
  ggtitle("n = 200") +  # chart title text, left justified
  theme(plot.title = element_text(size = 16, face = 'bold')) +  # title formatting
  theme(text = element_text(colour = '#1B4460', size = 16, face = 'bold')) +  # label text
  theme(
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent")
  )

# Call multiplot function - located in separate file. Orders plot column by column
# Save plot to desktop
png("~/Desktop/nnd_polar_transparent.png", width = 26.87, height = 26.87, units = 'cm', bg = 'transparent', res = 300)
multiplot(nnd50, nnd100, nnd200, polar50, polar100, polar200, cols=2)
dev.off()



# Create graphs for shoal area (convex hull)
area50 <- ggplot() + 
  theme_classic() + 
  geom_line(data=num_50, aes(x=step, y=area), colour="#6F115D", size = 1) +  # line
  theme(axis.text.y = element_text(size = 14, color = "#1B4460"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "#1B4460")) + 
  scale_y_continuous(limits = c(min_area, max_area)) +  # y-axis scale
  theme(axis.line = element_line(color="#1B4460", size = 1)) +
  xlab("step") + ylab("Shoal Area") +  # axis labels
  ggtitle("n = 50") +  # chart title text, left justified
  theme(plot.title = element_text(size = 16, face = 'bold')) +  # title formatting
  theme(text = element_text(colour = '#1B4460', size = 16, face = 'bold')) +  # label text
  theme(
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent")
  )

area100 <- ggplot() + 
  theme_classic() + 
  geom_line(data=num_100, aes(x=step, y=area), colour="#6F115D", size = 1) +  # line
  theme(axis.text.y = element_text(size = 14, color = "#1B4460"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "#1B4460")) + 
  scale_y_continuous(limits = c(min_area, max_area)) +  # y-axis scale
  theme(axis.line = element_line(color="#1B4460", size = 1)) +
  xlab("step") + ylab("Shoal Area") +  # axis labels
  ggtitle("n = 100") +  # chart title text, left justified
  theme(plot.title = element_text(size = 16, face = 'bold')) +  # title formatting
  theme(text = element_text(colour = '#1B4460', size = 16, face = 'bold')) +  # label text
  theme(
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent")
  )

area200 <- ggplot() + 
  theme_classic() + 
  geom_line(data=num_200, aes(x=step, y=area), colour="#6F115D", size = 1) +  # line
  theme(axis.text.y = element_text(size = 14, color = "#1B4460"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "#1B4460")) + 
  scale_y_continuous(limits = c(min_area, max_area)) +  # y-axis scale
  theme(axis.line = element_line(color="#1B4460", size = 1)) +
  xlab("step") + ylab("Shoal Area") +  # axis labels
  ggtitle("n = 200") +  # chart title text, left justified
  theme(plot.title = element_text(size = 16, face = 'bold')) +  # title formatting
  theme(text = element_text(colour = '#1B4460', size = 16, face = 'bold')) +  # label text
  theme(
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent")
  )

# Blank graph for multiplot
blank <- ggplot() + theme_classic() +
  theme(
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent")
  )

# Call multiplot function - located in separate file. Orders plot column by column
# Save plot to desktop
png("~/Desktop/area_transparent.png", width = 26.87, height = 17.9, units = 'cm', bg = 'transparent', res = 300)
multiplot(area50, area100, area200, blank, cols=2)
dev.off()
