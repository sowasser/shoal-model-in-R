# Script for building graphs for sensitivity analysis of the shoal model.
# Current data outputs from the model are:
  # 1. Nearest Neighbour Distance
  # 2. Polarization (median absolute deviation in heading)
  # 3. Area (convex hull)
# Current variations are by agent number: 50, 100, 200.

library(ggplot2)
library(here)

source("multiplot.R")  # function for multiple plots on one page.

columns <- c('step', 'nnd', 'polar', 'area')  # create new column names

num_50 <- read.csv("shoal_data_50.csv")
colnames(num_50) <- columns
num_100 <- read.csv("shoal_data_100.csv")
colnames(num_100) <- columns
num_200 <- read.csv("shoal_data_200.csv")
colnames(num_200) <- columns

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
  geom_line(data=num_50, aes(x=step, y=nnd), colour="purple4", size = 1) +  # line
  theme(axis.text.y = element_text(size = 14, color = "black"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "black")) + 
  scale_y_continuous(limits = c(min_nnd, max_nnd)) +  # y-axis scale
  theme(axis.line = element_line(color="black", size = 1)) +
  xlab("step") + ylab("Nearest Neighbour Distance") +  # axis labels
  ggtitle("n = 50") +  # chart title text, left justified
  theme(plot.title = element_text(size = 16, face = 'bold')) +  # title formatting
  theme(text = element_text(colour = 'black', size = 16, face = 'bold'))  # label text

nnd100 <- ggplot() + 
  theme_classic() + 
  geom_line(data=num_100, aes(x=step, y=nnd), colour="purple4", size = 1) +  # line
  theme(axis.text.y = element_text(size = 14, color = "black"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "black")) + 
  scale_y_continuous(limits = c(min_nnd, max_nnd)) +  # y-axis scale
  theme(axis.line = element_line(color="black", size = 1)) +
  xlab("step") + ylab("Nearest Neighbour Distance") +  # axis labels
  ggtitle("n = 100") +  # chart title text, left justified
  theme(plot.title = element_text(size = 16, face = 'bold')) +  # title formatting
  theme(text = element_text(colour = 'black', size = 16, face = 'bold'))  # label text

nnd200 <- ggplot() + 
  theme_classic() + 
  geom_line(data=num_200, aes(x=step, y=nnd), colour="purple4", size = 1) +  # line
  theme(axis.text.y = element_text(size = 14, color = "black"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "black")) + 
  scale_y_continuous(limits = c(min_nnd, max_nnd)) +  # y-axis scale
  theme(axis.line = element_line(color="black", size = 1)) +
  xlab("step") + ylab("Nearest Neighbour Distance") +  # axis labels
  ggtitle("n = 200") +  # chart title text, left justified
  theme(plot.title = element_text(size = 16, face = 'bold')) +  # title formatting
  theme(text = element_text(colour = 'black', size = 16, face = 'bold'))  # label text


polar50 <- ggplot() + 
  theme_classic() + 
  geom_line(data=num_50, aes(x=step, y=polar), colour="chartreuse2", size = 1) +  # line
  theme(axis.text.y = element_text(size = 14, color = "black"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "black")) + 
  scale_y_continuous(limits = c(min_polar, max_polar)) +  # y-axis scale
  theme(axis.line = element_line(color="black", size = 1)) +
  xlab("step") + ylab("Polarization") +  # axis labels
  ggtitle("n = 50") +  # chart title text, left justified
  theme(plot.title = element_text(size = 16, face = 'bold')) +  # title formatting
  theme(text = element_text(colour = 'black', size = 16, face = 'bold'))  # label text

polar100 <- ggplot() + 
  theme_classic() + 
  geom_line(data=num_100, aes(x=step, y=polar), colour="chartreuse2", size = 1) +  # line
  theme(axis.text.y = element_text(size = 14, color = "black"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "black")) + 
  scale_y_continuous(limits = c(min_polar, max_polar)) +  # y-axis scale
  theme(axis.line = element_line(color="black", size = 1)) +
  xlab("step") + ylab("Polarization") +  # axis labels
  ggtitle("n = 100") +  # chart title text, left justified
  theme(plot.title = element_text(size = 16, face = 'bold')) +  # title formatting
  theme(text = element_text(colour = 'black', size = 16, face = 'bold'))  # label text

polar200 <- ggplot() + 
  theme_classic() + 
  geom_line(data=num_200, aes(x=step, y=polar), colour="chartreuse2", size = 1) +  # line
  theme(axis.text.y = element_text(size = 14, color = "black"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "black")) + 
  scale_y_continuous(limits = c(min_polar, max_polar)) +  # y-axis scale
  theme(axis.line = element_line(color="black", size = 1)) +
  xlab("step") + ylab("Polarization") +  # axis labels
  ggtitle("n = 200") +  # chart title text, left justified
  theme(plot.title = element_text(size = 16, face = 'bold')) +  # title formatting
  theme(text = element_text(colour = 'black', size = 16, face = 'bold'))  # label text
  
  
# Call multiplot function - located in separate file. Orders plot column by column
# Save plot to desktop
png("nnd_polar.png", width = 26.87, height = 26.87, units = 'cm', res = 300)
multiplot(nnd50, nnd100, nnd200, polar50, polar100, polar200, cols=2)
dev.off()



# Create graphs for shoal area (convex hull)
area50 <- ggplot() + 
  theme_classic() + 
  geom_line(data=num_50, aes(x=step, y=area), colour="purple4", size = 1) +  # line
  theme(axis.text.y = element_text(size = 14, color = "black"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "black")) + 
  scale_y_continuous(limits = c(min_area, max_area)) +  # y-axis scale
  theme(axis.line = element_line(color="black", size = 1)) +
  xlab("step") + ylab("Shoal Area") +  # axis labels
  ggtitle("n = 50") +  # chart title text, left justified
  theme(plot.title = element_text(size = 16, face = 'bold')) +  # title formatting
  theme(text = element_text(colour = 'black', size = 16, face = 'bold'))  # label text


area100 <- ggplot() + 
  theme_classic() + 
  geom_line(data=num_100, aes(x=step, y=area), colour="purple4", size = 1) +  # line
  theme(axis.text.y = element_text(size = 14, color = "black"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "black")) + 
  scale_y_continuous(limits = c(min_area, max_area)) +  # y-axis scale
  theme(axis.line = element_line(color="black", size = 1)) +
  xlab("step") + ylab("Shoal Area") +  # axis labels
  ggtitle("n = 100") +  # chart title text, left justified
  theme(plot.title = element_text(size = 16, face = 'bold')) +  # title formatting
  theme(text = element_text(colour = 'black', size = 16, face = 'bold'))  # label text

area200 <- ggplot() + 
  theme_classic() + 
  geom_line(data=num_200, aes(x=step, y=area), colour="purple4", size = 1) +  # line
  theme(axis.text.y = element_text(size = 14, color = "black"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "black")) + 
  scale_y_continuous(limits = c(min_area, max_area)) +  # y-axis scale
  theme(axis.line = element_line(color="black", size = 1)) +
  xlab("step") + ylab("Shoal Area") +  # axis labels
  ggtitle("n = 200") +  # chart title text, left justified
  theme(plot.title = element_text(size = 16, face = 'bold')) +  # title formatting
  theme(text = element_text(colour = 'black', size = 16, face = 'bold'))  # label text

# Blank graph for multiplot
blank <- ggplot() + theme_classic()

# Call multiplot function - located in separate file. Orders plot column by column
# Save plot to desktop
png("area.png", width = 26.87, height = 17.9, units = 'cm', res = 300)
multiplot(area50, area100, area200, blank, cols=2)
dev.off()
