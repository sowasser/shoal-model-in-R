# Script for building graphs for sensitivity analysis of the shoal model.

setwd("~/Desktop/Local/Mackerel/Mackerel_Data/shoal-model-in-R")

library(ggplot2)
source("multiplot.R")

columns <- c('step', 'nnd', 'polar')  # create new column names

num_100 <- read.csv("shoal_data_100.csv")
colnames(num_100) <- columns
num_50 <- read.csv("shoal_data_50.csv")
colnames(num_50) <- columns
num_200 <- read.csv("shoal_data_200.csv")
colnames(num_200) <- columns

# Colors for BES poster
# Cream: #FEF9F3
# Dark blue: #1B4460
# Light blue: #0A7794
# Burgundy: #6F115D


# Create graphs for nearest neighbour distance and polarization for each
# variation of agent number (50, 100, 200).
nnd50 <- ggplot() + 
  theme_classic() + 
  geom_line(data=num_50, aes(x=step, y=nnd), colour="#0A7794", size = 1) +  # line
  theme(axis.text.y = element_text(size = 10, color = "#1B4460"),  # axis text size & color
        axis.text.x = element_text(size = 10, color = "#1B4460")) + 
  theme(axis.line = element_line(color="#1B4460", size = 1)) +
  xlab("step") + ylab("Nearest Neighbour Distance") +  # axis labels
  ggtitle("n = 50") +  # chart title text, left justified
  theme(plot.title = element_text(size = 12, face = 'bold')) +  # title formatting
  theme(text = element_text(colour = '#1B4460', size = 12, face = 'bold'))  # label text

polar50 <- ggplot() + 
  theme_classic() + 
  geom_line(data=num_50, aes(x=step, y=polar), colour="#6F115D", size = 1) +  # line
  theme(axis.text.y = element_text(size = 10, color = "#1B4460"),  # axis text size & color
        axis.text.x = element_text(size = 10, color = "#1B4460")) + 
  theme(axis.line = element_line(color="#1B4460", size = 1)) +
  xlab("step") + ylab("Polarization") +  # axis labels
  ggtitle("n = 50") +  # chart title text, left justified
  theme(plot.title = element_text(size = 12, face = 'bold')) +  # title formatting
  theme(text = element_text(colour = '#1B4460', size = 12, face = 'bold'))  # label text

nnd100 <- ggplot() + 
  theme_classic() + 
  geom_line(data=num_100, aes(x=step, y=nnd), colour="#0A7794", size = 1) +  # line
  theme(axis.text.y = element_text(size = 10, color = "#1B4460"),  # axis text size & color
        axis.text.x = element_text(size = 10, color = "#1B4460")) + 
  theme(axis.line = element_line(color="#1B4460", size = 1)) +
  xlab("step") + ylab("Nearest Neighbour Distance") +  # axis labels
  ggtitle("n = 100") +  # chart title text, left justified
  theme(plot.title = element_text(size = 12, face = 'bold')) +  # title formatting
  theme(text = element_text(colour = '#1B4460', size = 12, face = 'bold'))  # label text

polar100 <- ggplot() + 
  theme_classic() + 
  geom_line(data=num_100, aes(x=step, y=polar), colour="#6F115D", size = 1) +  # line
  theme(axis.text.y = element_text(size = 10, color = "#1B4460"),  # axis text size & color
        axis.text.x = element_text(size = 10, color = "#1B4460")) + 
  theme(axis.line = element_line(color="#1B4460", size = 1)) +
  xlab("step") + ylab("Polarization") +  # axis labels
  ggtitle("n = 100") +  # chart title text, left justified
  theme(plot.title = element_text(size = 12, face = 'bold')) +  # title formatting
  theme(text = element_text(colour = '#1B4460', size = 12, face = 'bold'))  # label text

nnd200 <- ggplot() + 
  theme_classic() + 
  geom_line(data=num_200, aes(x=step, y=nnd), colour="#0A7794", size = 1) +  # line
  theme(axis.text.y = element_text(size = 10, color = "#1B4460"),  # axis text size & color
        axis.text.x = element_text(size = 10, color = "#1B4460")) + 
  theme(axis.line = element_line(color="#1B4460", size = 1)) +
  xlab("step") + ylab("Nearest Neighbour Distance") +  # axis labels
  ggtitle("n = 200") +  # chart title text, left justified
  theme(plot.title = element_text(size = 12, face = 'bold')) +  # title formatting
  theme(text = element_text(colour = '#1B4460', size = 12, face = 'bold'))  # label text

polar200 <- ggplot() + 
  theme_classic() + 
  geom_line(data=num_200, aes(x=step, y=polar), colour="#6F115D", size = 1) +  # line
  theme(axis.text.y = element_text(size = 10, color = "#1B4460"),  # axis text size & color
        axis.text.x = element_text(size = 10, color = "#1B4460")) + 
  theme(axis.line = element_line(color="#1B4460", size = 1)) +
  xlab("step") + ylab("Polarization") +  # axis labels
  ggtitle("n = 200") +  # chart title text, left justified
  theme(plot.title = element_text(size = 12, face = 'bold')) +  # title formatting
  theme(text = element_text(colour = '#1B4460', size = 12, face = 'bold'))  # label text

# Call multiplot function - located in seperate file. Orders plot column by column
multiplot(nnd50, nnd100, nnd200, polar50, polar100, polar200, cols=2)
