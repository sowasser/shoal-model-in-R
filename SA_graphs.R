# Script for building graphs for sensitivity analysis of the shoal model.

setwd("~/Desktop/Local/Mackerel/Mackerel_Data/shoal-model-in-R")

library(ggplot2)

columns <- c('step', 'nnd', 'polar')  # create new column names

num_100 <- read.csv("shoal_data_100.csv")
colnames(num_100) <- columns
num_50 <- read.csv("shoal_data_50.csv")
colnames(num_50) <- columns
num_200 <- read.csv("shoal_data_200.csv")
colnames(num_200) <- columns


# Create graphs for nearest neighbour distance and polarization for each
# variation of agent number (50, 100, 200).
nnd50 <- ggplot() + 
  theme_bw() + 
  geom_line(data=num_50, aes(x=step, y=nnd), colour="blue", size = 1) +  # line
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +  # no gridlines
  theme(axis.text.y = element_text(size = 10),  # axis text size
        axis.text.x = element_text(size = 10)) + 
  theme(axis.line = element_line(color="black", size = 1)) +
  xlab("step") + ylab("Nearest Neighbour Distance") +  # axis labels
  ggtitle("n = 50") +  # chart title text, left justified
  theme(plot.title = element_text(size = 12, face = 'bold')) +  # title formatting
  theme(text = element_text(colour = 'black', size = 12, face = 'bold')) +   # label text
  theme(panel.border = element_rect(colour = 'white',size = 1))  # no border (white border)
polar50 <- ggplot() + 
  theme_bw() + 
  geom_line(data=num_50, aes(x=step, y=polar), colour="red", size = 1) +  # line
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +  # no gridlines
  theme(axis.text.y = element_text(size = 10),  # axis text size
        axis.text.x = element_text(size = 10)) + 
  theme(axis.line = element_line(color="black", size = 1)) +
  xlab("step") + ylab("Polarization") +  # axis labels
  ggtitle("n = 50") +  # chart title text, left justified
  theme(plot.title = element_text(size = 12, face = 'bold')) +  # title formatting
  theme(text = element_text(colour = 'black', size = 12, face = 'bold')) +   # label text
  theme(panel.border = element_rect(colour = 'white',size = 1))  # no border (white border)

nnd100 <- ggplot() + 
  theme_bw() + 
  geom_line(data=num_100, aes(x=step, y=nnd), colour="blue", size = 1) +  # line
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +  # no gridlines
  theme(axis.text.y = element_text(size = 10),  # axis text size
        axis.text.x = element_text(size = 10)) + 
  theme(axis.line = element_line(color="black", size = 1)) +
  xlab("step") + ylab("Nearest Neighbour Distance") +  # axis labels
  ggtitle("n = 100") +  # chart title text, left justified
  theme(plot.title = element_text(size = 12, face = 'bold')) +  # title formatting
  theme(text = element_text(colour = 'black', size = 12, face = 'bold')) +   # label text
  theme(panel.border = element_rect(colour = 'white',size = 1))  # no border (white border)
polar100 <- ggplot() + 
  theme_bw() + 
  geom_line(data=num_100, aes(x=step, y=polar), colour="red", size = 1) +  # line
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +  # no gridlines
  theme(axis.text.y = element_text(size = 10),  # axis text size
        axis.text.x = element_text(size = 10)) + 
  theme(axis.line = element_line(color="black", size = 1)) +
  xlab("step") + ylab("Polarization") +  # axis labels
  ggtitle("n = 100") +  # chart title text, left justified
  theme(plot.title = element_text(size = 12, face = 'bold')) +  # title formatting
  theme(text = element_text(colour = 'black', size = 12, face = 'bold')) +   # label text
  theme(panel.border = element_rect(colour = 'white',size = 1))  # no border (white border)

nnd200 <- ggplot() + 
  theme_bw() + 
  geom_line(data=num_200, aes(x=step, y=nnd), colour="blue", size = 1) +  # line
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +  # no gridlines
  theme(axis.text.y = element_text(size = 10),  # axis text size
        axis.text.x = element_text(size = 10)) + 
  theme(axis.line = element_line(color="black", size = 1)) +
  xlab("step") + ylab("Nearest Neighbour Distance") +  # axis labels
  ggtitle("n = 200") +  # chart title text, left justified
  theme(plot.title = element_text(size = 12, face = 'bold')) +  # title formatting
  theme(text = element_text(colour = 'black', size = 12, face = 'bold')) +   # label text
  theme(panel.border = element_rect(colour = 'white',size = 1))  # no border (white border)
polar200 <- ggplot() + 
  theme_bw() + 
  geom_line(data=num_200, aes(x=step, y=polar), colour="red", size = 1) +  # line
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +  # no gridlines
  theme(axis.text.y = element_text(size = 10),  # axis text size
        axis.text.x = element_text(size = 10)) + 
  theme(axis.line = element_line(color="black", size = 1)) +
  xlab("step") + ylab("Polarization") +  # axis labels
  ggtitle("n = 200") +  # chart title text, left justified
  theme(plot.title = element_text(size = 12, face = 'bold')) +  # title formatting
  theme(text = element_text(colour = 'black', size = 12, face = 'bold')) +   # label text
  theme(panel.border = element_rect(colour = 'white',size = 1))  # no border (white border)

# Call multiplot function - located in seperate file. Orders plot column by column
multiplot(nnd50, nnd100, nnd200, polar50, polar100, polar200, cols=2)
