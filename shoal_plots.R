# Plots for the results of the data collectors of the preliminary shoaling
# model: nearest neighbour distance and polarization.

setwd("~/Desktop/Dropbox/NUIG/Mackerel/Mackerel_Data")
library(ggplot2)

data <- read.csv("shoal_data.csv")
colnames(data) <- c("step", "NND", "polar")

nnd <- ggplot() +
  theme_bw() + 
  geom_line(data=data, aes( x=step, y=NND), colour="blue", size = 2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(panel.border = element_blank()) + 
  theme(axis.text = element_text(size = 12)) + 
  theme(axis.line = element_line(color="black", size = 0.5)) +
  xlab("\nStep") + ylab("Nearest Neighbour Distance\n") +
  theme(text = element_text(colour = 'black', size = 14, face = 'bold'))
nnd

polar <- ggplot() +
  theme_bw() + 
  geom_line(data=data, aes( x=step, y=polar), colour="blue", size = 2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(panel.border = element_blank()) + 
  theme(axis.text = element_text(size = 12)) + 
  theme(axis.line = element_line(color="black", size = 0.5)) +
  xlab("\nStep") + ylab("Polarization (MAD)\n") +
  theme(text = element_text(colour = 'black', size = 14, face = 'bold'))
polar
