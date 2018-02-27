# This R script is for working with the digitized videos from LoggerPro.
# Since most of this file includes operations done by hand, rather than 
# iteratively, it's only really suited for working with small datasets.

# LoggerPro exports a .csv file with x,y position for each individual and the
# x,y velocity for each. In this script, the data are imported and cleaned.
# Then, the mean distance between individuals is calculated for each step of
# the model. This mean distance is then plotted two ways.

# install.packages("foreach")
# install.packages("ggplot2")

library(foreach)
library(ggplot2)

track <- read.csv("fish_30-6s_f7-2.csv") 

track <- read.table("file.csv", header=FALSE, sep=",")

# track <- track[c(1, 4:19)]  # Remove empty columns from mistakes while digitizing

# rename columns. Is there a better way to do this?
colnames(track) <- c("time", "x1", "y1", "x1_v", "y1_v", 
                     "x2", "y2", "x2_v", "y2_v", 
                     "x3", "y3", "x3_v", "y3_v", 
                     "x4", "y4", "x4_v", "y4_v",
                     "x5", "y5", "x5_v", "y5_v",
                     "x6", "y6", "x6_v", "y6_v",
                     "x7", "y7", "x7_v", "y7_v")

position <- track[c(1, 2, 3, 6, 7, 10, 11, 14, 15, 18, 19, 22, 23, 26, 27)]  # extract position data
velocity <- track[c(1, 4, 5, 8, 9, 12, 13, 16, 17, 20, 21, 24, 25, 28, 29)]  # extract velocities

# calculate Euclidean distance between individuals.
# There has GOT to be a better way to do this.

# separate fish into individual dataframes
pos1 <- position[c(2, 3)]
pos2 <- position[c(4, 5)]
pos3 <- position[c(6, 7)]
pos4 <- position[c(8, 9)]
pos5 <- position[c(10, 11)]
pos6 <- position[c(12, 13)]
pos7 <- position[c(14, 15)]

dist <- function(p1, p2) sqrt(sum((p1 - p2) ^ 2))  # function for Euclidean dist

# Distance between each different combination of fish. Better way?
dist1_2 = foreach(i = 1:nrow(pos1), .combine = c) %do% dist(pos1[i,],pos2[i,])
dist1_3 = foreach(i = 1:nrow(pos1), .combine = c) %do% dist(pos1[i,],pos3[i,])
dist1_4 = foreach(i = 1:nrow(pos1), .combine = c) %do% dist(pos1[i,],pos4[i,])
dist2_3 = foreach(i = 1:nrow(pos2), .combine = c) %do% dist(pos2[i,],pos3[i,])
dist2_4 = foreach(i = 1:nrow(pos2), .combine = c) %do% dist(pos2[i,],pos4[i,])
dist3_4 = foreach(i = 1:nrow(pos3), .combine = c) %do% dist(pos3[i,],pos4[i,])

distance <- cbind(position[1], dist1_2, dist1_3, dist1_4, dist2_3, dist2_4, dist3_4)
mean_d <- rowMeans(distance[,-1])  # calculate means, excluding time
overall_mean <- mean(mean_d)
mean_dist <- cbind(position[1], mean_d)  # re-bind with time

write.csv(mean_dist, file = "mean_dist.csv")  # write .csv file of mean distance & time

# Plot mean distancce against time
plot(x=mean_dist$time, y=mean_dist$mean_d, type="l")

# Nicer graph using ggplot
ggplot() + 
  theme_classic() + 
  geom_line(data=mean_dist, aes(x=time, y=mean_d), colour="blue", size = 1) +  # line
  theme(axis.text.y = element_text(size = 14, color = "black"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "black")) + 
  theme(axis.line = element_line(color="black", size = 1)) +
  xlab("time (min)") + ylab("Mean Distance (cm)") +  # axis labels
  theme(plot.title = element_text(size = 16, face = 'bold')) +  # title formatting
  theme(text = element_text(colour = 'black', size = 16, face = 'bold'))  # label text
