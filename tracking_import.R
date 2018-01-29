# This R script is for working with the digitized videos from LoggerPro.
# Since most of this file includes operations done by hand, rather than 
# iteratively, it's only really suited for working with small datasets.

# LoggerPro exports a .csv file with x,y position for each individual and the
# x,y velocity for each. In this script, the data is imported and cleaned.
# Then, the mean distance between individuals is calculated for each step of
# the model. This mean distance is then plotted.

library(foreach)

track <- read.csv("tracking_fast_trial_17Nov.csv") 

track <- track[c(1, 4:19)]  # Remove empty columns from mistakes while digitizing

# rename columns. Is there a better way to do this?
colnames(track) <- c("time", "x1", "y1", "x1_v", "y1_v", 
                     "x2", "y2", "x2_v", "y2_v", 
                     "x3", "y3", "x3_v", "y3_v", 
                     "x4", "y4", "x4_v", "y4_v")

position <- track[c(1, 2, 3, 6, 7, 10, 11, 14, 15)]  # extract position data
velocity <- track[c(1, 4, 5, 8, 9, 12, 13, 16, 17)]  # extract velocities

# calculate Euclidean distance between individuals.
# There has GOT to be a better way to do this.

# separate fish into individual dataframes
pos1 <- position[c(2, 3)]
pos2 <- position[c(4, 5)]
pos3 <- position[c(6, 7)]
pos4 <- position[c(8, 9)]

dist <- function(p1, p2) sqrt(sum((p1 - p2) ^ 2))  # function for Euclidean dist

# Distance between each different combination of fish. Blerg.
dist1_2 = foreach(i = 1:nrow(pos1), .combine = c) %do% dist(pos1[i,],pos2[i,])
dist1_3 = foreach(i = 1:nrow(pos1), .combine = c) %do% dist(pos1[i,],pos3[i,])
dist1_4 = foreach(i = 1:nrow(pos1), .combine = c) %do% dist(pos1[i,],pos4[i,])
dist2_3 = foreach(i = 1:nrow(pos2), .combine = c) %do% dist(pos2[i,],pos3[i,])
dist2_4 = foreach(i = 1:nrow(pos2), .combine = c) %do% dist(pos2[i,],pos4[i,])
dist3_4 = foreach(i = 1:nrow(pos3), .combine = c) %do% dist(pos3[i,],pos4[i,])

# Combine into one dataframe. Maybe not necessary?
distance <- cbind(pos[1], dist1_2, dist1_3, dist1_4, dist2_3, dist2_4, dist3_4)

# Calculate mean distance at each step
mean <- function(d1, d2, d3, d4, d5, d6) mean(d1, d2, d3, d4, d5, d6)
mean_dist <- foreach(i = 1:nrow(dist1_2)) %do% 
  mean(dist1_2[i,],dist1_3[i,],dist1_4[i,],dist2_3[i,],dist2_4[i,],dist3_4[i,])

# This isn't working. Foreach documentation: https://cran.r-project.org/web/packages/foreach/vignettes/foreach.pdf
