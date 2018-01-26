# This R script is for working with the digitized videos from LoggerPro.
# The program exports a .csv file with x,y position for each individual and the
# x,y velocity for each. 

library(foreach)

track <- read.csv("sticklebacks1_300x.csv")

# track <- track[c(1, 4:19)]  # Remove empty columns from mistakes while digitizing

# rename columns. Is there a better way to do this?
colnames(track) <- c("time", "x1", "y1", "x1_v", "y1_v", 
                     "x2", "y2", "x2_v", "y2_v", 
                     "x3", "y3", "x3_v", "y3_v", 
                     "x4", "y4", "x4_v", "y4_v",
                     "x5", "y5", "x5_v", "y5_v",
                     "x6", "y6", "x6_v", "y6_v",
                     "x7", "y7", "x7_v", "y7_v",
                     "x8", "y8", "x8_v", "y8_v",
                     "x9", "y9", "x9_v", "y9_v",
                     "x10", "y10", "x10_v", "y10_v",
                     "x11", "y11", "x11_v", "y11_v",
                     "x12", "y12", "x12_v", "y12_v",
                     "x13", "y13", "x13_v", "y13_v",
                     "x14", "y14", "x14_v", "y14_v",
                     "x15", "y15", "x15_v", "y15_v",
                     "x16", "y16", "x16_v", "y16_v",
                     "x17", "y17", "x17_v", "y17_v",
                     "x18", "y18", "x18_v", "y18_v",
                     "x19", "y19", "x19_v", "y19_v")

pos <- track[c(1, 2, 3, 6, 7, 10, 11, 14, 15)]  # extract position data
velocity <- track[c(1, 4, 5, 8, 9, 12, 13)]  # extract velocities

# calculate Euclidean distance between individuals.
# There has GOT to be a better way to do this.

# separate fish into individual dataframes
pos1 <- pos[c(2, 3)]
pos2 <- pos[c(4, 5)]
pos3 <- pos[c(6, 7)]
pos4 <- pos[c(8, 9)]

# working from: https://stackoverflow.com/questions/24746892/how-to-calculate-euclidian-distance-between-two-points-defined-by-matrix-contain
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
