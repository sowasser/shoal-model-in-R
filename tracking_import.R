# This R script is for working with the digitized videos from LoggerPro.
# The program exports a .csv file with x,y position for each individual and the
# x,y velocity for each. 

library(foreach)

track <- read.csv("tracking_fast_trial_17Nov.csv")

track <- track[c(1, 4:19)]  # Remove empty columns from mistakes while digitizing

# rename columns. Is there a better way to do this?
colnames(track) <- c("time", "x1", "y1", "x1_v", "y1_v", 
                     "x2", "y2", "x2_v", "y2_v", 
                     "x3", "y3", "x3_v", "y3_v", 
                     "x4", "y4", "x4_v", "y4_v")

pos <- track[c(1, 2, 3, 6, 7, 10, 11, 14, 15)]  # extract position data
velocity <- track[c(1, 4, 5, 8, 9, 12, 13)]  # extract velocities

# calculate distance between individuals.
# working from: https://stackoverflow.com/questions/24746892/how-to-calculate-euclidian-distance-between-two-points-defined-by-matrix-contain
dist <- function(p1, p2) sqrt(sum((p1 - p2) ^ 2))

foreach(i = 1:nrow(p1), .combine = c) %do% dist(p1[i,],p2[i,])

