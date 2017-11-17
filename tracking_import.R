# This R script is for working with the digitized videos from LoggerPro.
# The program exports a .csv file with x,y position for each individual and the
# x,y velocity for each. 

setwd("~/Desktop/Local/Mackerel/Mackerel_Data")
track <- read.csv("tracking_fast_trial_17Nov.csv")

track <- track[c(1, 4:19)]  # Remove empty columns from mistakes while digitizing

# rename columns. Is there a better way to do this?
colnames(track) <- c("time", "x1", "y1", "x1_v", "y1_v", 
                     "x2", "y2", "x2_v", "y2_v", 
                     "x3", "y3", "x3_v", "y3_v", 
                     "x4", "y4", "x4_v", "y4_v")