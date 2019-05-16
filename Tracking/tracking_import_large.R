# This R script is for working with the digitized videos from LoggerPro.
# Since some of these videos have dozens of fish, more automated, iterated
# data management and analysis is necessary. This script is for these more
# complicated datasets.

# LoggerPro exports a .csv file with x,y position for each individual and the
# x,y velocity for each. In the script, the data are imported and cleaned. 
# Various statistics can then be run on the data.

library(foreach)

track <- read.csv("sticklebacks1_300x.csv")

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

pos_x <- track[c(seq(from = 2, to = 77, by = 4))]  # extract x coordinates
pos_y <- track[c(seq(from = 3, to = 77, by = 4))]  # extract y coordinates
# now need to zip x & y together.
