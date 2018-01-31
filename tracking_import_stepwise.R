# This R script is for working with the digitized videos from LoggerPro.
# While the other tracking import scripts are for working with a single fish
# tracked over each frame of a video, this script is for working with multiple
# fish positions tracked for select frames.

# LoggerPro exports a .csv file with x,y position for each individual. In this
# script, the data are imported and cleaned. Various statistics can then be run.

track <- read.csv("sticklebacks2_500x20.csv")
track <- track[,-1]  # remove first column
colnames(track) <- c("x1", "y1", "x2", "y2", "x3", "y3", "x4", "y4", "x5",
                     "y5", "x6", "y6", "x7", "y7", "x8", "y8", "x9", "y9",
                     "x10", "y10", "x11", "y11", "x12", "y12", "x13", "y13",
                     "x14", "y14")

# Separate into dataframes for each step and remove empty rows
s1 <- na.omit(track[,1:2]) 
s2 <- na.omit(track[,3:4])
s3 <- na.omit(track[,5:6])
s4 <- na.omit(track[,7:8])
s5 <- na.omit(track[,9:10])
s6 <- na.omit(track[,11:12])
s7 <- na.omit(track[,13:14])
s8 <- na.omit(track[,15:16])
s9 <- na.omit(track[,17:18])
s10 <- na.omit(track[,19:20])
s11 <- na.omit(track[,21:22])
s12 <- na.omit(track[,23:24])
s13 <- na.omit(track[,25:26])
s14 <- na.omit(track[,27:28])

# So the issue is that I didn't record the same number of fish for each step.
# Might have to process each step separately - not worth deleting observations!
