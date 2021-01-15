# Script for weighted densities because ggtern affects general ggplot function.
# The .csv files generated here are then imported into tilt_angle.R to create 
# the plots.

# TODO: Quit R and clear session after running this.
library(reshape2)
library(MASS)
library(ggtern)
library(Peacock.test)

path <- "~/Desktop/DO NOT ERASE/1NUIG/Mackerel/Mackerel Data/"  # for laptop

step <- c(1:400)  # Create list of number of steps

# Read in & manage data for 300 agents ----------------------------------------
# Read in position data
x_coord_300 <- read.csv(paste0(path, "heatmap_x_300_3.csv"))
y_coord_300 <- read.csv(paste0(path, "heatmap_y_300_3.csv"))

# Remove first column (index of the pandas dataframe) & add step column
x_coord_300 <- cbind(step, x_coord_300[, -1])
y_coord_300 <- cbind(step, y_coord_300[, -1])

# Reshape data from wide to long, with all x & y data in 1 column
new_x_300 <- melt(x_coord_300, id.vars = "step")
new_y_300 <- melt(y_coord_300, id.vars = "step")

# Read in heading data
heading <- read.csv(paste0(path, "headings_300.csv"))

# Add step column & reshape from wide to long to match position data
heading <- cbind(step, heading[, -1])
new_heading <- melt(heading, id.vars = "step")

# Combine all data together, rename columns, sort by step & fish
pos_head <- cbind(new_x_300, new_y_300[, 3], new_heading[, 3])
colnames(pos_head) <- c("step", "fish", "x", "y", "angle")
pos_head <- pos_head[order(pos_head$step, pos_head$fish), ]


# Calculate density and weighted density to show difference in a graph --------
step250 <- pos_head[which(pos_head$step==250), ]  # select a step
density <- kde2d(x=step250$x, y=step250$y) # calculate density

# Expand into dataframe & add column describing weighting
density_df <- data.frame(expand.grid(x=density$x, y=density$y), 
                         z=as.vector(density$z))
density_df <- cbind(density_df, rep("(A) no weighting", length(density_df$x)))
colnames(density_df) <- c("x", "y", "z", "weighting")

# Calculate weighted density, expand, and add column describing weighting
density_wt <- kde2d.weighted(x=step250$x, y=step250$y, w=step250$angle)
density_wt_df <- data.frame(expand.grid(x=density_wt$x, y=density_wt$y), 
                            z=as.vector(density_wt$z))
density_wt_df <- cbind(density_wt_df, rep("(B) weighted by tilt angle", length(density_wt_df$x)))
colnames(density_wt_df) <- c("x", "y", "z", "weighting")

# Combine baseline & weighted data into one dataframe & plot
density_all <- rbind(density_df, density_wt_df)

write.csv(density_all, paste0(path, "density_step250.csv"))

# 2D Kolmogorov-Smirnov test of difference ------------------------------------
peacock2(density$z, density_wt$z)  # p=0.08

# Compare z-values from the non-weighted & weighted densities -----------------
selected_steps <- c("50", "100", "150", "200", "250", "300")
# No obstructions
s50 <- pos_head[which(pos_head$step==50), ]
s100 <- pos_head[which(pos_head$step==100), ]
s150 <- pos_head[which(pos_head$step==150), ]  
s200 <- pos_head[which(pos_head$step==200), ]  
s250 <- pos_head[which(pos_head$step==250), ]  
s300 <- pos_head[which(pos_head$step==300), ] 

d50 <- MASS::kde2d(x=s50$x, y=s50$y)  # calculate non-weighted density
d100 <- MASS::kde2d(x=s100$x, y=s100$y)
d150 <- MASS::kde2d(x=s150$x, y=s150$y) 
d200 <- MASS::kde2d(x=s200$x, y=s200$y) 
d250 <- MASS::kde2d(x=s250$x, y=s250$y) 
d300 <- MASS::kde2d(x=s300$x, y=s300$y) 

dwt50 <- kde2d.weighted(x=s50$x, y=s50$y, w=s50$angle)  # calculate weighted density
dwt100 <- kde2d.weighted(x=s100$x, y=s100$y, w=s100$angle)
dwt150 <- kde2d.weighted(x=s150$x, y=s150$y, w=s150$angle) 
dwt200 <- kde2d.weighted(x=s200$x, y=s200$y, w=s200$angle) 
dwt250 <- kde2d.weighted(x=s250$x, y=s250$y, w=s250$angle) 
dwt300 <- kde2d.weighted(x=s300$x, y=s300$y, w=s300$angle) 

# Combine mean densities into one dataframe
dens <- c(mean(d50$z), mean(d100$z), mean(d150$z), mean(d200$z), mean(d250$z), mean(d300$z))
dens2 <- cbind(selected_steps, dens, rep("not weighted", length(dens)))
denswt <- c(mean(dwt50$z), mean(dwt100$z), mean(dwt150$z), mean(dwt200$z), mean(dwt250$z), mean(dwt300$z))
dens2wt <- cbind(selected_steps, denswt, rep("weighted", length(denswt)))
dens_all <- data.frame(rbind(dens2, dens2wt))
dens_all2 <- cbind(dens_all, rep("no obstruction", length(dens_all$V3)))
colnames(dens_all2) <- c("step", "density", "weighting", "model")

# Thermocline
# Add step column & reshape from wide to long to match position data
heading_c <- read.csv(paste0(path, "headings_cline.csv"))
heading_c <- cbind(step, heading_c[, -1])
new_heading_c <- melt(heading_c, id.vars = "step")

# Combine all data together, rename columns, sort by step & fish
x_coord_c <- read.csv(paste0(path, "heatmap_x_cline.csv"))
y_coord_c <- read.csv(paste0(path, "heatmap_y_cline.csv"))

# Remove first column (index of the pandas dataframe) & add step column
x_coord_c <- cbind(step, x_coord_c[, -1])
y_coord_c <- cbind(step, y_coord_c[, -1])

# Reshape data from wide to long, with all x & y data in 1 column
new_x_c <- melt(x_coord_c, id.vars = "step")
new_y_c <- melt(y_coord_c, id.vars = "step")
pos_head_c <- cbind(new_x_c, new_y_c[, 3], new_heading_c[, 3])
colnames(pos_head_c) <- c("step", "fish", "x", "y", "angle")

# Select steps and calculate densities
s50_c <- pos_head_c[which(pos_head_c$step==50), ]
s100_c <- pos_head_c[which(pos_head_c$step==100), ]
s150_c <- pos_head_c[which(pos_head_c$step==150), ]  
s200_c <- pos_head_c[which(pos_head_c$step==200), ]  
s250_c <- pos_head_c[which(pos_head_c$step==250), ]  
s300_c <- pos_head_c[which(pos_head_c$step==300), ] 

d50_c <- MASS::kde2d(x=s50_c$x, y=s50_c$y)
d100_c <- MASS::kde2d(x=s100_c$x, y=s100_c$y)
d150_c <- MASS::kde2d(x=s150_c$x, y=s150_c$y) 
d200_c <- MASS::kde2d(x=s200_c$x, y=s200_c$y) 
d250_c <- MASS::kde2d(x=s250_c$x, y=s250_c$y) 
d300_c <- MASS::kde2d(x=s300_c$x, y=s300_c$y) 

dwt50_c <- kde2d.weighted(x=s50_c$x, y=s50_c$y, w=s50_c$angle) 
dwt100_c <- kde2d.weighted(x=s100_c$x, y=s100_c$y, w=s100_c$angle) 
dwt150_c <- kde2d.weighted(x=s150_c$x, y=s150_c$y, w=s150_c$angle) 
dwt200_c <- kde2d.weighted(x=s200_c$x, y=s200_c$y, w=s200_c$angle) 
dwt250_c <- kde2d.weighted(x=s250_c$x, y=s250_c$y, w=s250_c$angle) 
dwt300_c <- kde2d.weighted(x=s300_c$x, y=s300_c$y, w=s300_c$angle) 

# Combine mean densities into one dataframe
dens_c <- c(mean(d50_c$z), mean(d100_c$z), mean(d150_c$z), mean(d200_c$z), mean(d250_c$z), mean(d300_c$z))
dens2_c <- cbind(selected_steps, dens_c, rep("not weighted", length(dens_c)))
denswt_c <- c(mean(d50_c$z), mean(dwt100_c$z), mean(dwt150_c$z), mean(dwt200_c$z), mean(dwt250_c$z), mean(dwt300_c$z))
dens2wt_c <- cbind(selected_steps, denswt_c, rep("weighted", length(denswt_c)))
dens_all_c <- data.frame(rbind(dens2_c, dens2wt_c))
dens_all2_c <- cbind(dens_all_c, rep("thermocline", length(dens_all_c$V3)))
colnames(dens_all2_c) <- c("step", "density", "weighting", "model")

# Sloped bottom
# Add step column & reshape from wide to long to match position data
heading_slope <- read.csv(paste0(path, "headings_slope2.csv"))
heading_slope <- cbind(step, heading_slope[, -1])
new_heading_slope <- melt(heading_slope, id.vars = "step")

x_coord_slope <- read.csv(paste0(path, "heatmap_x_slope2.csv"))
y_coord_slope <- read.csv(paste0(path, "heatmap_y_slope2.csv"))

# Remove first column (index of the pandas dataframe) & add step column
x_coord_slope <- cbind(step, x_coord_slope[, -1])
y_coord_slope <- cbind(step, y_coord_slope[, -1])

# Reshape data from wide to long, with all x & y data in 1 column
new_x_slope <- melt(x_coord_slope, id.vars = "step")
new_y_slope <- melt(y_coord_slope, id.vars = "step")

# Combine all data together, rename columns, sort by step & fish
pos_head_slope <- cbind(new_x_slope, new_y_slope[, 3], new_heading_slope[, 3])
colnames(pos_head_slope) <- c("step", "fish", "x", "y", "angle")

# Select steps and calculate densities
s50_slope <- pos_head_slope[which(pos_head_slope$step==50), ]
s100_slope <- pos_head_slope[which(pos_head_slope$step==100), ]
s150_slope <- pos_head_slope[which(pos_head_slope$step==150), ]  
s200_slope <- pos_head_slope[which(pos_head_slope$step==200), ]  
s250_slope <- pos_head_slope[which(pos_head_slope$step==250), ]  
s300_slope <- pos_head_slope[which(pos_head_slope$step==300), ] 

d50_slope <- MASS::kde2d(x=s50_slope$x, y=s50_slope$y)
d100_slope <- MASS::kde2d(x=s100_slope$x, y=s100_slope$y)
d150_slope <- MASS::kde2d(x=s150_slope$x, y=s150_slope$y) 
d200_slope <- MASS::kde2d(x=s200_slope$x, y=s200_slope$y) 
d250_slope <- MASS::kde2d(x=s250_slope$x, y=s250_slope$y) 
d300_slope <- MASS::kde2d(x=s300_slope$x, y=s300_slope$y) 

dwt50_slope <- kde2d.weighted(x=s50_slope$x, y=s50_slope$y, w=s50_slope$angle) 
dwt100_slope <- kde2d.weighted(x=s100_slope$x, y=s100_slope$y, w=s100_slope$angle) 
dwt150_slope <- kde2d.weighted(x=s150_slope$x, y=s150_slope$y, w=s150_slope$angle) 
dwt200_slope <- kde2d.weighted(x=s200_slope$x, y=s200_slope$y, w=s200_slope$angle) 
dwt250_slope <- kde2d.weighted(x=s250_slope$x, y=s250_slope$y, w=s250_slope$angle) 
dwt300_slope <- kde2d.weighted(x=s300_slope$x, y=s300_slope$y, w=s300_slope$angle) 

# Combine mean densities into one dataframe
dens_slope <- c(mean(d50_slope$z), mean(d100_slope$z), mean(d150_slope$z), mean(d200_slope$z), mean(d250_slope$z), mean(d300_slope$z))
dens2_slope <- cbind(selected_steps, dens_slope, rep("not weighted", length(dens_slope)))
denswt_slope <- c(mean(d50_slope$z), mean(dwt100_slope$z), mean(dwt150_slope$z), mean(dwt200_slope$z), mean(dwt250_slope$z), mean(dwt300_slope$z))
dens2wt_slope <- cbind(selected_steps, denswt_slope, rep("weighted", length(denswt_slope)))
dens_all_slope <- data.frame(rbind(dens2_slope, dens2wt_slope))
dens_all2_slope <- cbind(dens_all_slope, rep("sloped bottom", length(dens_all_slope$V3)))
colnames(dens_all2_slope) <- c("step", "density", "weighting", "model") 

# Combine data again and export
wt_den_steps <- rbind(dens_all2, dens_all2_c, dens_all2_slope)
write.csv(wt_den_steps, paste0(path, "weighted_densities.csv"))
