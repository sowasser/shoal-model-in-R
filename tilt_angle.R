# Script for calculating density relative to tilt angle by creating a scaling
# factor from the heading of each agent/fish for each step of a model run.

library(reshape2)
library(MASS)
library(viridis)
library(Peacock.test)
library(ggplot2)

# TODO: only load if running the weighted density calculation! Interferes with ggplot.
# library(ggtern)

custom <- c("white", "#FDE725", "#B4DE2C", "#6DCD59", "#35B779", "#1F9E89",
            "#30688E", "#3D4A89", "#482879", "#440D54")
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


# Graph heading/angle for selected steps to show weighting --------------------
# Subset, find sum of heading for each step & make new dataframe

# Normal model
steps <- subset(pos_head, step<301)
step_sums <- tapply(steps$angle, steps$step, sum)
step_subset <- c(1:300)
step_sums_df <- data.frame(cbind(step_subset, step_sums))
sums <- cbind(step_sums_df, rep("no obstruction", 
                                length(step_sums_df$step_subset)))
colnames(sums) <- c("step", "sum", "model")

# Model with thermocline
# Add step column & reshape from wide to long to match position data
heading_c <- read.csv(paste0(path, "headings_cline.csv"))
heading_c <- cbind(step, heading_c[, -1])
new_heading_c <- melt(heading_c, id.vars = "step")
# Combine all data together, rename columns, sort by step & fish
pos_head_c <- cbind(new_x_c, new_y_c[, 3], new_heading_c[, 3])
colnames(pos_head_c) <- c("step", "fish", "x", "y", "angle")
pos_head <- pos_head[order(pos_head$step, pos_head$fish), ]
# Subseet & get angle sum
steps_c <- subset(pos_head_c, step<301)
step_sums_c <- tapply(steps_c$angle, steps_c$step, sum)
step_subset_c <- c(1:300)
step_sums_df_c <- data.frame(cbind(step_subset_c, step_sums_c))
sums_c <- cbind(step_sums_df_c, rep("thermocline", 
                                    length(step_sums_df_c$step_subset)))
colnames(sums_c) <- c("step", "sum", "model")

# Model with slope
# Add step column & reshape from wide to long to match position data
heading_slope <- read.csv(paste0(path, "headings_slope.csv"))
heading_slope <- cbind(step, heading_slope[, -1])
new_heading_slope <- melt(heading_slope, id.vars = "step")
# Combine all data together, rename columns, sort by step & fish
pos_head_slope <- cbind(new_x_slope, new_y_slope[, 3], new_heading_slope[, 3])
colnames(pos_head_slope) <- c("step", "fish", "x", "y", "angle")
pos_head <- pos_head[order(pos_head$step, pos_head$fish), ]
# Subseet & get angle sum
steps_slope <- subset(pos_head_slope, step<301)
step_sums_slope <- tapply(steps_slope$angle, steps_slope$step, sum)
step_subset_slope <- c(1:300)
step_sums_df_slope <- data.frame(cbind(step_subset_slope, step_sums_slope))
sums_s <- cbind(step_sums_df_slope, rep("sloped bottom", 
                                        length(step_sums_df_slope$step_subset)))
colnames(sums_s) <- c("step", "sum", "model")

# Combine all model angle sums
all_sums <- rbind(sums, sums_c, sums_s)

# Linegraph of sum of heading across the steps selected
angles_graph <- ggplot(all_sums, aes(x = step, y = sum)) + 
  geom_line(color = "#440D54", size = 0.8) +
  ylab("sum of headings/angles (radians)") + xlab("step") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~model)

ggsave(filename="~/Desktop/angles_timeseries.pdf", plot=angles_graph,
       width=200, height=80, units="mm", dpi=300)


# Calculate density and weighted density & show difference in a graph ---------
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

density_weighted <- ggplot(density_all, aes(x = x, y = y, z = z)) + 
  stat_contour(geom="polygon", aes(fill = ..level..), bins=30) + 
  scale_fill_viridis("Density", discrete = FALSE) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~weighting)

ggsave(filename="~/Desktop/density_weighted.pdf", plot=density_weighted,
       width=180, height=80, units="mm", dpi=300)


# Compare z-values from the non-weighted & weighted densities -----------------
# Calculate density for step 250
peacock2(density$z, density_wt$z)  # p=0.08

# Calculate density for step 10
step10 <- pos_head[which(pos_head$step==10), ]  # select step
density_10 <- kde2d(x=step10$x, y=step10$y) # calculate density
density_wt_10 <- kde2d.weighted(x=step10$x, y=step10$y, w=step10$angle)

peacock2(density_10$z, density_wt_10$z)
