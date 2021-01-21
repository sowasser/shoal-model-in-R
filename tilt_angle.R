# Script for visualizing the effects of heading/tilt angle on density. Density
# calculations are performed in weighted_densities.R and then imported here.

library(reshape2)
library(viridis)
library(ggplot2)

custom <- c("white", "#FDE725", "#B4DE2C", "#6DCD59", "#35B779", "#1F9E89",
            "#30688E", "#3D4A89", "#482879", "#440D54")
path <- "~/Desktop/DO NOT ERASE/1NUIG/Mackerel/Mackerel Data/"  # for laptop

step <- c(1:400)  # Create list of number of steps

# Read in & manage data -------------------------------------------------------
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

# THERMOCLINE
# Read in position data 
x_coord_c <- read.csv(paste0(path, "heatmap_x_cline.csv"))
y_coord_c <- read.csv(paste0(path, "heatmap_y_cline.csv"))
# Remove first column (index of the pandas dataframe) & add step column
x_coord_c <- cbind(step, x_coord_c[, -1])
y_coord_c <- cbind(step, y_coord_c[, -1])
# Reshape data from wide to long, with all x & y data in 1 column
new_x_c <- melt(x_coord_c, id.vars = "step")
new_y_c <- melt(y_coord_c, id.vars = "step")
# Read in heading data
heading_c <- read.csv(paste0(path, "headings_cline.csv"))
heading_c <- cbind(step, heading_c[, -1])
new_heading_c <- melt(heading_c, id.vars = "step")
# Combine all data together, rename columns, sort by step & fish
pos_head_c <- cbind(new_x_c, new_y_c[, 3], new_heading_c[, 3])
colnames(pos_head_c) <- c("step", "fish", "x", "y", "angle")

# SLOPE
# Position data
x_coord_slope <- read.csv(paste0(path, "heatmap_x_slope2.csv"))
y_coord_slope <- read.csv(paste0(path, "heatmap_y_slope2.csv"))
# Remove first column (index of the pandas dataframe) & add step column
x_coord_slope <- cbind(step, x_coord_slope[, -1])
y_coord_slope <- cbind(step, y_coord_slope[, -1])
# Reshape data from wide to long, with all x & y data in 1 column
new_x_slope <- melt(x_coord_slope, id.vars = "step")
new_y_slope <- melt(y_coord_slope, id.vars = "step")
# Read in heading data
heading_slope <- read.csv(paste0(path, "headings_slope2.csv"))
heading_slope <- cbind(step, heading_slope[, -1])
new_heading_slope <- melt(heading_slope, id.vars = "step")
# Combine all data together, rename columns, sort by step & fish
pos_head_slope <- cbind(new_x_slope, new_y_slope[, 3], new_heading_slope[, 3])
colnames(pos_head_slope) <- c("step", "fish", "x", "y", "angle")


# Graph heading/angle for selected steps to show how angle changes ------------
# Subset, find sum of heading for each step & make new dataframe

# Normal model
steps <- subset(pos_head, step<301)
step_means <- tapply(steps$angle, steps$step, mean)
step_subset <- c(1:300)
step_means_df <- data.frame(cbind(step_subset, step_means))
means <- cbind(step_means_df, rep("no obstruction", 
                                length(step_means_df$step_subset)))
colnames(means) <- c("step", "mean", "model")

# Model with thermocline
steps_c <- subset(pos_head_c, step<301)
step_means_c <- tapply(steps_c$angle, steps_c$step, mean)
step_subset_c <- c(1:300)
step_means_df_c <- data.frame(cbind(step_subset_c, step_means_c))
means_c <- cbind(step_means_df_c, rep("thermocline", 
                                    length(step_means_df_c$step_subset)))
colnames(means_c) <- c("step", "mean", "model")

# Model with slope
steps_slope <- subset(pos_head_slope, step<301)
step_means_slope <- tapply(steps_slope$angle, steps_slope$step, mean)
step_subset_slope <- c(1:300)
step_means_df_slope <- data.frame(cbind(step_subset_slope, step_means_slope))
means_s <- cbind(step_means_df_slope, rep("sloped bottom", 
                                        length(step_means_df_slope$step_subset)))
colnames(means_s) <- c("step", "mean", "model")

# Combine all model angle means, mins, and maxes
all_means <- rbind(means, means_c, means_s)

all_mins <- rbind(tapply(steps$angle, steps$step, min), 
                  tapply(steps_c$angle, steps_c$step, min),
                  tapply(steps_slope$angle, steps_slope$step, min))
min(all_mins)

all_maxs <- rbind(tapply(steps$angle, steps$step, max), 
                  tapply(steps_c$angle, steps_c$step, max),
                  tapply(steps_slope$angle, steps_slope$step, max))
max(all_maxs)

# Test to see if the models are statistically different - 
kruskal.test(mean ~ model, data=all_means)
pairwise.wilcox.test(all_means$mean, all_means$model, p.adjust.method = "holm")

# Linegraph of mean of heading across the steps selected
angles_graph <- ggplot(all_means, aes(x = step, y = mean)) + 
  geom_line(color = "#0d0887", size = 0.8) +  # color to match plasma 
  ylab("mean of headings/angles (radians)") + xlab("step") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~model)

ggsave(filename="~/Desktop/angles_timeseries.pdf", plot=angles_graph,
       width=200, height=80, units="mm", dpi=300)

# Do the same with standard deviation
step_stdev <- tapply(steps$angle, steps$step, sd)
step_stdev_df <- data.frame(cbind(step_subset, step_stdev))
stdev <- cbind(step_stdev_df, rep("no obstruction", 
                                  length(step_stdev_df$step_subset)))
colnames(stdev) <- c("step", "stdev", "model")

step_stdev_c <- tapply(steps_c$angle, steps_c$step, sd)
step_stdev_df_c <- data.frame(cbind(step_subset_c, step_stdev_c))
stdev_c <- cbind(step_stdev_df_c, rep("thermocline", 
                                      length(step_stdev_df_c$step_subset)))
colnames(stdev_c) <- c("step", "stdev", "model")

step_stdev_slope <- tapply(steps_slope$angle, steps_slope$step, sd)
step_stdev_df_slope <- data.frame(cbind(step_subset_slope, step_stdev_slope))
stdev_slope <- cbind(step_stdev_df_slope, rep("sloped bottom", 
                                              length(step_stdev_df_slope$step_subset)))
colnames(stdev_slope) <- c("step", "stdev", "model")

all_stdev <- rbind(stdev, stdev_c, stdev_slope)

stdev_graph <- ggplot(all_stdev, aes(x = step, y = stdev)) + 
  geom_line(color = "#7302a8", size = 0.8) +  # color to match plasma
  ylab("SD of headings/angles (radians)") + xlab("step") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~model)

ggsave(filename="~/Desktop/angles_stdev.pdf", plot=stdev_graph,
       width=200, height=80, units="mm", dpi=300)


# Calculate density and weighted density & show difference in a graph ---------
# Combine baseline & weighted data into one dataframe & plot
density_step250 <- read.csv(paste0(path, "density_step250.csv"))

density_weighted <- ggplot(density_step250, aes(x = x, y = y, z = z)) + 
  stat_contour(geom="polygon", aes(fill = ..level..), bins=30) + 
  scale_fill_viridis("Density", discrete = FALSE, option="plasma") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~weighting)

ggsave(filename="~/Desktop/density_weighted.pdf", plot=density_weighted,
       width=180, height=80, units="mm", dpi=300)


# Non-weighted and weighted densities for steps across models -----------------
wt_den <- read.csv(paste0(path, "weighted_densities.csv"))

# Make steps a factor so all will show up
wt_den$step <- as.factor(wt_den$step)

wt_den_plot <- ggplot(wt_den, aes(fill = weighting, y = density, x = step)) +
  geom_bar(position = "dodge", stat = "identity") +
  ylab("density") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  guides(fill=guide_legend(title=" ")) +
  scale_fill_manual(values = c("#0d0887", "#7302a8")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~model, scales = "free") +
  theme(legend.position = "bottom")

ggsave(filename="~/Desktop/wt_den.pdf", wt_den_plot,
       width=200, height=80, units="mm", dpi=300)
