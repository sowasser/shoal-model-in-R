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
# Can only calculate density for each step
step100 <- pos_head[which(pos_head$step==100), ]
step150 <- pos_head[which(pos_head$step==150), ]
step200 <- pos_head[which(pos_head$step==200), ]
step250 <- pos_head[which(pos_head$step==250), ]  
step300 <- pos_head[which(pos_head$step==300), ]

# Combine all 
all_angles <- c(sum(step100$angle), sum(step150$angle), sum(step200$angle), 
                sum(step250$angle), sum(step300$angle))
step_labels <- c("step 100", "step 150", "step 200", "step 250", "step 300")

all_angles_df <- data.frame(cbind(step_labels, all_angles))
colnames(all_angles_df) <- c("step", "sum of angles")

angles_graph <- ggplot(all_angles_df, aes(x = step_labels, y = all_angles)) +
  geom_bar(stat = "identity", fill = "#440D54") +
  scale_fill_viridis(discrete = TRUE) +
  ylab("sum of all headings/angles (radians)") + xlab(" ") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave(filename="~/Desktop/angles.pdf", plot=angles_graph,
       width=150, height=120, units="mm", dpi=300)


# Calculate density and weighted density & show difference in a graph ---------
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
