# Script for calculating density relative to tilt angle by creating a scaling
# factor from the heading of each agent/fish for each step of a model run.

library(reshape2)
library(MASS)
library(ggtern)
library(viridis)

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


# Calculate density & scale with heading/tilt angle ---------------------------
# Can only calculate density for each step
step200 <- pos_head[which(pos_head$step==200), ]  # select step
density <- kde2d(x=step200$x, y=step200$y) # calculate density

# Expand into dataframe & add column describing weighting
density_df <- data.frame(expand.grid(x=density$x, y=density$y), 
                         z=as.vector(density$z))
density_df <- cbind(density_df, rep("no weighting", length(density_df$x)))
colnames(density_df) <- c("x", "y", "z", "weighting")

# Calculate weighted density, expand, and add column describing weighting
density_wt <- kde2d.weighted(x=step200$x, y=step200$y, w=step200$angle)
density_wt_df <- data.frame(expand.grid(x=density_wt$x, y=density_wt$y), 
                            z=as.vector(density_wt$z))
density_wt_df <- cbind(density_wt_df, rep("weighted by tilt angle", length(density_wt_df$x)))
colnames(density_wt_df) <- c("x", "y", "z", "weighting")

# Combine baseline & weighted data into one dataframe & plot
density_all <- rbind(density_df, density_wt_df)

density_weighted <- ggplot(density_all, aes(x = x, y = y, z = z)) + 
  stat_contour(geom="polygon", aes(fill = ..level..), bins=100) + 
  scale_fill_viridis("Density", discrete = FALSE) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~weighting)

ggsave(filename="~/Desktop/density_weighted.pdf", plot=density_weighted,
       width=180, height=100, units="mm", dpi=300)

