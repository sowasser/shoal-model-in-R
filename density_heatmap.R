# Script working towards creating animated heatmaps of density to better 
# compare with acoustic data. The position of each agent at each step was
# recorded from the model, then exported in a simplified format to actually
# create the heatmap in R using gganimate.

library(reshape2)

path <- "~/Desktop/DO NOT ERASE/1NUIG/Mackerel/Mackerel Data/"  # for laptop

x_coord <- read.csv(paste0(path, "heatmap_x.csv"))
y_coord <- read.csv(paste0(path, "heatmap_y.csv"))

step <- (1:10)  # Create list of number of steps


# Remove first column (index of the pandas dataframe) & add step column
x_coord <- cbind(step, x_coord[, -1])
y_coord <- cbind(step, y_coord[, -1])

# Reshape data from wide to long, with all x & y data in 1 column
new_x <- melt(x_coord, id.vars = "step")
new_y <- melt(y_coord, id.vars = "step")

# Combine all data together & rename columns again
pos_data <- cbind(new_x, new_y[, 3])
colnames(pos_data) <- c("step", "fish", "x", "y")
