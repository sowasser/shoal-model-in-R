# Script working towards creating animated heatmaps of density to better 
# compare with acoustic data. The position of each agent at each step was
# recorded from the model, run with shoal_model_pos.py and 
# data_sensitivity_heatmap.py, then exported in a simplified format to actually
# create the heatmap in R using gganimate.

library(reshape2)
library(ggplot2)
library(transformr)
library(gganimate)
library(gifski)
library(viridis)

path <- "~/Desktop/DO NOT ERASE/1NUIG/Mackerel/Mackerel Data/"  # for laptop

# Density graphs and gif for 100 agents ---------------------------------------

x_coord <- read.csv(paste0(path, "heatmap_x.csv"))
y_coord <- read.csv(paste0(path, "heatmap_y.csv"))

step <- c(1:400)  # Create list of number of steps

# Remove first column (index of the pandas dataframe) & add step column
x_coord <- cbind(step, x_coord[, -1])
y_coord <- cbind(step, y_coord[, -1])

# Reshape data from wide to long, with all x & y data in 1 column
new_x <- melt(x_coord, id.vars = "step")
new_y <- melt(y_coord, id.vars = "step")

# Combine all data together, rename columns, sort by step & fish
pos_data <- cbind(new_x, new_y[, 3])
colnames(pos_data) <- c("step", "fish", "x", "y")
pos_data <- pos_data[order(pos_data$step, pos_data$fish), ]


# Select steps for density graph
step200 <- pos_data[which(pos_data$step==200), ]
step201 <- pos_data[which(pos_data$step==201), ]
step202 <- pos_data[which(pos_data$step==202), ]
step203 <- pos_data[which(pos_data$step==203), ]
step204 <- pos_data[which(pos_data$step==204), ]
step205 <- pos_data[which(pos_data$step==205), ]

pos_data_graph_subset <- rbind(step200, step201, step202, 
                               step203, step204, step205)

# Plot graph of densities across different steps
density_plot <- ggplot(pos_data_graph_subset, aes(x = x, y = y)) +
  stat_density2d(aes(fill=..level..), geom="polygon") +
  scale_fill_viridis("Density", discrete = FALSE) +
  geom_point(colour="black", size = 0.01, alpha = 0.3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) +
  xlab(" ") +
  ylab(" ") +
  ggtitle("IBM with 100 individuals") +
  facet_wrap(~step, scale="free")

ggsave(filename="~/Desktop/density.pdf", plot=density_plot,
       width=180, height=100, units="mm", dpi=300)


# Select trial data - last 50 steps of the model
pos_data_subset <- pos_data[15001:20000, ]

# Gif of positions & density
density <- ggplot(pos_data_subset, aes(x = x, y = y, group = step)) +
  stat_density2d(aes(fill=..level..), geom="polygon") +
  scale_fill_viridis("Density", discrete = FALSE) +
  geom_point(colour="black", alpha = 0.3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) +
  xlab(" ") +
  ylab(" ") +
  theme(legend.position = "none") +  # remove the legend
  
  # gganimate stuff below 
  transition_states(step, transition_length = 3, state_length = 1) +
  labs(title = "Step: {closest_state}")

animate(density, duration = 5, fps = 20, width = 200, height = 200, renderer = gifski_renderer())
anim_save("density.gif", animation = density, path = "~/Desktop/")


# With 300 agents -------------------------------------------------------------
x_coord_300 <- read.csv(paste0(path, "heatmap_x_300.csv"))
y_coord_300 <- read.csv(paste0(path, "heatmap_y_300.csv"))

# Remove first column (index of the pandas dataframe) & add step column
x_coord_300 <- cbind(step, x_coord_300[, -1])
y_coord_300 <- cbind(step, y_coord_300[, -1])

# Reshape data from wide to long, with all x & y data in 1 column
new_x_300 <- melt(x_coord_300, id.vars = "step")
new_y_300 <- melt(y_coord_300, id.vars = "step")

# Combine all data together, rename columns, sort by step & fish
pos_data_300 <- cbind(new_x_300, new_y_300[, 3])
colnames(pos_data_300) <- c("step", "fish", "x", "y")
pos_data_300 <- pos_data_300[order(pos_data_300$step, pos_data_300$fish), ]


# Select steps for density graph
step200_300 <- pos_data_300[which(pos_data_300$step==200), ]
step201_300 <- pos_data_300[which(pos_data_300$step==201), ]
step202_300 <- pos_data_300[which(pos_data_300$step==202), ]
step203_300 <- pos_data_300[which(pos_data_300$step==203), ]
step204_300 <- pos_data_300[which(pos_data_300$step==204), ]
step205_300 <- pos_data_300[which(pos_data_300$step==205), ]

pos_data_graph_subset_300 <- rbind(step200_300, step201_300, step202_300,
                                   step203_300, step204_300, step205_300)

# Plot graph of densities across different steps
density_plot_300 <- ggplot(pos_data_graph_subset_300, aes(x = x, y = y)) +
  stat_density2d(aes(fill=..level..), geom="polygon") +
  scale_fill_viridis("Density", discrete = FALSE) +
  geom_point(colour="black", size = 0.01, alpha = 0.3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) +
  xlab(" ") +
  ylab(" ") +
  ggtitle("IBM with 300 individuals") +
  facet_wrap(~step, scale="free")

ggsave(filename="~/Desktop/density_300.pdf", plot=density_plot_300,
       width=180, height=100, units="mm", dpi=300)


# Select trial data - last 50 steps of the model
pos_data_subset_300 <- pos_data_300[15001:20000, ]

# Gif of positions & density
density_300 <- ggplot(pos_data_subset_300, aes(x = x, y = y, group = step)) +
  stat_density2d(aes(fill=..level..), geom="polygon") +
  scale_fill_viridis("Density", discrete = FALSE) +
  geom_point(colour="black", alpha = 0.3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) +
  xlab(" ") +
  ylab(" ") +
  theme(legend.position = "none") +  # remove the legend
  
  # gganimate stuff below 
  transition_states(step, transition_length = 3, state_length = 1) +
  labs(title = "Step: {closest_state}")

animate(density_300, duration = 5, fps = 20, width = 200, height = 200, 
        renderer = gifski_renderer())
anim_save("density_300.gif", animation = density_300, path = "~/Desktop/")
