# Script working towards creating animated heatmaps of density to better 
# compare with acoustic data. The position of each agent at each step was
# recorded from the model, then exported in a simplified format to actually
# create the heatmap in R using gganimate.

library(reshape2)
library(ggplot2)
library(transformr)
library(gganimate)
library(gifski)
library(viridis)

path <- "~/Desktop/DO NOT ERASE/1NUIG/Mackerel/Mackerel Data/"  # for laptop

x_coord <- read.csv(paste0(path, "heatmap_x.csv"))
y_coord <- read.csv(paste0(path, "heatmap_y.csv"))

step <- c(1:200)  # Create list of number of steps


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


# Create graph of a few indicative steps (every 50?)
step1 <- pos_data[which(pos_data$step==1), ]
step50 <- pos_data[which(pos_data$step==50), ]
step100 <- pos_data[which(pos_data$step==100), ]
step150 <- pos_data[which(pos_data$step==150), ]
step200 <- pos_data[which(pos_data$step==200), ]
step250 <- pos_data[which(pos_data$step==250), ]
step300 <- pos_data[which(pos_data$step==300), ]
step350 <- pos_data[which(pos_data$step==350), ]
step400 <- pos_data[which(pos_data$step==400), ]

pos_data_graph_subset <- rbind(step1, step50, step100, step150, step200,
                               step250, step300, step350, step400)

density_plot <- ggplot(pos_data_graph_subset, aes(x = x, y = y)) +
  stat_density2d(aes(fill=..level..), geom="polygon") +
  scale_fill_viridis("Density", discrete = FALSE) +
  # geom_point(colour="black", size = 0.01) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab(" ") +
  ylab(" ") +
  facet_wrap(~step, scale="free")

ggsave(filename="~/Desktop/density.pdf", plot=density_plot,
       width=180, height=150, units="mm", dpi=300)


# Select trial data - last 50 steps of the model
pos_data_subset <- pos_data[15001:20000, ]

# Plot positions as a density heatmap & save
density <- ggplot(pos_data_subset, aes(x = x, y = y, group = step)) +
  stat_density2d(aes(fill=..level..), geom="polygon") +
  scale_fill_viridis("Density", discrete = FALSE) +
  geom_point(colour="black") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab(" ") +
  ylab(" ") +
  
  # gganimate stuff below 
  transition_states(step, transition_length = 3, state_length = 1) +
  labs(title = "Step: {closest_state}")

animate(density, duration = 5, fps = 20, width = 200, height = 200, renderer = gifski_renderer())
anim_save("density.gif", animation = density, path = "~/Desktop/")
