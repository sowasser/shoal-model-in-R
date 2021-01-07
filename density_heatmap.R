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
  scale_y_continuous(trans = "reverse") +  # reverse axis to match Mesa indexing
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) +
  xlab(" ") +
  ylab(" ") +
  facet_wrap(~step)

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
x_coord_300 <- read.csv(paste0(path, "heatmap_x_300_2.csv"))
y_coord_300 <- read.csv(paste0(path, "heatmap_y_300_2.csv"))

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
  scale_y_continuous(trans = "reverse") +  # reverse axis to match Mesa indexing
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) +
  xlab(" ") +
  ylab(" ") +
  facet_wrap(~step)

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


# With a thermocline ----------------------------------------------------------
x_coord_c <- read.csv(paste0(path, "heatmap_x_cline300.csv"))
y_coord_c <- read.csv(paste0(path, "heatmap_y_cline300.csv"))

# Remove first column (index of the pandas dataframe) & add step column
x_coord_c <- cbind(step, x_coord_c[, -1])
y_coord_c <- cbind(step, y_coord_c[, -1])

# Reshape data from wide to long, with all x & y data in 1 column
new_x_c <- melt(x_coord_c, id.vars = "step")
new_y_c <- melt(y_coord_c, id.vars = "step")

# Combine all data together, rename columns, sort by step & fish
pos_data_c <- cbind(new_x_c, new_y_c[, 3])
colnames(pos_data_c) <- c("step", "fish", "x", "y")
pos_data_c <- pos_data_c[order(pos_data_c$step, pos_data_c$fish), ]

# Select steps for density graph
step200_c <- pos_data_c[which(pos_data_c$step==200), ]
step201_c <- pos_data_c[which(pos_data_c$step==201), ]
step202_c <- pos_data_c[which(pos_data_c$step==202), ]
step203_c <- pos_data_c[which(pos_data_c$step==203), ]
step204_c <- pos_data_c[which(pos_data_c$step==204), ]
step205_c <- pos_data_c[which(pos_data_c$step==205), ]

pos_data_graph_subset_c <- rbind(step200_c, step201_c, step202_c,
                                   step203_c, step204_c, step205_c)

# Plot graph of densities across different steps
density_plot_c <- ggplot(pos_data_graph_subset_c, aes(x = x, y = y)) +
  stat_density2d(aes(fill=..level..), geom="polygon") +
  scale_fill_viridis("Density", discrete = FALSE) +
  geom_point(colour="black", size = 0.01, alpha = 0.3) +
  # scale_y_continuous(trans = "reverse") +  # reverse axis to match Mesa indexing
  geom_hline(yintercept=25, linetype = "dotted") +  # add line to represent thermo or halocline
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) +
  xlab(" ") +
  ylab(" ") +
  facet_wrap(~step)

ggsave(filename="~/Desktop/density_c.pdf", plot=density_plot_c,
       width=180, height=100, units="mm", dpi=300)


# With a sloped bottom --------------------------------------------------------
x_coord_slope <- read.csv(paste0(path, "heatmap_x_slope300.csv"))
y_coord_slope <- read.csv(paste0(path, "heatmap_y_slope300.csv"))

# Remove first column (index of the pandas dataframe) & add step column
x_coord_slope <- cbind(step, x_coord_slope[, -1])
y_coord_slope <- cbind(step, y_coord_slope[, -1])

# Reshape data from wide to long, with all x & y data in 1 column
new_x_slope <- melt(x_coord_slope, id.vars = "step")
new_y_slope <- melt(y_coord_slope, id.vars = "step")

# Combine all data together, rename columns, sort by step & fish
pos_data_slope <- cbind(new_x_slope, new_y_slope[, 3])
colnames(pos_data_slope) <- c("step", "fish", "x", "y")
pos_data_slope <- pos_data_slope[order(pos_data_slope$step, pos_data_slope$fish), ]

# Select steps for density graph
step200_slope <- pos_data_slope[which(pos_data_slope$step==200), ]
step201_slope <- pos_data_slope[which(pos_data_slope$step==201), ]
step202_slope <- pos_data_slope[which(pos_data_slope$step==202), ]
step203_slope <- pos_data_slope[which(pos_data_slope$step==203), ]
step204_slope <- pos_data_slope[which(pos_data_slope$step==204), ]
step205_slope <- pos_data_slope[which(pos_data_slope$step==205), ]

pos_data_graph_subset_slope <- rbind(step200_slope, step201_slope, step202_slope,
                                 step203_slope, step204_slope, step205_slope)

# Plot graph of densities across different steps
density_plot_slope <- ggplot(pos_data_graph_subset_slope, aes(x = x, y = y)) +
  stat_density2d(aes(fill=..level..), geom="polygon") +
  scale_fill_viridis("Density", discrete = FALSE) +
  geom_point(colour="black", size = 0.01, alpha = 0.3) +
  scale_y_continuous(trans = "reverse") +  # reverse axis to match Mesa indexing
  # geom_abline(intercept=1, slope=1.7) +  # add line to represent sloped bottom
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) + 
  xlab(" ") +
  ylab(" ") +
  facet_wrap(~step)

ggsave(filename="~/Desktop/density_slope.pdf", plot=density_plot_slope,
       width=180, height=100, units="mm", dpi=300)


# 300 agents, speed = 1 -------------------------------------------------------
x_coord_300s1 <- read.csv(paste0(path, "heatmap_x_300_speed1.csv"))
y_coord_300s1 <- read.csv(paste0(path, "heatmap_y_300_speed1.csv"))

# Remove first column (index of the pandas dataframe) & add step column
x_coord_300s1 <- cbind(step, x_coord_300s1[, -1])
y_coord_300s1 <- cbind(step, y_coord_300s1[, -1])

# Reshape data from wide to long, with all x & y data in 1 column
new_x_300s1 <- melt(x_coord_300s1, id.vars = "step")
new_y_300s1 <- melt(y_coord_300s1, id.vars = "step")

# Combine all data together, rename columns, sort by step & fish
pos_data_300s1 <- cbind(new_x_300s1, new_y_300s1[, 3])
colnames(pos_data_300s1) <- c("step", "fish", "x", "y")
pos_data_300s1 <- pos_data_300s1[order(pos_data_300s1$step, pos_data_300s1$fish), ]

# Select steps for density graph
step200_300s1 <- pos_data_300s1[which(pos_data_300s1$step==200), ]
step201_300s1 <- pos_data_300s1[which(pos_data_300s1$step==201), ]
step202_300s1 <- pos_data_300s1[which(pos_data_300s1$step==202), ]
step203_300s1 <- pos_data_300s1[which(pos_data_300s1$step==203), ]
step204_300s1 <- pos_data_300s1[which(pos_data_300s1$step==204), ]
step205_300s1 <- pos_data_300s1[which(pos_data_300s1$step==205), ]

pos_data_graph_subset_300s1 <- rbind(step200_300s1, step201_300s1, step202_300s1,
                                     step203_300s1, step204_300s1, step205_300s1)

# Plot graph of densities across different steps
density_plot_300s1 <- ggplot(pos_data_graph_subset_300s1, aes(x = x, y = y)) +
  stat_density2d(aes(fill=..level..), geom="polygon") +
  scale_fill_viridis("Density", discrete = FALSE) +
  geom_point(colour="black", size = 0.01, alpha = 0.3) +
  scale_y_continuous(trans = "reverse") +  # reverse axis to match Mesa indexing
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) + 
  xlab(" ") +
  ylab(" ") +
  facet_wrap(~step)

ggsave(filename="~/Desktop/density_300s1.pdf", plot=density_plot_300s1,
       width=180, height=100, units="mm", dpi=300)


# 300 agents, speed = 15 ------------------------------------------------------
x_coord_300s15 <- read.csv(paste0(path, "heatmap_x_300_speed15.csv"))
y_coord_300s15 <- read.csv(paste0(path, "heatmap_y_300_speed15.csv"))

# Remove first column (index of the pandas dataframe) & add step column
x_coord_300s15 <- cbind(step, x_coord_300s15[, -1])
y_coord_300s15 <- cbind(step, y_coord_300s15[, -1])

# Reshape data from wide to long, with all x & y data in 1 column
new_x_300s15 <- melt(x_coord_300s15, id.vars = "step")
new_y_300s15 <- melt(y_coord_300s15, id.vars = "step")

# Combine all data together, rename columns, sort by step & fish
pos_data_300s15 <- cbind(new_x_300s15, new_y_300s15[, 3])
colnames(pos_data_300s15) <- c("step", "fish", "x", "y")
pos_data_300s15 <- pos_data_300s15[order(pos_data_300s15$step, pos_data_300s15$fish), ]

# Select steps for density graph
step200_300s15 <- pos_data_300s15[which(pos_data_300s15$step==200), ]
step201_300s15 <- pos_data_300s15[which(pos_data_300s15$step==201), ]
step202_300s15 <- pos_data_300s15[which(pos_data_300s15$step==202), ]
step203_300s15 <- pos_data_300s15[which(pos_data_300s15$step==203), ]
step204_300s15 <- pos_data_300s15[which(pos_data_300s15$step==204), ]
step205_300s15 <- pos_data_300s15[which(pos_data_300s15$step==205), ]

pos_data_graph_subset_300s15 <- rbind(step200_300s15, step201_300s15, step202_300s15,
                                      step203_300s15, step204_300s15, step205_300s15)

# Plot graph of densities across different steps
density_plot_300s15 <- ggplot(pos_data_graph_subset_300s15, aes(x = x, y = y)) +
  stat_density2d(aes(fill=..level..), geom="polygon") +
  scale_fill_viridis("Density", discrete = FALSE) +
  geom_point(colour="black", size = 0.01, alpha = 0.3) +
  scale_y_continuous(trans = "reverse") +  # reverse axis to match Mesa indexing
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) + 
  xlab(" ") +
  ylab(" ") +
  facet_wrap(~step)

ggsave(filename="~/Desktop/density_300s15.pdf", plot=density_plot_300s15,
       width=180, height=100, units="mm", dpi=300)


# Combine all first steps -----------------------------------------------------

run.100 <- cbind(step200, rep("n = 100", length(step200$step)))
colnames(run.100) <- c("step", "fish", "x", "y", "run")
run.300 <- cbind(step200_300, rep("n = 300", length(step200_300$step)))
colnames(run.300) <- c("step", "fish", "x", "y", "run")
run.300.c <- cbind(step200_c, rep("n = 300 with 'thermocline'", length(step200_c$step))) 
colnames(run.300.c) <- c("step", "fish", "x", "y", "run")
run.300.slope <- cbind(step200_slope, rep("n = 300 with slope", length(step200_slope$step)))  
colnames(run.300.slope) <- c("step", "fish", "x", "y", "run")
run.300.s1 <- cbind(step200_300s1, rep("n = 300, speed = 1", length(step200_300s1$step)))
colnames(run.300.s1) <- c("step", "fish", "x", "y", "run")
run.300.s15 <- cbind(step200_300s15, rep("n = 300, speed = 15", length(step200_300s15$step)))
colnames(run.300.s15) <- c("step", "fish", "x", "y", "run")

all_runs <- rbind(run.100, run.300, run.300.c, run.300.slope, run.300.s1, run.300.s15)

density_plot_all <- ggplot(all_runs, aes(x = x, y = y)) +
  stat_density2d(aes(fill=..level..), geom="polygon") +
  scale_fill_viridis("Density", discrete = FALSE) +
  geom_point(colour="black", size = 0.01, alpha = 0.3) +
  scale_y_continuous(trans = "reverse") +  # reverse axis to match Mesa indexing
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) + 
  xlab(" ") +
  ylab(" ") +
  facet_wrap(~run)

ggsave(filename="~/Desktop/density_all.pdf", plot=density_plot_all,
       width=180, height=100, units="mm", dpi=300)
