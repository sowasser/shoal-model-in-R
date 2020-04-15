# Code for plots to check prior & posterior distributions for the abc model.

library(ggplot2)
library(gridExtra)

# Tracking Graphs -------------------------------------------------------------
require(gridExtra)
track_cent <- ggplot(data = tracking, aes(x=step, y=centroid)) +
  theme_classic() + geom_point() + geom_line()
track_nnd <- ggplot(data = tracking, aes(x=step, y=nnd)) +
  theme_classic() + geom_point() + geom_line()
track_area <- ggplot(data = tracking, aes(x=step, y=area)) +
  theme_classic() + geom_point() + geom_line()
track_polar <- ggplot(data = tracking, aes(x=step, y=polar)) +
  theme_classic() + geom_point() + geom_line()

# Model Graphs ----------------------------------------------------------------
require(gridExtra)
# set "x" and "group" to the parameter you're looking at
# set "y" to the summary statistic you're looking at
# TODO: change this to something that works better - maybe facets?
speed_polar <- ggplot(data = model, aes(x=speed, y=polar)) + 
  theme_classic() + geom_point()
speed_nnd <- ggplot(data = model, aes(x=speed, y=nnd)) + 
  theme_classic() + geom_point()
speed_area <- ggplot(data = model, aes(x=speed, y=area)) + 
  theme_classic() + geom_point()
speed_cent <- ggplot(data = model, aes(x=speed, y=centroid)) + 
  theme_classic() + geom_point()

vision_polar <- ggplot(data = model, aes(x=vision, y=polar)) + 
  theme_classic() + geom_point()
vision_nnd <- ggplot(data = model, aes(x=vision, y=nnd)) + 
  theme_classic() + geom_point()
vision_area <- ggplot(data = model, aes(x=vision, y=area)) + 
  theme_classic() + geom_point()
vision_cent <- ggplot(data = model, aes(x=vision, y=centroid)) + 
  theme_classic() + geom_point()

sep_polar <- ggplot(data = model, aes(x=separation, y=polar)) + 
  theme_classic() + geom_point()
sep_nnd <- ggplot(data = model, aes(x=separation, y=nnd)) + 
  theme_classic() + geom_point()
sep_area <- ggplot(data = model, aes(x=separation, y=area)) + 
  theme_classic() + geom_point()
sep_cent <- ggplot(data = model, aes(x=separation, y=centroid)) + 
  theme_classic() + geom_point()

# Create multiple graphs in page, row by row ----------------------------------
grid.arrange(speed_polar, vision_polar, sep_polar, track_polar,
             speed_nnd, vision_nnd, sep_nnd, track_nnd,
             speed_area, vision_area, sep_area, track_area,
             speed_cent, vision_cent, sep_cent, track_cent, ncol=4)


# Prior/posterior distributions from abc --------------------------------------
# Plan is boxplots (either grouped or multiple plots for the prior & posterior
# distributions of each statistic)

priors <- model_params
posteriors <- shoaling.abc$unadj.values
