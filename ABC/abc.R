# Approximate Bayesian Computation using the 'abc' package

# 'abc' requires:
#   1. a vector of observed summary statistics
#   2. a matrix of the simulated summary statistics, where each row corresponds
#      to a simulation and each column corresponds to a summary statistic
#   3. a matrix of the simulated parameter values, where each row corrresponds
#      to a model run.

# More documentation about abc:
# https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/j.2041-210X.2011.00179.x
# https://cran.r-project.org/web/packages/abc/abc.pdf

# TODO: Make sure to run ichec_import.R to get the data ready!


library(abc)
library(ggplot2)
library(gridExtra)

path <- "~/Desktop/DO NOT ERASE/1NUIG/Mackerel/Mackerel Data/"  # for laptop
# path <- "~/Desktop/Local/Mackerel/Mackerel Data/"  # for desktop

# Read in local Python data ---------------------------------------------------

# Mean of all runs, calculated for every step of the model & changes in a variable
# speed <- read.csv(paste0(path,"var-speed.csv"))
# vision <- read.csv(paste0(path,"var-vision.csv"))
# separation <- read.csv(paste0(path,"var-sep.csv"))

# model <- rbind(speed, vision, separation)
# colnames(model) <- c("polar", "nnd", "area", "centroid", "speed", "vision", "separation")


# Read in ICHEC data ----------------------------------------------------------

# Mean of all runs, calculated for every step of the model & changes in a variable
model <- read.csv(paste0(path,"ICHEC_data_30Mar20.csv"))  # TODO: change this date
colnames(model) <- c("polar", "nnd", "area", "centroid", "speed", "vision", "separation")


# Data from video tracking ----------------------------------------------------
tracking <- read.csv(paste0(path, "stepwise_data.csv"))
colnames(tracking) <- c("step", "centroid", "nnd", "area", "polar")


# Check summary statistics 


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


# Adjust data inputs and run ABC ----------------------------------------------
# matrix of observed summary statistics, in same order as from the model:
# polar, nnd, area, centroid
tracking_means <- t(colMeans(tracking, na.rm = FALSE, dims = 1))
real_fish <- tracking_means[, c(4, 3, 2, 1)]

# matrix of simulated parameter values, where each row corresponds to a
# simulation and each column correponds to a parameter.
model_params <- model[, 5:7]

# matrix of simulated summary statistics, where each row corresponds to  a 
# simulation and each column corresponds to a summary statistic.
model_stats <- model[, 1:4]


# Use 'abc' to accept top 1% of runs as approximate posteriors
shoaling.abc <- abc(target = real_fish,   # observed summary statistics
                    param = model_params,  # simulated parameter values, i.e. dependent variable(s)
                    sumstat = model_stats,  # simulated summary statistics / independent variables
                    tol = 0.1, method = "rejection")  # proportion of runs to accept; type of ABC to use

summary(shoaling.abc)

# then plot posterior distribution

# Better summary statistics ---------------------------------------------------

create.fish.sumstats <- function(fish.stats) {
  if (is.vector(fish.stats)) {
    better.sumstats <- c(min(fish.stats[1:60]), max(fish.stats[1:60]),
                         mean(fish.stats[1:60]), sd(fish.stats[1:60]),
                         min(fish.stats[61:120]), max(fish.stats[61:120]),
                         mean(fish.stats[61:120]), sd(fish.stats[61:120]))
    names(better.sumstats) <- c(paste0(c("min.", "max.", "mean.", "sd."),
                                       "step.dist"), paste0(c("min.", "max.", "mean.", "sd."), "total.disp"))
  } else {
    better.sumstats <- cbind(apply(fish.stats[, 1:60], 1, min),
                             apply(fish.stats[, 1:60], 1, max),
                             apply(fish.stats[, 1:60], 1, mean),
                             apply(fish.stats[, 1:60], 1, sd),
                             apply(fish.stats[, 61:120], 1, min),
                             apply(fish.stats[, 61:120], 1, max),
                             apply(fish.stats[, 61:120], 1, mean),
                             apply(fish.stats[, 61:120], 1, sd))
    colnames(better.sumstats) <- c(paste0(c("min.", "max.", "mean.", "sd."),
                                          "step.dist"), paste0(c("min.", "max.", "mean.", "sd."), "total.disp"))
  }
  better.sumstats
}

fish.data.ss <- create.fish.sumstats()
