# Approximate Bayesian Computation using the 'abc' package

# 'abc' requires:
#   1. a vector of observed summary statistics
#   2. a matrix of the simulated summary statistics, where each row corresponds
#      to a simulation and each column corresponds to a summary statistic
#   3. a matrix of the simulated parameter values, where each row corrresponds
#      to a parameter.

# More documentation about abc:
# https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/j.2041-210X.2011.00179.x
# https://cran.r-project.org/web/packages/abc/abc.pdf


library(abc)
library(ggplot2)

# path <- "~/Desktop/DO NOT ERASE/1NUIG/Mackerel/Mackerel Data/"  # for laptop
path <- "~/Desktop/Local/Mackerel/Mackerel Data/"  # for desktop


# Read in data & calculate means ----------------------------------------------

# Mean of all runs, calculated for every step of the model & changes in a variable
model_means <- read.csv(paste0(path,"means_var-speed.csv"))

# Calculate overall means for each data collector
# overall_means <- data.frame(colMeans(model_means))
polar_model_mean <- mean(model_means$polar)
nnd_model_mean <- mean(model_means$nnd)
area_model_mean <- mean(model_means$area)
cent_model_mean <- mean(model_means$centroid)

# Data from video tracking
tracking <- read.csv(paste0(path, "stepwise_data.csv"))

# Means from tracking data
polar_track_mean <- mean(tracking$polar)
nnd_track_mean <- mean(tracking$nnd)
area_track_mean <- mean(tracking$area)
cent_track_mean <- mean(tracking$centroid)


# Check summary statistics ----------------------------------------------------
# boxplots
polar_plot <- ggplot(data = model_means, aes(x=var, y=polar, group=var)) + 
  theme_classic() +
  geom_boxplot()
polar_plot

nnd_plot <- ggplot(data = model_means, aes(x=var, y=nnd, group=var)) + 
  theme_classic() +
  geom_boxplot()
nnd_plot

area_plot <- ggplot(data = model_means, aes(x=var, y=area, group=var)) + 
  theme_classic() +
  geom_boxplot()
area_plot

cent_plot <- ggplot(data = model_means, aes(x=var, y=centroid, group=var)) + 
  theme_classic() +
  geom_boxplot()
cent_plot

# Run ABC ---------------------------------------------------------------------
# import matrix of simulated summary statistics - from model
model_summary <- read.csv(paste0(path, "single_run.csv"))

# import matrix of simulated parameter values - from model
model_params <- read.csv(paste0(path, ""))

# Use 'abc' to accept top 1% of runs as approximate posteriors
shoaling.abc <- abc(target = ____,   # observed summary statistics
                    param = ____,  # simulated parameter values, i.e. dependent variable(s)
                    sumstat = ____,  # simulated summary statistics / independent variables
                    tol = 0.1, method = "rejection")  # proportion of runs to accept; type of ABC to use

summary(shoaling.abc)

# then plot
