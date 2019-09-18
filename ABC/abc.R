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

# Read in data ----------------------------------------------------------------

# Mean of all runs, calculated for every step of the model & changes in a variable
speed <- read.csv(paste0(path,"var-speed.csv"))
vision <- read.csv(paste0(path,"var-vision.csv"))
separation <- read.csv(paste0(path,"var-sep.csv"))

model <- rbind(speed, vision, separation)
colnames(model) <- c("polar", "nnd", "area", "centroid", "speed", "vision", "separation")

# Data from video tracking
tracking <- read.csv(paste0(path, "stepwise_data.csv"))


# Check summary statistics ----------------------------------------------------
# set "x" and "group" to the parameter you're looking at
# set "y" to the summary statistic you're looking at
# Todo: change this to something that works better for a lognormal distribution.
# boxplot <- ggplot(data = model, aes(x=vision, y=polar, group=vision)) + 
#   theme_classic() +
#   geom_boxplot()
# boxplot


# Adjust data inputs and run ABC ----------------------------------------------
# matrix of observed summary statistics, in same order as from the model:
# polar, nnd, area, centroid
tracking_means <- t(colMeans(tracking, na.rm = FALSE, dims = 1))
target <- tracking_means[, c(4, 3, 2, 1)]

# matrix of simulated parameter values, where each row corresponds to a
# simulation and each column correponds to a parameter.
param <- model[, 5:7]

# matrix of simulated summary statistics, where each row corresponds to  a 
# simulation and each column corresponds to a summary statistic.
sumstat <- model[, 1:4]


# Use 'abc' to accept top 1% of runs as approximate posteriors
shoaling.abc <- abc(target,   # observed summary statistics
                    param,  # simulated parameter values, i.e. dependent variable(s)
                    sumstat,  # simulated summary statistics / independent variables
                    tol = 0.1, method = "loclinear")  # proportion of runs to accept; type of ABC to use

summary(shoaling.abc)

# then plot
