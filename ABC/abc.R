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
model <- read.csv(paste0(path,"ICHEC_data_17Apr20.csv"))  # TODO: change this date


# Data from video tracking ----------------------------------------------------
tracking <- read.csv(paste0(path, "stepwise_data.csv"))
colnames(tracking) <- c("step", "cent", "nnd", "area", "polar")
tracking <- tracking[, c("cent", "nnd", "polar", "area")]  # reorder to match other data


# TODO: check summary statistics 

# Adjust data inputs and run ABC ----------------------------------------------
# matrix of observed summary statistics, in same order as from the model:
# polar, nnd, area, centroid
tmin <- t(apply(tracking, 2, min))
tmax <- t(apply(tracking, 2, max))
tmean <- t(colMeans(tracking, na.rm = FALSE, dims = 1))
tsd <- t(apply(tracking, 2, sd))

real_fish <- c(tmin[1], tmin[2], tmin[3], tmin[4], 
               tmax[1], tmax[2], tmax[3], tmax[4],
               tmean[2], tmean[2], tmean[3], tmean[4],
               tsd[1], tsd[2], tsd[3], tsd[4])


# matrix of simulated parameter values, where each row corresponds to a
# simulation and each column correponds to a parameter.
model_params <- model[, 18:20]

# matrix of simulated summary statistics, where each row corresponds to  a 
# simulation and each column corresponds to a summary statistic.
model_stats <- model[, 2:17]


# Use 'abc' to accept top 1% of runs as approximate posteriors
shoaling.abc <- abc(target = real_fish,   # observed summary statistics
                    param = model_params,  # simulated parameter values, i.e. dependent variable(s)
                    sumstat = model_stats,  # simulated summary statistics / independent variables
                    tol = 0.1, method = "rejection")  # proportion of runs to accept; type of ABC to use

summary(shoaling.abc)

# TODO: plot posterior distribution

