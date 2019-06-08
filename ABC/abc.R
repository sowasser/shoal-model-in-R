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

# path <- "~/Desktop/DO NOT ERASE/1NUIG/Mackerel/Mackerel Data/"  # for laptop
path <- "~/Desktop/Local/Mackerel/Mackerel Data/"  # for desktop


# Read in data & calculate means ----------------------------------------------

# Mean of all runs, calculated for every step of the model
run_means <- read.csv(paste0(path,"batch_means_runs.csv"))
# Mean of all steps, calculated for every run of the model
step_means <- read.csv(paste0(path,"batch_means_steps.csv"))

# Calculate overall means for each data collector
# overall_means <- data.frame(colMeans(run_means))
polar_model_mean <- mean(run_means$polar)
nnd_model_mean <- mean(run_means$nnd)
area_model_mean <- mean(run_means$area)
cent_model_mean <- mean(run_means$centroid)

# Data from video tracking
tracking <- read.csv(paste0(path, "stepwise_data.csv"))

# Means from tracking data
polar_track_mean <- mean(tracking$polar)
nnd_track_mean <- mean(tracking$nnd)
area_track_mean <- mean(tracking$area)
cent_track_mean <- mean(tracking$centroid)


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
