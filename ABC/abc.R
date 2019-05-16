# Approximate Bayesian Computation using the 'abc' package

# 'abc' requires:
#   1. a vector of observed summary statistics
#   2. a matrix of the simulated summary statistics, where each row corresponds
#      to a simulation and each column corresponds to a summary statistic
#   3. a matrix of the simulated parameter values, where each row corrresponds
#      to a parameter.

library(abc)

path <- "~/Desktop/DO NOT ERASE/1NUIG/Mackerel/Mackerel Data"


# import observed summary statistics - from tracking data
tracking_summary <- read.csv(paste0(path, ""))

# import matrix of simulated summary statistics - from model
model_summary <- read.csv(paste0(path, ""))

# import matrix of simulated parameter values - from model
model_params <- read.csv(paste0(path, ""))

# Use 'abc' to accept top 1% of runs as approximate posteriors
shoaling.abc <- abc(target = ____,   # observed summary statistics
                    param = ____,  # simulated parameter values, i.e. dependent variable(s)
                    sumstat = ____,  # simulated summary statistics / independent variables
                    tol = 0.1, method = "rejection")  # proportion of funs to accept; type of ABC to use

summary(shoaling.abc)

# then plot
