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


# import observed summary statistics - from tracking data
tp <- read.csv(paste0(path, "step1_polar.csv"))
tn <- read.csv(paste0(path, "step1_nnd.csv"))
tc <- read.csv(paste0(path, "step1_cent_dist.csv"))
ta <- read.csv(paste0(path, "step1_shoal_area.csv"))

tracking_summary <- as.data.frame(cbind(tp[, 2], tn[, 2], tc[, 2], ta[, 2]))
colnames(tracking_summary) <- c("polar", "nnd", "cent", "area")

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
