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

library(abc)
library(tidyverse)
library(car)

date <- "12Jun2020"  # TODO: change date to correct data off of ICHEC.

general_path <- "~/Desktop/DO NOT ERASE/1NUIG/Mackerel/Mackerel Data/"  # for laptop
# path <- "~/Desktop/Local/Mackerel/Mackerel Data/"  # for desktop

# Read in local Python data ---------------------------------------------------

# Mean of all runs, calculated for every step of the model & changes in a variable
# speed <- read.csv(paste0(path,"var-speed.csv"))
# vision <- read.csv(paste0(path,"var-vision.csv"))
# separation <- read.csv(paste0(path,"var-sep.csv"))

# model <- rbind(speed, vision, separation)
# colnames(model) <- c("polar", "nnd", "area", "centroid", "speed", "vision", "separation")


# Read in data from video tracking --------------------------------------------
tracking <- read.csv(paste0(general_path, "stepwise_data.csv"))
colnames(tracking) <- c("step", "cent", "nnd", "area", "polar")
tracking <- tracking[, c("cent", "nnd", "polar", "area")]  # reorder to match other data

# Import ICHEC data ----------------------------------------------------------
# path2 <- "~/Desktop/Local/Mackerel/Mackerel Data/ICHEC/27feb2020"  # desktop
setwd(paste0("~/Desktop/DO NOT ERASE/1NUIG/Mackerel/Mackerel Data/ICHEC/", date))  # laptop
ichec_path <- paste0("~/Desktop/DO NOT ERASE/1NUIG/Mackerel/Mackerel Data/ICHEC/", date) # laptop

# Testing if data import will work with one file ------------------------------
# setwd(path)  # laptop
# test <- read.table("sep_output0.txt", sep = "")

# Create a list of all of the files in this location, read them as tables,
# and consolidate them into one. 
model <- list.files(ichec_path, pattern="*.txt") %>% map_df(~read.table(., sep = ""))

# Create .csv file with all the ICHEC data
write.csv(model, paste0(general_path, "ICHEC_data_", date, ".csv"))


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
               tmean[1], tmean[2], tmean[3], tmean[4],
               tsd[1], tsd[2], tsd[3], tsd[4])


# matrix of simulated parameter values, where each row corresponds to a
# simulation and each column correponds to a parameter.
model_params <- model[, 17:22]

# matrix of simulated summary statistics, where each row corresponds to  a 
# simulation and each column corresponds to a summary statistic.
model_stats <- model[, 1:16]


# Use 'abc' to accept top 1% of runs as approximate posteriors
shoaling.abc <- abc(target = real_fish,   # observed summary statistics
                    param = model_params,  # simulated parameter values, i.e. dependent variable(s)
                    sumstat = model_stats,  # simulated summary statistics / independent variables
                    tol = 0.001, method = "rejection")  # proportion of runs to accept; type of ABC to use
summary(shoaling.abc)


# Check if any post distribs are significantly different from priors ----------
# Create array with one column for label, one for the parameter
sd1 <- cbind(model_params$speed, rep(1, length(model_params$speed)))
vs1 <- cbind(model_params$vision, rep(1, length(model_params$vision)))
sp1 <- cbind(model_params$separation, rep(1, length(model_params$separation)))
co1 <- cbind(model_params$cohere, rep(1, length(model_params$cohere)))
sep1 <- cbind(model_params$separate, rep(1, length(model_params$separate)))
mt1 <- cbind(model_params$match, rep(1,  length(model_params$match)))

# Repeat for posterior distribution
post_all <- as.data.frame(shoaling.abc$unadj.values)
sd2 <- cbind(post_all$speed, rep(2, length(post_all$speed)))
vs2 <- cbind(post_all$vision, rep(2, length(post_all$vision)))
sp2 <- cbind(post_all$separation, rep(2, length(post_all$separation)))
co2 <- cbind(post_all$cohere, rep(2, length(post_all$cohere)))
sep2 <- cbind(post_all$separate, rep(2, length(post_all$separate)))
mt2 <- cbind(post_all$match, rep(2, length(post_all$match)))

# Combine into separate dataframes
sd_all <- as.data.frame(rbind(sd1, sd2))
vs_all <- as.data.frame(rbind(vs1, vs2))
sp_all <- as.data.frame(rbind(sp1, sp2))
co_all <- as.data.frame(rbind(co1, co2))
sep_all <- as.data.frame(rbind(sep1, sep2))
mt_all <- as.data.frame(rbind(mt1, mt2))

# Run Mann Whitney U test
wilcox.test(V1 ~ V2, sd_all)  # p < 2.2e-16
wilcox.test(V1 ~ V2, vs_all)  # p = 0.705
wilcox.test(V1 ~ V2, sp_all)  # p = 5.217e-10
wilcox.test(V1 ~ V2, co_all)  # p = 0.2953
wilcox.test(V1 ~ V2, sep_all)  # p = 4.982e-11
wilcox.test(V1 ~ V2, mt_all)  # p = 2.146e-07


# Run cross-validation of ABC results -----------------------------------------
shoaling.cv <- cv4abc(param = model_params,
                      sumstat = model_stats, 
                      abc.out = shoaling.abc, 
                      nval = 100, tols = 0.01)  # size of cross validation sample; tolerance rate
summary(shoaling.cv)

# Plots for the relationship between true & estimated values are in abc_plots.R

# Linear models of cross-validation results
cv_true <- as.data.frame(shoaling.cv$true)
cv_estim <- as.data.frame(shoaling.cv$estim)
colnames(cv_estim) <- c("speed", "vision", "separation", "cohere", "separate", "match")

summary(lm(cv_true$speed ~ cv_estim$speed))  # R2 = 0.7039
summary(lm(cv_true$vision ~ cv_estim$vision))  # R2 = 0.6123
summary(lm(cv_true$separation ~ cv_estim$separation))  # R2 = 0.2969 
summary(lm(cv_true$cohere ~ cv_estim$cohere))  # R2 = 0.06308
summary(lm(cv_true$separate ~ cv_estim$separate))  # R2 = 0.1157
summary(lm(cv_true$match ~ cv_estim$match))  # R2 = 0.08451 
