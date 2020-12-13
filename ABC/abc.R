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
library(dplyr)
library(car)
library(abctools)
library(ggcorrplot)
library(reshape2)


date <- "11Dec2020"  # TODO: change date to correct data off of ICHEC.

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
tracking <- read.csv(paste0(general_path, "stepwise_data_scaled.csv"))
colnames(tracking) <- c("step", "cent", "nnd", "area", "polar")
tracking <- tracking[, c("cent", "nnd", "polar", "area")]  # reorder to match other data

# Import ICHEC data ----------------------------------------------------------
# path2 <- "~/Desktop/Local/Mackerel/Mackerel Data/ICHEC/27feb2020"  # desktop
setwd(paste0("~/Desktop/DO NOT ERASE/1NUIG/Mackerel/Mackerel Data/ICHEC/", date))  # laptop
ichec_path <- paste0("~/Desktop/DO NOT ERASE/1NUIG/Mackerel/Mackerel Data/ICHEC/", date) # laptop

# Testing if data import will work with one file ------------------------------
# setwd(path)  # laptop
# test <- read.table("sep_output0.txt", sep = "")

# TODO: run this if new files from ICHEC need to be collated ------------------
# Create a list of all of the files in this location, read them as tables,
# and consolidate them into one. 
model <- list.files(ichec_path, pattern="*.txt") %>% map_df(~read.table(., sep = ""))
write.csv(model, paste0(general_path, "ICHEC_data_", date, ".csv"))  # create .csv

# TODO: run this if collation already completed -------------------------------
model_saved <- read.csv(paste0(general_path, "ICHEC_data_", date, ".csv"))
model <- model_saved[,-1]  # Remove 1st column, generated when data was written to .csv


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
colnames(model_params) <- c("speed", "vision", "spacing", "cohere", "separate", "match")


# matrix of simulated summary statistics, where each row corresponds to  a 
# simulation and each column corresponds to a summary statistic.
model_stats <- model[, 1:16]


# Correlations ----------------------------------------------------------------
model_stats_means <- model_stats %>% select(polar_mean, nnd_mean, cent_mean, area_mean)
model_stats_cor <- cor(model_stats_means, method = "spearman")
ggcorrplot(model_stats_cor, type = "lower", lab = TRUE, colors = c("#79D151", "white", "#29788E"))


# Run ABC ---------------------------------------------------------------------

# Use 'abc' to accept top 1% of runs as approximate posteriors
shoaling.abc <- abc(target = real_fish,   # observed summary statistics
                    param = model_params,  # simulated parameter values, i.e. dependent variable(s)
                    sumstat = model_stats,  # simulated summary statistics / independent variables
                    tol = 0.001, method = "rejection")  # proportion of runs to accept; type of ABC to use
summary(shoaling.abc)


# Check if any post distribs are significantly different from priors ----------
# Create array with one column for label, one for the parameter
sd1 <- cbind(model_params$speed, rep("prior", length(model_params$speed)))
vs1 <- cbind(model_params$vision, rep("prior", length(model_params$vision)))
sp1 <- cbind(model_params$spacing, rep("prior", length(model_params$spacing)))
co1 <- cbind(model_params$cohere, rep("prior", length(model_params$cohere)))
sep1 <- cbind(model_params$separate, rep("prior", length(model_params$separate)))
mt1 <- cbind(model_params$match, rep("prior",  length(model_params$match)))

# Repeat for posterior distribution
post_all <- as.data.frame(shoaling.abc$unadj.values)
sd2 <- cbind(post_all$speed, rep("post", length(post_all$speed)))
vs2 <- cbind(post_all$vision, rep("post", length(post_all$vision)))
sp2 <- cbind(post_all$spacing, rep("post", length(post_all$spacing)))
co2 <- cbind(post_all$cohere, rep("post", length(post_all$cohere)))
sep2 <- cbind(post_all$separate, rep("post", length(post_all$separate)))
mt2 <- cbind(post_all$match, rep("post", length(post_all$match)))

# Combine into separate dataframes
sd_all <- as.data.frame(rbind(sd1, sd2))
vs_all <- as.data.frame(rbind(vs1, vs2))
sp_all <- as.data.frame(rbind(sp1, sp2))
co_all <- as.data.frame(rbind(co1, co2))
sep_all <- as.data.frame(rbind(sep1, sep2))
mt_all <- as.data.frame(rbind(mt1, mt2))

# Levene's test for homogeneity of varaince btw prior & posterior distributions
l_sd <- leveneTest(y = as.numeric(as.character(sd_all$V1)), group = sd_all$V2)  
l_vs <- leveneTest(y = as.numeric(as.character(vs_all$V1)), group = vs_all$V2)  
l_sp <- leveneTest(y = as.numeric(as.character(sp_all$V1)), group = sp_all$V2)  
l_co <- leveneTest(y = as.numeric(as.character(co_all$V1)), group = co_all$V2)  
l_sep <- leveneTest(y = as.numeric(as.character(sep_all$V1)), group = sep_all$V2)  
l_mt <- leveneTest(y = as.numeric(as.character(mt_all$V1)), group = mt_all$V2)

# Post-hoc p-value adjustment for multiple tests - Holm method
# Collate p-values from Levene's tests
lpv <- c(l_sd[1,3], l_vs[1,3], l_sp[1,3], l_co[1,3], l_sep[1,3], l_mt[1,3])

ladj <- p.adjust(p = lpv, method = "holm")  # run adjustment

# Combine with other values
lname <- c("speed", "vision", "spacing", "cohere", "match", "separate")
levene_out <- as.data.frame(cbind(lname, lpv, ladj))


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
colnames(cv_estim) <- c("speed", "vision", "spacing", "cohere", "separate", "match")

summary(lm(cv_true$speed ~ cv_estim$speed))  # R2 = 0.7776
summary(lm(cv_true$vision ~ cv_estim$vision))  # R2 = 0.587
summary(lm(cv_true$spacing ~ cv_estim$spacing))  # R2 = 0.2939  
summary(lm(cv_true$cohere ~ cv_estim$cohere))  # R2 = 0.03297
summary(lm(cv_true$separate ~ cv_estim$separate))  # R2 = 0.06506 
summary(lm(cv_true$match ~ cv_estim$match))  # R2 = 0.04561


# Test coverage property of ABC results --------------------------
testsets <- sample(1:nrow(model_stats), 100)  # Subset of model runs to include
eps <- exp(seq(log(0.1), log(10), length.out = 15))  # Epsilon values

# Coverage test for parameter estimation
shoaling.cov <- cov.pi(param = model_params,
                       sumstat = model_stats,
                       testsets = testsets,  # the rows of sumstat to be used as pseudo-observed data?
                       multicore = TRUE,
                       cores = 4,
                       tol = seq(0.1, 1, by=0.1),   # proportions of ABC acceptances
                       diagnostics = "KS")  # Kolmogorov-Smirinov

diag <- shoaling.cov$diag  # Summary data with p-value from diagonstic test (KS)
diag_sig <- subset(diag, pvalue <= 0.01)  # Select rows where KS p-value < 0.01

# Ceate a new dataframe of the p-values organised for the manuscript table
diag_summary <- dcast(subset(diag, select = -c(test)), tol ~ parameter)
diag_summary <- diag_summary[, c("tol", "speed", "vision", "spacing", 
                             "cohere", "separate", "match")]

# Run Kolmogorov-Smirnov test to see if coverage distributions vary 
# significantly from a uniform distribution of the same size & shape. 

# Having an issue because there are "ties" (repeated values) in the 
# distributions, which shouldn't be present in a continuous distribution - 
# probably attributable to rounding errors.
ks_sd <- ks.test(raw$speed, punif(1000, 0.1, 1))
ks_vs <- ks.test(raw$vision, punif(1000, 0.1, 1))
ks_sp <- ks.test(raw$spacing, punif(1000, 0.1, 1))
ks_co <- ks.test(raw$cohere, punif(1000, 0.1, 1))
ks_sep <- ks.test(raw$separate, punif(1000, 0.1, 1))
ks_mt <- ks.test(raw$match, punif(1000, 0.1, 1))

kspv <- c(ks_sd[2], ks_vs[2], ks_sp[2], ks_co[2], ks_sep[2], ks_mt[2])
ksadj <- p.adjust(p = kspv, method = "holm")

ks_out <- as.data.frame(cbind(lname, kspv, ksadj))
