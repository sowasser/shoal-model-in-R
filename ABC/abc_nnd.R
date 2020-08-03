# Approximate Bayesian Computation using the 'abc' package, for just NND

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
library(abctools)
library(ggplot2)
library(reshape2)

date_nnd <- "02Jul2020_nnd"  # TODO: change date to correct data off of ICHEC.

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
track_nnd <- tracking[, "nnd"]

# Import ICHEC data ----------------------------------------------------------
# path2 <- "~/Desktop/Local/Mackerel/Mackerel Data/ICHEC/27feb2020"  # desktop
# setwd(paste0("~/Desktop/DO NOT ERASE/1NUIG/Mackerel/Mackerel Data/ICHEC/", date_nnd))  # laptop
# ichec_path <- paste0("~/Desktop/DO NOT ERASE/1NUIG/Mackerel/Mackerel Data/ICHEC/", date_nnd) # laptop

# Testing if data import will work with one file ------------------------------
# setwd(path)  # laptop
# test <- read.table("sep_output0.txt", sep = "")

# Create a list of all of the files in this location, read them as tables,
# and consolidate them into one. 
# model_nnd <- list.files(ichec_path, pattern="*.txt") %>% map_df(~read.table(., sep = ""))

# Create .csv file with all the ICHEC data
# write.csv(model_nnd, paste0(general_path, "ICHEC_data_", date_nnd, ".csv"))

# TODO: run this if collation already completed
model_saved_nnd <- read.csv(paste0(general_path, "ICHEC_data_", date_nnd, ".csv"))
model_nnd <- model_saved_nnd[,-1]  # Remove 1st column, generated when data was written to .csv


# Adjust data inputs and run ABC ----------------------------------------------
# matrix of observed summary statistics, in same order as from the model
real_fish_nnd <- c(min(track_nnd), max(track_nnd), mean(track_nnd), sd(track_nnd))


# matrix of simulated parameter values, where each row corresponds to a
# simulation and each column correponds to a parameter.
model_params_nnd <- model_nnd[, 5:10]
colnames(model_params_nnd) <- c("speed", "vision", "spacing", "cohere", "separate", "match")

# matrix of simulated summary statistics, where each row corresponds to  a 
# simulation and each column corresponds to a summary statistic.
model_stats_nnd <- model_nnd[, 1:4]


# Use 'abc' to accept top 1% of runs as approximate posteriors
shoaling.nnd <- abc(target = real_fish_nnd,   # observed summary statistics
                    param = model_params_nnd,  # simulated parameter values, i.e. dependent variable(s)
                    sumstat = model_stats_nnd,  # simulated summary statistics / independent variables
                    tol = 0.001, method = "rejection")  # proportion of runs to accept; type of ABC to use
summary(shoaling.nnd)


# Check if any post distribs are significantly different from priors ----------

# Create array with one column for label, one for the parameter
sd1_nnd <- cbind(model_params_nnd$speed, rep("prior", length(model_params_nnd$speed)))
vs1_nnd <- cbind(model_params_nnd$vision, rep("prior", length(model_params_nnd$vision)))
sp1_nnd <- cbind(model_params_nnd$spacing, rep("prior", length(model_params_nnd$spacing)))
co1_nnd <- cbind(model_params_nnd$cohere, rep("prior", length(model_params_nnd$cohere)))
sep1_nnd <- cbind(model_params_nnd$separate, rep("prior", length(model_params_nnd$separate)))
mt1_nnd <- cbind(model_params_nnd$match, rep("prior",  length(model_params_nnd$match)))

# Repeat for posterior distribution
post_nnd <- as.data.frame(shoaling.nnd$unadj.values)
sd2_nnd <- cbind(post_nnd$speed, rep("post", length(post_nnd$speed)))
vs2_nnd <- cbind(post_nnd$vision, rep("post", length(post_nnd$vision)))
sp2_nnd <- cbind(post_nnd$spacing, rep("post", length(post_nnd$spacing)))
co2_nnd <- cbind(post_nnd$cohere, rep("post", length(post_nnd$cohere)))
sep2_nnd <- cbind(post_nnd$separate, rep("post", length(post_nnd$separate)))
mt2_nnd <- cbind(post_nnd$match, rep("post", length(post_nnd$match)))

# Combine into separate dataframes
sd_nnd <- as.data.frame(rbind(sd1_nnd, sd2_nnd))
vs_nnd <- as.data.frame(rbind(vs1_nnd, vs2_nnd))
sp_nnd <- as.data.frame(rbind(sp1_nnd, sp2_nnd))
co_nnd <- as.data.frame(rbind(co1_nnd, co2_nnd))
sep_nnd <- as.data.frame(rbind(sep1_nnd, sep2_nnd))
mt_nnd <- as.data.frame(rbind(mt1_nnd, mt2_nnd))

# Levene's test for homogeneity of varaince btw prior & posterior distributions
l_sd_nnd <- leveneTest(y = as.numeric(as.character(sd_nnd$V1)), group = sd_nnd$V2)  
l_vs_nnd <- leveneTest(y = as.numeric(as.character(vs_nnd$V1)), group = vs_nnd$V2)  
l_sp_nnd <- leveneTest(y = as.numeric(as.character(sp_nnd$V1)), group = sp_nnd$V2)  
l_co_nnd <- leveneTest(y = as.numeric(as.character(co_nnd$V1)), group = co_nnd$V2)  
l_sep_nnd <- leveneTest(y = as.numeric(as.character(sep_nnd$V1)), group = sep_nnd$V2)  
l_mt_nnd <- leveneTest(y = as.numeric(as.character(mt_nnd$V1)), group = mt_nnd$V2)

# Post-hoc p-value adjustment for multiple tests - Holm method
# Collate p-values from Levene's tests
lpv_nnd <- c(l_sd_nnd[1,3], l_vs_nnd[1,3], l_sp_nnd[1,3], l_co_nnd[1,3], l_sep_nnd[1,3], l_mt_nnd[1,3])

ladj_nnd <- p.adjust(p = lpv_nnd, method = "holm")  # run adjustment

# Combine with other values
lname <- c("speed", "vision", "spacing", "cohere", "match", "separate")
levene_out_nnd <- as.data.frame(cbind(lname, lpv_nnd, ladj_nnd))


# Run cross-validation of ABC results -----------------------------------------
shoaling.cv.nnd <- cv4abc(param = model_params_nnd,
                          sumstat = model_stats_nnd, 
                          abc.out = shoaling.nnd, 
                          nval = 100, tols = 0.01)  # size of cross validation sample; tolerance rate
summary(shoaling.cv.nnd)

# Plots for the relationship between true & estimated values are in abc_plots.R

# Linear models of cross-validation results
cv_true_nnd <- as.data.frame(shoaling.cv.nnd$true)
cv_estim_nnd <- as.data.frame(shoaling.cv.nnd$estim)
colnames(cv_estim_nnd) <- c("speed", "vision", "spacing", "cohere", "separate", "match")

summary(lm(cv_true_nnd$speed ~ cv_estim_nnd$speed))  # R2 = 0.5746
summary(lm(cv_true_nnd$vision ~ cv_estim_nnd$vision))  # R2 = 0.508
summary(lm(cv_true_nnd$spacing ~ cv_estim_nnd$spacing))  # R2 = 0.2047 
summary(lm(cv_true_nnd$cohere ~ cv_estim_nnd$cohere))  # R2 = 0.1009
summary(lm(cv_true_nnd$separate ~ cv_estim_nnd$separate))  # R2 = 0.1582
summary(lm(cv_true_nnd$match ~ cv_estim_nnd$match))  # R2 = -0.008139


# Plotting --------------------------------------------------------------------
# custom_color <- c("#463682", "#287D8E", "#3CBB76", "#DCE41A")
custom_color <- c("#404387", "#22A784", "#790251", "#2A788E", "#45015A", "#fDE725")
color2 <- c("#79D151", "#29788E")

plot_date_nnd <- "03Aug2020_NND"

# Data needs to be transformed to be one vector of values labeled with which
# parameter it is and which distribution it's from.

# Reshape dataframes to fit what's needed for ggplot2
priors_nnd <- melt(model_params_nnd)  
priors_nnd <- cbind(priors_nnd, rep("prior", length(priors_nnd$value)))
colnames(priors_nnd) <- c("parameter", "value", "distribution")

post_all_nnd <- as.data.frame(shoaling.nnd$unadj.values)
posts_nnd <- melt(post_all_nnd)
posts_nnd <- cbind(posts_nnd, rep("posterior", length(posts_nnd$value)))
colnames(posts_nnd) <- c("parameter", "value", "distribution")

dists_nnd <- rbind(priors_nnd, posts_nnd)  # Combine everything into one dataframe

# Make sure all columns will be recongised appropriately for R/ggplot
dists_nnd$value <- as.numeric(as.character(dists_nnd$value))

# Plot distributions as density plots
dist_density_nnd <- ggplot(dists_nnd, aes(x = value, fill = distribution, color = distribution)) +
  theme_bw() +
  scale_fill_manual(values = color2) +
  scale_color_manual(values = color2) +
  geom_density(alpha = 0.5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~parameter, scale="free")

ggsave(filename= paste0("~/Desktop/dist_density_", plot_date_nnd, ".pdf"), 
       plot=dist_density_nnd, width=180, height=130, units="mm", dpi=300)


# Cross-validation plots of true vs. estimated parameter values
# Data from cross-validation output needs to be reshaped to be plotted with ggplot2
cv_true_raw_nnd <- as.data.frame(shoaling.cv.nnd$true)
cv_true_nnd <- melt(cv_true_raw_nnd)

cv_estim_raw_nnd <- as.data.frame(shoaling.cv.nnd$estim)
colnames(cv_estim_raw_nnd) <- c("speed", "vision", "spacing", "cohere", "separate", "match")
cv_estim_nnd <- melt(cv_estim_raw_nnd)

cv_all_nnd <- cbind(cv_true_nnd, cv_estim_nnd$value)
colnames(cv_all_nnd) <- c("parameter", "true", "estimated")

#cv_plots_nnd <- ggplot(cv_all_nnd, aes(x = true, y = estimated)) + #select data, include color-coding
#  theme_bw() +
#  geom_point(size=0.5) +
#  geom_smooth(method = "lm", se = FALSE, color = "#29788E") + #trendline and get rid of shaded confidence region, change size
#  xlab("true value") +
#  ylab("estimated value") +
#  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
#  facet_wrap(~parameter, scale="free")

# ggsave(filename= paste0("~/Desktop/cv_plots_", plot_date, ".pdf"), 
#       plot=cv_plots_nnd, width=11, height=7, units="in")


# Mann-Whitney U tests to compare general ABC and NND-only ABC ----------------
# Must run general ABC script first
wilcox_sd <- wilcox.test(y = as.numeric(as.character(post_all$speed)), 
                         as.numeric(as.character(post_all_nnd$speed)))
wilcox_vs <- wilcox.test(y = as.numeric(as.character(post_all$vision)), 
                         as.numeric(as.character(post_all_nnd$vision)))
wilcox_sp <- wilcox.test(y = as.numeric(as.character(post_all$spacing)), 
                         as.numeric(as.character(post_all_nnd$spacing)))
wilcox_co <- wilcox.test(y = as.numeric(as.character(post_all$cohere)), 
                         as.numeric(as.character(post_all_nnd$cohere)))
wilcox_sep <- wilcox.test(y = as.numeric(as.character(post_all$separate)), 
                          as.numeric(as.character(post_all_nnd$separate)))
wilcox_mt <- wilcox.test(y = as.numeric(as.character(post_all$match)), 
                         as.numeric(as.character(post_all_nnd$match)))

wilcox_sd
wilcox_vs
wilcox_sp
wilcox_co
wilcox_sep
wilcox_mt
