# This script, developed by Dr. Andrew Jackson, Zoology professor at Trinity 
# College Dublin, calculates & plots the summary statistics of variables

# TODO: write the functions that'll do this more seamlessly

single_run <- read.csv("~/Desktop/Local/Mackerel/Mackerel Data/single_run.csv")

# NND -------------------------------------------------------------------------
nnd_mu <- mean(single_run$nnd)  # mean
nnd_med <- median(single_run$nnd)  # median
nnd_variance <- var(single_run$nnd)  # variance
nnd_stan.dev <- sd(single_run$nnd)  # standard deviation
nnd_n <- length(single_run$nnd)  # the number of observations
nnd_standard.error <- sd(single_run$nnd) / sqrt(length(single_run$nnd))  # SE of the mean
nnd_maximum <- max(single_run$nnd, na.rm=T)
nnd_minimum <- min(single_run$nnd, na.rm=T)

nnd_summary <- rbind(nnd_mu, nnd_med, nnd_variance, nnd_stan.dev, nnd_n, nnd_standard.error, nnd_maximum, nnd_minimum)

# Polarization ----------------------------------------------------------------
polar_mu <- mean(single_run$polar)  # mean
polar_med <- median(single_run$polar)  # median
polar_variance <- var(single_run$polar)  # variance
polar_stan.dev <- sd(single_run$polar)  # standard deviation
polar_n <- length(single_run$polar)  # the number of observations
polar_standard.error <- sd(single_run$polar) / sqrt(length(single_run$polar))  # SE of the mean
polar_maximum <- max(single_run$polar, na.rm=T)
polar_minimum <- min(single_run$polar, na.rm=T)

polar_summary <- rbind(polar_mu, polar_med, polar_variance, polar_stan.dev, polar_n, polar_standard.error, polar_maximum, polar_minimum)

# Shoal Area ------------------------------------------------------------------
area_mu <- mean(single_run$area)  # mean
area_med <- median(single_run$area)  # median
area_variance <- var(single_run$area)  # variance
area_stan.dev <- sd(single_run$area)  # standard deviation
area_n <- length(single_run$area)  # the number of observations
area_standard.error <- sd(single_run$area) / sqrt(length(single_run$area))  # SE of the mean
area_maximum <- max(single_run$area, na.rm=T)
area_minimum <- min(single_run$area, na.rm=T)

area_summary <- rbind(area_mu, area_med, area_variance, area_stan.dev, area_n, area_standard.error, area_maximum, area_minimum)

# Distance from Centroid ------------------------------------------------------
cent_mu <- mean(single_run$cent)  # mean
cent_med <- median(single_run$cent)  # median
cent_variance <- var(single_run$cent)  # variance
cent_stan.dev <- sd(single_run$cent)  # standard deviation
cent_n <- length(single_run$cent)  # the number of observations
cent_standard.error <- sd(single_run$cent) / sqrt(length(single_run$cent))  # SE of the mean
cent_maximum <- max(single_run$cent, na.rm=T)
cent_minimum <- min(single_run$cent, na.rm=T)

cent_summary <- rbind(cent_mu, cent_med, cent_variance, cent_stan.dev, cent_n, cent_standard.error, cent_maximum, cent_minimum)


