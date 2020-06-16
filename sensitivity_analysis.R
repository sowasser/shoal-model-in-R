# Script for sensistivity analysis of the parameters varying in the Approximate
# Bayesian Computation framework in abc.R, generated in the data_sensitivity.py
# script in fish-shoaling-model

library(ggplot2)
library(viridis)

path <- "~/Desktop/DO NOT ERASE/1NUIG/Mackerel/Mackerel Data/"  # for laptop

# Read in data from the model running with only one parameter varying
var_speed <- read.csv(paste0(path, "var-speed.csv"))
var_vision <- read.csv(paste0(path, "var-vision.csv"))
var_sep <- read.csv(paste0(path, "var-sep.csv"))
var_cohere <- read.csv(paste0(path, "var-cohere.csv"))
var_separate <- read.csv(paste0(path, "var-separate.csv"))
var_match <- read.csv(paste0(path, "var-match.csv"))

# Combine into one dataframe
var_all <- rbind(var_speed, var_vision, var_sep, var_cohere, var_separate, var_match)

# Conduct some kinda analysis ;)

# Graphs of varying parameter vs. summary statistics
