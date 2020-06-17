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

# Isolate data columns & add a factor label
speed <- as.data.frame(cbind(var_speed$speed, rep("speed", length(var_speed$speed))))
vision <- as.data.frame(cbind(var_vision$vision, rep("vision", length(var_vision$vision))))
sep <- as.data.frame(cbind(var_sep$sep, rep("separation", length(var_sep$sep))))
cohere <- as.data.frame(cbind(var_cohere$cohere, rep("cohere", length(var_cohere$cohere))))
separate <- as.data.frame(cbind(var_separate$separate, rep("separate", length(var_separate$separate))))
match <- as.data.frame(cbind(var_match$match, rep("match", length(var_match$match))))

boid_params <- rbind(speed, vision, sep)
move_params <- rbind(cohere, match, separate)

# Separate out statistics and stack them
boid_stats <- rbind(var_speed[, 1:4], var_vision[, 1:4], var_sep[, 1:4])
move_stats <- rbind(var_cohere[, 1:4], var_separate[, 1:4], var_match[, 1:4])

colnames(boid_stats) <- c("cent", "nnd", "polar", "area")
colnames(move_stats) <- c("cent", "nnd", "polar", "area")

cent <- cbind(boid_stats$cent, rep("distance from centroid", length(boid_stats$cent)))
nnd <- cbind(boid_stats$nnd, rep("nearest neighbour distance", length(boid_stats$nnd)))
polar <- cbind(boid_stats$polar, rep("polarization", length(boid_stats$polar)))
area <- cbind(boid_stats$area, rep("shoal area", length(boid_stats$area)))

all_stats <- rbind(cent, nnd, polar, area)

# Combine parameter values and statistics all into one dataframe
SA_boid <- cbind(boid_params, all_stats)
colnames(SA_boid) <- c("input", "parameter", "output", "statistic")
SA_boid$input <- as.numeric(as.character(SA_boid$input))
SA_boid$output <- as.numeric(as.character(SA_boid$output))

SA_move <- cbind(move_params, all_stats)
colnames(SA_move) <- c("input", "parameter", "output", "statistic")
SA_move$input <- as.numeric(as.character(SA_move$input))
SA_move$output <- as.numeric(as.character(SA_move$output))

# Conduct some kinda analysis ;)

# Graphs of varying parameter vs. summary statistics
SA_boid_plots <- ggplot(SA_boid, aes(x = output, y = input, color = parameter)) + #select data, include color-coding
  theme_bw() +
  geom_point(size=0.3) +
  stat_ellipse() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~statistic, scale="free")

pdf("~/Desktop/boid_plots.pdf")
print(SA_boid_plots)
dev.off()

SA_move_plots <- ggplot(SA_move, aes(x = output, y = input, color = parameter)) + #select data, include color-coding
  theme_bw() +
  geom_point(size=0.3) +
  stat_ellipse() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~statistic, scale="free")

pdf("~/Desktop/move_plots.pdf")
print(SA_move_plots)
dev.off()

