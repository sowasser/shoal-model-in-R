# Script for sensistivity analysis of the parameters varying in the Approximate
# Bayesian Computation framework in abc.R, generated in the data_sensitivity.py
# script in fish-shoaling-model

library(ggplot2)
library(viridis)

path <- "~/Desktop/DO NOT ERASE/1NUIG/Mackerel/Mackerel Data/"  # for laptop

# Read in data from the model running with only one parameter varying
var_speed <- read.csv(paste0(path, "var-speed100.csv"))
var_vision <- read.csv(paste0(path, "var-vision100.csv"))
var_sep <- read.csv(paste0(path, "var-sep100.csv"))
var_cohere <- read.csv(paste0(path, "var-cohere100.csv"))
var_separate <- read.csv(paste0(path, "var-separate100.csv"))
var_match <- read.csv(paste0(path, "var-match100.csv"))

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

# Graphs of varying parameter vs. summary statistics
SA_boid_plots <- ggplot(SA_boid, aes(x = output, y = input, color = parameter)) + #select data, include color-coding
  theme_bw() +
  geom_point(size=0.3) +
  geom_smooth(method = "lm", se = FALSE) + #trendline without shaded confidence region
  xlab("statistic output") +
  ylab("parameter input") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~statistic, scale="free")

pdf("~/Desktop/boid_plots.pdf")
print(SA_boid_plots)
dev.off()

SA_move_plots <- ggplot(SA_move, aes(x = output, y = input, color = parameter)) + #select data, include color-coding
  theme_bw() +
  geom_point(size=0.3) +
  geom_smooth(method = "lm", se = FALSE) + #trendline without shaded confidence region
  xlab("statistic output") +
  ylab("parameter input") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~statistic, scale="free")

pdf("~/Desktop/move_plots.pdf")
print(SA_move_plots)
dev.off()

# Linear regression for sensitivity analysis
summary(lm(var_speed$speed ~ var_speed$Polarization))
summary(lm(var_speed$speed ~ var_speed$Nearest.Neighbour.Distance))
summary(lm(var_speed$speed ~ var_speed$Mean.Distance.from.Centroid))
summary(lm(var_speed$speed ~ var_speed$Shoal.Area))

summary(lm(var_vision$vision ~ var_vision$Polarization))
summary(lm(var_vision$vision ~ var_vision$Nearest.Neighbour.Distance))
summary(lm(var_vision$vision ~ var_vision$Mean.Distance.from.Centroid))
summary(lm(var_vision$vision ~ var_vision$Shoal.Area))

summary(lm(var_sep$sep ~ var_sep$Polarization))
summary(lm(var_sep$sep ~ var_sep$Nearest.Neighbour.Distance))
summary(lm(var_sep$sep ~ var_sep$Mean.Distance.from.Centroid))
summary(lm(var_sep$sep ~ var_sep$Shoal.Area))

summary(lm(var_cohere$cohere ~ var_cohere$Polarization))
summary(lm(var_cohere$cohere ~ var_cohere$Nearest.Neighbour.Distance))
summary(lm(var_cohere$cohere ~ var_cohere$Mean.Distance.from.Centroid))
summary(lm(var_cohere$cohere ~ var_cohere$Shoal.Area))

summary(lm(var_separate$separate ~ var_separate$Polarization))
summary(lm(var_separate$separate ~ var_separate$Nearest.Neighbour.Distance))
summary(lm(var_separate$separate ~ var_separate$Mean.Distance.from.Centroid))
summary(lm(var_separate$separate ~ var_separate$Shoal.Area))

summary(lm(var_match$match ~ var_match$Polarization))
summary(lm(var_match$match ~ var_match$Nearest.Neighbour.Distance))
summary(lm(var_match$match ~ var_match$Mean.Distance.from.Centroid))
summary(lm(var_match$match ~ var_match$Shoal.Area))


