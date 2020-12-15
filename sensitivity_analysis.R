# Script for sensistivity analysis of the parameters varying in the Approximate
# Bayesian Computation framework in abc.R, generated in the data_sensitivity.py
# script in fish-shoaling-model

library(ggplot2)
library(mgcv)

custom_color <- c("#404387", "#22A784", "#790251", "#2A788E", "#45015A", "#fDE725")
color3 <- c("#440154", "#29788E", "#79D151")
color3_2 <- c("#22A784", "#45015A", "#fDE725")

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
spacing <- as.data.frame(cbind(var_sep$sep, rep("spacing", length(var_sep$sep))))
cohere <- as.data.frame(cbind(var_cohere$cohere, rep("cohere", length(var_cohere$cohere))))
separate <- as.data.frame(cbind(var_separate$separate, rep("separate", length(var_separate$separate))))
match <- as.data.frame(cbind(var_match$match, rep("match", length(var_match$match))))

boid_params <- rbind(speed, vision, spacing)
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


# GAM for sensitivity analysis ------------------------------------------------
summary(gam(Polarization ~ s(speed, bs = "tp"), data = var_speed))  # R2 = 0.279
summary(gam(Nearest.Neighbour.Distance ~ s(speed, bs = "tp"), data = var_speed))  # 0.168
summary(gam(Mean.Distance.from.Centroid ~ s(speed, bs = "tp"), data = var_speed))  # 0.248
summary(gam(Shoal.Area ~ s(speed, bs = "tp"), data = var_speed))  # 0.267

summary(gam(Polarization ~ s(vision, bs = "tp"), data = var_vision))  # R2 = 0.0842
summary(gam(Nearest.Neighbour.Distance ~ s(vision, bs = "tp"), data = var_vision))  # 0.898
summary(gam(Mean.Distance.from.Centroid ~ s(vision, bs = "tp"), data = var_vision))  # 0.383
summary(gam(Shoal.Area ~ s(vision, bs = "tp"), data = var_vision))  # 0.796

summary(gam(Polarization ~ s(separation, bs = "tp"), data = var_sep))  # R2 = 0.27
summary(gam(Nearest.Neighbour.Distance ~ s(separation, bs = "tp"), data = var_sep))  # 0.94
summary(gam(Mean.Distance.from.Centroid ~ s(separation, bs = "tp"), data = var_sep))  # 0.461
summary(gam(Shoal.Area ~ s(separation, bs = "tp"), data = var_sep))  # 0.82

summary(gam(Polarization ~ s(cohere, bs = "tp"), data = var_cohere))  # R2 = 0.0105
summary(gam(Nearest.Neighbour.Distance ~ s(cohere, bs = "tp"), data = var_cohere))  # 0.832
summary(gam(Mean.Distance.from.Centroid ~ s(cohere, bs = "tp"), data = var_cohere))  # 0.198
summary(gam(Shoal.Area ~ s(cohere, bs = "tp"), data = var_cohere))  # 0.659

summary(gam(Polarization ~ s(separate, bs = "tp"), data = var_separate))  # R2 = 0.234
summary(gam(Nearest.Neighbour.Distance ~ s(separate, bs = "tp"), data = var_separate))  # 0.938
summary(gam(Mean.Distance.from.Centroid ~ s(separate, bs = "tp"), data = var_separate))  # 0.467
summary(gam(Shoal.Area ~ s(separate, bs = "tp"), data = var_separate))  # 0.822

summary(gam(Polarization ~ s(match, bs = "tp"), data = var_match))  # R2 = 0.504
summary(gam(Nearest.Neighbour.Distance ~ s(match, bs = "tp"), data = var_match))  # 0.631
summary(gam(Mean.Distance.from.Centroid ~ s(match, bs = "tp"), data = var_match))  # 0.482
summary(gam(Shoal.Area ~ s(match, bs = "tp"), data = var_match))  # 0.441

# Graphs of varying parameter vs. summary statistics --------------------------
SA_boid_plots <- ggplot(SA_boid, aes(x = input, y = output, color = parameter)) + #select data, include color-coding
  theme_bw() +
  scale_color_manual(values = color3) +
  geom_point(size=0.3) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "tp"), se = FALSE) + #trendline without shaded confidence region
  xlab("parameter input") +
  ylab("statistic output") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~statistic, scale="free")

ggsave(filename="~/Desktop/boid_plots.pdf", plot=SA_boid_plots, 
       width=180, height=150, units="mm", dpi=300)

SA_move_plots <- ggplot(SA_move, aes(x = input, y = output, color = parameter)) + #select data, include color-coding
  theme_bw() +
  scale_color_manual(values = color3) +
  geom_point(size=0.3) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "tp"), se = FALSE) + #trendline without shaded confidence region
  xlab("parameter input") +
  ylab("statistic output") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~statistic, scale="free")

ggsave(filename="~/Desktop/move_plots.pdf", plot=SA_move_plots, 
       width=180, height=150, units="mm", dpi=300)
