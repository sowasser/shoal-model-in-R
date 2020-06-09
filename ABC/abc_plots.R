# Code for plots to check:
  # 1. the summary statistics for the simultated vs. empirical data
  # 2. prior & posterior distributions for the abc model (from abc.R).

library(ggplot2)

# Comparison of summary stats from model vs. real fish ------------------------
# Data needs to be transformed to be one vector of values labeled with which 
# data source it's from.

# For tracking data: 
# Create array with one column for label, one for the statistic
tc <- cbind(tracking$cent, rep("cent", length(tracking$cent)))
tn <- cbind(tracking$cent, rep("nnd", length(tracking$nnd)))
tp <- cbind(tracking$polar, rep("polar", length(tracking$polar)))
ta <- cbind(tracking$area, rep("area", length(tracking$area)))
# Stack all into one dataframe
track_stats <- as.data.frame(rbind(tc, tn, tp, ta))
# Combine values and statistic label in one column with data source label
track_stats <- cbind(track_stats, rep("tracking", length(track_stats$V1)))
colnames(track_stats) <- c("value", "statistic", "data_source")

# Repeat for model data
mc <- cbind(model$cent_mean, rep("cent", length(model$cent_mean)))
mn <- cbind(model$nnd_mean, rep("nnd", length(model$nnd_mean)))
mp <- cbind(model$polar_mean, rep("polar", length(model$polar_mean)))
ma <- cbind(model$area_mean, rep("area", length(model$area_mean)))
model_stats <- as.data.frame(rbind(mc, mn, mp, ma))
model_stats <- cbind(model_stats, rep("model", length(model_stats$V1)))
colnames(model_stats) <- c("value", "statistic", "data_source")

stats <- rbind(track_stats, model_stats)  # Combine everything in to one dataframe

# Make sure all columns will be recognised appropriately for R/ggplot
stats$value <- as.numeric(as.character(stats$value))
stats$statistic <- as.factor(stats$statistic)
stats$data_source <- as.factor(stats$data_source)

# Text for facet labels
stat_labs <- c("distance from centroid", 
               "nearest neighbour distance",
               "polarization",
               "shoal area")
names(stat_labs) <- c("cent", "nnd", "polar", "area")

stat_boxplot <- ggplot(stats, aes(x = statistic, y = value, fill = data_source)) +
  geom_boxplot() +
  theme_bw() +
  xlab(" ") + ylab(" ") +  # axis labels
  labs(fill = "data source") +  # legend title
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~statistic, scale="free", labeller = labeller(statistic = stat_labs))

pdf(paste0("~/Desktop/stat_boxplot_", date, ".pdf"))
print(stat_boxplot)
dev.off()


# Prior/posterior distributions from abc --------------------------------------
# Data needs to be transformed to be one vector of values labeled with which
# parameter it is and which distribution it's from.

# For prior distribution:
# Create array with one column for label, one for the parameter
prior_sd <- cbind(model_params$speed, rep("speed", length(model_params$speed)))
prior_vs <- cbind(model_params$vision, rep("vision", length(model_params$vision)))
prior_sp <- cbind(model_params$separation, rep("separation", length(model_params$separation)))
prior_co <- cbind(model_params$cohere, rep("cohere", length(model_params$cohere)))
prior_sep <- cbind(model_params$separate, rep("separate", length(model_params$separate)))
prior_mt <- cbind(model_params$match, rep("match", length(model_params$match)))

# Stack all into one dataframe
priors <- as.data.frame(rbind(prior_sd, prior_vs, prior_sp, prior_co, prior_sep, prior_mt))
# Combine values and parameter label in one column with distribution label
priors <- cbind(priors, rep("prior", length(priors$V1)))
colnames(priors) <- c("value", "parameter", "distribution")

# Repeat for posterior distribution
post_all <- as.data.frame(shoaling.abc$unadj.values)
post_sd <- cbind(post_all$speed, rep("speed", length(post_all$speed)))
post_vs <- cbind(post_all$vision, rep("vision", length(post_all$vision)))
post_sp <- cbind(post_all$separation, rep("separation", length(post_all$separation)))
post_co <- cbind(post_all$cohere, rep("cohere", length(post_all$cohere)))
post_sep <- cbind(post_all$separate, rep("separate", length(post_all$separate)))
post_mt <- cbind(post_all$match, rep("match", length(post_all$match)))

posts <- as.data.frame(rbind(post_sd, post_vs, post_sp, post_co, post_sep, post_mt))
posts <- cbind(posts, rep("posterior", length(posts$V1)))
colnames(posts) <- c("value", "parameter", "distribution")

dists <- rbind(priors, posts)  # Combine everything into one dataframe

# Make sure all columns will be recongised appropriately for R/ggplot
dists$value <- as.numeric(as.character(dists$value))
dists$parameter <- as.factor(dists$parameter)
dists$distribution <- as.factor(dists$distribution)

dist_boxplot <- ggplot(dists, aes(x = parameter, y = value, fill = distribution)) +
  geom_boxplot() + 
  theme_bw() + 
  xlab(" ") + ylab(" ") +  # axis labels
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~parameter, scale="free")

pdf(paste0("~/Desktop/dist_boxplot_", date, ".pdf"))
print(dist_boxplot)
dev.off()


# Cross-validation plots of true vs. estimated parameter values ---------------
cv_true <- as.data.frame(shoaling.cv$true)
cv_estim <- as.data.frame(shoaling.cv$estim)
colnames(cv_estim) <- c("speed", "vision", "separation", "cohere", "separate", "match")

# Combine all true & estimated parameter values into one dataframe w/3 columns
cv_sd <- cbind(rep("speed", length(cv_true$speed)), as.numeric(cv_true$speed), as.numeric(cv_estim$speed))
cv_vs <- cbind(rep("vision", length(cv_true$vision)), as.numeric(cv_true$vision), as.numeric(cv_estim$vision))
cv_sp <- cbind(rep("separation", length(cv_true$separation)), as.numeric(cv_true$separation), as.numeric(cv_estim$separation))
cv_co <- cbind(rep("cohere", length(cv_true$cohere)), as.numeric(cv_true$cohere), as.numeric(cv_estim$cohere))
cv_sep <- cbind(rep("separate", length(cv_true$separate)), as.numeric(cv_true$separate), as.numeric(cv_estim$separate))
cv_mt <- cbind(rep("match", length(cv_true$match)), as.numeric(cv_true$match), as.numeric(cv_estim$match))

cv_all <- as.data.frame(rbind(cv_sd, cv_vs, cv_sp, cv_co, cv_sep, cv_mt))
colnames(cv_all) <- c("parameter", "true", "estimated")

cv_plots <- ggplot(cv_all, aes(x = true, y = estimated)) + #select data, include color-coding
  theme_bw() +
  geom_point(size=0.5) +
  geom_smooth(method = "lm", se = FALSE) + #trendline and get rid of shaded confidence region, change size
  xlab("true value") + #x-axis label
  ylab("estimated value") + #y-axis label
  theme(axis.text.x=element_blank(), axis.text.y=element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~parameter, scale="free")

pdf(paste0("~/Desktop/cv_plots_", date, ".pdf"))
print(cv_plots)
dev.off()

