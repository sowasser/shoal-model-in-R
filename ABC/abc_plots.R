# Code for plots to check:
  # 1. the summary statistics for the simultated vs. empirical data
  # 2. prior & posterior distributions for the abc model (from abc.R).

library(ggplot2)
library(gridExtra)

# Comparison of summary stats from model vs. real fish ------------------------





# Prior/posterior distributions from abc --------------------------------------
# Data needs to be transformed to be one vector of values labeled with which
# parameter it is and which distribution it's from

# Create labels for parameters
sd <- rep("speed", length(model_params$speed))
vs <- rep("vision", length(model_params$vision))
sp <- rep("separation", length(model_params$sep))

# Combine values with their parameter label in one column
prior_sd <- cbind(model_params$speed, sd)
prior_vs <- cbind(model_params$vision, vs)
prior_sp <- cbind(model_params$sep, sp)
priors <- as.data.frame(rbind(prior_sd, prior_vs, prior_sp))

# Combine values and parameter label in one column with distribution label
cat_priors <- rep("prior", length(priors$sd))
priors <- cbind(priors, cat_priors)
colnames(priors) <- c("value", "parameter", "distribution")

# Combine values with their statistic label in one column
post_all <- as.data.frame(shoaling.abc$unadj.values)
post_sd <- cbind(post_all$speed, sd)
post_vs <- cbind(post_all$vision, vs)
post_sp <- cbind(post_all$sep, sp)
posts <- as.data.frame(rbind(post_sd, post_vs, post_sp))

# Combine values and parameter label in one column with distribution label
cat_posts <- rep("posterior", length(posts$sd))
posts <- cbind(posts, cat_posts)
colnames(posts) <- c("value", "parameter", "distribution")

dists <- rbind(priors, posts)  # Combine everything into one dataframe

# Make sure all columns will be regongised appropriately for R/ggplot
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
