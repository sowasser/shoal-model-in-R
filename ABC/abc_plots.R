# Code for plots to check prior & posterior distributions for the abc model.

library(ggplot2)
library(gridExtra)

# Tracking Graphs -------------------------------------------------------------
# New tracking dataframe just for graphs
track_graphs <- tracking

# Add "step" column
track_graphs$step <- 1:nrow(track_graphs)


require(gridExtra)
track_cent <- ggplot(data = track_graphs, aes(x=step, y=cent)) +
  theme_classic() + geom_point() + geom_line()
track_nnd <- ggplot(data = track_graphs, aes(x=step, y=nnd)) +
  theme_classic() + geom_point() + geom_line()
track_area <- ggplot(data = track_graphs, aes(x=step, y=area)) +
  theme_classic() + geom_point() + geom_line()
track_polar <- ggplot(data = track_graphs, aes(x=step, y=polar)) +
  theme_classic() + geom_point() + geom_line()

# Model Graphs ----------------------------------------------------------------
require(gridExtra)
# set "x" and "group" to the parameter you're looking at
# set "y" to the summary statistic you're looking at
# TODO: change this to something that works better - maybe facets?
speed_polar <- ggplot(data = model, aes(x=speed, y=polar_mean)) + 
  theme_classic() + geom_point()
speed_nnd <- ggplot(data = model, aes(x=speed, y=nnd_mean)) + 
  theme_classic() + geom_point()
speed_area <- ggplot(data = model, aes(x=speed, y=area_mean)) + 
  theme_classic() + geom_point()
speed_cent <- ggplot(data = model, aes(x=speed, y=cent_mean)) + 
  theme_classic() + geom_point()

vision_polar <- ggplot(data = model, aes(x=vision, y=polar_mean)) + 
  theme_classic() + geom_point()
vision_nnd <- ggplot(data = model, aes(x=vision, y=nnd_mean)) + 
  theme_classic() + geom_point()
vision_area <- ggplot(data = model, aes(x=vision, y=area_mean)) + 
  theme_classic() + geom_point()
vision_cent <- ggplot(data = model, aes(x=vision, y=cent_mean)) + 
  theme_classic() + geom_point()

sep_polar <- ggplot(data = model, aes(x=sep, y=polar_mean)) + 
  theme_classic() + geom_point()
sep_nnd <- ggplot(data = model, aes(x=sep, y=nnd_mean)) + 
  theme_classic() + geom_point()
sep_area <- ggplot(data = model, aes(x=sep, y=area_mean)) + 
  theme_classic() + geom_point()
sep_cent <- ggplot(data = model, aes(x=sep, y=cent_mean)) + 
  theme_classic() + geom_point()

# Create multiple graphs in page, row by row ----------------------------------
prior_plots <- arrangeGrob(speed_polar, vision_polar, sep_polar, track_polar,
                           speed_nnd, vision_nnd, sep_nnd, track_nnd,
                           speed_area, vision_area, sep_area, track_area,
                           speed_cent, vision_cent, sep_cent, track_cent, ncol=4)

# TODO: figure out how to save a smaller file here.
ggsave(file=paste0("~/Desktop/prior_plots", date, ".pdf"), 
       plot=prior_plots,
       width=20, height=20, units="cm", dpi=300, limitsize=TRUE)


# Prior/posterior distributions from abc --------------------------------------
# Data needs to be transformed to be one vector of values labeled with which
# statistic it is and which distribution it's from

# Create labels for statistics
sd <- rep("speed", length(model_params$speed))
vs <- rep("vision", length(model_params$vision))
sp <- rep("separation", length(model_params$sep))

# Combine values with their statistic label in one column
prior_sd <- cbind(model_params$speed, sd)
prior_vs <- cbind(model_params$vision, vs)
prior_sp <- cbind(model_params$sep, sp)
priors <- as.data.frame(rbind(prior_sd, prior_vs, prior_sp))

# Combine values and statistics label in one column with distribution label
cat_priors <- rep("prior", length(priors$sd))
priors <- cbind(priors, cat_priors)
colnames(priors) <- c("value", "statistic", "distribution")

# Combine values with their statistic label in one column
post_all <- as.data.frame(shoaling.abc$unadj.values)
post_sd <- cbind(post_all$speed, sd)
post_vs <- cbind(post_all$vision, vs)
post_sp <- cbind(post_all$sep, sp)
posts <- as.data.frame(rbind(post_sd, post_vs, post_sp))

# Combine values and statistics label in one column with distribution label
cat_posts <- rep("posterior", length(posts$sd))
posts <- cbind(posts, cat_posts)
colnames(posts) <- c("value", "statistic", "distribution")

dists <- rbind(priors, posts)  # Combine everything into one dataframe

# Make sure all columns will be regongised appropriately for R/ggplot
dists$value <- as.numeric(as.character(dists$value))
dists$statistic <- as.factor(dists$statistic)
dists$distribution <- as.factor(dists$distribution)

dist_boxplot <- ggplot(dists, aes(x = statistic, y = value, fill = distribution)) +
  geom_boxplot() + 
  theme_bw() + 
  xlab(" ") + ylab(" ") +  # axis labels
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~statistic, scale="free")

pdf(paste0("~/Desktop/dist_boxplot_", date, ".pdf"))
print(dist_boxplot)
dev.off()
