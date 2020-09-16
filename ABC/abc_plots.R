# Code for plots to check:
  # 1. prior & posterior distributions for the abc model (from abc.R).
  # 2. The true vs. estimated values for the parameters following cross-validation
# Have to run script abc.R before this to import/create variables.

library(ggplot2)
library(reshape2)

# custom_color <- c("#463682", "#287D8E", "#3CBB76", "#DCE41A")
custom_color <- c("#404387", "#22A784", "#790251", "#2A788E", "#45015A", "#fDE725")
color2 <- c("#79D151", "#29788E")

plot_date <- "03Aug2020"

# Prior/posterior distributions from abc --------------------------------------
# Data needs to be transformed to be one vector of values labeled with which
# parameter it is and which distribution it's from.

# Reshape dataframes to fit what's needed for ggplot2
priors <- melt(model_params)  
priors <- cbind(priors, rep("prior", length(priors$value)))
colnames(priors) <- c("parameter", "value", "distribution")

post_all <- as.data.frame(shoaling.abc$unadj.values)
posts <- melt(post_all)
posts <- cbind(posts, rep("posterior", length(posts$value)))
colnames(posts) <- c("parameter", "value", "distribution")

dists <- rbind(priors, posts)  # Combine everything into one dataframe

# Make sure all columns will be recongised appropriately for R/ggplot
dists$value <- as.numeric(as.character(dists$value))

# Plot distributions as boxplots
dist_boxplot <- ggplot(dists, aes(x = parameter, y = value, fill = distribution)) +
  geom_boxplot() + 
  theme_bw() + 
  scale_fill_manual(values = color2) +
  xlab(" ") + ylab(" ") +  # axis labels
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~parameter, scale="free")

ggsave(filename= paste0("~/Desktop/dist_boxplot_", plot_date, ".pdf"), 
       plot=dist_boxplot, width=180, height=150, units="in", dpi=300)


# Plot distributions as density plots
dist_density <- ggplot(dists, aes(x = value, fill = distribution, color = distribution)) +
  theme_bw() +
  scale_fill_manual(values = color2) +
  scale_color_manual(values = color2) +
  geom_density(alpha = 0.5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~parameter, scale="free")

ggsave(filename= paste0("~/Desktop/dist_density_", plot_date, ".pdf"), 
       plot=dist_density, width=180, height=130, units="mm", dpi=300)


# Cross-validation plots of true vs. estimated parameter values ---------------
# Data from cross-validation output needs to be reshaped to be plotted with ggplot2
cv_true_raw <- as.data.frame(shoaling.cv$true)
cv_true <- melt(cv_true_raw)

cv_estim_raw <- as.data.frame(shoaling.cv$estim)
colnames(cv_estim_raw) <- c("speed", "vision", "spacing", "cohere", "separate", "match")
cv_estim <- melt(cv_estim_raw)

cv_all <- cbind(cv_true, cv_estim$value)
colnames(cv_all) <- c("parameter", "true", "estimated")

cv_plots <- ggplot(cv_all, aes(x = true, y = estimated)) + #select data, include color-coding
  theme_bw() +
  geom_point(size=0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "#29788E") + #trendline and get rid of shaded confidence region, change size
  xlab("true value") +
  ylab("estimated value") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~parameter, scale="free")

ggsave(filename= paste0("~/Desktop/cv_plots_", plot_date, ".pdf"), 
       plot=cv_plots, width=180, height=130, units="mm", dpi=300)


# Coverage plots --------------------------------------------------------------
# Checking distribution of values across different proportions of ABC acceptances

# Separate out and save the data from the coverage test
diag <- shoaling.cov$diag  # Summary data with p-value from diagonstic test (KS)
raw <- subset(shoaling.cov$raw, select = -c(testset, nacc))  # remove columns 
raw_coverage <- melt(raw, id="tol")

# Plot coverage test outcomes
coverage_hist <- ggplot() +
  theme_bw() +
  geom_histogram(data = raw_coverage, aes(x = value), bins = 10, color = "#29788E", fill = "#87b7c4") +
  xlab("tolerance (p value)") +
  ylab("number") +
  ylim(0, 400) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~variable, scale="free")

ggsave(filename= paste0("~/Desktop/coverage_hist_", plot_date, ".pdf"), 
       plot=coverage_hist, width=180, height=130, units="mm", dpi=300)
