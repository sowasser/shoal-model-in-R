# Raincloud plot for prior and posterior distributions from ABC data

library(readr)
library(tidyr)
library(ggplot2)
library(Hmisc)
library(plyr)
library(reshape2)

# Link to geom_flat_violin function
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")


# Reshape dataframes to fit what's needed for ggplot2
priors1 <- model_params[, 1:3]
priors1 <- melt(priors1)
priors1 <- cbind(priors1, rep("prior", length(priors1$value)))
colnames(priors1) <- c("parameter", "value", "distribution")

post_all <- as.data.frame(shoaling.abc$unadj.values)
posts1 <- post_all[, 1:3]
posts1 <- melt(posts1)
posts1 <- cbind(posts1, rep("posterior", length(posts1$value)))
colnames(posts1) <- c("parameter", "value", "distribution")

dists1 <- rbind(priors1, posts1)  # Combine everything into one dataframe

raincloud1 <- ggplot(data = dists1, aes(y = value, x = parameter, fill = parameter)) +
  geom_flat_violin(position = position_nudge(x = .1, y = 0)) +
  # geom_point(aes(y = parameter, color = distribution), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = .1, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
  # expand_limits(x = 5.15) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  coord_flip() +
  theme_bw() +
  scale_fill_manual(values = custom_color) +
  xlab(" ") + ylab(" ") +  # axis labels
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~distribution, scale="free")

pdf("~/Desktop/raincloud1.pdf", width=10, height=5)
print(raincloud1)
dev.off()


priors2 <- model_params[, 4:6]
priors2 <- melt(priors2)
priors2 <- cbind(priors2, rep("prior", length(priors2$value)))
colnames(priors2) <- c("parameter", "value", "distribution")

post_all <- as.data.frame(shoaling.abc$unadj.values)
posts2 <- post_all[, 4:6]
posts2 <- melt(posts2)
posts2 <- cbind(posts2, rep("posterior", length(posts2$value)))
colnames(posts2) <- c("parameter", "value", "distribution")

dists2 <- rbind(priors2, posts2)  # Combine everything into one dataframe

raincloud2 <- ggplot(data = dists2, aes(y = value, x = parameter, fill = parameter)) +
  geom_flat_violin(position = position_nudge(x = .1, y = 0)) +
  # geom_point(aes(y = parameter, color = distribution), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = .1, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
  # expand_limits(x = 5.25) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  coord_flip() +
  theme_bw() +
  scale_fill_manual(values = custom_color) +
  xlab(" ") + ylab(" ") +  # axis labels
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~distribution, scale="free")

pdf("~/Desktop/raincloud2.pdf", width=10, height=5)
print(raincloud2)
dev.off()
