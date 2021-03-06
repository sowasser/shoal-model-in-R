# Script for timeseries analysis of ideal model (with parameter inputs gleaned
# from ABC) and real fish.

library(reshape2)
library(ggplot2)
library(ggcorrplot)
library(dplyr)

color2_2 <- c("#440154", "#29788E")
color3 <- c("#440154", "#29788E", "#79d151")
color4 <- c("#79d151", "#fee900", "#29788E", "#440154")

path <- "~/Desktop/DO NOT ERASE/1NUIG/Mackerel/Mackerel Data/"  # for laptop

steps <- c(1:99)  


# Read in modelled (general) data & find means for each statistic--------------
general_runs <- "~/Desktop/DO NOT ERASE/1NUIG/Mackerel/Mackerel Data/general runs/"  

g0 <- cbind(steps, read.csv(paste0(general_runs, "single_run_0.csv"))[200:298, -1])
g1 <- cbind(steps, read.csv(paste0(general_runs, "single_run_1.csv"))[200:298, -1])
g2 <- cbind(steps, read.csv(paste0(general_runs, "single_run_2.csv"))[200:298, -1])
g3 <- cbind(steps, read.csv(paste0(general_runs, "single_run_3.csv"))[200:298, -1])
g4 <- cbind(steps, read.csv(paste0(general_runs, "single_run_4.csv"))[200:298, -1])
g5 <- cbind(steps, read.csv(paste0(general_runs, "single_run_5.csv"))[200:298, -1])
g6 <- cbind(steps, read.csv(paste0(general_runs, "single_run_6.csv"))[200:298, -1])
g7 <- cbind(steps, read.csv(paste0(general_runs, "single_run_7.csv"))[200:298, -1])
g8 <- cbind(steps, read.csv(paste0(general_runs, "single_run_8.csv"))[200:298, -1])
g9 <- cbind(steps, read.csv(paste0(general_runs, "single_run_9.csv"))[200:298, -1])

g_nnd <- cbind(g0$nnd, g1$nnd, g2$nnd, g3$nnd, g4$nnd, g5$nnd, g6$nnd, g7$nnd ,g8$nnd, g9$nnd)
mean_nnd <- as.data.frame(cbind(steps, 
                                rowMeans(g_nnd), 
                                apply(g_nnd, 1, FUN=min), apply(g_nnd, 1, FUN=max),
                                rep("nearest neighbour distance", length(99))))

g_cent <- cbind(g0$cent, g1$cent, g2$cent, g3$cent, g4$cent, g5$cent, g6$cent, g7$cent ,g8$cent, g9$cent)
mean_cent <- as.data.frame(cbind(steps, 
                                 rowMeans(g_cent), 
                                 apply(g_cent, 1, FUN=min), apply(g_cent, 1, FUN=max),
                                 rep("distance from centroid", length(99))))

g_polar <- cbind(g0$polar, g1$polar, g2$polar, g3$polar, g4$polar, g5$polar, g6$polar, g7$polar ,g8$polar, g9$polar)
mean_polar <- as.data.frame(cbind(steps, 
                                  rowMeans(g_polar), 
                                  apply(g_polar, 1, FUN=min), apply(g_polar, 1, FUN=max),
                                  rep("polarization", length(99))))

g_area <- cbind(g0$area, g1$area, g2$area, g3$area, g4$area, g5$area, g6$area, g7$area ,g8$area, g9$area)
mean_area <- as.data.frame(cbind(steps, 
                                 rowMeans(g_area), 
                                 apply(g_area, 1, FUN=min), apply(g_area, 1, FUN=max),
                                 rep("shoal area", length(99))))

general_all <- cbind(rbind(mean_nnd, mean_cent, mean_polar, mean_area), 
                     rep("modelled (general)", length(396)))
colnames(general_all) <- c("step", "mean", "min", "max", "statistic", "source")
general_all$step <- as.numeric(general_all$step)
general_all$mean <- as.numeric(general_all$mean)
general_all$min <- as.numeric(general_all$min)
general_all$max <- as.numeric(general_all$max)


# Read in model runs with parameters set to mean of prior distributions -------
prior_runs <- "~/Desktop/DO NOT ERASE/1NUIG/Mackerel/Mackerel Data/prior runs/"  

p0 <- cbind(read.csv(paste0(prior_runs, "single_run_prior_0.csv"))[200:298, -1])
p1 <- cbind(read.csv(paste0(prior_runs, "single_run_prior_1.csv"))[200:298, -1])
p2 <- cbind(read.csv(paste0(prior_runs, "single_run_prior_2.csv"))[200:298, -1])
p3 <- cbind(read.csv(paste0(prior_runs, "single_run_prior_3.csv"))[200:298, -1])
p4 <- cbind(read.csv(paste0(prior_runs, "single_run_prior_4.csv"))[200:298, -1])
p5 <- cbind(read.csv(paste0(prior_runs, "single_run_prior_5.csv"))[200:298, -1])
p6 <- cbind(read.csv(paste0(prior_runs, "single_run_prior_6.csv"))[200:298, -1])
p7 <- cbind(read.csv(paste0(prior_runs, "single_run_prior_7.csv"))[200:298, -1])
p8 <- cbind(read.csv(paste0(prior_runs, "single_run_prior_8.csv"))[200:298, -1])
p9 <- cbind(read.csv(paste0(prior_runs, "single_run_prior_9.csv"))[200:298, -1])

p_nnd <- cbind(p0$nnd, p1$nnd, p2$nnd, p3$nnd, p4$nnd, p5$nnd, p6$nnd, p7$nnd ,p8$nnd, p9$nnd)
p_mean_nnd <- as.data.frame(cbind(steps, 
                                  rowMeans(p_nnd), 
                                  apply(p_nnd, 1, FUN=min), apply(p_nnd, 1, FUN=max),
                                  rep("nearest neighbour distance", length(99))))

p_cent <- cbind(p0$cent, p1$cent, p2$cent, p3$cent, p4$cent, p5$cent, p6$cent, p7$cent ,p8$cent, p9$cent)
p_mean_cent <- as.data.frame(cbind(steps, 
                                   rowMeans(p_cent), 
                                   apply(p_cent, 1, FUN=min), apply(p_cent, 1, FUN=max),
                                   rep("distance from centroid", length(99))))

p_polar <- cbind(p0$polar, p1$polar, p2$polar, p3$polar, p4$polar, p5$polar, p6$polar, p7$polar ,p8$polar, p9$polar)
p_mean_polar <- as.data.frame(cbind(steps, 
                                    rowMeans(p_polar), 
                                    apply(p_polar, 1, FUN=min), apply(p_polar, 1, FUN=max),
                                    rep("polarization", length(99))))

p_area <- cbind(p0$area, p1$area, p2$area, p3$area, p4$area, p5$area, p6$area, p7$area ,p8$area, p9$area)
p_mean_area <- as.data.frame(cbind(steps, 
                                   rowMeans(p_area), 
                                   apply(p_area, 1, FUN=min), apply(p_area, 1, FUN=max),
                                   rep("shoal area", length(99))))

priors_all <- cbind(rbind(p_mean_nnd, p_mean_cent, p_mean_polar, p_mean_area), 
                    rep("modelled (mean priors)", length(396)))
colnames(priors_all) <- c("step", "mean", "min", "max", "statistic", "source")
priors_all$step <- as.numeric(priors_all$step)
priors_all$mean <- as.numeric(priors_all$mean)
priors_all$min <- as.numeric(priors_all$min)
priors_all$max <- as.numeric(priors_all$max)


# Read in modelled (NND-only) data & find mean NND ----------------------------
NND_runs <- "~/Desktop/DO NOT ERASE/1NUIG/Mackerel/Mackerel Data/NND runs/"  

n0 <- cbind(read.csv(paste0(NND_runs, "single_run_nnd_0.csv"))[200:298, -1])
n1 <- cbind(read.csv(paste0(NND_runs, "single_run_nnd_1.csv"))[200:298, -1])
n2 <- cbind(read.csv(paste0(NND_runs, "single_run_nnd_2.csv"))[200:298, -1])
n3 <- cbind(read.csv(paste0(NND_runs, "single_run_nnd_3.csv"))[200:298, -1])
n4 <- cbind(read.csv(paste0(NND_runs, "single_run_nnd_4.csv"))[200:298, -1])
n5 <- cbind(read.csv(paste0(NND_runs, "single_run_nnd_5.csv"))[200:298, -1])
n6 <- cbind(read.csv(paste0(NND_runs, "single_run_nnd_6.csv"))[200:298, -1])
n7 <- cbind(read.csv(paste0(NND_runs, "single_run_nnd_7.csv"))[200:298, -1])
n8 <- cbind(read.csv(paste0(NND_runs, "single_run_nnd_8.csv"))[200:298, -1])
n9 <- cbind(read.csv(paste0(NND_runs, "single_run_nnd_9.csv"))[200:298, -1])

n_nnd <- cbind(n0, n1, n2, n3, n4, n5, n6, n7, n8, n9)
nnd_mean_nnd <- as.data.frame(cbind(steps, 
                                    rowMeans(n_nnd), 
                                    apply(n_nnd, 1, FUN=min), apply(n_nnd, 1, FUN=max),
                                    rep("nearest neighbour distance", length(99))))
nnd_only <- cbind(nnd_mean_nnd, rep("modelled (NND-only)", length(99)))
colnames(nnd_only) <- c("step", "mean", "min", "max", "statistic", "source")
nnd_only$step <- as.numeric(nnd_only$step)
nnd_only$mean <- as.numeric(nnd_only$mean)
nnd_only$min <- as.numeric(nnd_only$min)
nnd_only$max <- as.numeric(nnd_only$max)


# Read in tracked data --------------------------------------------------------
tracking_scaled <- read.csv(paste0(path, "stepwise_data_scaled.csv"))
track <- cbind(steps, tracking_scaled[, -1], rep("tracked", length(99)))
colnames(track) <- c("step", 
                     "distance from centroid", 
                     "nearest neighbour distance", 
                     "shoal area", 
                     "polarization", 
                     "source")

track_all <- melt(track, id.vars = c("step", "source"))
colnames(track_all) <- c("step", "source", "statistic", "value")


# Graphs of timeseries --------------------------------------------------------
time_graphs <- ggplot() + 
  theme_bw() + 
  scale_color_manual(values=color4) +  # Custom color palette defined above
  # Add data derived from prior distributions
  geom_ribbon(data=priors_all, aes(x=step, ymin=min, ymax=max), fill="#fee900", alpha=0.3) +
  geom_line(data=priors_all, aes(x=step, y=mean, color=source)) +
  # Add general-ABC derived data
  geom_ribbon(data=general_all, aes(x=step, ymin=min, ymax=max), fill="#79d151", alpha=0.3) +
  geom_line(data=general_all, aes(x=step, y=mean, color=source)) +
  # Add NND-only derived data
  geom_ribbon(data=nnd_only, aes(x=step, ymin=min, ymax=max), fill="#29788e", alpha=0.3) +
  geom_line(data=nnd_only, aes(x=step, y=mean, color=source)) +
  # Add tracking data
  geom_line(data=track_all, aes(x=step, y=value, color=source), size = 1) +
  xlab("step") +
  ylab("statistic") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~statistic, scale="free")

ggsave(filename="~/Desktop/timeseries_graphs.pdf", plot=time_graphs, 
       width=200, height=140, units="mm", dpi=300)


# Export data used for further analysis in Python -----------------------------
colnames(mean_nnd) <- c("steps", "mean", "min", "max", "stat")
write.csv(mean_nnd, paste0(path, "ts_general_NND.csv"))

colnames(mean_cent) <- c("steps", "mean", "min", "max", "stat")
write.csv(mean_cent, paste0(path, "ts_general_cent.csv"))

colnames(mean_polar) <- c("steps", "mean", "min", "max", "stat")
write.csv(mean_polar, paste0(path, "ts_general_polar.csv"))

colnames(mean_area) <- c("steps", "mean", "min", "max", "stat")
write.csv(mean_area, paste0(path, "ts_general_area.csv"))


colnames(p_mean_nnd) <- c("steps", "mean", "min", "max", "stat")
write.csv(p_mean_nnd, paste0(path, "ts_prior_NND.csv"))

colnames(p_mean_cent) <- c("steps", "mean", "min", "max", "stat")
write.csv(p_mean_cent, paste0(path, "ts_prior_cent.csv"))

colnames(p_mean_polar) <- c("steps", "mean", "min", "max", "stat")
write.csv(p_mean_polar, paste0(path, "ts_prior_polar.csv"))

colnames(p_mean_area) <- c("steps", "mean", "min", "max", "stat")
write.csv(p_mean_area, paste0(path, "ts_prior_area.csv"))


write.csv(nnd_only, paste0(path, "ts_nnd_NND.csv"))

colnames(tracking_scaled) <- c("step", "cent", "nnd", "area", "polar")
write.csv(tracking_scaled, paste0(path, "timeseries_tracked.csv"))
