# Script for timeseries analysis of ideal model (with parameter inputs gleaned
# from ABC) and real fish.

library(reshape2)
library(ggplot2)
library(ggcorrplot)
library(dplyr)

color2_2 <- c("#440154", "#29788E")
color3 <- c("#440154", "#29788E", "#79D151")

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
  geom_ribbon(data=general_all, aes(x=step, ymin=min, ymax=max, fill=source), fill=color3[1], alpha=0.5) +
  geom_ribbon(data=nnd_only, aes(x=step, ymin=min, ymax=max, fill=source), fill=color3[2], alpha=0.5) +
  geom_line(data=track_all, aes(x=step, y=value), size = 2, color=color3[3]) +
  xlab("step") +
  ylab("statistic") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~statistic, scale="free")

ggsave(filename="~/Desktop/timeseries_graphs_test.pdf", plot=time_graphs, 
       width=180, height=140, units="mm", dpi=300)


# Overlap ---------------------------------------------------------------------
# Calculate number of simulated values within range of observed values
overlap_cent <- subset(model_run$cent, 
                       model_run$cent > min(tracking_scaled$cent) & model_run$cent < max(tracking_scaled$cent))
overlap_nnd <- subset(model_run$nnd, 
                      model_run$nnd > min(tracking_scaled$nnd) & model_run$nnd < max(tracking_scaled$nnd))
overlap_polar <- subset(model_run$polar, 
                        model_run$polar > min(tracking_scaled$polar) & model_run$polar < max(tracking_scaled$polar))
overlap_area <- subset(model_run$area, 
                       model_run$area > min(tracking_scaled$area) & model_run$area < max(tracking_scaled$area))

# Find percentages & create new dataframe
percentages <- c((length(overlap_cent) / 99) * 100, 
                 (length(overlap_nnd) / 99) * 100, 
                 (length(overlap_polar) / 99) * 100, 
                 (length(overlap_area) / 99) * 100)

# Calculation of overlap for NND-only
overlap_nndtracked <- subset(model_run_nnd$nnd, 
                             model_run_nnd$nnd > min(tracking_scaled$nnd) & model_run_nnd$nnd < max(tracking_scaled$nnd))
overlap_nndmodels <- subset(model_run_nnd$nnd, 
                            model_run_nnd$nnd > min(model_run$nnd) & model_run_nnd$nnd < max(model_run$nnd))

percentages_nnd <- c((length(overlap_nndtracked) / 99) * 100, 
                    (length(overlap_nndmodels) / 99) * 100)

# Correlations ----------------------------------------------------------------
stats <- model_run_all[, -1]
model_cor <- cor(stats, method = "spearman")
ggcorrplot(model_cor, type = "lower", lab = TRUE, colors = c("#79D151", "white", "#29788E"))


# Export data used for further analysis in Python -----------------------------
model_run2 <- cbind(model_run, model_run_nnd$nnd)
colnames(model_run2) <- c("step", "cent", "nnd", "polar", "area", "nnd_only")
write.csv(model_run2, paste0(path, "timeseries_modelled.csv"))

colnames(tracking_scaled) <- c("step", "cent", "nnd", "area", "polar")
write.csv(tracking_scaled, paste0(path, "timeseries_tracked.csv"))
