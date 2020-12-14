# Script for timeseries analysis of ideal model (with parameter inputs gleaned
# from ABC) and real fish.

library(fs)
library(ggplot2)
library(ggcorrplot)
library(dplyr)

color2_2 <- c("#440154", "#29788E")
color3 <- c("#440154", "#29788E", "#79D151")

path <- "~/Desktop/DO NOT ERASE/1NUIG/Mackerel/Mackerel Data/"  # for laptop

steps <- c(1:99)  


# Read in modelled (general) data ---------------------------------------------
general_runs <- "~/Desktop/DO NOT ERASE/1NUIG/Mackerel/Mackerel Data/general runs/"  

g0 <- cbind(steps, read.csv(paste0(general_runs, "single_run_0.csv"))[200:298, -1], rep("modelled (general)", length(99)), rep("1", length(99)))
g1 <- cbind(steps, read.csv(paste0(general_runs, "single_run_1.csv"))[200:298, -1], rep("modelled (general)", length(99)), rep("2", length(99)))
g2 <- cbind(steps, read.csv(paste0(general_runs, "single_run_2.csv"))[200:298, -1], rep("modelled (general)", length(99)), rep("3", length(99)))
g3 <- cbind(steps, read.csv(paste0(general_runs, "single_run_3.csv"))[200:298, -1], rep("modelled (general)", length(99)), rep("4", length(99)))
g4 <- cbind(steps, read.csv(paste0(general_runs, "single_run_4.csv"))[200:298, -1], rep("modelled (general)", length(99)), rep("5", length(99)))
g5 <- cbind(steps, read.csv(paste0(general_runs, "single_run_5.csv"))[200:298, -1], rep("modelled (general)", length(99)), rep("6", length(99)))
g6 <- cbind(steps, read.csv(paste0(general_runs, "single_run_6.csv"))[200:298, -1], rep("modelled (general)", length(99)), rep("7", length(99)))
g7 <- cbind(steps, read.csv(paste0(general_runs, "single_run_7.csv"))[200:298, -1], rep("modelled (general)", length(99)), rep("8", length(99)))
g8 <- cbind(steps, read.csv(paste0(general_runs, "single_run_8.csv"))[200:298, -1], rep("modelled (general)", length(99)), rep("9", length(99)))
g9 <- cbind(steps, read.csv(paste0(general_runs, "single_run_9.csv"))[200:298, -1], rep("modelled (general)", length(99)), rep("10", length(99)))

colnames(g0) <- c("step", "distance from centroid", "nearest neighbour distance", "polarization", "shoal area", "source", "run")
colnames(g1) <- c("step", "distance from centroid", "nearest neighbour distance", "polarization", "shoal area", "source", "run")
colnames(g2) <- c("step", "distance from centroid", "nearest neighbour distance", "polarization", "shoal area", "source", "run")
colnames(g3) <- c("step", "distance from centroid", "nearest neighbour distance", "polarization", "shoal area", "source", "run")
colnames(g4) <- c("step", "distance from centroid", "nearest neighbour distance", "polarization", "shoal area", "source", "run")
colnames(g5) <- c("step", "distance from centroid", "nearest neighbour distance", "polarization", "shoal area", "source", "run")
colnames(g6) <- c("step", "distance from centroid", "nearest neighbour distance", "polarization", "shoal area", "source", "run")
colnames(g7) <- c("step", "distance from centroid", "nearest neighbour distance", "polarization", "shoal area", "source", "run")
colnames(g8) <- c("step", "distance from centroid", "nearest neighbour distance", "polarization", "shoal area", "source", "run")
colnames(g9) <- c("step", "distance from centroid", "nearest neighbour distance", "polarization", "shoal area", "source", "run")

g0_all <- melt(g0, id.vars = c("step", "source", "run"))
g1_all <- melt(g1, id.vars = c("step", "source", "run"))
g2_all <- melt(g2, id.vars = c("step", "source", "run"))
g3_all <- melt(g3, id.vars = c("step", "source", "run"))
g4_all <- melt(g4, id.vars = c("step", "source", "run"))
g5_all <- melt(g5, id.vars = c("step", "source", "run"))
g6_all <- melt(g6, id.vars = c("step", "source", "run"))
g7_all <- melt(g7, id.vars = c("step", "source", "run"))
g8_all <- melt(g8, id.vars = c("step", "source", "run"))
g9_all <- melt(g9, id.vars = c("step", "source", "run"))


# Read in modelled (NND-only) data --------------------------------------------
NND_runs <- "~/Desktop/DO NOT ERASE/1NUIG/Mackerel/Mackerel Data/general runs/"  

n0 <- cbind(steps, read.csv(paste0(NND_runs, "single_run_0.csv"))[200:298, -1], rep("modelled (NND-only)", length(99)), rep("1", length(99)))
n1 <- cbind(steps, read.csv(paste0(NND_runs, "single_run_1.csv"))[200:298, -1], rep("modelled (NND-only)", length(99)), rep("2", length(99)))
n2 <- cbind(steps, read.csv(paste0(NND_runs, "single_run_2.csv"))[200:298, -1], rep("modelled (NND-only)", length(99)), rep("3", length(99)))
n3 <- cbind(steps, read.csv(paste0(NND_runs, "single_run_3.csv"))[200:298, -1], rep("modelled (NND-only)", length(99)), rep("4", length(99)))
n4 <- cbind(steps, read.csv(paste0(NND_runs, "single_run_4.csv"))[200:298, -1], rep("modelled (NND-only)", length(99)), rep("5", length(99)))
n5 <- cbind(steps, read.csv(paste0(NND_runs, "single_run_5.csv"))[200:298, -1], rep("modelled (NND-only)", length(99)), rep("6", length(99)))
n6 <- cbind(steps, read.csv(paste0(NND_runs, "single_run_6.csv"))[200:298, -1], rep("modelled (NND-only)", length(99)), rep("7", length(99)))
n7 <- cbind(steps, read.csv(paste0(NND_runs, "single_run_7.csv"))[200:298, -1], rep("modelled (NND-only)", length(99)), rep("8", length(99)))
n8 <- cbind(steps, read.csv(paste0(NND_runs, "single_run_8.csv"))[200:298, -1], rep("modelled (NND-only)", length(99)), rep("9", length(99)))
n9 <- cbind(steps, read.csv(paste0(NND_runs, "single_run_9.csv"))[200:298, -1], rep("modelled (NND-only)", length(99)), rep("10", length(99)))

colnames(n0) <- c("step", "distance from centroid", "nearest neighbour distance", "polarization", "shoal area", "source", "run")
colnames(n1) <- c("step", "distance from centroid", "nearest neighbour distance", "polarization", "shoal area", "source", "run")
colnames(n2) <- c("step", "distance from centroid", "nearest neighbour distance", "polarization", "shoal area", "source", "run")
colnames(n3) <- c("step", "distance from centroid", "nearest neighbour distance", "polarization", "shoal area", "source", "run")
colnames(n4) <- c("step", "distance from centroid", "nearest neighbour distance", "polarization", "shoal area", "source", "run")
colnames(n5) <- c("step", "distance from centroid", "nearest neighbour distance", "polarization", "shoal area", "source", "run")
colnames(n6) <- c("step", "distance from centroid", "nearest neighbour distance", "polarization", "shoal area", "source", "run")
colnames(n7) <- c("step", "distance from centroid", "nearest neighbour distance", "polarization", "shoal area", "source", "run")
colnames(n8) <- c("step", "distance from centroid", "nearest neighbour distance", "polarization", "shoal area", "source", "run")
colnames(n9) <- c("step", "distance from centroid", "nearest neighbour distance", "polarization", "shoal area", "source", "run")

n0_all <- melt(n0, id.vars = c("step", "source", "run"))
n1_all <- melt(n1, id.vars = c("step", "source", "run"))
n2_all <- melt(n2, id.vars = c("step", "source", "run"))
n3_all <- melt(n3, id.vars = c("step", "source", "run"))
n4_all <- melt(n4, id.vars = c("step", "source", "run"))
n5_all <- melt(n5, id.vars = c("step", "source", "run"))
n6_all <- melt(n6, id.vars = c("step", "source", "run"))
n7_all <- melt(n7, id.vars = c("step", "source", "run"))
n8_all <- melt(n8, id.vars = c("step", "source", "run"))
n9_all <- melt(n9, id.vars = c("step", "source", "run"))


# Tracked data, combine all, and graph ----------------------------------------
tracking_scaled <- read.csv(paste0(path, "stepwise_data_scaled.csv"))
track <- cbind(steps, tracking_scaled[, -1], rep("tracked", length(99)))
colnames(track) <- c("step", 
                     "distance from centroid", 
                     "nearest neighbour distance", 
                     "polarization", 
                     "shoal area", 
                     "source")

track_all <- melt(track, id.vars = c("step", "source"))

general_all <- as.data.frame(rbind(g0_all, g1_all, g2_all, g3_all, g4_all, 
                                   g5_all, g6_all, g7_all, g8_all, g9_all))

nnd_all <- as.data.frame(rbind(n0_all, n1_all, n2_all, n3_all, n4_all, n5_all, 
                               n6_all, n7_all, n8_all, n9_all))


# Graphs of timeseries
time_graphs <- ggplot() + 
  theme_bw() + 
  geom_line(data=general_all, aes(x=step, y=value, color = run), size = 0.5) +  # line
  geom_line(data=track_all, aes(x=step, y=value, color=source), size = 3) +
  xlab("step") +
  ylab("statistic") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~variable, scale="free")

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
