# Script for timeseries analysis of ideal model (with parameter inputs gleaned
# from ABC) and real fish.

library(ggplot2)
library(ggcorrplot)

color2_2 <- c("#440154", "#29788E")
color3 <- c("#440154", "#29788E", "#79D151")

path <- "~/Desktop/DO NOT ERASE/1NUIG/Mackerel/Mackerel Data/"  # for laptop

tracking_scaled <- read.csv(paste0(path, "stepwise_data_scaled.csv"))

model_run_all <- read.csv(paste0(path, "single_run.csv"))

model_run_all_nnd <- read.csv(paste0(path, "single_run_nnd.csv"))


# Timeseries ------------------------------------------------------------------
model_run <- model_run_all[100:198,]  # only keep 99 steps so same length as tracking_scaled
model_run_nnd <- model_run_all_nnd[100:198,]

steps <- c(1:99)  

# Reformat and stack to make graphs
tp <- cbind(steps, tracking_scaled$polar, rep("polarization", length(tracking_scaled$polar)))
tn <- cbind(steps, tracking_scaled$nnd, rep("nearest neighbour distance", length(tracking_scaled$nnd)))
tc <- cbind(steps, tracking_scaled$cent, rep("distance from centroid", length(tracking_scaled$cent)))
ta <- cbind(steps, tracking_scaled$area, rep("shoal area", length(tracking_scaled$area)))

track_all <- cbind(rbind(tp, tn, tc, ta), rep("tracked", length(396)))

mp <- cbind(steps, model_run$polar, rep("polarization", length(model_run$polar)))  
mn <- cbind(steps, model_run$nnd, rep("nearest neighbour distance", length(model_run$nnd)))
mc <- cbind(steps, model_run$cent, rep("distance from centroid", length(model_run$cent)))
ma <- cbind(steps, model_run$area, rep("shoal area", length(model_run$area)))

model_all <- cbind(rbind(mp, mn, mc, ma), rep("modelled (general)", length(396)))

# Just for NND, data from the parameters derived from the NND-only ABC
nndn <- cbind(steps, 
              model_run_nnd$nnd, 
              rep("nearest neighbour distance", length(model_run_nnd$nnd)), 
              rep("modelled (nnd-only)", length(396)))


timeseries_all <- as.data.frame(rbind(track_all, model_all, nndn))
colnames(timeseries_all) <- c("step", "value", "statistic", "source")
timeseries_all$value <- as.numeric(as.character(timeseries_all$value))
timeseries_all$step <- as.numeric(as.character(timeseries_all$step))

# Graphs of timeseries
time_graphs <- ggplot() + 
  theme_bw() + 
  scale_color_manual(values = color3) +
  geom_line(data=timeseries_all, aes(x=step, y=value, color = source), size = 1) +  # line
  xlab("step") +
  ylab("statistic") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~statistic, scale="free")

ggsave(filename="~/Desktop/timeseries_graphs.pdf", plot=time_graphs, 
       width=180, height=140, units="mm", dpi=300)


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
