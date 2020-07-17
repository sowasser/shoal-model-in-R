# Script for timeseries analysis of ideal model (with parameter inputs gleaned
# from ABC) and real fish.

library(ggplot2)
library(ggcorrplot)

color2_2 <- c("#440154", "#29788E")

path <- "~/Desktop/DO NOT ERASE/1NUIG/Mackerel/Mackerel Data/"  # for laptop

tracking <- read.csv(paste0(path, "stepwise_data_scaled.csv"))

model_run_all <- read.csv(paste0(path, "single_run.csv"))


# Timeseries ------------------------------------------------------------------
model_run <- model_run_all[100:198,]  # only keep 99 steps so same length as tracking

steps <- c(1:99)  

# Reformat and stack to make graphs
tp <- cbind(steps, tracking$polar, rep("polarization", length(tracking$polar)))
tn <- cbind(steps, tracking$nnd, rep("nearest neighbour distance", length(tracking$nnd)))
tc <- cbind(steps, tracking$cent, rep("distance from centroid", length(tracking$cent)))
ta <- cbind(steps, tracking$area, rep("shoal area", length(tracking$area)))

track_all <- cbind(rbind(tp, tn, tc, ta), rep("tracked", length(396)))

mp <- cbind(steps, model_run$polar, rep("polarization", length(model_run$polar)))  
mn <- cbind(steps, model_run$nnd, rep("nearest neighbour distance", length(model_run$nnd)))
mc <- cbind(steps, model_run$cent, rep("distance from centroid", length(model_run$cent)))
ma <- cbind(steps, model_run$area, rep("shoal area", length(model_run$area)))

model_all <- cbind(rbind(mp, mn, mc, ma), rep("modelled", length(396)))

timeseries_all <- as.data.frame(rbind(track_all, model_all))
colnames(timeseries_all) <- c("step", "value", "statistic", "source")
timeseries_all$value <- as.numeric(as.character(timeseries_all$value))
timeseries_all$step <- as.numeric(as.character(timeseries_all$step))

# Graphs of timeseries
time_graphs <- ggplot() + 
  theme_bw() + 
  scale_color_manual(values = color2_2) +
  geom_line(data=timeseries_all, aes(x=step, y=value, color = source), size = 0.5) +  # line
  xlab("step") +
  ylab("statistic") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~statistic, scale="free")

pdf("~/Desktop/timeseries_graphs.pdf")
print(time_graphs)
dev.off()


# Correlations ----------------------------------------------------------------
stats <- model_run_all[, -1]
model_cor <- cor(stats)
ggcorrplot(model_cor, type = "lower", lab = TRUE, colors = c("#79D151", "white", "#29788E"))
