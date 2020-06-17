# Script for timeseries analysis of ideal model (with parameter inputs gleaned
# from ABC) and real fish.

path <- "~/Desktop/DO NOT ERASE/1NUIG/Mackerel/Mackerel Data/"  # for laptop

tracking <- read.csv(paste0(path, "stepwise_data.csv"))

step_means <- read.csv(paste0(path, "step_means_17June.csv"))
step_means <- step_means[1:99,]  # only keep first 99 steps so same length as tracking

steps <- c(1:99)  

# Reformat and stack to make graphs
tp <- cbind(steps, tracking$polar, rep("polarization", length(tracking$polar)))
tn <- cbind(steps, tracking$nnd, rep("nearest neighbour distance", length(tracking$nnd)))
tc <- cbind(steps, tracking$centroid, rep("distance from centroid", length(tracking$centroid)))
ta <- cbind(steps, tracking$area, rep("shoal area", length(tracking$area)))

track_all <- cbind(rbind(tp, tn, tc, ta), rep("tracked", length(396)))

mp <- cbind(steps, step_means$polar, rep("polarization", length(step_means$polar)))  
mn <- cbind(steps, step_means$nnd, rep("nearest neighbour distance", length(step_means$nnd)))
mc <- cbind(steps, step_means$centroid, rep("distance from centroid", length(step_means$centroid)))
ma <- cbind(steps, step_means$area, rep("shoal area", length(step_means$area)))

model_all <- cbind(rbind(mp, mn, mc, ma), rep("modelled", length(396)))

timeseries_all <- as.data.frame(rbind(track_all, model_all))
colnames(timeseries_all) <- c("step", "value", "statistic", "source")
timeseries_all$value <- as.numeric(as.character(timeseries_all$value))
timeseries_all$step <- as.numeric(as.character(timeseries_all$step))

# Graphs of timeseries
time_graphs <- ggplot() + 
  theme_bw() + 
  geom_line(data=timeseries_all, aes(x=step, y=value, color = source), size = 0.5) +  # line
  xlab("step") +
  ylab("statistic") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~statistic, scale="free")

pdf("~/Desktop/timeseries_graphs.pdf")
print(time_graphs)
dev.off()