# Analysis of survey of acoustic experts & plots for publication

library(reshape2)
library(ggplot2)
library(viridis)

path <- "~/Desktop/DO NOT ERASE/1NUIG/Mackerel/Mackerel Data/Survey Data/"  # for laptop

# Plot of confidence in species ID for experts and non-experts
confidence <- read.csv(paste0(path, "confidence.csv"))
confidence <- confidence[, 1:4]  # Drop weird extra column
colnames(confidence) <- c("echogram", "confidence", "expert", "non-expert")  # Re-name columns to work for graph 

con <- melt(confidence, id=c("echogram", "confidence"))  # Reshape dataframe for ggplot
colnames(con) <- c("echogram", "confidence", "expertise", "value")

con$echogram <- factor(con$echogram, levels = c("1", "2", "3", "4", "5", "6", 
                                                "7", "8", "9", "10", "overall"))

con_graph <- ggplot(con, aes(fill = expertise, y = value, x = confidence)) +
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(~echogram, scale = "free") +
  ylab(" ") +
  guides(fill=guide_legend(title=" ")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave(filename="~/Desktop/confidence.pdf", con_graph,
       width=200, height=140, units="mm", dpi=300)