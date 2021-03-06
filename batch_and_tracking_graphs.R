# This script combines the batch graphs and the tracking graphs for 
# presentations and the like.

# Colors for ICES poster:
# Pink: #C830CC
# Purple: "#8971E1"
# Green: "#92D050"
# Background: "#27282E"

library(ggplot2)
library(foreach)

# path <- "~/Desktop/DO NOT ERASE/1NUIG/Mackerel/Mackerel Data/"  # for laptop
path <- "~/Desktop/Local/Mackerel/Mackerel Data/"  # for desktop
multi_path <- "~/Desktop/Local/Mackerel/shoal-model-in-R/"  # Path for multiplot function on desktop

source(paste0(multi_path,"multiplot.R"))

# Colors ----------------------------------------------------------------------
bkgrnd = "#27282E"
c1 = "#C830CC"
c2 = "#8971E1"
c3 = "#92D050"

# Read in data ----------------------------------------------------------------
step_means <- read.csv(paste0(path,"step_means.csv"))
tracking <- read.csv(paste0(path, "stepwise_data.csv"))

# Run means for every step of the models --------------------------------------

polar_runs <- ggplot() + 
  theme_classic() + 
  geom_line(data=step_means, aes(x=X, y=polar), colour=c1, size = 1) +  # line
  theme(axis.text.y = element_text(size = 14, color = "white"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "white")) + 
  xlab("step") + ylab("Mean Polarization") +  # axis labels
  theme(plot.title = element_text(size = 16, face = "bold")) +  # title formatting
  theme(text = element_text(colour = "white", size = 16, face = "bold"))  + # label text
  theme(axis.line = element_line(color="white", size = 1),  # axis line & tick color
        axis.ticks = element_line(color="white")) +
  theme(panel.background = element_rect(fill = bkgrnd),  # plot & background colors, no outline
        plot.background = element_rect(fill = bkgrnd, size = 0))

nnd_runs <- ggplot() + 
  theme_classic() + 
  geom_line(data=step_means, aes(x=X, y=nnd), colour=c1, size = 1) +  # line
  theme(axis.text.y = element_text(size = 14, color = "white"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "white")) + 
  xlab("step") + ylab("Mean Nearest Neighbour Distance") +  # axis labels
  theme(plot.title = element_text(size = 16, face = "bold")) +  # title formatting
  theme(text = element_text(colour = "white", size = 16, face = "bold")) + # label text
  theme(axis.line = element_line(color="white", size = 1), # axis line & tick color
        axis.ticks = element_line(color="white")) +
  theme(panel.background = element_rect(fill = bkgrnd),  # plot & background colors, no outline
        plot.background = element_rect(fill = bkgrnd, size = 0))

area_runs <- ggplot() + 
  theme_classic() + 
  geom_line(data=step_means, aes(x=X, y=area), colour=c1, size = 1) +  # line
  theme(axis.text.y = element_text(size = 14, color = "white"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "white")) + 
  xlab("step") + ylab("Mean Shoal Area") +  # axis labels
  theme(plot.title = element_text(size = 16, face = "bold")) +  # title formatting
  theme(text = element_text(colour = "white", size = 16, face = "bold"))  + # label text
  theme(axis.line = element_line(color="white", size = 1),  # axis line & tick color
        axis.ticks = element_line(color="white")) +
  theme(panel.background = element_rect(fill = bkgrnd),  # plot & background colors, no outline
        plot.background = element_rect(fill = bkgrnd, size = 0))

cent_runs <- ggplot() + 
  theme_classic() + 
  geom_line(data=step_means, aes(x=X, y=centroid), colour=c1, size = 1) +  # line
  theme(axis.text.y = element_text(size = 14, color = "white"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "white")) + 
  xlab("step") + ylab("Mean Distance from Centroid") +  # axis labels
  theme(plot.title = element_text(size = 16, face = "bold")) +  # title formatting
  theme(text = element_text(colour = "white", size = 16, face = "bold")) + # label text
  theme(axis.line = element_line(color="white", size = 1),  # axis line & tick color
        axis.ticks = element_line(color="white")) +
  theme(panel.background = element_rect(fill = bkgrnd),  # plot & background colors, no outline
        plot.background = element_rect(fill = bkgrnd, size = 0))


# Tracking graphs -------------------------------------------------------------

track_polar <- ggplot() + 
  theme_classic() + 
  geom_line(data=tracking, aes(x=X, y=polar), colour=c2, size = 1) +  # line
  theme(axis.text.y = element_text(size = 14, color = "white"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "white")) + 
  xlab("step") + ylab("Polarization") +  # axis labels
  theme(plot.title = element_text(size = 16, face = "bold")) +  # title formatting
  theme(text = element_text(colour = "white", size = 16, face = "bold"))  + # label text
  theme(axis.line = element_line(color="white", size = 1),  # axis line & tick color
        axis.ticks = element_line(color="white")) +
  theme(panel.background = element_rect(fill = bkgrnd),  # plot & background colors, no outline
        plot.background = element_rect(fill = bkgrnd, size = 0))

track_nnd <- ggplot() + 
  theme_classic() + 
  geom_line(data=tracking, aes(x=X, y=nnd), colour=c2, size = 1) +  # line
  theme(axis.text.y = element_text(size = 14, color = "white"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "white")) + 
  xlab("step") + ylab("Nearest Neighbour Distance") +  # axis labels
  theme(plot.title = element_text(size = 16, face = "bold")) +  # title formatting
  theme(text = element_text(colour = "white", size = 16, face = "bold")) + # label text
  theme(axis.line = element_line(color="white", size = 1), # axis line & tick color
        axis.ticks = element_line(color="white")) +
  theme(panel.background = element_rect(fill = bkgrnd),  # plot & background colors, no outline
        plot.background = element_rect(fill = bkgrnd, size = 0))

track_area <- ggplot() + 
  theme_classic() + 
  geom_line(data=tracking, aes(x=X, y=area), colour=c2, size = 1) +  # line
  theme(axis.text.y = element_text(size = 14, color = "white"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "white")) + 
  xlab("step") + ylab("Shoal Area") +  # axis labels
  theme(plot.title = element_text(size = 16, face = "bold")) +  # title formatting
  theme(text = element_text(colour = "white", size = 16, face = "bold"))  + # label text
  theme(axis.line = element_line(color="white", size = 1),  # axis line & tick color
        axis.ticks = element_line(color="white")) +
  theme(panel.background = element_rect(fill = bkgrnd),  # plot & background colors, no outline
        plot.background = element_rect(fill = bkgrnd, size = 0))

track_cent <- ggplot() + 
  theme_classic() + 
  geom_line(data=tracking, aes(x=X, y=centroid), colour=c2, size = 1) +  # line
  theme(axis.text.y = element_text(size = 14, color = "white"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "white")) + 
  xlab("step") + ylab("Distance from Centroid") +  # axis labels
  theme(plot.title = element_text(size = 16, face = "bold")) +  # title formatting
  theme(text = element_text(colour = "white", size = 16, face = "bold")) + # label text
  theme(axis.line = element_line(color="white", size = 1),  # axis line & tick color
        axis.ticks = element_line(color="white")) +
  theme(panel.background = element_rect(fill = bkgrnd),  # plot & background colors, no outline
        plot.background = element_rect(fill = bkgrnd, size = 0))


# Boxplots for mean data collectors over all steps & runs ---------------------
# polar_box <- ggplot() + 
#   theme_classic() +
#   geom_boxplot(data = model_means, aes(x=var, y=polar, group=var), # boxplot
#                colour="white",  fill="#C830CC", width=4) +  # line & box fill colors; box width
#   theme(axis.text.y = element_text(size = 14, color = "white"),  # axis text size & color
#         axis.text.x = element_text(size = 14, color = "white")) + 
#   xlab("speed") + ylab("Mean Polarization") +  # axis labels
#   theme(plot.title = element_text(size = 16, face = "bold")) +  # title formatting
#   theme(text = element_text(colour = "white", size = 16, face = "bold")) + # label text
#   theme(axis.line = element_line(color="white", size = 1),  # axis line & tick color
#         axis.ticks = element_line(color="white")) +
#   theme(panel.background = element_rect(fill = "#27282E"),  # plot & background colors, no outline
#         plot.background = element_rect(fill = "#27282E", size = 0))
# 
# nnd_box <- ggplot() + 
#   theme_classic() +
#   geom_boxplot(data = model_means, aes(x=var, y=nnd, group=var), # boxplot
#                colour="white",  fill="#C830CC", width=4) +  # line & box fill colors; box width
#   theme(axis.text.y = element_text(size = 14, color = "white"),  # axis text size & color
#         axis.text.x = element_text(size = 14, color = "white")) + 
#   xlab("speed") + ylab("Mean Nearest Neighbour Distance") +  # axis labels
#   theme(plot.title = element_text(size = 16, face = "bold")) +  # title formatting
#   theme(text = element_text(colour = "white", size = 16, face = "bold")) + # label text
#   theme(axis.line = element_line(color="white", size = 1),  # axis line & tick color
#         axis.ticks = element_line(color="white")) +
#   theme(panel.background = element_rect(fill = "#27282E"),  # plot & background colors, no outline
#         plot.background = element_rect(fill = "#27282E", size = 0))
# 
# area_box <- ggplot() + 
#   theme_classic() +
#   geom_boxplot(data = model_means, aes(x=var, y=area, group=var), # boxplot
#                colour="white",  fill="#C830CC", width=4) +  # line & box fill colors; box width
#   theme(axis.text.y = element_text(size = 14, color = "white"),  # axis text size & color
#         axis.text.x = element_text(size = 14, color = "white")) + 
#   xlab("speed") + ylab("Mean Shoal Area") +  # axis labels
#   theme(plot.title = element_text(size = 16, face = "bold")) +  # title formatting
#   theme(text = element_text(colour = "white", size = 16, face = "bold")) + # label text
#   theme(axis.line = element_line(color="white", size = 1),  # axis line & tick color
#         axis.ticks = element_line(color="white")) +
#   theme(panel.background = element_rect(fill = "#27282E"),  # plot & background colors, no outline
#         plot.background = element_rect(fill = "#27282E", size = 0))
# 
# cent_box <- ggplot() + 
#   theme_classic() +
#   geom_boxplot(data = model_means, aes(x=var, y=centroid, group=var), # boxplot
#                colour="white",  fill="#C830CC", width=4) +  # line & box fill colors; box width
#   theme(axis.text.y = element_text(size = 14, color = "white"),  # axis text size & color
#         axis.text.x = element_text(size = 14, color = "white")) + 
#   xlab("speed") + ylab("Mean Distance from Centroid") +  # axis labels
#   theme(plot.title = element_text(size = 16, face = "bold")) +  # title formatting
#   theme(text = element_text(colour = "white", size = 16, face = "bold")) + # label text
#   theme(axis.line = element_line(color="white", size = 1),  # axis line & tick color
#         axis.ticks = element_line(color="white")) +
#   theme(panel.background = element_rect(fill = "#27282E"),  # plot & background colors, no outline
#         plot.background = element_rect(fill = "#27282E", size = 0))

# Call multiplot function to combine graphs -----------------------------------
png("~/Desktop/means_with_tracking.png", width = 16, height = 14, units = 'in', res = 300)
multiplot(polar_runs, nnd_runs, cent_runs, # multiplot fills by column
          track_polar, track_nnd, track_cent, cols=2)
dev.off()