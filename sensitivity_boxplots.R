# Soecifically the boxplots for variations in model parameters (from 
# data_sensitivity.py in the Python shoaling model repository.)

# Colors for ICES poster:
# Pink: #C830CC
# Purple: "#8971E1"
# Green: "#92D050"
# Background: "#27282E

library(ggplot2)

# path <- "~/Desktop/DO NOT ERASE/1NUIG/Mackerel/Mackerel Data/"  # for laptop
path <- "~/Desktop/Local/Mackerel/Mackerel Data/"  # for desktop

# multi_path <- "~/Desktop/DO NOT ERASE/1NUIG/Mackerel/Mackerel Data/"  # Path for multiplot function on laptop
multi_path <- "~/Desktop/Local/Mackerel/shoal-model-in-R/"  # Path for multiplot function on desktop


source(paste0(multi_path,"multiplot.R"))

# Read in data ----------------------------------------------------------------
speed <- read.csv(paste0(path,"var-speed.csv"))
separation <- read.csv(paste0(path,"var-sep.csv"))
vision <- read.csv(paste0(path,"var-vision.csv"))

columns <- c("x", "centroid", "nnd", "polar", "area", "speed", "vision", "separation")

colnames(speed) <- columns
colnames(separation) <- columns
colnames(vision) <- columns

# Boxplots for varying speed over all steps & runs ----------------------------
polar_speed <- ggplot() + 
  theme_classic() +
  geom_boxplot(data = speed, aes(x=speed, y=polar, group=speed), # boxplot
               colour="white",  fill="#C830CC", width=3) +  # line & box fill colors; box width
  scale_x_continuous(name="speed", breaks = c(0.1, 0.5, 2, 5, 8)) +  # set x axis labels to exact points
  theme(axis.text.y = element_text(size = 14, color = "white"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "white")) + 
  xlab("speed") + ylab("Mean Polarization") +  # axis labels
  theme(plot.title = element_text(size = 16, face = "bold")) +  # title formatting
  theme(text = element_text(colour = "white", size = 16, face = "bold")) + # label text
  theme(axis.line = element_line(color="white", size = 1),  # axis line & tick color
        axis.ticks = element_line(color="white")) +
  theme(panel.background = element_rect(fill = "#27282E"),  # plot & background colors, no outline
        plot.background = element_rect(fill = "#27282E", size = 0))

nnd_speed <- ggplot() + 
  theme_classic() +
  geom_boxplot(data = speed, aes(x=speed, y=nnd, group=speed), # boxplot
               colour="white",  fill="#C830CC", width=3) +  # line & box fill colors; box width
  scale_x_continuous(name="speed", breaks = c(0.1, 0.5, 2, 5, 8)) +  # set x axis labels to exact points
  theme(axis.text.y = element_text(size = 14, color = "white"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "white")) + 
  ylab("Mean Nearest Neighbour Distance") +  # axis labels
  theme(plot.title = element_text(size = 16, face = "bold")) +  # title formatting
  theme(text = element_text(colour = "white", size = 16, face = "bold")) + # label text
  theme(axis.line = element_line(color="white", size = 1),  # axis line & tick color
        axis.ticks = element_line(color="white")) +
  theme(panel.background = element_rect(fill = "#27282E"),  # plot & background colors, no outline
        plot.background = element_rect(fill = "#27282E", size = 0))

area_speed <- ggplot() + 
  theme_classic() +
  geom_boxplot(data = speed, aes(x=speed, y=area, group=speed), # boxplot
               colour="white",  fill="#C830CC", width=3) +  # line & box fill colors; box width
  scale_x_continuous(name="speed", breaks = c(0.1, 0.5, 2, 5, 8)) +  # set x axis labels to exact points
  theme(axis.text.y = element_text(size = 14, color = "white"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "white")) + 
  ylab("Mean Shoal Area") +  # axis labels
  theme(plot.title = element_text(size = 16, face = "bold")) +  # title formatting
  theme(text = element_text(colour = "white", size = 16, face = "bold")) + # label text
  theme(axis.line = element_line(color="white", size = 1),  # axis line & tick color
        axis.ticks = element_line(color="white")) +
  theme(panel.background = element_rect(fill = "#27282E"),  # plot & background colors, no outline
        plot.background = element_rect(fill = "#27282E", size = 0))

cent_speed <- ggplot() + 
  theme_classic() +
  geom_boxplot(data = speed, aes(x=speed, y=centroid, group=speed), # boxplot
               colour="white",  fill="#C830CC", width=3) +  # line & box fill colors; box width
  scale_x_continuous(name="speed", breaks = c(0.1, 0.5, 2, 5, 8)) +  # set x axis labels to exact points
  theme(axis.text.y = element_text(size = 14, color = "white"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "white")) + 
  ylab("Mean Distance from Centroid") +  # axis labels
  theme(plot.title = element_text(size = 16, face = "bold")) +  # title formatting
  theme(text = element_text(colour = "white", size = 16, face = "bold")) + # label text
  theme(axis.line = element_line(color="white", size = 1),  # axis line & tick color
        axis.ticks = element_line(color="white")) +
  theme(panel.background = element_rect(fill = "#27282E"),  # plot & background colors, no outline
        plot.background = element_rect(fill = "#27282E", size = 0))


# Boxplots for varying separation distance over all steps & runs --------------
polar_sep <- ggplot() + 
  theme_classic() +
  geom_boxplot(data = separation, aes(x=separation, y=polar, group=separation), # boxplot
               colour="white",  fill="#8971E1", width=3) +  # line & box fill colors; box width
  scale_x_continuous(name="separation", breaks = c(0.1, 0.5, 2, 5, 8)) +  # set x axis labels to exact points
  theme(axis.text.y = element_text(size = 14, color = "white"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "white")) + 
  ylab("Mean Polarization") +  # axis labels
  theme(plot.title = element_text(size = 16, face = "bold")) +  # title formatting
  theme(text = element_text(colour = "white", size = 16, face = "bold")) + # label text
  theme(axis.line = element_line(color="white", size = 1),  # axis line & tick color
        axis.ticks = element_line(color="white")) +
  theme(panel.background = element_rect(fill = "#27282E"),  # plot & background colors, no outline
        plot.background = element_rect(fill = "#27282E", size = 0))

nnd_sep <- ggplot() + 
  theme_classic() +
  geom_boxplot(data = separation, aes(x=separation, y=nnd, group=separation), # boxplot
               colour="white",  fill="#8971E1", width=3) +  # line & box fill colors; box width
  scale_x_continuous(name="separation", breaks = c(0.1, 0.5, 2, 5, 8)) +  # set x axis labels to exact points
  theme(axis.text.y = element_text(size = 14, color = "white"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "white")) + 
  ylab("Mean Nearest Neighbour Distance") +  # axis labels
  theme(plot.title = element_text(size = 16, face = "bold")) +  # title formatting
  theme(text = element_text(colour = "white", size = 16, face = "bold")) + # label text
  theme(axis.line = element_line(color="white", size = 1),  # axis line & tick color
        axis.ticks = element_line(color="white")) +
  theme(panel.background = element_rect(fill = "#27282E"),  # plot & background colors, no outline
        plot.background = element_rect(fill = "#27282E", size = 0))

area_sep <- ggplot() + 
  theme_classic() +
  geom_boxplot(data = separation, aes(x=separation, y=area, group=separation), # boxplot
               colour="white",  fill="#8971E1", width=3) +  # line & box fill colors; box width
  scale_x_continuous(name="separation", breaks = c(0.1, 0.5, 2, 5, 8)) +  # set x axis labels to exact points
  theme(axis.text.y = element_text(size = 14, color = "white"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "white")) + 
  ylab("Mean Shoal Area") +  # axis labels
  theme(plot.title = element_text(size = 16, face = "bold")) +  # title formatting
  theme(text = element_text(colour = "white", size = 16, face = "bold")) + # label text
  theme(axis.line = element_line(color="white", size = 1),  # axis line & tick color
        axis.ticks = element_line(color="white")) +
  theme(panel.background = element_rect(fill = "#27282E"),  # plot & background colors, no outline
        plot.background = element_rect(fill = "#27282E", size = 0))

cent_sep <- ggplot() + 
  theme_classic() +
  geom_boxplot(data = separation, aes(x=separation, y=centroid, group=separation), # boxplot
               colour="white",  fill="#8971E1", width=3) +  # line & box fill colors; box width
  scale_x_continuous(name="separation", breaks = c(0.1, 0.5, 2, 5, 8)) +  # set x axis labels to exact points
  theme(axis.text.y = element_text(size = 14, color = "white"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "white")) + 
  ylab("Mean Distance from Centroid") +  # axis labels
  theme(plot.title = element_text(size = 16, face = "bold")) +  # title formatting
  theme(text = element_text(colour = "white", size = 16, face = "bold")) + # label text
  theme(axis.line = element_line(color="white", size = 1),  # axis line & tick color
        axis.ticks = element_line(color="white")) +
  theme(panel.background = element_rect(fill = "#27282E"),  # plot & background colors, no outline
        plot.background = element_rect(fill = "#27282E", size = 0))


# Boxplots for varying vision radius over all steps & runs --------------------
polar_vision <- ggplot() + 
  theme_classic() +
  geom_boxplot(data = vision, aes(x=vision, y=polar, group=vision), # boxplot
               colour="white",  fill="#92D050", width=3) +  # line & box fill colors; box width
  scale_x_continuous(name="vision", breaks = c(1, 5, 10, 15, 20)) +  # set x axis labels to exact points
  theme(axis.text.y = element_text(size = 14, color = "white"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "white")) + 
  xlab("vision") + ylab("Mean Polarization") +  # axis labels
  theme(plot.title = element_text(size = 16, face = "bold")) +  # title formatting
  theme(text = element_text(colour = "white", size = 16, face = "bold")) + # label text
  theme(axis.line = element_line(color="white", size = 1),  # axis line & tick color
        axis.ticks = element_line(color="white")) +
  theme(panel.background = element_rect(fill = "#27282E"),  # plot & background colors, no outline
        plot.background = element_rect(fill = "#27282E", size = 0))

nnd_vision <- ggplot() + 
  theme_classic() +
  geom_boxplot(data = vision, aes(x=vision, y=nnd, group=vision), # boxplot
               colour="white",  fill="#92D050", width=3) +  # line & box fill colors; box width
  scale_x_continuous(name="vision", breaks = c(1, 5, 10, 15, 20)) +  # set x axis labels to exact points
  theme(axis.text.y = element_text(size = 14, color = "white"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "white")) + 
  ylab("Mean Nearest Neighbour Distance") +  # axis labels
  theme(plot.title = element_text(size = 16, face = "bold")) +  # title formatting
  theme(text = element_text(colour = "white", size = 16, face = "bold")) + # label text
  theme(axis.line = element_line(color="white", size = 1),  # axis line & tick color
        axis.ticks = element_line(color="white")) +
  theme(panel.background = element_rect(fill = "#27282E"),  # plot & background colors, no outline
        plot.background = element_rect(fill = "#27282E", size = 0))

area_vision <- ggplot() + 
  theme_classic() +
  geom_boxplot(data = vision, aes(x=vision, y=area, group=vision), # boxplot
               colour="white",  fill="#92D050", width=3) +  # line & box fill colors; box width
  scale_x_continuous(name="vision", breaks = c(1, 5, 10, 15, 20)) +  # set x axis labels to exact points
  theme(axis.text.y = element_text(size = 14, color = "white"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "white")) + 
  ylab("Mean Shoal Area") +  # axis labels
  theme(plot.title = element_text(size = 16, face = "bold")) +  # title formatting
  theme(text = element_text(colour = "white", size = 16, face = "bold")) + # label text
  theme(axis.line = element_line(color="white", size = 1),  # axis line & tick color
        axis.ticks = element_line(color="white")) +
  theme(panel.background = element_rect(fill = "#27282E"),  # plot & background colors, no outline
        plot.background = element_rect(fill = "#27282E", size = 0))

cent_vision <- ggplot() + 
  theme_classic() +
  geom_boxplot(data = vision, aes(x=vision, y=centroid, group=vision), # boxplot
               colour="white",  fill="#92D050", width=3) +  # line & box fill colors; box width
  scale_x_continuous(name="vision", breaks = c(1, 5, 10, 15, 20)) +  # set x axis labels to exact points
  theme(axis.text.y = element_text(size = 14, color = "white"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "white")) + 
  ylab("Mean Distance from Centroid") +  # axis labels
  theme(plot.title = element_text(size = 16, face = "bold")) +  # title formatting
  theme(text = element_text(colour = "white", size = 16, face = "bold")) + # label text
  theme(axis.line = element_line(color="white", size = 1),  # axis line & tick color
        axis.ticks = element_line(color="white")) +
  theme(panel.background = element_rect(fill = "#27282E"),  # plot & background colors, no outline
        plot.background = element_rect(fill = "#27282E", size = 0))

# Call multiplot function to combine graphs -----------------------------------
png("~/Desktop/sensitivity_boxplots.png", width = 22, height = 18, units = 'in', res = 300)
multiplot(polar_speed, nnd_speed, cent_speed, area_speed,  # multiplot fills by column
          polar_sep, nnd_sep, cent_sep, area_sep, 
          polar_vision, nnd_vision, cent_vision, area_vision, cols=3)
dev.off()
