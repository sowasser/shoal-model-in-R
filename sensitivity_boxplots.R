# Soecifically the boxplots for variations in model parameters (from 
# data_sensitivity.py in the Python shoaling model repository.)

# Colors for ICES poster:
# Pink: #C830CC
# Purple: "#8971E1"
# Green: "#92D050"
# Background: "#27282E


# path <- "~/Desktop/DO NOT ERASE/1NUIG/Mackerel/Mackerel Data/"  # for laptop
path <- "~/Desktop/Local/Mackerel/Mackerel Data/"  # for desktop
multi_path <- "~/Desktop/Local/Mackerel/shoal-model-in-R/"  # Path for multiplot function on desktop

source(paste0(multi_path,"multiplot.R"))

# Read in data ----------------------------------------------------------------
var_speed <- read.csv(paste0(path,"means_var-speed.csv"))
var_sep <- read.csv(paste0(path,"means_var-sep.csv"))
var_vision <- read.csv(paste0(path,"means_var-vision.csv"))

# Boxplots for varying speed over all steps & runs ----------------------------
polar_speed <- ggplot() + 
  theme_classic() +
  geom_boxplot(data = var_speed, aes(x=var, y=polar, group=var), # boxplot
               colour="white",  fill="#C830CC", width=4) +  # line & box fill colors; box width
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
  geom_boxplot(data = var_speed, aes(x=var, y=nnd, group=var), # boxplot
               colour="white",  fill="#C830CC", width=4) +  # line & box fill colors; box width
  theme(axis.text.y = element_text(size = 14, color = "white"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "white")) + 
  xlab("speed") + ylab("Mean Nearest Neighbour Distance") +  # axis labels
  theme(plot.title = element_text(size = 16, face = "bold")) +  # title formatting
  theme(text = element_text(colour = "white", size = 16, face = "bold")) + # label text
  theme(axis.line = element_line(color="white", size = 1),  # axis line & tick color
        axis.ticks = element_line(color="white")) +
  theme(panel.background = element_rect(fill = "#27282E"),  # plot & background colors, no outline
        plot.background = element_rect(fill = "#27282E", size = 0))

area_speed <- ggplot() + 
  theme_classic() +
  geom_boxplot(data = var_speed, aes(x=var, y=area, group=var), # boxplot
               colour="white",  fill="#C830CC", width=4) +  # line & box fill colors; box width
  theme(axis.text.y = element_text(size = 14, color = "white"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "white")) + 
  xlab("speed") + ylab("Mean Shoal Area") +  # axis labels
  theme(plot.title = element_text(size = 16, face = "bold")) +  # title formatting
  theme(text = element_text(colour = "white", size = 16, face = "bold")) + # label text
  theme(axis.line = element_line(color="white", size = 1),  # axis line & tick color
        axis.ticks = element_line(color="white")) +
  theme(panel.background = element_rect(fill = "#27282E"),  # plot & background colors, no outline
        plot.background = element_rect(fill = "#27282E", size = 0))

cent_speed <- ggplot() + 
  theme_classic() +
  geom_boxplot(data = var_speed, aes(x=var, y=centroid, group=var), # boxplot
               colour="white",  fill="#C830CC", width=4) +  # line & box fill colors; box width
  theme(axis.text.y = element_text(size = 14, color = "white"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "white")) + 
  xlab("speed") + ylab("Mean Distance from Centroid") +  # axis labels
  theme(plot.title = element_text(size = 16, face = "bold")) +  # title formatting
  theme(text = element_text(colour = "white", size = 16, face = "bold")) + # label text
  theme(axis.line = element_line(color="white", size = 1),  # axis line & tick color
        axis.ticks = element_line(color="white")) +
  theme(panel.background = element_rect(fill = "#27282E"),  # plot & background colors, no outline
        plot.background = element_rect(fill = "#27282E", size = 0))


# Boxplots for varying separation distance over all steps & runs --------------
polar_sep <- ggplot() + 
  theme_classic() +
  geom_boxplot(data = var_sep, aes(x=var, y=polar, group=var), # boxplot
               colour="white",  fill="#8971E1", width=4) +  # line & box fill colors; box width
  theme(axis.text.y = element_text(size = 14, color = "white"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "white")) + 
  xlab("separation") + ylab("Mean Polarization") +  # axis labels
  theme(plot.title = element_text(size = 16, face = "bold")) +  # title formatting
  theme(text = element_text(colour = "white", size = 16, face = "bold")) + # label text
  theme(axis.line = element_line(color="white", size = 1),  # axis line & tick color
        axis.ticks = element_line(color="white")) +
  theme(panel.background = element_rect(fill = "#27282E"),  # plot & background colors, no outline
        plot.background = element_rect(fill = "#27282E", size = 0))

nnd_sep <- ggplot() + 
  theme_classic() +
  geom_boxplot(data = var_sep, aes(x=var, y=nnd, group=var), # boxplot
               colour="white",  fill="#8971E1", width=4) +  # line & box fill colors; box width
  theme(axis.text.y = element_text(size = 14, color = "white"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "white")) + 
  xlab("separation") + ylab("Mean Nearest Neighbour Distance") +  # axis labels
  theme(plot.title = element_text(size = 16, face = "bold")) +  # title formatting
  theme(text = element_text(colour = "white", size = 16, face = "bold")) + # label text
  theme(axis.line = element_line(color="white", size = 1),  # axis line & tick color
        axis.ticks = element_line(color="white")) +
  theme(panel.background = element_rect(fill = "#27282E"),  # plot & background colors, no outline
        plot.background = element_rect(fill = "#27282E", size = 0))

area_sep <- ggplot() + 
  theme_classic() +
  geom_boxplot(data = var_sep, aes(x=var, y=area, group=var), # boxplot
               colour="white",  fill="#8971E1", width=4) +  # line & box fill colors; box width
  theme(axis.text.y = element_text(size = 14, color = "white"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "white")) + 
  xlab("separation") + ylab("Mean Shoal Area") +  # axis labels
  theme(plot.title = element_text(size = 16, face = "bold")) +  # title formatting
  theme(text = element_text(colour = "white", size = 16, face = "bold")) + # label text
  theme(axis.line = element_line(color="white", size = 1),  # axis line & tick color
        axis.ticks = element_line(color="white")) +
  theme(panel.background = element_rect(fill = "#27282E"),  # plot & background colors, no outline
        plot.background = element_rect(fill = "#27282E", size = 0))

cent_sep <- ggplot() + 
  theme_classic() +
  geom_boxplot(data = var_sep, aes(x=var, y=centroid, group=var), # boxplot
               colour="white",  fill="#8971E1", width=4) +  # line & box fill colors; box width
  theme(axis.text.y = element_text(size = 14, color = "white"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "white")) + 
  xlab("separation") + ylab("Mean Distance from Centroid") +  # axis labels
  theme(plot.title = element_text(size = 16, face = "bold")) +  # title formatting
  theme(text = element_text(colour = "white", size = 16, face = "bold")) + # label text
  theme(axis.line = element_line(color="white", size = 1),  # axis line & tick color
        axis.ticks = element_line(color="white")) +
  theme(panel.background = element_rect(fill = "#27282E"),  # plot & background colors, no outline
        plot.background = element_rect(fill = "#27282E", size = 0))


# Boxplots for varying vision radius over all steps & runs --------------------
polar_vision <- ggplot() + 
  theme_classic() +
  geom_boxplot(data = var_vision, aes(x=var, y=polar, group=var), # boxplot
               colour="white",  fill="#92D050", width=4) +  # line & box fill colors; box width
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
  geom_boxplot(data = var_vision, aes(x=var, y=nnd, group=var), # boxplot
               colour="white",  fill="#92D050", width=4) +  # line & box fill colors; box width
  theme(axis.text.y = element_text(size = 14, color = "white"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "white")) + 
  xlab("vision") + ylab("Mean Nearest Neighbour Distance") +  # axis labels
  theme(plot.title = element_text(size = 16, face = "bold")) +  # title formatting
  theme(text = element_text(colour = "white", size = 16, face = "bold")) + # label text
  theme(axis.line = element_line(color="white", size = 1),  # axis line & tick color
        axis.ticks = element_line(color="white")) +
  theme(panel.background = element_rect(fill = "#27282E"),  # plot & background colors, no outline
        plot.background = element_rect(fill = "#27282E", size = 0))

area_vision <- ggplot() + 
  theme_classic() +
  geom_boxplot(data = var_vision, aes(x=var, y=area, group=var), # boxplot
               colour="white",  fill="#92D050", width=4) +  # line & box fill colors; box width
  theme(axis.text.y = element_text(size = 14, color = "white"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "white")) + 
  xlab("vision") + ylab("Mean Shoal Area") +  # axis labels
  theme(plot.title = element_text(size = 16, face = "bold")) +  # title formatting
  theme(text = element_text(colour = "white", size = 16, face = "bold")) + # label text
  theme(axis.line = element_line(color="white", size = 1),  # axis line & tick color
        axis.ticks = element_line(color="white")) +
  theme(panel.background = element_rect(fill = "#27282E"),  # plot & background colors, no outline
        plot.background = element_rect(fill = "#27282E", size = 0))

cent_vision <- ggplot() + 
  theme_classic() +
  geom_boxplot(data = var_vision, aes(x=var, y=centroid, group=var), # boxplot
               colour="white",  fill="#92D050", width=4) +  # line & box fill colors; box width
  theme(axis.text.y = element_text(size = 14, color = "white"),  # axis text size & color
        axis.text.x = element_text(size = 14, color = "white")) + 
  xlab("vision") + ylab("Mean Distance from Centroid") +  # axis labels
  theme(plot.title = element_text(size = 16, face = "bold")) +  # title formatting
  theme(text = element_text(colour = "white", size = 16, face = "bold")) + # label text
  theme(axis.line = element_line(color="white", size = 1),  # axis line & tick color
        axis.ticks = element_line(color="white")) +
  theme(panel.background = element_rect(fill = "#27282E"),  # plot & background colors, no outline
        plot.background = element_rect(fill = "#27282E", size = 0))

# Call multiplot function to combine graphs -----------------------------------
png("~/Desktop/sensitivity_boxplots.png", width = 22, height = 18, units = 'in', res = 300)
multiplot(polar_speed, nnd_speed, area_speed, cent_speed,  # multiplot fills by column
          polar_sep, nnd_sep, area_sep, cent_sep,
          polar_vision, nnd_vision, area_vision, cent_vision, cols=3)
dev.off()