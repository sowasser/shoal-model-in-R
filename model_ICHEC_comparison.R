# Comparing the ICHEC and local Python data for ABC

library(ggplot2)
library(gridExtra)

path <- "~/Desktop/DO NOT ERASE/1NUIG/Mackerel/Mackerel Data/"  # laptop
ICHEC_data <- read.csv(paste0(path,"ICHEC_data_19Mar20.csv"))

# Mean of all runs, calculated for every step of the model & changes in a variable
speed <- read.csv(paste0(path,"var-speed.csv"))
vision <- read.csv(paste0(path,"var-vision.csv"))
separation <- read.csv(paste0(path,"var-sep.csv"))

model <- rbind(speed, vision, separation)
colnames(model) <- c("polar", "nnd", "area", "centroid", "speed", "vision", "separation")


# Model Graphs ----------------------------------------------------------------
require(gridExtra)
# set "x" and "group" to the parameter you're looking at
# set "y" to the summary statistic you're looking at
# TODO: change this to something that works better - maybe facets?
speed_polar <- ggplot(data = model, aes(x=speed, y=polar)) + 
  theme_classic() + geom_point()
speed_nnd <- ggplot(data = model, aes(x=speed, y=nnd)) + 
  theme_classic() + geom_point()
speed_area <- ggplot(data = model, aes(x=speed, y=area)) + 
  theme_classic() + geom_point()
speed_cent <- ggplot(data = model, aes(x=speed, y=centroid)) + 
  theme_classic() + geom_point()

vision_polar <- ggplot(data = model, aes(x=vision, y=polar)) + 
  theme_classic() + geom_point()
vision_nnd <- ggplot(data = model, aes(x=vision, y=nnd)) + 
  theme_classic() + geom_point()
vision_area <- ggplot(data = model, aes(x=vision, y=area)) + 
  theme_classic() + geom_point()
vision_cent <- ggplot(data = model, aes(x=vision, y=centroid)) + 
  theme_classic() + geom_point()

sep_polar <- ggplot(data = model, aes(x=separation, y=polar)) + 
  theme_classic() + geom_point()
sep_nnd <- ggplot(data = model, aes(x=separation, y=nnd)) + 
  theme_classic() + geom_point()
sep_area <- ggplot(data = model, aes(x=separation, y=area)) + 
  theme_classic() + geom_point()
sep_cent <- ggplot(data = model, aes(x=separation, y=centroid)) + 
  theme_classic() + geom_point()


# Model Graphs ----------------------------------------------------------------
require(gridExtra)
# set "x" and "group" to the parameter you're looking at
# set "y" to the summary statistic you're looking at
# TODO: change this to something that works better - maybe facets?
speed_polar2 <- ggplot(data = ICHEC_data, aes(x=speed, y=polar)) + 
  theme_classic() + geom_point()
speed_nnd2 <- ggplot(data = ICHEC_data, aes(x=speed, y=nnd)) + 
  theme_classic() + geom_point()
speed_area2 <- ggplot(data = ICHEC_data, aes(x=speed, y=area)) + 
  theme_classic() + geom_point()
speed_cent2 <- ggplot(data = ICHEC_data, aes(x=speed, y=cent)) + 
  theme_classic() + geom_point()

vision_polar2 <- ggplot(data = ICHEC_data, aes(x=vision, y=polar)) + 
  theme_classic() + geom_point()
vision_nnd2 <- ggplot(data = ICHEC_data, aes(x=vision, y=nnd)) + 
  theme_classic() + geom_point()
vision_area2 <- ggplot(data = ICHEC_data, aes(x=vision, y=area)) + 
  theme_classic() + geom_point()
vision_cent2 <- ggplot(data = ICHEC_data, aes(x=vision, y=cent)) + 
  theme_classic() + geom_point()

sep_polar2 <- ggplot(data = ICHEC_data, aes(x=sep, y=polar)) + 
  theme_classic() + geom_point()
sep_nnd2 <- ggplot(data = ICHEC_data, aes(x=sep, y=nnd)) + 
  theme_classic() + geom_point()
sep_area2 <- ggplot(data = ICHEC_data, aes(x=sep, y=area)) + 
  theme_classic() + geom_point()
sep_cent2 <- ggplot(data = ICHEC_data, aes(x=sep, y=cent)) + 
  theme_classic() + geom_point()


# Create multiple graphs in page, row by row ----------------------------------
pdf("~/Desktop/model_distributions.pdf", width = 9, height = 12)
grid.arrange(speed_polar, vision_polar, sep_polar,
             speed_nnd, vision_nnd, sep_nnd,
             speed_area, vision_area, sep_area,
             speed_cent, vision_cent, sep_cent, ncol=3)
dev.off()

pdf("~/Desktop/ICHEC_distributions.pdf", width = 9, height = 12)
grid.arrange(speed_polar2, vision_polar2, sep_polar2,
             speed_nnd2, vision_nnd2, sep_nnd2,
             speed_area2, vision_area2, sep_area2,
             speed_cent2, vision_cent2, sep_cent2, ncol=3)
dev.off()


