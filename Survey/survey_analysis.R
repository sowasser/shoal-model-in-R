# Analysis of survey acoustic species identification between experts and 
# non-experts & plots for publication


library(reshape2)
library(ggplot2)
library(viridis)

path <- "~/Desktop/DO NOT ERASE/1NUIG/Mackerel/Mackerel Data/Survey Data/"  # for laptop

# Plot of confidence in species ID for experts and non-experts ----------------
confidence <- read.csv(paste0(path, "confidence.csv"))
confidence <- confidence[, 1:4]  # Drop weird extra column
colnames(confidence) <- c("echogram", "confidence", "expert", "non-expert")  # Re-name columns to work for graph 

con <- melt(confidence, id=c("echogram", "confidence"))  # Reshape dataframe for ggplot
colnames(con) <- c("echogram", "confidence", "expertise", "value")

# Set order of echograms to show correctly in ggplot
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


# Plot of methods used for identification -------------------------------------
method <- read.csv(paste0(path, "method.csv"))
colnames(method) <- c("method", "expert", "non-expert")

meth <- melt(method)  # Reshape by expertise for ggplot
colnames(meth) <- c("method", "expertise", "value")

# Set order of methods to match survey
meth$method <- factor(meth$method, levels = c("school size", "school shape", 
                                              "school depth", "backscatter strength"))

meth_graph <- ggplot(meth, aes(fill = expertise, y = value, x = method)) +
  geom_bar(position = "dodge", stat = "identity") +
  ylab(" ") +
  guides(fill=guide_legend(title=" ")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave(filename="~/Desktop/method.pdf", meth_graph,
       width=150, height=120, units="mm", dpi=300)