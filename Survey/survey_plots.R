# Plots for analysis of survey acoustic species identification between experts
# and non-experts 

library(plyr)
library(reshape2)
library(ggplot2)
library(viridis)

path <- "~/Desktop/DO NOT ERASE/1NUIG/Mackerel/Mackerel Data/Survey Data/"  # for laptop
color2 <- c("#440154", "#31688e")  # Nicer subset of viridis colors


# Plot of demographic data for expert vs. non-expert --------------------------
demographics <- read.csv(paste0(path, "demo_summary.csv"))
demographics$level <- factor(demographics$level, 
                             levels = c("other", 
                                        "experience in North Atlantic",
                                        "trained in analysis of acoustics",
                                        "participated in acoustic survey",
                                        "employed in commercial fisheries",
                                        "employed in marine science",
                                        "studied marine science"))

demo_graph <- ggplot(data = demographics, aes(x = expertise, y = count, fill = level)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  scale_fill_viridis(discrete = TRUE) +
  ylab(" ") + xlab(" ") +
  guides(fill=guide_legend(title="experience type"))

ggsave(filename="~/Desktop/demographics.pdf", demo_graph,
       width=180, height=150, units="mm", dpi=300)


# Plot of identifications (hopefully with echograms alongside) ----------------
expert <- read.csv(paste0(path, "expertise.csv"))
colnames(expert) <- c("echogram", "species", "expert", "non-expert", 
                      "local experience", "no experience")

# Reshape dataframe for ggplot, ID are the columns you don't want to change!
exp <- melt(expert, id=c("echogram", "species")) 
colnames(exp) <- c("echogram", "species", "expertise", "percent")

# Set order of echograms to show correctly in ggplot
exp$echogram <- factor(exp$echogram, levels = c("1", "2", "3", "4", "5", "6", 
                                                "7", "8", "9", "10"))
exp$species <- factor(exp$species, levels = c("her", "spr", "bof", "mac", 
                                              "hom", "whb", "other"))

exp_graph_all <- ggplot(exp, aes(fill = expertise, y = percent, x = species)) +
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(~echogram, scale = "free", ncol=2) +
  ylab(" ") +
  ylim(0, 100) +  # set y limit to always be 100
  guides(fill=guide_legend(title="expertise level")) +
  scale_fill_viridis(discrete = TRUE) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave(filename="~/Desktop/expertise_all.pdf", exp_graph_all,
       width=250, height=250, units="mm", dpi=300)


# Graph of just expert & non-expert identification ----------------------------
sub_exp <- melt(expert[, 1:4], id=c("echogram", "species")) 
colnames(sub_exp) <- c("echogram", "species", "expertise", "percent")

# Set order of echograms to show correctly in ggplot
sub_exp$echogram <- factor(sub_exp$echogram, levels = c("1", "2", "3", "4", 
                                                        "5", "6", "7", "8", 
                                                        "9", "10"))
sub_exp$species <- factor(sub_exp$species, levels = c("her", "spr", "bof", 
                                                      "mac", "hom", "whb", 
                                                      "other"))

exp_graph <- ggplot(sub_exp, aes(fill = expertise, y = percent, x = species)) +
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(~echogram, scale = "free", ncol=2) +
  ylab(" ") + xlab(" ") +
  ylim(0, 100) +  # set y limit to always be 100
  guides(fill=guide_legend(title=" ")) +
  scale_fill_manual(values = color2) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave(filename="~/Desktop/identification.pdf", exp_graph,
       width=170, height=250, units="mm", dpi=300)


# Plot of overall confidence --------------------------------------------------
conf_overall <- read.csv(paste0(path, "conf_overall.csv"))
colnames(conf_overall) <- c("confidence", "expert", "non-expert", "low score", 
                            "high score")  # Re-name columns for graph

con.over <- melt(conf_overall, id=c("confidence"))  # Reshape dataframe for ggplot

con.over$variable <- factor(con.over$variable, levels = c("expert", "non-expert",
                                                          "high score", "low score"))

con.over_graph <- ggplot(con.over, aes(fill = variable, y = value, x = confidence)) +
  geom_bar(position = "dodge", stat = "identity") +
  ylab(" ") +
  ylim(0, 52) +  # set y limit to include all data but stop at 50
  guides(fill=guide_legend(title=" ")) +
  scale_fill_viridis(discrete = TRUE) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave(filename="~/Desktop/overall_confidence.pdf", con.over_graph,
       width=200, height=140, units="mm", dpi=300)


# Plot of confidence for experts and non-experts for all echograms ------------
confidence <- read.csv(paste0(path, "confidence.csv"))
confidence <- confidence[, 1:4]  # Drop weird extra column
colnames(confidence) <- c("echogram", "confidence", "expert", "non-expert")  # Re-name columns for graph 

con <- melt(confidence, id=c("echogram", "confidence"))  # Reshape dataframe for ggplot
colnames(con) <- c("echogram", "confidence", "expertise", "value")

# Set order of echograms to show correctly in ggplot
con$echogram <- factor(con$echogram, levels = c("1", "2", "3", "4", "5", "6", 
                                                "7", "8", "9", "10"))

con_graph <- ggplot(con, aes(fill = expertise, y = value, x = confidence)) +
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(~echogram, scale = "free", ncol=2) +
  ylab(" ") +
  ylim(0, 52) +  # set y limit to include all data but stop at 50
  guides(fill=guide_legend(title=" ")) +
  scale_fill_manual(values = color2) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave(filename="~/Desktop/confidence.pdf", con_graph,
       width=170, height=250, units="mm", dpi=300)


# Plot of methods used for identification -------------------------------------
method <- read.csv(paste0(path, "method.csv"))
colnames(method) <- c("method", "expert", "non-expert")

meth <- melt(method)  # Reshape by expertise for ggplot
colnames(meth) <- c("method", "expertise", "value")

# Set order of methods to match survey
meth$method <- factor(meth$method, levels = c("school size", 
                                              "school shape", 
                                              "school depth", 
                                              "backscatter strength"))

meth_graph <- ggplot(meth, aes(fill = expertise, y = value, x = method)) +
  geom_bar(position = "dodge", stat = "identity") +
  ylab(" ") +
  guides(fill=guide_legend(title=" ")) +
  scale_fill_manual(values = color2) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave(filename="~/Desktop/method.pdf", meth_graph,
       width=150, height=120, units="mm", dpi=300)