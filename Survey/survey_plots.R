# Plots for analysis of survey acoustic species identification between experts
# and non-experts 

library(plyr)
library(reshape2)
library(ggplot2)
library(viridis)
library(treemapify)
library(ggalluvial)
library(ggrepel)

path <- "~/Desktop/DO NOT ERASE/1NUIG/Mackerel/Mackerel Data/Survey Data/"  # for laptop
color2 <- c("#440154", "#31688e")  # Nicer subset of viridis colors


# Alluvial plot of demographic data -------------------------------------------
demo2 <- read.csv(paste0(path, "demo_summary2.csv"))

demo2$level <- factor(demo2$level, 
                      levels = c("studied marine science",
                                 "employed in marine science",
                                 "participated in acoustic survey",
                                 "trained in analysis of acoustics",
                                 "experience in North Atlantic",
                                 "employed in commercial fisheries"))

demo_alluvial <- ggplot(data = demo2,
                        aes(axis1 = level, axis2 = expertise, y = count)) +
  theme_classic() + 
  theme(axis.text.x=element_text(size=12)) +
  scale_x_discrete(limits = c("experience", "self-identification"), expand = c(.05, .05)) +
  ylab("total count") +
  theme(legend.position = "none") +  # no legend
  geom_alluvium(aes(fill = level), width = 1/12) +
  guides(fill = FALSE) +
  geom_stratum(width = 1/12, fill = "grey", color = "white") +
  geom_label_repel(stat = "stratum", 
                   aes(label = after_stat(stratum)), 
                   label.padding = 0.2) +
  scale_fill_viridis(discrete = TRUE)

ggsave(filename="~/Desktop/demo_alluvial.pdf", demo_alluvial,
       width=270, height=170, units="mm", dpi=300)


# Plot of number of correct answers for experts & non-experts -----------------
correct_answers <- read.csv(paste0(path, "correct_answers.csv"))
freq_correct <- count(correct_answers, c("overall", "expertise"))

correct_graph <- ggplot(data = freq_correct, aes(x = overall, y = freq, fill = expertise)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  scale_fill_manual(values = color2) +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")) +
  ylab("frequency") + xlab("number of correct identifications") +
  guides(fill=guide_legend(title=" ")) 
  
ggsave(filename="~/Desktop/correct_answers.pdf", correct_graph,
       width=180, height=150, units="mm", dpi=300)


# Plot of expert vs. non-expert identifications for each echogram -------------
expert <- read.csv(paste0(path, "expertise.csv"))
colnames(expert) <- c("echogram", "species", "expert", "non-expert", 
                      "local experience", "no experience")

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
  ylab("percent of answers") + xlab("species") +
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
  ylab("percent") +
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
  ylab("percent") +
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
  ylab("percent") +
  guides(fill=guide_legend(title=" ")) +
  scale_fill_manual(values = color2) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave(filename="~/Desktop/method.pdf", meth_graph,
       width=150, height=120, units="mm", dpi=300)