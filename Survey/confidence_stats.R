# Stats for overall confidence by how many correct identifications of the
# echograms

path <- "~/Desktop/DO NOT ERASE/1NUIG/Mackerel/Mackerel Data/Survey Data/"  # for laptop

library(tidyverse)

overall_score <- read.csv(paste0(path, "correct_confidence.csv"))

# Select score (both completely & partially correct) & coded confidence -------
score_conf <- overall_score %>% select(overall, code)

# Test correlation between score & confidence
cor(score_conf, method = "spearman")  # 0.2469148

# Split between score levels (1-4, 5-8)
low_score <- score_conf[1:59,]
high_score <- score_conf[60:118,]

# Test to see if there's a difference in confidence btw low & high scoring
wilcox.test(low_score$code, high_score$code)  # p = 0.0175
mean(low_score$code)  # 1.79661
mean(high_score$code)  # 2.186441


# Select expertise & confidence -----------------------------------------------
expert_conf <- overall_score %>% select(expertise, code)

# Sort by expertise
expert_conf <- expert_conf[order(expert_conf$expertise), ]
row.names(expert_conf) <- NULL  # reset row index

# Split between expertise
expert <- expert_conf[1:92,]
nonexpert <- expert_conf[93:118,]

# Test for difference in confidence by expertise
wilcox.test(expert$code, nonexpert$code)  # p = 0.2102
mean(expert$code)  # 1.934783
mean(nonexpert$code)  # 2.192308
