# Stats for overall confidence by how many correct identifications of the
# echograms

path <- "~/Desktop/DO NOT ERASE/1NUIG/Mackerel/Mackerel Data/Survey Data/"  # for laptop

library(tidyverse)

overall_score <- read.csv(paste0(path, "correct_confidence.csv"))

# Select only the score (both completely & partially correct) & coded confidence
score_conf <- overall_score %>% select(overall, code)

# Split between
low_score <- score_conf[1:59,]
high_score <- score_conf[60:118,]

# Test to see if there's a difference in confidence btw low & high scoring
wilcox.test(low_score$code, high_score$code)

# Test correlation between score & confidence
cor(score_conf, method = "spearman")
