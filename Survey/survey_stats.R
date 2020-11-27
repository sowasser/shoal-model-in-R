# Analysis of the results of a survey of species identification from echograms
# and the success rate between experts and non-experts.

path <- "~/Desktop/DO NOT ERASE/1NUIG/Mackerel/Mackerel Data/Survey Data/"  # for laptop

library(tidyverse)
library(questionr)

# Odds ratio for expert/non-expert correct species ID -------------------------
correct <- read.csv(paste0(path, "raw_correct.csv"))

exp <- correct$expert.ratio
nonexp <- correct$non.expert.ratio
  
r1 <- odds.ratio(exp[1], nonexp[1])
r2 <- odds.ratio(exp[2], nonexp[2])
r3 <- odds.ratio(exp[3], nonexp[3])
r4 <- odds.ratio(exp[4], nonexp[4])
r5 <- odds.ratio(exp[5], nonexp[5])
r6 <- odds.ratio(exp[6], nonexp[6])
r7 <- odds.ratio(exp[7], nonexp[7])
r8 <- odds.ratio(exp[8], nonexp[8])
r9 <- odds.ratio(exp[9], nonexp[9])
r10 <- 0
r11 <- odds.ratio(exp[11], nonexp[11])
r12 <- odds.ratio(exp[12], nonexp[12])
r13 <- odds.ratio(exp[13], nonexp[13])

# Overall
ratios <- c(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13)

correct_ratios <- cbind(correct, ratios)

overall_mean <- mean(ratios)
overall_stdev <- sd(ratios)
t.test(ratios, mu = 1)  # p = 0.1051, set the mean at 1 instead of 0

# blue whiting
whb <- c(r2, r3, r7)
whb_mean <- mean(whb)
whb_stdev <- sd(whb)
t.test(whb, mu = 1)  # p = 0.01913

# boarfish 
bof <- c(r4, r11)
bof_mean <- mean(bof)
bof_stdev <- sd(bof)
t.test(bof, mu = 1)  # p = 0.3268

# Herring 
her <- c(r1, r5, r6, r9)
her_mean <- mean(her)
her_stdev <- sd(her)
t.test(her, mu = 1)  # p = 0.486

# Mackerel 
mac <- c(r10, r12)
mac_mean <- mean(mac)
mac_stdev <- sd(mac)
t.test(mac, mu = 1)  # p = 0.9097
