# Analysis of the results of a survey of species identification from echograms
# and the success rate between experts and non-experts.

path <- "~/Desktop/DO NOT ERASE/1NUIG/Mackerel/Mackerel Data/Survey Data/"  # for laptop

library(tidyverse)
library(questionr)

# Odds ratio for expert/non-expert correct species ID -------------------------
correct <- read.csv(paste0(path, "raw_correct.csv"))

exp <- correct$expert.ratio
nonexp <- correct$non.expert.ratio
  
exp_non.r1 <- odds.ratio(exp[1], nonexp[1])
exp_non.r2 <- odds.ratio(exp[2], nonexp[2])
exp_non.r3 <- odds.ratio(exp[3], nonexp[3])
exp_non.r4 <- odds.ratio(exp[4], nonexp[4])
exp_non.r5 <- odds.ratio(exp[5], nonexp[5])
exp_non.r6 <- odds.ratio(exp[6], nonexp[6])
exp_non.r7 <- odds.ratio(exp[7], nonexp[7])
exp_non.r8 <- odds.ratio(exp[8], nonexp[8])
exp_non.r9 <- odds.ratio(exp[9], nonexp[9])
exp_non.r10 <- 0
exp_non.r11 <- odds.ratio(exp[11], nonexp[11])
exp_non.r12 <- odds.ratio(exp[12], nonexp[12])
exp_non.r13 <- odds.ratio(exp[13], nonexp[13])

# Overall
exp_non.ratios <- c(exp_non.r1, exp_non.r2, exp_non.r3, exp_non.r4, exp_non.r5, 
                    exp_non.r6, exp_non.r7, exp_non.r8, exp_non.r9, exp_non.r10, 
                    exp_non.r11, exp_non.r12, exp_non.r13)

exp_non.overall_mean <- mean(exp_non.ratios)
exp_non.overall_stdev <- sd(exp_non.ratios)
t.test(exp_non.ratios, mu = 1)  # p = 0.1051, set the mean at 1 instead of 0

# blue whiting
exp_non.whb <- c(exp_non.r2, exp_non.r3, exp_non.r7)
exp_non.whb_mean <- mean(exp_non.whb)
exp_non.whb_stdev <- sd(exp_non.whb)
t.test(exp_non.whb, mu = 1)  # p = 0.01913

# boarfish 
exp_non.bof <- c(exp_non.r4, exp_non.r11)
exp_non.bof_mean <- mean(exp_non.bof)
exp_non.bof_stdev <- sd(exp_non.bof)
t.test(exp_non.bof, mu = 1)  # p = 0.3268

# Herring 
exp_non.her <- c(exp_non.r1, exp_non.r5, exp_non.r6, exp_non.r9)
exp_non.her_mean <- mean(exp_non.her)
exp_non.her_stdev <- sd(exp_non.her)
t.test(exp_non.her, mu = 1)  # p = 0.486

# Mackerel 
exp_non.mac <- c(exp_non.r10, exp_non.r12)
exp_non.mac_mean <- mean(exp_non.mac)
exp_non.mac_stdev <- sd(exp_non.mac)
t.test(exp_non.mac, mu = 1)  # p = 0.9097
