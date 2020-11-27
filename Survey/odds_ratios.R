# Analysis of the results of a survey of species identification from echograms
# and the success rate between experts and non-experts.

path <- "~/Desktop/DO NOT ERASE/1NUIG/Mackerel/Mackerel Data/Survey Data/"  # for laptop

library(tidyverse)
library(questionr)

correct <- read.csv(paste0(path, "raw_correct.csv"))

# Odds ratio for expert/non-expert correct species ID -------------------------
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


# Odds ratio for expert/local expertise correct species ID --------------------
local <- correct$local.ratio

exp_local.r1 <- odds.ratio(exp[1], local[1])
exp_local.r2 <- odds.ratio(exp[2], local[2])
exp_local.r3 <- odds.ratio(exp[3], local[3])
exp_local.r4 <- odds.ratio(exp[4], local[4])
exp_local.r5 <- odds.ratio(exp[5], local[5])
exp_local.r6 <- odds.ratio(exp[6], local[6])
exp_local.r7 <- odds.ratio(exp[7], local[7])
exp_local.r8 <- odds.ratio(exp[8], local[8])
exp_local.r9 <- odds.ratio(exp[9], local[9])
exp_local.r10 <- 0
exp_local.r11 <- odds.ratio(exp[11], local[11])
exp_local.r12 <- odds.ratio(exp[12], local[12])
exp_local.r13 <- odds.ratio(exp[13], local[13])

# Overall
exp_local.ratios <- c(exp_local.r1, exp_local.r2, exp_local.r3, exp_local.r4, 
                      exp_local.r5, exp_local.r6, exp_local.r7, exp_local.r8, 
                      exp_local.r9, exp_local.r10, exp_local.r11, exp_local.r12, 
                      exp_local.r13)

exp_local.overall_mean <- mean(exp_local.ratios)
exp_local.overall_stdev <- sd(exp_local.ratios)
t.test(exp_local.ratios, mu = 1)  # p = 0.5914, set the mean at 1 instead of 0

# blue whiting
exp_local.whb <- c(exp_local.r2, exp_local.r3, exp_local.r7)
exp_local.whb_mean <- mean(exp_local.whb)
exp_local.whb_stdev <- sd(exp_local.whb)
t.test(exp_local.whb, mu = 1)  # p = 0.6972

# boarfish 
exp_local.bof <- c(exp_local.r4, exp_local.r11)
exp_local.bof_mean <- mean(exp_local.bof)
exp_local.bof_stdev <- sd(exp_local.bof)
t.test(exp_local.bof, mu = 1)  # p = 0.9136

# Herring 
exp_local.her <- c(exp_local.r1, exp_local.r5, exp_local.r6, exp_local.r9)
exp_local.her_mean <- mean(exp_local.her)
exp_local.her_stdev <- sd(exp_local.her)
t.test(exp_local.her, mu = 1)  # p = 0.4171

# Mackerel 
exp_local.mac <- c(exp_local.r10, exp_local.r12)
exp_local.mac_mean <- mean(exp_local.mac)
exp_local.mac_stdev <- sd(exp_local.mac)
t.test(exp_local.mac, mu = 1)  # p = 0.9423


# Odds ratio for expert/no expertise correct species ID -----------------------
none <- correct$none.ratio

exp_none.r1 <- odds.ratio(exp[1], none[1])
exp_none.r2 <- odds.ratio(exp[2], none[2])
exp_none.r3 <- odds.ratio(exp[3], none[3])
exp_none.r4 <- odds.ratio(exp[4], none[4])
exp_none.r5 <- odds.ratio(exp[5], none[5])
exp_none.r6 <- odds.ratio(exp[6], none[6])
exp_none.r7 <- 0
exp_none.r8 <- odds.ratio(exp[8], none[8])
exp_none.r9 <- odds.ratio(exp[9], none[9])
exp_none.r10 <- 0
exp_none.r11 <- odds.ratio(exp[11], none[11])
exp_none.r12 <- odds.ratio(exp[12], none[12])
exp_none.r13 <- odds.ratio(exp[13], none[13])

# Overall
exp_none.ratios <- c(exp_none.r1, exp_none.r2, exp_none.r3, exp_none.r4, 
                     exp_none.r5, exp_none.r6, exp_none.r7, exp_none.r8, 
                     exp_none.r9, exp_none.r10, exp_none.r11, exp_none.r12, 
                     exp_none.r13)

exp_none.overall_mean <- mean(exp_none.ratios)
exp_none.overall_stdev <- sd(exp_none.ratios)
t.test(exp_none.ratios, mu = 1)  # p = 0.09079, set the mean at 1 instead of 0

# blue whiting
exp_none.whb <- c(exp_none.r2, exp_none.r3, exp_none.r7)
exp_none.whb_mean <- mean(exp_none.whb)
exp_none.whb_stdev <- sd(exp_none.whb)
t.test(exp_none.whb, mu = 1)  # p = 0.4453

# boarfish 
exp_none.bof <- c(exp_none.r4, exp_none.r11)
exp_none.bof_mean <- mean(exp_none.bof)
exp_none.bof_stdev <- sd(exp_none.bof)
t.test(exp_none.bof, mu = 1)  # p = 0.4918

# Herring 
exp_none.her <- c(exp_none.r1, exp_none.r5, exp_none.r6, exp_none.r9)
exp_none.her_mean <- mean(exp_none.her)
exp_none.her_stdev <- sd(exp_none.her)
t.test(exp_none.her, mu = 1)  # p = 0.3884

# Mackerel 
exp_none.mac <- c(exp_none.r10, exp_none.r12)
exp_none.mac_mean <- mean(exp_none.mac)
exp_none.mac_stdev <- sd(exp_none.mac)
t.test(exp_none.mac, mu = 1)  # p = 0.716
