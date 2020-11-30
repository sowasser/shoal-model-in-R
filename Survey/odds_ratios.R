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

# Run t-test with mean to test at 1 instead of 0
wilcox.test(exp_non.ratios, mu = 1)# p = 0.09424, 
mean(exp_non.ratios)  # 1.429627

# Run t-tests for species & difficulty, but I don't think these stats (other
# than the means) are valid because of the really small sample sizes.

# blue whiting
exp_non.whb <- c(exp_non.r2, exp_non.r3, exp_non.r7)
t.test(exp_non.whb, mu = 1)  # p = 0.01913, mean = 1.698624 ***

# boarfish 
exp_non.bof <- c(exp_non.r4, exp_non.r11)
t.test(exp_non.bof, mu = 1)  # p = 0.3268, mean = 1.677139

# Herring 
exp_non.her <- c(exp_non.r1, exp_non.r5, exp_non.r6, exp_non.r9)
t.test(exp_non.her, mu = 1)  # p = 0.486, mean = 1.566292

# Mackerel 
exp_non.mac <- c(exp_non.r10, exp_non.r12)
t.test(exp_non.mac, mu = 1)  # p = 0.9097, mean = 0.875

# Easy
exp_non.easy <- c(exp_non.r1, exp_non.r2, exp_non.r9)
t.test(exp_non.easy, mu = 1)  # p = 0.3737, mean = 2.011724

# Medium
exp_non.med <- c(exp_non.r3, exp_non.r4, 
                 exp_non.r11, exp_non.r12, exp_non.r13)
wilcox.test(exp_non.med, mu = 1)  # p = 0.02005, mean = 1.571573 ***

# Hard
exp_non.hard <- c(exp_non.r5, exp_non.r6, exp_non.r7, exp_non.r8)
t.test(exp_non.hard, mu = 1)  # p = 0.5308, mean = 1.173029


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
wilcox.test(exp_local.ratios, mu = 1)  # p = 0.7354
mean(exp_local.ratios)  # mean = 1.098387

# blue whiting
exp_local.whb <- c(exp_local.r2, exp_local.r3, exp_local.r7)
t.test(exp_local.whb, mu = 1)  # p = 0.6972, mean = 1.10375

# boarfish 
exp_local.bof <- c(exp_local.r4, exp_local.r11)
t.test(exp_local.bof, mu = 1)  # p = 0.9136 = 1.053391

# Herring 
exp_local.her <- c(exp_local.r1, exp_local.r5, exp_local.r6, exp_local.r9)
t.test(exp_local.her, mu = 1)  # p = 0.4171, mean = 1.382674

# Mackerel 
exp_local.mac <- c(exp_local.r10, exp_local.r12)
t.test(exp_local.mac, mu = 1)  # p = 0.9423, mean = 0.916667

# Easy
exp_local.easy <- c(exp_local.r1, exp_local.r2, exp_local.r9)
t.test(exp_local.easy, mu = 1)  # p = 0.3239, mean = 1.575181

# Medium
exp_local.med <- c(exp_local.r3, exp_local.r4, 
                 exp_local.r11, exp_local.r12, exp_local.r13)
t.test(exp_local.med, mu = 1)  # p = 0.7348, mean = 1.086118

# Hard
exp_local.hard <- c(exp_local.r5, exp_local.r6, exp_local.r7, exp_local.r8)
t.test(exp_local.hard, mu = 1)  # p = 0.9102, mean = 1.030724


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
wilcox.test(exp_none.ratios, mu = 1)  # p = 0.1841 
mean(exp_none.ratios)  # mean = 1.772895

# blue whiting
exp_none.whb <- c(exp_none.r2, exp_none.r3, exp_none.r7)
t.test(exp_none.whb, mu = 1)  # p = 0.4453, mean = 1.97619

# boarfish 
exp_none.bof <- c(exp_none.r4, exp_none.r11)
t.test(exp_none.bof, mu = 1)  # p = 0.4918, mean = 2.429947

# Herring 
exp_none.her <- c(exp_none.r1, exp_none.r5, exp_none.r6, exp_none.r9)
t.test(exp_none.her, mu = 1)  # p = 0.3884, mean = 1.734213

# Mackerel 
exp_none.mac <- c(exp_none.r10, exp_none.r12)
t.test(exp_none.mac, mu = 1)  # p = 0.716, mean = 1.916667

# Easy
exp_none.easy <- c(exp_none.r1, exp_none.r2, exp_none.r9)
t.test(exp_none.easy, mu = 1)  # p = 0.1276, mean = 2.923395

# Medium
exp_none.med <- c(exp_none.r3, exp_none.r4, 
                 exp_none.r11, exp_none.r12, exp_none.r13)
t.test(exp_none.med, mu = 1)  # p = 0.07524, mean = 2.457693 

# Hard
exp_none.hard <- c(exp_none.r5, exp_none.r6, exp_none.r7, exp_none.r8)
t.test(exp_none.hard, mu = 1)  # p = 0.1211, mean = 0.4972452


# Odds ratio for non-expert/no expertise correct species ID -------------------
nonexp_none.r1 <- odds.ratio(nonexp[1], none[1])
nonexp_none.r2 <- odds.ratio(nonexp[2], none[2])
nonexp_none.r3 <- odds.ratio(nonexp[3], none[3])
nonexp_none.r4 <- odds.ratio(nonexp[4], none[4])
nonexp_none.r5 <- odds.ratio(nonexp[5], none[5])
nonexp_none.r6 <- odds.ratio(nonexp[6], none[6])
nonexp_none.r7 <- 0
nonexp_none.r8 <- odds.ratio(nonexp[8], none[8])
nonexp_none.r9 <- odds.ratio(nonexp[9], none[9])
nonexp_none.r10 <- 0
nonexp_none.r11 <- odds.ratio(nonexp[11], none[11])
nonexp_none.r12 <- odds.ratio(nonexp[12], none[12])
nonexp_none.r13 <- odds.ratio(nonexp[13], none[13])

# Overall
nonexp_none.ratios <- c(nonexp_none.r1, nonexp_none.r2, nonexp_none.r3, 
                        nonexp_none.r4, nonexp_none.r5, nonexp_none.r6, 
                        nonexp_none.r7, nonexp_none.r8, nonexp_none.r9, 
                        nonexp_none.r10, nonexp_none.r11, nonexp_none.r12, 
                        nonexp_none.r13)
wilcox.test(nonexp_none.ratios, mu = 1)  # p = 0.7798
mean(nonexp_none.ratios)  # mean = 1.146281

# blue whiting
nonexp_none.whb <- c(nonexp_none.r2, nonexp_none.r3, nonexp_none.r7)
t.test(nonexp_none.whb, mu = 1)  # p = 0.8147, mean = 1.15565

# boarfish 
nonexp_none.bof <- c(nonexp_none.r4, nonexp_none.r11)
t.test(nonexp_none.bof, mu = 1)  # p = 0.6459, mean = 1.328571

# Herring 
nonexp_none.her <- c(nonexp_none.r1, nonexp_none.r5, nonexp_none.r6, 
                     nonexp_none.r9)
t.test(nonexp_none.her, mu = 1)  # p = 0.5081, mean = 1.31884

# Mackerel 
nonexp_none.mac <- c(nonexp_none.r10, nonexp_none.r12)
t.test(nonexp_none.mac, mu = 1)  # p = 0.9448, mean = 1.095238

# Easy
nonexp_none.easy <- c(nonexp_none.r1, nonexp_none.r2, nonexp_none.r9)
t.test(nonexp_none.easy, mu = 1)  # p = 0.195, mean = 1.840758

# Medium
nonexp_none.med <- c(nonexp_none.r3, nonexp_none.r4, nonexp_none.r11, 
                     nonexp_none.r12, nonexp_none.r13)
t.test(nonexp_none.med, mu = 1)  # p = 0.1454, mean = 1.477874

# Hard
nonexp_none.hard <- c(nonexp_none.r5, nonexp_none.r6, nonexp_none.r7, 
                      nonexp_none.r8)
t.test(nonexp_none.hard, mu = 1)  # p = 0.08514, mean = 0.4975036
