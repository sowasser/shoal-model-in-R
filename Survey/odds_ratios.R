# Analysis of the results of a survey of species identification from echograms
# and the success rate between experts and non-experts.

path <- "~/Desktop/DO NOT ERASE/1NUIG/Mackerel/Mackerel Data/Survey Data/"  # for laptop

library(tidyverse)
library(questionr)

correct <- read.csv(paste0(path, "raw_correct.csv"))

# Calculate odds ratio --------------------------------------------------------
odds <- function(group1, group2) {
  # Remove #10 cause it's an invalid ratio - just 0
  new1 <- group1[-10]
  new2 <- group2[-10]
  
  # Calculate odds ratio
  ratio <- rep(0, length(new1))
  for(i in 1:12){
    ratio[i] <- odds.ratio(new1[i], new2[i])
  }
  
  # Add 0 in for #10
  ratio <- c(ratio[1:9], 0, ratio[10:12])
  return(ratio)
}

# Odds ratio for expert/non-expert correct species ID -------------------------
exp_non.r <- odds(correct$expert.ratio, correct$non.expert.ratio)

# Run t-test with mean to test at 1 instead of 0
wilcox.test(exp_non.r, mu = 1)# p = 0.09424, 
mean(exp_non.r)  # 1.429627

# Run t-tests for species & difficulty, but I don't think these stats (other
# than the means) are valid because of the really small sample sizes.

# blue whiting
exp_non.whb <- c(exp_non.r[2], exp_non.r[3], exp_non.r[7])
t.test(exp_non.whb, mu = 1)  # p = 0.01913, mean = 1.698624 ***

# boarfish 
exp_non.bof <- c(exp_non.r[4], exp_non.r[11])
t.test(exp_non.bof, mu = 1)  # p = 0.3268, mean = 1.677139

# Herring 
exp_non.her <- c(exp_non.r[1], exp_non.r[5], exp_non.r[6], exp_non.r[9])
t.test(exp_non.her, mu = 1)  # p = 0.486, mean = 1.566292

# Mackerel 
exp_non.mac <- c(exp_non.r[10], exp_non.r[12])
t.test(exp_non.mac, mu = 1)  # p = 0.9097, mean = 0.875

# Easy
exp_non.easy <- c(exp_non.r[1], exp_non.r[2], exp_non.r[9])
t.test(exp_non.easy, mu = 1)  # p = 0.3737, mean = 2.011724

# Medium
exp_non.med <- c(exp_non.r[3], exp_non.r[4], 
                 exp_non.r[11], exp_non.r[12], exp_non.r[13])
wilcox.test(exp_non.med, mu = 1)  # p = 0.02005, mean = 1.571573 ***

# Hard
exp_non.hard <- c(exp_non.r[5], exp_non.r[6], exp_non.r[7], exp_non.r[8])
t.test(exp_non.hard, mu = 1)  # p = 0.5308, mean = 1.173029


# Odds ratio for expert/local expertise correct species ID --------------------
exp_local.r <- odds(correct$expert.ratio, correct$local.ratio)

wilcox.test(exp_local.r, mu = 1)  # p = 0.7354
mean(exp_local.r)  # mean = 1.098387

# blue whiting
exp_local.whb <- c(exp_local.r[2], exp_local.r[3], exp_local.r[7])
t.test(exp_local.whb, mu = 1)  # p = 0.6972, mean = 1.10375

# boarfish 
exp_local.bof <- c(exp_local.r[4], exp_local.r[11])
t.test(exp_local.bof, mu = 1)  # p = 0.9136 = 1.053391

# Herring 
exp_local.her <- c(exp_local.r[1], exp_local.r[5], exp_local.r[6], exp_local.r[9])
t.test(exp_local.her, mu = 1)  # p = 0.4171, mean = 1.382674

# Mackerel 
exp_local.mac <- c(exp_local.r[10], exp_local.r[12])
t.test(exp_local.mac, mu = 1)  # p = 0.9423, mean = 0.916667

# Easy
exp_local.easy <- c(exp_local.r[1], exp_local.r[2], exp_local.r[9])
t.test(exp_local.easy, mu = 1)  # p = 0.3239, mean = 1.575181

# Medium
exp_local.med <- c(exp_local.r[3], exp_local.r[4], 
                 exp_local.r[11], exp_local.r[12], exp_local.r[13])
t.test(exp_local.med, mu = 1)  # p = 0.7348, mean = 1.086118

# Hard
exp_local.hard <- c(exp_local.r[5], exp_local.r[6], exp_local.r[7], exp_local.r[8])
t.test(exp_local.hard, mu = 1)  # p = 0.9102, mean = 1.030724


# Odds ratio for expert/no expertise correct species ID -----------------------
odds2 <- function(group1, group2) {
  # Remove #7 & #10 cause they're invalid ratios - just 0
  new1 <- group1[-10]
  new2 <- group2[-10]
  
  new1 <- new1[-7]
  new2 <- new2[-7]
  
  # Calculate odds ratio
  ratio <- rep(0, length(new1))
  for(i in 1:11){
    ratio[i] <- odds.ratio(new1[i], new2[i])
  }
  
  # Add 0 in for #10
  ratio <- c(ratio[1:6], 0, ratio[7:8], 0, ratio[9:11])
  return(ratio)
}

exp_none.r <- odds2(correct$expert.ratio, correct$none.ratio)

wilcox.test(exp_none.r, mu = 1)  # p = 0.1841 
mean(exp_none.r)  # mean = 1.772895

# blue whiting
exp_none.whb <- c(exp_none.r[2], exp_none.r[3], exp_none.r[7])
t.test(exp_none.whb, mu = 1)  # p = 0.4453, mean = 1.97619

# boarfish 
exp_none.bof <- c(exp_none.r[4], exp_none.r[11])
t.test(exp_none.bof, mu = 1)  # p = 0.4918, mean = 2.429947

# Herring 
exp_none.her <- c(exp_none.r[1], exp_none.r[5], exp_none.r[6], exp_none.r[9])
t.test(exp_none.her, mu = 1)  # p = 0.3884, mean = 1.734213

# Mackerel 
exp_none.mac <- c(exp_none.r[10], exp_none.r[12])
t.test(exp_none.mac, mu = 1)  # p = 0.716, mean = 1.916667

# Easy
exp_none.easy <- c(exp_none.r[1], exp_none.r[2], exp_none.r[9])
t.test(exp_none.easy, mu = 1)  # p = 0.1276, mean = 2.923395

# Medium
exp_none.med <- c(exp_none.r[3], exp_none.r[4], 
                 exp_none.r[11], exp_none.r[12], exp_none.r[13])
t.test(exp_none.med, mu = 1)  # p = 0.07524, mean = 2.457693 

# Hard
exp_none.hard <- c(exp_none.r[5], exp_none.r[6], exp_none.r[7], exp_none.r[8])
t.test(exp_none.hard, mu = 1)  # p = 0.1211, mean = 0.4972452


# Odds ratio for non-expert/no expertise correct species ID -------------------
nonexp_none.r <- odds2(correct$non.expert.ratio, correct$none.ratio)

wilcox.test(nonexp_none.r, mu = 1)  # p = 0.7798
mean(nonexp_none.r)  # mean = 1.146281

# blue whiting
nonexp_none.whb <- c(nonexp_none.r[2], nonexp_none.r[3], nonexp_none.r[7])
t.test(nonexp_none.whb, mu = 1)  # p = 0.8147, mean = 1.15565

# boarfish 
nonexp_none.bof <- c(nonexp_none.r[4], nonexp_none.r[11])
t.test(nonexp_none.bof, mu = 1)  # p = 0.6459, mean = 1.328571

# Herring 
nonexp_none.her <- c(nonexp_none.r[1], nonexp_none.r[5], nonexp_none.r[6], 
                     nonexp_none.r9)
t.test(nonexp_none.her, mu = 1)  # p = 0.5081, mean = 1.31884

# Mackerel 
nonexp_none.mac <- c(nonexp_none.r[10], nonexp_none.r[12])
t.test(nonexp_none.mac, mu = 1)  # p = 0.9448, mean = 1.095238

# Easy
nonexp_none.easy <- c(nonexp_none.r[1], nonexp_none.r2[2] nonexp_none.r[9])
t.test(nonexp_none.easy, mu = 1)  # p = 0.195, mean = 1.840758

# Medium
nonexp_none.med <- c(nonexp_none.r[3], nonexp_none.r[4], nonexp_none.r[11], 
                     nonexp_none.r[12], nonexp_none.r[13])
t.test(nonexp_none.med, mu = 1)  # p = 0.1454, mean = 1.477874

# Hard
nonexp_none.hard <- c(nonexp_none.r[5], nonexp_none.r[6], nonexp_none.r[7], 
                      nonexp_none.r[8])
t.test(nonexp_none.hard, mu = 1)  # p = 0.08514, mean = 0.4975036
