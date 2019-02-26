# This script is for running Approximate Bayesian Computation (ABC), comparing
# output from the shoal model to the data collected from video of the
# sticklebacks. ABC allows you to use existing data to estimate parameters and
# compare model structure.

# ABC requires:
# * Priors for the parameters - what a model knows beyond the data, i.e.
# expert knowledge. The priors are represented by a distribution, like
# Negative Binomial or Beta.
# * Data the model should fit
# * Criteria for when simulated data match the actual data
# * Many runs of the model for comparison

# For the shoal model, the priors will be determined with a sensitivity analysis,
# through which the extremes of he various parameters (vision, speed, rule
# balance, etc.) are tested.

# The packages available for R is EasyABC.

library(EasyABC)

# tutorial here: https://www.r-bloggers.com/the-easyabc-package-for-approximate-bayesian-computation-in-r/
  

# we want to use ABC to infer the parameters that were used.
# we sample from the same model and use mean and variance
# as summary statstitics for the model and the data.

# observed summary statistics
summarydata = c(mean(data), sd(data))

model <- function(par){
  
  # stochastic model generates a sample for given par
  samples <- rnorm(10, mean =par[1], sd = par[2])
  
  # returning simulated summary statistics
  return(c(mean(samples), sd(samples)))
}

# normalization of the summary statistics, 1,1 probably not the most appropriate choice but to keep it easy
tabnormalization=c(1,1)

# definition of the (flat priors)
priormatrix=cbind(c(-15,2),c(15,4))

# call EasyABC
ABC_Marjoram_original<-ABC_mcmc(method="Marjoram_original", model=model, prior_matrix=priormatrix,
                                n_obs=10000, n_between_sampling=1, summary_stat_target=summarydata, dist_max=1, proposal_range=c(1,1),
                                tab_normalization = tabnormalization, use_seed = F)


str(ABC_Marjoram_original)
par(mfrow=c(2,1))
hist(ABC_Marjoram_original$param[5000:10000,1], main = "Posterior for slope")
hist(ABC_Marjoram_original$param[5000:10000,2], main = "Posterior for intercept")

