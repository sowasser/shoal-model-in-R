# Working through tutorial from the NetLogo/ABC workshop at BES 2017. While
# that workshop focused on running a NetLogo model through R using EasyABC, 
# attempting here to skip that and just use 'abc' to calculate posterior
# distribution with data generated in Python.

library(abc)

path <- "~/Desktop/DO NOT ERASE/1NUIG/Mackerel/Mackerel Data"

# Import model output, which is more complicated than just data output
basking.runs <- readRDS(paste0(path,"/basking.runs.1e5.RDS"))

# Import empirical data
basking.data <- read.table(paste0(path, "/basking.data.txt"))
basking.data <- shoaling.data[, 2]  # this is written as [, 1] in the tutorial but I think that's wrong

# Use 'abc' to accept top 1% of runs as approximate posteriors
basking.abc <- abc(target = one.model.run,   # argument target: empirical data
                   param = basking.runs$param,  # priors
                   sumstat = basking.runs$stats,  # model outputs
                   tol = 0.1, method = "rejection")  # proportion of funs to accept; type of ABC to use

summary(basking.abc)
plot.basking.dists(basking.runs, basking.abc)