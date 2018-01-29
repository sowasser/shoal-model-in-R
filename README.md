## shoal-model-in-R
This repository has two functions:

1. It serves as an extension for [my fish shoaling model][model] built in Python using the [Mesa individual-based modelling framework][mesa]. I'm more comfortable with plotting using [ggplot2][gg] in R, so this repository is for all of the data that I export from my model.
2. It includes scripts for importing data from [LoggerPro][lp], which I use to track fish in video. These data can then be used for statistical analyses of their behaviour. Currently, I've separated these into two scripts: 
	1. `tracking_import.R` is for smaller datasets (i.e. <10 fish). Simple code.
	2. `tracking_import_large.R` is for larger datasets and I'm attempting to automate a lot of the data cleaning & management, since the amount of data quickly gets out of hand.

## Installation

* Clone this repository to your computer: `git clone https://github.com/sowasser/shoal-model-in-R.git`
* I am using R version 3.4.2 (2017-09-28) - "Short Summer"
* Required packages are in `requirements.R`


[model]: https://github.com/sowasser/fish-shoaling-model
[mesa]: https://github.com/projectmesa/mesa
[gg]: http://ggplot2.org/
[lp]: https://www.vernier.com/products/software/lp/
