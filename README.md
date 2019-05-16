## shoal-model-in-R
This repository is an extension of [my fish shoaling model][model] built in Python using the [Mesa individual-based modelling framework][mesa]. There are some tasks I'm more comfortable completing in R, or for which I can find better packages, so those are housed here. This repository contains 3 main projects:

1. Graphs made using `ggplot2`, which I'm more comfortable with when creating publication-quality figures.
2. Scripts for importing data from [LoggerPro][lp], which I use to track fish in video. These data can then be used for statistical analyses of their behaviour. I've also done this in Python in my main repository. Currently, I've separated these into three scripts: 
	1. [`tracking_import.R`][track1] is for smaller datasets (i.e. <10 fish). Simple code.
	2. [`tracking_import_large.R`][track2] is for larger datasets and I'm attempting to automate a lot of the data cleaning & management, since the amount of data quickly gets out of hand.
	3. [`tracking_import_stepwise.R`][track3] is for videos with the largest number of fish, where all individuals were tracked for select frames of the video. The resulting dataframe is organized opposite to the other scripts and is therefore handled differently.
3.  Approximate Bayesian Computation, for comparing my model to the videos of tracked fish. I'm still working out the best package to use, so this is a work in progress.

## Installation

* Clone this repository to your computer: `git clone https://github.com/sowasser/shoal-model-in-R.git`
* I am using R version 3.6.0 (2019-04-26) - "Planting of a Tree"
* Required packages are in [`requirements.R`][req]


[model]: https://github.com/sowasser/fish-shoaling-model
[mesa]: https://github.com/projectmesa/mesa
[gg]: http://ggplot2.org/
[lp]: https://www.vernier.com/products/software/lp/
[track1]: https://github.com/sowasser/shoal-model-in-R/blob/master/tracking_import.R
[track2]: https://github.com/sowasser/shoal-model-in-R/blob/master/tracking_import_large.R
[track3]: https://github.com/sowasser/shoal-model-in-R/blob/master/tracking_import_stepwise.R
[req]: https://github.com/sowasser/shoal-model-in-R/blob/master/requirements.R
