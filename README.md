# Why?
This is an educational repository that teaches how to generate random numbers
with various parameter settings using probability density functions
([PDF](https://en.wikipedia.org/wiki/Probability_density_function)) and
probability mass functions
([PMF](https://en.wikipedia.org/wiki/Probability_mass_function)); it also plots the simulated (generated) data
showing the parameters values and the corresponding graphs. Briefly, it

- simulates univariate continuous probability distribution 
- simulates univariate discrete probability distribution 
- simulates bi-variate normal distribution in various covariance matrix (requires 'mvtnorm' package)
- plots the data using traditional plot system i.e. `graphics` package
- declares `runifd`,`qunifd`, `punifd` and `dunifd` functions to support discrete uniform distribution

# TODO
- simulate and plot multinomial distributions
- use `persp` for bivariate distributions
- add circular distributation like von-Mises
- add stochastic processes lile Brownian motion, etc.

# Snapshots
For snapshots see the `png` files.
