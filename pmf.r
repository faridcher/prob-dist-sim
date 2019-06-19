# Binomial
size <- c(40,30,25)
prob <- c(.3,.6,.9)
xscale<- c(0,40)
simdist <- sim_dist(size,prob,xscale,dbinom)
#CDF
#simdist <- sim_dist(size,prob,xscale,pbinom)

# uniform
source('unifd.r')
min <- c(-5,-2)
max <- c(5, 2)
xscale <- c(-5,5)
simdist <- sim_dist(min, max, xscale, dunifd)

# Geometric
prob <- c(.2,.5,.8,1)
xscale <- c(0,10)
simdist <- sim_dist(prob,NULL,xscale,dgeom)

# Negative Binomial
size <- c(1,5,10,20,40)
prob <- rep(.5,5)
xscale <- c(0,20)
simdist <- sim_dist(size,prob,xscale,dnbinom)

# Poisson
lambda <-c(1,4,10)
xscale <- c(0,20)
simdist <- sim_dist(lambda,NULL,xscale,dpois)
