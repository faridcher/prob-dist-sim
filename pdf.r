# Uniform
min <- c(-5,-2)
max <- c(5, 2)
xscale <- c(-6,6)
simdist <- sim_dist(min,max,xscale,dunif)

# Logistic
location <- c(0,0,0,-2)
scale <- c(.2,1,5,.5)
xscale <- c(-5,5)
simdist <- sim_dist(location,scale,xscale,dlogis)

# Normal
mu <- c(0,0,0,-2)
sdev <- c(.2,1,5,.5)
xscale <- c(-5,5)
simdist <- sim_dist(mu,sdev,xscale,dnorm)
#CDF
#simdist <- sim_dist(size,prob,xscale,pnorm)

# LogNormal
mu <- c(0,2,0,0.5,0.25,0.125)
sdev <- c(3,2,1,1,1,1)
xscale <- c(0.1,5)
simdist <- sim_dist(mu,sdev,xscale,dlnorm)

# StudentT
dfree <- c(1,2,5,Inf, 5)
ncp <- c(0,0,0,0, 2.5)
xscale <- c(-5,8)
simdist <- sim_dist(dfree,ncp,xscale,dt)

# F
df1 <- c(1,2,5,100,100)
df2 <- c(1,1,2,1,100)
ncp <- c(rep(0,5))
xscale <- c(0,5)
simdist <- sim_dist(df1,df2,xscale,df)

# dchisq
# df <= 2 is like exponential and df > 2 is like ?
k <- 1:5
#k <- seq(1,3,.2)
xscale <- c(0,10)
simdist <- sim_dist(k,NULL,xscale,dchisq)

# dgamma
shape <- c(1,2,7,5,9)
rate <- 1/c(2,2,1,0.5)
xscale <- c(0,20)
simdist <- sim_dist(shape,rate,xscale,dgamma)


# dbeta
shp1 <- c(0.5 ,5 ,1 ,2 ,2 )
shp2 <- c(0.5,1,3,2,5)
xscale<- 0:1
simdist <- sim_dist(shp1,shp2,xscale,dbeta)

# dexp
rate <- .5:2.5
xscale <- 0:5
simdist <- sim_dist(rate,NULL,xscale,dexp)

# Weibull
shape <- rep(1,4)
scale <- c(.5, 1 ,1.5 , 5)
xscale<- c(0,2.5)
simdist <- sim_dist(shape,scale,xscale,dweibull)
