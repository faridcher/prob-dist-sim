# png(file="plots%03.png",height=1000,width=1000)
source('sim-dist.r')

# Discrete
par(mfrow=c(2,3), oma=c(0,0,1,0))
source('pmf.r')
title("Univariate Discrete Probability Mass Functions (PMF)", outer = T)

# Continuous
par(mfrow=c(3,4),oma=c(0,0,1,0))
source('pdf.r')
title("Univariate Continuous Probability Distribution Functions (PDF)",outer=T)

# Multinomial

# Multivariate normal
par(mfrow=c(2,3), oma=c(0,0,1,0))
source('mvnorm.r')
title("Bivariate Normal Distribution",outer = T)

rm(list=ls())
# dev.off()
