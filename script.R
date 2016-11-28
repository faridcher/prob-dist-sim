source("functions.R")

# Continuous --------------------------------------------------------------

pdf(file="continuous.pdf", width = 14,height = 14)
op <- par(mfrow=c(3,4))
switch_margin()
cont_dist_plot()
dev.off()


# Discrete ----------------------------------------------------------------

pdf(file="discrete.pdf",width = 10,height = 10)
op <- par(mfrow=c(2,3))
switch_margin()
discrete_dist_plot()
dev.off()


# Multivariate normal -----------------------------------------------------

pdf(file="mvtnorm.pdf",width = 10,height = 10)
op <- par(mfrow=c(2,3))
mvnorm_plot()
dev.off()
