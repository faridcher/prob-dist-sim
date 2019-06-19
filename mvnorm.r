if(!require('mvtnorm', quietly = TRUE))
    install.packages("mvtnorm")

x <- seq(-3,3,length.out = 100)
y <- x

sigma <- list(
  sigma1 = c(1,0,0,1),
  sigma2 = c(2,0,0,2),
  sigma3 = c(2,0,0,1),
  sigma4 = c(1,0,0,2),
  sigma5 = c(1,.5,.5,1),
  sigma6 = c(1,-.5,-.5,1)
)

mvnorm_single_plot <- function(x,y,sigma){
  xy <- expand.grid(x,y)
  sig_mat <- matrix(sigma,2,2)
  z <- mvtnorm::dmvnorm(xy,mean = c(0,0),sigma=sig_mat)
  z_mat <- matrix(z,length(x))

  a <- apply(sig_mat,1,paste,collapse=' ')
  exp <- substitute(Sigma==bgroup("(", atop(x,y) ,")"), list(x=a[1],y=a[2]) )

  image(x,y,z_mat,col=topo.colors(20),main=exp , asp=1 )
}
bb <- lapply(sigma, mvnorm_single_plot,x=x,y=y)
