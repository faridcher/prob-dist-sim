## all possible outcomes of Multinom(size = 5, K = 4)
X <- t(as.matrix(expand.grid(0:5, 0:5, 0:5)))
X <- X[, colSums(X) <= 5]
X <- rbind(X, 5 - colSums(X))
round(apply(X, 2, function(x) dmultinom(x, prob = c(1,2,5,5))), 5)

rmultinom(10,12,c(.1,.2,.7))
dmultinom(c(0,0,3),prob=c(1,2,5),3)

# Binomial is a special case of Multinomial 
rmultinom(10, 20,c(.5,.5))[1,] # two prob is needed. one for success and one for failure
rbinom(10,20,.5)
dmultinom(c(1,2),3,prob=c(.5,.5))
dbinom(2,3,.5) #2 win

## TODO first do with image. then persp for bi-norm and binom
rmultinom(100,5,c(1/2,1/3,1/6))[,]
  %>% plot3D::hist3D(z=.)
