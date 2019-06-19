#' Simulate a probability density or mass function and plot it
#' 
#' This func
#' @param p1 a numeric vector to pass to the distribution function (dist1)
#' @param p2 a numeric vector to pass to the distribution function (dist1)
#' @param xscale range of x to simulate and plot
#' @param dist1 distribution function
#' @param plot whether to plot
#'
#' @return simulated data grouped by parameters
#'
#' @examples
#' shape <- c(1,2,7,5,9)
#' rate <- 1/c(2,2,1,0.5)
#' xscale <- c(0,20)
#' dist1 <- dgamma
#' simdist <- sim_dist(shape,rate,xscale,dist1)
sim_dist <- function(p1=c(5,10),p2 = NULL,xscale = c(0, 20),
                          dist1 = dnorm, plot = T) {
  dist_name <- deparse(substitute(dist1))
  dist_name <- substr(dist_name,2,nchar(dist_name))
  discrete_dist <- c("unifd", "binom", "geom", "pois", "multinom", "hyper", "nbinom","dmultinom")
  is_discrete <- dist_name %in% discrete_dist

  if(!is_discrete)
    x <- seq(xscale[1], xscale[2], length.out =  100)
  else
    x <- seq(xscale[1], xscale[2])

  p1_s <- deparse(substitute(p1))
  p2_s <- deparse(substitute(p2))

  p2_isnull <- rep(is.null(p2), length(x))
  dsts <- lapply(1:length(p1),
                 function(i)
                   cbind(x = x,
                         distrib = ifelse(p2_isnull,
                                          dist1(x, p1[i]),
                                          dist1(x, p1[i], p2[i])),
                         fact = i,
                         params = ifelse(p2_isnull,
                                         paste0(p1_s,"=",p1[i]),
                                         paste0(p1_s,"=",p1[i], ", ",p2_s,"=",p2[i])
                         )
                   ))
  dsts <- do.call(rbind, dsts)

  type1 <- ifelse(is_discrete,"p","n")
  if (plot)
  {
    plot_line_color(x = dsts[, 1], y = dsts[, 2], fact = dsts[, 3],
                    type=type1)
    title(dist_name)
    uu <- unique(dsts[,4])
    legend("topright",legend=uu,col=seq_along(uu),lty=1:length(uu),lwd=2,
           bty = "n",bg="transparent")
  }
  return(dsts)
}

#' Plot color lines
#'
#' @param x
#' @param y
#' @param fact a factor variable to be used for coloring
#' @param lwd line width
#' @param ... params to pass to the lines function
#'
#' @return
#' @export
#'
#' @examples
#' k <- 1:5
#' x <- seq(0,10,length.out =  100)
#' dsts <- lapply(1:length(k), function(i) cbind(x=x, distri=dchisq(x,k[i]),fact=i) )
#' dsts <- do.call(rbind,dsts)
#' plot_line_color(x=dsts[,1],y=dsts[,2],fact=dsts[,3]),legend_draw=T)
plot_line_color <- function(x,y,fact,type='n',lwd=2,legend_draw=F,...)
{
  plot(x,y,col=fact,pch=19,type=type)
  xy <- cbind(x,y)
  facts <- unique(fact)
  invisible(
    lapply(seq_along(fact), function(j) {
      xy2 <- subset(xy,fact==j)
      lines(xy2[ ,1],xy2[,2],col=j,lty=j,lwd=lwd,...)
    })
  )
  if (legend_draw)
    legend("topright",legend=facts,col=1:length(facts),lty=1:length(facts),
         bty = "n",bg="transparent")

  grid()
}
