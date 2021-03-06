# discrete uniform distribution (not part of base R)
dunifd <- function(x, min=0, max=1)
  ifelse(x>=min & x<=max & round(x)==x, 1/(max-min+1), 0)
punifd <- function(q, min=0, max=1)
  ifelse(q<min, 0, ifelse(q>=max, 1, (floor(q)-min+1)/(max-min+1)))
qunifd <- function(p, min=0, max=1)
  floor(p*(max-min+1))
runifd <- function(n, min=0, max=1)
  sample(min:max, n, replace=T)
