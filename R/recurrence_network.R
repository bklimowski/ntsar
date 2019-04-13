

r <- cumsum(rnorm(1000))
res <- matrix(0, 1000, 1000)
eps<- 0.2
for (ii in 1:length(r)) {
  res[ii,] <- abs(r - r[ii]) < eps
}
image(res, col=gray.colors(2, start = 1, end = 0))
