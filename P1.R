# explore the bias of an approximate regression model
verification <- function(nrep) {
  X <- runif(nrep, 0, 1) # sample of X
  Y <- X ^ 0.75
  summary(lm(Y ~ X + 0))
}

verification(500000)