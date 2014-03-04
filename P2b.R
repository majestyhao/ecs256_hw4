P2b <- function(nrep, sig = FALSE) {
  X <- runif(nrep, 0 ,1)
  for (i in 1:9)
    X <- cbind(X, runif(nrep, 0, 1), 10)
  Y <- X[, 1] + X[, 2] + X[, 3] + 0.1 * X[, 4] + 0.01 * X[, 5]
  if (sig == TRUE) {
    sigTest(Y, X)
  } else {
    prsm(Y, X[, 1:10], predacc = ar2, printdel = T)
    prsm(Y, X[, 1:10], predacc = aiclogit, printdel = T)
  }
}

sigTest <- function(Y, X) {
  lmout <- summary(lm(Y ~ X[, 1] + X[, 2] + X[, 3] + X[, 4] + X[, 5] + 
                        X[, 6] + X[, 7] + X[, 8] + X[, 9] + X[, 10])) 
  print(lmout$signif.stars)
}

main<- function() {
  nrep <- 100
  f <- TRUE
  while (f) {
    if (nrep == 100000)
      f <- FALSE
    for (i in 1:3)
      P2b(nrep)
    P2b(nrep, sig = TRUE)
    nrep <- nrep * 10
  }
}

main()