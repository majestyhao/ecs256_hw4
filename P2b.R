P2b <- function(nrep, sig = FALSE, k = 0.01) {
  X <- NULL
  for (i in 1:10)
    X <- cbind(X, runif(nrep, 0, 1))
  mx <- X[, 1] + X[, 2] + X[, 3] + 0.1 * X[, 4] + 0.01 * X[, 5] # Y's mean given X
  Y <- runif(nrep, mx - 1, mx + 1)
  
  if (sig == TRUE) {
    cat("significant test: \n")
    sigTest(Y, X)
    cat("\n")
  } else {
    if (min(Y) >= 0 && max(Y) <= 1) {
      cat("AIC approach: \n")
      prsm(Y, X[, 1:10], k, predacc = aiclogit, printdel = T) # y values must be 0 <= y <= 1
    } else {
      cat("R2 approach: \n")
      prsm(Y, X[, 1:10], k, predacc = ar2, printdel = T)
      cat("\n")
    }
  }
}

sigTest <- function(Y, X) {
  drop <- rep(FALSE, ncol(X))
  lmout <- summary(lm(Y ~ X)) 
  for (i in 2:ncol(X) + 1)
    if (lmout$coefficients[i, 4] >= 0.05) {
      drop[i - 1] <- TRUE
    }
  cat("reserved: ")
  for (i in 1:ncol(X))
    if (drop[i] == FALSE) {
      cat(i)
      cat(" ")
    }
  cat("\n")
}

main<- function() {
  nrep <- 100
  f <- TRUE
  while (f) {
    print(nrep)
    if (nrep == 100000)
      f <- FALSE
    for (i in 1:3) {
      cat("k = 0.01, ")
      P2b(nrep)
    }    
    for (i in 1:3) {
      cat("k = 0.05, ")
      P2b(nrep, k = 0.05)
    }    
    for (i in 1:3) {
      P2b(nrep, sig = TRUE)
    }          
    nrep <- nrep * 10
  }
}

main()