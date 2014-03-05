# pm <- read.table("pima-indians-diabetes.data", header = T, sep = ",", row.names=NULL)
# print(head(pm))
# Y <- pm[, 9]
# if (ncol(a) != NULL)
# print(rep(0, ncol(a)))
# a <- cbind(1, 2, 3)
# print(ncol(a))
# X <- NULL
# X <- cbind(X, pm[, 1], pm[, 2], pm[, 6], pm[, 7])
# X <- cbind(X, pm[, 1], pm[, 2], pm[, 3], pm[, 6], pm[, 7], pm[,8])
# print(ncol(X))
# summary(lm(Y ~ pm[, 2]))
# lmout <- summary(lm(Y ~ X[, 2]))
# print(as.name(rownames(lmout$coefficients)[2]))
# X[, 4] = NULL 
# X[, 5] = NULL
# print(aiclogit(Y, X))
test <- function(nrep, sig = FALSE) {
  X <- runif(nrep, 0 ,1)
  for (i in 1:9)
    X <- cbind(X, runif(nrep, 0, 1), 10)
  Y <- X[, 1] + X[, 2] + X[, 3] + 0.1 * X[, 4] + 0.01 * X[, 5]
  if (sig == TRUE) {
    sigTest(Y, X)
  } else {
    prsm(Y, X[, 1:10], predacc = ar2, printdel = T)
    cat("\n")
    # prsm(Y, X[, 1:10], predacc = aiclogit, printdel = T) # error: y values must be 0 <= y <= 1
  }
}

test(1000)

