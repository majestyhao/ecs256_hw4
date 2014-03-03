# Explore a new approach for Model selection probelm: select predictor
# 1. choose a prediction accuracy criterion and k: R2 & AIC, incooroprates 
# protection against overfitting; 
# 2. starting with the full model (use all avail preodicator)， del one 
# at a time as long as the PAC doesn't deteriorate much (how to justify it?)
# 3. handle even n is large
prsm <- function(Y, X, k = 0.01, predacc = ar2, crit = NULL, printdel = F) {
  # y: The vector of response values in the data.
  # x: The matrix of predictor values.
  # k: The "almost" measure, discussed above.
  # predacc: The PAC function, with arguments y and x.
  # crit: Either "max" or "min", depending on whether good values of the PAC
  # are large or small.
  # printdel: If TRUE, gives a "progress report" as the computation proceeds. 
  # Each time a predictor is deleted, the new value of the PAC is printed out, 
  # along with the name of the variable.
  PAC <- rep(0, ncol(X))
  if (identical(predacc, ar2)) {
    for (i in ncol(X):1) {
      PAC[i] <- ar2(Y, X)
      X[, i] = NULL
    }
    if (printdel == T)
      print(PAC)
  }  else {
    for (i in ncol(X):1) {
      PAC[i] <- aiclogit(Y, X)
      X[, i] = NULL
    }
    if (printdel == T)
      print(PAC)
  }
}

# adjusted R^2
# as argument "predacc" for prsm()
ar2 <- function(Y, X) {
  # call lm()
  # then call summary() to returns the R^2
  pN <- ncol(X) # less function call.. predictor number
  sampleN <- nrow(X) # sample size
  residualCol <- 8 - pN
  # print(residualCol)
  if (residualCol != 0) {
    for (i in 1:residualCol) {
      X[, pN + i] = c(rep(0, sampleN))
      # X[, i] = NULL
    }
  }
  # print(X)
  lmout <- lm(Y ~ X[, 1] + X[, 2] + X[, 3] + X[, 4] + X[, 5] + 
                X[, 6] + X[, 7] + X[, 8])
  # summary()
  Yh <- rep(0, sampleN) # Y-hat
  for (j in 1:sampleN) {
    for (i in 1:pN) {
      if (i == 0) {
        Yh[j] <- lmout$coefficients[i] # intercept
      } else {
        Yh[j] <- Yh[j] + lmout$coefficients[i + 1] * X[j, i]
      }       
    }
  }
  r2 <- 1 - sum((Y - Yh) ^ 2)/sum((Y - mean(Y)) ^ 2)
  ar2 <- 1 - (1 - r2) * (sampleN - 1)/(sampleN - pN - 1) # adjusted r2
  # print(r2)
  return(ar2) 
}

aiclogit <- function(Y, X) {
  # call glm()
  # summary() for the logistic model, AIC value returned
  pN <- ncol(X) # less function call..
  sampleN <- nrow(X)
  residualCol <- 8 - pN
  # print(residualCol)
  if (residualCol != 0) {
    for (i in 1:residualCol) {
      X[, pN + i] = c(rep(0, sampleN))
      # X[, i] = NULL
    }
  }
  # print(X)
  glmout <- glm(Y ~ X[, 1] + X[, 2] + X[, 3] + X[, 4] + X[, 5] + 
                  X[, 6] + X[, 7] + X[, 8])
  # print(summary(glmout))
  # print(glmout)
  aic <- AIC(glmout)
  return(aic) # well, just habbit..
}

main <- function() {
  pm <- read.table("pima-indians-diabetes.data", header = F, sep = ",")
  prsm(pm[, 9], pm[, 1:8], predacc = ar2, printdel = T)
  # prsm(pm[,9], pm[, 1:8], predacc = aiclogit, printdel = F)
}

main()
