# Explore a new approach for Model selection probelm: select predictor
# 1. choose a prediction accuracy criterion and k: R2 & AIC, incorprates 
# protection against overfitting; 
# 2. starting with the full model (use all avail preodicator)， del one 
# at a time as long as the PAC doesn't deteriorate much (k)
# 3. handle even n is large
prsm <- function(Y, X, k = 0.01, predacc = ar2, crit = NULL, printdel = F) {
  # y: The vector of response values in the data.
  # x: The matrix of predictor values.
  # k: The "almost" measure, discussed above.
  # predacc: The PAC function, with arguments y and x.
  # crit: Either "max" or "min"
  # printdel: "progress report" as the computation proceeds. 
  
  PAC <- rep(0, ncol(X) + 1)
  pN <- ncol(X)
  Xb <- X # backup for X
  drop <- cbind(0) # predicator for drop
  startPoint <- pN/2 # why start from here..
  delFlag <- FALSE
  conFlag <- TRUE
  i <- startPoint # company with startpoint
  reserve <- NULL # print result
  
  if (identical(predacc, ar2)) {
    PAC[pN + 1] <- ar2(Y, X)
  } else {
    PAC[pN + 1] <- aiclogit(Y, X)
  }
  cat("full outcome = ")
  cat(PAC[pN + 1])
  pac <- PAC[pN + 1]
  cat("\n")
  
  while(conFlag) { 
    if (i == startPoint - 1)
      conFlag <- FALSE
    # reconstruct/recombine X
    X <- NULL
    # X <- Xb
    # X[, i] <- NULL # NULL is incorrect...why?
    flag <- FALSE
    for (j in 1:pN) {
      if (j == i)
        flag <- TRUE
      if (ncol(drop) > 1) {
        for(m in 2:ncol(drop)) {
          if (j == drop[1, m]) {
            flag <- TRUE
          }
        }
      }
      if (flag == FALSE) {
        X <- cbind(X, Xb[, j])
      }
      flag <- FALSE       
    }
    
    if (identical(predacc, ar2)) {
      PAC[i] <- ar2(Y, X)
      if (PAC[i]/pac >= 1 - k) {
        delFlag <- TRUE
      } else {
        reserve <- c(reserve, i)
      } 
    } else {
      PAC[i] <- aiclogit(Y, X)
      if (PAC[i]/pac < 1 + k) {
        delFlag <- TRUE
      } else {
        reserve <- c(reserve, i)   
      }
    }
    
    if (delFlag == TRUE) {
      # delete the predicator
      # Xb <- X
      pac <- PAC[i]
      drop <- cbind(drop, i)
      cat("deleted ")
      cat(i)
      cat(": \n new outcome: ")
      cat(PAC[i])         
      cat("\n")
    }  
    delFlag <- FALSE
    i <- i + 1
    if (i == pN + 1)
      i <- 1
  } 
  print(reserve)
}

# adjusted R^2
# as argument "predacc" for prsm()
ar2 <- function(Y, X) {
  # call lm()
  # then call summary() to returns the R^2
  pN <- ncol(X) # less function call.. predictor number
  sampleN <- nrow(X) # sample size
  residualCol <- 10 - pN
  if (residualCol != 0) {
    for (i in 1:residualCol) {
      X <- cbind(X, rep(0, sampleN))
    }
  }
  lmout <- summary(lm(Y ~ X[, 1] + X[, 2] + X[, 3] + X[, 4] + X[, 5] + 
                        X[, 6] + X[, 7] + X[, 8] + X[, 9] + X[, 10]))  
  return(lmout$adj.r.squared) 
}

aiclogit <- function(Y, X) {
  # call glm()
  # summary() for the logistic model, AIC value returned
  pN <- ncol(X) # less function call..
  sampleN <- nrow(X)
  residualCol <- 10 - pN
  if (residualCol != 0) {
    for (i in 1:residualCol) {
      X <- cbind(X, rep(0, sampleN))
    }
  }
  glmout <- glm(Y ~ X[, 1] + X[, 2] + X[, 3] + X[, 4] + X[, 5] + 
                  X[, 6] + X[, 7] + X[, 8] + X[, 9] + X[, 10], family = binomial)
  aic <- summary(glmout)$aic # AIC(glmout)
  return(aic) # well, just habbit..
}


main <- function() {
  pm <- read.table("pima-indians-diabetes.data", header = T, sep = ",", row.names=NULL)
  prsm(pm[, 9], pm[, 1:8], predacc = ar2, printdel = T)
  cat("\n")
  prsm(pm[, 9], pm[, 1:8], predacc = aiclogit, printdel = T)
}

main()
