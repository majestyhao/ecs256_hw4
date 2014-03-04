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
  # drop <- cbind(0) # predicator for drop
  drop <- c(rep(FALSE, pN))
  delFlag <- FALSE
  conFlag <- TRUE
  maxT <- 0 # Pr(>|t|)
  i <- 1
  
  if (identical(predacc, ar2)) {
    PAC[pN + 1] <- ar2(Y, X)$adj.r.squared
    maxT <- match(max(ar2(Y, X)$coefficients[, 4]), ar2(Y, X)$coefficients[, 4]) - 1
  } else {
    PAC[pN + 1] <- aiclogit(Y, X)$aic
    maxT <- match(max(aiclogit(Y, X)$coefficients[, 4]), aiclogit(Y, X)$coefficients[, 4]) - 1
  }
  cat("full outcome = ")
  cat(PAC[pN + 1])
  pac <- PAC[pN + 1]
  cat("\n")
  
  while(conFlag) { 
    if (i == pN)
      conFlag <- FALSE
    # reconstruct/recombine X
    X <- NULL
    # X <- Xb
    # X[, i] <- NULL # NULL is incorrect...why?
    drop[maxT] <- TRUE
    for (j in 1:pN) {
      if (drop[j] == FALSE) 
        X <- cbind(X, Xb[, j])      
    }
    
    if (identical(predacc, ar2)) {      
      # mname <- rownames(ar2(Y, X)$coefficients)[match(max(ar2(Y, X)$coefficients[, 4]), ar2(Y, X)$coefficients[, 4]) - 1]
      # print(as.name(mname))
      PAC[maxT] <- ar2(Y, X)$adj.r.squared
      nmaxT <- match(max(ar2(Y, X)$coefficients[, 4]), ar2(Y, X)$coefficients[, 4]) - 1      
      counter <- 1
      for (m in 1:pN) {
        if (drop[m] == FALSE) {
          if (counter == nmaxT) {
            nmaxT <- m
            break
          }
          counter <- counter + 1
        }
      }
      
      if (PAC[maxT]/pac >= 1 - k) {
        delFlag <- TRUE
      } else {
        drop[maxT] <- FALSE
      } 
    } else {
      PAC[maxT] <- aiclogit(Y, X)$aic
      nmaxT <- match(max(aiclogit(Y, X)$coefficients[, 4]), aiclogit(Y, X)$coefficients[, 4]) - 1
      counter <- 1
      for (m in 1:pN) {
        if (drop[m] == FALSE) {          
          if (counter == nmaxT) {
            nmaxT <- m
            break
          }
          counter <- counter + 1
        }
      }
      
      if (PAC[maxT]/pac < 1 + k) {
        delFlag <- TRUE
      } else {  
        drop[maxT] <- FALSE
      }
    }
    
    if (delFlag == TRUE) {
      # delete the predicator
      # Xb <- X
      pac <- PAC[maxT]
      cat("deleted ")
      cat(maxT)
      cat(": \n new outcome: ")
      cat(PAC[maxT])         
      cat("\n")
    }  
    delFlag <- FALSE
    i <- i + 1
    maxT <- nmaxT
  } 
  for (i in 1:pN) 
    if (drop[i] == FALSE)
      cat(i)
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
  
  return(lmout) 
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
  glmout <- summary(glm(Y ~ X[, 1] + X[, 2] + X[, 3] + X[, 4] + X[, 5] + 
                          X[, 6] + X[, 7] + X[, 8] + X[, 9] + X[, 10], family = binomial))
  # aic <- summary(glmout)$aic # AIC(glmout)
  return(glmout) # well, just habbit..
}


main <- function() {
  pm <- read.table("pima-indians-diabetes.data", header = T, sep = ",", row.names=NULL)
  prsm(pm[, 9], pm[, 1:8], predacc = ar2, printdel = T)
  cat("\n")
  prsm(pm[, 9], pm[, 1:8], predacc = aiclogit, printdel = T)
}

main()
