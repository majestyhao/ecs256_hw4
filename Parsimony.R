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
  
  if (!is.matrix(X)) 
    X <- as.matrix(X)
  
  if (!is.matrix(Y)) 
    Y <- as.matrix(Y)
  
  PAC <- rep(0, ncol(X) + 1)
  pN <- ncol(X)
  Xb <- X # backup for X
  # drop <- cbind(0) 
  drop <- c(rep(FALSE, pN)) # predicator for drop
  delFlag <- FALSE
  minP <- 0 # min influential PAC 
  
  if (identical(predacc, ar2)) {
    PAC[pN + 1] <- ar2(Y, X)$adj.r.squared
    minP <- pN + 1
  } else {
    PAC[pN + 1] <- aiclogit(Y, X)$aic
    minP <- pN + 1
  }
  cat("full outcome = ")
  cat(PAC[pN + 1])
  pac <- PAC[pN + 1]
  cat("\n")
  
  for (it in 1: pN) {  
    # reconstruct/recombine X
    Xb2 <- NULL
    # X <- Xb
    # X[, i] <- NULL # NULL is incorrect...why?
    for (j in 1:pN) {
      if (drop[j] == FALSE) 
        Xb2 <- cbind(Xb2, Xb[, j])      
    }
    pac <- rep(0, ncol(Xb2))
    
    for (i in 1:ncol(Xb2)) {
      # reconstruct/recombine X
      X <- NULL
      for (j in 1:ncol(Xb2)) {
        if (j != i) 
          X <- cbind(X, Xb2[, j])      
      }
      
      if (identical(predacc, ar2)) {      
        # mname <- rownames(ar2(Y, X)$coefficients)[match(max(ar2(Y, X)$coefficients[, 4]), ar2(Y, X)$coefficients[, 4]) - 1]
        # print(as.name(mname))
        pac[i] <- ar2(Y, X)$adj.r.squared
      } else {
        pac[i] <- aiclogit(Y, X)$aic              
      }
    }
    
    if (identical(predacc, ar2)) {
      crit <- min
    } else {      
      crit <- max
    }
    
    if (identical(crit, min)) {
      nminP <- match(max(pac), pac)
      if (pac[nminP]/PAC[minP] >= 1 - k) {
        delFlag <- TRUE
      } 
    } else {
      nminP <- match(min(pac), pac)
      if (pac[nminP]/PAC[minP] < 1 + k) {
        delFlag <- TRUE
      }
    }
    counter <- 1
    for (m in 1:pN) {
      if (drop[m] == FALSE) {          
        if (counter == nminP) {
          nminP <- m
          break
        }
        counter <- counter + 1
      }
    }
    PAC[nminP] <- pac[counter]
    
    if (delFlag == TRUE) {
      # delete the predicator
      # Xb <- X
      drop[nminP] = TRUE
      minP <- nminP
      if (printdel == TRUE) {
        cat("deleted ")
        cat(colnames(Xb)[nminP])
        cat("\n new outcome = ")
        cat(PAC[nminP])         
        cat("\n")
      }
    }  else {
      break
    }
    delFlag <- FALSE      
  } 
  
  cat("reserved: ")
  for (i in 1:pN) 
    if (drop[i] == FALSE) {
      cat(i)
      cat(" ")
    }
  cat("\n")
}

# adjusted R^2
# as argument "predacc" for prsm()
ar2 <- function(Y, X) {
  # call lm()
  # then call summary() to returns the R^2
  X1 <- NULL
  for (i in 1:ncol(X))
    X1 <- cbind(X1, X[, i])
  lmout <- summary(lm(Y ~ X1))
  # or: lmout <- summary(lm(Y ~ ., data = X), if X is a data frame  
  return(lmout) 
}

aiclogit <- function(Y, X) {
  # call glm()
  # summary() for the logistic model, AIC value returned
  X1 <- NULL
  for (i in 1:ncol(X))
    X1 <- cbind(X1, X[, i])
  glmout <- summary(glm(Y ~ X1, family = binomial))
  # or: glmout <- summary(glm(Y ~ ., data = X , family = binomial)), if X is a data frame
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
