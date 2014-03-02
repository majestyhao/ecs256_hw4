# Explore a new approach for Model selection probelm: select predictor
# 1. choose a prediction accuracy criterion and k: R2 & IC, incooroprates 
# protection against overfitting; 
# 2. starting with the full model (use all avail preodicator)， del one 
# at a time as long as the PAC doesn't deteriorate much (how to justify it?)
# 3. handle even n is large
prsm <- function(y, x, k = 0.0.1, predacc = ar2, crit = NULL, printdel + F) {
  # y: The vector of response values in the data.
  # x: The matrix of predictor values.
  # k: The "almost" measure, discussed above.
  # predacc: The PAC function, with arguments y and x.
  # crit: Either "max" or "min", depending on whether good values of the PAC are large or small.
  # printdel: If TRUE, gives a "progress report" as the computation proceeds. Each time a predictor is deleted, the new value of the PAC is printed out, along with the name of the variable.
  # crit: Either "max" or "min", depending on whether good values of the PAC are 
  # large or small.
  # printdel: If TRUE, gives a "progress report" as the computation proceeds. 
  # Each time a predictor is deleted, the new value of the PAC is printed out, along with the name of the variable.
}

# as argument "predacc" for prsm()
ar2 <- function() {
  # call lm()
  # then call summary() to returns the R^2
}

aiclogit <- function() {
  # call glm()
  # summary() for the logistic model, AIC value returned
}

