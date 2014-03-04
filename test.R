pm <- read.table("pima-indians-diabetes.data", header = T, sep = ",", row.names=NULL)
# print(head(pm))
Y <- pm[, 9]
a <- 0
# if (ncol(a) != NULL)
# print(rep(0, ncol(a)))
# a <- cbind(1, 2, 3)
# print(ncol(a))
X <- NULL
# X <- cbind(X, pm[, 1], pm[, 2], pm[, 6], pm[, 7])
X <- cbind(X, pm[, 1], pm[, 2], pm[, 3], pm[, 6], pm[, 7], pm[,8])
# print(ncol(X))
summary(lm(Y ~ X[, 2]))



# X[, 4] = NULL 
# X[, 5] = NULL
# print(aiclogit(Y, X))

