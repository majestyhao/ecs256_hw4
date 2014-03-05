# pm <- read.table("pima-indians-diabetes.data", header = T, sep = ",", row.names=NULL)
# print(head(pm))
Y <- pm[, 9]
# if (ncol(a) != NULL)
# print(rep(0, ncol(a)))
# a <- cbind(1, 2, 3)
# print(ncol(a))
X <- NULL
X <- cbind(X, pm[, 1], pm[, 2], pm[, 6], pm[, 7])
# X <- cbind(X, pm[, 1], pm[, 2], pm[, 3], pm[, 6], pm[, 7], pm[,8])
# print(ncol(X))
print(summary(lm(Y ~ X)))
# lmout <- summary(lm(Y ~ X[, 2]))
# print(as.name(rownames(lmout$coefficients)[2]))


