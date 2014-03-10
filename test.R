pima <- read.table("pima-indians-diabetes.data", header = T, sep = ",", row.names=NULL)
# pima <- data.frame("NPreg"=pima[,1], "Gluc"=pima[,2], "BP"=pima[,3], "Thick"=pima[,4], "Insul"=pima[,5], "BMI"=pima[,6], "Genet"=pima[,7], "Age"=pima[,8], "Class"=pima[,9])
colnames(pima)[1]
# print(pm$header)
# headP <- head(pm)
#print(headP)
# Y <- pm[, 9]
# if (ncol(a) != NULL)
# print(rep(0, ncol(a)))
# a <- cbind(1, 2, 3)
# print(ncol(a))
# X <- NULL
# print(head(Y))
# X <- cbind(X, pm[, 1], pm[, 2], pm[, 3], pm[, 4], pm[, 5], pm[,6], pm[,7], pm[,8])
# print(ncol(X))
# lmout <- summary(lm(pima[, 9] ~ pima[, 8]))
# print(lmout)

# lmout <- summary(lm(Y ~ X[, 2]))
# print(as.name(rownames(lmout$coefficients)[2]))
