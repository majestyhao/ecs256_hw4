pm <- read.table("pima-indians-diabetes.data", header = F, sep = ",")
Y <- pm[, 9]
X <- pm[, 1:8]
# x <- rbind(rep(0, nrow(pm)))
# x <- c(rep(0, nrow(pm)))
ar2(pm[,9], pm[, 1:8])
