players <- read.table("Baseball.dat", header = T) # page 318

# sample estimate is inside the Coeff - Estimate: 
# lm() 通过 sample data Xi, Yi给出拟合的 Y 的函数
# 即对 population's regression function 的拟合 function
summary(lm(players$Weight ~ players$Height)) # only consider Height
lmout <- lm(players$Weight ~ players$Height + players$Age)
summary(lmout) # add Age
vcov(lmout)