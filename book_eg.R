players <- read.table("Baseball.dat", header = T) # page 318
# head(players)
# players$Weight

function <- verifyEstimate() {
  w <- 0
  for (i in 1:nrow(players)) {
    # print(players$Weight[i])
    if (i == 641)
      w <- w + 0
    else
      w <- w + players$Weight[i]
  }
  
  meanW <- (w/(nrow(players) - 1))
  meanH <- (sum(players$Height)/nrow(players))
  print(-155.092 + 4.841 * meanH)
  print(meanW)
}

# sample estimate is inside the Coeff - Estimate: 
#  sample's mean height based on sample's mean weight
# verification
# verifyEstimate()
summary(lm(players$Weight ~ players$Height)) 
