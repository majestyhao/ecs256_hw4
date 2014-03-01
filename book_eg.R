players <- read.table("Baseball.dat", header = T) # page 318
# head(players)
# players$Weight

summary(lm(players$Weight ~ players$Height))