load("FinelineNumber.RData")

fine.t <- t(Fine_m)

kmean.fine <- kmeans(fine.t, centers = 20, iter.max = 50, nstart = 10, algorithm = "Lloyd")
