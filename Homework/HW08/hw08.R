library(ISLR2)
library(cluster)

data("USArrests")

hc.complete <- hclust(dist(USArrests), method = "complete")

plot(hc.complete, main = "Complete")

print(cutree(hc.complete, 3))

scaled_data <- scale(USArrests)

hc.complete <- hclust(dist(scaled_data), method = "complete")

plot(hc.complete, main = "Complete")

print(cutree(hc.complete, 3))
