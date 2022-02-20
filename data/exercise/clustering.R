# https://qiita.com/Haruka-Ogawa/items/fcda36cc9060ba851225
library(cluster)

data <- iris[,1:4]
distance <- dist(data)
hc <- hclust(distance, method = "ward.D2")

plot(hc)

result <- cutree(hc,k=3)

answer <- iris[,5]
table <- table(answer, result)

print(table)

km <- kmeans(data,3)

clusplot(data, km$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

table <- table(answer, km$cluster)

print(table)