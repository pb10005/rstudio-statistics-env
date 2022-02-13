# https://qiita.com/Haruka-Ogawa/items/fcda36cc9060ba851225

data <- iris[,1:4]
distance <- dist(data)
hc <- hclust(distance, method = "ward.D2")

plot(hc)

result <- cutree(hc,k=3)

answer <- iris[,5]
table <- table(answer, result)

print(table)