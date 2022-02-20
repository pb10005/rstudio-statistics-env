# https://qiita.com/japanesebonobo/items/d6e072f43aac8ebee685
pilots.pca <- princomp(pilots[,2:7], cor = FALSE)
print(summary(pilots.pca))
biplot(pilots.pca)
print(pilots.pca$loadings)