# https://htsuda.net/stats/regression.html
library( datarium )
mkt <- as.data.frame( marketing )
fit = lm( formula = sales ~ youtube, data = mkt )
result <- summary( fit )
print(result)