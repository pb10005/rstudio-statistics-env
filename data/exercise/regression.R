# https://htsuda.net/stats/regression.html
library( datarium )
library( car )

mkt <- as.data.frame( marketing )
fit = lm( formula = sales ~ youtube, data = mkt )
result <- summary( fit )
print(result)

# Q-Q plot
fit = lm( sales ~ youtube + facebook + newspaper, data = mkt )
qqPlot( fit, labels = row.names( mkt ), id.method = "identity", simulate = T )
newfit = lm( sales ~ youtube + facebook + newspaper, data = mkt[ -c( 6, 131 ), ] )
qqPlot( newfit, labels = row.names( mkt ), id.method = "identity", simulate = T )

# Durbin-Watson test
print(car::durbinWatsonTest( fit ))

# Component plus residual plots
par( mar = c( 4.5, 4.5, 3, 1 ) )
car::crPlots( fit )