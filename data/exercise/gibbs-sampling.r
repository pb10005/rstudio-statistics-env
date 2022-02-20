# https://toukeier.hatenablog.com/entry/gibbs-sampler-by-r/
library(pscl)

# 観測
heikin <- c(6.0,10.0,7.6,3.5,1.4,2.5,5.6,3.0,2.2,5.0,
            3.3,7.6,5.8,6.7,2.8,4.8,6.3,5.3,5.4,3.3,
            3.4,3.8,3.3,5.7,6.3,8.4,4.6,2.8,7.9,8.9)
mean1 <- mean(heikin)
n <- length(heikin)

x <- seq(0.05,10,0.05)

# 事前分布は逆ガンマ分布
curve(densigamma(x,alpha=0.01, beta=0.01),from=0.05,to=10,lwd=3)

n0 <- 2*0.01
n0S0 <- 2*0.01
S0 <- n0S0/n0

mu0 <- 5
m0 <- 1/4

m1 <- m0+n
n1 <- n0+n　
mu1 <- (n*mean1+m0*mu0)/(m0+n)
Q <- sum((heikin-mean1)^2)
n1S1 <- n0S0+Q+m0*n/(m0+n)*(mean1-mu0)^2

# 分散のサンプリング
# 周辺密度は逆ガンマ分布
my.sigma2 <- function(mu){
  sigma2 <- rigamma(1, alpha=(n1+1)/2, beta=(n1S1+m1*(mu-mu1)^2)/2)
  return(sigma2)
}

# 平均のサンプリング
# 周辺密度は正規分布
my.mu <- function(sigma2){
  mu <- rnorm(1, mean=mu1, sd=sqrt(sigma2/m1))
  return(mu)
}

my.montecarlo <- function(n){
  set.seed(2021018)
  mu <- 5
  new.mu <- 5
  sigma2 <- 4
  for (i in 1:n){
    new.sigma2 <- my.sigma2(new.mu)
    sigma2 <- sigma2 + new.sigma2
    new.mu <- my.mu(new.sigma2)
    mu <- mu + new.mu
  }
  return(list("mu"=mu/n, "sigma2"=sigma2/n))
}

result <- my.montecarlo(100000)
print(result)
curve(densigamma(x, alpha=(n1+1)/2, beta=(n1S1+m1*(result$mu-mu1)^2)/2),from=0.01,to=8.0, ylim=c(0,0.4),lwd=3)
curve(dnorm(x, mean=mu1, sd=sqrt(result$sigma2/m1)),from=0,to=10,lwd=3)
