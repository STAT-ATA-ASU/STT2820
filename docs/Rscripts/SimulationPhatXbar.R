# Chapter 17 simulation of phat to find sampling distribution

phatfunc <- function(m, p, n) {
  trialvals <- sample(c(0, 1), m*n, replace = TRUE, prob = c(1 - p, p))
  trialmat <- matrix(trialvals, ncol = n)
  
  NumSuccesses <- apply(trialmat, 1, sum)
  Phats <- NumSuccesses/n
  
  hist(Phats, breaks = seq(from = min(Phats), to = max(Phats), by = 1/n))
  
  #sampled mean and sd of phats
  cat("mean of phat =", mean(Phats))
  cat("\nSD of phat =", sd(Phats))
}

phatfunc(m = 10000, p = 0.6, n = 81)
phatfunc(m = 10000, p = 0.8, n = 256)
phatfunc(m = 10000, p = 0.3, n = 625)

p <- c(0.6, 0.8, 0.3)
n <- c(81, 256, 625)
CheckSDphat <- sqrt(p*(1-p)/n)
CheckSDphat


#Simulation of xbar to find sampling distribution

estSD <- numeric(3)

#Normal Distribution
mu <- 10 
sigma <- 18 
n <- 81 

m <- 100000 #number of experiments

trialvals <- rnorm(m*n, mu, sigma)

trialmat <- matrix(trialvals, ncol = n)  

xbars <- apply(trialmat, 1, mean)

hist(xbars)
plot(density(xbars))
mean(xbars)
sd(xbars)
estSD[1] <- sd(xbars)

#Binomial Distribution
n <- 100
p <- 0.2

mu <- n*p
sigma <- sqrt(n*p*(1 - p))
mu
sigma

m <- 100000 #number of experiments

trialvals <- rbinom(m*n, n, p)

trialmat <- matrix(trialvals, ncol = n)  

xbars <- apply(trialmat, 1, mean)

hist(xbars)
plot(density(xbars))
mean(xbars)
sd(xbars)
estSD[2] <- sd(xbars)

#Exponential Distribution
mu <- 10
sigma <- 10
n <- 625

m <- 100000
trialvals <- rexp(m*n, rate = 1/mu)

trialmat <- matrix(trialvals, ncol = n)  

xbars <- apply(trialmat, 1, mean)

hist(xbars)
plot(density(xbars))
mean(xbars)
sd(xbars)
estSD[3] <- sd(xbars)
#mean of xbar = ?

sigmaPOP <- c(18, 4, 10)
n <- c(81, 100, 625)

sigmaPOP/sqrt(n)
estSD
