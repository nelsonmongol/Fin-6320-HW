setwd("C:\\Users\\nelsonmongol\\Desktop\\Spring 2018\\Fin6320")
rawData <- read.csv("SP500.csv", header = T)
dt <- paste(as.character(rawData$Date), "20", sep = "-")
rawData$Date <- as.Date(dt, format = "%b-%y-%d")
head(rawData)

ret <- diff(log(rawData$SP500))

r.bar <- mean(ret)
dev <- (ret - r.bar) ^ 2

library(tseries)
fit <- garch(ret, order = c(1,1))
theta <- coef(fit)
w <- theta[1]
a <- theta[2]
b <- theta[3]

resids <- residuals(fit)
hist(resids, breaks = 50)

cond.vols <- fitted(fit)[,1]
cond.vols[1] <- mean(dev)
hist(cond.vols, breaks = 50)


#Stationary Bootstrap

n <- length(ret)
r2 <- ret
s2 <- cond.vols * cond.vols
r0 <- r2[n]
s0 <- s2[n]

s1 <- w + a * r0 + b * s0

library(urca)
adf.s1 <- ur.df(y = ret, type = "none", selectlags = "BIC")
summary(adf.s1)

steps <- 10
reps <- 10000
m <- 10
q <- 1 / m
r <- matrix(0, nrow = reps, ncol = steps)

for(b in 1:reps)
{
  u <- sample(1:steps, size = steps, replace = TRUE)
  for(t in 2:steps)
  {
    v <- runif(1)
    if(v < q) { u[t] <- sample(1:steps, size = 1) }
    else {
      u[t] <- u[t-1] + 1
      if(u[t] > steps) { u[t] <- u[t] - steps }
    }
  }
  r[b,] <- ret[u]
}

ret.pred <- apply(r[,2:(steps)], 1, sum)
hist(ret.pred, breaks = 50)

alpha <- c(0.01, 0.05, 0.1)

VaR <- quantile(ret.pred, probs = alpha)