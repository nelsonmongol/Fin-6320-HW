setwd("C:\\Users\\nelsonmongol\\Desktop\\Spring 2018\\Fin6320\\data")
rawData <- read.csv("SP500.csv", header = T)
rawData$Date <- as.Date(as.character(rawData$Date), format = "%d-%b-%y")

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
resids[1] <- mean(resids, na.rm = T)
hist(resids, breaks = 50)

cond.vols <- fitted(fit)[,1]
cond.vols[1] <- mean(dev)
hist(cond.vols, breaks = 50)


#Historical Simulation

n <- length(ret)
r2 <- ret * ret
s2 <- cond.vols * cond.vols
r0 <- r2[n]
s0 <- s2[n]
hist(s2, breaks = 50)

steps <- 10
B <- 10000
r <- matrix(0, ncol = steps+1, nrow = B)
st <- matrix(0, ncol=steps+1, nrow = B)
Z <- matrix(0, ncol=steps+1, nrow = B)


for(i in 1:B)
{
  z <- sample(resids, size = 11, replace = T)
  st[i,1] <- w + a * r0 + b * s0
  r[i,1] <- z[1] * sqrt(st[i,1])
  Z[i, ] <- z
  for(t in 2:steps)
  {
    st[i,t] <- w + a * (r[i,t-1] * r[i,t-1]) + b * st[i,t-1]
    r[i,t] <- z[t] * sqrt(st[i,t])
  }
}

ret.pred <- apply(r[,2:(steps+1)], 1, sum)
hist(ret.pred, breaks = 50)

alpha <- c(0.01, 0.05, 0.1)

VaR <- quantile(ret.pred, probs = alpha)
