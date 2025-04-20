library(ggplot2)
library(rjags)


df <- read.csv('node-attrs.csv')

## x <- (df$area - mean(df$area)) / sd(df$area)
## y <- (df$sf_mean - mean(df$sf_mean)) / sd(df$sf_mean)
x <- df$area
y <- df[(length(df)-11):length(df)]
n <- dim(y)[1]
m <- dim(y)[2]

model.input="
model {
# sampling distribution
for (j in 1:m) {
  for (i in 1:n) {
    y[i,j] ~ dnorm(x[i] * (beta + alpha * z[i,j]), 1 / sigma^2)
  }
  # connection to inputs
  z[1,j] <- (y[2,j] + y[7,j]) / (x[2] + x[7])
  z[2,j] <- (y[3,j] + y[5,j]) / (x[3] + x[5])
  z[3,j] <- (y[4,j]) / (x[4])
  z[4,j] <- 0
  z[5,j] <- (y[6,j]) / (x[6])
  z[6,j] <- 0
  z[7,j] <- (y[8,j] + y[9,j] + y[11,j]) / (x[8] + x[9] + x[11])
  z[8,j] <- 0
  z[9,j] <- (y[10,j]) / (x[10])
  z[10,j] <- 0
  z[11,j] <- (y[12,j]) / (x[12])
  z[12,j] <- (y[13,j]) / (x[13])
  z[13,j] <- 0
}
# parameter priors
alpha ~ dunif(0, 1)
beta ~ dunif(0, 1)
sigma ~ dgamma(1, gamma0)
}
"

model.output="
model {
# sampling distribution
for (j in 1:m) {
  for (i in 1:n) {
    y[i,j] ~ dnorm(x[i] * (beta + alpha * z[i,j]), 1 / sigma^2)
  }
  # connection to output
  z[1, j] <- 0
  z[2, j] <- y[1, j] / x[1]
  z[3, j] <- y[2, j] / x[2]
  z[4, j] <- y[3, j] / x[3]
  z[5, j] <- y[2, j] / x[2]
  z[6, j] <- y[5, j] / x[5]
  z[7, j] <- y[1, j] / x[1]
  z[8, j] <- y[7, j] / x[7]
  z[9, j] <- y[7, j] / x[7]
  z[10, j] <- y[9, j] / x[9]
  z[11, j] <- y[7, j] / x[7]
  z[12, j] <- y[11, j] / x[11]
  z[13, j] <- y[12, j] / x[12]
}
# parameter priors
alpha ~ dunif(0, 1)
beta ~ dunif(0, 1)
sigma ~ dgamma(1, gamma0)
}
"

model.linear="
model {
# sampling distribution
for (j in 1:m) {
  for (i in 1:n) {
    y[i,j] ~ dnorm(x[i] * beta[j], 1 / sigma^2)
  }
  beta[j] ~ dunif(0, 5)
}
# parameter priors
alpha <- 0
sigma ~ dgamma(1, gamma0)
}
"

data.bayes <- list(y = y,
                   x = x,
                   m = m,
                   n = n,
                   gamma0 = 2)

model.smpl <- jags.model( file = textConnection(model.output),
                         data = data.bayes)

adapt(object = model.smpl,
      n.iter = 10^3)

N = 10^4
n.thin = 10
n.iter = N * n.thin
output = jags.samples(model = model.smpl,
variable.names = c("beta", "alpha", "sigma"),
n.iter = n.iter,
thin = n.thin,
)

names(output)

plot(density(output$beta[,,1]))

plot(density(output$alpha[,,1]))


plot(density(output$sigma[,,1]))

## plot(output$beta[,,1], type='l')
