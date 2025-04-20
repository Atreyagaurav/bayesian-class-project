library(ggplot2)
library(rjags)


df <- read.csv('node-attrs.csv')

## x <- (df$area - mean(df$area)) / sd(df$area)
## y <- (df$sf_mean - mean(df$sf_mean)) / sd(df$sf_mean)
x <- df$area
y <- df$sf_mean

model.input="
data {
  n <- length(y)
}
model {
# sampling distribution
for (i in 1:n) {
  y[i] ~ dnorm(x[i] * (beta + alpha * z[i]), 1 / sigma^2)
}
# connection to inputs
z[1] <- (y[2] + y[7]) / (x[2] + x[7])
z[2] <- (y[3] + y[5]) / (x[3] + x[5])
z[3] <- (y[4]) / (x[4])
z[4] <- 0
z[5] <- (y[6]) / (x[6])
z[6] <- 0
z[7] <- (y[8] + y[9] + y[11]) / (x[8] + x[9] + x[11])
z[8] <- 0
z[9] <- (y[10]) / (x[10])
z[10] <- 0
z[11] <- (y[12]) / (x[12])
z[12] <- (y[13]) / (x[13])
z[13] <- 0
# parameter priors
alpha ~ dunif(0, 1)
beta ~ dunif(0, 1)
sigma ~ dgamma(1, gamma0)
}
"

model.output="
data {
  n <- length(y)
}
model {
# sampling distribution
for (i in 1:n) {
  y[i] ~ dnorm(x[i] * (beta + alpha * z[i]), 1 / sigma^2)
}
# connection to output
z[1] <- 0
z[2] <- y[1] / x[1]
z[3] <- y[2] / x[2]
z[4] <- y[3] / x[3]
z[5] <- y[2] / x[2]
z[6] <- y[5] / x[5]
z[7] <- y[1] / x[1]
z[8] <- y[7] / x[7]
z[9] <- y[7] / x[7]
z[10] <- y[9] / x[9]
z[11] <- y[7] / x[7]
z[12] <- y[11] / x[11]
z[13] <- y[12] / x[12]
# parameter priors
alpha ~ dunif(0, 1)
beta ~ dunif(0, 1)
sigma ~ dgamma(1, gamma0)
}
"

model.linear="
data {
  n <- length(y)
}
model {
# sampling distribution
for (i in 1:n) {
  y[i] ~ dnorm(x[i] * beta, 1 / sigma^2)

}
# parameter priors
alpha <- 0
beta ~ dunif(0, 1)
sigma ~ dgamma(1, gamma0)
}
"

data.bayes <- list(y = y,
                   x = x,
                   gamma0 = 2)

model.smpl <- jags.model( file = textConnection(model.both),
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

plot(output$beta[,,1], type='l')
