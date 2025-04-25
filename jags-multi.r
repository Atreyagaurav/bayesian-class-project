library(ggplot2)
library(rjags)

MISSING = FALSE

df <- read.csv('csvs/annual-means.csv')


y_cols = mapply(function(y) sprintf("y_%d", y), 2005:2024)
x <- df$area
y.org <- df[y_cols]
n <- dim(y)[1]
m <- dim(y)[2]

y <- y.org

## generate it once, and run 3 models for comparision
if (MISSING) {
    ## outlet node and leaf nodes cannot be interpolated with both
    ## methods, so let's make syntetic gaps in the nodes with both
    ## output and inputs
    valid.nodes <- c(1, 2, 4, 6, 8, 10, 11) + 1
    valid.nodes.count <- length(valid.nodes)
    gaps.each <- 2
    ## node, year, value
    gaps <- matrix(nrow=valid.nodes.count * gaps.each, ncol=3)
    ind <- 1
    y <- y.org
    for (i in valid.nodes) {
        for (j in sample(1:m, gaps.each)) {
            gaps[ind, ] <- c(i, j, y.org[i,j])
            y[i,j] <- NA
            ind <- ind+1
        }
    }

    ## ## IF you overwrite y and need to recreate the gaps
    ## for (i in 1:dim(gaps)[1]) {
    ##     mi = gaps[i,][1]
    ##     mj = gaps[i,][2]
    ##     y[mi, mj] <- NA
    ## }
}

## valid values: "basic", "linear", "output" and "inputs".
model = "basic"

data.bayes <- list(y = y,
                   x = x,
                   m = m,
                   n = n)

model.smpl <- jags.model(file = sprintf("jags/%s.jags", model),
                         data = data.bayes)

adapt(object = model.smpl,
      n.iter = 10^3)

vars = c("beta", "sigma")
if (model == "linear") {
    vars = c(vars, "tau", "beta0")
}
if (MISSING) {
    vars = c(vars, "y")
}

N = 10^4
n.thin = 10
n.iter = N * n.thin
output = jags.samples(model = model.smpl,
variable.names = vars,
n.iter = n.iter,
thin = n.thin,
)

names(output)

if (MISSING) {
    pdf(sprintf("images/%s-missing.pdf", model), width=4, height=8)
    par(mfrow=c(valid.nodes.count, gaps.each), mai=c(.25,.25,.25,.25))
    for (i in 1:dim(gaps)[1]) {
        mi = gaps[i,][1]
        mj = gaps[i,][2]
        mv = gaps[i,][3]
        vals <- output$y[mi, mj, , 1]
        plot(density(vals), main=bquote(y[.(mi) ~ ',' ~ .(mj+2004)]))
        abline(v=mv, col="red")
    }
    dev.off()
}


if (model == "linear") {
    pdf(sprintf("images/%s-beta.pdf", model), width=7, height=5)
    par(mfrow=c(5,4), mai=c(0.25,0.25,0.25,0.25))
    for (j in 1:m) {
        plot(density(output$beta[j,,1]), main=bquote(beta[.(j)]), xlim=c(0.2, 0.9))
    }
    dev.off()
    
    pdf(sprintf("images/%s-tau.pdf", model), width=7, height=5)
    plot(density(output$tau[,,1]), main=bquote(tau))
    dev.off()
    pdf(sprintf("images/%s-beta0.pdf", model), width=7, height=5)
    plot(density(output$beta0[,,1]), main=bquote(beta[0]))
    dev.off()
} else {
    pdf(sprintf("images/%s-beta.pdf", model), width=7, height=5)
    plot(density(output$beta[,,1]), main=expression(beta))
    dev.off()
}


## pdf(sprintf("images/%s-alpha.pdf", model), width=7, height=5)
## plot(density(output$alpha[,,1]), main=expression(alpha))
## dev.off()


pdf(sprintf("images/%s-sigma.pdf", model), width=7, height=5)
plot(density(output$sigma[,,1]), main=expression(sigma))
dev.off()

## plot(output$beta[,,1], type='l')

main.ind <- c(0, 6, 10, 11, 12) + 1
main.area <- x[main.ind]
main.y <- y[main.ind, 1]
main.xy <- data.frame(area=main.area, value=main.y)

axis <- exp(seq(log(90), log(6000), length.out = 50))


calc.y <- function(val) {
    if (model == "linear") {
        y.new <- output$beta[1,,1] * val + mapply(function(s) rnorm(1, 0, s), val * output$sigma[,,1])
    }
    return (quantile(y.new, c(0.025, 0.25, 0.5, 0.75, 0.975)))
}


intervals <- as.data.frame(t(mapply(calc.y, axis)))
names(intervals) <- c("lower", "q1", "median", "q3", "upper")

intervals$area <- axis


if (model == "linear") {
    ## currently only supported for linear, other models need more thoughts
    pdf(sprintf("images/%s-mainb.pdf", model), width=7, height=4)
    ggplot(data=intervals) + geom_ribbon(mapping=aes(x=area, ymin=lower, ymax=upper), alpha = 0.3) + geom_line(mapping=aes(x=area, y=median), color="blue") + geom_point(data=main.xy, mapping=aes(x=area, y=value), color="red")
    dev.off()
}

area <- x[7]
sf <- y[7,]
main.xy <- data.frame(year=2005:2024, value=as.numeric(sf))

axis <- 2005:2024


calc.y <- function(val) {
    if (model == "linear") {
    y.new <- output$beta[val-2004,,1] * area + mapply(function(s) rnorm(1, 0, s), area * output$sigma[,,1])
    } else if (model == "inputs") {
        z <- as.numeric(y[8,] + y[9,] + y[11,]) / (x[8] + x[9] + x[11])
        y.new <- output$beta[,,1] * area * z[val-2004] + mapply(function(s) rnorm(1, 0, s), area * output$sigma[,,1])
    } else if (model == "output") {
        z <- as.numeric(y[1, ]) / x[1]
        y.new <- output$beta[,,1] * area * z[val-2004] + mapply(function(s) rnorm(1, 0, s), area * output$sigma[,,1])
    }
    
    return (quantile(y.new, c(0.025, 0.25, 0.5, 0.75, 0.975)))
}


intervals <- as.data.frame(t(mapply(calc.y, axis)))
names(intervals) <- c("lower", "q1", "median", "q3", "upper")

intervals$year <- axis

pdf(sprintf("images/%s-node6.pdf", model), width=7, height=4)
ggplot(data=intervals) + geom_ribbon(mapping=aes(x=year, ymin=lower, ymax=upper), alpha = 0.3) + geom_line(mapping=aes(x=year, y=median), color="blue") + geom_point(data=main.xy, mapping=aes(x=year, y=value), color="red")
dev.off()

if (model == "linear") {
    linear.intervals <- intervals
} else if (model == "inputs") {
    inputs.intervals <- intervals
} else if (model == "output") {
    output.intervals <- intervals
}


## only to be run after running the above code for all 3 models
## there is no loop, you have to run them manually
linear.intervals$model <- "linear"
inputs.intervals$model <- "inputs"
output.intervals$model <- "output"

all.intervals <- rbind(linear.intervals, inputs.intervals, output.intervals)

pdf("images/combined-node6.pdf", width=7, height=4)
ggplot(data=all.intervals) + geom_ribbon(mapping=aes(x=year, ymin=lower, ymax=upper, fill=model), alpha = 0.3) + geom_line(mapping=aes(x=year, y=median, color=model)) + geom_point(data=main.xy, mapping=aes(x=year, y=value), color="black")
dev.off()
