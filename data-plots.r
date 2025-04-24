library(ggplot2)
library(reshape2)


## For annual model
df <- read.csv('csvs/annual-means-long.csv')

df$node <- as.factor(df$node)

ggplot(df, mapping=aes(x=year, y=streamflow, color=node)) +
    geom_line() + geom_point() +
    scale_y_continuous(trans='log10')


ggplot(df, mapping=aes(x=area, y=streamflow, color=node)) +
    geom_point() + facet_wrap(~ year) + 
    scale_y_continuous(trans='log10') +
    scale_x_continuous(trans='log10')


## For monthly model: abandoned because months are not iid, and rjags
## does not support circular relationships
df <- read.csv('csvs/node-attrs.csv')
cols <- mapply(function (i) {sprintf("sf_month_%d", i)}, 1:12)

df.long <- melt(df, id.vars=c("name", "area", "INDEX"), measure.vars=cols, variable.name="month", value.name="streamflow")

df.long$month_int <- rep(NA, length(df.long$month))
for (m in 1:12) {
    df.long$month_int[df.long$month == sprintf("sf_month_%d", m)] <- m
}

df.long$node <- as.factor(df.long$INDEX)


ggplot(df.long, mapping=aes(x=month_int, y=streamflow, color=node)) +
    geom_line() + geom_point() +
    scale_y_continuous(trans='log10')


ggplot(df.long, mapping=aes(x=area, y=streamflow, color=node)) +
    geom_point() + facet_wrap(~ month) + 
    scale_y_continuous(trans='log10') +
    scale_x_continuous(trans='log10')


model <- lm(sf_mean ~ -1 + area, data=df)
summary(model)


ggplot(df) + geom_point(mapping=aes(x=area, y=streamflow))  +
    scale_y_continuous(trans='log10') +
    scale_x_continuous(trans='log10')

