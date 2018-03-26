library(ggplot2)

interval_mean <- aggregate(activityDF$steps,
                          list(interval=activityDF$interval),
                          FUN=mean, na.rm=T)

names(interval_mean) <- c("interval", "steps")

g <- ggplot(interval_mean, aes(interval,steps)) + geom_line() + ylab("steps")

print(g)

