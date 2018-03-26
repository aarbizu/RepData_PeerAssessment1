library(ggplot2)

daily_sum <- aggregate(activityDF$steps, 
                       list(Date=activityDF$date), 
                       FUN=sum, 
                       simplify=TRUE)

names(daily_sum) <- c("Date", "steps")

g <- ggplot(daily_sum, aes(steps)) + 
    geom_histogram(na.rm=TRUE, binwidth = 5000) +
    xlab("Daily Step Total") +
    ylab("Occurrences")

print(g)