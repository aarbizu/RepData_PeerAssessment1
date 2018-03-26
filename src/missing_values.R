library(dplyr)

imputedDF <- activityDF

imputedDF <- inner_join(imputedDF, interval_mean, by = "interval")

imputedDF[is.na(imputedDF$steps),1] <- imputedDF[is.na(imputedDF$steps),4]

imputed_daily_sum <- aggregate(imputedDF$steps, 
                               list(Date=imputedDF$date), 
                               FUN=sum, 
                               simplify=TRUE)

names(imputed_daily_sum) <- c("Date", "steps")

g <- ggplot(imputed_daily_sum, aes(steps)) + 
    geom_histogram(na.rm=TRUE, binwidth = 5000) +
    xlab("Daily Step Total") +
    ylab("Occurrences")

print(g)