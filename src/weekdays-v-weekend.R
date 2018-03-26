imputedDF$date <- as.Date(imputedDF$date)
imputedDF$dayOfWeek <- weekdays(imputedDF$date, c(TRUE))
imputedDF$isWeekend <- sapply(imputedDF$dayOfWeek, 
                              function(x) { 
                                  if(x == "Sat" | x == "Sun") 
                                  { 
                                    "Weekend" 
                                  } else { 
                                    "Weekday"
                                  }
                              },
                              simplify = TRUE
                          )


graph <- ggplot(imputedDF, aes(interval,steps)) + 
         geom_line() + 
         facet_grid(isWeekend ~ .)

print(graph)