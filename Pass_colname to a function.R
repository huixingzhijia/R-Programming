
##Example 1
library(dplyr)
library(ggplot2)
set.seed(205)
dat = data.frame(t=rep(1:2, each=10), 
                 pairs=rep(1:10,2), 
                 value=rnorm(20))

# working example
ggplot(dat %>% group_by(pairs) %>%
         mutate(slope = (value[t==2] - value[t==1])/(2-1)),
       aes(t, value, group=pairs, colour=slope > 0)) +
  geom_point() +
  geom_line() +
  stat_summary(fun.y=mean,geom="line",lwd=2,aes(group=1))

# attempt at turning into a function
plotFun <- function(df, groupBy, dv, time) {
  ggplot(df %>% group_by_(groupBy) %>%
           mutate_(slope = interp(~(dv2[time2==2] - dv2[time2==1])/(2-1),
                                  dv2=as.name(dv), 
                                  time2=as.name(time))),
         aes_string(time, dv, group = groupBy, 
                    colour = 'slope > 0')) +
    geom_point() +
    geom_line() +
    stat_summary(fun.y=mean,geom="line",lwd=2,aes(group=1))
}

# plot
plotFun(dat, "pairs", "value", "t")


##Example 2
simpleFunction <- function(dataset, col_name, value){
  require("dplyr")
  require("lazyeval")
  filter_criteria <- interp(~y == x, y = as.name(col_name),x=10)
                            #.values=list(y = as.name(col_name), x = value))
  dataset %>%
    filter_(filter_criteria) %>%
    summarise(mean_speed = mean(speed)) -> dataset
  return(dataset)
}


simpleFunction(cars, "dist", 10)












