# Author: Wenhui Zeng
# Title: Week 5 Exercises

# Part 1: Base Graphics Package Exercises
# Don't forget to save the default graphics parameters.
opar <- par()

# 1. As part of Week 2's exercises, we looked at how the growth of Tetrahymena 
# cells were relateded to the concentration of those cells? Import the
# "hellung.csv" data once again, then use par(mfrow=...) to split a graphics
# window into two columns. Then make two histograms: one showing the
# distribution of the concentration variable, and one showing a log 
# trasformation of the concentration variable.
hellung<-read.csv("hellung.csv")

par(mfrow = c(1,2)) # specifies we want 1 row and 2 columns.change the background

# Now do our boxplots.
hist(hellung$conc, ylab = "concentration")
hist(sqrt(hellung$conc), ylab = "log(con)")


par(opar)
# 2. Create a scatterplot of cell diameter as a function of the log of cell 
# concentration. Then drop on a linear trendline and a 95% CI for the trendline. 


# Hint: Rememeber that to make the CI curves, we will need to make predictions
# from the linear model. Because of this it will be easiest to create a
# transformed version of concentration within the data frame, rather than doing
# transformation within the model, i.e. first run something like this:

hellung$log.conc <- log(hellung$conc)

hellung.lm <- lm(log.conc ~ diameter, data =hellung)
plot(log.conc ~ diameter, data = hellung)
pred.frame <- data.frame(diameter = 19:27)

pp <- predict(hellung.lm, int = "p", newdata = pred.frame) 

# we have fitted value for age 1,2....

# ...anbd calculate the confidence interval of line:
pc <- predict(hellung.lm, int = "c", newdata = pred.frame)

plot(log.conc ~ diameter, data =hellung, 
     ylim = range(hellung$log.conc, pp, na.rm = T), 
     ylab = "diameter", xlab="log(con)")
matlines(pred.frame$diameter, pc, lty=c(1,2,2), col="black")
matlines(pred.frame$diameter, pp, lty=c(1,3,3), col="black")


# Recal also from our Week 2 exercises our ANOVA model of red cell folate levels
# by ventilation treatment group ("red.cell.folate.csv").
red.cell<-read.csv("red.cell.folate.csv")

# 3. Illustrate the distribution of folate levels, stratified by ventilation 
# using the boxplot() function, ...
boxplot(folate ~ ventilation, data = red.cell, ylab = "ventilation", 
        xlab = "folate")

# 4. ...then compare the means and confidence intervals of folate by ventilation
# using a vertical stripchart().

stripchart(folate ~ ventilation, data = red.cell, method = "jitter", vert = T,
           ylab = "ventilation", 
           xlab = "folate",
           col = "gray", pch = 1)


m <- tapply(red.cell$folate, red.cell$ventilation, mean, na.rm = T)
s <- tapply(red.cell$folate, red.cell$ventilation, sd, na.rm = T)

# Instead, use this to count sample sizes by ventilation: 
n <- tapply(!is.na(red.cell$folate),red.cell$ventilation, sum)
ci <- 2*s/sqrt(n)
arrows(1:3, m+ci, 1:3, m-ci, angle = 90, code = 3, length = 0.1)

lines(1:3, m, type = "l")

# Part 2: ggplot2 Graphics
library(ggplot2)

# 5. Recreate the histograms of concentration and log concentration in ggplot2.
# (These do not have to be plotted in the same window as above.
 
 #Recreate the histograms of concentration in ggplot2
 conc.diameter  <- ggplot(data = hellung)
 conc.diameter + geom_histogram(aes(x = conc))
 
#Recreate the histograms of log concentration in ggplot2 
 log.conc.diameter  <- ggplot(data = hellung)
 log.conc.diameter + geom_histogram(aes(x = log.conc))

# 6. Show a scatterplot, trendline, and CI of diameter by concentration.
 con.dia<- ggplot(data = hellung, aes(x = conc, y = diameter))
 
 # Now add a Geom layer: points
 con.dia + geom_point()+geom_smooth()
 
# 7. Show boxplots of red cell folate, stratified by ventilation treatment.
redcell <- ggplot(data = red.cell)
redcell  + geom_boxplot(aes(y = folate, x = ventilation))
 
# 8. Show means and CI of red cell folate stratified by ventilation treatent.
redcell + aes(y = folate, x = ventilation) + stat_summary(fun.data = "mean_cl_normal")
 
 
 