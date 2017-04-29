# Author: Wenhui Zeng
# Title: Week 4 Exercises: Functions and Loops

# Part I: Write a Custom Function
# Let's briefely reconsider the Juul data. A logistic regression on this data
# might look something like this.
# First get the data
library(ISwR)
data(juul2) # This copies the data frame from the ISwR package into the 
# Global Environment so we can see it in R Studio's environment window.
# Now, quickly make our factors as we have before:

juul2$menarche <- factor(juul2$menarche, labels = c("No", "Yes"))
juul2$tanner <- factor(juul2$tanner, labels = c("I", "II", "III", "IV", "V"))
# Then we make a glm object of menarche for girls between 8 and 20 years old.
juul2.glm <- glm(menarche ~ age + height + weight + tanner, 
                 data = juul2[juul2$age > 8 & juul2$age < 20, ], 
                 family = binomial("logit"))
# Create and view a summary table of that glm.
glm.summary <- summary(juul2.glm)
glm.summary
# That summary table is pretty good, but I want to see Odds Ratios, so I will
# extract the estimates, their standard errors, and odds ratios from the summary
# table like this. Note that I'm rounding everything in the table to 2 decimal
# places, and using cbind to put everything into a nice table.
round(cbind("Estimate" = glm.summary$coefficients[ , 1], 
            "Std. Error" = glm.summary$coefficients[ , 2],
            "Odds Ratio" = exp(glm.summary$coefficients[ , 1])), 2)

# Now suppose you want to run a series of GLMs. It would be a hassle to re-write
# the scripts for making our OR table every time. 

# Exercise 1: Using the above script example, write a function that makes
# a table of Coefficient Estimates, Std. Errors, and Odds Ratios from a glm type
# object.
#method 1

custom.glm <- function(x) {
  list(round(cbind(Estimate=x[ , 1],
     std = x[ , 2],
      OR = exp(x[ , 1])),2))
     }
custom.glm(x = glm.summary$coefficients)
#method 2
#custom.glm <- function(x) {
 # list(
 # Estimate=x[ , 1],
 # std = x[ , 2],
 # OR = exp(x[ , 1])
 # )
 # }
 custom.glm(x = glm.summary$coefficients)

# Exercise 2: use your function to display a custom OR table for the following
# glm object:
 
new.juul2.glm <- glm(menarche ~ age + tanner, 
                     data = juul2[juul2$age > 8 & juul2$age < 20, ], 
                     family = binomial("logit"))

juul2.glm<-summary(new.juul2.glm)
juul2.glm
# NO idea??????????????????????????????
custom.glm(x = juul2.glm$coefficients)
exp(OR = coef(cancer.glm))


#########
# Loops #
#########

# 3. Use a for loop print(summary(...)) of each variable of the juul2 data.

for(variable in colnames(juul2)){
  print(summary(juul2[ , variable]))}


# 4. if else
# Here is a simulation of a mean of a sample of 100 random normal variates from
# a standard normal distribution:
simulation.mean <- mean(rnorm(100))
# Use an if statement to evaluate if this number is positive or negative. If it
# is positive print("Positive"), else print("Negative")

 if (simulation.mean >0) ("Positive") else ("Negative")

# 5. Use apply to calculate the sum across rows of my.matrix
my.matrix <- matrix(1:100, nrow = 10)
apply(X = my.matrix, FUN = sum, MARGIN = 1)

# 6. Use apply to calculate the sum of each column of my.matrix

apply(X = my.matrix, FUN = sum, MARGIN = 2)


