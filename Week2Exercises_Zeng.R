# Author: Wenhui Zeng
# Title: Week 2 Exercises

#feedback
#5 use tapply() to get statistics by ventilation treatment

# 1. Researchers studying human physiology wanted to know how movement through a
# fluid is influenced by the viscosity of the fluid. They reasoned that the 
# increased drag imparted by a highly viscous fluid might be offset by an 
# increase of power with each movement.  These researchers asked 18 swimmers to 
# swim laps in a pool filled with water, and then again in a pool filled with 
# syrup (true story: Gettelfinger and Cussler, 2004).  They analyzed lap times 
# (see data set "swimmers.csv") and divided each swimmer's syrup speed by their 
# water speed (syrup/water).  If the syrup has no effect on swim speed, then the
# average relative speed of these swimmers should be 1.  Using a 1-sample 
# t-test, test whether fluid viscosity influences movement speed.
swimmer<-read.csv("swimmers.csv")

mean(swimmer$speed, na.rm = T)
# answer key Descriptive statistics
sd(swimmers$speed)
length(swimmers$speed)
# one-sample t-test where theoretical mean=1
t.test(swimmer$speed, mu=1) 
 
#We find that 1.1695, df = 17, p-value >0.05, therefore fail to reject the null hypothesis.
# The fluid viscosity doesn't influence movement speed.there is no difference between the true mean and the sample mean

# 2. No Smoking Day in Britain occurs on 
# the second Wednesday of March each year.  Data was recorded from 1987 to 1996 
# on the number of nonfatal injuries on the preceding Wednesday, and then again 
# on No Smoking Day (Waters et al. 1998).  This makes a nicely paired dataset to
# compare injuries before No Smoking Day to those after ("injuries.csv").  Use a
# paired t-test to test whether smoking influences the rate of injury.

injuries<-read.csv("injuries.csv")
#answer key
# Note: for a paird t-test, we are not interested in overall group statistics; 
# rather we want to know the describe the difference between the two groups
mean(injuries$beforensd - injuries$onnsd) # mean difference between treatments
sd(injuries$beforensd - injuries$onnsd) # sd of the difference
length(injuries$beforensd)
# Run paired t-test:
t.test(injuries$beforensd, injuries$onnsd, paired = T)

# We find that t = -2.447, df = 9, P<0.05, therefore reject the null hypothesis.
# The number of nonfatal injuries is difference after no smoking day.


# 3. Traveling across time zones causes "jet lag", but exposure to light is 
# believed to cause a phase shift in our internal body clocks helping to sync us
# to the new time zone.  To test this, researchers measured circadian rhythm by
# the daily cycle of melatonin production in 15 subjects randomly assigned to 
# either a light or control treatment (Write and Czeisler, 2002). Subjects were 
# awakened from sleep and either subjected to a single three-hour episode of 
# bright lights applied to their eyes only (light treatment) or no light for 
# three hours (control treatment). Effects of treatment on the circadian rhythm 
# were measured two days later by the magnitude of phase shift (measured in 
# hours) in each subject's daily cycle of melatonin production ("jetlag.csv"). 
# A negative measurement shows a delay in melatonin production, indicative of a 
# subject adapting to the new time zone. Does light exposure help to resolve jet
# lag?
jetlag<-read.csv("jetlag.csv")
#answer key

# Get descriptives of phaseshift for each treatment; no need to worry about 
# missing (NA) values:
tapply(jetlag$phaseshift, jetlag$treatment, mean)
tapply(jetlag$phaseshift, jetlag$treatment, sd)
tapply(jetlag$phaseshift, jetlag$treatment, length)
# Run Welch t-test:
t.test(jetlag$phaseshift ~ jetlag$treatment) 
# since both vectors come from the same data.frame, you could use this notation:
t.test(phaseshift ~ treatment, data = jetlag)
# If you assume that variances are equal, then you can get the slightly more
# powerful standard t-test
t.test(phaseshift ~ treatment, data = jetlag, var.equal = T)

#my answer
t.test(phaseshift ~ treatment, data = jetlag, var.equal=T)
#The t = 3.6379, df = 13, p-value <0.05, we reject the null hypothesis
#The light exposure help to resolve jet lag. The mean group in the light is more negative than control group.

# 4. File "red.cell.folate.csv" shows red cell folate levels (ug/l) in patients 
# receiving three different methods of ventilation during anesthesia (N2O+O2 
# for 24 hrs, N2O+O2 during opperation only, and O2 only for 24 hrs). Use ANOVA
# to detect potential differences in folate levels among the three treatments.
red<-read.csv("red.cell.folate.csv")
red.aov <- aov(folate ~ ventilation, data = red)
summary(red.aov) 
TukeyHSD(red.aov)
#The p-value<0.05 The folate levels was different among the three treatments
# The post-hoc test shows that there is difference between N2O+O2 and N2O+O2 treatment. There is no difference
#between N2O+O2 and  O2,  O2 and N2O+O2


# 5. Calculate descriptive statistics (mean, sd, n) of red cell folate for each 
# treatment.
#for each treatment like folate and ventilation
mean(red$folate, na.rm = T)
sd(red$folate, na.rm = T)
length(red$folate[!is.na(red$folate)])
# the mean of red cell is 283.2273, the standard deviation of red cell is 51.284, and the number non-missing value
#of red cell is 22

#Answer Key
# Again, there are no NA values here, so na.rm=T is not needed.
# For fun (and education), notice that I'm creating objects m, s, & n. I can use
# those to calculate a standard error of the mean. Here object "m" is created:
m <- tapply(red$folate, red$ventilation, mean)
m # displayes the results of m
# Alternatively, enclosing an expression within (...) causes the object to be
# created AND displayed simultaneously. Cool, eh?
(s <- tapply(red$folate, red$ventilation, sd))
(n <- tapply(red$folate, red$ventilation, length))
(sem <- s/sqrt(n)) # calculates your standard error of the mean



# 6. How is the growth of Tetrahymena cells relateded to the concentration of
# cells?  The data set "hellung.csv" contains measures of diameter and
# concentration for cultures of Tetrahymena cells, a "model" species sometimes
# used in biomedical research (it also contains a binary variable indicating
# whether glucose was added to the growth medium, but ignore that variable for
# now). Model cellular growth as a linear function of cell contration.
hellung<-read.csv("hellung.csv")
head(hellung) # just look at the first 6 observations
hellung.lm <- lm(diameter~conc, data = hellung)
summary(hellung.lm)
#the Y(diameter)=2.424e+01-(-7.535e-06)X(concentration)
plot(hellung.lm, which = 1:4)


# 7. How well does a patient's prognosis match his or her actual survival 
# probability? Researchers followed 66 cancer patients from their initial 
# diagnosis through their first 5 years of treatment.  The data file 
# "cancer.csv" contains two of the variables that were measured.  The 
# "prognosis" variable indicates the doctors' best guess of a patient's 
# probability of surviving for 5 years.  The "outcome" variable indicates 
# whether the patient actually survived for 5 years.  Use logistic regression 
# to predict actual survival from the initial prognosis.
cancer<-read.csv("cancer.csv", h=T)
cancer.glm <- glm(outcome ~ prognosis, family = binomial("logit"), data=cancer)
summary(cancer.glm)
exp(coef(cancer.glm))
#answer key
# Get odds ratios and 95% CI:
exp(cbind(OR = coef(cancer.glm), confint(cancer.glm)))
# What is the actually probability of survival given a physician's prognosis?
# You could use predict(...) from the original data
predict(cancer.glm, cancer, type="resp")
# or extract the fitted values from the .glm object:
cancer.glm$fitted.values


# 8. The sheep.deaths.csv data is of 150 wild sheep followed for 50 months. 
# Sheep were treated for parasites with 3 different treatments (group A = high, 
# B = low, C = placebo), and their initial wieght was recorded. Then their month
# of death (death) as well as status at the end of the study (0 = censored, 
# 1 = dead) was recorded. Create and compare KM curves for these treatemts, and
# perform a simple Cox regression.

library(survival)
sheep<-read.csv("sheep.deaths.csv")
summary(sheep)
#survive object is the surv(death,status) fit the survival curve by treatment, KM curves
plot(survfit(Surv(death, status) ~ group, data = sheep), col = c(2, 3, 4),xlab = "Time to event (Month)")

legend(x = max(sheep$death), y = 1, legend = c("A", "B","C"), lty = 1,col = c(2, 3, 4), xjust = 1, bty = "n")

# Test for differences among these curves using a log-rank test
survdiff(Surv(death, status == 1) ~ group, data = sheep)
# Construct and interpret a Cox Proportional Hazards model
sheep.cph <- coxph(Surv(death, status == 1) ~ group, data = sheep)
summary(sheep.cph)

#Answer key
# The Kaplan Meier estimate shows the proportion of individuals surviving at
# each point in time.
plot(survfit(
  Surv(death, status) ~ group, data = sheep), 
  col = c(2, 3, 4), xlab = "Age at Death (months)")
legend(x = max(sheep$death), y = 1, 
       legend = levels(sheep$group), lty = 1, 
       col = c(2, 3, 4), xjust = 1)
# Test whether two or more KM curves are identical
survdiff(Surv(death, status==1) ~ group, data = sheep)
model1.cph <- coxph(Surv(death, status==1) ~ group, 
                    data = sheep)
summary(model1.cph)





