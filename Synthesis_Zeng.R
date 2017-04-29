# Author: Wenhui Zeng
# Title: Synthesis Project I

# Raw data source: http://www.cdc.gov/brfss/about/index.htm
# Data provided by instructor was cleaned by a capstone student.

# Data description from CDC website:
# "The Behavioral Risk Factor Surveillance System (BRFSS) is the nation's
# premier system of health-related telephone surveys that collect state data
# bout U.S. residents regarding their health-related risk behaviors, chronic
# health conditions, and use of preventive services. Established in 1984 with 15
# states, BRFSS now collects data in all 50 states as well as the District of
# Columbia and three U.S. territories. BRFSS completes more than 400,000 adult
# interviews each year, making it the largest continuously conducted health
# survey system in the world."

###############################
# Synthesis project description

# Go to the BRFSS website and read through the brief description of that data.
# http://www.cdc.gov/brfss/about/index.htm

# Click the link for "Survey Data and Documentation" then click the link for
# the "Annual Survey Data" then click the link for the "2014 Data", or paste
# the following link into your browser:
# http://www.cdc.gov/brfss/annual_data/annual_2014.html

# Download and skim through the "2014 BRFSS Overview" PDF.
# Download the "2014 BRFSS Codebook" PDF and the "2014 BRFSS DATA (SAS Transport
# Format)" XPT files.

# Now skim through the codebook, and pick a few variables of interest to you.
# At minimum, you should pick:
# 2 numeric variables (eg. number of poor health days, age, etc.)
# 1 categorical variable (eg. marital status)
# 1 binary health outcome variable (eg. coronary heart disease)





#########################
# Import and Clean Data #
#########################

# Unzip the data, and then 
# Import the LLCP2014.XPT file into R using the following script:
library(foreign)
# if that package isn't found, then uncomment and run:
# install.packages("foreign") # then reload the foreign package
LLCP2014 <- read.xport(file = "LLCP2014.XPT")




#subset the data with desired variables
# Create a data.frame object that contains only your variables of interest.
synthesis<-data.frame(LLCP2014$MENTHLTH,LLCP2014$AVEDRNK2,
                      LLCP2014$SXORIENT,LLCP2014$RRCLASS2,LLCP2014$CHCKIDNY,LLCP2014$HLTHPLN1)


# Next, clean the numeric variables by removing any non-numeric values:
# For example, if you were to use the MENTHLTH variable, you would replace all 
# "77" ("don't know") and "99" ("refused") records with "NA", and "88" with "0".


#MENTHLTH (how many days during the past 30 days was your mental health not good)
synthesis$LLCP2014.MENTHLTH[synthesis$LLCP2014.MENTHLTH==77]<-NA
synthesis$LLCP2014.MENTHLTH[synthesis$LLCP2014.MENTHLTH==99]<-NA
synthesis$LLCP2014.MENTHLTH[synthesis$LLCP2014.MENTHLTH==88]<-0

# numeric variable :AVEDRNK2 (Avg alcoholic drinks per day in past 30 )
synthesis$LLCP2014.AVEDRNK2[synthesis$LLCP2014.AVEDRNK2==77]<-NA
synthesis$LLCP2014.AVEDRNK2[synthesis$LLCP2014.AVEDRNK2==99]<-NA


# Next, clean your categorical variables by turning them into factors with
# appropriate lables for each level. You may also strip any unwanted levels.
# For example, if you were to use the "MEDCOST" variable, I would recode so that
# 1="yes", 2="no" and make everything else "NA."

#Categorical variable :SXORIENT (Sexual orientation or gender identity) 

synthesis$LLCP2014.SXORIENT[synthesis$LLCP2014.SXORIENT==1 ]<-"Straight"
synthesis$LLCP2014.SXORIENT[synthesis$LLCP2014.SXORIENT==2 ]<-"Lesbian or gay"
synthesis$LLCP2014.SXORIENT[synthesis$LLCP2014.SXORIENT==3 ]<-"Bisexuel"
synthesis$LLCP2014.SXORIENT[synthesis$LLCP2014.SXORIENT==4 ]<-"Other"
synthesis$LLCP2014.SXORIENT[synthesis$LLCP2014.SXORIENT==7 ]<-"NA"
synthesis$LLCP2014.SXORIENT[synthesis$LLCP2014.SXORIENT==9]<-"NA"

#RRCLASS2(How do other people usually classify you in this country?)
synthesis$LLCP2014.RRCLASS2[synthesis$LLCP2014.RRCLASS2==1 ]<-"White"
synthesis$LLCP2014.RRCLASS2[synthesis$LLCP2014.RRCLASS2==2 ]<-"Black or African American"
synthesis$LLCP2014.RRCLASS2[synthesis$LLCP2014.RRCLASS2==3 ]<-"Hispanic or Latino"
synthesis$LLCP2014.RRCLASS2[synthesis$LLCP2014.RRCLASS2==4 ]<-"Asian"
synthesis$LLCP2014.RRCLASS2[synthesis$LLCP2014.RRCLASS2==5 ]<-"Native Hawaiian or Other Pacific Islander"
synthesis$LLCP2014.RRCLASS2[synthesis$LLCP2014.RRCLASS2==6 ]<-"American Indian or Alaska Native"
synthesis$LLCP2014.RRCLASS2[synthesis$LLCP2014.RRCLASS2==7 ]<-"NA"
synthesis$LLCP2014.RRCLASS2[synthesis$LLCP2014.RRCLASS2==8 ]<-"NA"
synthesis$LLCP2014.RRCLASS2[synthesis$LLCP2014.RRCLASS2==9 ]<-"NA"


#HLTHPLN1 (Do you have any kind of health care coverage) 
synthesis$LLCP2014.HLTHPLN1 [synthesis$LLCP2014.HLTHPLN1 ==1 ]<-"Yes"
synthesis$LLCP2014.HLTHPLN1 [synthesis$LLCP2014.HLTHPLN1 ==2 ]<-"No"
synthesis$LLCP2014.HLTHPLN1 [synthesis$LLCP2014.HLTHPLN1 ==7 ]<-"NA"
synthesis$LLCP2014.HLTHPLN1 [synthesis$LLCP2014.HLTHPLN1 ==9 ]<-"NA"

#outcome variable
#CHCKIDNY((Ever told) you have kidney disease?
#recode the variable so that it is ready for logistic regression analysis

synthesis$LLCP2014.CHCKIDNY [synthesis$LLCP2014.CHCKIDNY ==1 ]<-1
synthesis$LLCP2014.CHCKIDNY [synthesis$LLCP2014.CHCKIDNY ==2 ]<-0
synthesis$LLCP2014.CHCKIDNY [synthesis$LLCP2014.CHCKIDNY ==7 ]<-NA
synthesis$LLCP2014.CHCKIDNY [synthesis$LLCP2014.CHCKIDNY ==9 ]<-NA

#################
# Data Analysis #
#################

# Using one numeric response and one categorical explanitory variable, perform
# an ANOVA.

synthesis.aov.sex <- aov(LLCP2014.CHCKIDNY ~ LLCP2014.SXORIENT, data = synthesis)
synthesis.aov.drk <- aov(LLCP2014.CHCKIDNY ~ LLCP2014.AVEDRNK2, data = synthesis)
summary(synthesis.aov.sex)
summary(synthesis.aov.drk)
# Run post-hoc analysis
TukeyHSD(synthesis.aov.sex)

# Using two numeric variables, perform a linear regression.


  
synthesis.lm <- lm(LLCP2014.MENTHLTH ~LLCP2014.AVEDRNK2, data = synthesis)
summary(synthesis.lm )


# Using one binary categorical health outcome variable and one numeric and/or
# categorical explanitory variable, perform a logistic regression.

synthesis.glm <- glm(LLCP2014.CHCKIDNY ~ LLCP2014.AVEDRNK2+LLCP2014.RRCLASS2, 
                     family = binomial("logit"),data = synthesis)

summary(synthesis.glm )



