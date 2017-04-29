# Author: Wenhui Zeng
# Title: Week 3 Exercises


# 1. Import the data juul.xlsx BY ANY MEANS NECESSARY. 
juul <- read.csv(file = "juul.csv", header = TRUE)

# 2. Reclassify the menarche variable into a factor where 1=no, and 2=yes.
juul$menarche <- factor(juul$menarche, labels = c("no", "yes"))

# 3. Create a data subset containing only the tanner and sex variables
juul.data<-subset(juul, select=c(tanner, sex))

# 4: Create a subset containing only boys assuming boys are coded in 1
juul$sex <- factor(juul$sex, labels = c("Male", "Female"))
juul.boy <- juul[juul$sex == "Male", ]


# 5: Create a subset containing all records with igf1 under 637.2.
juul.igf1 <- juul[juul$igf1 <637.2, ]

# 6: How many kids under 14 years of age are in tanner stage 5?
juul.tanner5.age14 <- juul[juul$tanner == "5" & juul$age < 14, ]
dim(juul.tanner5.age14)[1]
#there are 100 kids under 14 and in tanner stage 5

# 7: How many women experieced each side effect?
#load samples
load("drug.study.RData")

#merge the two data sets so we can find the people with side effects
affected <- merge(x = patient.dmg, y = patient.visits, by = "ID", 
                  all.y = TRUE)
#subset the data with female patient 
affected.female <- affected[affected$sex == "2", ]
#calculate how many observations/row that female patient with side effects
length(affected.female$ID)


# 8: What proportion of women experienced side effects?
#how many women in the data set
#subset the patient.dmg data set with female 
patient.female<-patient.dmg[patient.dmg$sex=="2",]
#calculate how many women in the total data set
length(patient.female$ID)
#calculate the proportion
length(affected.female$ID)/length(patient.female$ID)




# 9: How many girls at each tanner stage have experienced menarche?

table(juul$sex, juul$tanner)


# 10: Create a function that calculates the standard error of the mean for a 
# vector.
sem<- function(x) 
              sd(x,na.rm=TRUE)/sqrt(length(na.omit(x)))


# 11: Use this function to calculate the SEM of age in the juul data.
# I don't why I can't use sem(y=juul$age, x=1)???????
sem(c(y = juul$age, x =1))






