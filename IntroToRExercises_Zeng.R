# Introduction to R Exercises
# Author: Wenhui Zeng

# 1 no need to create x and y first

# 3 never create an object called "c" because you end up masking the c() function

# 8 you accidentally transposed 5 and 10.

# 15 brain is already a dataframe. no need to copy it to d

# 1. Display a concatenation of the numbers 2, 5, 7, 4, 6, 9.
x <- c(2, 5, 7)
y <- c(4, 6, 9)
c(x, y)


#key answer
c(2, 5, 7, 4, 6, 9)
# 2. Create an object called MyNumbers from that concatenation.

MyNumbers<-c(2, 5, 7, 4, 6, 9)

# 3. Multiply that vector by 3.
Mynumber*3

# 4. Add 3 to each value within MyNumbers, and store it as an object called MyNumbersPlus3.
MyNumbersPlus3<-3+MyNumbers

# 5. Concatenate the following character vector: Male, Male, Female, Male, Female, Female into an object called "sex".
sex<-c("Male", "Male", "Female", "Male", "Female", "Female")


# 6. Do you need to explicitly state levels when converting a character vector to a factor? Run this and examine the resulting object within the Global Environment window of R Studio to find out:
sex.f <- as.factor(sex)
# we don't need to explicity state the levels, because the  it already said the factor has 2 levels: male and female
# No. as.factor(...) tries to determine appropriate levels when converting a non-factor vector into a factor.


# 7. Find the length of sex.f.
length(sex.f)
#the length of the sex.f. is 6

# 8. Display a vector that first repeats the word "Blue" 10 times, then "Green" 5 times using the rep(...) function.<-c("Blue","green")
color<-c("Blue","green")
rep(color,c(5,10))
#answer key
rep(c("blue", "green"), c(10, 5))

# 9. Display a vector that alternates "Blue" then "Green" 10 times.
rep(color,10)
#answer key
rep(c("blue", "green"), 10)

# 10. Display a 6-column matrix of sequence of numbers from 1 through 42.
x <- matrix(1:42, nrow=7)
#or
matrix(1:42, ncol = 6)

# 11. Store a 5-row matrix of a sequence of UPPERCASE letters A through Y, 
#filling the letters by column rather than row(use the by row option), 
#as an object called MyLetterMatrix

MyLetterMatrix<-matrix(LETTERS[1:25],nrow=5,byrow=F)
MyLetterMatrix

# 12. What is the 3rd element in your sex.f vector?
sex.f[3]

# 13. Which elements in MyNumbersPlus3 are less than 8?
MyNumbersPlus3[MyNumbersPlus3 <8]
#answer key
MyNumbersPlus3 < 8 # to display simple T/F for each element of MyNumbersPlus3.
MyNumbersPlus3[MyNumbersPlus3 < 8] # displays the actual values of MyNumbersPlus3 that are < 8


# 14. What letter is in row 3, column 2 of MyLetterMatrix?
MyLetterMatrix[3,2]

# 15. Import the "brain.csv" data again, then display a histogram of proficiency.
#using the session on the tool tab to choose the working directory
brain <- read.csv("brain.csv")
d<-data.frame(brain)
d
d$proficiency
hist(d$proficiency)
#answer key 
brain.csv <- read.csv("brain.csv")
hist(brain.csv$proficiency)


