# Introduction To R
# Reproducible Research Using R
# (Helper Script)

## ----basic-math----------------------------------------------------------------
2 + 2

10 / 3

5^2

(3 + 7) * 2

5 * 5 * 5 / 37 + 42


## ----built-in-math-------------------------------------------------------------
# Built-in functions
sqrt(25) # square root

log(10) # natural log

log10(1000) # base-10 log

round(3.14159, digits = 3) # round the output to only have 3 digits

___(5*5/37, 4) # rounding the output of an equation


## ----object-creation-----------------------------------------------------------
# Assigning to objects (variables)
___(25) # the answer is 5

x <- 5 # Making x equal to 5

# Or, we can save the code we wrote and not the answer
x <- sqrt(25)

# Updating x
x <- x + 1 
x # It is now equal to 6 and not 5

# Objects are case sensitive
X <- ___

# You can override code and change a variables value
___ <- 5

# Good names: use letters, numbers, underscores; start with a letter
best_number <- 42

# Inspecting objects
class(x) # x is numeric, as we already knew!


## wont_work <- a, b, c # no parenthesis
## 
## also_wont_work <- (a, b, c) # no c before the parenthesis

## ----numeric-vectors-----------------------------------------------------------
# Numeric vector
___ <- c(1, 2, 3, 4, 5) # c stands for combine/concatenate 

number_vector

# If we tried to run this numeric_vector_2 <- (1,2,3,4,5) it would not work
___ <- c(6:10) # The colon is like saying "everything from 6 to 10"

numeric_vector_2

# The length command tells you how many values are inside the vector
length(number_vector) # 5

length(___) # 5

# It is possible to add numeric vectors as long as they're the same length

___ + numeric_vector_2

# Vectorized math
number_vector * 10


## ----character-vectors---------------------------------------------------------
# Quotation marks around each of the fruits
fruits <- c("apple", "banana", "cherry")

fruits

# Quotation marks around the entire string
Fruits <- c("apple, banana, cherry")

Fruits


## ----logical-vectors-----------------------------------------------------------
# Using full word capitalized
some_truths <- c(TRUE, FALSE, TRUE)

some_truths

class(___)

# Using first letter capitalized
some_lies <- c(F, F, T)

some_lies

class(some_lies)

# Once you put quotation marks, it makes them a character
truth_logic <- c("TRUE","FALSE","TRUE")

class(truth_logic) 


## ----factors-------------------------------------------------------------------
# Creating the colors vector as factors
colors <- factor(c("red", "blue", "red", "green")) # 4 values

colors

class(___)

levels(colors) # Checking to see how many levels there are (3)


## ----indexing------------------------------------------------------------------
# fruits <- c("apple", "banana", "cherry")
fruits[1] # first element

fruits[2:3] # slice


## ----mixing-vectors------------------------------------------------------------
# Putting two numeric and one character into a vector.
mix <- c(1, 2, "three")

mix # becomes character

class(mix)


## ----dataframe-----------------------------------------------------------------
# Build a tiny class roster data frame
names_vec <- c("John", "Bob", "Carmen", "Sarah")

ages_vec <- c(20, 22, 21, 23)

major_vec <- c("Psych", "Econ", "Psych", "CS")

roster <- data.frame(
  name = names_vec,
  age = ages_vec,
  major = major_vec,
  stringsAsFactors = FALSE)

roster

class(roster) # Class is now data.frame


## ----built-in-data-------------------------------------------------------------
# iris
head(iris) # By default returns 6 rows

tail(iris) # By default returns 6 rows

str(___) # Provides the structure of each column

summary(iris) # Provides statistics for each column

colnames(___) # Provides the column names of your data


## ----calling-columns-----------------------------------------------------------
median(roster$age)


## ----new-column----------------------------------------------------------------
# Having one value for each row of data
roster$year <- 2025

# Having a different value for each row of data
roster$minor <- c("Chemistry", "Biology", "History", "Art")

roster


## ----loading-data-practice-----------------------------------------------------
# read.csv example (commented because path varies by machine)
# my_df <- read.csv("/path/to/your/file.csv")

# Working directory helpers
# getwd()
# setwd("/path/where/you/want") # <- avoid hard-coding in projects; we’ll cover R Projects later
#setwd("~/Documents/Work/Brooklyn College/Fall 2025")


## ----packages-intro------------------------------------------------------------
# install.packages("tidyverse")
# library(readxl)
# library(tidyverse)


## ----help-intro----------------------------------------------------------------
?str

