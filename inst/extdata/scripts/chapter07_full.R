# Correlation
# Reproducible Research Using R
# (Full Script)

## ----Loading-Data--------------------------------
library(readxl)
library(tidyverse)

examData <- read_xlsx("Exam_Data.xlsx")

library(skimr)

skim(examData)
colnames(examData)


## ----Cleaning-Names----------------------------------------------------------
library(janitor)
colnames(examData) # Before we clean the data

examData <- clean_names(examData)

colnames(examData) # After we clean the data


## ----Scatterplot-correlation----
p1 <- ggplot(examData, aes(x = studying_hours, y = exam_score)) +
  geom_point(aes(color = first_generation_college_student), size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(
    title = "Studying Time vs Exam Performance",
    subtitle = "Do students who study more score higher?",
    x = "Studying Time (hours)", y = "Exam Score (%)"
  )

p1


## ----Anxiety-Performance----
p2 <- ggplot(examData, aes(x = anxiety_score, y = exam_score)) +
  geom_point(aes(color = first_generation_college_student), size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(
    title = "Exam Anxiety vs Exam Performance",
    subtitle = "Does anxiety relate to exam performance?",
    x = "Exam Anxiety (0–100)", y = "Exam Score (%)"
  )

p2


## ----Studying-anxiety----
p3 <- ggplot(examData, aes(x = studying_hours, y = anxiety_score)) +
  geom_point(aes(color = first_generation_college_student), size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(
    title = "Studying vs Exam Anxiety",
    subtitle = "Do students who study more have more anxiety?",
    x = "Studying Time (hours)", y = "Exam Anxiety (0–100)"
  )

p3


## ----Pairs----
pairs(examData[, c("studying_hours", "exam_score", "anxiety_score")])


## ----GGally-Correlation----
library(GGally)

ggpairs(examData[, c("studying_hours", "exam_score", "anxiety_score")])


## ----Strength-Guidelines-------------
library(knitr)
library(kableExtra)

cor_table <- data.frame(
  "Absolute value of r" = c("r < 0.25", "0.25 < r < 0.5", "0.5 < r < 0.75", "r > 0.75"),
  "Strength of relationship" = c("No relationship", "Weak relationship", "Moderate relationship", "Strong relationship"), check.names = FALSE)

kable(cor_table, 
      booktabs = TRUE) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))



## ----Correlation---------------------------------------------------------------
cor(examData$studying_hours, examData$exam_score)     # Study time vs performance

cor(examData$anxiety_score, examData$exam_score)    # Anxiety vs performance

cor(examData$studying_hours, examData$anxiety_score)  # Study time vs anxiety


## ----Correlation-test----------------------------------------------------------
cor.test(examData$studying_hours, examData$exam_score)     # Study time vs performance

cor.test(examData$anxiety_score, examData$exam_score)    # Anxiety vs performance

cor.test(examData$studying_hours, examData$anxiety_score)  # Study time vs anxiety


## ----matrix-correlation--------------------------------------------------------
# Selecting numeric variables only (if dataset contains non-numeric columns)

examData_numeric <- examData %>% select(where(is.numeric))

cor(examData_numeric)

# Hint: use = "pairwise.complete.obs" handles any missing values safely
corr_matrix <- cor(examData_numeric, use = "pairwise.complete.obs")

corr_matrix


## ----R-squared-----------------------------------------------------------------
# R^2 tells us the percentage of variance shared between two variables.

# Calculating the R^2 value
cor(examData$anxiety_score, examData$exam_score)^2

# Making it look pretty
round(cor(examData$anxiety_score, examData$exam_score)^2*100,2)

# What about the others?
cor(examData$studying_hours, examData$exam_score)^2*100

cor(examData$studying_hours, examData$anxiety_score)^2*100


## ----ppcor-test----------------------------------
library(ppcor)

# Partial correlation between Anxiety and Exam controlling for Studying
pcor.test(examData$anxiety_score, examData$exam_score, examData$studying_hours)

# Uno reverse: controlling for Anxiety instead of Studying
pcor.test(examData$studying_hours, examData$exam_score, examData$anxiety_score)


## ----Biserial------------------------------------------------------------------
# What do you do when you have a biserial (you're either dead or alive)
# Or when you have a point-biserial (failed by 1 pts vs failed by 42 pts vs passed by 4pts)
examData$college_binary <- ifelse(examData$first_generation_college_student=="No",0,1)

cor.test(examData$exam_score, examData$college_binary)


## ----Grouping-College----------------------------------------------------------
# This allows us to see if relationships differ by if they're first generation
examData %>%
  group_by(first_generation_college_student) %>%
  summarise(
    r_rev_exam = cor(studying_hours, exam_score),
    r_anx_exam = cor(anxiety_score, exam_score))

