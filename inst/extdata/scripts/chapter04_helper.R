# Comparing Two Means
# Reproducible Research Using R
# (Helper Script)

## ----creating the data---------------------------------------------------------
mets <- data.frame(
  Game= c(1,2,3,4,5,6,7,8,9,10),
  Mets_Score= c(4,20,12,5,3,9,9,10,4,2)) # This is where we manually put our numbers in

head(mets) # This calls the first 6 rows of the data

yankees <- data.frame(
  Game= c(1:10), # The colon tells R "any number between 1 and 10."
  Yankees_Score= c(3,8,2,3,2,6,5,3,2,7))

tail(___) # This calls the last 6 rows of the data


## ----creating the next ten-----------------------------------------------------
mets_second_ten <- data.frame(
  Game= c(11:20),
  Mets_Score= c(10,0,7,1,8,5,3,3,4,1))

yankees_second_ten <- data.frame(
  Game= c(11:20),
  Yankees_Score= c(0,4,1,8,4,4,4,4,6,1))


## ----baseball-rbind------------------------------------------------------------
# Mets
nrow(___) # Seeing how many rows are in mets

nrow(mets_second_ten) # Seeing how many rows are in mets_second_ten

mets_all<- rbind(mets, ___) # Combining the rows of mets and mets_second_ten

nrow(mets_all) # Seeing how many rows are in mets_all

# Yankees
nrow(___)

nrow(yankees_second_ten)

yankees_all<- rbind(___, yankees_second_ten)


## ----baseball-merge------------------------------------------------------------
baseball_data <- merge(___, mets_all, by = "Game")

nrow(___) # Checks the number of rows

ncol(___) # Checks the number of columns

baseball_data


## ----baseball-sql--------------------------------------------------------------
library(tidyverse)

baseball_data_sql <- inner_join(yankees_all, ___, by = "Game")

nrow(___) # Checks the number of rows

ncol(___) # Checks the number of columns

baseball_data_sql


## ----baseball-pivot-longer-----------------------------------------------------
# Lets say we want to turn out data from wide format into long format, so we can run 
baseball_data_long <- baseball_data %>% pivot_longer(cols = c(___, Mets_Score),
                                                     names_to = "Team",
                                                     values_to = "Score")

nrow(baseball_data_long)

baseball_data_long


## ----baseball-pivot-wide-------------------------------------------------------
# Lets say we want to turn out data from wide format into long format, so we can run 
baseball_data_wide <- baseball_data_long %>% pivot_wider(names_from = "Team",
                                                         values_from = "Score")
nrow(___)

baseball_data_wide


## ----baseball-mean-------------------------------------------------------------
# Calculating the individual means of the teams scores, we can use our wide format
yankees_average_score <- mean(___$Yankees_Score) 

# Note, we are saving this as a variable so it can be called anytime, but you could just run mean(baseball_data$Yankees_Score)
yankees_average_score

mets_average_score <- mean(baseball_data$___)
# If we want to round our numbers, we can use the code below
# mets_average_score<- round(mean(baseball_data$Mets_Score),0)
mets_average_score

# Calculating the means of all the scores, we can use our long format
overall_average <- mean(baseball_data_long$Score)

overall_average


## ----baseball-plot----
library(ggplot2)
ggplot(___, aes(Team, Score)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.4, size = 1.5) +
  coord_flip()+
  labs(
    title = "Runs Scored by Team Across the First 20 Games",
    x = "Team",
    y = "Runs Scored"
  ) +
  theme_classic()


## ----baseball-ttest------------------------------------------------------------
# Unpaired t.test
baseball_t_test <- t.test(___$Yankees_Score, ___$Mets_Score, paired = F)
# From a structural standpoint, if we needed to run a paired t.test, all we would need to do is change "paired = F" to "paired = T"

baseball_t_test

