# Logistic Regression
# Reproducible Research Using R
# (Helper Script)

## ----loading-data-ISLR2--------------------------
library(___)
library(tidyverse)

cc_data <- Default # this data comes from the ISLR2 package

library(skimr)

___(cc_data)


## ----proportion-odds-----------------------------
table(cc_data$default)

# Proportion of people who default
p <- mean(cc_data$default == "Yes")
p*100

odds <- p / (1 - p)

# Below provides us with the default rate
cat("Default rate:", round(p*100,2), "%\n")

# Below provides us with the odds of someone defaulting
cat("Odds of defaulting:", ___(odds,4), "\n")

# Below provides us with the odds ratio
cat("Equivalent odds ratio (1 in every", round(1/odds,0), ")\n")

# Cross-tab between student status and default
cc_data %>% count(default, student) %>% ___(desc(n))

# Quick descriptive stats
library(mosaic)

favstats(income ~ default, data = cc_data)

favstats(balance ~ default, data = ___)


## ----fig-default-bargraph----
library(ggthemes)
library(ggpubr)

# Default distribution
ggplot(cc_data, aes(x = default, fill = default)) +
  geom_bar() +
  labs(title = "Count of Defaulters vs Non-Defaulters", y = "Count", x = "Default?") +
  theme_economist()


## ----fig-default-histogram----
library(ggthemes)
library(ggpubr)

# Balance by default
ggplot(___, aes(x = balance, fill = default)) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 30) +
  labs(title = "Higher Balances Increase Likelihood of Default",
       x = "Average Credit Card Balance ($)", y = "Count") +
  theme_economist_white()


## ----fig-default-scatter----
# Balance vs. Income
ggplot(cc_data, aes(x = ___, y = income, color = ___)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  labs(title = "Customers Who Default Tend to Have Higher Balances",
       subtitle = "Each point represents a customer",
       x = "Average Credit Card Balance ($)",
       y = "Annual Income ($)",
       color = "Default") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))


## ----splitting-data------------------------------
# set.seed allows us to get the same split of the data each time we run this code
set.seed(42)

library(caTools) # Package that helps us split our data into Train and Test sets.

# We are going to be doing a 70/30 split. It is good practice to have most of your data in the training
sample <- sample.split(cc_data$default, SplitRatio = 0.7)

# This creates our test data, what we will use to train our logistic regression model
training_data <- subset(cc_data, sample == TRUE)

# This creates our test data, what we will measure our model against

test_data <- subset(cc_data, sample == FALSE)


## ----creating-model------------------------------
# We are using binomial as family since default is yes/no

train_model <- glm(default ~ student + balance + income,
                   family = "binomial", data = training_data) 

summary(___)


## ----convert-log-odds----------------------------
# Convert log-odds → odds ratios
exp(coef(___))

# Example: students have 0.43× the odds (≈57% lower odds) of defaulting


## ----McFadden-pseudo-R2--------------------------
# McFadden’s pseudo-R2
library(pscl)

pR2(train_model)["McFadden"]


## ----variable-importance-------------------------
# Variable importance
library(caret)

varImp(train_model) %>% arrange(desc(Overall))
# balance seems to be the most impactful in our model


## ----multicollinearity---------------------------
# Multicollinearity check
library(car)

vif(train_model)
# does not seem to be any multicollinearity
# anything less than 5 means no multicollinearity


## ----predicting----------------------------------------------------------------
___$pred_prob <- predict(train_model, newdata = test_data, type = "response")

test_data$pred_class <- ifelse(test_data$pred_prob > 0.5, "Yes", "No") %>% as.factor()


## ----confusion-matrix----------------------------
conf_matrix <- confusionMatrix(test_data$pred_class, test_data$default, positive = "Yes")

conf_matrix


## ----pROC-plot----
library(pROC)

roc_obj <- roc(test_data$default, test_data$pred_prob, levels = c("No", "Yes"))
plot(roc_obj, col = "darkblue", lwd = 2,
     main = "ROC Curve – Logistic Regression")

auc(roc_obj)

