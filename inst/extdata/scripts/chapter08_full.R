# Linear Regression
# Reproducible Research Using R
# (Full Script)

## ----Loading-Data----------------------------------------------
library(tidyverse)
library(readxl)

pizza_sales<- read_xlsx("Pizza_Prices.xlsx")

library(skimr)

skim(pizza_sales)


## ----Cleaning-regression-------------------------
library(janitor)

# Let's get rid of any spaces in the column names
pizza_sales<- clean_names(pizza_sales)

# We just want to see the relationship between volume and price, so week does not matter
pizza_sales<- pizza_sales %>% select(-1)

# Let's rename our column names to make things easier
pizza_sales <- pizza_sales %>%
  rename(price = total_price, volume = total_volume)


## ----fig-scatter-Price-Volume----
# Now, let us graph our data using ggplot
# As always with ggplot, we have our data, our aesthetics, and our geometry.

pizza_plot <- ggplot(pizza_sales, aes(x = price, y = volume)) +
  geom_point(size = 3)  +
  geom_smooth(method = "lm", se = FALSE) + # this adds a "line of best fit"
  theme_minimal() +
  labs(
    title = "Analysis of Pizza Sales",
    subtitle = "You know the rule, one bite...",
    x = "Price ($)", y = "Volume"
  )

pizza_plot


## ----fig-scatter-ggformula----
library(ggformula)
pizza_plot_ggformula <- gf_point(volume ~ price, data = pizza_sales) %>% gf_lm(color ="purple")

pizza_plot_ggformula


## ----fig-patchwork-regression----
library(patchwork)

pizza_plot + pizza_plot_ggformula


## ----Correlation---------------------------------------------------------------
cor.test(pizza_sales$volume,pizza_sales$price)


## ----LM------------------------------------------------------------------------
pizza_lm <- lm(volume ~ price, data=pizza_sales)
# The code above is saving our linear regression model as pizza_lm

summary(pizza_lm)
# When we call summary on our model, we can find out our key insights


## ----LM-with-broom-------------------------------------------------------------
# If we want to get the same information from our lm model using a different package
library(broom)

tidy(pizza_lm)

glance(pizza_lm)


## ----Predicting-and-residuals--------------------------------------------------
pizza_sales <- pizza_sales %>%
  mutate(predicted = predict(pizza_lm),
         residuals = residuals(pizza_lm))


## ----Scatterplot-with-residual-lines----
# What if we want to see actual vs predicted on our plot?
pizza_plot_residuals <- ggplot(pizza_sales, aes(x = price, y = volume)) +
  geom_point(size = 3)  +
  geom_smooth(method = "lm", se = FALSE) +
  geom_segment(aes(xend = price, yend = predicted), color = "gray", linewidth = 0.7) +
  theme_minimal() +
  labs(
    title = "Analysis of Pizza Sales",
    subtitle = "Gray lines show residuals (differences between actual and model predictions)",
    x = "Price ($)", y = "Volume"
  )

pizza_plot_residuals


## ----Graphing-residuals----
# Let us graph our residuals to make sure they are random
ggplot(pizza_sales, aes(x = predicted, y = residuals)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(se = FALSE, color = "red", linetype = "dotted")+
  labs(
    title = "Residuals Plot",
    x = "Predicted Volume",
    y = "Residuals"
  )


## ----Bptest-using-lmtest-------------------------
library(lmtest)

bptest(pizza_lm)


## ----Adding-Variables--------------------------------------------------------
# Now, let's add some other variables to our data
set.seed(42)

# 1) Dave Portnoy visit.
pizza_sales <- pizza_sales %>%
  mutate(portnoy_visit = rbinom(n(), size = 1, prob = 0.06))  # about 3–6% of weeks

# 2) Ad spend
pizza_sales <- pizza_sales %>%
  mutate(ad_spend = round(runif(n(), 800, 6000), 0))


## ----Multivariate-Regression-------------------------------------------------
m1 <- lm(volume ~ price, data = pizza_sales)                      # baseline

m2 <- lm(volume ~ price + portnoy_visit, data = pizza_sales)      # add portnoy visit

m3 <- lm(volume ~ price + ad_spend, data = pizza_sales)           # add ad spending

m4 <- lm(volume ~ price + ad_spend + portnoy_visit, data = pizza_sales)  # both


## ----AIC---------------------------------------------------------------------
# How do we know which model is the best?
library(AICcmodavg)

AIC(m1, m2, m3, m4) %>% arrange(AIC)
# This code provides us with an AIC (Akaike Information Criterion) value.
# The smaller the AIC, the better the model’s trade-off between complexity and fit.

# Let's say we want to use the broom function again to do a model comparison
models <- list(m1, m2, m3, m4)
# In the code above, we are taking all of our models and putting them into a "list" of models

names(models) <- c("m1","m2","m3","m4")
# In the code above, we are just giving each item in the list a name

# In the code below, we are saying "Hey, run glance on every single item on the models list and create a data frame

map_df(models, ~glance(.x), .id = "model") %>% select(model, r.squared,adj.r.squared,p.value,AIC)


## ----Bonus-------------------------------------------------------------------
full_model <- lm(volume ~ price + ad_spend + portnoy_visit, data = pizza_sales)

step_model <- step(full_model, direction = "both")

