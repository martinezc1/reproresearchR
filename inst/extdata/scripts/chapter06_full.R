# Analyzing Categorical Data
# Reproducible Research Using R
# (Full Script)

## ----loading-data-CA----------------------------
library(readxl)
library(tidyverse)

infection <- read_xlsx("Infection_Treatments.xlsx")

summary(infection)

str(infection)

library(skimr)

skim(infection)


## ----contingency-tables--------------------------------------------------------
# All three are at the same frequency
table(infection$Treatment)

# This is not an even split like Treatment. There are more people that were not infected vs infected.
infection %>% count(Infection)

# Now number of infected and infected per treatment.
table(infection$Treatment,infection$Infection)

# We can also create this using the xtabs commmand from the stats package
contingency_table <- xtabs(~Treatment+Infection, data=infection)

contingency_table

# Let us see what it looks like if we reverse it.
reverse_table <- xtabs(~Infection+Treatment, data=infection)

reverse_table


## ----percentages-CA------------------------------
library(janitor)

c_table <- infection %>%
  tabyl(Treatment, Infection) %>%         # Makes a contingency table
  adorn_totals("row") %>%                 # Adds totals for each treatment
  adorn_percentages("row") %>%            # Adds row percentages
  adorn_pct_formatting(digits = 1) %>%    # Makes it readable (adds % signs)
  adorn_ns()                              # Combines counts + percentages

c_table


## ----ggplot-infection-treatment-stacked----
# We can use the library ggthemes to add some flavor to our plots
library(ggthemes)

# Creating a stacked bar chart
stacked <- ggplot(infection, aes(x = Treatment, fill = Infection)) +
  geom_bar(position = "fill") +   # "fill" stacks to 100% height
  labs(
    title = "Proportion of Infections by Treatment Type",
    y = "Proportion of Participants",
    x = "Treatment"
  ) +
  theme_solarized()


## ----ggplot-infection-treatment-grouped----
# Creating a grouped bar chart
grouped <- ggplot(infection, aes(x = Treatment, fill = Infection)) +
  geom_bar(position = "dodge") +
  geom_text(
    stat = "count",                          # use counts from geom_bar
    aes(label = after_stat(count)),          # label each bar with its count
    position = position_dodge(width = 0.9),  # place correctly over side-by-side bars
    vjust = -0.3,                            # move labels slightly above bars
    size = 4
  ) +
  labs(
    title = "Number of Infections by Treatment Type",
    x = "Treatment",
    y = "Count of Participants",
    fill = "Infection Outcome"
  ) +
  theme_classic()


## ----grouped-stacked-patchwork----
# We can put them side by side to compare what they look like
library(patchwork)

grouped + stacked + plot_annotation(title = "Visualizing Infection Outcomes by Treatment (Stacked Vs. Grouped")


## ----plotly-graph----
# We can also use the plotly package to make the visual more interactive
library(plotly)

ggplotly(grouped)


## ----chisq_test----------------------------------------------------------------
chisq.test(contingency_table)


## ----chi-square-breakdown------------------------------------------------------
chi_square_test <- chisq.test(contingency_table)

# All the parts of the chi square can be called.
chi_square_test$statistic

chi_square_test$parameter

chi_square_test$p.value

chi_square_test$method

chi_square_test$data.name

# What our data already is
chi_square_test$observed

# What our data would look like if there was no relationship
chi_square_test$expected

# Difference between observed and expected
# Positive is more than expected
# Negative is less than expected
# Bigger number = bigger difference
# Represent how many standard deviations
chi_square_test$residuals

# Standard Residuals
# More like an actual z-score
chi_square_test$stdres


## ----crosstables---------------------------------------------------------------
library(gmodels)

# This allows us to see the contributions each category has on chi-square.
# Note, you (infection$Treatment, infection$Infection) if you want to stick to defaults
CrossTable(infection$Treatment, infection$Infection,
           prop.chisq = TRUE,    # Shows the chi-square contribution
           chisq = TRUE,         # shows chi-square test
           expected = TRUE,      # shows expected counts
           prop.r = TRUE,        # shows row proportions
           prop.c = TRUE)        # shows column proportions


## ----contributions-------------------------------------------------------------
# Calculate contribution to chi-square statistic
# X^2= ((observed-expected)^2)/expected
contributions <- ((chi_square_test$observed-chi_square_test$expected)^2)/chi_square_test$expected

contributions

percent_contributions <- contributions / chi_square_test$statistic * 100

percent_contributions


## ----pheatmap----
library(pheatmap)

# Create heatmap for percentage contributions
pheatmap(percent_contributions,
         display_numbers = TRUE,
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         main = "% Contribution to Chi-Square Statistic")


## ----cramerV-------------------------------------
library(rcompanion)

cramerV(contingency_table)



