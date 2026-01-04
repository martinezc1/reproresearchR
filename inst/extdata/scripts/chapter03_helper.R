# Visualizations
# Reproducible Research Using R
# (Full Script)

## ----AnscombeData--------------------------------
library(tidyverse)
library(quartets)
library(knitr)

anscombe_quartet %>%
  group_by(dataset) %>%
  summarise(mean_x = mean(x),
            variance_x = var(x),
            mean_y = mean(y),
            variance_y = var(y),
            correlation = cor(x, y)) %>%
  kable(digits = 2,caption = "A breakdown of summary statistics from the four individual datasets Anscombe created.")


## ----Anscombe-graph----
ggplot(anscombe_quartet, aes(x = x, y = y)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  facet_wrap(~dataset)


## ----baseR-plot----
values <- c(100, 17, 45, 55, 44)

barplot(values, xlab = "X-axis", ylab = "Y-axis", main ="Base R Bar Chart")


## ----mpg-kable-----------------------------------
kable(head(mpg), caption = "A base R dataset: Fuel economy data from 1999 to 2008 for 38 popular models of cars.")


## ----economics-kable-----------------------------------------------------------
kable(head(economics), caption = "A base R dataset: US Economic Time Series.")


## ----diamonds-kable------------------------------------------------------------
kable(head(diamonds), caption = "A base R dataset: Prices of over 50,000 round cut diamonds.")


## ----mtcars-kable--------------------------------------------------------------
kable(head(mtcars), caption = "A base R dataset: Motor Trend Car Road Tests.")


## ----example-ggplot2----
ggplot(data = mpg, aes(x = cty, y = hwy)) +
  geom_point()


## ----basic-scatterplot----
ggplot(data = mpg, aes(x = cty, y = hwy)) +
  geom_point()


## ----ggplot2-scatterplot-lm----
# Line of Best Fit
ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point() +
  geom_smooth(method = "lm") # lm stands for "linear model"


## ----scatterplot-advanced----
ggplot(data = mpg, aes(x = cty, y = hwy, color = class, shape = drv)) +
  geom_point(alpha = 0.8, size = 2.5) + # opacity and size
  labs(
    title = "City vs Highway MPG",      # adds a title to the plot
    x = "City MPG",                     # adds a x-axis label to the plot
    y = "Highway MPG",                  # adds a y-axis label to the plot
    color = "Vehicle Class",            # adds a color label to the plot
    shape = "Drivetrain"                # adds a shape label to the plot
  ) + 
  facet_wrap(~ class) +                 # breaks the graph into individual graphs
  geom_smooth(method = "lm", se = TRUE) # adds a line of best fit


## ----basic-barchart---------------
# BASIC (counts by class) - geom_bar() counts rows automatically
ggplot(mpg, aes(x = class)) +
  geom_bar()


## ----barplot-coordflip----
ggplot(mpg, aes(x = manufacturer)) +
  geom_bar(fill = "steelblue", color = "white") +
  coord_flip() +
  labs(title = "Counts by Manufacturer", x = "", y = "Count")


## ----stacked-barchart-example----
# What if we want a stacked bar chart (default with fill)?
ggplot(mpg, aes(x = manufacturer, fill = drv)) +
  geom_bar(position = "stack", color = "white") +
  coord_flip() +
  labs(
    title = "Counts by Manufacturer and Drivetrain",
    x = "",
    y = "Count",
    fill = "Drivetrain"
  ) +
  theme_minimal()


## ----grouped-barchart-----------
#What if we want a grouped bar chart
ggplot(mpg, aes(x = manufacturer, fill = drv)) +
  geom_bar(position = "dodge", color = "white") +
  coord_flip() +
  labs(
    title = "Counts by Manufacturer and Drivetrain",
    x = "",
    y = "Count",
    fill = "Drivetrain"
  ) +
  theme_minimal()


## ----Pre-summarizing-values----------------------------------------------------
# USING PRE-SUMMARIZED VALUES - geom_col() requires explicit values
class_counts <- mpg %>%
  count(class)  # counts rows by class

kable(class_counts, caption = "Pre-summarized values")


## ----basic-columnchart---------
ggplot(class_counts, aes(x = class, y = n)) +
  geom_col()


## ----columnchart-aesthetics----
# PLUS AESTHETICS (polished)
ggplot(class_counts, aes(x = reorder(class, n), y = n, fill = class)) +
  geom_col(width = 0.7, color = "white") +   # width = bar thickness, color = border
  coord_flip() +                             # flip for readability
  labs(title = "Counts by Vehicle Class", x = "", y = "Count") +
  theme(legend.position = "none")


## ----basic-histogram--------------
# BASIC
ggplot(mpg, aes(x = hwy)) +
  geom_histogram(binwidth = 3)


## ----styled-histogram-----------
# STYLED (bin edges + colors)
ggplot(mpg, aes(hwy)) +
  geom_histogram(binwidth = 5, boundary = 0,
                 fill = "red", color = "orange") +
  labs(title = "Highway MPG Distribution", x = "Highway MPG", y = "Frequency")


## ----filled-histogram, fig.cap= "An example of a filled histogram."------------
# MAPPED FILL (stacked by class)
ggplot(mpg, aes(hwy, fill = class)) +
  geom_histogram(binwidth = 10, color = "white") +
  labs(title = "Highway MPG Distribution by Vehicle Class",
       x = "Highway MPG", y = "Count", fill = "Vehicle Class") +
  theme(legend.position = "bottom")


## ----basic-densityplot, fig.cap= "An example of a basic density plot."---------
# BASIC
ggplot(diamonds, aes(x = price)) +
  geom_density()


## ----grouped-densityplot, fig.cap= "An example of a grouped density plot."-----
# GROUPED
ggplot(diamonds %>% filter(cut %in% c("Good", "Ideal", "Premium")),
       aes(price, color = cut)) +
  geom_density() +
  labs(title = "Price Density by Cut", x = "Price", y = "Density") +
  theme(legend.position = "bottom")


## ----basic-boxplot, fig.cap= "An example of a basic boxplot."------------------
# BASIC
ggplot(mpg, aes(x = class, y = hwy)) +
  geom_boxplot() +
  labs(title = "Highway MPG by Vehicle Class", x = "Vehicle Class", y = "Highway MPG")


## ----jittered-boxplot, fig.cap= "An example of a boxplot with jittered points."----
# WITH JITTERED POINTS OVERLAID
ggplot(mpg, aes(class, hwy)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.4, size = 1.5) +
  coord_flip() +
  labs(title = "Highway MPG by Vehicle Class (with Points)",
       x = "", y = "Highway MPG")


## ----basic-linegraph,  fig.cap= "An example of a basic line graph."------------
# BASIC: unemployment over time
ggplot(economics, aes(x = date, y = unemploy)) +
  geom_line() +
  labs(title = "US Unemployment Over Time",
       x = "Date", y = "Number Unemployed (thousands)")


## ----lm-linegraph, fig.cap= "An example of line graph with a LM line of best fit."----
# PLUS: LM vs LOESS contrast
ggplot(economics, aes(date, unemploy)) +
  geom_line(linewidth = 1.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "US Unemployment with Linear Trend",
       x = "Date", y = "Number Unemployed (thousands)")


## ----loess-linegraph, fig.cap= "An example of line graph with a LM line of best fit."----
ggplot(economics, aes(date, unemploy)) +
  geom_line(linewidth = 0.6) +
  geom_smooth(method = "loess", se = TRUE) +  # loess = flexible smoothing; se = confidence band
  labs(title = "US Unemployment with LOESS Smooth",
       x = "Date", y = "Number Unemployed (thousands)")


## ----basic-textplot, fig.cap= "An example of adding text inside a plot."-------
# BASIC: label extreme points
mpg_extremes <- mpg %>% slice_max(order_by = hwy, n = 5)
mpg_extremes
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_text(data = mpg_extremes, aes(label = model), nudge_y = 1, size = 3) +
  labs(title = "Top Highway MPG Models Labeled",
       x = "Engine Displacement (L)", y = "Highway MPG")


## ----computing-standard-error--------------------------------------------------
# Compute mean & standard error for hwy by class
summ_hwy <- mpg %>%
  group_by(class) %>%
  summarize(
    mean_hwy = mean(hwy, na.rm = TRUE),
    se_hwy   = sd(hwy, na.rm = TRUE) / sqrt(n()))


## ----basic-error-plot, fig.cap= "An example of a plot with error bars."--------
# Points + error bars (plot error bars first so points sit on top)
ggplot(summ_hwy, aes(class, mean_hwy)) +
  geom_errorbar(aes(ymin = mean_hwy - se_hwy, ymax = mean_hwy + se_hwy), width = 0.2) +
  geom_point(size = 2) +
  coord_flip() +
  labs(title = "Mean Highway MPG (± SE) by Class", x = "", y = "Mean Highway MPG")


## ----horizontal-referencelines, fig.cap= "An example of plot with a horizontal reference line."----
# Horizontal line at overall mean
overall_mean <- mean(mpg$hwy, na.rm = TRUE)

ggplot(mpg, aes(displ, hwy)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = overall_mean, linetype = "dashed") +
  labs(title = "Reference Line at Overall Mean Highway MPG",
       x = "Engine Displacement (L)", y = "Highway MPG")


## ----vertical-referencelines, fig.cap= "An example of plot with a vertical reference line."----
# Vertical line at displ = 3
ggplot(mpg, aes(displ, hwy)) +
  geom_point(alpha = 0.6) +
  geom_vline(xintercept = 3, linetype = "dotted") +
  labs(title = "Reference Line at Engine Displacement = 3L",
       x = "Engine Displacement (L)", y = "Highway MPG")

