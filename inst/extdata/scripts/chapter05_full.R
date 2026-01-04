## ----creating-our-data---------------------------
library(tidyverse)

set.seed(123)  # ensures reproducibility

memory <- tibble(
  method = rep(c("Flashcards", "Rereading", "Testing"), each = 20),
  score = c(
    rnorm(20, mean = 75, sd = 8),  # Flashcards group
    rnorm(20, mean = 70, sd = 9),  # Rereading group
    rnorm(20, mean = 85, sd = 7)   # Testing group
  )
)

glimpse(memory)


## ----summary table-------------------------------
# Creating a summarizing table
memory_summarized <- memory %>%
  group_by(method) %>%
  summarize(
    mean_score = mean(score),
    sd_score = sd(score),
    n = n()
  )

memory_summarized

# Instead of writing all of the code manually, we can use the code below.
library(mosaic)

favstats(score ~ method, data = memory)


## ----plots-anova----
ggplot(memory, aes(x = method, y = score, fill = method)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Memory Test Scores by Study Technique",
    x = "Study Technique",
    y = "Memory Test Score"
  ) +
  theme_minimal()


## ----anova-means-plot----
memory %>%
  group_by(method) %>%
  summarize(mean_score = mean(score)) %>%
  ggplot(aes(x = method, y = mean_score, fill = method)) +
  geom_col() +
  geom_text(aes(label = round(mean_score, 1)), vjust = -0.5) + # adds the means to bars
  labs(
    title = "Average Memory Score by Study Technique (ANOVA Means)",
    x = "Study Technique",
    y = "Average Score"
  ) +
  theme_minimal()


## ----t test--------------------------------------------------------------------
# Last chapter we learned how to run a t.test. Let's try it out now.
try(t.test(score ~ method, data = memory))


## ----anova---------------------------------------------------------------------
library(supernova)

# running an anova
anova_model <- aov(score ~ method, data = memory)

summary(anova_model)

supernova(anova_model)  # clearer ANOVA table


## ----post-hoc------------------------------------------------------------------
TukeyHSD(anova_model)


## ----post-hoc-plot----
plot(TukeyHSD(anova_model))


## ----adding-caffeine-----------------------------------------------------------
# Let's say we also measured caffeine intake (low vs high)
set.seed(42)
memory2 <- memory %>%
  mutate(
    caffeine = rep(c("Low", "High"), times = 30))


## ----interaction---------------------------------------------------------------
# To see the interaction between method and caffeine levels, we add a *
anova_2 <- aov(score ~ method * caffeine, data = memory2)

summary(anova_2)

supernova(anova_2)


## ----model-comparison----------------------------------------------------------
# Compare one-way vs. two-way models
library(AICcmodavg)
model.set <- list(
  aov(score ~ method, data = memory2),
  aov(score ~ method * caffeine, data = memory2))

model.names <- c("One-way", "Two-way")

aictab(model.set, modnames = model.names)

