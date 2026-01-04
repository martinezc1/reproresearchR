# Introduction To tidyverse
# Reproducible Research Using R
# (Helper Script)

## ----installing-packages----------------------------------------------------------
install.packages("tidyverse")
install.packages("palmerpenguins")
install.packages("fortunes")
install.packages("cowsay")

## ----loading-packages----------------------------------------------------------
library(tidyverse)
library(___)
library(___)
library(___)

# Quick sanity check that packages are loaded
sessionInfo()$otherPkgs %>% names()


## ----fun-packages--------------------------------------------------------------
# Fun packages
fortunes::fortune()   # random quote about R

cowsay::say("Welcome to tidyverse!", by = "cow")


## install.packages("nycOpenData")

## ----nycOpenData---------------------------------------------------------------
library(nycOpenData)
head(nyc_311())


## ----pipe-intro----------------------------------------------------------------
# In base R, to find the square root of a number, we would use this:
sqrt(42)

# With piping, we could rewrite it like this:
42 %>% ___()


## ----pipe-cleaning-------------------------------------------------------------
str_to_upper(str_trim(" i love r! "))

" i love r! " %>% ___() %>% ___()

# You can also move down instead if you like
# " Adelie " %>%
#   str_trim() %>%
#  str_to_upper()


## ----penguins------------------------------------------------------------------
ncol(penguins)      # Finding out the number of columns

nrow(___)      # Finding out the number of rows

colnames(penguins)  # Finding out the names of the columns


## ----distinct-one--------------------------------------------------------------
penguins %>% distinct(species)


## ----distinct-two--------------------------------------------------------------
penguins %>% distinct(species, island)


## ----select-names--------------------------------------------------------------
penguins %>% select(___, island, bill_length_mm, ___)


## ----select-index--------------------------------------------------------------
# This is saying let's take the first column, and then every column from 3 to 5
penguins %>% select(1,3:5) 


## ----filter--------------------------------------------------------------------
penguins %>%
  filter(species == "Adelie")


## ----filter-and----------------------------------------------------------------
penguins %>%
  ___(species == "Adelie" & island == "Torgersen")


## ----filter-or-----------------------------------------------------------------
penguins %>%
  ___(species == "Adelie" | island == "Torgersen")


## ----filter-logical------------------------------------------------------------
penguins %>% filter(body_mass_g >= 4000)

penguins %>% filter(___ %in% c("Adelie", "Gentoo"))


## ----filter-na-----------------------------------------------------------------
penguins %>% filter(is.na(body_mass_g))    # Filter for rows that have missing data

penguins %>% drop_na(___)          # Filters for rows that don't have missing data


## ----arrange-------------------------------------------------------------------
penguins %>% arrange(body_mass_g)          # ascending order

penguins %>% arrange(desc(___))    # descending order


## ----mutate--------------------------------------------------------------------
# Base R
penguins$bill_ratio <- penguins$___ / penguins$bill_depth_mm

penguins<- penguins %>%
  mutate(bill_ratio = bill_length_mm / ___)


## ----if-else-------------------------------------------------------------------
penguins %>%
  mutate(size_category = if_else(body_mass_g >= 3500, "Big","Small"))


## ----case-when-----------------------------------------------------------------
penguins %>%
  mutate(size_category = case_when(
    body_mass_g <= 3500 ~ "Small",
    ___ > 3500 & body_mass_g <= 4000 ~ "Big",
    ___ > 4000 ~ "Gigantic",
    TRUE ~ "Unknown"   # catch-all for NAs or anything else
  ))


## ----rename--------------------------------------------------------------------
penguins %>% rename(penguin_types = species) 


## ----all-together--------------------------------------------------------------
penguins_new <- penguins %>% 
  select(species, island, bill_length_mm, ___) %>%
  filter(___ == "Adelie") %>%
  mutate(size_category = if_else(___ >= 3500, "Big","Small")) %>%
  arrange(desc(body_mass_g)) %>% 
  rename(penguin_types = species) 


## ----tables--------------------------------------------------------------------
table(penguins$species)

penguins %>% count(___, sort = TRUE)


## ----summarize-----------------------------------------------------------------
penguins %>%
  group_by(___) %>%
  summarize(
    n         = n(),
    mean_mass = mean(body_mass_g, na.rm = TRUE))


## filter(species = "Adelie")  # WRONG
## 
## filter(species == "Adelie") # RIGHT

## ----na-math-------------------------------------------------------------------
mean(penguins$___)                 # Returns NA if any missing

mean(___$body_mass_g, na.rm = TRUE)   # CORRECT


## penguins %>%
##   select(species)     # Good
## 
## penguins
## %>% select(species)   # Bad

## penguins_select <- penguins %>%
##   select(species, island, bill_length_mm, body_mass_g)
## 
## penguins_new <- penguins_select %>%
##   filter(species == "Adelie")

## penguins_new <- penguins %>%
##   select(species, island, bill_length_mm, body_mass_g) %>%
##   filter(species == "Adelie")
