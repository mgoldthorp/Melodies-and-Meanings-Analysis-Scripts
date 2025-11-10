library(dplyr)
library(ggplot2)
library(purrr)


df <- read.csv("./MM_Demographics.csv", nrows = 111)

##Mean, Min, & Max age by condition
age <- df %>%
  mutate(
    condition = as.factor(Condition), 
    age = as.numeric(Age)
    ) %>%
  group_by(condition) %>%
  summarise(mean_age = mean(age),
            min(age),
            max(age)
            )

## Gender by condition
gender <- df %>%
  count(Condition, Gender)

## Race by condition
race <- df %>%
  group_by(Condition) %>%
  summarise(across(6:13, ~ sum(. == 1, na.rm = TRUE)))


## Ethnicity by condition
ethnicity <- df %>%
  group_by(Condition) %>%
  summarise(across(14:16, ~ sum(. == 1, na.rm = TRUE)))

