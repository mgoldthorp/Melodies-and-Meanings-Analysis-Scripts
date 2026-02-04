library(dplyr)
library(ggplot2)
library(purrr)
library(sjPlot)
library(officer)
library(flextable)
library(gdtools)



df <- read.csv("./MM_Demographics.csv")
df <- df[-c(53, 54, 85, 86, 87, 88),]

##Mean, Min, & Max age by condition
age <- df %>%
  mutate(
    condition = as.factor(Condition),
    age = as.numeric(Age)
    ) %>%
  group_by(condition) %>%
  summarise(mean = round(mean(age, na.rm = TRUE), 2),
            SD = round(sd(age), 2)
            #min = min(age, na.rm = TRUE),
            #max = max(age, na.rm = TRUE)
            )

## Gender by condition
gender_tbl <- df %>%
  group_by(Condition) %>%
  count(Gender) %>%
  tidyr::pivot_wider(
    names_from = Gender,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(Gender = paste0(f+m, "(", m, ")")) %>%
  select(Condition, Gender) %>%
  rename("N(Male)" = Gender)

## Race by condition
race_tbl <- df %>%
  group_by(Condition) %>%
  summarise(
    White = sum(W == 1, na.rm = TRUE),
    AA = sum(AA == 1, na.rm = TRUE),
    Other = sum(
      ASIAN == 1 |
      HISPANIC == 1 |
      ME == 1 |
      NH == 1 |
      OTHER == 1,
      na.rm = TRUE
    )) %>%
  mutate(
    total = White + AA + Other,
    across(c(White, AA, Other), ~round(.x / total, 2)
           )
    ) %>%
  select(-total)


## Ethnicity by condition
eth_tbl <- df %>%
  group_by(Condition) %>%
  summarise(
    Hispanic = sum(Hispanic == 1, na.rm = TRUE),
    NonHispanic = sum(Non.hispanic == 1, na.rm = TRUE)) %>%
  mutate(
    total = Hispanic + NonHispanic,
    across(c(Hispanic, NonHispanic), ~round(.x / total, 2)
    )
  ) %>%
  select(-total)

##Toddler Interactions
todint_tbl <- df %>%
  group_by(Condition) %>%
  summarise(
    weekly = sum(across(21:23, ~.x == 1), na.rm = TRUE),
    other = sum(across(24:25, ~.x  == 1), na.rm = TRUE),
    total = n()) %>%
  mutate(
    weekly = round(weekly/total, 2),
    other = round(other/total, 2)
  ) %>%
  select(-total)

##combine into one APA-style table
demo_tbl <- age %>%
  left_join(gender_tbl, by = c("condition" = "Condition")) %>%
  left_join(race_tbl, by = c("condition" = "Condition")) %>%
  left_join(todint_tbl, by = c("condition" = "Condition")) %>%
  select(condition, 'N(Male)', mean, SD, everything())

ft <- demo_tbl %>%
  flextable() %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 12, part = "all") %>%
  bold(i = ~condition %in% c("Age", "Gender", "Race", "Ethnicity")) %>%
  align(i = ~condition %in% c("Age", "Gender", "Race", "Ethnicity"), align = "left") %>%
  set_table_properties(layout = "autofit")
sect_properties <- prop_section(
  page_size = page_size(
    orient = "portrait",
    width = 8.5, height = 11
  ),
  type = "continuous",
  page_margins = page_mar(
    left = 1,
    right = 1
  )
)

save_as_docx(ft, path = "Demographics_Table1.docx", pr_section = sect_properties)
