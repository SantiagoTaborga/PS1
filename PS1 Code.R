# Problem Set 1
# Santiago Taborga
# 2025-01-29

# Setup
library(tidyverse)
library(knitr)
library(haven)
library(Matrix)
library(lfe)
library(dplyr)
library(tidyr)

madagascar <- read_spss('Madagascar_Round9.sav') %>%
  select(DATEINTR, Q1, Q2, Q100, Q101, Q78A, Q78B) %>%
  mutate(Per = 1 / nrow(madagascar) *100)

# 2. Describe respondents -----

tab1 <- 
  madagascar %>%
  select(Q2, Q100, Q101, Per, N) %>%
  pivot_longer(cols = -Per) %>%
  group_by(name, value) %>%
  summarise(Percent = round(sum(Per), 2),
            Number = sum(N)) %>% #test
  mutate(name = case_when(name == "Q100" ~ "Gender",
                          name == "Q101" ~ "Race",
                          name == "Q2" ~ "Language"),
         value = case_when((name == "Gender" & value == 1) ~ "Men",
                           (name == "Gender" & value == 2) ~ "Women",
                           (name == "Race" & value == 1) ~ "Black/African",
                           (name == "Race" & value == 2) ~ "White/European",
                           (name == "Race" & value == 3) ~ "Coloured/Mixed Race",
                           (name == "Language" & value == 420) ~ "Malagasy officiel",
                           (name == "Language" & value == 421) ~ "Malagasy avec spécificité régionale",
                           (name == "Language" & value == 9995) ~ "Other"))
  
colnames(tab1)[which(names(tab1) == "value")] <- "Value" 
colnames(tab1)[which(names(tab1) == "name")] <- "Variable"
tab1

# 3. China Attitudes -----
## Creating the new data
tab2 <- 
  madagascar %>%
  count(Q78A,name = "Number") %>%
  mutate(Percent = round((100 * Number/sum(Number)), 2))

## Finding the variable with the highest value

tab2$Q78A[tab2$Percent == max(tab2$Percent)]


## Making the table more presentable
tab2 <- tab2 %>%
  mutate(Q78A = case_when(Q78A == 1 ~ "Very negative",
                          Q78A == 2 ~ "Somewhat negative",
                          Q78A == 3 ~ "Neither positive nor negative",
                          Q78A == 4 ~ "Somewhat positive",
                          Q78A == 5 ~ "Very positive",
                          Q78A == 8 ~ "Refused",
                          Q78A == 9 ~ "Don't know",
                          Q78A == -1 ~ "Missing"))

colnames(tab2)[which(names(tab2) == "Q78A")] <- "Influence"
## Finding the variable with the highest value part 2
tab2$Influence[tab2$Percent == max(tab2$Percent)]

## Making the table more presentable part 2

tab2 <- kable(tab2, caption = "Political and Economic Influence: China")
tab2

# 4. United States attitudes ------
## Repeated everything, just changing the variable to Q78B

# 5. t-test -----

madagascar <-
  madagascar %>%
  mutate(
    across(
      Q78A:Q78B,
      ~ if_else(.x %in% 1:5, .x, NA)
    )
  )
t.test(madagascar$Q78A, madagascar$Q78B, paired = TRUE)

        