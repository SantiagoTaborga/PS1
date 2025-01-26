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
  select(Q2, Q100, Q101, Per) %>%
  pivot_longer(cols = -Per) %>%
  group_by(name, value) %>%
  summarise(Percent = round(sum(Per), 2)) %>%
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

 
