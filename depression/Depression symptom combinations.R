# Load libraries
library(UpSetR)
library(tidyverse)

# Source: https://www.sciencedirect.com/science/article/pii/S0165032714006326
# Citation: Fried, E. I., & Nesse, R. M. (2015). Depression is not a consistent syndrome: an investigation of unique symptom patterns in the STAR* D study. Journal of affective disorders, 172, 96-102.

# Enter intersections
expressionInput <- c(None=178, `Sad&Ene&Con&Ins&Int&App&Bla&Wei&Agi&Ret&Sui`=124,  `Sad&Ene&Con&Ins&Int&App&Wei`=119, `Sad&Ene&Con&Ins&Int&App&Bla&Wei`=119, `Sad&Ene&Con&Ins&Int&App`=113, `Sad&Ene&Con&Ins&Int&Bla`=113, `Ins`=108, `Sad&Ene&Con&Ins&Int&App&Bla&Wei&Agi`=100, `Sad&Ene&Con&Ins`=92, `Sad&Ene&Con&Ins&Int&App&Wei&Agi&Ret`=89)

# Replace abbreviations with names
names(expressionInput) <- names(expressionInput) %>%
  str_replace("Sad", "Sadness") %>%
  str_replace("Ene", "Energy loss") %>%
  str_replace("Con", "Concentration problems") %>%
  str_replace("Ins", "Insomnia") %>%
  str_replace("Int", "Interest loss") %>%
  str_replace("App", "Appetite problems") %>%
  str_replace("Bla", "Self-blame") %>%
  str_replace("Wei", "Weight problems") %>%
  str_replace("Agi", "Psychomotor agitation") %>%
  str_replace("Ret", "Psychomotor retardation") %>%
  str_replace("Sui", "Suicidal ideation") %>%
  str_replace("Hyp", "Hypersomnia")
    
# Create Upset plot
upset(fromExpression(expressionInput), nsets=11, order.by = "freq", mainbar.y.label = "Frequency of profile", sets.x.label = "Frequency of symptoms")
