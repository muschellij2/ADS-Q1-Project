wd = "C:/Users/divya/Documents/JHU/Y2/Q1/Advanced Data Science 711/Term 1 Project/Data"
setwd(wd)

library(BCRA); library(data.table); library(dplyr)

#Setting up data frame for use with BCRA package
public_sgk_female_age35_race_BCRA = public_sgk_female_age35_race %>% 
  mutate(T2 = age + 5, N_Biop = rep(99, nrow(.)), HypPlas = rep(99, nrow(.))) %>% 
  mutate(AgeMen = rep(99, nrow(.)), Age1st = rep(99, nrow(.)), N_Rels = rep(99, nrow(.))) %>% 
  mutate(id = 1:nrow(.), Race = racepredict) %>% 
  select(id, from_id, FirstName:Race) %>% 
  rename(T1 = age)
public_sgk_female_age35_race_BCRA$Race = as.character(public_sgk_female_age35_race_BCRA$Race)
public_sgk_female_age35_race_BCRA$Race = recode(public_sgk_female_age35_race_BCRA$Race, white = 1, black = 2, hisp = 3, other = 4, asian = 11)

#check.summary(public_sgk_female_age35_race_BCRA)
public_sgk_female_age35_race_BCRA = public_sgk_female_age35_race_BCRA %>% mutate(abs.risk = absolute.risk(public_sgk_female_age35_race_BCRA)/100)
write_csv(public_sgk_female_age35_race_BCRA, paste("C:/Users/divya/Documents/JHU/Y2/Q1/Advanced Data Science 711/Term 1 Project/Data/public_sgk_female_age35_race_BCRA", "_", filedate, "_raw.csv", sep = ""))