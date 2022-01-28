# include all libraries required by the R chunks here
library(knitr)
library(here)
library(bookdown)
library(dplyr)
library(tidyverse)

# wrangle data
raw_data <- read.csv(here::here("data", "fan_data_raw.csv"), stringsAsFactors = TRUE)

Q1 <- filter(raw_data, Surface == "Q1a" | Surface == "Q1b") %>% 
  mutate(tot_spots = sum(No_Spots)) %>% # age and band sd already calc, Q1a already averaged
  select(-Alluvial.Fan, -Surface, -No_Spots) %>% 
  colMeans() %>% 
  as.numeric()
save(Q1, file = here::here("data", "Q1.rda"))

# average data by surface
Q4 <- filter(raw_data, Surface == "Q4") %>% # filter by surface name
  mutate(B1_std = sd(B1), B2_std = sd(B2), B3_std = sd(B3), # calc std band reflectance
         B4_std = sd(B4),  B5_std = sd(B5), B6_std = sd(B6), B7_std = sd(B7),
         age_std = sd(age), # calc std age
         tot_spots = sum(No_Spots)) %>% # add column for sum of spot samples for surface
  select(-Alluvial.Fan, -Surface, -No_Spots) %>% # remove character vectors
  colMeans() %>% # take mean of columns (e.g. age, std, band values)
  as.numeric()# make sure all is numeric 

Q3 <- filter(raw_data, Surface == "Q3") %>% 
  mutate(B1_std = sd(B1), B2_std = sd(B2), B3_std = sd(B3),
         B4_std = sd(B4),  B5_std = sd(B5), B6_std = sd(B6), B7_std = sd(B7),
         age_std = sd(age),
         tot_spots = sum(No_Spots)) %>% 
  select(-Alluvial.Fan, -Surface, -No_Spots) %>% 
  colMeans() %>% 
  as.numeric()

Q2c <- filter(raw_data, Surface == "Q2c") %>% 
  mutate(B1_std = sd(B1), B2_std = sd(B2), B3_std = sd(B3),
         B4_std = sd(B4),  B5_std = sd(B5), B6_std = sd(B6), B7_std = sd(B7),
         age_std = sd(age),
         tot_spots = sum(No_Spots)) %>% 
  select(-Alluvial.Fan, -Surface, -No_Spots) %>% 
  colMeans() %>% 
  as.numeric() 

Q2b <- filter(raw_data, Surface == "Q2b") %>% 
  mutate(B1_std = sd(B1), B2_std = sd(B2), B3_std = sd(B3),
         B4_std = sd(B4),  B5_std = sd(B5), B6_std = sd(B6), B7_std = sd(B7),
         age_std = sd(age),
         tot_spots = sum(No_Spots)) %>% 
  select(-Alluvial.Fan, -Surface, -No_Spots) %>% 
  colMeans() %>% 
  as.numeric()

Q2a <- filter(raw_data, Surface == "Q2a") %>% 
  mutate(B1_std = sd(B1), B2_std = sd(B2), B3_std = sd(B3),
         B4_std = sd(B4),  B5_std = sd(B5), B6_std = sd(B6), B7_std = sd(B7),
         age_std = sd(age),
         tot_spots = sum(No_Spots)) %>% 
  select(-Alluvial.Fan, -Surface, -No_Spots) %>% 
  colMeans() %>% 
  as.numeric()

Q1b <- filter(raw_data, Surface == "Q1b") %>% 
  mutate(B1_std = sd(B1), B2_std = sd(B2), B3_std = sd(B3),
         B4_std = sd(B4),  B5_std = sd(B5), B6_std = sd(B6), B7_std = sd(B7),
         age_std = sd(age),
         tot_spots = sum(No_Spots)) %>% 
  select(-Alluvial.Fan, -Surface, -No_Spots) %>% 
  colMeans() %>% 
  as.numeric()

Q1a <- filter(raw_data, Surface == "Q1a") %>% 
  mutate(tot_spots = sum(No_Spots)) %>% # age and band sd already calc, Q1a already averaged
  select(-Alluvial.Fan, -Surface, -No_Spots) %>% 
  colMeans() %>% 
  as.numeric()

# merge surfaces into single data frame
# set up
attribute_names <- c("age", "age_sd", "B1", "B1_sd", "B2", "B2_sd", 
               "B3", "B3_sd", "B4", "B4_sd","B5", "B5_sd", 
               "B6", "B6_sd", "B7", "B7_sd", "tot_spots", "surface")

surface_names <- c("Q4", "Q3", "Q2c", "Q2b","Q2a", "Q1b", "Q1a")

# wide 
all_fans <- rbind(Q4, Q3, Q2c, Q2b, Q2a, Q1b, Q1a) %>% as.data.frame() %>% 
  mutate("surface" = surface_names)
colnames(all_fans) <- attribute_names
save(all_fans, file = here::here("data", "all_fans.rda")) # export df for use elsewhere

# long
attribute_names <- c("age", "age_sd", "B1", "B1_sd", "B2", "B2_sd", 
                     "B3", "B3_sd", "B4", "B4_sd","B5", "B5_sd", 
                     "B6", "B6_sd", "B7", "B7_sd", "tot_spots")
all_fans_long <- data.frame(Q4, Q3, Q2c, Q2b, Q2a, Q1b, Q1a, row.names = attribute_names)
save(all_fans_long, file = here::here("data", "all_fans_long.rda"))

