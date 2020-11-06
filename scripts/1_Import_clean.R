# LDP Reproducibility Assignment
# Seabird Decline in Canada project

# Load packages -----------------------------------------------------------

library(tidyverse)
library(janitor)
# library(sf)

# Import data -------------------------------------------------------------

# Trends 
# https://open.canada.ca/data/en/dataset/2d533032-3dc2-4302-b831-65e1bdcf78e7
trends <- read_csv("data/raw/trends-bird-populations.csv")
# Long term trends
# https://open.canada.ca/data/en/dataset/766a71ce-d798-49e2-bee2-7ec65c7c5d56
long_term <- read_csv("data/raw/long-term-changes-bird-populations.csv")
# QC seabirds
QCSB <- read_csv("data/raw/CDQS_BIOMQ_2017.csv")

# Clean data --------------------------------------------------------------

trends_clean <- trends %>% 
  row_to_names(2) %>% 
  rename(year = Year) %>%
  slice_head(n = nrow(.)-4) %>%
  mutate(year = as.numeric(year)) %>% 
  pivot_longer(cols = 2:ncol(.), 
               names_to = "species_group") %>% 
  mutate(species_group = str_remove(species_group, pattern = "[ ]\\(.*")) %>% 
  # mutate(species_group =  snakecase::to_snake_case(tolower(species_group))) %>% 
  mutate(value = as.numeric(ifelse(value == "n/a", NA, value))) 

long_term_clean <- long_term %>% 
  row_to_names(2) %>% 
  clean_names() %>% 
  slice_head(n = nrow(.)-4) %>% 
  mutate(species = str_split(species, ",")) %>% 
  unnest(cols = c(species)) %>% 
  mutate(species = str_replace(species, "\\[A\\] ", "")) %>% 
  mutate(species = str_trim(species))

QCSB_clean <- QCSB %>% 
  clean_names()

# Collect species name ----------------------------------------------------

trends_seabirds <- long_term_clean %>% 
  filter(species_group == 'Seabirds')
# 15 species
sort(unique(QCSB_clean$espece_species_en)) %in% 
  sort(unique(trends_seabirds$species)) %>% sum()

# Join --------------------------------------------------------------------

QCSB_joined <- QCSB_clean %>% 
  left_join(trends_seabirds, by = c("espece_species_en" = "species"))

# Export clean data -------------------------------------------------------

write_csv(trends_clean, "data/clean/trends_clean.csv")
write_csv(long_term_clean, "data/clean/long_term_clean.csv")
write_csv(QCSB_joined, "data/clean/QCSB_joined_clean.csv")

# -------------------------------------------------------------------------