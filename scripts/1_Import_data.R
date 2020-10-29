# LDP Reproducibility Assignment
# Seabird Decline in Canada project

# Load packages -----------------------------------------------------------

library(tidyverse)
library(janitor)
library(rgbif)
library(ggplot2)

# Import data -------------------------------------------------------------

# https://open.canada.ca/data/en/dataset/2d533032-3dc2-4302-b831-65e1bdcf78e7
trends <- read_csv("data/raw/trends-bird-populations.csv")
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
  mutate(species_group =  snakecase::to_snake_case(tolower(species_group))) %>% 
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
  sort(unique(trends_seabirds$species))

# Join --------------------------------------------------------------------

QCSB_joined <- QCSB_clean %>% 
  left_join(trends_seabirds, by = c("espece_species_en" = "species"))

# Basic vizualization -----------------------------------------------------

trends_clean %>% 
  ggplot(aes(x = year, y = value)) +
  geom_line(aes(col = species_group))

QCSb_subset <- QCSB_joined %>% 
  filter(!is.na(status)) %>% 
  filter(annee_year >1970) %>% 
  filter(nombre_de_nicheurs_number_of_breeders != 0) %>% 
  group_by(espece_species_en) %>% 
  mutate(n = n()) %>% ungroup %>% 
  filter( n >= 30) %>% 
  group_by(annee_year, espece_species_en) %>% 
  summarise(total = sum(nombre_de_nicheurs_number_of_breeders)) %>% ungroup

QCSb_subset %>% 
  ggplot(aes(x = annee_year, y = total, color = espece_species_en)) +
  geom_point() + geom_smooth(se = T) +
  scale_y_log10()
