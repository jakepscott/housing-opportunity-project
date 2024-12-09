# Load libs ---------------------------------------------------------------
library(tidyverse)
library(here)
library(sf)
library(janitor)

# Load data ---------------------------------------------------------------
pluto_raw <- read_csv(here("data/raw/pluto/pluto_24v3_1.csv"),
                      col_select =  c("bbl", "bldgclass")) %>% 
  clean_names()

tax_lots_with_subway <- read_csv(here("data/created/intersections/tax_lots_subway_5_buffer.csv")) %>% 
  mutate(has_subway = T)

tax_lots_with_parks <- read_csv(here("data/created/intersections/tax_lots_parks_25_buffer.csv")) %>% 
  mutate(has_park = T)

tax_lots_with_market <- read_csv(here("data/created/intersections/tax_lots_retail_5_buffer.csv")) %>% 
  mutate(has_market = T)

tax_lots_with_school <- read_csv(here("data/created/intersections/tax_lots_schools_1_buffer.csv")) %>% 
  mutate(has_school = T) %>% 
  select(bbl, has_school)

tax_lots_with_no_flood <- read_csv(here("data/created/intersections/non-flood-zone-tax-lots.csv")) %>% 
  mutate(has_no_flood = T)


# Join data ---------------------------------------------------------------
pluto_df <- pluto_raw %>% 
  # Remove parks from prime lots
  filter(bldgclass != "Q1") %>% 
  select(bbl)


prime_wide <- pluto_df %>% 
  left_join(tax_lots_with_subway) %>% 
  left_join(tax_lots_with_market) %>% 
  left_join(tax_lots_with_no_flood) %>% 
  left_join(tax_lots_with_school) %>% 
  left_join(tax_lots_with_parks) %>% 
  mutate(across(-bbl, ~ifelse(is.na(.), F, .))) 

prime_long <- prime_wide %>% 
  pivot_longer(-bbl, names_to = "amenity", values_to = "in_distance") 

# Create just prime bbls --------------------------------------------------
just_prime <- prime_long %>%
  group_by(bbl) %>% 
  summarise(num_amens = sum(in_distance)) %>% 
  filter(num_amens == 5) %>% 
  select(bbl)

  
# Save data --------------------------------------------------------------
write_csv(prime_wide, here("data/created/prime-bbls/num-amens-wide.csv"))
write_csv(prime_long, here("data/created/prime-bbls/num-amens-long.csv"))
write_csv(just_prime, here("data/created/prime-bbls/prime-bbls.csv"))
