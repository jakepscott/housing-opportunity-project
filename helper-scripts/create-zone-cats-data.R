# Load libs ---------------------------------------------------------------
library(tidyverse)
library(here)
library(sf)
library(janitor)
library(tidycensus)

# Load Data ---------------------------------------------------------------
# PLUTO
pluto <- read_csv(here("data/raw/pluto/pluto_24v3_1.csv"),
                  col_select =  c(bbl))

# PRIMO
# Potentials: lotarea, builtfar, residfar, resarea/lotarea
amens_long <- read_csv(here("data/created/prime-bbls/num-amens-long.csv"))
amens_wide <- read_csv(here("data/created/prime-bbls/num-amens-wide.csv"))


# Make zones --------------------------------------------------------------
num_amens_bbl <- amens_long %>% 
  group_by(bbl) %>% 
  summarise(num_amens = sum(in_distance)) %>% 
  mutate(num_amens_cat = factor(num_amens,
                                levels = 0:5, 
                                labels = c("None", "One", "Two", "Three", 
                                           "Four", "Five")))

# Which Missing -----------------------------------------------------------------
# Zone 4 specific
zone_4 <- num_amens_bbl %>% 
  filter(num_amens == 4)

# Add cats
zone_4_cats <- amens_wide %>% 
  filter(bbl %in% zone_4$bbl) %>% 
  mutate(category = case_when(!has_subway ~ "Missing Subway",
                              !has_market ~ "Missing Supermarket",
                              !has_no_flood ~ "In Floodplain",
                              !has_school ~ "Missing Public School", 
                              !has_park ~ "Missing Park")) %>% 
  select(bbl, category)

# Save --------------------------------------------------------------------
write_csv(num_amens_bbl, here("data/created/near-prime-bbls/amenity-zone-cats.csv"))
write_csv(zone_4_cats, here("data/created/near-prime-bbls/zone-4-with-cats.csv"))

