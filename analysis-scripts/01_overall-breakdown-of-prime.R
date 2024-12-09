# Load libs ---------------------------------------------------------------
library(tidyverse)
library(here)
library(sf)
library(janitor)
library(tidycensus)

# Load Data ---------------------------------------------------------------
# BBLs
prime <- read_csv(here("data/created/prime-bbls/prime-bbls.csv"))

# PLUTO
pluto <- read_csv(here("data/created/pluto-clean.csv"),
                      col_select =  c(bbl, landuse,
                                      landuse_cat, unitstotal,unitsres, address, borough))

# Join data ---------------------------------------------------------------
df <- prime %>% 
  left_join(pluto)

# Total prime buildings and units -----------------------------------------
# 237,406 tax lots
nrow(df)

# That is 27.7% of tax lots in the city
nrow(df)/nrow(pluto)

# 1,877,810 residential units
df %>% 
  summarise(total=sum(unitsres, na.rm = T))

# That is 50.7% of residential units in the city
(df %>% 
    summarise(total=sum(unitsres, na.rm = T)) %>% 
    pull(total))/(pluto %>%
                    summarise(total = sum(unitsres, na.rm = T)) %>% 
                    pull(total))*100

# By landuse class -------------------------------------------------------
landuse_agg <- df %>% 
  filter(!is.na(landuse)) %>% 
  group_by(landuse, landuse_cat) %>% 
  summarise(lots = n(),
            units = sum(unitstotal)) %>% 
  ungroup() %>% 
  mutate(per_lots = lots/sum(lots)*100, 
         per_units = units/sum(units, na.rm = T)*100)  %>% 
  ungroup()

# Most lots are resi
landuse_agg %>% 
  mutate(broad = ifelse(landuse %in% c(1:3), "Resi", landuse_cat)) %>% 
  group_by(broad) %>% 
  summarise(lots = sum(lots),
            per_lots = sum(per_lots),
            units = sum(units), 
            per_units = sum(per_units)) %>% 
  arrange(desc(per_lots)) 


# By borough --------------------------------------------------------------
# Share of lots that are PRIME by borough
pluto %>% 
  left_join(prime %>% 
              mutate(prime = T)) %>% 
  mutate(prime = ifelse(is.na(prime), F, prime)) %>% 
  group_by(borough, prime) %>% 
  summarise(n = n()) %>% 
  ungroup() %>%
  group_by(borough) %>% 
  mutate(per = n/sum(n)*100) %>% 
  filter(prime)
