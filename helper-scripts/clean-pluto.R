# Load libs ---------------------------------------------------------------
library(tidyverse)
library(here)
library(sf)
library(janitor)
library(tidycensus)

# Load data ---------------------------------------------------------------
pluto_raw <- read_csv(here("data/raw/pluto/pluto_24v3_1.csv"))

# Clean tibble data --------------------------------------------------------------
pluto <- pluto_raw %>% 
  mutate(landuse = as.numeric(landuse), 
         landuse_cat = case_when(landuse == 1 ~ "One and Two Family",
                                 landuse == 2 ~ "Multifamily Walk-Up", 
                                 landuse == 3 ~ "Multi-Family Elevator", 
                                 landuse == 4 ~ "Mixed Resi/Commercial", 
                                 landuse == 5 ~ "Commercial & Office", 
                                 landuse == 6 ~ "Industrial/Manufacturing", 
                                 landuse == 7 ~ "Transportation & Utility", 
                                 landuse == 8 ~ "Public Facilities", 
                                 landuse == 9 ~ "Open Space", 
                                 landuse == 10 ~ "Parking", 
                                 landuse == 11 ~ "Vacant")) %>%
  mutate(bldgclass_cat = case_when(substr(bldgclass, 1, 1)=="A" ~ "One family",
                                   substr(bldgclass, 1, 1)=="B" ~ "Two family",
                                   substr(bldgclass, 1, 1)=="C" ~ "Walk Up Apartment",
                                   substr(bldgclass, 1, 1)=="D" ~ "Elevator Apartment",
                                   substr(bldgclass, 1, 1)=="E" ~ "Warehouses",
                                   substr(bldgclass, 1, 1)=="F" ~ "Factory/Industrial",
                                   substr(bldgclass, 1, 1)=="G" ~ "Misc. Garages and Gasoline Stations",
                                   substr(bldgclass, 1, 1)=="H" ~ "Hotels",
                                   substr(bldgclass, 1, 1)=="I" ~ "Hospitals/Health",
                                   substr(bldgclass, 1, 1)=="J" ~ "Theaters",
                                   substr(bldgclass, 1, 1)=="K" ~ "Store Buildings",
                                   substr(bldgclass, 1, 1)=="L" ~ "Loft Buildings",
                                   substr(bldgclass, 1, 1)=="M" ~ "Churches/Religious Institutions",
                                   substr(bldgclass, 1, 1)=="N" ~ "Asylums and Homes",
                                   substr(bldgclass, 1, 1)=="O" ~ "Office",
                                   substr(bldgclass, 1, 1)=="P" ~ "Places of Public Assembly and Cultural",
                                   substr(bldgclass, 1, 1)=="Q" ~ "Outdoor Recreation",
                                   substr(bldgclass, 1, 1)=="R" ~ "Condominiums",
                                   substr(bldgclass, 1, 1)=="S" ~ "Residence - Multiple Use",
                                   substr(bldgclass, 1, 1)=="T" ~ "Transportation Facilities",
                                   substr(bldgclass, 1, 1)=="U" ~ "Utility Bureau Properties",
                                   substr(bldgclass, 1, 1)=="V" ~ "Vacant", 
                                   substr(bldgclass, 1, 1)=="W" ~ "Educational Structure", 
                                   substr(bldgclass, 1, 1)=="Y" ~ "Selected Government Installations", 
                                   substr(bldgclass, 1, 1)=="Z" ~ "Misc.")) %>% 
  mutate(bldgclass_cat = case_when(bldgclass_cat == "Misc. Garages and Gasoline Stations" & 
                                     bldgclass %in% c("G0", "G1", "G6", "G7") ~ "Parking", 
                                   bldgclass_cat == "Misc. Garages and Gasoline Stations" & 
                                     bldgclass %in% c("G3", "G4", "G5") ~ "Gas Stations", 
                                   T ~ bldgclass_cat))

# Save data ---------------------------------------------------------------
write_csv(pluto, here("data/created/pluto-clean.csv"))

