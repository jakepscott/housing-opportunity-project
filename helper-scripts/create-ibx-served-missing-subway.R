# Load libs ---------------------------------------------------------------
library(tidyverse)
library(here)
library(sf)
library(janitor)
library(tidycensus)

# Load Data ---------------------------------------------------------------
# Zone 4 category
zone_4_cats <-  read_csv(here("data/created/near-prime-bbls/zone-4-with-cats.csv"))

# Pluto shapefile
pluto_sf_raw <- read_sf(here("data/raw/pluto/nyc_mappluto_24v3_1_shp/MapPLUTO.shp"), 
                        query = 'SELECT bbl from MapPLUTO') %>% 
  clean_names()
# IBX
ibx_raw <- read_sf(here("data/raw/ibx/Unofficial-Interborough-Express-Alignment.shp"))

# Clean data --------------------------------------------------------------
# IBX
ibx <- ibx_raw %>% 
  st_transform(st_crs(pluto_sf_raw)) 

ibx_buffer <- ibx %>% 
  st_buffer(dist = 2640)

# Near-prime lots missing subway
missing_subway <- zone_4_cats %>% 
  filter(category == "Missing Subway")

missing_subway_sf <- missing_subway %>% 
  left_join(pluto_sf_raw) %>% 
  st_as_sf()

# Which would touch IBX ---------------------------------------------------
# Which would intersect with IBX buffer
intersections <-  missing_subway_sf %>% 
  st_intersects(ibx_buffer, sparse = T)

##lengths() computes the length of each element in the intersections list.
# I make it into a tibble where each row is the # of times the PLUTO tax lot in that row
# intersected the bus data. If it is 0, that means it did not intersect. So I only keep the ones
# where the number of intersections is greater than 0 
served_by_ibx <- missing_subway_sf %>% 
  bind_cols(intersections %>% 
              lengths() %>% 
              as_tibble() %>% 
              rename(num_intersections = value)) %>% 
  filter(num_intersections > 0) %>% 
  select(bbl) %>% 
  as_tibble() %>% 
  select(bbl)

write_csv(served_by_ibx, here("data/created/ibx/missing-subway-served-by-ibx.csv"))
