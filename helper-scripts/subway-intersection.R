# Load libs ---------------------------------------------------------------
library(tidyverse)
library(here)
library(sf)
library(janitor)

# Load data ---------------------------------------------------------------
# PLUTO
pluto_sf_raw <- read_sf(here("data/raw/pluto/nyc_mappluto_24v3_1_shp/MapPLUTO.shp"), 
                        query = 'SELECT bbl from MapPLUTO') %>% 
  clean_names()

# Subway stops
entrances_raw <- read_sf(here("data/raw/subways/MTA Subway Entrances and Exits_ 2024_20241128/geo_export_9eb7d1ac-307d-4dd8-84fd-7afd3ae37068.shp"))

# Clean data ------------------------------------------------------------
#Only include is entry is allowed
entrances <- entrances_raw %>% 
  filter(entry_allo == "YES")

# Buffering subways -------------------------------------------------------
entrances_buffer <- entrances %>% 
  st_transform(crs = st_crs(pluto_sf_raw)) %>% 
  st_buffer(dist = 2640)


# Find tax lots in subway buffer ------------------------------------------
# Does the tax lot intersect the subway buffer? The output is 
# a logical matrix where each row is a tax lot and each col is a buffer
# If the given lot (row) intersects the buffer (col) then that entry is true.
intersections <-  pluto_sf_raw %>% 
  st_intersects(entrances_buffer, sparse = T)

##lengths() computes the length of each element in the intersections list.
# I make it into a tibble where each row is the # of times the PLUTO tax lot in that row
# intersected the amenity data. If it is 0, that means it did not intersect. So I only keep the ones
# where the number of intersections is greater than 0 
selected_tax_lots <- pluto_sf_raw %>% 
  as_tibble() %>% 
  select(bbl) %>% 
  bind_cols(intersections %>% 
              lengths() %>% 
              as_tibble() %>% 
              rename(num_intersections = value)) %>% 
  filter(num_intersections > 0) %>% 
  select(bbl)

# Save data ---------------------------------------------------------------
write_csv(selected_tax_lots, here("data/created/intersections/tax_lots_subway_5_buffer.csv"))



