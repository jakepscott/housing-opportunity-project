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

# parks
parks_raw <- read_sf(here("data/raw/parks/Parks Properties_20241128/geo_export_1438017b-f1a4-4410-81d8-4712b12db5bf.shp"))

# Clean data ------------------------------------------------------------
# Run to avoid error checking polygon areas: https://github.com/r-spatial/sf/issues/1762
sf_use_s2(FALSE)
#  Only keep parks >1 acre
parks <- parks_raw %>%
  mutate(area = st_area(geometry),
         acres = units::set_units(area, "acre")) %>% 
  filter(as.numeric(acres) >= 1)

# Buffering parks -------------------------------------------------------
parks_buffer <- parks %>% 
  st_transform(crs = st_crs(pluto_sf_raw)) %>% 
  st_buffer(dist = 1320)

# Find tax lots in parks buffer ------------------------------------------
# Does the tax lot intersect the parks buffer? The output is 
# a logical matrix where each row is a tax lot and each col is a buffer
# If the given lot (row) intersects the buffer (col) then that entry is true.
intersections <-  pluto_sf_raw %>% 
  st_intersects(parks_buffer, sparse = T)

# lengths() computes the length of each element in the intersections list.
# I make it into a tibble where each row is the # of times the PLUTO tax lot in that row
# intersects the amenity data. If it is 0, that means it did not intersect. So I only keep the ones
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
write_csv(selected_tax_lots, here("data/created/intersections/tax_lots_parks_25_buffer.csv"))



