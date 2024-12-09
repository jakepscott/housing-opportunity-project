# Load libs ---------------------------------------------------------------
library(tidyverse)
library(here)
library(sf)
library(janitor)

# load data ---------------------------------------------------------------
# PLUTO shapefile
pluto_sf_raw <- read_sf(here("data/raw/pluto/nyc_mappluto_24v3_1_shp/MapPLUTO.shp"), 
                        query = 'SELECT bbl from MapPLUTO') 

# Flood risk
flood_2020 <- read_sf(here("data/raw/flooding/Sea Level Rise Maps (2020s 100-year Floodplain)/geo_export_3e8f58f8-6335-4277-b0be-accfcd78566b.shp"))
flood_2050 <- read_csv(here("data/raw/flooding/2050_1p_258r_shp_selection_final_elim50k_unionFEMA_20241201.csv"))

# Clean flood map ------------------------------------------------------------
flood_2050_sf <- flood_2050 %>% 
  st_as_sf(wkt = "the_geom") %>% 
  st_set_crs(st_crs(flood_2020)) %>% 
  st_transform(st_crs(pluto_sf_raw))

# Make into single polygon
flood_2050_dissolved <- flood_2050_sf %>% 
  st_union() %>% 
  st_as_sf()

# Find tax lots in flood zone ------------------------------------------
# Does the tax lot intersect the flood zone? The output is 
# a logical matrix where each row is a tax lot and each col is a flood zone polygon
# If the given lot (row) intersects the polygon (col) then that entry is true.
intersections <-  pluto_sf_raw %>% 
  st_intersects(flood_2050_sf, sparse = T)

# lengths() computes the length of each element in the intersections list.
# I make it into a tibble where each row is the # of times the PLUTO tax lot in that row
# intersects the flood data. If it is 0, that means it did not intersect. So I only keep the ones
# where the number of intersections is greater than 0 
selected_tax_lots <- pluto_sf_raw %>% 
  as_tibble() %>% 
  select(bbl) %>% 
  bind_cols(intersections %>% 
              lengths() %>% 
              as_tibble() %>% 
              rename(num_intersections = value)) %>% 
  filter(num_intersections == 0) %>% 
  select(bbl)

# Save data ---------------------------------------------------------------
write_csv(selected_tax_lots, here("data/created/intersections/non-flood-zone-tax-lots.csv"))

