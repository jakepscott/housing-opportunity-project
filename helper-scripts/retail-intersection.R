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

# Retail locations
retail_raw <- read_sf(here("data/raw/retail/Retail Food Stores_20241130/geo_export_7809f157-a5cf-4927-aad3-00a1c8f513bc.shp"))

# Clean data ------------------------------------------------------------
# Only keep retail stores >10,000 square feet in NYC
retail <- retail_raw %>% 
  filter(county %in% c('NEW YORK', 'KINGS', 'QUEENS', 'BRONX', 'RICHMOND')) %>% 
  filter(square_foo >= 10000)

# Remove pharmacy and other non-supermarkets
remove <- "pharmacy|walgreen|cvs|7 eleven|dollar tree|duane read|rite aid|family dollar|dollar general|7-eleven|RITE AID"

retail_clean <- retail %>% 
  filter(!str_detect(str_to_lower(dba_name), remove)) %>% 
  filter(!dba_name %in% c("QUEENS HEALTH EMPORIUM", "OH NUTs", 
                          "RAAKA CHOCOLATE", "RALPH AVE DOLLAR", 
                           "NATIONAL WHOLESALE LIQUIDATORS", "FIVE BOROUGHS BREWING CO"))


# Buffering retail -------------------------------------------------------
retail_buffer <- retail_clean %>% 
  st_transform(crs = st_crs(pluto_sf_raw)) %>% 
  st_buffer(dist = 2640)

# Find tax lots in subway buffer ------------------------------------------
# Does the tax lot intersect the retail buffer? The output is 
# a logical matrix where each row is a tax lot and each col is a buffer
# If the given lot (row) intersects the buffer (col) then that entry is true.
intersections <-  pluto_sf_raw %>% 
  st_intersects(retail_buffer, sparse = T)

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
write_csv(selected_tax_lots, here("data/created/intersections/tax_lots_retail_5_buffer.csv"))



