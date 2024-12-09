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
schools_raw <- read_sf(here("data/raw/schools/SchoolPoints_APS_2024_08_28/SchoolPoints_APS_2024_08_28.shp"))
highschool_raw <- read_csv(here("data/raw/schools/2021_DOE_High_School_Directory_20241130.csv"))
middleschool_raw <- read_csv(here("data/raw/schools/2021_DOE_Middle_School_Directory_20241130.csv"))
elementary_raw <- read_csv(here("data/raw/schools/2021_DOE_Kindergarten_Admissions_Guide_20241130.csv")) %>% 
  clean_names()

# Clean data ------------------------------------------------------------
# Clean full school data
school_sf <- schools_raw %>% 
  st_transform(st_crs(pluto_sf_raw)) %>% 
  clean_names() %>% 
  rename(dbn = ats)

# Clean schools
# Highschools
highschool <- highschool_raw %>% 
  mutate(school_type = "high") %>% 
  select(dbn, school_type) %>% 
  left_join(school_sf) %>% 
  st_as_sf()

# Middle Schools
middle <- middleschool_raw %>%
  mutate(school_type = "middle") %>% 
  rename(dbn = schooldbn) %>% 
  select(dbn, school_type) %>% 
  left_join(school_sf) %>% 
  st_as_sf()

# Elementary Schools
elementary <- elementary_raw %>% 
  mutate(school_type = "elementary") %>% 
  select(dbn, school_type) %>% 
  left_join(school_sf) %>% 
  st_as_sf()

# Buffering schools -------------------------------------------------------
schools_buffer <- highschool %>%
  bind_rows(middle) %>% 
  bind_rows(elementary) %>% 
  st_buffer(dist = 5280)

# Find tax lots in subway buffer ------------------------------------------
# Does the tax lot intersect the school buffer? The output is 
# a logical matrix where each row is a tax lot and each col is a buffer
# If the given lot (row) intersects the buffer (col) then that entry is true.
intersections_hs <-  pluto_sf_raw %>% 
  st_intersects(schools_buffer %>% 
                  filter(school_type == "high"), sparse = T)

intersections_ms <-  pluto_sf_raw %>% 
  st_intersects(schools_buffer %>% 
                  filter(school_type == "middle"), sparse = T)

intersections_es <-  pluto_sf_raw %>% 
  st_intersects(schools_buffer %>% 
                  filter(school_type == "elementary"), sparse = T)

##lengths() computes the length of each element in the intersections list.
# I make it into a tibble where each row is the # of times the PLUTO tax lot in that row
# intersected the amenity data. If it is 0, that means it did not intersect. So I only keep the ones
# where the number of intersections is greater than 0 
# Highschool
selected_tax_lots_hs <- pluto_sf_raw %>% 
  as_tibble() %>% 
  select(bbl) %>%  
  bind_cols(intersections_hs %>% 
              lengths() %>% 
              as_tibble() %>% 
              rename(num_intersections = value)) %>% 
  filter(num_intersections > 0) %>% 
  select(bbl)

#Middle school
selected_tax_lots_ms <- pluto_sf_raw %>% 
  as_tibble() %>% 
  select(bbl) %>% 
  bind_cols(intersections_ms %>% 
              lengths() %>% 
              as_tibble() %>% 
              rename(num_intersections = value)) %>% 
  filter(num_intersections > 0) %>% 
  select(bbl)

# Elementary school
selected_tax_lots_es <- pluto_sf_raw %>% 
  as_tibble() %>% 
  select(bbl) %>%
  bind_cols(intersections_es %>% 
              lengths() %>% 
              as_tibble() %>% 
              rename(num_intersections = value)) %>% 
  filter(num_intersections > 0) %>% 
  select(bbl)

# Only keep lots with a highschool, middle school, 
# and elementary school withina mile
tax_lots_with_3_schools <- selected_tax_lots_hs %>% 
  bind_rows(selected_tax_lots_ms) %>% 
  bind_rows(selected_tax_lots_es) %>% 
  count(bbl) %>% 
  filter(n == 3)

# Save data ---------------------------------------------------------------
write_csv(tax_lots_with_3_schools, here("data/created/intersections/tax_lots_schools_1_buffer.csv"))
