# Load libs ---------------------------------------------------------------
library(tidyverse)
library(here)
library(sf)
library(janitor)
library(tidycensus)
library(basemaps)

# Load Data ---------------------------------------------------------------
# prime
prime <- read_csv(here("data/created/prime-bbls/prime-bbls.csv"))

# PLUTO sf
pluto_sf_raw <- read_sf(here("data/raw/pluto/nyc_mappluto_24v3_1_shp/MapPLUTO.shp"), 
                        query = 'SELECT bbl from MapPLUTO') %>% 
  clean_names()

# Pluto df
pluto <- read_csv(here("data/created/pluto-clean.csv"),
                  col_select =  c(bbl, landuse))

# Create shapefile ---------------------------------------------------------------------
prime_sf <- prime %>% 
  left_join(pluto_sf_raw) %>% 
  st_as_sf()

st_write(prime_sf, here("data/created/sfs-for-qgis/prime-lots-sf.shp"))

# # OPTIONAL: Run to create map in R ----------------------------------------
# prime_sf_union <- prime_sf %>% 
#   st_union() %>% 
#   st_sf() 
# 
# 
# prime_sf_union %>% 
#   ggplot() +
#   geom_sf(data = nyc_geo) +
#     geom_sf(fill = "#39b5ac",
#           color = NA) +
#   theme_void()
# 
# ggsave(here("figures/prime-tax-lots.png"), bg = "white", 
#        dpi = 600, height = 6, width = 6, units = "in")

