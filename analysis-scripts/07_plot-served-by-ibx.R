# Load libs ---------------------------------------------------------------
library(tidyverse)
library(here)
library(sf)
library(janitor)
library(tidycensus)
windowsFonts(`Roboto Condensed`=windowsFont("Roboto Condensed"))

# Load Data ---------------------------------------------------------------
# Zone 4 category
zone_4_cats <-  read_csv(here("data/created/near-prime-bbls/zone-4-with-cats.csv"))

# Served by IBX
served_by_ibx <-  read_csv(here("data/created/ibx/missing-subway-served-by-ibx.csv"))

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

# Add shapefile
missing_subway_sf <- missing_subway %>% 
  left_join(pluto_sf_raw) %>% 
  st_as_sf()

# Create data for plot ----------------------------------------------------
served_sf <- served_by_ibx %>%
  left_join(pluto_sf_raw) %>% 
  st_as_sf() %>% 
  st_union() %>% 
  st_sf() %>% 
  mutate(category = "Served by IBX")

not_served_sf <- missing_subway_sf %>% 
  filter(!bbl %in% served_by_ibx$bbl) %>% 
  st_union() %>% 
  st_sf() %>% 
  mutate(category = "Not Served by IBX")

joined_served <- served_sf %>% 
  bind_rows(not_served_sf)

st_write(joined_served, here("data/created/sfs-for-qgis/served-by-ibx.shp"))

# # Optional: Plot in R -----------------------------------------------------
# # NYC Counties
# nyc_geo <- get_acs(geography = "county", year = 2022,
#                    variables = "B01003_001", geometry = T, 
#                    state = c("New York"),
#                    county = c("New York", "Kings", "Queens", "Bronx", "Richmond")) %>% 
#   tigris::erase_water()
# 
# # Plot --------------------------------------------------------------------
# joined_served %>% 
#   ggplot() +
#   geom_sf(data = nyc_geo) +
#   geom_sf(data = ibx_buffer, 
#           fill = "grey60", 
#           color = "grey50", 
#           lwd = .5, alpha = .2) +
#   geom_sf(aes(fill = category),
#           color = NA, show.legend = F) +
#   # geom_sf(data = subway_lines, 
#   #         color = "grey70") +
#   geom_sf(data = ibx, 
#           color = "#00BFFF", lwd = 1) +
#   scale_fill_manual(values = c("#ff686b", "#39b5ac")) +
#   theme_void(base_size = 12,
#              base_family = "Roboto Condensed") +
#   theme(legend.position = "bottom", 
#         legend.text = element_text(size = rel(1.35)))
# 
# 
# ggsave(here("figures/served-by-ibx.png"), bg = "white", 
#        dpi = 600, height = 8, width = 8, units = "in")

