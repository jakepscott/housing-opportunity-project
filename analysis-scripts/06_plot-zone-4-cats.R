# Load libs ---------------------------------------------------------------
library(tidyverse)
library(here)
library(sf)
library(janitor)
library(tidycensus)
windowsFonts(`Roboto Condensed`=windowsFont("Roboto Condensed"))

# Load Data ---------------------------------------------------------------
# Zone 5 categories
zone_4_cats <-  read_csv(here("data/created/near-prime-bbls/zone-4-with-cats.csv"))


# Pluto shapefile
pluto_sf_raw <- read_sf(here("data/raw/pluto/nyc_mappluto_24v3_1_shp/MapPLUTO.shp"), 
                        query = 'SELECT bbl from MapPLUTO') %>% 
  clean_names()

# Join data ---------------------------------------------------------------
zone_4_cats_geo <- zone_4_cats %>% 
  left_join(pluto_sf_raw) %>% 
  st_as_sf()

# Categories of missing ---------------------------------------------------
missing_subway <- zone_4_cats_geo %>% 
  filter(category == "Missing Subway") %>% 
  st_union() %>% 
  st_sf() %>% 
  mutate(category = "Missing Subway")

missing_market <- zone_4_cats_geo %>% 
  filter(category == "Missing Supermarket") %>% 
  st_union() %>% 
  st_sf() %>% 
  mutate(category = "Missing Supermarket")

missing_park <- zone_4_cats_geo %>% 
  filter(category == "Missing Park") %>% 
  st_union() %>% 
  st_sf() %>% 
  mutate(category = "Missing Park")

missing_school <- zone_4_cats_geo %>% 
  filter(category == "Missing Public School") %>% 
  st_union() %>% 
  st_sf() %>% 
  mutate(category = "Missing Public School")

in_flood <- zone_4_cats_geo %>% 
  filter(category == "In Floodplain") %>% 
  st_union() %>% 
  st_sf() %>% 
  mutate(category = "In Floodplain")

# Join categories of missing ----------------------------------------------
unions <- missing_subway %>% 
  bind_rows(missing_subway) %>% 
  bind_rows(missing_market) %>% 
  bind_rows(missing_park) %>% 
  bind_rows(missing_school) %>% 
  bind_rows(in_flood)

st_write(unions, here("data/created/sfs-for-qgis/all-but-one-amen.shp"))

# OPTIONAL: Plot in R -----------------------------------------------------
# # NYC Counties
# nyc_geo <- get_acs(geography = "county", year = 2022,
#                    variables = "B01003_001", geometry = T, 
#                    state = c("New York"),
#                    county = c("New York", "Kings", "Queens", "Bronx", "Richmond")) %>% 
#   tigris::erase_water() 
# 
# # Plot -------------------------------------------------------------------
# unions %>% 
#   ggplot() +
#   geom_sf(data = nyc_geo) +
#   geom_sf(aes(fill = category),
#           color = NA) +
#   scale_fill_manual(values = c("#76A5AF", "#77DD77", "#C39BD3", "#FF9999", "#FFB347")) +
#   guides(fill=guide_legend(nrow=2,byrow=TRUE))+
#   labs(fill = NULL) +
#   theme_void(base_size = 12,
#              base_family = "Roboto Condensed") +
#   theme(legend.position = "top", 
#         legend.text = element_text(size = rel(1.35)))
# 
# ggsave(here("figures/zone-4-cats.png"), bg = "white", 
#        dpi = 600, height = 8, width = 8, units = "in")
# 
# 
# unions %>% 
#   ggplot() +
#   geom_sf(data = nyc_geo) +
#   geom_sf(aes(fill = category),
#           color = NA, show.legend = F) +
#   scale_fill_manual(values = c("#76A5AF", "#77DD77", "#C39BD3", "#FF9999", "#FFB347")) +
#   facet_wrap(~ category) +
#   theme_void()
# 
# ggsave(here("figures/zone-4-cats-facet.png"), bg = "white", 
#        dpi = 600, height = 10, width = 10, units = "in")



