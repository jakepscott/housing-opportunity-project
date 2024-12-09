# Load libs ---------------------------------------------------------------
library(tidyverse)
library(here)
library(sf)
library(janitor)
library(tidycensus)
windowsFonts(`Roboto Condensed`=windowsFont("Roboto Condensed"))

# Load Data ---------------------------------------------------------------
# PRIMO
num_amens_bbl <- read_csv(here("data/created/near-prime-bbls/amenity-zone-cats.csv"))

# PLUTO sf
pluto_sf_raw <- read_sf(here("data/raw/pluto/nyc_mappluto_24v3_1_shp/MapPLUTO.shp"), 
                        query = 'SELECT bbl from MapPLUTO') %>% 
  clean_names()

# PLUTO
pluto_df_raw <- read_csv(here('data/raw/pluto/pluto_24v3_1.csv'), 
                         col_select = c("bbl", "bldgclass"))


# Remove parks ------------------------------------------------------------
num_amens_lot_no_park <- num_amens_bbl %>% 
  left_join(pluto_df_raw %>% 
              select(bbl, bldgclass) %>%  
              filter(bldgclass == "Q1") %>% 
              mutate(park = T)) %>% 
  filter(is.na(park)) 
  

# Make shapefile
num_amens_sf <- pluto_sf_raw %>% 
  left_join(num_amens_lot_no_park) %>% 
  filter(!is.na(num_amens))

# Unify amenity zones --------------------------------------------------------------------
union_5 <- num_amens_sf %>% 
  filter(num_amens == 5) %>% 
  st_union() %>% 
  st_sf() %>% 
  mutate(num_amens = 5)

union_4 <- num_amens_sf %>% 
  filter(num_amens == 4) %>% 
  st_union() %>% 
  st_sf() %>% 
  mutate(num_amens = 4)

union_3 <- num_amens_sf %>% 
  filter(num_amens == 3) %>% 
  st_union() %>% 
  st_sf() %>% 
  mutate(num_amens = 3)

union_2 <- num_amens_sf %>% 
  filter(num_amens == 2) %>% 
  st_union() %>% 
  st_sf() %>% 
  mutate(num_amens = 2)

union_1 <- num_amens_sf %>% 
  filter(num_amens == 1) %>% 
  st_union() %>% 
  st_sf() %>% 
  mutate(num_amens = 1)

union_0 <- num_amens_sf %>% 
  filter(num_amens == 0) %>% 
  st_union() %>% 
  st_sf() %>% 
  mutate(num_amens = 0)

unions <- union_5 %>% 
  bind_rows(union_4) %>% 
  bind_rows(union_3) %>% 
  bind_rows(union_2) %>% 
  bind_rows(union_1) %>% 
  bind_rows(union_0) %>% 
  mutate(num_amens_cat = factor(num_amens,
                                levels = 0:5, 
                                labels = c("None", "One", "Two", "Three", 
                                           "Four", "Five")))

st_write(unions, here("data/created/sfs-for-qgis/num-amens-by-lot.shp"))


# OPTIONAL: Map in R ------------------------------------------------------
# # NYC Counties
# nyc_geo <- get_acs(geography = "county", year = 2022,
#                    variables = "B01003_001", geometry = T, 
#                    state = c("New York"),
#                    county = c("New York", "Kings", "Queens", "Bronx", "Richmond")) %>% 
#   tigris::erase_water() 
# 
# # Plot all amenity zones --------------------------------------------------
# unions %>% 
#   ggplot() +
#   geom_sf(data = nyc_geo) +
#   geom_sf(aes(fill = num_amens_cat),
#           color = NA) +
#   # geom_sf(data = subway_lines, 
#   #         color = "black") +
#   scale_color_identity() +
#   scale_fill_manual(values = c("black","#ff686b","#ffa69e","#e9c46a","#99D8A7", "#39b5ac")) +
#   guides(fill=guide_legend(nrow=2,byrow=TRUE))+
#   labs(fill = NULL) +
#   theme_void(base_size = 12,
#              base_family = "Roboto Condensed") +
#   theme(legend.position = "bottom", 
#         legend.text = element_text(size = rel(1.35)))
# 
# 
# ggsave(here("figures/amenity-zones.png"), bg = "white", 
#        dpi = 600, height = 8, width = 8, units = "in")

# # Faceted -----------------------------------------------------------------
# unions %>% 
#   ggplot() +
#   geom_sf(data = nyc_geo) +
#   geom_sf(aes(fill = num_amens_cat),
#           color = NA, 
#           show.legend = F) +
#   scale_fill_manual(values = c("black","#ff686b","#ffa69e","#e9c46a","#84dcc6", "#39b5ac")) +
#   facet_wrap(~num_amens_cat) +
#   theme_void()
# 
# ggsave(here("figures/amenity-zones-faceted.png"), bg = "white", 
#        dpi = 600, height = 10, width = 10, units = "in")

