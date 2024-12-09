# Load libs ---------------------------------------------------------------
library(tidyverse)
library(here)
library(sf)
library(janitor)
library(tidycensus)
options(scipen = 999)
windowsFonts(`Roboto Condensed`=windowsFont("Roboto Condensed"))

# prime
prime <- read_csv(here("data/created/prime-bbls/prime-bbls.csv"))

#PLUTO
pluto <- read_csv(here("data/created/pluto-clean.csv"),
                  col_select =  c(bbl, bldgclass, bldgclass_cat, landuse, borough,
                                  landuse_cat, ownertype, numfloors, lotarea, 
                                  latitude, longitude, builtfar, bldgarea,
                                  unitstotal,unitsres, address, zonedist1, residfar))

# PLUTO sf
pluto_sf_raw <- read_sf(here("data/raw/pluto/nyc_mappluto_24v3_1_shp/MapPLUTO.shp"), 
                        query = 'SELECT bbl from MapPLUTO') %>% 
  clean_names()

# Data for underutilization calcs
source(here("helper-scripts/units-for-underutilization-calcs.R"))

# Join data ---------------------------------------------------------------
df <- prime %>% 
  left_join(pluto) %>% 
  mutate(zone_number = parse_number(zonedist1))


# By landuse class -------------------------------------------------------
landuse_agg <- df %>% 
  filter(!is.na(landuse)) %>% 
  count(landuse, landuse_cat, sort = T) %>% 
  mutate(per = n/sum(n)*100)  

# Vacant ------------------------------------------------------------------
vacant <- df %>% 
  filter(landuse_cat == "Vacant")

# vacant %>%
#   filter(ownertype == "C")  %>%
#   left_join(pluto_sf_raw) %>%
#   st_write("data/created/sfs-for-qgis/underutilized/vacant-city-owned.shp")


#5,269 PRIME properties are vacant, 827 owned by City
vacant %>% 
  count(ownertype) %>% 
  mutate(total = sum(n),
         per = n/total*100)

# By borough
vacant %>% 
  count(borough) %>% 
  mutate(total = sum(n),
         per = n/total*100)

# Assuming typical number of units per square foot of lot is developed, 53,228 units
# Could be upwardly biased because some lots are vacant for good reason or should not be made
# residential
# Could be downwardly biased because I just took average units per lot foot which includes a
# lot of very small MF props, could be overly restrictive. 
# 53,228
(all_lots_vacant <- vacant %>% 
    # Only look at lots> 1815, which is the bottom 10th percentile of lot area for MF
  filter(lotarea > mf_lot_size) %>% 
  select(lotarea) %>% 
    # Units per foot from units-for-underutilization-calcs.R. Number of units per foot in MF
    # buildings
  mutate(units_per_foot = units_per_foot,
         mean_units_build = mean_units_build, 
         units_area = lotarea*units_per_foot) %>% 
  summarise(total_units_area = sum(units_area),
            total_units_mean = sum(mean_units_build)))

all_lots_vacant <- all_lots_vacant %>% 
  select(units = total_units_area) %>% 
  mutate(cat = "units_vacant_all")
  
# City owned vacant land
(city_owned_vacant <- vacant %>% 
  filter(ownertype == "C") %>% 
  filter(lotarea > pluto %>% 
           filter(bldgclass_cat %in% c("Walk Up Apartment", "Elevator Apartment")) %>% 
           pull(lotarea) %>% 
           quantile(probs = .1)) %>% 
  select(lotarea) %>% 
  mutate(units_per_foot = units_per_foot,
         mean_units_build = mean_units_build, 
         units_area = lotarea*units_per_foot) %>% 
  summarise(total_units_area = sum(units_area),
            total_units_mean = sum(mean_units_build)))

city_owned_vacant <- city_owned_vacant %>% 
  select(units = total_units_area) %>% 
  mutate(cat = "units_vacant_city")


vacant_units <- all_lots_vacant %>% 
  bind_rows(city_owned_vacant)

# Parking ----------------------------------------------------------------
# 3,467 parking lots
(parking <- df %>% 
  filter(landuse_cat == "Parking"))

# parking %>%
#   filter(bldgclass == "Z2") %>%
#   left_join(pluto_sf_raw) %>%
#   st_write("data/created/sfs-for-qgis/underutilized/parking-city-owned.shp")
# 
# 
parking %>% 
  count(bldgclass, sort =T) %>% 
  mutate(per = n/sum(n)*100)

# 49,257
parking_units_all <- parking %>% 
  # Only look at lots> 1815, which is the bottom 10th percentile of lot area for MF
  filter(lotarea > mf_lot_size) %>% 
  # Find number of units that could be build using units per foot in MF props
  mutate(units_area = lotarea*units_per_foot) %>% 
  summarise(units = sum(units_area)) %>% 
  mutate(cat = "units_parking_all")

parking_units_city <- parking %>% 
  filter(bldgclass == "Z2") %>% 
  # Only look at lots> 1815, which is the bottom 10th percentile of lot area for MF
  filter(lotarea > mf_lot_size) %>% 
  # Find number of units that could be build using units per foot in MF props
  mutate(units_area = lotarea*units_per_foot) %>%   summarise(units = sum(units_area)) %>% 
  mutate(cat = "units_parking_city")

parking_units <- parking_units_all %>% 
  bind_rows(parking_units_city)

# Churches ----------------------------------------------------------------
# 2,670 churches
(churches <- df %>% 
  filter(bldgclass_cat == "Churches/Religious Institutions"))

# churches %>%
#   left_join(pluto_sf_raw) %>%
#   st_write("data/created/sfs-for-qgis/underutilized/churches.shp")


# Churches could produce 24,572 homes
all_churches <- churches %>% 
  select(bbl, address, lotarea, builtfar, numfloors, bldgarea) %>% 
  # Calculate likely first floor size, subtract that from total lot area
  # that is lot area left over on which to build housing
  mutate(num_full_floors = floor(numfloors),
         partial_floor = numfloors - num_full_floors, 
         bottom_floor = bldgarea/numfloors,
         free_land = lotarea - bottom_floor)  %>% 
  # Only look at lots> 1815, which is the bottom 10th percentile of lot area for MF
  filter(free_land > mf_lot_size) %>% 
  mutate(units_area = free_land*units_per_foot) %>% 
  arrange(desc(units_area)) 
  
# 24,572
church_units <- all_churches %>% 
  summarise(units = sum(units_area)) %>% 
  mutate(cat = "units_churches")

# Commercial --------------------------------------------------------------
(commercial <- df %>% 
  filter(landuse_cat %in% c("Commercial & Office")))

# commercial %>%
#   left_join(pluto_sf_raw) %>%
#   st_write("data/created/sfs-for-qgis/underutilized/one-story-commercial.shp")

commercial %>% 
  count(bldgclass_cat, sort =T) %>% 
  mutate(total = sum(n))

#3,051 1 story commercial buildings
commercial %>% 
  filter(bldgclass_cat %in% c("Store Buildings")) %>% 
  filter(numfloors == 1) 

commercial_units <- commercial %>% 
  filter(bldgclass_cat %in% c("Store Buildings")) %>% 
  filter(numfloors == 1) %>% 
  summarise(units = n()) %>% 
  mutate(units = units*15,
         cat = "units_commercial")

# Zoning of single family -------------------------------------------------
# 105,732
(low_density <- df %>%
  filter(landuse_cat=="One and Two Family"))

low_density %>% 
  count(zonedist1, zone_number, sort = T) %>% 
  filter(str_sub(zonedist1, 1, 1) == "R") %>% 
  group_by(high_den = zone_number >= 5) %>% 
  summarise(builds = sum(n))

# Make buildings match the average units for their zone: 326,160
units_low_den_r5_higher_match_mean <- low_density %>% 
  filter(str_sub(zonedist1, 1, 1) == "R")  %>% 
  filter(zone_number > 4) %>%
  # From units-for-underutilization-calcs.R
  left_join(mean_units_by_zone) %>% 
  # Take the mean units for the zone, subtract actual units, remainder 
  # is what units could be added
  mutate(additional_units = mean_units - unitsres) %>% 
  summarise(units = sum(additional_units)) %>% 
  mutate(cat = "units_low_den_r5_higher_match_mean")

# Upzoning R4 to R5, R6, or R7
units_low_den_r4_lower_upzone <- low_density %>% 
  filter(str_sub(zonedist1, 1, 1) == "R")  %>% 
  filter(zone_number <= 4) %>% 
  # These come from mean_units_by_zone, produced by units-for-underutilization-calcs.R
  mutate(upzone_1 = 3.6463923 - unitsres, 
         upzone_2 = 7.8456319 - unitsres, 
         upzone_3 = 17.85801 - unitsres) %>% 
  summarise(units_low_den_r4_match_r5 = sum(upzone_1),
            units_low_den_r4_match_r6 = sum(upzone_2),
            units_low_den_r4_match_r7 = sum(upzone_3)) %>% 
  pivot_longer(everything(),names_to = "cat", values_to = "units")  

# Upzone all of R5 or R6 --------------------------------------------------
units_upzone_r5_r6 <- df %>%
  filter(landuse_cat %in% c("One and Two Family",
                            "Multi-Family Elevator", 
                            "Mixed Resi/Commercial",
                            "Multifamily Walk-Up")) %>% 
  filter(str_sub(zonedist1, 1, 1) == "R")  %>% 
  filter(zone_number %in% c(5,6)) %>% 
  # This comes from mean_units_by_zone, produced by units-for-underutilization-calcs.R
  mutate(upzone = 17.85801 - unitsres) %>% 
  filter(upzone >= 0) %>% 
  group_by(zone_number) %>% 
  summarise(units = sum(upzone, na.rm = T)) %>% 
  mutate(cat = ifelse(zone_number == 5, "units_r5_match_r7", "units_r6_match_r7")) %>% 
  select(-zone_number)

# Join everything ------------------------------------------------------------
full_units <- vacant_units %>% 
  bind_rows(parking_units) %>% 
  bind_rows(church_units) %>% 
  bind_rows(commercial_units) %>% 
  bind_rows(units_low_den_r5_higher_match_mean) %>% 
  bind_rows(units_low_den_r4_lower_upzone) %>% 
  bind_rows(units_upzone_r5_r6)

full_units <- full_units %>% 
  mutate(label = case_when(cat == "units_vacant_all" ~ "Build on all vacant lots",
                           cat == "units_vacant_city" ~ "Build on city-owned vacant lots",
                           cat == "units_parking_all" ~ "Build on all parking lots",
                           cat == "units_parking_city" ~ "Build on city-owned parking lots",
                           cat == "units_churches" ~ "Build on open faith-based organization land",
                           cat == "units_commercial" ~ "Build on top of single-story retail",
                           cat == "units_low_den_r5_higher_match_mean" ~ "Densify one and two unit properties in higher density zones to match average unit count of zone",
                           cat == "units_low_den_r4_match_r5" ~ "Densify one and two unit properties in R4 zone to match average unit count of R5 zone",
                           cat == "units_low_den_r4_match_r6" ~ "Densify one and two unit properties in R4 zone to match average unit count of R6 zone",
                           cat == "units_low_den_r4_match_r7" ~ "Densify one and two unit properties in R4 zone to match average unit count of R7 zone",
                           cat == "units_r5_match_r7" ~ "Densify properties in R5 zone to match average unit count of R7 zone",
                           cat == "units_r6_match_r7" ~ "Densify properties in R6 zone to match average unit count of R7 zone"))

# Plot --------------------------------------------------------------------
full_units %>% 
  ggplot(aes(fct_reorder(label, units), units)) +
  geom_col(fill = "#00BFFF") +
  geom_text(aes(label = prettyNum(round(units), big.mark = ",")),
            hjust = -.1) +
  scale_x_discrete(labels = function(x){str_wrap(x,25)}) +
  scale_y_continuous(expand = expansion(c(0,.2)),
                     labels = function(y){prettyNum(y, big.mark = ",")}) +
  coord_flip() +
  labs(y = "Units that could be built on PRIME lots",
       x = NULL, 
       title = "Hundreds of thousands of homes could be added to already PRIME locations", 
       subtitle = "Units that could be added to PRIME lots using given method") +
  theme_bw(base_size = 12,
           base_family = "Roboto Condensed") +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(), 
        plot.title.position = "plot",
        plot.title = element_text(size = rel(1.5), 
                                  face = "bold"),
        plot.subtitle = element_text(size = rel(1.2),
                                     face = "italic",
                                     color = "grey30"))

ggsave(here("figures/underutilization-column.png"),
       dpi = 600, height = 10.5, width = 8, units = "in")
