# Load libs ---------------------------------------------------------------
library(tidyverse)
library(here)
library(sf)
library(janitor)
library(tidycensus)
windowsFonts(`Roboto Condensed`=windowsFont("Roboto Condensed"))


# Load Data ---------------------------------------------------------------
# Served by IBX
served_by_ibx <-  read_csv(here("data/created/ibx/missing-subway-served-by-ibx.csv"))

# Pluto df
pluto <- read_csv(here("data/created/pluto-clean.csv"),
                  col_select =  c(bbl, bldgclass, bldgclass_cat, landuse, borough,
                                  landuse_cat, ownertype, numfloors, lotarea, 
                                  latitude, longitude, builtfar, bldgarea,
                                  unitstotal,unitsres, address, zonedist1, residfar))
# Data for underutilization calcs
source(here("helper-scripts/units-for-underutilization-calcs.R"))

# Join data ---------------------------------------------------------------
df <- served_by_ibx %>% 
  left_join(pluto) %>% 
  mutate(zone_number = parse_number(zonedist1))

# Number of properties ----------------------------------------------------
# 10,955 
df %>% 
  summarise(props = n(),
            units = sum(unitsres, na.rm = T))

# By landuse class -------------------------------------------------------
# 82%, 8,921 one and two unit properties
(landuse_agg <- df %>% 
  filter(!is.na(landuse)) %>% 
  count(landuse, landuse_cat, sort = T) %>% 
  mutate(per = n/sum(n)*100))

# By building class -------------------------------------------------------
df %>% 
  count(bldgclass_cat, sort = T) %>% 
  mutate(per = n/sum(n)*100)

# Vacant ------------------------------------------------------------------
vacant <- df %>% 
  filter(landuse_cat == "Vacant")

#174 PRIME properties are vacant, 15 owned by City
vacant %>% 
  count(ownertype) %>% 
  mutate(total = sum(n),
         per = n/total*100)

# Assuming typical number of units per square foot of lot is developed, 53,228 units
# Could be upwardly biased because some lots are vacant for good reason of should be made
# non resie
# Could be downwardly biased because I just took average units per lot foot which includes a
# lot of very small MF props, could be overly restrictive
(all_lots_vacant <- vacant %>% 
    filter(lotarea > mf_lot_size) %>% 
    select(lotarea) %>% 
    mutate(units_per_foot = units_per_foot,
           mean_units_build = mean_units_build, 
           units_area = lotarea*units_per_foot) %>% 
    summarise(total_units_area = sum(units_area),
              total_units_mean = sum(mean_units_build)))

vacant_units <- all_lots_vacant %>% 
  select(units = total_units_area) %>% 
  mutate(cat = "units_vacant_all")

# Parking ----------------------------------------------------------------
# 77 parking lots
(parking <- df %>% 
   filter(landuse_cat == "Parking"))

parking %>% 
  count(bldgclass, sort =T) %>% 
  mutate(per = n/sum(n)*100)

# All parking units: 1,933
parking_units <- parking %>%
  # Only look at lots> 1815, which is the bottom 10th percentile of lot area for MF
  filter(lotarea > mf_lot_size) %>% 
  mutate(units_area = lotarea*units_per_foot) %>% 
  summarise(units = sum(units_area)) %>% 
  mutate(cat = "units_parking_all")

# Churches ----------------------------------------------------------------
# 34 churches
(churches <- df %>% 
   filter(bldgclass_cat == "Churches/Religious Institutions"))

# Churches could produce 24,572 homes
all_churches <- churches %>% 
  select(address, lotarea, builtfar, numfloors, bldgarea) %>% 
  #count(numfloors) %>% 
  mutate(num_full_floors = floor(numfloors),
         partial_floor = numfloors - num_full_floors, 
         bottom_floor = bldgarea/numfloors,
         free_land = lotarea - bottom_floor)  %>% 
  filter(free_land > mf_lot_size) %>% 
  mutate(units_area = free_land*units_per_foot) %>% 
  arrange(desc(units_area)) 

# 252
church_units <- all_churches %>% 
  summarise(units = sum(units_area)) %>% 
  mutate(cat = "units_churches")

# Commercial --------------------------------------------------------------
(commercial <- df %>% 
   filter(landuse_cat %in% c("Commercial & Office")))

#80 1 story commercial buildings
commercial %>% 
  filter(bldgclass_cat %in% c("Store Buildings")) %>% 
  filter(numfloors == 1) 

# 1,200 units
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
  left_join(mean_units_by_zone) %>% 
  mutate(additional_units = mean_units - unitsres) %>% 
  summarise(units = sum(additional_units)) %>% 
  mutate(cat = "units_low_den_r5_higher_match_mean")

# Upzoning R4 to R5, R6, or R7
units_low_den_r4_lower_upzone <- low_density %>% 
  filter(str_sub(zonedist1, 1, 1) == "R")  %>% 
  filter(zone_number <= 4) %>% 
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
  filter(str_sub(zonedist1, 1, 1) == "R")  %>% 
  filter(zone_number %in% c(5,6)) %>% 
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
  mutate(label = case_when(cat == "units_vacant_all" ~ "Build on vacant lots",
                           cat == "units_parking_all" ~ "Build on parking lots",
                           cat == "units_churches" ~ "Build on open faith-based organization land",
                           cat == "units_commercial" ~ "Build on top of single-story retail",
                           cat == "units_low_den_r5_higher_match_mean" ~ "Densify one and two unit properties to match average unit count of their zone",
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
  scale_x_discrete(labels = function(x){str_wrap(x,20)}) +
  scale_y_continuous(expand = expansion(c(0,.2)),
                     labels = function(y){prettyNum(y, big.mark = ",")}) +
  labs(y = "Units that could be built on PRIME lots",
       x = NULL, 
       title = "Hundreds of thousands of homes could be added to tax lots that the IBX would\nmake PRIME", 
       subtitle = "Units that could be added using given method") +
  coord_flip() +
  theme_bw(base_size = 12,
           base_family = "Roboto Condensed") +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(), 
        plot.title.position = "plot",
        plot.title = element_text(size = rel(1.5), 
                                  face = "bold"),
        plot.subtitle = element_text(size = rel(1),
                                     face = "italic",
                                     color = "grey30"))

ggsave(here("figures/underutilization-column-ibx.png"),
       dpi = 600, height = 10, width = 8, units = "in")


