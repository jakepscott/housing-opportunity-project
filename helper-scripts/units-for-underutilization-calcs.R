# Data for housing calcs --------------------------------------------------
# Res units per square foot of multifamily buildings in NYC
units_per_foot <- pluto %>% 
  filter(bldgclass_cat %in% c("Walk Up Apartment", "Elevator Apartment")) %>% 
  filter(unitsres > 2) %>% 
  select(unitsres, lotarea) %>% 
  mutate(units_per_foot = (unitsres/lotarea)) %>% 
  summarise(units_per_foot = mean(units_per_foot)) %>% 
  pull(units_per_foot)

# Average num of res units in apartments: 15.6
mean_units_build <- pluto %>% 
  filter(unitsres > 0) %>% 
  filter(bldgclass_cat %in% c("Walk Up Apartment", "Elevator Apartment")) %>% 
  summarise(mean_units = mean(unitsres)) %>% 
  pull(mean_units)

# MF property needs at least 1815 square feet of lot
mf_lot_size <- pluto %>% 
  filter(bldgclass_cat %in% c("Walk Up Apartment", "Elevator Apartment")) %>% 
  pull(lotarea) %>% 
  quantile(prob = .1)# Mean resi units per building by zone

mean_units_by_zone <- pluto %>% 
  filter(str_sub(zonedist1, 1, 1) == "R") %>% 
  group_by(zonedist1) %>% 
  summarise(mean_units = mean(unitsres, na.rm = T)) 