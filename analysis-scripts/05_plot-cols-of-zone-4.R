# Load libs ---------------------------------------------------------------
library(tidyverse)
library(here)
library(sf)
library(janitor)
library(tidycensus)
windowsFonts(`Roboto Condensed`=windowsFont("Roboto Condensed"))

# Load Data ---------------------------------------------------------------
zone_4_cats <-  read_csv(here("data/created/near-prime-bbls/zone-4-with-cats.csv"))

# Column of which missing -------------------------------------------------
zone_4_cats %>% 
  count(category, sort = T) %>% 
  mutate(total = sum(n),
         per = n/total*100) %>% 
  ggplot(aes(fct_reorder(category, n), n)) +
  geom_col(aes(fill = category), 
           show.legend = F) +
  geom_text(aes(label = prettyNum(round(n), big.mark = ",")),
            hjust = -.1) +
  scale_x_discrete(labels = function(x){str_wrap(x,20)}) +
  scale_y_continuous(expand = expansion(c(0,.2)),
                     labels = function(y){prettyNum(y, big.mark = ",")}) +
  scale_fill_manual(values = c("#76A5AF", "#77DD77", "#C39BD3", "#FF9999", "#FFB347")) +
  labs(y = "Number of properties missing one amenity",
       x = NULL,
       title = "There are 276,000 properties that would be PRIME with the addition of just\none amenity", 
       subtitle = "Properties that would be PRIME but for one amenity") +
  coord_flip() +
  theme_bw(base_size = 12,
           base_family = "Roboto Condensed") +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title.position = "plot",
        plot.title = element_text(size = rel(1.5), 
                                  face = "bold"),
        axis.text.y = element_text(size = rel(1.2)),
        plot.subtitle = element_text(size = rel(1.2),
                                     face = "italic",
                                     color = "grey30"))

ggsave(here("figures/amenity-zone-4-column.png"),
       dpi = 600, height = 8, width = 8, units = "in")
