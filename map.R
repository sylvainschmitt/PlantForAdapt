library(rnaturalearth)
library(tidyverse)
library(sf)
library(ggspatial)
br <- list(
  "MG - Fazenda Sao Nicolau (Mato Grosso)" = tibble(lat = -9.855691350801436, 
                                      lon = -58.24784096252843),
  "PA - Belterra (Para)" = tibble(lat = -2.663636, 
                          lon = -54.929739),
  "MA - Mata Atlântica sites" = tibble(lat = -11.807296, 
                                         lon = -37.755278),
  "MA - Mata Atlântica sites" = tibble(lat = -22.710228, 
                                             lon = -48.162938)
) %>% bind_rows(.id = "Site") %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) 
fg <- tibble(
  Site = "FG - French Guiana sites",
  x = c(350620.95269, 355363.53402, 168169.37966,
         213649.32011, 330035.98548, 331298.07581),
  y = c(530957.9664, 514012.23106, 606905.4673,
         598499.05919, 546583.00355, 535346.50928),
  loc = c("egyptienne", "kaw", "malgache", 
          "montagne_fer", "macouria", "tonnegrande")
) %>% 
    st_as_sf(coords = c("x", "y"), crs = 32622) %>% 
    st_transform(crs = 4326)
sites <- bind_rows(br, fg)
sa <- ne_countries(scale = 'medium', type = 'map_units', returnclass = 'sf') %>% 
  st_crop(sites %>% st_buffer(10^6))
fg_zoom <- ne_countries(scale = 'medium', type = 'map_units', returnclass = 'sf') %>% 
  st_crop(xmin = -54.6028, xmax = -51.6346, ymax = 5.7507, ymin = 2.1122)
g_sub <- ggplot() + 
    geom_sf(data = fg_zoom, fill = NA, col = "darkgrey") +
    geom_sf(data = fg, size = 2, colour="black", pch=21, fill = "red") +
    theme_bw() +
    annotation_scale(location = "br") +
  theme_void()
g <- ggplot() + 
  geom_sf(data = sa, fill = NA, col = "darkgrey") +
  geom_sf(data = sites, size = 3, colour="black",pch=21, aes(fill = Site)) +
  theme_bw() +
  scale_fill_discrete("") +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 4, byrow = TRUE)) +
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tl",  
                         style = north_arrow_nautical()) +
  annotation_custom(ggplotGrob(g_sub),
                    xmin = -50, xmax = -30, 
                    ymin = 0, ymax = 17)
ggsave("fig.png", g, width = 4, height = 6)
