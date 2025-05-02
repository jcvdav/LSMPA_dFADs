library(tidyverse)
library(sf)
library(here)

rfmo_data <- readRDS(file = here("processed_data", "annual_rfmo_effort_1deg.rds"))
mpas <- st_read(dsn = here("processed_data/selected_LSMPAs_viz.gpkg")) |> 
  filter(wdpaid == "555512151")

around <- rfmo_data |> 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |> 
  st_filter(st_buffer(mpas, dist = units::as_units(200, "nautical_mile"))) %>%
  st_filter(st_union(mpas), .predicate = st_disjoint) %>%
  bind_cols(st_coordinates(.)) |> 
  st_drop_geometry() |> 
  rename(lon = X, lat = Y) |> 
  group_by(year, lat, lon) |> 
  summarize(sets_dfad = mean(sets_dfad, na.rm = T))

  
ggplot(around) + 
  geom_tile(aes(x = lon, y = lat, fill = log10(sets_dfad))) +
  geom_sf(data = mpas) +
  geom_sf(data = st_buffer(mpas, dist = units::as_units(200, "nautical_mile")),
          fill = "transparent") +
  facet_wrap(~year, ncol = 4) +
  theme_minimal() +
  labs(fill = "# dFAD Sets (IOTC data)",
       title = "Historical dFAD fishing around the British Indian\nOcean Territory Marine Protected Area (Chagos)",
       subtitle = "Implemented in 2010") +
  theme(legend.position = "top")
