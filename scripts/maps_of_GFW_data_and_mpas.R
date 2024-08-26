################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Extract tuna PS effort layer at 1x1 degree from GFW and plot globally against large-scale MPAs
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  rnaturalearth,
  sf,
  terra,
  tidyverse,
  cowplot
)

theme_set(theme_minimal(base_size = 8) +
            theme(axis.title = element_blank(),
                  legend.position = "bottom"))

# Load data --------------------------------------------------------------------
ps_effort <- readRDS(here("processed_data", "tuna_ps_effort_1deg.rds"))

coast <- ne_countries()
mpas <- st_read(here("processed_data", "selected_LSMPAs_viz.gpkg"))

buffered_mpas <- mpas %>% 
  st_buffer(dist = units::as_units(200, "nautical_miles")) %>% 
  st_union() %>% 
  st_as_sf() %>% 
  st_make_valid()

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
total <- ggplot() +
  geom_tile(data = ps_effort,
            aes(x = lon, y = lat, fill = total_fishing_hours)) +
  geom_sf(data = mpas,
          fill = "darkred",
          color = "black",
          linewidth = 0.1) +
  geom_sf(data = buffered_mpas,
          fill = "transparent",
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = coast,
          fill = "transparent",
          color = "black",
          linewidth = 1) +
  geom_sf(data = coast,
          fill = "gray",
          color = "gray",
          linewidth = 0.1) +
  scale_fill_viridis_c(option = "mako",
                       trans = "log10") +
  labs(title = "Total Purse Seine Fishing Activity (2015-2023)",
       fill = "Fishing activity\n[log-10(hours)]")

sunrise_pm_1 <- ggplot() +
  geom_tile(data = ps_effort,
            aes(x = lon, y = lat, fill = fishing_hours_sunrise_pm1)) +
  geom_sf(data = mpas,
          fill = "darkred",
          color = "black",
          linewidth = 0.1) +
  geom_sf(data = coast,
          fill = "transparent",
          color = "black",
          linewidth = 1) +
  geom_sf(data = coast,
          fill = "gray",
          color = "gray",
          linewidth = 0.1) +
  scale_fill_viridis_c(option = "mako",
                       trans = "log10") +
  labs(title = "Total Purse Seine Fishing Activity ± 1 hr of Sunrise (2015-2023)",
       fill = "Fishing activity\n[log-10(hours)]")

sunrise_pm_2 <- ggplot() +
  geom_tile(data = ps_effort,
            aes(x = lon, y = lat, fill = fishing_hours_sunrise_pm2)) +
  geom_sf(data = mpas,
          fill = "darkred",
          color = "black",
          linewidth = 0.1) +
  geom_sf(data = coast,
          fill = "transparent",
          color = "black",
          linewidth = 1) +
  geom_sf(data = coast,
          fill = "gray",
          color = "gray",
          linewidth = 0.1) +
  scale_fill_viridis_c(option = "mako",
                       trans = "log10") +
  labs(title = "Total Purse Seine Fishing Activity ± 2 hr of Sunrise (2015-2023)",
       fill = "Fishing activity\n[log-10(hours)]")

sunrise_pm_3 <- ggplot() +
  geom_tile(data = ps_effort,
            aes(x = lon, y = lat, fill = fishing_hours_sunrise_pm3)) +
  geom_sf(data = mpas,
          fill = "darkred",
          color = "black",
          linewidth = 0.1) +
  geom_sf(data = coast,
          fill = "transparent",
          color = "black",
          linewidth = 1) +
  geom_sf(data = coast,
          fill = "gray",
          color = "gray",
          linewidth = 0.1) +
  scale_fill_viridis_c(option = "mako",
                       trans = "log10") +
  labs(title = "Total Purse Seine Fishing Activity ± 3 hr of Sunrise (2015-2023)",
       fill = "Fishing activity\n[log-10(hours)]")

plot_grid(total,
          sunrise_pm_1,
          sunrise_pm_2,
          sunrise_pm_3)

# LOOK AT THE DISCREPANCIES
ps_effort %>%
  select(contains("hours")) %>% 
  plot(pch = ".", asp = 1)

ps_effort %>% 
  select(contains("hours")) %>% 
  summarize_all(sum) %>% 
  mutate(pct_pm1 = (fishing_hours_sunrise_pm1 / total_fishing_hours) * 100,
         pct_pm2 = (fishing_hours_sunrise_pm2 / total_fishing_hours) * 100,
         pct_pm3 = (fishing_hours_sunrise_pm3 / total_fishing_hours) * 100) %>% 
  select(total_fishing_hours, contains("pct"))

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------

by_buffer <- function(buffer) {
  ps_effort %>% 
    st_as_sf(coords = c("lon", "lat"),
             crs = "EPSG:4326") %>% 
    st_transform(crs = "EPSG:8858") %>% 
    st_filter(buffer) %>% 
    st_drop_geometry() %>% 
    summarize_all(sum) %>% 
    mutate(pct_pm1 = (fishing_hours_sunrise_pm1 / total_fishing_hours),
           pct_pm2 = (fishing_hours_sunrise_pm2 / total_fishing_hours),
           pct_pm3 = (fishing_hours_sunrise_pm3 / total_fishing_hours)) %>% 
    select(total_fishing_hours, contains("pct"))
}

mpas <- st_read(here("processed_data", "selected_LSMPAs_viz.gpkg")) %>% 
  st_transform(crs = "EPSG:8858")

mpa_10 <- mpas %>% 
  st_buffer(dist = units::as_units(10, "nautical_miles")) %>% 
  st_union() %>% 
  st_as_sf()

mpa_50 <- mpas %>% 
  st_buffer(dist = units::as_units(50, "nautical_miles")) %>% 
  st_union() %>% 
  st_as_sf() %>% 
  st_make_valid()

mpa_100 <- mpas %>% 
  st_buffer(dist = units::as_units(100, "nautical_miles")) %>% 
  st_union() %>% 
  st_as_sf() %>% 
  st_make_valid()

mpa_150 <- mpas %>% 
  st_buffer(dist = units::as_units(150, "nautical_miles")) %>% 
  st_union() %>% 
  st_as_sf() %>% 
  st_make_valid()

mpa_200 <- mpas %>% 
  st_buffer(dist = units::as_units(200, "nautical_miles")) %>% 
  st_union() %>% 
  st_as_sf() %>% 
  st_make_valid()

mpa_250 <- mpas %>% 
  st_buffer(dist = units::as_units(250, "nautical_miles")) %>% 
  st_union() %>% 
  st_as_sf()

mpa_300 <- mpas %>% 
  st_buffer(dist = units::as_units(300, "nautical_miles")) %>% 
  st_union() %>% 
  st_as_sf() %>% 
  st_make_valid()

mpa_350 <- mpas %>% 
  st_buffer(dist = units::as_units(350, "nautical_miles")) %>% 
  st_union() %>% 
  st_as_sf() %>% 
  st_make_valid()

mpa_400 <- mpas %>% 
  st_buffer(dist = units::as_units(400, "nautical_miles")) %>% 
  st_union() %>% 
  st_as_sf() %>% 
  st_make_valid()

mpa_450 <- mpas %>% 
  st_buffer(dist = units::as_units(450, "nautical_miles")) %>% 
  st_union() %>% 
  st_as_sf() %>% 
  st_make_valid()

mpa_500 <- mpas %>% 
  st_buffer(dist = units::as_units(500, "nautical_miles")) %>% 
  st_union() %>% 
  st_as_sf() %>% 
  st_make_valid()

list(mpa_50 = mpa_50,
     mpa_100 = mpa_100,
     mpa_150 = mpa_150,
     mpa_200 = mpa_200,
     mpa_250 = mpa_250,
     mpa_300 = mpa_300,
     mpa_350 = mpa_350,
     mpa_400 = mpa_400,
     mpa_450 = mpa_450,
     mpa_500 = mpa_500) %>%
  map_dfr(by_buffer, .id = "ring") %>% 
  mutate(ring = as.numeric(str_extract(ring, "[:digit:]+"))) %>% 
  ggplot(aes(x = ring, y = pct_pm1)) +
  geom_smooth(method = "lm") +
  geom_point(aes(size = log10(total_fishing_hours))) +
  labs(x = "Distance from MPA border (NM)",
       y = "Fisihng activity ± 1 hr of sunrise (% of total)") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal(base_size = 7) +
  theme(legend.position = "bottom")

effort_near_mpas <- ps_effort %>% 
  st_as_sf(coords = c("lon", "lat"),
           crs = "EPSG:4326") %>% 
  st_filter(buffered_mpas)

effort_near_mpas %>% 
  st_drop_geometry() %>% 
  summarize_all(sum) %>% 
  mutate(pct_pm1 = (fishing_hours_sunrise_pm1 / total_fishing_hours) * 100,
         pct_pm2 = (fishing_hours_sunrise_pm2 / total_fishing_hours) * 100,
         pct_pm3 = (fishing_hours_sunrise_pm3 / total_fishing_hours) * 100) %>% 
  select(total_fishing_hours, contains("pct"))

list(#mpa_10 = mpa_10,
  mpa_50 = mpa_50,
  mpa_100 = mpa_100,
  mpa_150 = mpa_150,
  mpa_200 = mpa_200,
  mpa_250 = mpa_250,
  mpa_300 = mpa_300,
  mpa_350 = mpa_350,
  mpa_400 = mpa_400,
  mpa_450 = mpa_450,
  mpa_500 = mpa_500) %>% 
  map_dfr(tibble,
          .id = "ring") %>% 
  mutate(ring = as.numeric(str_extract(ring, "[:digit:]+"))) %>% 
  st_as_sf() %>% 
  ggplot(aes(color = ring)) +
  geom_sf(fill = "transparent",
          linewidth = 1)
