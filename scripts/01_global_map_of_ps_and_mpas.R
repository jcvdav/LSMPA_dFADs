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
  tidyterra,
  tidyverse,
  cowplot
)

#We'll use a Pacific-center (180 W) Mollweide from: https://epsg.io/54009
proj <- "+proj=moll +lon_0=180 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs"
center <- 180

theme_set(theme_linedraw(base_size = 9) +
            theme(axis.title = element_blank(),
                  axis.text = element_blank(),
                  text = element_text(family = "Helvetica",
                                      color = 'black'),
                  legend.position = "top",
                  legend.background = element_blank(),
                  legend.key.width = unit(30, "pt"),
                  legend.key.height = unit(10, "pt"),
                  legend.box.spacing = element_blank(),
                  panel.spacing = element_blank(),,
                  legend.title = element_text(hjust = 0.5)))

# Load data --------------------------------------------------------------------
rfmo_ps_effort <- readRDS(file = here("processed_data", "annual_rfmo_effort_1deg.rds")) %>%
  filter(between(year, 2013, 2021)) %>% 
  select(-c(year, src)) %>% 
  group_by(lat, lon) %>% 
  summarize_all("mean", na.rm = T) %>% 
  filter(sets_dfad > 0)


rfmo_ps_effort_raster <- st_as_sf(x = rfmo_ps_effort,
                                  coords = c("lon", "lat"),
                                  crs = 4326) %>% 
  select(sets_dfad) %>% 
  vect() %>% 
  rasterize(x = .,
            y = rast(ncol = 360, nrow = 180,
                     xmin = -180, xmax = 180,
                     resolution = 1, crs = "EPSG:4326"),
            field = "sets_dfad")

sum(values(rfmo_ps_effort_raster), na.rm = T)

rfmo_ps_intensity_raster <- (rfmo_ps_effort_raster / cellSize(rfmo_ps_effort_raster)) %>%
  project(proj, res = 100000, method = "bilinear")

rfmo_ps_effort_raster_reproj <- rfmo_ps_intensity_raster * (100000 ^ 2)

sum(values(rfmo_ps_effort_raster_reproj), na.rm = T)

coast <- ne_countries(scale = "small") %>% 
  st_break_antimeridian(lon_0 = center) %>% 
  st_transform(proj)
  
mpas <- st_read(here("processed_data", "selected_LSMPAs_viz.gpkg")) %>% 
  st_break_antimeridian(lon_0 = center) %>% 
  st_transform(proj)

buffered_mpas <- mpas %>% 
  st_buffer(dist = units::as_units(200, "nautical_miles")) %>% 
  st_union() %>% 
  st_as_sf() %>% 
  st_make_valid()

## VISUALIZE ###################################################################
# Historical (2018-2022) track data are from Imzilen et al., 2022. Historical set data (1991-2021) from WCPFC, IATTC, and ICCAT.
# X ----------------------------------------------------------------------------
# Mean annual reported dFAD fishing intensity (2013-2021). Data from WCPFC, IATTC, ICCAT, and IOTC
main_map <- ggplot() +
  geom_spatraster(data = log(rfmo_ps_effort_raster_reproj)) +
  scale_fill_viridis_c(option = "mako",
                       name = "Effort [log(num. sets)]",
                       guide = guide_colorbar(frame.colour = "black",
                                              ticks.colour = "black"),
                       na.value = "transparent") +
  theme(legend.title.position = "top") +
  geom_sf(data = mpas,
          fill = scales::muted("red"),
          color = "black",
          linewidth = 0.1) +
  geom_sf(data = buffered_mpas,
          fill = "transparent",
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = coast,
          fill = "transparent",
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = coast,
          fill = "gray90",
          color = "gray90",
          linewidth = 0) +
  scale_x_continuous(expand = c(0, 0.1)) +
  scale_y_continuous(expand = c(0, 0.1))

## EXPORT ######################################################################
ggsave(plot = main_map,
       filename = "global_map_of_dfad_effort_and_mpas.pdf",
       units = "cm",
       width = 9.2,
       height = 6)



#### EXTRAS
dFAD_tracks <- read_csv(here("raw_data", "data-fads20122018-imzilen2022.csv"))

gfw_ps_effort <- readRDS(here("processed_data", "annual_gfw_tuna_ps_effort_1deg.rds")) %>% 
  select(-year) %>% 
  group_by(lat, lon) %>% 
  summarize_all("sum", na.rm = T) %>% 
  filter(n_vessels_p3m1 > 0)

tracks_map <- ggplot() +
  geom_tile(data = dFAD_tracks,
            aes(x = lon, y = lat, fill = log(num))) +
  scale_fill_viridis_c(option = "viridis",
                       name = "Reported dFAD track intensity [log(num. tracks)]",
                       guide = guide_colorbar(frame.colour = "black",
                                              ticks.colour = "black")) +
  theme(legend.title.position = "top") +
  geom_sf(data = mpas,
          fill = scales::muted("red"),
          color = "black",
          linewidth = 0.1) +
  geom_sf(data = buffered_mpas,
          fill = "transparent",
          color = "black",
          linewidth = 0.1) +
  geom_sf(data = coast,
          fill = "transparent",
          color = "black",
          linewidth = 1) +
  geom_sf(data = coast,
          fill = "gray50",
          color = "gray50",
          linewidth = 0.1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

gfw_map <- ggplot() +
  geom_tile(data = gfw_ps_effort,
            aes(x = lon, y = lat, fill = log(fishing_hours_sunrise_p3m1))) +
  scale_fill_viridis_c(option = "inferno",
                       name = "Inferred dFAD fishing effort [log(num. sets)]",
                       guide = guide_colorbar(frame.colour = "black",
                                              ticks.colour = "black")) +
  theme(legend.title.position = "top") +
  geom_sf(data = mpas,
          fill = scales::muted("red"),
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
          fill = "gray50",
          color = "gray50",
          linewidth = 0.1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

tracks_and_gfw_map <- plot_grid(tracks_map, gfw_map,
                                ncol = 1,
                                labels = "AUTO")
ggsave(plot = tracks_and_gfw_map,
       filename = "global_map_of_tracks_gfw_and_mpas.pdf",
       units = "cm",
       width = 12.1,
       height = 16)

