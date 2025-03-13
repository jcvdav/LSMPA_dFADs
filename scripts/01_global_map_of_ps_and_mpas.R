################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Map RFMO dfad effort
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
# We'll summarize it as the mean annual effort ocurring between 2013 (first year of IOTC data) and 2021 (last year common to all)
rfmo_ps_effort <- readRDS(file = here("processed_data", "annual_rfmo_effort_1deg.rds")) |>
  filter(between(year, 2013, 2021),
         sets_dfad >= 1) |> 
  select(-c(year, src)) |> 
  group_by(lat, lon) |> 
  summarize_all("mean", na.rm = T) |> 
  filter(sets_dfad > 0)

# Now build a raster
rfmo_ps_effort_raster <- st_as_sf(x = rfmo_ps_effort,
                                  coords = c("lon", "lat"),
                                  crs = 4326) |> 
  select(sets_dfad) |> 
  vect() |> 
  rasterize(x = _,
            y = rast(ncol = 360, nrow = 180,
                     xmin = -180, xmax = 180,
                     resolution = 1, crs = "EPSG:4326"),
            field = "sets_dfad")

target_resolution <- 100000

rfmo_ps_intensity_raster <- (rfmo_ps_effort_raster / (cellSize(rfmo_ps_effort_raster) / (target_resolution ^ 2)))
rfmo_ps_effort_raster_reproj <- project(rfmo_ps_intensity_raster, proj, res = target_resolution, method = "bilinear")

# Check that values are relatively the same
sum(values(rfmo_ps_effort_raster), na.rm = T)
sum(values(rfmo_ps_effort_raster_reproj), na.rm = T)

coast <- ne_countries(scale = "medium") |> 
  st_break_antimeridian(lon_0 = center) |> 
  st_segmentize(dfMaxLength = 1000) |> 
  st_transform(proj)
  
mpas <- st_read(here("processed_data", "selected_LSMPAs_viz.gpkg")) |> 
  st_break_antimeridian(lon_0 = center) |> 
  st_transform(proj)

buffered_mpas <- mpas |> 
  st_buffer(dist = units::as_units(200, "nautical_miles")) |> 
  st_union() |> 
  st_as_sf() |> 
  st_make_valid()

## VISUALIZE ###################################################################
# Historical (2018-2022) track data are from Imzilen et al., 2022. Historical set data (1991-2021) from WCPFC, IATTC, and ICCAT.
# X ----------------------------------------------------------------------------
# Mean annual reported dFAD fishing intensity (2013-2021). Data from WCPFC, IATTC, ICCAT, and IOTC
main_map <- ggplot() +
  geom_spatraster(data = log10(rfmo_ps_effort_raster_reproj)) +
  scale_fill_viridis_c(option = "viridis",
                       name = expression(Effort (log[10](num.~sets~per~10000~km^2))),
                       guide = guide_colorbar(frame.colour = "black",
                                              ticks.colour = "black"),
                       na.value = "transparent") +
  theme(legend.title.position = "top") +
  theme(panel.grid.major = element_blank()) +
  geom_sf(data = mpas,
          fill = scales::muted("red"),
          color = scales::muted("red"),
          linewidth = 0.1) +
  geom_sf(data = buffered_mpas,
          fill = "transparent",
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = coast,
          fill = "transparent",
          color = "black",
          linewidth = 0.2) +
  geom_sf(data = coast,
          fill = "gray90",
          color = "gray90",
          linewidth = 0.1) +
  scale_x_continuous(expand = c(0, 0.1)) +
  scale_y_continuous(expand = c(0, 0.1))

# main_map

## EXPORT ######################################################################
ggsave(plot = main_map,
       filename = here("results", "figs", "global_map_of_dfad_effort_and_mpas.pdf"),
       units = "cm",
       width = 9.2,
       height = 6)
