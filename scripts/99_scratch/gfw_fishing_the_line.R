################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# jc_villasenor@miami.edu
# date
#
# GFW events around LSMPAs
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  rnaturalearth,
  sf,
  tidyverse,
  gfwr
)


key <- Sys.getenv("GFW_TOKEN")

# Load data --------------------------------------------------------------------
coast <- ne_countries()

mpas <- st_read(dsn = here("processed_data/selected_LSMPAs_viz.gpkg"))

theme_set(theme_linedraw(base_size = 8) +
            theme(text = element_text(family = "Helvetica",
                                      color = 'black'),
                  legend.position = "top",
                  legend.background = element_blank(),
                  legend.key.width = unit(15, "pt"),
                  legend.key.height = unit(10, "pt"),
                  legend.box.spacing = element_blank(),
                  legend.title = element_text(hjust = 0.5),
                  legend.title.position = "top",
                  strip.background = element_rect(fill = "transparent"),
                  strip.text = element_text(color = "black")))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
select_mpas <- mpas |> 
  filter(wdpaid %in% c(
    # "555705293",   # Cordillera de Coiba
    "11753",         # Galapagos
    "309888",        # PIPA
    "555629385",     # Revillagigedo
    "555651558",      # Asención
    "555512151"   # Chagos - Can't do Chagos because 1) there is no data before it was implemented an 2) there is no "other sets" data to calculate dFAD set as % of total
  )) |> 
  nngeo::st_remove_holes() |> 
  select(-a) |> 
  mutate(name = case_when(name == "British Indian Ocean Territory Marine Protected Area (Chagos)" ~ "Chagos",
                          name == "Phoenix Islands Protected Area" ~ "PIPA",
                          name == "Ascension Island Marine Protected Area" ~ "Asencsion", 
                          T ~ name))

mpa_buf_200 <- select_mpas |> 
  st_buffer(dist = units::set_units(200, "nautical_miles"))


# Get GFW events

my_get_rast <- function(yr) {
  gfwr::get_raster(spatial_resolution = "HIGH",
                   temporal_resolution = "YEARLY",
                   start_date = paste0(yr,"-01-01"),
                   end_date = paste0(yr,"-12-31"),
                   region_source = "USER_SHAPEFILE", 
                   region = mpa_buf_200,
                   filter_by = "geartype = 'tuna_purse_seines'",
                   key = key)
}

gfw_rast <- c(2012:2024) |> 
  map_dfr(my_get_rast) |> 
  janitor::clean_names() |> 
  rename(year = time_range,
         hours = apparent_fishing_hours)

gfw_sf <- st_as_sf(gfw_rast, coords = c("lon", "lat"), crs = 4326) |> 
  st_join(mpa_buf_200) |> 
  drop_na(name) |> 
  st_filter(st_union(select_mpas), .predicate = st_disjoint) %>%
  mutate(dist = as.numeric(st_distance(., st_union(select_mpas))[,1]) / 1852,
         post = ifelse(year < year_enforced, "Before", "After"),
         post = fct_relevel(post, c("Before", "After")))

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
ggplot(gfw_sf, aes(x = dist, y = hours / 123.21, color = post)) +
  geom_point(pch = ".") +
  # geom_smooth() +
  facet_grid(name~post, scales = "free_y") +
  labs(x = "Distance (nautical miles)",
       y = "Effort (hours / km2)")

ggplot(gfw_sf, aes(x = dist, y = hours / 123.21, color = post)) +
  # geom_point(pch = ".") +
  geom_smooth() +
  facet_grid(name~post, scales = "free_y") +
  labs(x = "Distance (nautical miles)",
       y = "Effort (hours / km2)")

plot_mpa <- function(this_name, data){
  
  mpa <- select_mpas |> 
    filter(name == this_name)
  
  ggplot() +
    geom_point(data = data,
               aes(x = lon, y = lat, color = hours / 123.21),
               pch = ".") +
    scale_color_viridis_c(trans = "log10") +
    geom_sf(data = mpa) +
    facet_wrap(~post) +
    labs(title = this_name,
         color = "hours / (km2 year)")
}


nested <- gfw_sf %>%
  bind_cols(st_coordinates(.)) |> 
  st_drop_geometry() |> 
  rename(lon = X,
         lat = Y) |> 
  group_by(post, lat, lon, name) |> 
  summarize(hours = mean(hours, na.rm = T),
            .groups = "drop") |> 
  group_by(name) |> 
  nest()

a <- nested %$% 
  map2(name, data, plot_mpa)

cowplot::plot_grid(plotlist = a,
                   ncol = 2)


gfw_sf |> 
  st_drop_geometry() |> 
  mutate(dist = floor(dist / 10) * 10 + 5) |> 
  group_by(name, year, post, dist) |> 
  summarize(hours = sum(hours) / 123.21) |> 
  group_by(name, post, dist) |> 
  summarize(hours = mean(hours)) |> 
  ggplot(aes(x = dist, y = hours, color = post)) +
  geom_point() +
  facet_grid(name ~ post, scales = "free_y") +
  labs(x = "Distance (NM)", y = "Effort (hours / (km2 year)")

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
