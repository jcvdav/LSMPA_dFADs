################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Description
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  tidyverse,
  rnaturalearth,
  janitor,
  sf,
  cowplot,
  fixest
)

# Load data --------------------------------------------------------------------
gal <- st_read(dsn = here("processed_data/selected_LSMPAs_viz.gpkg")) %>% 
  filter(wdpaid == "11753")

gal_outline <- nngeo::st_remove_holes(gal)

iattc <- read_csv(here("raw_data/rfmo_data/IATTC/PublicPSTuna/PublicPSTunaSetType.csv")) %>% 
  clean_names()

coast <- ne_countries()

# Set defaults -----------------------------------------------------------------
theme_set(theme_minimal(base_size = 8) +
            theme(axis.title = element_blank(),
                  legend.position = "bottom"))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
gal_buf_200 <- gal %>% 
  st_buffer(dist = units::as_units(200, "nautical_mile")) %>% 
  mutate(dist_200 = 1) %>%
  select(dist_200)
gal_buf_100 <- gal %>% 
  st_buffer(dist = units::as_units(100, "nautical_mile")) %>% 
  mutate(dist_100 = 1) %>% 
  select(dist_100)

gal_pre_post <- iattc %>% 
  filter(between(year, 1992, 2012)) %>% 
  mutate(post = ifelse(year >= 2002, "After", "Before"),
         post = fct_relevel(post, "Before", "After")) %>% 
  st_as_sf(coords = c("lon_c1", "lat_c1"),
           crs = "EPSG:4326") %>% 
  st_join(gal_buf_200) %>% 
  st_join(gal_buf_100) %>% 
  st_filter(gal_buf_200) %>%
  st_filter(st_union(gal_outline), .predicate = st_disjoint) %>% 
  bind_cols(st_coordinates(.)) %>% 
  st_drop_geometry() %>% 
  group_by(X, Y, post, dist_100, dist_200) %>% 
  mutate(all_sets = sum(num_sets)) %>% 
  ungroup() %>% 
  filter(set_type == "OBJ") %>% 
  group_by(post, X, Y, all_sets, dist_100, dist_200) %>% 
  summarize(num_sets = sum(num_sets),
            .groups = "drop") %>% 
  mutate(fad_pct_of_total = num_sets / all_sets,
         dist_buff = ifelse(is.na(dist_100) & dist_200 == 1, "far", "near"))

feols(fad_pct_of_total ~ dist_buff*post, data = gal_pre_post)

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
gal1 <- ggplot() +
  geom_tile(data = gal_pre_post,
            aes(x = X, y = Y, fill = fad_pct_of_total)) +
  geom_sf(data = gal) +
  geom_sf(data = gal_buf_200, fill = "transparent") +
  geom_sf(data = gal_buf_100, fill = "transparent") +
  facet_wrap(~post) +
  scale_fill_viridis_c(option = "mako") +
  labs(x = "",
       y = "",
       title = "FAD sets (% of total) Before (1992-2001) vs After (2002-2012)")

gal2 <- gal_pre_post %>% 
  select(post, X, Y, fad_pct_of_total) %>% 
  pivot_wider(names_from = post,
              values_from = fad_pct_of_total) %>% 
  mutate(delta = After - Before) %>% 
  ggplot() +
  geom_tile(aes(x = X, y = Y, fill = delta)) +
  geom_sf(data = gal) +
  geom_sf(data = gal_buf_200, fill = "transparent") +
  geom_sf(data = gal_buf_100, fill = "transparent") +
  scale_fill_gradient2(name = "Change") +
  labs(x = "",
       y = "",
       title = "Change in FAD sets (% After - % Before)")

plot_grid(gal1, gal2, ncol = 1)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------