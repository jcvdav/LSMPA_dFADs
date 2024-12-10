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
coast <- ne_countries()

pipa <- st_read(here("processed_data/selected_LSMPAs_viz.gpkg")) %>% 
  filter(wdpaid == "555512002")

wcpfc <- readRDS(file = "annual_wcpfc_effort_1deg.rds")

# Set defaults -----------------------------------------------------------------
theme_set(theme_minimal(base_size = 8) +
            theme(axis.title = element_blank(),
                  legend.position = "bottom"))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
pipa_buf_200 <- pipa %>% 
  st_buffer(dist = units::as_units(200, "nautical_mile")) %>% 
  mutate(dist_200 = 1) %>%
  select(dist_200)
pipa_buf_100 <- pipa %>% 
  st_buffer(dist = units::as_units(100, "nautical_mile")) %>% 
  mutate(dist_100 = 1) %>% 
  select(dist_100)

# X ----------------------------------------------------------------------------
pipa_pre_post <- wcpfc %>% 
  filter(between(year, 2005, 2022)) %>% 
  mutate(post = ifelse(year >= 2015, "After", "Before"),
         post = fct_relevel(post, "Before", "After")) %>% 
  st_as_sf(coords = c("lon", "lat"),
           crs = "EPSG:4326") %>% 
  st_join(pipa_buf_200) %>% 
  st_join(pipa_buf_100) %>% 
  st_filter(pipa_buf_200) %>%
  st_filter(st_union(pipa), .predicate = st_disjoint) %>% 
  bind_cols(st_coordinates(.)) %>% 
  st_drop_geometry() %>% 
  group_by(post, X, Y, dist_100, dist_200) %>% 
  summarize_all(sum) %>% 
  ungroup() %>% 
  mutate(fad_pct_of_total = sets_dfad / sets_tot,
         dist_buff = ifelse(is.na(dist_100) & dist_200 == 1, "far", "near"))

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
pipa1 <- ggplot() +
  geom_tile(data = pipa_pre_post,
            aes(x = X, y = Y, fill = fad_pct_of_total)) +
  geom_sf(data = pipa) +
  geom_sf(data = pipa_buf_200, fill = "transparent") +
  geom_sf(data = pipa_buf_100, fill = "transparent") +
  facet_wrap(~post) +
  scale_fill_viridis_c(option = "mako") +
  labs(x = "",
       y = "",
       title = "FAD sets (% of total) Before (2005 - 20014) vs After (2012-2022)")

pipa2 <- pipa_pre_post %>% 
  select(post, X, Y, fad_pct_of_total) %>% 
  pivot_wider(names_from = post,
              values_from = fad_pct_of_total) %>% 
  mutate(delta = After - Before) %>% 
  ggplot() +
  geom_tile(aes(x = X, y = Y, fill = delta)) +
  geom_sf(data = pipa) +
  geom_sf(data = pipa_buf_200, fill = "transparent") +
  geom_sf(data = pipa_buf_100, fill = "transparent") +
  scale_fill_gradient2(name = "Change") +
  labs(x = "",
       y = "",
       title = "Change in FAD sets (% After - % Before)")

plot_grid(pipa1, pipa2, ncol = 1)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
