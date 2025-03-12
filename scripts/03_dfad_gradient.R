################################################################################
# MPA dFAD effort proximity analysis
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Two main objectives:
# 1) Looking at gradient of effort moving away from boundary in a BACI set-up
# 2) What % of total FAD effort ocurrs within X miles of an MPA (and how does it
# compare to the amount of total water):
# For example, 90% of dFAD effort ocurring worlwide happens within 200 NM of
# an LSMPA, despite the fact that only 10% of purse seine fishing grounds are 
# lcoated within 200 NM of an LSMPA.
# 
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  fixest,
  rnaturalearth,
  nngeo,
  sf,
  tidyverse
)

theme_set(theme_linedraw(base_size = 8) +
            theme(text = element_text(family = "Helvetica",
                                      color = 'black'),
                  legend.position = "top",
                  legend.title.position = "top",
                  legend.box.spacing = element_blank(),
                  legend.title = element_text(hjust = 0.5)))

# Load data --------------------------------------------------------------------
annual_pre_post <- readRDS(file = here("processed_data/annual_pre_post_activity_by_select_mpa.rds"))
mpas <- st_read(here("processed_data", "selected_LSMPAs_viz.gpkg"))

## PROCESSING ##################################################################
select_mpas <- mpas %>% 
  filter(wdpaid %in% c(
    # "555705293",   # Cordillera de Coiba
    "11753",         # Galapagos
    "309888",        # PIPA
    "555629385",     # Revillagigedo
    "555651558"      # Asención
    # "555512151"   # Chagos - Can't do Chagos because 1) there is no data before it was implemented an 2) there is no "other sets" data to calculate dFAD set as % of total
    # "555624169",   # Nazca
    # "555622118"    # Palau
  )) %>% 
  nngeo::st_remove_holes()


# Build donuts -----------------------------------------------------------------
sf_use_s2(T)

st_erase = function(x, y) st_difference(x, st_union(st_combine(y)))

mpa_50 <- select_mpas %>% 
  st_buffer(dist = units::as_units(50, "nautical_miles")) %>% 
  st_make_valid() %>% 
  st_erase(select_mpas)

mpa_100 <- select_mpas %>% 
  st_buffer(dist = units::as_units(100, "nautical_miles")) %>% 
  st_make_valid() %>%
  st_erase(st_buffer(select_mpas, dist = units::as_units(50, "nautical_miles")))

mpa_150 <- select_mpas %>% 
  st_buffer(dist = units::as_units(150, "nautical_miles")) %>% 
  st_make_valid() %>% 
  st_erase(st_buffer(select_mpas, dist = units::as_units(100, "nautical_miles")))

mpa_200 <- select_mpas %>% 
  st_buffer(dist = units::as_units(200, "nautical_miles")) %>% 
  st_make_valid() %>% 
  st_erase(st_buffer(select_mpas, dist = units::as_units(150, "nautical_miles")))

# Quick check of the polygons
mapview::mapview(list(mpa_50,
                      mpa_100,
                      mpa_150,
                      mpa_200))

# Function to extract data by buffer
by_buffer <- function(buffer) {
  annual_pre_post %>% 
    filter(between(event, -5, 4)) %>% 
    select(-src) %>% 
    st_as_sf(coords = c("lon", "lat"),
             crs = "EPSG:4326") %>%
    st_filter(mpas,.predicate = st_disjoint) %>% 
    st_filter(buffer) %>% 
    st_drop_geometry() %>% 
    group_by(year, event, wdpaid, name, post) %>%
    summarize(sets_tot = sum(sets_tot),
              sets_dfad = sum(sets_dfad),
              .groups = "drop") %>%
    mutate(dfad_prop_tot = sets_dfad / sets_tot)
}

dist_gradient <- list(
  mpa_50 = mpa_50,
  mpa_100 = mpa_100,
  mpa_150 = mpa_150,
  mpa_200 = mpa_200) %>%
  map_dfr(by_buffer, .id = "ring") %>% 
  mutate(ring_num = as.numeric(str_extract(ring, "[:digit:]+"))) %>% 
  mutate(post = 1 * (post == "After"),
         ring = str_extract(ring, "[:digit:]+"),
         ring = fct_reorder(ring, ring_num))

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
gradient_plot <- dist_gradient %>% 
  group_by(name, post, ring_num) %>% 
  summarize(dfad_prop_tot_var = (sd(dfad_prop_tot, na.rm = T)) ^ 2,
            dfad_prop_tot = mean(dfad_prop_tot, na.rm = T),
            .groups = "drop") %>%
  pivot_wider(names_from = post,
              values_from = c(dfad_prop_tot, dfad_prop_tot_var),
              names_prefix = "post_") %>% 
  mutate(dif = (dfad_prop_tot_post_1 - dfad_prop_tot_post_0),
         dif_sd = sqrt(dfad_prop_tot_var_post_0 + dfad_prop_tot_var_post_1),
         name = case_when(name == "Phoenix Islands Protected Area" ~ "PIPA",
                          name == "Ascension Island Marine Protected Area" ~ "Ascension",
                          T ~ name)) %>% 
  ggplot(aes(x = ring_num - 25, y = dif, fill = name, color = name, group = name)) +
  geom_line() +
  geom_hline(yintercept = 0) +
  geom_point(size = 3,
             color = "black",
             pch = 21) +
  scale_color_viridis_d(option = "viridis",
                        aesthetics = c("color", "fill")) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Distance form MPA boundary (NM)",
       y = "Mean change in dFAD effort",
       fill = "Large-Scale Marine Protected Area",
       color = "Large-Scale Marine Protected Area")


ggsave(plot = gradient_plot,
       filename = "dFAD_gradient_plot.pdf",
       units = "cm",
       width = 9.2,
       height = 6)

dist_gradient %>% 
  mutate(post = ifelse(post == 1, "After", "Before"),
         post = fct_relevel(post, "Before", "After")) %>% 
  group_by(name, post, ring_num) %>% 
  summarize(dfad_prop_tot_sd = sd(dfad_prop_tot, na.rm = T),
            dfad_prop_tot = mean(dfad_prop_tot, na.rm = T),
            .groups = "drop") %>% 
  ggplot(aes(x = ring_num - 25, y = dfad_prop_tot, fill = post, color = post)) +
  geom_pointrange(aes(ymin = dfad_prop_tot - dfad_prop_tot_sd,
                      ymax = dfad_prop_tot + dfad_prop_tot_sd),
                  fatten = 1,
                  size = 3,
                  pch = 21,
                  color = "black") +
  geom_line() +
  geom_hline(yintercept = 0) +
  theme_linedraw(base_size = 10) +
  scale_color_viridis_d(option = "cividis", aesthetics = c("color", "fill")) +
  guides(fill = guide_legend(override.aes = list(size = 1))) +
  labs(x = "Distance form MPA boundary",
       y = "%Change in mean relative dFAD effort",
       fill = "MPA",
       color = "MPA",
       linetype = "Period") +
  theme(legend.position = "inside",
        legend.position.inside = c(0.1, 0.6),
        legend.justification.inside = c(0, 0)) +
  facet_wrap(~name, scales = "free")
  
dist_gradient %>% 
  group_by(name, post, ring_num) %>% 
  summarize(sets_dfad = mean(sets_dfad, na.rm = T)) %>%
  pivot_wider(names_from = post,
              values_from = sets_dfad,
              names_prefix = "post_") %>% 
  mutate(dif = (post_1 - post_0)) %>% 
  ggplot(aes(x = ring_num - 25, y = dif, color = name)) +
  geom_line() +
  geom_hline(yintercept = 0) +
  geom_point(size = 3) +
  theme_linedraw(base_size = 10) +
  theme(legend.position = "bottom") +
  labs(x = "Distance form MPA boundary",
       y = "Change in dFAD sets")
