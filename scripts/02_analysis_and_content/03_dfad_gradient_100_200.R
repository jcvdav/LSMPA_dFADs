################################################################################
# MPA dFAD effort proximity analysis
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  fixest,
  modelsummary,
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
                  legend.title = element_text(hjust = 0.5)))

# Load data --------------------------------------------------------------------
annual_pre_post <- readRDS(file = here("processed_data/annual_pre_post_activity_by_select_mpa.rds")) |> 
  filter(between(event, -5, 4))
mpas <- st_read(here("processed_data", "selected_LSMPAs_viz.gpkg"))

## PROCESSING ##################################################################
select_mpas <- mpas |> 
  filter(wdpaid %in% c(
    # "555705293",   # Cordillera de Coiba
    "11753",         # Galapagos
    "309888",        # PIPA
    "555629385",     # Revillagigedo
    "555651558"      # Asención
    # "555512151"   # Chagos - Can't do Chagos because 1) there is no data before it was implemented an 2) there is no "other sets" data to calculate dFAD set as % of total
  )) |> 
  nngeo::st_remove_holes()


# Build donuts -----------------------------------------------------------------
st_erase <- function(x, y) st_difference(x, st_union(st_combine(y)))

# 100 nautical miles
mpa_100 <- select_mpas |> 
  st_buffer(dist = units::as_units(100, "nautical_miles")) |> 
  st_make_valid() |>
  st_erase(select_mpas)

# 200 nautical miles
mpa_200 <- select_mpas |> 
  st_buffer(dist = units::as_units(200, "nautical_miles")) |> 
  st_make_valid() |> 
  st_erase(st_buffer(select_mpas, dist = units::as_units(100, "nautical_miles")))

rings <- bind_rows(
  mpa_100 |> mutate(near = 1),
  mpa_200 |> mutate(near = 0)
) |> 
  select(-a)

# Function to extract data by buffer
by_buffer <- function(buffer) {
  annual_pre_post |> 
    select(-src) |> 
    st_as_sf(coords = c("lon", "lat"),
             crs = "EPSG:4326") |>
    st_filter(mpas, .predicate = st_disjoint) |> # Remove points within MPAs
    st_filter(buffer) %>% # Keep points within buffer
    bind_cols(st_coordinates(.)) |> 
    st_drop_geometry() |> # Remove spatial features
    group_by(year, event, wdpaid, name, post) |> # Calculate mean by year and donut
    summarize(sets_tot = sum(sets_tot),
              sets_dfad = sum(sets_dfad),
              .groups = "drop") |>
    mutate(dfad_prop_tot = sets_dfad / sets_tot) # Calculate proportion
}

# Build the data set -----------------------------------------------------------
dist_gradient <- list(
  mpa_100 = mpa_100,
  mpa_200 = mpa_200) |>
  map_dfr(by_buffer, .id = "ring") |> 
  mutate(ring_num = as.numeric(str_extract(ring, "[:digit:]+"))) |> 
  mutate(post = 1 * (post == "After"),
         ring = str_extract(ring, "[:digit:]+"),
         ring = fct_reorder(ring, ring_num)) |> 
  mutate(ring = fct_relevel(ring, "200", "100")) |> # Reorderd ring levels with the most distant one as the reference one because we care about changes near the MPA relative to changes far
  mutate(name = case_when(name == "Ascension Island Marine Protected Area" ~ "Ascensión",
                          name == "Phoenix Islands Protected Area" ~ "PIPA",
                          T ~ name))

# Build data set by pixel ------------------------------------------------------
pixel <- annual_pre_post |> 
  select(year, lon, lat, contains("tot"), contains("dfad"), src, post, event) |> 
  mutate(id = paste(lon, lat, sep = "_"),
         post = ifelse(post == "After", 1, 0)) |> 
  st_as_sf(coords = c("lon", "lat"),
           crs = "EPSG:4326",
           remove = F) |>
  st_filter(mpas, .predicate = st_disjoint) |> # remove points inside the MPA
  st_join(rings) |> 
  drop_na(near) |> 
  st_drop_geometry()

# Calculate the BACI-like means manually as a check
BACI_means <- dist_gradient |> 
  group_by(name, post, ring_num) |> 
  summarize(dfad_prop_tot_var = (sd(dfad_prop_tot, na.rm = T)) ^ 2,
            dfad_prop_tot = mean(dfad_prop_tot, na.rm = T),
            .groups = "drop") |>
  pivot_wider(names_from = post,
              values_from = c(dfad_prop_tot, dfad_prop_tot_var),
              names_prefix = "post_") |> 
  mutate(dif = (dfad_prop_tot_post_1 - dfad_prop_tot_post_0),
         dif_sd = sqrt(dfad_prop_tot_var_post_0 + dfad_prop_tot_var_post_1)) 

## MODELLING ###################################################################
# Fit models with %dFAD sets
disc_glob <- feols(dfad_prop_tot ~ post + ring + post:ring | mpa,
                   data = dist_gradient |> rename(mpa = name) |> mutate(id = paste(mpa, ring)),
                   panel.id = ~id + year,
                   vcov = "NW")

disc <- feols(dfad_prop_tot ~ post + ring + post:ring,
              data = dist_gradient,
              panel.id = ~ring + year,
              vcov = "NW",
              split = ~name)

etable(disc_glob, disc)

# Fir models witrh %dFAD sets but nwo by pixel
disc_pixel <- feols(dfad_prop_tot ~ post + near + post:near,
                    data = pixel,
                    panel.id = ~id + year,
                    vcov = "NW",
                    split = ~name)

disc_pixel_fe <- feols(dfad_prop_tot ~ post + near + post:near | id + year,
                    data = pixel,
                    panel.id = ~id + year,
                    vcov = "NW",
                    split = ~name)

# Now absolute sets
abs_disc_glob <- feols(sets_dfad ~ post + ring + post:ring | mpa,
                       data = dist_gradient |> rename(mpa = name) |> mutate(id = paste(mpa, ring)),
                       panel.id = ~id + year,
                       vcov = "NW")

abs_disc <- feols(sets_dfad ~ post + ring + post:ring,
                  data = dist_gradient,
                  panel.id = ~ring + year,
                  vcov = "NW",
                  split = ~name)

etable(abs_disc_glob, abs_disc)

## Build regression tables -----------------------------------------------------
modelsummary(list("Asención" = disc[[1]],
                  "Galápagos" = disc[[2]],
                  "PIPA" = disc[[3]],
                  "Revillagigedo" = disc[[4]],
                  "Pooled" = disc_glob),
             title = "Coefficient estimates for linear model testing for changes in %dFAD effort near MPA boundaries. Numbers in parentheses are panel-robust standard errors. For MPA-level regressions (columns 1-4), standard errors are calculated at the ring-by-year level. For pooled regression (column 5) standard errors are calculated at the MPA-by-ring-year level.",
             output = here("results", "tabs", "regression_results_0_100_200.docx"),
             stars = panelsummary:::econ_stars(),
             coef_rename = c("ring50" = "50 nm ring",
                             "ring100" = "100 nm ring",
                             "ring150" = "150 nm ring",
                             "post" = "after",
                             "(Intercept)" = "Intercept"),
             gof_omit = "R2$|IC|RM|Wi")

# Pixel-level table
modelsummary(list("Basic DID" = disc_pixel,
                  "Fixed effects by year and pixel" = disc_pixel_fe),
             output = here("results", "tabs", "regression_results_pixel_near_far.docx"),
             shape = "rbind",
             stars = panelsummary:::econ_stars(),
             gof_omit = "IC|RM|Wi")

# Extract coefficient estiamtes into tables for plotting -----------------------
global_model <- broom::tidy(disc_glob, conf.int = T) |> 
  filter(str_detect(term, ":")) |> 
  mutate(ring = as.numeric(str_extract(term, "[:digit:]+")))

coef_table <- map_dfr(disc, broom::tidy, conf.int = T, .id = "sample") |> 
  filter(str_detect(term, ":")) |> 
  mutate(ring = as.numeric(str_extract(term, "[:digit:]+")),
         sample = str_remove(sample, "sample.var: name; sample: "))


abs_global_model <- broom::tidy(abs_disc_glob, conf.int = T) |> 
  filter(str_detect(term, ":")) |> 
  mutate(ring = as.numeric(str_extract(term, "[:digit:]+")))

abs_coef_table <- map_dfr(abs_disc, broom::tidy, conf.int = T, .id = "sample") |> 
  filter(str_detect(term, ":")) |> 
  mutate(ring = as.numeric(str_extract(term, "[:digit:]+")),
         sample = str_remove(sample, "sample.var: name; sample: "))

## VISUALIZE ###################################################################

mean_measures <- dist_gradient |> 
  mutate(post = ifelse(post == 1, "After", "Before"),
         post = fct_relevel(post, "Before", "After")) |> 
  group_by(name, post, ring_num) |> 
  summarize(dfad_prop_tot_sd = sd(dfad_prop_tot, na.rm = T),
            dfad_prop_tot = mean(dfad_prop_tot, na.rm = T),
            .groups = "drop") |> 
  ggplot(aes(x = ring_num - 25, y = dfad_prop_tot, fill = post, color = post)) +
  geom_pointrange(aes(ymin = dfad_prop_tot - dfad_prop_tot_sd,
                      ymax = dfad_prop_tot + dfad_prop_tot_sd),
                  size = 1,
                  pch = 21,
                  color = "black") +
  geom_line() +
  geom_hline(yintercept = 0) +
  theme_linedraw(base_size = 10) +
  scale_color_viridis_d(option = "cividis", aesthetics = c("color", "fill")) +
  scale_y_continuous(labels = scales::percent) +
  guides(fill = guide_legend(override.aes = list(size = 1))) +
  labs(x = "Distance form MPA boundary (nm)",
       y = "Relative dFAD effort",
       fill = "Period",
       color = "Period") +
  theme(legend.position = "inside",
        legend.position.inside = c(0.1, 0.6),
        legend.justification.inside = c(0, 0)) +
  facet_wrap(~name, scales = "free")

abs_mean_measures <- dist_gradient |> 
  mutate(post = ifelse(post == 1, "After", "Before"),
         post = fct_relevel(post, "Before", "After")) |> 
  group_by(name, post, ring_num) |> 
  summarize(dfad_sd = sd(sets_dfad, na.rm = T),
            dfad = mean(sets_dfad, na.rm = T),
            .groups = "drop") |> 
  ggplot(aes(x = ring_num - 25, y = dfad, fill = post, color = post)) +
  geom_pointrange(aes(ymin = dfad - dfad,
                      ymax = dfad + dfad),
                  size = 1,
                  pch = 21,
                  color = "black") +
  geom_line() +
  geom_hline(yintercept = 0) +
  theme_linedraw(base_size = 10) +
  scale_color_viridis_d(option = "cividis", aesthetics = c("color", "fill")) +
  guides(fill = guide_legend(override.aes = list(size = 1))) +
  labs(x = "Distance form MPA boundary (nm)",
       y = "dFAD effort",
       fill = "Period",
       color = "Period") +
  theme(legend.position = "None") +
  facet_wrap(~name, scales = "free")


means <- cowplot::plot_grid(mean_measures,
                            abs_mean_measures,
                            labels = "AUTO",
                            ncol = 1)


## EXPORT ######################################################################
ggsave(plot = means,
       filename = here("results", "figs", "dFAD_effort_by_ring_0_100_200.pdf"),
       units = "cm",
       dpi = 600,
       width = 20,
       height = 20)
