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
                  legend.box.spacing = element_blank(),
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

# 50 nautical miles
mpa_50 <- select_mpas |> 
  st_buffer(dist = units::as_units(50, "nautical_miles")) |> 
  st_make_valid() |> 
  st_erase(select_mpas)

# 100 nautical miles
mpa_100 <- select_mpas |> 
  st_buffer(dist = units::as_units(100, "nautical_miles")) |> 
  st_make_valid() |>
  st_erase(st_buffer(select_mpas, dist = units::as_units(50, "nautical_miles")))

# 100 nautical miles
mpa_150 <- select_mpas |> 
  st_buffer(dist = units::as_units(150, "nautical_miles")) |> 
  st_make_valid() |> 
  st_erase(st_buffer(select_mpas, dist = units::as_units(100, "nautical_miles")))
# 150 nautical miles
mpa_200 <- select_mpas |> 
  st_buffer(dist = units::as_units(200, "nautical_miles")) |> 
  st_make_valid() |> 
  st_erase(st_buffer(select_mpas, dist = units::as_units(150, "nautical_miles")))


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

dist_gradient <- list(
  mpa_50 = mpa_50,
  mpa_100 = mpa_100,
  mpa_150 = mpa_150,
  mpa_200 = mpa_200) |>
  map_dfr(by_buffer, .id = "ring") |> 
  mutate(ring_num = as.numeric(str_extract(ring, "[:digit:]+"))) |> 
  mutate(post = 1 * (post == "After"),
         ring = str_extract(ring, "[:digit:]+"),
         ring = fct_reorder(ring, ring_num)) |> 
  mutate(ring = fct_relevel(ring, "200", "150", "100", "50")) |> # Reorderd ring levels with the most distant one as the reference one because we care about changes near the MPA relative to changes far
  mutate(name = case_when(name == "Ascension Island Marine Protected Area" ~ "Ascensión",
                          name == "Phoenix Islands Protected Area" ~ "PIPA",
                          T ~ name))
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

## Build regression table

modelsummary(list(disc, "Pooled" = disc_glob),
             title = "Coefficient estimates for linear model testing for changes in %dFAD effort near MPA boundaries. Numbers in parentheses are panel-robust standard errors. For MPA-level regressions (columns 1-4), standard errors are calculated at the ring-by-year level. For poled regression (column 5) standard errors are calculated at the mpa-by-ring-year level.",
             output = here("results", "tabs", "regression_results.docx"),
             stars = panelsummary:::econ_stars(),
             coef_rename = c("ring50" = "50 nm ring",
                             "ring100" = "100 nm ring",
                             "ring150" = "150 nm ring",
                             "post" = "after",
                             "(Intercept)" = "Intercept"),
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

# X ----------------------------------------------------------------------------
colors <- c("#000000",
            "#4941a8",
            "#8e2c28",
            "#b2b2b2")

gradient_plot <- ggplot(data = coef_table,
                        aes(x = ring, y = estimate)) + 
  geom_ribbon(data = global_model, aes(x = ring, ymin = conf.low,
                                       ymax = conf.high),
              fill = "gray",
              alpha = 0.5) +
  geom_ribbon(data = global_model, aes(x = ring, ymin = estimate - std.error,
                                       ymax = estimate + std.error),
              fill = "gray",
              alpha = 0.75) +
  geom_line(data = global_model, aes(x = ring, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_linerange(aes(ymin = conf.low,
                     ymax = conf.high,
                     group = sample),
                 linewidth = 0.25,
                 position = position_dodge(width = 5)) +
  geom_linerange(aes(ymin = estimate - std.error,
                     ymax = estimate + std.error,
                     color = sample),
                 linewidth = 1,
                 position = position_dodge(width = 5)) +
  geom_point(aes(fill = sample),
             size = 3,
             shape = 21,
             color = "black",
             position = position_dodge(width = 5)) +
  scale_color_manual(values = colors, aesthetics = c("fill", "colour")) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Distance form MPA boundary (NM)",
       y = "Change in dFAD effort\n(percentage points)",
       fill = "Large-Scale Marine Protected Area",
       color = "Large-Scale Marine Protected Area")

abs_gradient_plot <- ggplot(data = abs_coef_table,
                        aes(x = ring, y = estimate)) + 
  geom_ribbon(data = abs_global_model, aes(x = ring, ymin = conf.low,
                                       ymax = conf.high),
              fill = "gray",
              alpha = 0.5) +
  geom_ribbon(data = abs_global_model, aes(x = ring, ymin = estimate - std.error,
                                       ymax = estimate + std.error),
              fill = "gray",
              alpha = 0.75) +
  geom_line(data = abs_global_model, aes(x = ring, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_linerange(aes(ymin = conf.low,
                     ymax = conf.high,
                     group = sample),
                 linewidth = 0.25,
                 position = position_dodge(width = 5)) +
  geom_linerange(aes(ymin = estimate - std.error,
                     ymax = estimate + std.error,
                     color = sample),
                 linewidth = 1,
                 position = position_dodge(width = 5)) +
  geom_point(aes(fill = sample),
             size = 3,
             shape = 21,
             color = "black",
             position = position_dodge(width = 5)) +
  scale_color_manual(values = colors, aesthetics = c("fill", "colour")) +
  labs(x = "Distance form MPA boundary (NM)",
       y = "Change in dFAD effort (# sets)",
       fill = "Large-Scale Marine Protected Area",
       color = "Large-Scale Marine Protected Area")


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
                  fatten = 1,
                  size = 3,
                  pch = 21,
                  color = "black") +
  geom_line() +
  geom_hline(yintercept = 0) +
  theme_linedraw(base_size = 10) +
  scale_color_viridis_d(option = "cividis", aesthetics = c("color", "fill")) +
  scale_y_continuous(labels = scales::percent) +
  guides(fill = guide_legend(override.aes = list(size = 1))) +
  labs(x = "Distance form MPA boundary",
       y = "Change in relative dFAD effort",
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
       y = "Change in dFAD effort",
       fill = "Period",
       color = "Period") +
  theme(legend.position = "None") +
  facet_wrap(~name, scales = "free")


means <- cowplot::plot_grid(mean_measures,
                            abs_mean_measures,
                            labels = "AUTO",
                            ncol = 1)


## EXPORT ######################################################################

ggsave(plot = gradient_plot,
       filename = here("results", "figs", "dFAD_gradient_plot.pdf"),
       units = "cm",
       width = 9.2,
       height = 6)

ggsave(plot = abs_gradient_plot,
       filename = here("results", "figs", "abs_dFAD_gradient_plot.pdf"),
       units = "cm",
       width = 9.2,
       height = 6)

ggsave(plot = means,
       filename = here("results", "figs", "dFAD_effort_by_ring.pdf"),
       units = "cm",
       width = 20,
       height = 20)


