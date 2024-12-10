################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# COMPARE RFMO DATA VS GFW EFFORT
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------

# Load data --------------------------------------------------------------------
gfw_ps_effort <- readRDS(here("processed_data", "annual_gfw_tuna_ps_effort_1deg.rds"))

rfmo_ps_effort <- readRDS(file = here("processed_data", "annual_rfmo_effort_1deg.rds")) %>%
  group_by(src, year, lat, lon) %>%
  summarize(sets_tot = sum(sets_tot, na.rm = T),
            sets_dfad = sum(sets_dfad, na.rm = T),
            .groups = "drop") 

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
combined <- rfmo_ps_effort %>% 
  inner_join(gfw_ps_effort, by = join_by(lat, lon, year)) %>% 
  select(-year) %>%
  group_by(src, lat, lon) %>%
  summarize_all(sum, na.rm = T) %>%
  ungroup() %>%
  filter((sets_dfad/sets_tot) > 0.1,
         sets_dfad > 10) %>%
  mutate(hemisphere = ifelse(lat < 0, "South", "North"),
         src = fct_relevel(src, "wcpfc", "iattc", "iccat"))

gof <- feols(n_vessels_p3m1 ~ sets_dfad,
      data = combined,
      split = ~paste(src, hemisphere)) %>% 
  map_dfr(broom::glance, .id = "model") %>% 
  mutate(hemisphere = str_extract(model, "North|South"),
         src = str_extract(model, "iccat|wcpfc|iattc|iotc"),
         src = fct_relevel(src, "wcpfc", "iattc", "iccat", "iotc"),
         string = paste("R^2 = ",round(adj.r.squared, 3),
                        "\nN = ", nobs))
  

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
ggplot(data = combined,
       aes(x = sets_dfad,
           y = n_vessels_p3m1,
           color = hemisphere)) +
  geom_abline(intercept = 0,
              slope = 1,
              linetype = "dashed") +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm",
              color = "red") +
  geom_text(data = gof,
            aes(x = 200, y = 700,
                label = string),
            color = "black",
            size = 3) +
  facet_wrap(hemisphere~src, ncol = 4) +
  coord_equal() +
  theme_linedraw() +
  labs(x = "#dFAD sets reported by RFMOs",
       y = "#dFAD sets inferred from GFW data",
       color = "Hemisphere",
       subtitle = "Grid cells with less than 10 dFAD sets and where dFAD sets are less
than 10% of total sets removed")
## EXPORT ######################################################################

# X ----------------------------------------------------------------------------