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
  tidyverse
)

# Load data --------------------------------------------------------------------
wcpfc <- readRDS(file = here("processed_data", "annual_wcpfc_effort_1deg.rds"))
iattc <- readRDS(file = here("processed_data", "annual_iattc_effort_1deg.rds"))
iccat <- readRDS(file = here("processed_data", "annual_iccat_effort_1deg.rds"))
iotc <- readRDS(file = here("processed_data", "annual_iotc_effort_1deg.rds"))


## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
rfmo <- bind_rows(wcpfc, iattc, iccat, iotc) %>% 
  filter(between(year, 1991, 2021))

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
rfmo %>%
  group_by(lat, lon) %>%
  summarize(sets_dfad = sum(sets_dfad, na.rm = T)) %>% 
  ggplot(mapping = aes(x = lon, y = lat, fill = log(sets_dfad))) +
  geom_tile() +
  coord_equal() +
  theme_void()

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(object = rfmo,
        file = here("processed_data", "annual_rfmo_effort_1deg.rds"))
