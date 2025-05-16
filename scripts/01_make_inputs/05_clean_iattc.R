################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Description
# Downloaded from: https://www.iattc.org/en-US/Data/Public-domain
# Downloaded on Dec 12, 2024
# Metadata available in PSTuna-Atun.pdf in the same folder
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  tidyverse,
  janitor
)

# Load data --------------------------------------------------------------------
mon_iattc_raw <- read_csv(here("raw_data/rfmo_data/IATTC/PublicPSTuna/PublicPSTunaSetType.csv")) %>% 
  clean_names()

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
annual_iattc <- mon_iattc_raw %>% 
  mutate(catch_tot = alb + bet + bkj + bzx + pbf + skj + tun + yft) %>% 
  select(year,
         lat = lat_c1,
         lon = lon_c1,
         set_type,
         num_sets,
         catch_tot) %>% 
  group_by(year, lat, lon) %>% 
  summarize(sets_tot = sum(num_sets, na.rm = T),
            sets_dfad = sum(num_sets[set_type == "OBJ"], na.rm = T),
            catch_tot = sum(catch_tot, na.rm = T),
            catch_dfad = sum(num_sets[set_type == "OBJ"], na.rm = T),
            .groups = "drop") %>% 
  filter(sets_tot > 0) %>% 
  mutate(cpue_tot = catch_tot / sets_tot,
         cpue_dfad = catch_dfad / sets_dfad,
         dfad_prop_tot = sets_dfad / sets_tot,
         src = "iattc")

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(annual_iattc, file = here("processed_data", "annual_iattc_effort_1deg.rds"))
