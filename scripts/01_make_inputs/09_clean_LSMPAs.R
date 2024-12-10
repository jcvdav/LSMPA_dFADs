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
  janitor,
  readxl,
  smoothr,
  rmapshaper,
  sf,
  tidyverse
)

# Load data --------------------------------------------------------------------
list_of_lsmpas <- read_excel(here("raw_data", "list_LSMPAs.xlsx"), sheet = 1) %>% 
  clean_names() %>% 
  select(wdpa_id, name, year_enforced) %>% 
  filter(!wdpa_id == "220201")

all_mpas <- list.files(path =  here("raw_data", "WDPA_WDOECM_Feb2023_Public_marine_shp"),
                       pattern = "polygons.shp",
                       full.names = T,
                       recursive = T) %>%
  map_dfr(st_read) %>%
  st_as_sf() %>%
  clean_names()

## PROCESSING ##################################################################

# Filter for relevant MPAs ------------------------------------------------------
LSMPAs <- all_mpas %>% 
  filter(wdpaid %in% list_of_lsmpas$wdpa_id | 
           wdpa_pid %in% list_of_lsmpas$wdpa_id |
           name %in% list_of_lsmpas$name) %>% 
  st_make_valid() %>% 
  filter(st_area(.) >= units::as_units(100000, "km2")) %>% 
  ms_simplify() %>%
  mutate(wdpaid = as.character(wdpaid)) %>%
  select(wdpaid, wdpa_pid, name, status_yr) %>% 
  left_join(list_of_lsmpas %>% select(-name), by = join_by(wdpaid == wdpa_id)) %>% 
  left_join(list_of_lsmpas %>% select(-name), by = join_by(wdpa_pid == wdpa_id)) %>% 
  mutate(year_enforced = coalesce(year_enforced.x, year_enforced.y),
         year_enforced = coalesce(year_enforced, status_yr)) %>% 
  group_by(wdpaid, name, year_enforced) %>% 
  summarize(a = sum(1),
            .groups = "drop") 

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
plot(LSMPAs, max.plot = 1)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
st_write(obj = LSMPAs,
         dsn = here("processed_data", "selected_LSMPAs_viz.gpkg"),
         delete_dsn = T)
