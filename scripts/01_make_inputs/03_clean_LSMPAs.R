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
  select(name, wdpa_id) %>% 
  filter(!wdpa_id %in% c("220201", "2628", "555543712"))

all_mpas <- list.files(path =  here("raw_data", "WDPA_WDOECM_Feb2023_Public_marine_shp"),
                       pattern = "polygons.shp",
                       full.names = T,
                       recursive = T) %>%
  map_dfr(st_read) %>%
  st_as_sf() %>%
  clean_names()

## PROCESSING ##################################################################

# Filter for relvant MPAs ------------------------------------------------------
LSMPAs <- all_mpas %>% 
  filter(wdpaid %in% list_of_lsmpas$wdpa_id | 
           wdpa_pid %in% list_of_lsmpas$wdpa_id |
           name %in% list_of_lsmpas$name) %>% 
  filter(!wdpaid %in% c("220201", "2628", "555543712")) %>% 
  filter(!wdpa_pid %in% c("220201", "2628", "555543712")) %>% 
  ms_simplify() %>% 
  group_by(wdpaid, name) %>% 
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
