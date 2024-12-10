################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Clean IOTC data
# FAD activity data (2013-2022)
# Data come from: https://iotc.org/documents/fad-activity-data-2013-2022
# The website shows this:
# Reference: 
#   IOTC-2023-WGFAD05-DATA01
# File: 
#   Package icon IOTC-2023-WGFAD05-DATA01-FA.zip
# Type: 
#   Datasets
# Year: 
#   2023
# Meeting: 
#   IOTC ad hoc Working Group on FADs (WGFAD)
# Meeting session: 
#   5
# Availability: 
#   29 September 2023
# Authors: 
#   IOTC Secretariat
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  readxl,
  janitor,
  tidyverse
)

# Load data --------------------------------------------------------------------
iotc_raw <- read_excel("raw_data/rfmo_data/IOTC/IOTC-2023-WGFAD05-DATA01-FA.xlsx",
                       sheet = 7,
                       col_types = c("text","numeric", "numeric", "numeric",
                                     "text", "text", "text", "text",
                                     "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")) %>% 
  clean_names()

# UDFs -------------------------------------------------------------------------
# Define a function to make grid codes into coordinates
grid_to_coords <- function(x) {
  # Extract pieces of the grid codes
  # Size of the rectangle (5 indicates 1 degree and 6 indicates 5 degree)
  size <- str_sub(x, start = 0, end = 1)
  size <- case_when(size == 5 ~ 1,
                    size == 6 ~ 5)
  # Quadrant indicate shemispere
  quadrant <- as.numeric(str_sub(x, start = 2, end = 2))
  
  # Now extract the lat and long
  lat <- as.numeric(str_sub(x, start = 3, end = 4))
  lon <- as.numeric(str_sub(x, start = 5, end = 7))
  
  # Position in the hemisphere
  lat <- ifelse(quadrant %in% c(1, 4), lat, -1 * lat)
  lon <- ifelse(quadrant %in% c(1, 2), lon, -1 * lon)
  
  # Adjust the coordinate to be in the center of the cell
  # The IOTC grids are assumed to occur in the Indicate the corner of the square
  # closest to 0o latitude and 0o longitude, which is annoying...
  lat <- case_when(quadrant == 1 ~ lat + (size / 2),
                   quadrant == 2 ~ lat - (size / 2),
                   quadrant == 3 ~ lat - (size / 2),
                   quadrant == 4 ~ lat + (size / 2))
  
  lon <- case_when(quadrant == 1 ~ lon + (size / 2),
                   quadrant == 2 ~ lon + (size / 2),
                   quadrant == 3 ~ lon - (size / 2),
                   quadrant == 4 ~ lon - (size / 2))
  
  grid <- paste0(size, "x", size)
  
  
  return(data.frame(lat, lon, grid))
}

## PROCESSING ##################################################################

# Clean the data ---------------------------------------------------------------
iotc <- iotc_raw %>% 
  # Filters
  
  filter(
    # Filter vessel type
    vessel_type_code == "PS",               # PS:	Purse seiner
    # Filter activity type
    activity_type_code %in% c("DD",         # DD:	Deployment of drifting FAD
                              "DH",         # DH:	Retrieval/encounter and hauling of drifting FAD
                              "DI",         # DI:	Retrieval/encounter, hauling, and intervention on electronic equipment of drifting FAD
                              "DL",         # DL:	Loss of drifting FAD (tracking signal lost)
                              "DR"),        # DR:	Retrieval of drifting FAD 
    # Filter type of floating object
    fob_type_code %in% c("DRT",	            # DRT: Other drifting objects located using a tracking system (radio or satellite transmission) (e.g. dead animal, etc)
                         "FAD",	            # FAD: Drifting raft or FAD without a net NOT located using a tracking system (radio or satellite transmission)
                         "FDT",	            # FDT: Drifting raft or FAD without a net located using a tracking system (radio or satellite transmission)
                         "LGT",	            # LGT: Drifting log or debris located using a tracking system (radio or satellite transmission)
                         "NFD",	            # NFD: Drifting raft or FAD with a net NOT located using a tracking system (radio or satellite transmission)
                         "NFT")             # NFT: Drifting raft or FAD with a net located using a tracking system (radio or satellite transmission)
  )

unique_coords <- tibble(fishing_ground_code = unique(iotc$fishing_ground_code)) %>%
  filter(str_sub(fishing_ground_code, 1, 1) == c("5")) %>% # The first digit indicates the grid size, so we only want 1X1 degs grids
  mutate(coords = map(fishing_ground_code, grid_to_coords)) %>%
  unnest(coords) %>% 
  drop_na(lat, lon)

annual_iotc <- iotc %>% 
  inner_join(unique_coords, by = "fishing_ground_code") %>% 
  mutate(catch_dfad = alb + bet + skj + yft) %>% 
  select(year, lat, lon,
         sets_dfad = num_sets_on_fob,
         catch_dfad) %>% 
  group_by(year, lat, lon) %>% 
  summarize(sets_dfad = sum(sets_dfad, na.rm = T),
            catch_dfad = sum(catch_dfad, na.rm = T)) %>% 
  filter(sets_dfad > 0,
         catch_dfad > 0) %>% 
  mutate(cpue_dfad = catch_dfad / sets_dfad,
         src = "iotc")
  
## VISUALIZE ###################################################################
ggplot(annual_iotc,
       aes(x = lon, y = lat, fill = sets_dfad)) +
  geom_tile() +
  facet_wrap(~year) +
  coord_equal()

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(annual_iotc, file = here("processed_data", "annual_iotc_effort_1deg.rds"))
