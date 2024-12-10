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
  suntools,
  tidyverse,
  furrr
)


# UDFs -------------------------------------------------------------------------
# From NOAA's deffinition: 
# https://forecast.weather.gov/glossary.php?word=dawn#:~:text=Nautical%20Dawn,for%20objects%20to%20be%20distiguishable.
# Nautical Dawn
# The time at which the sun is 12 degrees below the horizon in the morning. Nautical dawn is defined as that time at which there is just enough sunlight for objects to be distiguishable.

# nautical_twilight <- function(lon, lat, date) {
#   crepuscule(crds = matrix(c(lon, lat), nrow = 1),
#              dateTime = ymd_h(paste(date, "1"), tz = "UTC"),
#              solarDep = 12,
#              direction = "dawn",
#              POSIXct.out = TRUE) %>% 
#     pull(time) %>% 
#     as.character()
# }

sunrise <- function(lon, lat, date) {
  sunriset(crds = matrix(c(lon, lat), nrow = 1),
           dateTime = ymd_h(paste(date, "1"), tz = "UTC"),
           direction = "sunrise",
           POSIXct.out = TRUE) %>% 
    pull(time) %>% 
    as.character()
}

## PROCESSING ##################################################################

plan(multisession, workers = 14)
# Begin parallel computation ---------------------------------------------------
spat_date_grid <- expand_grid(lon = seq(-179.5, 179.5, by = 1),
                              lat = seq(-49.5, 49.5, by = 1),
                              date = seq(date("2018-12-31"),
                                         date("2019-12-31"),
                                         by = "1 day")) %>% 
  mutate(sunrise = future_pmap_chr(.l = list(lon = lon, lat = lat, date = date),
                                   .f = sunrise,
                                   .options = furrr_options(seed = 1))) %>% 
  mutate(sunrise = ymd_hms(sunrise, tz = "UTC")) %>% 
  select(lon, lat, date, sunrise)
plan(sequential)
## EXPORT ######################################################################

# Save as a csv ----------------------------------------------------------------
write_csv(x = spat_date_grid,
          file = here("raw_data", "daily_sunrise_times_1_deg.csv"))


