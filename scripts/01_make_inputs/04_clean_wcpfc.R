################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Clean WCPFC data
# Source: https://www.wcpfc.int/wcpfc-public-domain-aggregated-catcheffort-data-download-page
# Raw data downloaded on: Oct 10, 2024
# Page says: "These data files were last updated on the 31st October 2023."
# Two sources:
# 1) Aggregated data, grouped by 1°x1° latitude/longitude grids, YEAR and MONTH
#     PURSE SEINE fishery. Data cover 1950 to 2022 for the WCPFC Convention Area
# The available metadata states: 
# Field Name Picture Description
# ________________________________________________________________________________________
# YY N( 4 ) Year
# MM N( 2 ) Month
# LAT_short C( 3 ) Latitude. It represents the latitude of the
# south-west corner of 1° square for these data.
# LON_short C( 4 ) Longitude. It represents the longitude of
# the south-west corner of 1° square for these data.
# CWP_GRID N( 11 ) Coordinating Working Party No
# DAYS N( 6 ) Days fishing and searching (effort).
# SETS_UNA N( 6 ) Number of Sets (Unassociated schools).
# SETS_LOG N( 6 ) Number of Sets (Natural Log/debris).
# SETS_DFAD N( 6 ) Number of Sets (Drifting FAD).
# SETS_AFAD N( 6 ) Number of Sets (Anchored FAD).
# SETS_OTH N( 6 ) Number of Sets (Other set types combined).
# SKJ_C_UNA N( 8, 3) Skipjack catch in metric tonnes (Unassociated schools).
# YFT_C_UNA N( 8, 3) Yellowfin catch (metric tonnes) (Unassociated schools).
# BET_C_UNA N( 8, 3) Bigeye catch (metric tonnes) (Unassociated schools).
# OTH_C_UNA N( 8, 3) Other species catch (metric tonnes) (Unassociated schools).
# SKJ_C_LOG N( 8, 3) Skipjack catch in metric tonnes (Natural-Log schools).
# YFT_C_LOG N( 8, 3) Yellowfin catch (metric tonnes) (Natural-Log schools).
# BET_C_LOG N( 8, 3) Bigeye catch (metric tonnes) (Natural-Log schools).
# OTH_C_LOG N( 8, 3) Other species catch (metric tonnes) (Natural-Log schools).
# SKJ_C_DFAD N( 8, 3) Skipjack catch in metric tonnes (Drifting FAD schools).
# YFT_C_DFAD N( 8, 3) Yellowfin catch (metric tonnes) (Drifting FAD schools).
# BET_C_DFAD N( 8, 3) Bigeye catch (metric tonnes) (Drifting FAD schools).
# OTH_C_DFAD N( 8, 3) Other species catch (metric tonnes) (Drifting FAD schools).
# SKJ_C_AFAD N( 8, 3) Skipjack catch in metric tonnes (Anchored FAD schools).
# YFT_C_AFAD N( 8, 3) Yellowfin catch (metric tonnes) (Anchored FAD schools).
# BET_C_AFAD N( 8, 3) Bigeye catch (metric tonnes) (Anchored FAD schools).
# OTH_C_AFAD N( 8, 3) Other species catch (metric tonnes) (Anchored FAD schools).
# SKJ_C_OTH N( 8, 3) Skipjack catch in metric tonnes (Schools from other set types).
# YFT_C_OTH N( 8, 3) Yellowfin catch (metric tonnes) (Schools from other set types).
# BET_C_OTH N( 8, 3) Bigeye catch (metric tonnes) (Schools from other set types).
# OTH_C_OTH N( 8, 3) Other species catch (metric tonnes) (Schools from other set types).

# 2) Aggregated data, grouped by 1°x1° latitude/longitude grids, FLAG, YEAR and QUARTER
#     PURSE SEINE fishery. Data cover 1950 to 2022 for the WCPFC Convention Area
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  tidyverse
)

# Load data --------------------------------------------------------------------
mon_wcpfc_raw <- read_csv(here("raw_data/rfmo_data/WCPFC/WCPFC_S_PUBLIC_BY_1x1_MM_3/WCPFC_S_PUBLIC_BY_1x1_MM.CSV"))
# qtr_wcpfc_raw <- read_csv(here("raw_data/rfmo_data/WCPFC/WCPFC_S_PUBLIC_BY_1x1_QTR_FLAG_4/WCPFC_S_PUBLIC_BY_1x1_QTR_FLAG.CSV"))
## PROCESSING ##################################################################
# The steps needed are:
# - Retain only data from tuna species
# X ----------------------------------------------------------------------------

fix_coord <- function(x) {
  hem <- str_extract(x, "[:alpha:]")
  coord <- str_extract(string = x,
                       pattern = "[:digit:]+")
  sign <- ifelse(hem %in% c("S", "W"), -1, 1)
  fixed <- (sign * as.numeric(coord)) + 0.5
  return(fixed)
}

annual_wcpfc <- mon_wcpfc_raw %>% 
  # Fix lat  / lon, which are stored as characters and point to the south-west corner
  mutate(lat = fix_coord(lat_short),
         lon = fix_coord(lon_short)) %>%
  # Build the columns we care about: totla effort, total catch, dFAD effort, and dFAD catch
  mutate(sets_tot = sets_una + sets_log + sets_dfad + sets_afad + sets_oth,
         catch_tot =
           skj_c_una + yft_c_una + bet_c_una + 
           skj_c_log + yft_c_log + bet_c_log +
           skj_c_dfad + yft_c_dfad + bet_c_dfad +
           oth_c_dfad + skj_c_afad + yft_c_afad +
           bet_c_afad + oth_c_afad + skj_c_oth +
           yft_c_oth + bet_c_oth + oth_c_oth,
         catch_dfad = skj_c_dfad + yft_c_dfad + bet_c_dfad) %>% 
  rename(year = yy) %>% 
  group_by(year, lat, lon) %>% 
  summarize_at(.vars = vars(sets_tot, sets_dfad, catch_tot, catch_dfad), .funs = "sum") %>% 
  ungroup() %>% 
  filter(sets_tot > 0) %>% 
  mutate(cpue_tot = catch_tot / sets_tot,
         cpue_dfad = catch_dfad / sets_dfad,
         dfad_prop_tot = sets_dfad / sets_tot,
         src = "wcpfc")

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(annual_wcpfc, file = here("processed_data", "annual_wcpfc_effort_1deg.rds"))
