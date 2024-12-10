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
  DBI,
  bigrquery,
  tidyverse
)

bq_auth("juancarlos@ucsb.edu")

# Load data --------------------------------------------------------------------
rfmo_vessels <- readxl::read_excel(path = here("raw_data/registries/ps_rfmo_master.xlsx"),
                                   sheet = 1)

# Establish connection ---------------------------------------------------------
con <- dbConnect(bigquery(),
                 project = "emlab-gcp",
                 billing = "lsmpas")

vi <- tbl(src = con,
          from = "world-fishing-827.gfw_research.vi_ssvid_v20240701")
segs <- tbl(src = con,
            from = "world-fishing-827.gfw_research.pipe_v20201001_segs")
tracks <- tbl(src = con,
              from = "world-fishing-827.gfw_research.pipe_v20201001_fishing")
twilight_datetimes <- tbl(con,
                          "emlab-gcp.jc_scratch.daily_sunrise_times_1_deg_v_20240828") %>% 
  mutate(month = sql("EXTRACT(MONTH FROM date)"),
         day = sql("EXTRACT(DAY FROM date)"))

## PROCESSING ##################################################################

# Find tuna purse seines -------------------------------------------------------
ps <- vi %>% 
  filter(sql("best.best_vessel_class = 'tuna_purse_seines'"),
         sql("best.best_length_m >= 20"),
         on_fishing_list_best) %>% 
  mutate(
    # AIS info
    ais_mostcommon_shipname = sql("ais_identity.n_shipname_mostcommon.value"),
    ais_mostcommon_callsign = sql("ais_identity.n_callsign_mostcommon.value"),
    ais_mostcommon_imo = sql("ais_identity.n_imo_mostcommon.value"),
    # Registry info
    registry_vessel_class = sql("registry_info.best_known_vessel_class"),
    registry_shipname = sql("registry_info.best_known_shipname"),
    registry_callsign = sql("registry_info.best_known_callsign"),
    registry_imo = sql("registry_info.best_known_imo"),
    registries_listed = sql("registry_info.registries_listed"),
    registry_length_m = sql("registry_info.best_known_length_m"),
    registry_tonnage_gt = sql("registry_info.best_known_tonnage_gt"),
    registry_flag = sql("registry_info.best_known_flag"),
    # Inferred info
    inferred_vessel_class = sql("inferred.inferred_vessel_class"),
    inferred_length_m = sql("inferred.avg_inferred_length"),
    inferred_tonnage_gt = sql("inferred.avg_inferred_tonnage"),
    # Best info
    best_vessel_class = sql("best.best_vessel_class"),
    best_flag = sql("best.best_flag"),
    best_length_m = sql("best.best_length_m"),
    best_tonnage_gt = sql("best.best_tonnage_gt")
  ) %>% 
  select(ssvid,
         ais_mostcommon_shipname, registry_shipname,
         ais_mostcommon_callsign, registry_callsign,
         ais_mostcommon_imo, registry_imo,
         best_vessel_class, registry_vessel_class, inferred_vessel_class,
         best_length_m, registry_length_m, inferred_length_m,
         best_tonnage_gt, registry_tonnage_gt, inferred_tonnage_gt,
         best_flag, registry_flag,
         registries_listed) %>% 
  # We retain those where either the IMO or Callsign has appeared in a registry
  filter(registry_imo %in% !!unique(rfmo_vessels$registry_imo) |
         registry_callsign %in% !!unique(rfmo_vessels$registry_callsign))

# Build a table of "good segments" to only keep valid and relevant data
good_segs <- segs %>% 
  filter(good_seg,
         !overlapping_and_short) %>% 
  select(seg_id)

# Now get the sunrise times
twilight_times <- twilight_datetimes %>% 
  mutate(sunrise_time = sql("EXTRACT(time FROM sunrise)"))

# Then, use this position_sunrise_datetime to ADD and SUBTRACT the 1-hr period needed
ps_grided_by_ssvid <- tracks %>% 
  select(seg_id, ssvid, timestamp, lat, lon, hours, nnet_score) %>%
  inner_join(ps, by = join_by("ssvid")) %>%                                     # This keeps the relevant purse seiners only
  inner_join(good_segs, by = join_by("seg_id")) %>%                             # This keeps good segments (track data) only
  filter(nnet_score > 0,                                                        # Remove non-fishing activity
         sql("hours is not NULL"),                                              # Remove null hours (ie. start of segments)
         sql("EXTRACT(YEAR FROM timestamp) BETWEEN 2015 and 2023"),             # Keep data between 2015 and 2023 only
         between(lat, -50, 50)) %>%                                             # And between 50 north and 50 south
  mutate(lat = floor(lat) + 0.5,                                                # Center it at 1 degree
         lon = floor(lon) + 0.5,
         month = sql("EXTRACT(MONTH FROM timestamp)"),
         day = sql("EXTRACT(DAY FROM timestamp)"),
         year = sql("EXTRACT(YEAR FROM timestamp)")) %>% 
  left_join(twilight_datetimes, by = join_by("lat", "lon", "month", "day")) %>% #We join it by location and month-day (NOTE NO YERAR)
  # Our sunrise table doesn't have a year component
  mutate(position_sunrise_timestamp = sql("TIMESTAMP(CONCAT(EXTRACT(date FROM timestamp), ' ', EXTRACT(time FROM sunrise)))")) %>%
  mutate(sunrise_p3m1 = sql("CAST(timestamp BETWEEN
                            TIMESTAMP_ADD(position_sunrise_timestamp, INTERVAL -1 HOUR)
                            AND
                            TIMESTAMP_ADD(position_sunrise_timestamp, INTERVAL 3 HOUR)
                            AS int64)"),
         sunrise_pm1 = sql("CAST(timestamp BETWEEN
                            TIMESTAMP_ADD(position_sunrise_timestamp, INTERVAL -1 HOUR)
                            AND
                            TIMESTAMP_ADD(position_sunrise_timestamp, INTERVAL 1 HOUR)
                            AS int64)"),
         sunrise_pm3 = sql("CAST(timestamp BETWEEN
                            TIMESTAMP_ADD(position_sunrise_timestamp, INTERVAL -3 HOUR)
                            AND
                            TIMESTAMP_ADD(position_sunrise_timestamp, INTERVAL 3 HOUR)
                            AS int64)")) %>% 
  select(lat, lon, year, month, day, ssvid, hours, sunrise_p3m1, sunrise_pm1, sunrise_pm3) %>% 
  group_by(year, month, day, ssvid, lat, lon) %>%
  summarize(total_fishing_hours = sum(hours, na.rm = T),
            fishing_hours_sunrise_p3m1 = sum(hours * sunrise_p3m1, na.rm = T),
            fishing_hours_sunrise_pm1 = sum(hours * sunrise_pm1, na.rm = T),
            fishing_hours_sunrise_pm3 = sum(hours * sunrise_pm3, na.rm = T),
            .groups = "drop")

annual_ps_grided_total <- ps_grided_by_ssvid %>% 
  group_by(year, month, day, lat, lon) %>% 
  summarize(total_n_vessels = n_distinct(ssvid),
            total_fishing_hours = sum(total_fishing_hours, na.rm = T),
            n_vessels_p3m1 = n_distinct(ssvid[fishing_hours_sunrise_p3m1 > 0]),
            fishing_hours_sunrise_p3m1 = sum(fishing_hours_sunrise_p3m1, na.rm = T),
            n_vessels_pm1 = n_distinct(ssvid[fishing_hours_sunrise_pm1 > 0]),
            fishing_hours_sunrise_pm1 = sum(fishing_hours_sunrise_pm1, na.rm = T),
            n_vessels_pm3 = n_distinct(ssvid[fishing_hours_sunrise_pm3 > 0]),
            fishing_hours_sunrise_pm3 = sum(fishing_hours_sunrise_pm3, na.rm = T),
            .groups = "drop") %>% 
  select(-c(month, day)) %>% 
  group_by(year, lat, lon) %>% 
  summarize_all("sum") %>% 
  ungroup()

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
annual_ps_grided_total_local <- collect(annual_ps_grided_total)
# Save to disc -----------------------------------------------------------------
saveRDS(annual_ps_grided_total_local,
        file = here("processed_data", "annual_gfw_tuna_ps_effort_1deg.rds"))
