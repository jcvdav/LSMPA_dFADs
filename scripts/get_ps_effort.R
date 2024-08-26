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
con <- dbConnect(bigquery(),
                 project = "emlab-gcp")

vi <- tbl(src = con,
          from = "world-fishing-827.gfw_research.vi_ssvid_v20240701")
segs <- tbl(src = con,
            from = "world-fishing-827.gfw_research.pipe_v20201001_segs")
tracks <- tbl(src = con,
              from = "world-fishing-827.gfw_research.pipe_v20201001_fishing")
twilight_datetimes <- tbl(con,
                          "emlab-gcp.jc_scratch.daily_twilight_and_sunrise_times_1_deg_v_20240813") %>% 
  mutate(month = sql("EXTRACT(MONTH FROM date)"),
         day = sql("EXTRACT(DAY FROM date)")) %>% 
  filter(!(day == 29 & month == 2),
         !(sql("EXTRACT(day FROM sunrise) = 29") & 
             sql("EXTRACT(MONTH FROM sunrise) = 2")))

## PROCESSING ##################################################################

# Find tuna purse seines -------------------------------------------------------
ps <- vi %>% 
  filter(sql("best.best_vessel_class = 'tuna_purse_seines'"),
         on_fishing_list_best) %>% 
  mutate(registry_shipname = sql("registry_info.best_known_shipname"),
         registry_callsign = sql("registry_info.best_known_callsign"),
         registry_imo = sql("registry_info.best_known_imo"),
         best_flag = sql("best.best_flag"),
         best_length_m = sql("best.best_length_m"),
         best_tonnage_gt = sql("best.best_tonnage_gt"),
         best_engine_power_kw = sql("best.best_engine_power_kw")) %>% 
  select(ssvid,
         registry_shipname, registry_callsign, registry_imo,
         best_flag, best_length_m, best_tonnage_gt, best_engine_power_kw)

good_segs <- segs %>% 
  filter(good_seg,
         !overlapping_and_short) %>% 
  select(seg_id)

twilight_times <- twilight_datetimes %>% 
  mutate(sunrise_time = sql("EXTRACT(time FROM sunrise)"))



# I need to fix the definition of "before" and "after" sunrise. I think the
# strategy is to use datetimes, instead of times. But the datetimes need to be 
# created "in place" and they should be done as follows:
# take the "date" from the position timestamp
# take the "time" portion form the sunrise calculaton
# combine these to build a position_sunrise_datetime object that is relevant to the local
# context
# Then, use this position_sunrise_datetime to ADD and SUBTRACT the 1-hr period needed
ps_grided_by_ssvid <- tracks %>% 
  select(seg_id, ssvid, timestamp, lat, lon, hours, nnet_score) %>%
  inner_join(ps, by = join_by("ssvid")) %>%
  inner_join(good_segs, by = join_by("seg_id")) %>%
  filter(nnet_score > 0,
         sql("hours is not NULL"),
         sql("EXTRACT(YEAR FROM timestamp) BETWEEN 2015 and 2024"),
         between(lat, -50, 50)) %>%
  mutate(lat = floor(lat) + 0.5,
         lon = floor(lon) + 0.5,
         month = sql("EXTRACT(MONTH FROM timestamp)"),
         day = sql("EXTRACT(DAY FROM timestamp)"),
         year = sql("EXTRACT(YEAR FROM timestamp)")) %>% 
  left_join(twilight_datetimes, by = join_by("lat", "lon", "month", "day")) %>%
  mutate(position_sunrise_timestamp = sql("TIMESTAMP(CONCAT(EXTRACT(date FROM timestamp), ' ', EXTRACT(time FROM sunrise)))")) %>%
  # mutate(position_sunrise_timestamp = sql("TIMESTAMP(
  # CONCAT(
  #   EXTRACT(YEAR FROM timestamp), '-',
  # EXTRACT(MONTH FROM sunrise), '-',
  # EXTRACT(DAY FROM sunrise), ' ',
  # EXTRACT(time FROM sunrise)))")) %>%
  # mutate(time_from_sunrise = sql("TIMESTAMP_DIFF(timestamp, position_sunrise_timestamp, HOUR)")) %>% 
  mutate(sunrise_pm1 = sql("CAST(timestamp BETWEEN
                            TIMESTAMP_ADD(position_sunrise_timestamp, INTERVAL -1 HOUR)
                            AND
                            TIMESTAMP_ADD(position_sunrise_timestamp, INTERVAL 1 HOUR)
                            AS int64)"),
         sunrise_pm2 = sql("CAST(timestamp BETWEEN
                            TIMESTAMP_ADD(position_sunrise_timestamp, INTERVAL -2 HOUR)
                            AND
                            TIMESTAMP_ADD(position_sunrise_timestamp, INTERVAL 2 HOUR)
                            AS int64)"),
         sunrise_pm3 = sql("CAST(timestamp BETWEEN
                            TIMESTAMP_ADD(position_sunrise_timestamp, INTERVAL -3 HOUR)
                            AND
                            TIMESTAMP_ADD(position_sunrise_timestamp, INTERVAL 3 HOUR)
                            AS int64)")) %>% 
  select(lat, lon, year, ssvid, hours, sunrise_pm1, sunrise_pm2, sunrise_pm3) %>% 
  group_by(year, ssvid, lat, lon) %>%
  summarize(total_fishing_hours = sum(hours, na.rm = T),
            fishing_hours_sunrise_pm1 = sum(hours * sunrise_pm1, na.rm = T),
            fishing_hours_sunrise_pm2 = sum(hours * sunrise_pm2, na.rm = T),
            fishing_hours_sunrise_pm3 = sum(hours * sunrise_pm3, na.rm = T),
            .groups = "drop")

annual_ps_grided_total <- ps_grided_by_ssvid %>% 
  group_by(year, lat, lon) %>% 
  summarize(total_fishing_hours = sum(total_fishing_hours, na.rm = T),
            fishing_hours_sunrise_pm1 = sum(fishing_hours_sunrise_pm1, na.rm = T),
            fishing_hours_sunrise_pm2 = sum(fishing_hours_sunrise_pm2, na.rm = T),
            fishing_hours_sunrise_pm3 = sum(fishing_hours_sunrise_pm3, na.rm = T),
            .groups = "drop")

annual_effort_by_ssvid <- ps_grided_by_ssvid %>% 
  left_join(ps, by = join_by("ssvid")) %>% 
  group_by(ssvid, year, registry_shipname, registry_callsign, registry_imo, best_flag, best_length_m, best_tonnage_gt, best_engine_power_kw) %>% 
  summarize(total_fishing_hours = sum(total_fishing_hours, na.rm = T),
            fishing_hours_sunrise_pm1 = sum(fishing_hours_sunrise_pm1, na.rm = T),
            fishing_hours_sunrise_pm2 = sum(fishing_hours_sunrise_pm2, na.rm = T),
            fishing_hours_sunrise_pm3 = sum(fishing_hours_sunrise_pm3, na.rm = T),
            .groups = "drop")



## EXPORT ######################################################################

# X ----------------------------------------------------------------------------

annual_ps_grided_total_local <- collect(annual_ps_grided_total)

ps_grided_total_local <- annual_ps_grided_total_local %>% 
  group_by(lat, lon) %>% 
  summarize(total_fishing_hours = sum(total_fishing_hours, na.rm = T),
            fishing_hours_sunrise_pm1 = sum(fishing_hours_sunrise_pm1, na.rm = T),
            fishing_hours_sunrise_pm2 = sum(fishing_hours_sunrise_pm2, na.rm = T),
            fishing_hours_sunrise_pm3 = sum(fishing_hours_sunrise_pm3, na.rm = T),
            .groups = "drop")

annual_effort_by_ssvid_local <- collect(annual_effort_by_ssvid)

saveRDS(annual_ps_grided_total_local,
        file = here("processed_data", "annual_tuna_ps_effort_1deg.rds"))

saveRDS(ps_grided_total_local,
        file = here("processed_data", "tuna_ps_effort_1deg.rds"))

saveRDS(annual_effort_by_ssvid_local,
        file = here("processed_data", "annual_fishing_effort_by_vessel.rds"))


###################### TEST ####################################################
a <- tracks %>% 
  # select(seg_id, ssvid, timestamp, lat, lon, hours, nnet_score) %>%
  inner_join(ps, by = join_by("ssvid")) %>%
  # inner_join(good_segs, by = join_by("seg_id")) %>%
  filter(nnet_score > 0,
         # sql("hours is not NULL"),
         # sql("EXTRACT(YEAR FROM timestamp) BETWEEN 2015 and 2024"),
         between(lat, -50, 50)) %>%
  head(10000) %>% 
  mutate(lat = floor(lat) + 0.5,
         lon = floor(lon) + 0.5,
         month = sql("EXTRACT(MONTH FROM timestamp)"),
         day = sql("EXTRACT(DAY FROM timestamp)"),
         year = sql("EXTRACT(YEAR FROM timestamp)")) %>% 
  left_join(twilight_datetimes, by = join_by("lat", "lon", "month", "day")) %>%
  # mutate(position_sunrise_timestamp = sql("TIMESTAMP(CONCAT(EXTRACT(date FROM timestamp), ' ', EXTRACT(time FROM sunrise)))")) %>%
  mutate(position_sunrise_timestamp = sql("TIMESTAMP(
  CONCAT(
    EXTRACT(YEAR FROM timestamp), '-',
  EXTRACT(MONTH FROM sunrise), '-',
  EXTRACT(DAY FROM sunrise), ' ',
  EXTRACT(time FROM sunrise)))")) %>%
  mutate(time_from_sunrise = sql("TIMESTAMP_DIFF(timestamp, position_sunrise_timestamp, HOUR)")) %>% 
  select(ssvid, timestamp, position_sunrise_timestamp, sunrise, time_from_sunrise) %>% 
  collect()


a %>% 
  mutate(time_from_sunrise = case_when(time_from_sunrise >= 13 ~ time_from_sunrise -24,
                                       time_from_sunrise <= -13 ~ time_from_sunrise +24,
                                       T ~ time_from_sunrise)) %>%
  # mutate(time_from_sunrise = ifelse(time_from_sunrise <= -13,
  #                                   time_from_sunrise + 24,
  #                                   time_from_sunrise)) %>% 
  ggplot(aes(x = time_from_sunrise )) +
  geom_histogram(aes(y = after_stat(count / sum(count))), binwidth = 1) 
