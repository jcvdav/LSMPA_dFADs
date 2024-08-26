#!/bin/bash

# Upload all local csv files to GCS Bucket
gsutil cp ./raw_data/daily_twilight_and_sunrise_times_1_deg.csv gs://lsmpa_dfads

# Delete BQ table
# bq rm -f -t lsmpas:lsmpa_dfads.daily_twilight_and_sunrise_times_1_deg_v_20240813

# Create a BQ table
bq mk --table \
--schema lon:NUMERIC,lat:NUMERIC,date:DATE,yday:INTEGER,nautical_twilight:TIMESTAMP,sunrise:TIMESTAMP \
--description "Daily twilignt and sunrise times for 1-deg bins (-180 to 180, and -50 to 50)." \
emlab-gcp:jc_scratch.daily_twilight_and_sunrise_times_1_deg_v_20240813

# Upload from GCS bcket to Big Query table
bq load \
--source_format=CSV \
--skip_leading_rows=1 \
--replace \
emlab-gcp:jc_scratch.daily_twilight_and_sunrise_times_1_deg_v_20240813 \
gs://lsmpa_dfads/daily_twilight_and_sunrise_times_1_deg.csv